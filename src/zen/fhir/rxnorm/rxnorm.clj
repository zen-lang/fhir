(ns zen.fhir.rxnorm.rxnorm
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.data.csv :as csv]
            [cheshire.core :as json]
            [next.jdbc :as jdbc]
            [next.jdbc.sql :as sql]
            [next.jdbc.result-set :as rs]
            [next.jdbc.prepare :as prep]
            )
  (:import java.util.zip.ZipInputStream
           java.util.zip.GZIPOutputStream))

#_(def rxnorm-path "Path to rxnorm bundle, now not implemented"
    "/tmp/Terminology_RxNorm_full_06062022.zip")


(def rxnorm-path
  "Path to unzipped rxnorm bundle"
  "/tmp/rxnorm/RxNorm_full_06062022")


(def dbpath
  "Path where to create sqlite database"
  "/tmp/rxnorm.db")


(def db
  "JDBC connection string for sqlite database"
  (str "jdbc:sqlite:" dbpath))


(defn migrate-db
  "Run sqlite database migration.
  Migration is taken from resource file rxnorm/init-tables.sql
  Queries are splitted with string: --;--

  These migrations drop tables for rxnconso and rxncui and then create tables to load these rrf files"
  [db]
  (let [migration-string (slurp (io/resource "rxnorm/init-tables.sql"))
        migrations (str/split migration-string #"--;--\n")]
    (doseq [migration migrations]
      (jdbc/execute! db [migration]))))


(defn load-csv
  "Load rrf file into the database.
  The table name is lower case filename without extension.
  The table must be prepared with migrate-db first."
  [db file]
  (let [csv (->> (csv/read-csv (slurp (str rxnorm-path "/rrf/" (str/upper-case file) ".RRF"))
                           :separator \|)
                 (mapv butlast))
        column-count (count (first csv))
        insert-spec (str \( (str/join ", " (repeat column-count "?")) \))
        table (str/lower-case file)]
    (jdbc/with-transaction [tx db]
      (with-open [stmt (jdbc/prepare tx
                                     [(format "INSERT INTO %s VALUES %s"
                                              (name table) insert-spec)])]
        (doseq [row csv]
          (jdbc/execute! (prep/set-parameters stmt row)))))))


(defn get-concepts
  "Get concepts from database using SQL query from resource rxnorm/concepts.sql"
  [db]
  (let [sql (slurp (io/resource "rxnorm/concepts.sql"))]
    (jdbc/execute! db [sql])))


(defn prepare-fhir-concept
  "Create FHIR concept from RxNorm concept retrieved from database

  additional-valueset-links is a map where keys are concept codes and values
  are vectors containing valueset ids except main valueset id.

  raw-concept is an RxNorm concept retrieved from sqlite via get-concepts."
  [additional-valueset-links raw-concept]
  (let [code (:rxnconso/rxcui raw-concept)
        display (:rxnconso/str raw-concept)
        tty (:rxnconso/tty raw-concept)]
    {:id (str "rxnorm-20220603-" code)
     :resourceType "Concept"
     :_source "zen.fhir"
     :code code
     :system "http://www.nlm.nih.gov/research/umls/rxnorm"
     :display display
     :property [{:code "type"
                 :value {:code tty}}
                {:code "status"
                 :value {:code "active"}}]
     :valueset (cond-> ["rxnorm-2022-06-06"]
                 (contains? additional-valueset-links code)
                 (into (get additional-valueset-links code)))}))


(defn prepare-fhir-concepts
  "Convert multiple RxNorm concepts to FHIR concepts.
  See prepare-fhir-concept."
  [additional-valueset-links raw-concepts]
 (->> raw-concepts
      (mapv (partial prepare-fhir-concept additional-valueset-links))))


(def codesystem
  "RxNorm CodeSystem resource"
  {:resourceType "CodeSystem"
   :id "rxnorm"
   :language "en"
   :url "http://www.nlm.nih.gov/research/umls/rxnorm"
   :date "2022-06-06"
   :identifier [{:system "urn:ietf:rfc:3986"
                 :value "urn:oid:2.16.840.1.113883.6.88"}]
   :name "RxNorm"
   :title "RxNorm"
   :description "RxNorm is a standardized nomenclature for clinical drugs"
   :content "complete"
   :status "active"
   :publisher "National Library of Medicine (NLM)"
   :contact [{:name "National Library of Medicine (NLM)"
              :telecom [{:system "email"
                         :value "rxnorminfo@nlm.nih.gov"}]}]
   :property [{:code "type"
               :type "code"}
              {:code "status"
               :value "code"}]
   :version "06062022"})


(def valueset-main
  "RxNorm ValueSet resource containing all codes"
  {:resourceType "ValueSet",
   :id "rxnorm-2022-06-06",
   :description "This value set includes all RxNorm codes.",
   :version "06062022"
   :compose {:include [{:system "http://www.nlm.nih.gov/research/umls/rxnorm"}]}
   :url "http://www.nlm.nih.gov/research/umls/rxnorm/vs"
   :status "active"})


(defn write-line
  "Write data as a single line json string terminated with NL

  w: object to write to
  x: data to write"
  [w x]
  (.write w (cheshire.core/generate-string x))
  (.write w "\n"))


(defn load-prepared-valueset
  "Load valueset prepared with extract-valueset.py

  The prepared valueset file is a json array in which
  the first element is a valueset resource and the second
  is an array of concept codes included in this valueset.

  loaded-valuesets is internal cache which is a map
  where :valuesets key corresponds to a vector of valueset resources
  and :links key gives a map linking concept code and an arra
  of valueset ids containing the corresponding code.

  This function updates loaded-valuesets adding new valueset
  to :valueset key and updating concept code -> valueset links."
  [loaded-valuesets file]
  (let [loaded-links (or (:links loaded-valuesets) {})
        loaded-vs-resources (or (:valuesets loaded-valuesets) [])
        content (slurp file)
        [vs concepts] (json/parse-string content keyword)
        vs-id (:id vs)]
    {:links (reduce (fn [links id]
               (if (contains? links id)
                 (update links id conj vs-id)
                 (assoc links id [vs-id])))
             loaded-links
             concepts)
     :valuesets (conj loaded-vs-resources vs)}))


(defn load-prepared-valuesets
  "Load multiple valuesets prepared with extract-valueset.py.
  See load-prepared-valueset."
  ([files]
   (load-prepared-valuesets {} files))

  ([loaded-valuesets files]
   (reduce load-prepared-valueset loaded-valuesets files)))


(defn write-ndjson-gz-zip-bundle
  "Create terminology bundle.
  A terminology bundle is a .zip file containing .ndjson.gz file.

  additional-valuesets is an argument that contains additional valuesets to include
  which are loaded with load-prepared-valuesets.

  target is the filename the bundle will be created with."
  [additional-valuesets target]
  (let [codesystem codesystem
        valueset valueset-main
        concepts (prepare-fhir-concepts (:links additional-valuesets) (get-concepts db))]
    (with-open [zip (-> target
                        clojure.java.io/file
                        clojure.java.io/output-stream
                        (java.util.zip.ZipOutputStream.))]
      (let [entry (-> "rxnorm-terminology-bundle.ndjson.gz"
                      (java.util.zip.ZipEntry.))]
        (.putNextEntry zip entry)
        (with-open [gzip (-> zip
                             (java.util.zip.GZIPOutputStream. true)
                             (java.io.OutputStreamWriter.)
                             (java.io.BufferedWriter.))]

          (write-line gzip codesystem)
          (write-line gzip valueset)
          (doseq [vs (:valuesets additional-valuesets)]
            (write-line gzip vs))
          (doseq [c concepts]
            (write-line gzip c)))))))


(comment
  (with-out-str (time (migrate-db db))) ; Recreate RxNorm tables
  (with-out-str (time (load-csv db "rxnconso"))) ; Load RxNorm concepts
  (with-out-str (time (load-csv db "rxncui"))) ; Load RxNorm concept deprecation info
  (with-out-str (time (def rcs (get-concepts db)))) ; Prepare and get RxNorm concepts
  (with-out-str (time (def avs (load-prepared-valuesets ["/tmp/rxnorm/cdsfaaid.json"
                                                         "/tmp/rxnorm/cdi.json"])))) ; Load additional valuesets
  (with-out-str (time (write-ndjson-gz-zip-bundle avs "/tmp/tgt.zip")))) ; Create terminology bundle containing additional valuesets
