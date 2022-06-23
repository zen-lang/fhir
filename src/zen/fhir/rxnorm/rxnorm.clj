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

#_(def rxnorm-path "/tmp/Terminology_RxNorm_full_06062022.zip")
(def rxnorm-path "/tmp/rxnorm/RxNorm_full_06062022")
(def dbpath "/tmp/rxnorm.db")

(def db (str "jdbc:sqlite:" dbpath))

(defn migrate-db [db]
  (let [migration-string (slurp (io/resource "rxnorm/init-tables.sql"))
        migrations (str/split migration-string #"--;--\n")]
    (doseq [migration migrations]
      (jdbc/execute! db [migration]))))

(defn load-csv [db file]
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

(defn get-concepts [db]
  (let [sql (slurp (io/resource "rxnorm/concepts.sql"))]
    (jdbc/execute! db [sql])))

(defn prepare-fhir-concept [raw-concept]
  (let [code (:rxnconso/rxcui raw-concept)
        display (:rxnconso/str raw-concept)
        tty (:rxnconso/str raw-concept)]
    {:code code
     :id (str "rxnorm-" code)
     :resourceType "Concept"
     :system "http://www.nlm.nih.gov/research/umls/rxnorm"
     :display display
     :property [{:code "type"
                 :value {:code tty}}]
     :valueset ["rxnorm"]}))

(defn prepare-fhir-concepts [raw-concepts]
 (->> raw-concepts
      (mapv prepare-fhir-concept)))

(def codesystem
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
   :version "06062022"})

(def valueset-main
  {:resourceType "ValueSet",
   :id "rxnorm-2022-06-06",
   :description "This value set includes all RxNorm codes.",
   :version "06062022"
   :compose {:include [{:system "http://www.nlm.nih.gov/research/umls/rxnorm"}]}
   :url "http://www.nlm.nih.gov/research/umls/rxnorm/vs"
   :status "active"})

(defn write-line [w x]
  (.write w (cheshire.core/generate-string x))
  (.write w "\n"))


(defn write-ndjson-gz-zip-bundle [target]
  (let [codesystem codesystem
        valueset valueset-main
        concepts (prepare-fhir-concepts (get-concepts db))]
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
          (doseq [c concepts]
            (write-line gzip c)))))))


(comment
  (time (migrate-db db))
  (time (load-csv db "rxnconso"))
  (time (load-csv db "rxncui"))
  (with-out-str (time (def rcs (get-concepts db))))
  (with-out-str (time (def cos (prepare-fhir-concepts rcs))))
  (with-out-str (time (def bndl (generate-ndjson-bundle valueset-main codesystem cos))))
  (with-out-str (time (write-ndjson-gz-zip-bundle "/tmp/tgt.zip")))

  )
