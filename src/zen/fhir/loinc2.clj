(ns zen.fhir.loinc2
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [cheshire.core :as json]
   [clojure.xml :as xml]
   [clojure.data.csv :as csv]
   [next.jdbc :as jdbc]
   [next.jdbc.sql :as sql]
   [next.jdbc.result-set :as rs]
   [next.jdbc.prepare :as prep]
   [zen.fhir.loinc.xml :as loinc.xml]
   [clojure.walk]
   [clj-http.client :as client]
   [clj-http.cookies]
   [clj-http.core])
  (:import [java.sql ResultSet ResultSetMetaData]
           java.io.File
           [java.util.zip ZipInputStream]))

;; This file produces three types of resource:
;; 1) CodeSystem
;; 2) ValueSets
;; 3) Concept

;; To create core LOINC terms few preparation steps are needed
;; 1) collect all properties
;;    1. Collect primary properties (also known as parts) from PartPrimaryLink.csv
;;    2. Collect non-primary properties from PartLinkSupplementary.csv
;;    3. Collect `answers-list` property from AnswerListLink.csv
;;    4. Collect `score` property from AnswerList.csv
;;    5. Collect `panel-items` from PanelsAndForms.csv


(def loinc-login-url "https://loinc.org/wp-login.php")
(def loinc-download-url "https://loinc.org/download/loinc-complete/")
(def loinc-login (System/getenv "LOINC_LOGIN"))
(def loinc-password (System/getenv "LOINC_PASSWORD"))

(def tmp-dir (System/getenv "RUNNER_TEMP"))

(def download-path (str tmp-dir "/loinc_downloaded.zip"))
(def loinc-path (str tmp-dir "/loinc"))
(def db (str "jdbc:sqlite:" tmp-dir "/loinc.db"))

(defn unzip-file
  "uncompress zip archive.
  `input` - name of zip archive to be uncompressed.
  `output` - name of folder where to output."
  [input output]
  (with-open [stream (-> input io/input-stream ZipInputStream.)]
    (loop [entry (.getNextEntry stream)]
      (if entry
        (let [save-path (str output File/separatorChar (.getName entry))
              out-file (File. save-path)]
          (if (.isDirectory entry)
            (if-not (.exists out-file)
              (.mkdirs out-file))
            (let [parent-dir (File. (.substring save-path 0 (.lastIndexOf save-path (int File/separatorChar))))]
              (if-not (.exists parent-dir) (.mkdirs parent-dir))
              (clojure.java.io/copy stream out-file)))
          (recur (.getNextEntry stream)))))))

(defn get-loinc-bundle []
  (let [loinc-package (-> (binding [clj-http.core/*cookie-store* (clj-http.cookies/cookie-store)]
                            (client/post loinc-login-url {:form-params {"log" loinc-login
                                                                        "pwd" loinc-password}})

                            (client/post loinc-download-url {:form-params {"tc_accepted" "1"
                                                                           "tc_submit" "Download"}
                                                             :as :byte-array}))
                          :body)]
    (with-open [w (io/output-stream download-path)]
      (.write w loinc-package)
      (unzip-file download-path loinc-path))))

(def json-builder-adapter
  (rs/builder-adapter
   rs/as-unqualified-lower-maps
   (fn [builder ^ResultSet rs ^Integer i]
     (let [rsm ^ResultSetMetaData (:rsmeta builder)]
       (rs/read-column-by-index
        (if (= i 2)
          (cheshire.core/parse-string (.getObject rs i) true)
          (.getObject rs i))
        rsm
        i)))))

(defn read-loinc-codes []
  (let [csv (slurp (str loinc-path "/LoincTable/Loinc.csv"))
        data (csv/read-csv csv)]
    (mapv zipmap (repeat (first data)) (rest data))))

(defn read-loinc-csv [path]
  (let [csv (slurp path)
        data (csv/read-csv csv)]
    {:header (first data)
     :data (vec (rest data))}))

(defn prepare-loinc-table [db table reader]
  (let [data (csv/read-csv reader)
        header (first data)
        _ (println "Header:" header)
        column-spec (->> header
                         (mapv #(format "`%s` TEXT" %))
                         (str/join ",\n")
                         (format "(%s)"))
        insert-columns (->> header
                            (mapv #(str "`" % "`"))
                            (str/join ", ")
                            (format "(%s)"))
        insert-spec (->> (repeat (count header) "?")
                         (str/join ", ")
                         (format "(%s)"))]
    (println ::debug (format "DROP TABLE IF EXISTS %s"  (name table)))
    (println ::debug (format "CREATE TABLE IF NOT EXISTS %s %s" (name table) column-spec))
    (jdbc/execute! db [(format "DROP TABLE IF EXISTS %s"  (name table))])
    (jdbc/execute! db [(format "CREATE TABLE IF NOT EXISTS %s %s" (name table) column-spec)])
    (jdbc/with-transaction [tx db]
      (with-open [stmt (jdbc/prepare tx
                                     [(format "INSERT INTO %s %s VALUES %s"
                                              (name table) insert-columns insert-spec)])]
        (doseq [row (rest data)]
          (jdbc/execute! (prep/set-parameters stmt row)))))))

(defn load-loinc-csv [db table path]
  (let [reader (io/reader path)]
    (prepare-loinc-table db table reader)))

(defn create-idx [db table column]
  (println ::debug (format "DROP INDEX IF EXISTS %s_%s_idx" (name table) (name column)))
  (println ::debug (format "CREATE INDEX %s_%s_idx ON %s (%s)"
                           (name table) (name column) (name table) (name column)))
  (jdbc/execute! db [(format "DROP INDEX IF EXISTS %s_%s_idx" (name table) (name column))])
  (jdbc/execute! db [(format "CREATE INDEX %s_%s_idx ON %s (%s)"
                             (name table) (name column) (name table) (name column))]))

(defn create-idxs [db table columns]
  (cond
    (string? columns) (create-idx db table columns)
    (vector? columns) (doseq [column columns]
                        (create-idx db table column))))

(defn load-loinc-data [db loinc-base-path]
  (let [path (fn [relative] (str loinc-base-path "/" relative))
        file->table {"LoincTable/Loinc.csv" "loinc"
                     "AccessoryFiles/PartFile/Part.csv" "part"
                     "AccessoryFiles/PartFile/LoincPartLink_Primary.csv" "partlink_primary"
                     "AccessoryFiles/PartFile/LoincPartLink_Supplementary.csv" "partlink_supplementary"
                     "AccessoryFiles/MultiAxialHierarchy/MultiAxialHierarchy.csv" "hierarchy"
                     "AccessoryFiles/AnswerFile/AnswerList.csv" "answerlist"
                     "AccessoryFiles/AnswerFile/LoincAnswerListLink.csv" "answerlistlink"
                     "AccessoryFiles/GroupFile/GroupLoincTerms.csv" "loinc_groups"
                     "AccessoryFiles/GroupFile/Group.csv" "groups"
                     "AccessoryFiles/GroupFile/ParentGroup.csv" "parent_groups"
                     "AccessoryFiles/PanelsAndForms/PanelsAndForms.csv", "panels_and_forms"}
        table->idx-col {"loinc" "LOINC_NUM"
                        "part" "PartNumber"
                        "partlink_primary" "LoincNumber"
                        "partlink_supplementary" "LoincNumber"
                        "hierarchy" ["IMMEDIATE_PARENT" "CODE"]
                        "answerlist" ["AnswerListId" "AnswerStringId"]
                        "answerlistlink" ["LoincNumber" "AnswerListId"]
                        "loinc_groups" ["LoincNumber" "GroupId"]
                        "groups" ["ParentGroupId" "GroupId"]
                        "parent_groups" "ParentGroupId"
                        "panels_and_forms", ["ParentLoinc" "Loinc"]}]
    (doseq [[file table] file->table
            :let [filepath (path file)]]
      (load-loinc-csv db table filepath)
      (println ::debug table (get table->idx-col table))
      (create-idxs db table (get table->idx-col table)))))

(defn create-primary-link-table [db]
  (let [sql (-> (io/resource "loinc/part-link.sql")
                slurp
                (str/replace "{{partlink_table}}" "partlink_primary")
                (str/replace "{{partlink_json_table}}" "partlink_primary_json"))]
    (jdbc/execute! db [sql])
    (create-idxs db "partlink_primary" "LoincNumber")))

(defn create-supplementary-link-table [db]
  (let [sql (-> (io/resource "loinc/part-link.sql")
                slurp
                (str/replace "{{partlink_table}}" "partlink_supplementary")
                (str/replace "{{partlink_json_table}}" "partlink_supplementary_json"))]
    (jdbc/execute! db [sql])
    (create-idxs db "partlink_supplementary" "LoincNumber")))

(defn get-base-properties [db]
  (let [columns (->> (jdbc/execute! db ["PRAGMA table_info(loinc)"])
                     (mapv :name))
        main-properties #{"LOINC_NUM" "COMPONENT" "PROPERTY" "TIME_ASPCT" "SYSTEM" "SCALE_TYP" "METHOD_TYP"}]
    (->> columns
         (filterv #(not (main-properties %))))))

(defn multiline [& strings]
  (str/join "\n" strings))

(defn pad [n s]
  (->> (str/split-lines s)
       (mapv #(str (str/join (repeat n " ")) %))
       (str/join "\n")))

(defn sql-json-base-property [base-property]
  (format "'%s', %s"
          (str/lower-case base-property) base-property))

(defn generate-base-properties-sql [db]
  (let [base-properties (->> (get-base-properties db)
                             (mapv sql-json-base-property)
                             (str/join ",\n"))]
    (multiline "CREATE TABLE loinc_base_json AS"
               "SELECT LOINC_NUM AS id"
               "     , json_object("
               (str (pad 11 base-properties) ")")

               "       as property"
               "FROM loinc")))

(defn create-base-property-table [db cs]
  (let [sql (generate-base-properties-sql db)]
    (jdbc/execute! db [sql])))

(defn execute-file-sql [db sql-file & [options]]
  (let [sql (-> (io/resource sql-file)
                slurp)]
    (jdbc/execute! db [sql] options)))

(defn read-sql-file [sql-file]
  (-> (io/resource sql-file)
      slurp))

(defn create-core-concepts [db]
  (execute-file-sql db "loinc/concept-base.sql")
  (create-idx db "loinc_concept", "LoincNumber"))

(defn get-part-concepts [db]
  (execute-file-sql db "loinc/part-concept.sql" {:builder-fn json-builder-adapter}))

(defn get-answer-list-value-sets [db]
  (execute-file-sql db "loinc/answers-value-set.sql" {:builder-fn json-builder-adapter}))

(defn get-answers-concepts [db]
  (execute-file-sql db "loinc/answers-concepts.sql" {:builder-fn json-builder-adapter}))

(defn get-groups-valuesets [db]
  (execute-file-sql db "loinc/groups-valueset.sql" {:builder-fn json-builder-adapter}))

(defn get-parent-groups-valuesets [db]
  (execute-file-sql db "loinc/super-group-valueset.sql" {:builder-fn json-builder-adapter}))

(def valueset {:resourceType "ValueSet"
               :id "loinc"
               :version "2.72"
               :description "This value set includes all LOINC terms"
               :compose {:include [{:system "http://loinc.org"}]}
               :name "LOINC"
               :status "active"
               :_source "zen.fhir"})

(defn remove-empty-vals [m]
  (let [f (fn [x]
            (if (map? x)
              (let [kvs (filter (comp not #(or (nil? %) (= "" %)) second) x)]
                (if (empty? kvs) nil (into {} kvs)))
              x))]
    (clojure.walk/postwalk f m)))

(defn write-line [w x]
  (let [x' (remove-empty-vals x)]
    (.write w (cheshire.core/generate-string x'))
    (.write w "\n")))

(defn create-tables [cs db]
  (jdbc/execute! db ["DROP TABLE IF EXISTS loinc_concept"])
  (jdbc/execute! db ["DROP TABLE IF EXISTS partlink_primary_json"])
  (jdbc/execute! db ["DROP TABLE IF EXISTS partlink_supplementary_json"])
  (jdbc/execute! db ["DROP TABLE IF EXISTS loinc_base_json"])
  (create-base-property-table db cs)
  (create-primary-link-table db)
  (create-supplementary-link-table db)
  (create-core-concepts db)
  (execute-file-sql db "loinc/property-answer-list.sql")
  (execute-file-sql db "loinc/panel-items-property.sql")
  (execute-file-sql db "loinc/concepts-valuesets.sql"))

(defn write-concepts [db writer]
  (reduce (fn [acc row]
            (let [x (-> (cheshire.core/parse-string (:loinc_concept/concept row) true)
                        (remove-empty-vals)
                        (cheshire.core/generate-string))]
              (.write writer x)
              (.write writer "\n")))
          []
          (jdbc/plan db [(str "select * from loinc_concept"
                              " union "
                              (read-sql-file "loinc/part-concept.sql")
                              " union "
                              (read-sql-file "loinc/answers-concepts.sql"))])))

(defn get-value-sets [db]
  (->> (get-answer-list-value-sets db)
       (concat (get-groups-valuesets db))
       (concat (get-parent-groups-valuesets db))
       (map :valueset)))

(defn write-ndjson-gz-zip-bundle [target]
  (let [_ (get-loinc-bundle)
        cs (loinc.xml/get-loinc-codesystem-edn loinc-path)
        _ (load-loinc-data db loinc-path)
        _ (create-tables cs db)
        valuesets (get-value-sets db)]
    (with-open [zip (-> target
                        clojure.java.io/file
                        clojure.java.io/output-stream
                        (java.util.zip.ZipOutputStream.))]
      (let [entry (-> "sdc-loinc-terminology-bundle.ndjson.gz"
                      (java.util.zip.ZipEntry.))]
        (.putNextEntry zip entry)
        (with-open [gzip (-> zip
                             (java.util.zip.GZIPOutputStream. true)
                             (java.io.OutputStreamWriter.)
                             (java.io.BufferedWriter.))]

          (write-line gzip cs)
          (write-line gzip valueset)
          (doseq [v valuesets]
            (write-line gzip v))
          (write-concepts db gzip))))))

(comment
  (def db "jdbc:sqlite:/Users/bodyblock/Downloads/loinc/db.sqlite")

  (write-ndjson-gz-zip-bundle "/Users/bodyblock/Downloads/loinc_output_3.zip")

  (def cs (loinc.xml/get-loinc-codesystem-edn))

  (get-concepts db)

  (load-loinc-data db loinc-path)

  cs

  (create-tables cs db)

  (filter  (fn [c]
             (= (:code c) "12265-5")) (get-concepts db)))
