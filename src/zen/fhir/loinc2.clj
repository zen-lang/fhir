(ns zen.fhir.loinc2
  (:require [org.httpkit.client :as http]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [cheshire.core :as json]
            [clojure.xml :as xml]
            [clojure.data.csv :as csv]
            [next.jdbc :as jdbc]
            [next.jdbc.sql :as sql]
            [next.jdbc.prepare :as prep]
            [zen.fhir.loinc.xml :as loinc.xml]))

;; CSV
(def loinc-path "/Users/bodyblock/Downloads/loinc")

(defn read-loinc-codes []
  (let [csv (slurp (str loinc-path "/LoincTable/Loinc.csv"))
        data (csv/read-csv csv)]
    (mapv zipmap (repeat (first data)) (rest data))))

(defn read-loinc-csv [path]
  (let [csv (slurp path)
        data (csv/read-csv csv)]
    {:header (first data)
     :data (vec (rest data))}))

(defn prepare-loinc-table [db table data]
  (let [column-spec (->> (:header data)
                         (mapv #(format "`%s` TEXT" %))
                         (str/join ",\n")
                         (format "(%s)"))
        insert-columns (->> (:header data)
                            (mapv #(str "`" % "`"))
                            (str/join ", ")
                            (format "(%s)"))
        insert-spec (->> (repeat (count (:header data)) "?")
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
        (doseq [row (:data data)]
          (jdbc/execute! (prep/set-parameters stmt row)))))))

(defn load-loinc-csv [db table path]
  (let [data (read-loinc-csv path)]
    (prepare-loinc-table db table data)))

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
                     "AccessoryFiles/GroupFile/ParentGroup.csv" "parent_groups"}
        table->idx-col {"loinc" "LOINC_NUM"
                        "part" "PartNumber"
                        "partlink_primary" "LoincNumber"
                        "partlink_supplementary" "LoincNumber"
                        "hierarchy" ["IMMEDIATE_PARENT" "CODE"]
                        "answerlist" ["AnswerListId" "AnswerStringId"]
                        "answerlistlink" ["LoincNumber" "AnswerListId"]
                        "loinc_groups" ["LoincNumber" "GroupId"]
                        "groups" ["ParentGroupId" "GroupId"]
                        "parent_groups" "ParentGroupId"}]
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

(defn get-base-properties [codesystem]
  (->> (:property codesystem)
       (filter #(not= "Coding" (:type %)))
       (filter #(not (get #{"parent" "child"} (:code %))))
       (mapv :code)))

(defn multiline [& strings]
  (str/join "\n" strings))

(defn pad [n s]
  (->> (str/split-lines s)
       (mapv #(str (str/join (repeat n " ")) %))
       (str/join "\n")))

(defn sql-json-base-property [base-property]
  (format (multiline "'%s', json_object("
                     "    'code', '%s',"
                     "    'valueString', %s)")
          base-property base-property base-property))

(defn generate-base-properties-sql [codesystem]
  (let [base-properties (->> (get-base-properties codesystem)
                             (mapv sql-json-base-property)
                             (str/join ",\n"))]
    (multiline "CREATE TABLE loinc_base_json AS"
               "SELECT LOINC_NUM AS id"
               "     , json_object("
               (str (pad 11 base-properties) ")")

               "       as property"
               "FROM loinc")))

(defn create-base-property-table [db cs]
  (let [sql (generate-base-properties-sql cs)]
    (jdbc/execute! db [sql])))

(defn create-core-concepts [db]
  (let [sql (-> (io/resource "loinc/part-concept.sql")
                 slurp)]
    (jdbc/execute! db [sql])))

(defn create-part-concepts [db]
  (let [sql (-> (io/resource "loinc/part-concept.sql")
                slurp)]
    (jdbc/execute! db [sql])))

(defn get-answer-list-value-sets [db]
  (let [sql (-> (io/resource "loinc/answers-value-set.sql")
                slurp)]
    (jdbc/execute! db [sql])))

(defn get-answers-concepts [db]
  (let [sql (-> (io/resource "loinc/answers-concepts.sql")
                slurp)]
    (jdbc/execute! db [sql])))


(comment
  (def db "jdbc:sqlite:/Users/bodyblock/Downloads/loinc/db.sqlite")

  (def cs (loinc.xml/get-loinc-codesystem-edn))

  (load-loinc-data db loinc-path)

  (create-core-concepts db)

  (create-part-concepts db)

  (generate-base-properties-sql cs)

  (create-primary-link-table db)

  (create-supplementary-link-table db)

  (jdbc/execute! db ["DROP table partlink_supplementary_json"])

  (jdbc/execute! db ["SELECt * from partlink_primary_json limit 10"])

  (create-base-property-table db cs)



  )
