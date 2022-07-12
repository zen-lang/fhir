(ns zen.fhir.snomed.snomed
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [cheshire.core :as json]
            [next.jdbc :as jdbc]
            [next.jdbc.prepare :as prep]))

(def snomed-path
  "Path to unzipped SNOMED CT bundle"
  "")

(def dbpath
  "Path where to create sqlite database"
  "/tmp/snomed.db")

(def db
  "JDBC connection string for sqlite database"
  (str "jdbc:sqlite:" dbpath))


(defn snomed-files
  "List of SNOMED CT terminology snapshot file paths and resource names.

  file paths are constructed from snomed-path.
  I.e. if snomed-path is absolute, then result will have absolute paths

  resource names are second parts of underscore delimited file names.
  E.g. for sct2_Concept_... it will be Concept

  Return format: ({:name resource-name :path file-path} ...)"
  []
  (let [snapshot-relative-path "Snapshot/Terminology"
        snapshot-path (str snomed-path \/ snapshot-relative-path)]
    (->> (io/file snapshot-path)
        .list
        (filter #(str/ends-with? % ".txt"))
        (map
         (fn [filename]
           (let [full-path (str snapshot-path \/ filename)
                 ;; filenames in SNOMED CT are like
                 ;; sct2_Concept_Snapshot_...
                 resource-name (second (str/split filename #"_"))]
             {:path full-path
              :name resource-name}))))))

(defn init-db
  "Run sqlite database migration.
  Queries are splitted with string: --;-- "
  [db]
  (let [migration-sql (slurp (io/resource "snomed/init.sql"))
        migrations (str/split migration-sql #"\n--;--\n")]
    (doseq [migration migrations]
      (jdbc/execute! db [migration]))))


(defn read-tsv
  "Read tab-separated values lazily
  Returns lazy list of vector of values"
  [file]
  (let [reader (clojure.java.io/reader file)
        lines (line-seq reader)]
    [reader (map #(str/split % #"\t") lines)]))


(defn load-file
  "Load SNOMED CT tsv file.
  Table must exist before."
  [db table file]
  (let [[reader tsv] (read-tsv file)
        column-count (count (first tsv))
        insert-spec (str \( (str/join ", " (repeat column-count "?")) \))
        data (rest tsv)]
    (jdbc/with-transaction [tx db]
      (with-open [stmt (jdbc/prepare tx
                                     [(format "INSERT INTO %s VALUES %s"
                                              (name table) insert-spec)])]
        (doseq [row data]
          (jdbc/execute! (prep/set-parameters stmt row)))))
    (.close reader)))

(defn load-files
  "Load needed snomed files
  Concept, Description, Relationship, TextDefinition

  See load-file."
  [db files]
  (doseq [{path :path name :name} files
          :when (contains? #{"Concept" "Description" "Relationship" "TextDefinition"} name)]
    (load-file db (str/lower-case name) path)))


(comment
  (def sf (snomed-files))
  (init-db db)
  (time (load-files db sf))
  )
