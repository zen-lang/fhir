(ns zen.fhir.snomed.snomed
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [cheshire.core :as json]
            [next.jdbc :as jdbc]
            [next.jdbc.prepare :as prep]))

(def snomed-path
  "Path to unzipped SNOMED CT bundle
  NOTE: change path to yours local snomed bundle location"
  "/Users/ghrp/Downloads/SnomedCT_USEditionRF2_PRODUCTION_20220301T120000Z")

(def db
  "JDBC connection string for postgres database
  NOTE: change port to port of your local postgres installation"
  "jdbc:postgresql://localhost:5432/postgres?user=ghrp&password=postgrespw")

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

(defn prepare-tables [db]
  ;;Concept table indexes
  (jdbc/execute! db ["DROP INDEX IF EXISTS concept_id; CREATE INDEX concept_id ON concept (id);"])
  (jdbc/execute! db ["ALTER TABLE concept ADD COLUMN IF NOT EXISTS path jsonb"])
  (jdbc/execute! db ["ALTER TABLE concept ADD COLUMN IF NOT EXISTS display text"])
  (jdbc/execute! db ["ALTER TABLE concept ADD COLUMN IF NOT EXISTS definition text"])
  (jdbc/execute! db ["ALTER TABLE concept ADD COLUMN IF NOT EXISTS children text"])
  (jdbc/execute! db ["ALTER TABLE concept ADD COLUMN IF NOT EXISTS parents text"])

  ;;Description table indexes
  (jdbc/execute! db ["DROP INDEX IF EXISTS description_conceptid; CREATE INDEX description_conceptid ON description (conceptid);"])
  (jdbc/execute! db ["DROP INDEX IF EXISTS description_typeid; CREATE INDEX description_typeid ON description (typeid);"])

  ;;Relationship table delete every non "is-a" relation
  (jdbc/execute! db ["DELETE FROM relationship WHERE typeid <> '116680003' OR active <> '1'"])
  (jdbc/execute! db ["VACUUM (FULL) relationship"])

  (jdbc/execute! db ["DROP INDEX IF EXISTS relationship_sourceid; CREATE INDEX relationship_sourceid ON relationship (sourceid);"])
  (jdbc/execute! db ["DROP INDEX IF EXISTS relationship_destinationid; CREATE INDEX relationship_destinationid ON relationship (destinationid);"])

  )

(defn build-hierarchies [db]
  (time (jdbc/execute! db ["
WITH path_as AS (select CO.id id, (WITH RECURSIVE parent_line(path, id) AS (
                                                 SELECT ARRAY[]::text[] path, R.sourceId id, R.destinationId pid
                                                 FROM relationship R
                                                 WHERE R.sourceId = CO.id
                                                 UNION ALL
                                                 SELECT ARRAY[R.sourceId] || PL.path path, R.sourceId, R.destinationId pid
                                                 FROM parent_line PL
                                                 JOIN relationship R on R.sourceId = PL.pid)
             SELECT jsonb_agg(ARRAY ['138875005'] || path) path
             FROM parent_line PL
             WHERE pid = '138875005')
                from concept CO where co.active = '1' AND path is null)
UPDATE concept as c
SET path = path_as.path
FROM path_as
WHERE c.id = path_as.id"])))

(defn join-displays [db]
  (time
    (jdbc/execute! db ["
UPDATE concept c
SET display = d.term
FROM description d
WHERE d.typeid = '900000000000003001' AND d.active = '1' AND d.conceptid = c.id"])))

(defn join-textdefinitions [db]
  (time
    (jdbc/execute! db ["
with aggs as (select conceptid cid, string_agg(term, '; ') saggs from textdefinition where active = '1' group by conceptid)
UPDATE concept c
SET definition = a.saggs
FROM aggs a
WHERE a.cid = c.id"])))

(defn build-parents&children-props [db]
  (jdbc/execute! db ["
    UPDATE concept c
    SET parents =
     (SELECT json_agg(child)
      FROM
        (select distinct rel.destinationid AS child
         FROM relationship rel
         WHERE rel.sourceid = c.id) AS children);"])

  (jdbc/execute! db ["
    UPDATE concept c
    SET children =
     (SELECT json_agg(parent)
      FROM
        (select distinct rel.sourceid AS parent
         FROM relationship rel
         WHERE rel.destinationid = c.id) AS parents);
"]))


(def source "snomed")
(def system "http://snomed.info/sct")

(def code-system
  {:resourceType "CodeSystem"
   :id source
   :url system
   :date "2022-03-01"
   :description "SNOMED"
   :content "complete"
   :name "SNOMED-CT"
   :status "active"
   :caseSensitive true})

(def value-set
  {:id source
   :resourceType "ValueSet"
   :compose { :include [{:system system}]}
   :date "2022-03-01"
   :status "active"
   :name "SNOMED-CT"
   :description "SNOMED-CT"
   :url system})

(defn build-terminology-bundle [db out-path cs vs]
  (time
    (do
      (spit out-path (str (json/generate-string cs) \newline) :append true)
      (spit out-path (str (json/generate-string vs) \newline) :append true)
      (jdbc/execute! db [(format "
COPY
(SELECT  jsonb_strip_nulls(jsonb_build_object(
                           'id', ('%s' || id),
                           'resourceType', 'Concept',
                           'deprecated', (not (active::boolean)),
                           'system', '%s',
                           'valueset', jsonb_build_array('%s'),
                           'code', id,
                           'display', display,
                           'definition', definition,
                           'hierarchies', path)) FROM concept)
TO PROGRAM 'cat >> %s' csv delimiter e'\\x02' quote e'\\x01'"
                                 (str (str/replace (:url cs) "/" "-") "-")
                                 (:url cs)
                                 (:url vs)
                                 out-path)]))))

(def green "\u001B[32m")
(def clear-color "\u001B[0m")

(defn print-wrapper [bodies]
  (doseq [b bodies]
    (printf "Executing step: `%s`" (first b))
    (printf "%sStep `%s` finished.%s\nOutput: %s" green (first b) clear-color (with-out-str (time (eval b))))))

(defn pack-snomed-terminology-bundle [db]
  (def sf (snomed-files))

  (init-db db)

  (time (load-files db sf))

  (time (prepare-tables db))

  (build-hierarchies db)

  (join-displays db)

  (join-textdefinitions db)

  (build-parents&children-props db)

  )

(comment
  (def sf (snomed-files))

  (init-db db)

  (load-files db sf)

  (prepare-tables db)

  (build-hierarchies db)

  (join-displays db)

  (join-textdefinitions db)

  (build-terminology-bundle db "/tmp/snomed.ndjson" code-system value-set)

  )
