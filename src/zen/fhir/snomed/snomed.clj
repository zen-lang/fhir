(ns zen.fhir.snomed.snomed
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [cheshire.core :as json]
            [next.jdbc :as jdbc]
            [next.jdbc.prepare :as prep]))

(def db
  "JDBC connection string for postgres database
  NOTE: change port to port of your local postgres installation"
  "jdbc:postgresql://localhost:5432/postgres?user=ghrp&password=postgrespw")

(defn snomed-files
  "
  `sm-path` - Path to unzipped SNOMED CT bundle

  Description:
  List of SNOMED CT terminology snapshot file paths and resource names.

  file paths are constructed from `sm-path`.
  I.e. if `sm-path` is absolute, then result will have absolute paths

  resource names are second parts of underscore delimited file names.
  E.g. for sct2_Concept_... it will be Concept

  Return format: ({:name resource-name :path file-path} ...)"
  [sm-path]
  (let [snapshot-relative-path "Snapshot/Terminology"
        snapshot-path (str sm-path \/ snapshot-relative-path)]
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

(defn init-db-tables
  "Run sqlite database migration.
  Queries are splitted with string: --;-- "
  [db]
  (let [migration-sql (slurp (io/resource "snomed/init.sql"))
        migrations (str/split migration-sql #"\n--;--\n")]
    (doseq [migration migrations]
      (jdbc/execute! db [migration]))))

(defn load-file
  "Load SNOMED CT tsv file.
  Table must exist before."
  [db table path]
  (jdbc/execute! db [(format "COPY %s FROM '%s' csv header DELIMITER e'\\t'" table path)]))

(defn load-files
  "Load needed snomed files
  Concept, Description, Relationship, TextDefinition

  See load-file."
  [db files]
  (doseq [{path :path name :name} files
          :when (contains? #{"Concept" "Description" "Relationship" "TextDefinition"} name)]
    (load-file db (str/lower-case name) path)))


(defn prepare-tables&build-indexes [db]
  ;;Concept table indexes
  (jdbc/execute! db ["DROP INDEX IF EXISTS concept_id; CREATE INDEX concept_id ON concept (id);"])
  (jdbc/execute! db ["ALTER TABLE concept ADD COLUMN IF NOT EXISTS display text"])
  (jdbc/execute! db ["ALTER TABLE concept ADD COLUMN IF NOT EXISTS definition text"])
  (jdbc/execute! db ["ALTER TABLE concept ADD COLUMN IF NOT EXISTS ancestors jsonb"])
  (jdbc/execute! db ["ALTER TABLE concept ADD COLUMN IF NOT EXISTS root jsonb"])

  ;;Description table indexes
  (jdbc/execute! db ["DROP INDEX IF EXISTS description_conceptid; CREATE INDEX description_conceptid ON description (conceptid);"])
  (jdbc/execute! db ["DROP INDEX IF EXISTS description_typeid; CREATE INDEX description_typeid ON description (typeid);"])

  ;;Relationship table keep only active & "is-a" relations
  (jdbc/execute! db ["DELETE FROM relationship WHERE typeid <> '116680003' OR active <> '1'"])
  (jdbc/execute! db ["VACUUM (FULL) relationship"])

  (jdbc/execute! db ["DROP INDEX IF EXISTS relationship_sourceid; CREATE INDEX relationship_sourceid ON relationship (sourceid);"])
  (jdbc/execute! db ["DROP INDEX IF EXISTS relationship_destinationid; CREATE INDEX relationship_destinationid ON relationship (destinationid);"])

  ;;sdl table indexes
  (jdbc/execute! db ["DROP INDEX IF EXISTS sdl_src; CREATE INDEX sdl_src ON sdl (src);"])
  (jdbc/execute! db ["DROP INDEX IF EXISTS sdl_dst; CREATE INDEX sdl_dst ON sdl (dst);"])
  (jdbc/execute! db ["DROP INDEX IF EXISTS sdl_src_dst; CREATE UNIQUE INDEX sdl_src_dst ON sdl USING btree (src, dst);"]))

(defn populate-sdl-table
  "For each concept find every ancestor and calculate distance between concept and this ancestor
  according to http://people.apache.org/~dongsheng/horak/100309_dag_structures_sql.pdf"
  [db]
  (jdbc/execute! db [
"WITH RECURSIVE bfs AS
  (SELECT destinationid src,
          sourceid dst,
          1 dist
   FROM relationship
   UNION SELECT b.src src,
                r.sourceid dst,
                b.dist + 1 dist
   FROM bfs b
   INNER JOIN relationship r ON r.destinationid = b.dst)
INSERT INTO sdl
SELECT src,
       dst,
       min(dist)
FROM bfs
GROUP BY src,
         dst;"]))

(defn populate-concept-table-with-ancestors [db]
  "Calculate ancestors (based on sdl table) for each concept & create map with following strucutre
   {`code`: `distance-to-current-concept`}"
  (jdbc/execute! db [
"UPDATE concept c
 SET    ancestors = (SELECT jsonb_object_agg(src, dist)
                     FROM   sdl m
                     WHERE  m.dst = c.id);"]))

(defn calculate-concept-roots [db]
  (jdbc/execute! db ["DROP INDEX IF EXISTS concept_ancestors; CREATE INDEX concept_ancestors ON concept USING gin(ancestors);"])

  (jdbc/execute! db [
                     "WITH roots AS
  (SELECT c.id cid,
          jsonb_agg(t.id) rids
   FROM concept t
   LEFT JOIN concept c ON c.ancestors @?? ('$.\"' || t.id || '\"')::jsonpath
   WHERE t.ancestors @> '{\"138875005\": 1}'::JSONB
   GROUP BY c.id)
UPDATE concept c
SET root = rids
FROM roots r
WHERE c.id = r.cid;"]))

(defn join-displays [db]
  (jdbc/execute! db ["
UPDATE concept c
SET display = d.term
FROM description d
WHERE d.typeid = '900000000000003001' AND d.active = '1' AND d.conceptid = c.id"]))

(defn join-textdefinitions [db]
  (jdbc/execute! db ["
with aggs as (select conceptid cid, string_agg(term, '; ') saggs from textdefinition where active = '1' group by conceptid)
UPDATE concept c
SET definition = a.saggs
FROM aggs a
WHERE a.cid = c.id"]))

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
                           'ancestors', ancestors,
                           'property', jsonb_build_object('roots', root),
                           'definition', definition)) FROM concept)
TO PROGRAM 'cat >> %s' csv delimiter e'\\x02' quote e'\\x01'"
                               (str (str/replace (:url cs) "/" "-") "-")
                               (:url cs)
                               (:url vs)
                               out-path)])))

(def green "\u001B[32m")
(def clear-color "\u001B[0m")

(defmacro print-wrapper
  [& bodies]
  (reduce (fn [acc [f :as ff]]
            `(~@acc
              (printf ~"Executing step: `%s`\n" '~f)
              (flush)
              (printf "%sStep `%s` finished!%s\n%s\n"
                      ~green '~f ~clear-color (with-out-str (time ~ff)))
              (flush)))
          '(do)
          bodies))

(defn write-snomed-ndjson-gz-zip-bundle [in out]
  (with-open [zip (-> out
                      clojure.java.io/file
                      clojure.java.io/output-stream
                      (java.util.zip.ZipOutputStream.))]
    (let [entry (-> "snomed-terminology-bundle.ndjson.gz"
                    (java.util.zip.ZipEntry.))]
      (.putNextEntry zip entry)
      (with-open [gzip (-> zip
                           (java.util.zip.GZIPOutputStream. true)
                           (java.io.OutputStreamWriter.)
                           (java.io.BufferedWriter.))]

        (let [ndjson-reader (clojure.java.io/reader in)]
          (loop [ndjson-line (.readLine ndjson-reader)]
            (when ndjson-line
              (.write gzip (str ndjson-line \newline))
              (recur (.readLine ndjson-reader)))))))))

(defn pack-snomed-terminology-bundle
  "`db` - JDBC Connection string
   `sf` - result of (snomed-files path-to-unzipped-snomed)
   `out-path` - the path where the resulting snomed bundle will be placed"
  [db sf out-path]
  (print-wrapper
    (init-db-tables db)
    (load-files db sf)
    (prepare-tables&build-indexes db)
    (populate-sdl-table db)
    (populate-concept-table-with-ancestors db)
    (calculate-concept-roots db)
    (join-displays db)
    (join-textdefinitions db)
    (build-terminology-bundle db "/tmp/snomed" code-system value-set)
    (write-snomed-ndjson-gz-zip-bundle "/tmp/snomed" out-path)))


(comment
  (pack-snomed-terminology-bundle db
                                  (snomed-files "/Users/ghrp/Downloads/SnomedCT_USEditionRF2_PRODUCTION_20220301T120000Z")
                                  "/tmp/SnomedCT_USEditionRF2_PRODUCTION_20220301.zip")

  )
