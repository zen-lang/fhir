(ns zen.fhir.loinc2
  (:require [org.httpkit.client :as http]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [cheshire.core :as json]
            [clojure.xml :as xml]
            [clojure.zip :as zip]
            [clojure.data.csv :as csv]
            [next.jdbc :as jdbc]
            [next.jdbc.sql :as sql]
            [next.jdbc.prepare :as prep]))


(def login (System/getenv "LOINC_LOGIN"))
(def pass (System/getenv "LOINC_PASSWORD"))

(def loinc-codesystem-url "https://github.com/loinc/loinc-fhir-codesystem/archive/refs/tags/v0.10.zip")

(defn get-loinc-codesystem-xml-istream []
  (let [codesystem-zip-istream (-> (io/input-stream loinc-codesystem-url)
                                   (java.util.zip.ZipInputStream.))]
    (loop [entry (.getNextEntry codesystem-zip-istream)]
      (if (str/ends-with? (.getName entry) ".xml")
        codesystem-zip-istream
        (recur (.getNextEntry codesystem-zip-istream))))))

(defn get-loinc-codesystem-xml []
  (xml/parse (get-loinc-codesystem-xml-istream)))

(defn rename-value [attrs]
  (if (:value attrs)
    (-> attrs
        (assoc :xml/value (:value attrs))
        (dissoc :value))
    attrs))

(defn collect-fields [xml]
  (cond
    (vector? xml) (group-by :xml/tag (mapv collect-fields xml))
    (map? xml) (try
               {:xml/tag (:tag xml)
                :xml/data (merge (rename-value (:attrs xml)) (collect-fields (:content xml)))}
                  (catch Throwable e (println (:attrs xml) (:content xml))))
    :else xml))

(defn collect-data [intermediate]
  (cond
    (vector? intermediate) (mapv collect-data intermediate)

    (and (map? intermediate)
         (:xml/tag intermediate))
    (collect-data (:xml/data intermediate))

    (map? intermediate)
    (into {} (mapv (fn [[key value]] [key (collect-data value)]) intermediate))

    :else intermediate))

(defn unpack-value [intermediate]
  (cond
    (vector? intermediate) (mapv unpack-value intermediate)

    (and (map? intermediate) (:xml/value intermediate))
    (unpack-value (:xml/value intermediate))

    (map? intermediate)
    (into {} (mapv (fn [[key value]] [key (unpack-value value)]) intermediate))

    :else intermediate))


(defn unarray-needed? [key]
  (get #{:description :uri :type :code :id :compositional :status :hierarchyMeaning :valueSet
         :url :caseSensitive :title :name :copyright :experimental :publisher :content
         :system :value :versionNeeded} key))

(defn unarray [intermediate]
  (cond
    (vector? intermediate) (mapv unarray intermediate)

    (map? intermediate)
    (into {} (mapv (fn [[key value]]
                     (if (unarray-needed? key)
                       [key (unarray (first value))]
                       [key (unarray value)]))
                   intermediate))

    :else intermediate))

(defn string->bool [s]
  (case s
    "true" true
    "false" false
    s))

(defn cast-bool [intermediate]
  (cond
    (vector? intermediate) (mapv cast-bool intermediate)

    (map? intermediate)
    (into {} (mapv (fn [[key value]]
                     [key (cast-bool value)])
                   intermediate))

    (string? intermediate)
    (string->bool intermediate)

    :else intermediate))

(defn codesystem-xml->edn [xml]
  (-> xml
      collect-fields collect-data unpack-value cast-bool unarray
      (dissoc :xmlns)
      (assoc :resourceType "CodeSystem")))

(defn get-loinc-codesystem-edn []
  (codesystem-xml->edn (get-loinc-codesystem-xml)))

(def cs (get-loinc-codesystem-edn))

;;;; CSV
(def loinc-path "path")

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
                         (mapv #(format "%s TEXT" %))
                         (str/join ",\n")
                         (format "(%s)"))
        insert-columns (->> (:header data)
                            (str/join ", ")
                            (format "(%s)"))
        insert-spec (->> (repeat (count (:header data)) "?")
                         (str/join ", ")
                         (format "(%s)"))]
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
  (jdbc/execute! db [(format "CREATE INDEX %s_%s_idx ON %s (%s)"
                             (name table) (name column) (name table) (name column))]))


(defn load-loinc-data [db loinc-base-path]
  (let [path (fn [relative] (str loinc-base-path "/" relative))
        file->table {"LoincTable/Loinc.csv" "loinc"
                     "AccessoryFiles/PartFile/Part.csv" "part"
                     "AccessoryFiles/PartFile/LoincPartLink_Primary.csv" "partlink_primary"
                     "AccessoryFiles/PartFile/LoincPartLink_Supplementary.csv" "partlink_supplementary"}
        table->idx-col {"loinc" "LOINC_NUM"
                        "part" "PartNumber"
                        "partlink_primary" "LoincNumber"
                        "partlink_supplementary" "LoincNumber"}]
    (doseq [[file table] file->table
            :let [filepath (path file)]]
      (load-loinc-csv db table filepath)
      (println ::debug table (get table->idx-col table))
      (create-idx db table (get table->idx-col table)))))


(defn read-loinc-parts []
  (let [csv (slurp (str loinc-path "/AccessoryFiles/PartFile/Part.csv"))
        data (csv/read-csv csv)]
    (mapv zipmap (repeat (first data)) (rest data))))

(defn parts->concepts [codesystem parts]
  (mapv (fn [part]
          {:code (get part "PartNumber")
           :system (:url codesystem)
           :display (get part "PartName")})
        parts))

(defn create-part-index-inner-map [parts]
  (->> parts
       (mapv (fn [part] [(get part "PartName") (get part "PartNumber")]))
       (into {})))

(defn create-part-index [parts]
  (->> (group-by #(get % "PartTypeName") parts)
       (mapv (fn [[type parts-of-type]]
               [type (create-part-index-inner-map parts-of-type)]))
       (into {})))

(defn part->code [index type part]
  (get-in index [type part]))

(defn parse-system [system]
  (let [[system-core super-system] (str/split system #"\^")]
    {:system system
     :system-core system-core
     :super-system super-system}))

(defn generate-properties-system-part [index codesystem code]
  (let [type "SYSTEM"
        system (get code type)
        cs-url (:url codesystem)]
    (when system
      (let [system-parts (parse-system system)]
        [{:code "SYSTEM"
          :valueCoding {:system cs-url
                        :code (part->code index type (:system system-parts))
                        :display (:system system-parts)}}
         {:code "system-core"
          :valueCoding {:system cs-url
                        :code (part->code index type (:system-core system-parts))
                        :display (:system-core system-parts)}}
         {:code "super-system"
          :valueCoding {:system cs-url
                        :code (part->code index type (:super-system system-parts))
                        :display (:super-system system-parts)}}]))))

(defn generate-properties-component-part [codesystem code]
  (let [component (get code "COMPONENT")
        cs-url (:url codesystem)]
    [{:code "COMPONENT"
      :valueCoding {:system cs-url
                    :code component
                    :display component}}]))

(defn generate-properties-property-part [codesystem code]
  (let [property (get code "PROPERTY")
        cs-url (:url codesystem)]
    [{:code "PROPERTY"
      :valueCoding {:system cs-url
                    :code property
                    :display property}}]))

(defn generate-properties-time-aspect-part [codesystem code]
  (let [time-aspect (get code "TIME_ASPCT")
        cs-url (:url codesystem)]
    [{:code "TIME_ASPCT"
      :valueCoding {:system cs-url
                    :code time-aspect
                    :display time-aspect}}]))

(defn generate-properties-scale-type-part [codesystem code]
  (let [scale-type (get code "SCALE_TYP")
        cs-url (:url codesystem)]
    [{:code "SCALE_TYP"
      :valueCoding {:system cs-url
                    :code scale-type
                    :display scale-type}}]))

(defn generate-properties-method-type-part [codesystem code]
  (let [method-type (get code "METHOD_TYP")
        cs-url (:url codesystem)]
    [{:code "METHOD_TYP"
      :valueCoding {:system cs-url
                    :code method-type
                    :display method-type}}]))


#_(defn make-codeable-concepts [codesystem codes]
  (mapv (fn [code]
          {:code (get code "LOINC_NUM")
           :system (:url codesystem)
           :display (get code "LONG_COMMON_NAME")
           :property
           [{:code "SYSTEM"
             :valueCoding cs-url}]
           })
        codes))

#_(distinct (mapv #(get % "SYSTEM") codes))
