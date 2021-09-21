(ns zen.fhir.generator
  (:require [zen.fhir.utils :as utils]
            [com.rpl.specter :as sp]
            [clojure.string :as str]
            [cheshire.core :as json]
            [clojure.java.io]
            [clojure.pprint]
            [com.rpl.specter :as sp]))


(defmulti generate-kind-schema
  (fn [_fhir-inter [_url inter-res]]
    (keyword (:kind inter-res))))


(def fhir-primitive->zen-primitive
  '{"boolean" zen/boolean

    "decimal"     zen/number
    "integer"     zen/integer
    "unsignedInt" zen/integer
    "positiveInt" zen/integer

    "string"       zen/string
    "markdown"     zen/string
    "id"           zen/string
    "uuid"         zen/string
    "oid"          zen/string
    "uri"          zen/string
    "url"          zen/string
    "canonical"    zen/string
    "code"         zen/string
    "base64Binary" zen/string
    "xhtml"        zen/string

    "instant"  zen/string
    "dateTime" zen/datetime
    "date"     zen/date
    "time"     zen/string})


(defn confirms-base [fhir-inter [_url inter-res]]
  (let [base-schema (some-> (get-in fhir-inter ["StructureDefinition" (:baseDefinition inter-res) :zen.fhir/schema-ns])
                            name
                            (symbol "schema"))]
    {:confirms (when base-schema #{base-schema})}))


(defmethod generate-kind-schema :primitive-type [fhir-inter [url inter-res]]
  (let [tp         (get-in inter-res [:| :value :type])
        zen-type   (fhir-primitive->zen-primitive tp)]
    (merge {:type zen-type}
           (when (not= "http://hl7.org/fhir/StructureDefinition/Element"
                       (:baseDefinition inter-res))
             (confirms-base fhir-inter [url inter-res])))))


(defn url->symbol [fhir-inter url]
  (when-let [ext-ns (get-in fhir-inter ["StructureDefinition" url :zen.fhir/schema-ns])]
    (symbol (name ext-ns) "schema")))


(defn value-set->symbol [fhir-inter {:keys [url]}]
  (let [value-set (or (get-in fhir-inter ["ValueSet" url]))]
    (symbol (str (:zen.fhir/package-ns value-set) "." (:id value-set))
            "value-set")))


(declare els-schema)


(defn el-schema [fhir-inter [url el]]
  (let [sch (merge-with
              into
              {}
              (when-let [type-sym (some->> (:type el)
                                           (str "http://hl7.org/fhir/StructureDefinition/")
                                           (url->symbol fhir-inter))]
                {:confirms #{type-sym}})
              (when-let [ext-sym (some->> (:fhir/extension el) (url->symbol fhir-inter))]
                {:confirms #{ext-sym}})
              (when (seq (:| el))
                (els-schema fhir-inter [url el]))
              (when (seq (:fhir/flags el))
                (select-keys el #{:fhir/flags}))
              (when (get-in el [:binding :valueSet])
                {:zenbox/value-set {:symbol (value-set->symbol
                                            fhir-inter
                                            (get-in el [:binding :valueSet]))}})
              (when (= "Reference" (:type el))
                {:confirms #{'zenbox/Reference}
                 :zenbox/refers (into #{}
                                      (map (partial url->symbol fhir-inter))
                                      (:profiles el))})
              (when-let [text (or (:short el) (:definiton el))]
                {:zen/desc text}))]
    (if (:vector el)
      (merge {:type 'zen/vector
              :every sch}
             (select-keys el [:minItems :maxItems]))
      sch)))


(defn els-schema [fhir-inter [url inter-res]]
  (merge {:type 'zen/map
          :keys (sp/transform [sp/MAP-VALS]
                              #(el-schema fhir-inter [url %])
                              (:| inter-res))}
         (when-let [requires (->> (:| inter-res)
                                  (into #{}
                                        (comp
                                          (filter (comp :required val))
                                          (map key)))
                                  not-empty)]
           {:require requires})))


(defmethod generate-kind-schema :complex-type [fhir-inter [url inter-res]]
  (merge
    (confirms-base fhir-inter [url inter-res])
    (els-schema fhir-inter [url inter-res])))


(defmethod generate-kind-schema :resource [fhir-inter [url inter-res]]
  (merge-with
    into
    {:confirms #{'zenbox/Resource}}
    (confirms-base fhir-inter [url inter-res])
    (els-schema fhir-inter [url inter-res])))


(defmethod generate-kind-schema :logical [fhir-inter [url inter-res]]
  (merge
    (confirms-base fhir-inter [url inter-res])
    (els-schema fhir-inter [url inter-res])))


(defmethod generate-kind-schema :first-class-extension [fhir-inter [url inter-res]]
  (merge
    (confirms-base fhir-inter [url inter-res])
    (el-schema fhir-inter [url (dissoc inter-res :fhir/extension)]))) ;; Maybe dissoc in core?


(defn generate-zen-schema-dispatch [rt _fhir-inter [_url _inter-res]]
  (keyword rt))


(defmulti generate-zen-schema #'generate-zen-schema-dispatch)


(defmethod generate-zen-schema :default [_rt _fhir-inter [_url _inter-res]])


(defmethod generate-zen-schema :ValueSet [_rt _fhir-inter [_url inter-res]]
  (let [schema-ns   (:zen.fhir/schema-ns inter-res)]
    {schema-ns
     {'ns     schema-ns
      'import #{'zenbox}
      'value-set
      (utils/strip-nils
        {:zen/tags #{'zenbox/value-set}
         :zen/desc (:description inter-res)
         :uri (:url inter-res)
         :version (:version inter-res)})}}))


(defmethod generate-zen-schema :StructureDefinition [_rt fhir-inter [url inter-res]]
  (let [schema-ns   (:zen.fhir/schema-ns inter-res)
        imports     (into #{'zenbox}
                          (keep (fn [inter-path]
                                  (get-in fhir-inter (conj inter-path :zen.fhir/schema-ns))))
                          (mapcat (fn [[tp urls&reasons]] (map (fn [url] [tp url]) (keys urls&reasons)))
                                  (:deps inter-res)))
        instantiated-resource? (and (not (:abstract inter-res))
                                    (= "resource" (:kind inter-res)))
        severity-tag           (case (and instantiated-resource?
                                          (:derivation inter-res))
                                 "constraint"     'zenbox/profile-schema
                                 "specialization" 'zenbox/base-schema
                                 'zenbox/structure-schema)

        schema-part            (generate-kind-schema fhir-inter [url inter-res])]
    {schema-ns {'ns     schema-ns
                'import imports
                'schema (utils/strip-nils
                          (utils/safe-merge-with-into
                            {:zen/tags (into #{'zen/schema}
                                             (when severity-tag [severity-tag]))
                             :zen/desc (:text-description inter-res)
                             :zenbox/type (:type inter-res)
                             :zenbox/profileUri url}
                            schema-part))}}))


(defn ns-by-package [fhir-inter]
  (let [inter-by-package
        (->> fhir-inter
             (sp/select [sp/MAP-VALS sp/MAP-VALS #(contains? % :zen.fhir/schema-ns)])
             (group-by :zen.fhir/package-ns))]
    (sp/transform [sp/MAP-VALS sp/ALL]
                  :zen.fhir/schema-ns
                  inter-by-package)))


(defn packages-deps-nses [fhir-inter]
  (let [inter-by-package
        (->> fhir-inter
             (sp/select [sp/MAP-VALS sp/MAP-VALS #(contains? % :zen.fhir/schema-ns)])
             (group-by :zen.fhir/package-ns))
        packages
        (sp/transform [sp/MAP-VALS]
                      (fn find-first-package-info-data [package-inter]
                        (->> package-inter
                             (keep #(not-empty (select-keys % [:zen.fhir/package :zen.fhir/package-ns])))
                             first))
                      inter-by-package)]
    (into {}
          (map (fn package-deps [[package-ns inter-package]]
                 {package-ns
                  (keep (fn find-package-deps [[dep-kw _dep-ver]]
                          (->> (vals packages)
                               (filter #(= (name dep-kw) (get-in % [:zen.fhir/package :name])))
                               first
                               :zen.fhir/package-ns))
                        (get-in inter-package [:zen.fhir/package :dependencies]))}))
          packages)))


(defn generate-root-package-nses [fhir-inter]
  (into {}
        (for [[package-ns package-nses] (ns-by-package fhir-inter)]
          {package-ns
           {'ns     package-ns
            'import (set (concat (get (packages-deps-nses fhir-inter) package-ns)
                                 package-nses))}})))


(defn generate-zen-schemas* [fhir-inter]
  (into (generate-root-package-nses fhir-inter)
        (for [[rt inters] fhir-inter
              inter       inters]
          (generate-zen-schema rt fhir-inter inter))))


(defn generate-zen-schemas [ztx]
  (swap! ztx assoc :fhir.zen/ns (generate-zen-schemas* (:fhir/inter @ztx)))
  :done)


(defn order-zen-ns [zen-ns-map]
  (let [zen-ns             (get zen-ns-map 'ns)
        zen-import         (get zen-ns-map 'import)
        rest-zen-ns-map    (dissoc zen-ns-map 'ns 'import)
        ordered-zen-ns-map (cond->> (sort-by key rest-zen-ns-map)
                             (some? zen-import) (cons ['import zen-import])
                             (some? zen-ns)     (cons ['ns zen-ns])
                             :always            flatten
                             :always            (apply array-map))]
    ordered-zen-ns-map))


(defn format-zen-ns [zen-ns-map]
  (clojure.pprint/write (order-zen-ns zen-ns-map) :stream nil))


(defn spit-zen-schemas [ztx zrc-dir & [{:keys [package]}]]
  (doseq [[zen-ns ns-content] (get-in @ztx [:fhir.zen/ns])
          :let [nss  (name zen-ns)
                file (str zrc-dir (str/replace nss #"\." "/") ".edn")
                package-name (first (str/split nss #"\." 2))]
          :when (or (nil? package) (= package package-name))]
    (clojure.java.io/make-parents file)
    (spit file (format-zen-ns ns-content)))
  :done)


(defn spit-ndjson-gz-bundle! [dir filename resources]
  (let [f    (clojure.java.io/file (str dir \/ filename ".ndjson.gz"))
        outs (java.util.zip.GZIPOutputStream. (clojure.java.io/output-stream f) true)]
    (with-open [w (java.io.BufferedWriter. (java.io.OutputStreamWriter. outs))]
      (doseq [resource resources]
        (.write w (cheshire.core/generate-string resource))
        (.write w "\n")
        (.flush w)))))


(defn spit-terminology-bundle [ztx package-dir {package-ns :package}]
  (let [fhir-inter (:fhir/inter @ztx)
        resources (->> (select-keys fhir-inter ["ValueSet" "Concept" "CodeSystem"])
                       vals
                       (mapcat vals))
        package-resources (map :zen.fhir/resource (filter #(= package-ns (name (:zen.fhir/package-ns %))) resources))]
    (spit-ndjson-gz-bundle! package-dir (str package-ns "-terminology-bundle") package-resources)) )


(defn spit-zen-modules [ztx zrc-dir & [package-name]]
  (let [packages (-> (->> (get-in @ztx [:fhir/inter "StructureDefinition"])
                          vals
                          (map (comp name :zen.fhir/package-ns))
                          distinct)
                     (cond->> (some? package-name) (filter #{(name package-name)})))]
    (doseq [package packages]
      (spit-zen-schemas ztx zrc-dir {:package package})
      (spit-terminology-bundle ztx zrc-dir {:package package}))
    :done))


(defn spit-zen-npm-modules [ztx zrc-node-modules-dir ver & [package-name]]
  (let [packages (-> (->> (get-in @ztx [:fhir/inter "StructureDefinition"])
                          vals
                          (map (comp name :zen.fhir/package-ns))
                          distinct)
                     (cond->> (some? package-name) (filter #{(name package-name)})))
        packages-deps (packages-deps-nses (:fhir/inter @ztx))]
    (doseq [package packages
            :let [package-dir (str zrc-node-modules-dir \/ package \/)
                  package-file-path (str package-dir "/package.json")
                  package-deps (into {}
                                     (map #(-> {(str "@zen-lang/" %) ver}))
                                     (get packages-deps (symbol package)))
                  package-file {:name    (str "@zen-lang/" package)
                                :version ver
                                :author  "Health-Samurai" ;; TODO: parameterize this
                                :license "MIT"
                                :dependencies package-deps}]]

      (spit-zen-schemas ztx package-dir {:package package})
      (spit-terminology-bundle ztx package-dir {:package package})
      (spit package-file-path (json/generate-string package-file {:pretty true})))
    :done))

;; * resources, types
;;  -> sd (+deps => ctx)
;;    ->  elements
;;      -> element-> zen -> transformed element
;;      -> element -> resolve to symbols
;;      -> aggregate into zen tree - temp keys


;; npm
;; -> temp storage
;;    by resourceType
;;    SD{ulr {res} } => ctx

;; res
;;   -> shaping
;;   1. treeify
;;   2. vectorize (tree algorythm)
