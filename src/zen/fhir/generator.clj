(ns zen.fhir.generator
  (:require [zen.fhir.utils :as utils]
            [com.rpl.specter :as sp]
            [clojure.string :as str]
            [cheshire.core :as json]))


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


(defn type-string->type-symbol [fhir-inter tp]
  (let [tp-url (str "http://hl7.org/fhir/StructureDefinition/" tp)]
    (when-let [tp-ns (get-in fhir-inter ["StructureDefinition" tp-url :zen.fhir/schema-ns])]
      (symbol (name tp-ns) "schema"))))


(defn ext-url->ext-symbol [fhir-inter ext-url]
  (when-let [ext-ns (get-in fhir-inter ["StructureDefinition" ext-url :zen.fhir/schema-ns])]
    (symbol (name ext-ns) "schema")))


(defn els-schema [fhir-inter [url inter-res]]
  (letfn [(el-schema [fhir-inter el]
            (let [sch (merge {}
                             (when-let [type-sym (some->> (:type el) (type-string->type-symbol fhir-inter))]
                               {:confirms #{type-sym}})
                             (when-let [ext-sym (some->> (:fhir/extension el) (ext-url->ext-symbol fhir-inter))]
                               {:confirms #{ext-sym}})
                             (when (seq (:| el))
                               (els-schema fhir-inter [url el]))
                             (when (seq (:fhir/flags el))
                               (select-keys el #{:fhir/flags})))]
              (if (:vector el)
                (merge {:type 'zen/vector
                        :every sch}
                       (select-keys el [:minItems :maxItems]))
                sch)))]
    {:type 'zen/map
     :keys (sp/transform [sp/MAP-VALS]
                         (partial el-schema fhir-inter)
                         (:| inter-res))}))


(defmethod generate-kind-schema :complex-type [fhir-inter [url inter-res]]
  (merge
    (confirms-base fhir-inter [url inter-res])
    (els-schema fhir-inter [url inter-res])))


(defmethod generate-kind-schema :resource [fhir-inter [url inter-res]]
  (merge
    (confirms-base fhir-inter [url inter-res])
    (els-schema fhir-inter [url inter-res])))


(defmethod generate-kind-schema :logical [fhir-inter [url inter-res]]
  (merge
    (confirms-base fhir-inter [url inter-res])
    (els-schema fhir-inter [url inter-res])))


(defn generate-zen-schema [fhir-inter [url inter-res]]
  (let [sd-inter    (get fhir-inter "StructureDefinition")
        schema-ns   (:zen.fhir/schema-ns inter-res)
        imports     (into #{'zenbox}
                          (keep (comp :zen.fhir/schema-ns sd-inter))
                          (keys (apply concat (vals (:deps inter-res)))))

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
                          (merge {:zen/tags (into #{'zen/schema}
                                                  (when severity-tag [severity-tag]))
                                  :zenbox/type (:type inter-res)
                                  :zenbox/profileUri url}
                                 schema-part))}}))


(defn generate-zen-schemas* [fhir-inter]
  (into {}
        (map (partial generate-zen-schema fhir-inter))
        (get fhir-inter "StructureDefinition")))



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


(defn spit-zen-npm-modules [ztx zrc-node-modules-dir ver & [package-name]]
  (let [packages (-> (->> (get-in @ztx [:fhir/inter "StructureDefinition"])
                          vals
                          (map (comp name :zen.fhir/package-ns))
                          distinct)
                     (cond->> (some? package-name) (filter #{(name package-name)})))]
    (doseq [package packages
            :let [package-dir (str zrc-node-modules-dir \/ package \/)
                  package-file-path (str package-dir "/package.json")
                  package-file {:name    (str "@zen-lang/" package)
                                :version ver
                                :author  "Health-Samurai" ;; TODO: parameterize this
                                :license "MIT"}]]
      (spit-zen-schemas ztx package-dir {:package package})
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
