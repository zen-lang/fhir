(ns zen.fhir.generator
  (:require [zen.fhir.utils :as utils]))


(defn generate-zen-schema [fhir-inter [url inter-res]]
  (let [sd-inter    (get fhir-inter "StructureDefinition")
        schema-ns   (:zen.fhir/schema-ns inter-res)
        imports     (into #{}
                          (keep (comp :zen.fhir/schema-ns sd-inter))
                          (keys (apply concat (vals (:deps inter-res)))))
        base-schema (some-> (get-in sd-inter [(:baseDefinition inter-res) :zen.fhir/schema-ns])
                            name
                            (symbol "schema"))]
    {schema-ns {'ns     schema-ns
                'import imports
                'schema (utils/strip-nils
                          {:zen/tags #{'zen/schema}
                           :confirms (when base-schema #{base-schema})})}}))


(defn generate-zen-schemas* [fhir-inter]
  (into {}
        (map (partial generate-zen-schema fhir-inter))
        (get fhir-inter "StructureDefinition")))



(defn generate-zen-schemas [ztx]
  (swap! ztx assoc :fhir.zen/ns (generate-zen-schemas* (:fhir/inter @ztx)))
  :done)



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
