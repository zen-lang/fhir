(ns zen.fhir-light.loader
  (:require [zen.fhir-light.loader.group :as loader.group]
            [zen.fhir-light.loader.nest :as loader.nest]
            [zen.fhir-light.loader.to-zen :as loader.to-zen]
            [zen.v2-validation]))


(defn strdef->zen-ns [strdef]
  (let [grouped-strdef
        (loader.group/group-keys strdef loader.group/strdef-keys-types nil)

        nested-els
        (->> (get-in strdef [:differential :element])
             (map #(loader.group/group-keys %
                                            loader.group/elements-keys-types
                                            loader.group/elements-poly-keys-types))
             (map loader.group/enrich-loc)
             loader.nest/nest-by-enriched-loc)

        zen-res (loader.to-zen/nested->zen {:zf/strdef grouped-strdef} nested-els)

        bindings-ns (loader.to-zen/symbols-map->zen-nss (:zf/bindings zen-res))]

    (assoc zen-res :zf/bindings-ns bindings-ns)))
