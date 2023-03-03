(ns zen.fhir-light.loader
  (:require [zen.fhir-light.loader.group :as loader.group]
            [zen.fhir-light.loader.nest :as loader.nest]
            [zen.fhir-light.loader.to-zen :as loader.to-zen]))


(defn strdef->zen-ns [strdef]
  (let [grouped-strdef
        (loader.group/group-keys strdef loader.group/strdef-keys-types nil)

        ctx {:zf/strdef grouped-strdef}

        nested-els
        (->> (get-in strdef [:differential :element])
             (map #(loader.group/group-keys %
                                            loader.group/elements-keys-types
                                            loader.group/elements-poly-keys-types))
             (map #(loader.group/enrich-loc ctx %))
             loader.nest/nest-by-enriched-loc)


        this-binding (loader.to-zen/mk-binding grouped-strdef)

        zen-res (loader.to-zen/nested->zen ctx nested-els)

        zen-res (-> zen-res
                    (update :zf/schema
                            merge
                            {:zen/tags #{'zen/schema}
                             :zen/binding (:zf/sym this-binding)})
                    (update :zf/bindings
                            merge
                            (:zf/binding this-binding)))

        bindings-ns (loader.to-zen/symbols-map->zen-nss (:zf/bindings zen-res))]

    (assoc zen-res :zf/bindings-ns bindings-ns)))
