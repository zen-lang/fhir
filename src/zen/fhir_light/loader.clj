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

        symbols
        (-> (:zf/bindings zen-res)
            (merge (:zf/binding this-binding))
            (assoc (loader.to-zen/strdef->sym grouped-strdef)
                   (assoc (:zf/schema zen-res)
                          :zen/tags #{'zen/schema}
                          :zen/binding (:zf/sym this-binding))))

        nss (loader.to-zen/symbols-map->zen-nss symbols)]

    (into {} (map (juxt :ns identity)) nss)))
