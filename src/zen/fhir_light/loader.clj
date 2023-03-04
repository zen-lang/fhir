(ns zen.fhir-light.loader
  (:require [zen.fhir-light.loader.group :as loader.group]
            [zen.fhir-light.loader.nest :as loader.nest]
            [zen.fhir-light.loader.to-zen :as loader.to-zen]))


(defn strdef->zen-ns [strdef]
  (let [grouped-strdef
        (loader.group/group-keys strdef loader.group/strdef-keys-types nil)

        nested-els
        (->> (get-in strdef [:differential :element])
             (map #(loader.group/group-keys %
                                            loader.group/elements-keys-types
                                            loader.group/elements-poly-keys-types))
             (map #(loader.group/enrich-loc grouped-strdef %))
             loader.nest/nest-by-enriched-loc)

        zen-nss
        (loader.to-zen/strdef->zen-nss grouped-strdef nested-els)

        zen-nss-map
        (into {}
              (map (juxt :ns identity))
              zen-nss)]
    zen-nss-map))
