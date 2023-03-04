(ns zen.fhir-light.loader
  (:require [zen.fhir-light.loader.group :as loader.group]
            [zen.fhir-light.loader.nest :as loader.nest]
            [zen.fhir-light.loader.parse-id :as loader.parse-id]
            [zen.fhir-light.loader.to-zen :as loader.to-zen]))


(defn strdef->zen-ns [strdef]
  (let [grouped-strdef (loader.group/group-strdef strdef)

        nested-els (->> (get-in strdef [:differential :element])
                        loader.group/group-elements
                        (map #(loader.parse-id/enrich-loc grouped-strdef %))
                        loader.nest/nest-by-enriched-loc)

        zen-nss (loader.to-zen/strdef->zen-nss grouped-strdef nested-els)

        zen-nss-map (into {} (map (juxt :ns identity)) zen-nss)]
    zen-nss-map))
