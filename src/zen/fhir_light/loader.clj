(ns zen.fhir-light.loader
  (:require [zen.fhir-light.loader.group :as loader.group]
            [zen.fhir-light.loader.nest :as loader.nest]
            [zen.fhir-light.loader.parse-id :as loader.parse-id]
            [zen.fhir-light.loader.to-zen :as loader.to-zen]))


#_"TODO: element.type.profile is just a url without version.
         Need to come up with bindings that will resolve version without collisions"
(defn strdef->zen-ns
  "Accepts a strdef with #{:url :name :fhirVersion :type :kind :differential}
   Returns {zen.fhir.bindings.fhir-<stu3|r4|r4b|r5>.<primitive|complex>-type
            {<type> <type binding>}

            zen.fhir.bindings.fhir-<stu3|r4|r4b|r5>.structure
            {<type> <structure binding>}

            zen.fhir.fhir-<stu3|r4|r4b|r5>.<kind>.<type>
            {schema <strdef-zen-schema that also binds to a structure or a type>}

            zen.fhir.profiles.<strdef.name>.v<version>
            {schema <strdef-zen-schema that also binds to a struture or a type>}}"
  [strdef]
  (let [grouped-strdef (loader.group/group-strdef strdef)

        nested-els (->> (get-in strdef [:differential :element])
                        loader.group/group-elements
                        (map #(loader.parse-id/enrich-loc grouped-strdef %))
                        loader.nest/nest-by-enriched-loc)

        zen-nss (loader.to-zen/strdef->zen-nss grouped-strdef nested-els)

        zen-nss-map (into {} (map (juxt :ns identity)) zen-nss)]
    zen-nss-map))
