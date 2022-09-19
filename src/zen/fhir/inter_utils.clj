(ns zen.fhir.inter-utils
  (:require [com.rpl.specter :as sp]))


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
