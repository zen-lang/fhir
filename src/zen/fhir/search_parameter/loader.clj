(ns zen.fhir.search-parameter.loader
  (:require [zen.fhir.search-parameter.template :as template]
            [zen.fhir.search-parameter.fhirpath :as fhirpath]

            [com.rpl.specter :as sp]))

(defn process-search-parameter [ztx inter]
  (-> inter
      (dissoc :expression)
      (assoc :expr
             (let [knife (fhirpath/fhirpath->knife (:expression inter))]
               (into {}
                     (map (fn [base-rt]
                            (let [knife (get knife base-rt)
                                  jsonpath (fhirpath/knife->jsonpath knife)
                                  sp-template (keyword (:type inter))]
                              {(keyword base-rt)
                               {:knife    knife
                                :jsonpath jsonpath
                                :template sp-template
                                :sql (template/expand sp-template jsonpath)}})))
                     (:base inter))))))

(defn process-search-parameters [ztx]
  (swap! ztx update-in [:fhir/inter "SearchParameter"]
         #(sp/transform [sp/MAP-VALS]
                        (partial process-search-parameter ztx)
                        %)))
