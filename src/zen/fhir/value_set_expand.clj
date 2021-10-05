(ns zen.fhir.value-set-expand
  (:require [zen.core :as zen]
            [zen.fhir.value-set-expand]
            [cheshire.core]
            [clojure.java.io :as io]
            [fipp.edn]
            [clojure.string :as str]
            [zen.fhir.utils :as utils]
            [com.rpl.specter :as sp]))


(defn denormalize-value-sets [ztx]
  (let [value-sets (get-in @ztx [:fhir/inter "ValueSet"])
        concepts (get-in @ztx [:fhir/inter "Concept"])]
    #_(swap! ztx update-in
           [:fhir/inter "Concept"]
           #(sp/transform [sp/MAP-VALS]
                          (fn [concept] (assoc concept :valueset #{"http://hl7.org/fhir/ValueSet/administrative-gender"}))
                          %)))) ;; TODO
