(ns zen.fhir.tooling.terminology
  (:gen-class)
  (:require [zen.fhir.loinc]
            [zen.fhir.icd10]))


(defn -main []
  (zen.fhir.loinc/write-ndjson-gz-zip-bundle "loinc-terminology-bundle.zip")
  (zen.fhir.icd10/write-ndjson-gz-zip-bundle "icd-10-terminology-bundle.zip"))
