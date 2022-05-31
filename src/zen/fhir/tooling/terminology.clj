(ns zen.fhir.tooling.terminology
  (:gen-class)
  (:require [zen.fhir.loinc]
            [zen.fhir.icd10]
            [zen.fhir.loinc2]))


(defn -main []
  (zen.fhir.loinc2/write-ndjson-gz-zip-bundle "sdc-loinc-terminology-bundle.zip")
  (zen.fhir.icd10/write-ndjson-gz-zip-bundle "icd-10-terminology-bundle.zip"))
