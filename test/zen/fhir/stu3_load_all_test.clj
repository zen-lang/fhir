(ns zen.fhir.stu3-load-all-test
  (:require [clojure.java.io :as io]
            [clojure.test :as t]
            [matcho.core :as matcho]
            [zen.core]
            [zen.fhir.core :as fhir]))

(defonce ztx (zen.core/new-context {}))

(reset! ztx @(zen.core/new-context {}))

(def deps
  {"ValueSet"
   #{"http://hl7.org/fhir/ValueSet/administrative-gender"
     "http://hl7.org/fhir/us/mcode/ValueSet/mcode-cancer-staging-system-vs"
     "http://hl7.org/fhir/ValueSet/link-type"
     "http://hl7.org/fhir/ValueSet/use-context"
     "http://hl7.org/fhir/ValueSet/practitioner-specialty"
     "http://hl7.org/fhir/ValueSet/inactive"}
   "CodeSystem"
   #{"http://hl7.org/fhir/link-type"
     "http://hl7.org/fhir/practitioner-specialty"
     "http://terminology.hl7.org/CodeSystem/v3-ActMood"
     "http://hl7.org/fhir/administrative-gender"}})

(t/deftest ^:kaocha/pending stu3-load-all-test

  (fhir/load-all ztx "hl7.fhir.r3.core" {:node-modules-folder "r3/node_modules"})

  )
