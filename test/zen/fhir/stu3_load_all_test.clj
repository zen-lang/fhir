(ns zen.fhir.stu3-load-all-test
  (:require [clojure.test :as t]
            [zen.fhir.generator :as gen]
            [matcho.core :as matcho]
            [zen.core]
            [zen.fhir.core :as fhir]
            [clojure.java.io :as io]))

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

(def blacklist
  {"StructureDefinition"
   #{"http://hl7.org/fhir/StructureDefinition/allergyintolerance-substanceExposureRisk"
     "http://hl7.org/fhir/StructureDefinition/cqif-measureInfo"
     "http://hl7.org/fhir/StructureDefinition/cqif-questionnaire"
     "http://hl7.org/fhir/StructureDefinition/diagnosticreport-genetics"
     "http://hl7.org/fhir/StructureDefinition/elementdefinition-de"
     "http://hl7.org/fhir/StructureDefinition/familymemberhistory-genetic"
     "http://hl7.org/fhir/StructureDefinition/observation-genetics"
     "http://hl7.org/fhir/StructureDefinition/patient-clinicalTrial"
     "http://hl7.org/fhir/StructureDefinition/procedurerequest-genetics"
     "http://hl7.org/fhir/StructureDefinition/procedurerequest-geneticsItem"}})

(t/deftest ^:kaocha/pending stu3-load-all-test
  (fhir/load-all ztx nil {:node-modules-folder "r3/node_modules"
                          :blacklist blacklist})

  (gen/generate-zen-schemas ztx)

  (:fhir/inter @ztx)

  (gen/spit-zen-npm-modules ztx "tmp" "2.0.0")


  )
