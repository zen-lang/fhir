(ns zen.fhir.nictiz-test
  (:require [clojure.test :as t]
            [zen.fhir.generator :as gen]
            [matcho.core :as matcho]
            [zen.core]
            [zen.fhir.core :as c]
            [zen.fhir.nictiz :as nictiz]
            [clojure.java.io :as io]))

(defonce ztx (zen.core/new-context {}))

(reset! ztx @(zen.core/new-context {}))

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
  (nictiz/load-all ztx nil {:node-modules-folder "r3/node_modules"
                            :blacklist blacklist})

  (gen/generate-zen-schemas ztx)


  (:fhir/inter @ztx)



  (get-in @ztx [:fhir/inter :zen.fhir/package-ns])


  (gen/spit-zen-npm-modules ztx "/tmp/clj" "1.0.0")


  )
