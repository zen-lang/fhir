(ns zen.fhir.tooling
  (:gen-class)
  (:require [zen.core :as zen-core]
            [zen.fhir.core :refer [load-all]]
            [zen.fhir.generator :refer [spit-zen-npm-modules
                                        generate-zen-schemas]]
            [clojure.java.io :as io]))


(defn -main [node-modules-folder zrc-dir package-ver & [package-name]]
  (let [ztx (zen-core/new-context {})
        zen-fhir-lib-version (slurp (io/resource "zen-fhir-lib-version"))]
    (load-all ztx nil {#_#_:params {"hl7.fhir.r4.core"     {:zen.fhir/package-ns 'fhir-r4}
                                    "hl7.fhir.r3.core"     {:zen.fhir/package-ns 'fhir-r3}
                                    "hl7.fhir.us.core"     {:zen.fhir/package-ns 'us-core}
                                    "hl7.fhir.us.mcode"    {:zen.fhir/package-ns 'mcode-v1}
                                    "hl7.fhir.us.carin-bb" {:zen.fhir/package-ns 'carin-bb-v1}}
                       :node-modules-folder node-modules-folder})
    (generate-zen-schemas ztx)
    (spit-zen-npm-modules ztx zrc-dir package-ver package-name zen-fhir-lib-version)
    (prn :done)))


(comment
  (let [ztx (zen-core/new-context {})]
    (load-all ztx nil {:params {;"hl7.fhir.r4.core"     {:zen.fhir/package-ns 'fhir-r4}
                                "hl7.fhir.r4b.core"     {:zen.fhir/package-ns 'fhir-r4b}
                                ;"hl7.fhir.r3.core"     {:zen.fhir/package-ns 'fhir-r3}
                                ;"hl7.fhir.us.core"     {:zen.fhir/package-ns 'us-core}
                                ;"hl7.fhir.us.mcode"    {:zen.fhir/package-ns 'mcode-v1}
                                #_#_"hl7.fhir.us.carin-bb" {:zen.fhir/package-ns 'carin-bb-v1}}

                       :node-modules-folder "/home/hex/Development/Projects/fhir/r4b/hl7.fhir.r4b.core"
                       :blacklist  {"StructureDefinition"
                                    #{"http://hl7.org/fhir/StructureDefinition/allergyintolerance-substanceExposureRisk"
                                      "http://hl7.org/fhir/StructureDefinition/cqif-measureInfo"
                                      "http://hl7.org/fhir/StructureDefinition/cqif-questionnaire"
                                      "http://hl7.org/fhir/StructureDefinition/diagnosticreport-genetics"
                                      "http://hl7.org/fhir/StructureDefinition/elementdefinition-de"
                                      "http://hl7.org/fhir/StructureDefinition/familymemberhistory-genetic"
                                      "http://hl7.org/fhir/StructureDefinition/observation-genetics"
                                      "http://hl7.org/fhir/StructureDefinition/patient-clinicalTrial"
                                      "http://hl7.org/fhir/StructureDefinition/procedurerequest-genetics"
                                      "http://hl7.org/fhir/StructureDefinition/procedurerequest-geneticsItem"}}})
    (generate-zen-schemas ztx)
    (spit-zen-npm-modules ztx "/tmp/r3-zen/node_modules" "test")
    (prn :done))
  nil)
