(ns zen.fhir.tooling-packages
  (:gen-class)
  (:require [zen.core :as zen-core]
            [zen.fhir.core :refer [load-all]]
            [zen.fhir.generator :refer [spit-zen-packages
                                        generate-zen-schemas]]))


(defn -main [node-modules-folder out-dir & [package-name zen-fhir-path]]
  (let [ztx (zen-core/new-context {})]
    (load-all ztx nil {#_#_:params {"hl7.fhir.r4.core"     {:zen.fhir/package-ns 'fhir-r4}
                                    "hl7.fhir.r3.core"     {:zen.fhir/package-ns 'fhir-r3}
                                    "hl7.fhir.us.core"     {:zen.fhir/package-ns 'us-core-v3}
                                    "hl7.fhir.us.mcode"    {:zen.fhir/package-ns 'mcode-v1}
                                    "hl7.fhir.us.carin-bb" {:zen.fhir/package-ns 'carin-bb-v1}}
                       :node-modules-folder node-modules-folder})
    (generate-zen-schemas ztx)
    (spit-zen-packages ztx {:out-dir out-dir
                            :package package-name
                            :git-url-format (str out-dir "/%s"#_? )
                            :zen-fhir-lib-url zen-fhir-path})
    (prn :done)))


;;
;;(sut/spit-zen-packages ztx {:out-dir          test-dir
;; :git-url-format   (str test-dir "/%s")
;; :zen-fhir-lib-url (str (System/getProperty "user.dir") "/zen.fhir/")})



(comment
  (let [ztx (zen-core/new-context {})]
    (load-all ztx nil {#_#_:params {"hl7.fhir.r4.core"     {:zen.fhir/package-ns 'fhir-r4}
                                    "hl7.fhir.r3.core"     {:zen.fhir/package-ns 'fhir-r3}
                                    "hl7.fhir.us.core"     {:zen.fhir/package-ns 'us-core-v3}
                                    "hl7.fhir.us.mcode"    {:zen.fhir/package-ns 'mcode-v1}
                                    "hl7.fhir.us.carin-bb" {:zen.fhir/package-ns 'carin-bb-v1}}
                       :node-modules-folder "/home/puck/Work/sansara/box/libs/fhir/r3/node_modules"
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
