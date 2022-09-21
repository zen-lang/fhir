(ns zen.fhir.tooling-packages
  (:gen-class)
  (:require [zen.core]
            [zen.fhir.core]
            [zen.fhir.generator]))


#_(defn -main [node-modules-folder out-dir & [package-name zen-fhir-path]]
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
