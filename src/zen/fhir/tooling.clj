(ns zen.fhir.tooling
  (:gen-class)
  (:require [zen.core :as zen-core]
            [zen.fhir.core :refer [load-all]]
            [zen.fhir.generator :refer [spit-zen-npm-modules
                                        spit-zen-modules
                                        generate-zen-schemas
                                        packages-deps-nses]]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]))


(defn spit-aidbox-standalone-projects [node-modules-folder zrc-dir]
  (let [ztx (zen-core/new-context {})]
    (load-all ztx nil {:node-modules-folder node-modules-folder})
    (generate-zen-schemas ztx)
    (let [packages-deps (packages-deps-nses (:fhir/inter @ztx))]
      (doseq [[package-name deps] packages-deps
              :let [standalone-dir (str zrc-dir "/" package-name)]]
        (doseq [package (cons package-name deps)]
          (spit-zen-modules ztx standalone-dir package))
        (spit (format "%s/%s-aidbox-project.edn" standalone-dir package-name)
              (pr-str {'ns (str package-name "-aidbox-project")
                       'import #{(symbol package-name)}}))
        (shell/sh "bash" "-c" (format "pwd && cd %s && tar -czvf %s.tar.gz %s && rm -rf %s" zrc-dir package-name package-name package-name))))
    (prn :done)))


(defn -main [node-modules-folder zrc-dir package-ver & [package-name]]
  (let [ztx (zen-core/new-context {})]
    (load-all ztx nil {#_#_:params {"hl7.fhir.r4.core"     {:zen.fhir/package-ns 'fhir-r4}
                                    "hl7.fhir.r3.core"     {:zen.fhir/package-ns 'fhir-r3}
                                    "hl7.fhir.us.core"     {:zen.fhir/package-ns 'us-core-v3}
                                    "hl7.fhir.us.mcode"    {:zen.fhir/package-ns 'mcode-v1}
                                    "hl7.fhir.us.carin-bb" {:zen.fhir/package-ns 'carin-bb-v1}}
                       :node-modules-folder node-modules-folder})
    (generate-zen-schemas ztx)
    (spit-zen-npm-modules ztx zrc-dir package-ver package-name)
    (prn :done)))
