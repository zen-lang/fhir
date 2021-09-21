(ns zen.fhir.tooling.aidbox-standalone
  (:gen-class)
  (:require [zen.core :as zen-core]
            [zen.fhir.core :refer [load-all]]
            [zen.fhir.generator :refer [spit-zen-modules
                                        generate-zen-schemas
                                        packages-deps-nses]]
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
