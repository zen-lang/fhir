(ns zen.fhir.tooling.aidbox-standalone
  (:gen-class)
  (:require [zen.core :as zen-core]
            [zen.fhir.core :refer [load-all]]
            [zen.fhir.generator :refer [spit-zen-modules
                                        generate-zen-schemas
                                        packages-deps-nses]]
            [clojure.java.shell :as shell]))


(defn -main [node-modules-folder zrc-dir omit-deps?]
  (let [ztx (zen-core/new-context {})]
    (load-all ztx nil {:node-modules-folder node-modules-folder})
    (generate-zen-schemas ztx)
    (let [packages-deps (packages-deps-nses (:fhir/inter @ztx))]
      (doseq [[package-name deps] packages-deps
              :let [standalone-dir (str zrc-dir "/" package-name "/")]]
        (if omit-deps?
          (spit-zen-modules ztx standalone-dir package-name)
          (doseq [package (cons package-name deps)]
            (spit-zen-modules ztx standalone-dir package)))
        (shell/sh "bash" "-c" (str "cd " zrc-dir "/" package-name
                                   " && zip -r " package-name ".zip ."
                                   " && mv " package-name ".zip ../"
                                   " && cd .."
                                   " && rm -rf " package-name))))
    (prn :done)))

