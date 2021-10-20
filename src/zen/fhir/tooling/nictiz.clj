(ns zen.fhir.tooling.nictiz
  (:gen-class)
  (:require [zen.core :as zen-core]
            [zen.fhir.nictiz :refer [load-all]]
            [zen.fhir.generator :refer [spit-zen-npm-modules
                                        generate-zen-schemas]]))


(defn -main [node-modules-folder zrc-dir package-ver & [package-name]]
  (let [ztx (zen-core/new-context {})]
    (load-all ztx nil {:node-modules-folder node-modules-folder})
    (generate-zen-schemas ztx)
    (spit-zen-npm-modules ztx zrc-dir package-ver package-name)
    (prn :done)))
