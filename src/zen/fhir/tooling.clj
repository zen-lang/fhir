(ns zen.fhir.tooling
  (:require [zen.core :as zen-core]
            [zen.fhir.core :refer [load-all]]
            [zen.fhir.generator :refer [spit-zen-npm-modules
                                        generate-zen-schemas]]
            [clojure.java.io :as io]))


(defn -main [node-modules-folder zrc-dir]
  (let [ztx (zen-core/new-context {})]
    (load-all ztx nil {:params {"hl7.fhir.r4.core" {:zen.fhir/package-ns 'fhir-r4}
                                "hl7.fhir.us.core" {:zen.fhir/package-ns 'us-core-v3}
                                "hl7.fhir.us.carin-bb" {:zen.fhir/package-ns 'carin-bb-v1}}
                       :node-modules-folder node-modules-folder})
    (generate-zen-schemas ztx)
    (spit-zen-npm-modules ztx zrc-dir))
  (prn :done))
