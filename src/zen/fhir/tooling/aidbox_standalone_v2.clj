(ns zen.fhir.tooling.aidbox-standalone-v2
  (:gen-class)
  (:require [zen.core :as zen-core]
            [zen.fhir.core :refer [load-all]]
            [zen.fhir.generator :refer [spit-zen-modules
                                        generate-zen-schemas
                                        packages-deps-nses]]
            [zen.fhir.inter-utils]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [clojure.java.io :as io]))

(comment
  (require '[clj-async-profiler.core :as prof])

  (prof/profile
    #_{:event :alloc}
    (time
      (-main "node_modules2"
             "tmp-zrc2")))
  ;; without refactor "Elapsed time: 1740601.313208 msecs"
  ;; with refactor "Elapsed time: 518171.360916 msecs"
  ;; "Elapsed time: 296774.622458 msecs"

  ;; without refactor. old measures
  ;; With zen ns pprint Elapsed time: 66711.658208 msecs
  ;; Without pprint Elapsed time: 15276.263458 msecs
  ;; nlm vsac + r4 core + r4 terminology "Elapsed time: 1662008.051333 msecs"

  (time
    (prof/profile
      (-main "node_modules3"
             "tmp-zrc3")))

  ;; with refactor "Elapsed time: 3290.484459 msecs"
  ;; without refactor "Elapsed time: 5271.97525 msecs"
  (prof/list-event-types)

  (def srv (prof/serve-ui 8081)))


(defn coerce-to-internal-package-name [s]
  (-> s
      (str/replace "." "-")
      (str/lower-case)))

(defn -main [node-modules-folder zrc-dir & {:as opts}]
  (let [{:as opts, :strs [omit-deps? preserve-package alt-package-name rename-package]} opts
        ztx (zen-core/new-context {})]
    (load-all ztx nil {:node-modules-folder node-modules-folder})
    (generate-zen-schemas ztx)
    (let [packages-deps (packages-deps-nses (:fhir.zen/ns @ztx) (:fhir/inter @ztx))]
      (doseq [package-name (keys packages-deps)
              :when (or (nil? preserve-package)
                        (= package-name (symbol (coerce-to-internal-package-name preserve-package))))]
        (let [package-name package-name
              standalone-dir (str zrc-dir "/" package-name "/")
              rename-package (when rename-package (read-string (slurp "resources/rename-package.edn")))
              alt-package-name (or (get rename-package package-name) alt-package-name package-name)]
          (if omit-deps?
            (spit-zen-modules ztx standalone-dir package-name)
            (doseq [package (cons package-name (zen.fhir.inter-utils/get-all-deps package-name packages-deps))]
              (spit-zen-modules ztx standalone-dir package)))
          (shell/sh "bash" "-c" (str "cd " zrc-dir "/" package-name
                                     " && zip -r " alt-package-name ".zip ."
                                     " && mv " alt-package-name ".zip ../"
                                     " && cd .."
                                     " && rm -rf " package-name)))))
    (prn :done)))
