(ns zen.fhir.igs-etl.nz-base
  (:require [zen.utils]
            [cheshire.core]
            [clojure.java.shell]))


(defn fhir-ig->fhir-package! [{:keys [ig-path package-dir-dest]}]
  (let [package-manifest (cheshire.core/parse-string
                           (slurp (str ig-path "site/package.manifest.json"))
                           keyword)
        package-name     (:name package-manifest)
        _                (assert (some? package-name) "Must have package name")
        package-dest     (str package-dir-dest "/" package-name)]
    [(clojure.java.shell/sh "mkdir" "-p" package-dest)
     (clojure.java.shell/sh "sh" "-c" (str "cp -r " ig-path "/site/*.json " package-dest))
     (clojure.java.shell/sh "rm" "canonicals.json" "expansions.json" "usage-stats.json"
                            :dir package-dest)
     (clojure.java.shell/sh "mv" "package.manifest.json" "package.json"
                            :dir package-dest)]))


(defn etl! [{:keys [package-dir-dest]}]
  (let [ig-zip-url      "https://fhir.org.nz/ig/base/full-ig.zip"
        tmp-ig-dir-dest "/tmp/zen.fhir.igs-etl.nz-base/"
        ig-dir          "ig/"
        ig-path         (str tmp-ig-dir-dest \/ ig-dir)]
    (clojure.java.shell/sh "mkdir" "-p" tmp-ig-dir-dest)
    (zen.utils/unzip! ig-zip-url ig-path)
    (fhir-ig->fhir-package! {:ig-path ig-path :package-dir-dest package-dir-dest})))


(comment
  ;; clojure -X zen.fhir.igs-etl.nz-base/etl! :package-dir-dest '"/tmp/node_modules"'

  (def ig-zip-url "https://fhir.org.nz/ig/base/full-ig.zip")

  (def dest "/tmp/zen.fhir.igs-etl.nz-base/")

  (def ig-dir "ig/")
  (def ig-path (str dest \/ ig-dir))

  (def fhir-package-dir-path (str dest \/ "fhir-package"))

  (clojure.java.shell/sh "mkdir" "-p" dest)
  (zen.utils/unzip! ig-zip-url ig-path)
  (fhir-ig->fhir-package! ig-path fhir-package-dir-path)

  nil)
