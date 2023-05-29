(ns zen.fhir.igs-etl.nz
  (:require [zen.utils]
            [cheshire.core]
            [clojure.java.shell]
            [clojure.java.io :as io]))


(defn add-fhir-core-dependency-to-manifest! [path-to-manifest]
  (let [{:as parsed-manifest,
         :keys [fhirVersion]}
        (cheshire.core/parse-stream (io/reader path-to-manifest) keyword)

        manifest-with-specified-fhir-dep
        (update parsed-manifest :dependencies (fnil conj {}) ["hl7.fhir.r4.core" fhirVersion])

        updated-manifest
        (cheshire.core/generate-string manifest-with-specified-fhir-dep)]
    (spit path-to-manifest updated-manifest)
    {:manifest updated-manifest}))


(defn fhir-ig-site->fhir-package!
  "Should be used only when `package.tgz`, describing IG, is not
  available.

  `ig-path` is a path to a `site` folder, contained in the usual
  distributions of zipped IGs."
  [{:keys [ig-path package-dir-dest]}]
  (let [package-manifest (cheshire.core/parse-string
                           (slurp (io/file ig-path "/package.manifest.json"))
                           keyword)
        package-name     (:name package-manifest)
        _                (assert (some? package-name) "Must have package name")
        package-dest     (.getPath (io/file package-dir-dest package-name))]
    [(clojure.java.shell/sh "mkdir" "-p" package-dest)
     (clojure.java.shell/sh "sh" "-c" (str "cp -r " ig-path "/*.json " package-dest))
     (clojure.java.shell/sh "rm" "canonicals.json" "expansions.json" "usage-stats.json"
                            :dir package-dest)
     (clojure.java.shell/sh "mv" "package.manifest.json" "package.json"
                            :dir package-dest)
     (add-fhir-core-dependency-to-manifest! (io/file package-dest "package.json"))]))


(defn fhir-ig->fhir-package!
  "`ig-path` is a path to a `package` folder, which is unpacked from
  `site/package.tgz` contained in the usual distributions of zipped
  IGs."
  [{:keys [ig-path package-dir-dest]}]
  (let [package-manifest (cheshire.core/parse-string
                          (slurp (io/file ig-path "package.json"))
                          keyword)
        package-name     (:name package-manifest)
        _                (assert (some? package-name) "Must have package name")
        package-dest     (.getPath (io/file package-dir-dest package-name))]
    [(clojure.java.shell/sh "mkdir" "-p" package-dest)
     (clojure.java.shell/sh "sh" "-c" (str "cp -r " ig-path "/*.json " package-dest))
     (clojure.java.shell/sh "rm" "canonicals.json" "expansions.json" "usage-stats.json"
                            :dir package-dest)]))


(def igs [{:url      "https://fhir.org.nz/ig/base/full-ig.zip"
           :dir "zen.fhir.igs-etl.nz-base"}
          {:url "https://nhi-ig.hip-uat.digital.health.nz/full-ig.zip"
           :dir "zen.fhir.igs-etl.nz-nhi"}
          {:url "https://hpi-ig.hip-uat.digital.health.nz/full-ig.zip"
           :dir "zen.fhir.igs-etl.nz-hpi"}
          {:url "http://build.fhir.org/ig/HL7NZ/mdr/full-ig.zip"
           :dir "zen.fhir.igs-etl.nz-mdr"}])


(defn etl! [{:keys [package-dir-dest]}]
  (let [work-dir-path "/tmp"
        ig-dir "ig"]
    (doseq [{:keys [url dir]} igs
            :let [download-path (.getPath (io/file work-dir-path dir))
                  _ (println "Cleanup" (.getPath (io/file work-dir-path dir)) (:out (clojure.java.shell/sh "rm" "-rf" (.getPath (io/file work-dir-path dir)))))
                  ig-work-dir-path (.getPath (io/file download-path "site"))]]
      (zen.utils/unzip! url download-path)
      (let [package-archive (io/file ig-work-dir-path "package.tgz")
            _ (println ig-work-dir-path ":" (:out (clojure.java.shell/sh "md5sum" (str ig-work-dir-path \/ "package.tgz"))))]
        (if (.exists package-archive)
          (do
            (clojure.java.shell/sh "tar" "-xzf" (.getPath package-archive) "-C" ig-work-dir-path)
            (fhir-ig->fhir-package! {:ig-path
                                     (.getPath (io/file ig-work-dir-path "package"))
                                     :package-dir-dest
                                     package-dir-dest}))
          (fhir-ig-site->fhir-package! {:ig-path
                                        ig-work-dir-path
                                        :package-dir-dest
                                        package-dir-dest}))))))


(comment
  ;; clojure -X zen.fhir.igs-etl.nz/etl! :package-dir-dest '"/tmp/node_modules"'

  (def ig-zip-url "https://fhir.org.nz/ig/base/full-ig.zip")

  (def dest "/tmp/zen.fhir.igs-etl.nz/")

  (def ig-dir "ig/")
  (def ig-path (str dest \/ ig-dir))

  (def fhir-package-dir-path (str dest \/ "fhir-package"))

  (clojure.java.shell/sh "mkdir" "-p" dest)
  (zen.utils/unzip! ig-zip-url ig-path)
  (fhir-ig->fhir-package! ig-path fhir-package-dir-path)

  nil)
