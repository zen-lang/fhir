(ns build
  (:require [clojure.tools.build.api :as b]
            [clojure.string :as str]))


(def lib 'zen-lang/zen.fhir)

(def main-ns 'zen.fhir.tooling.cli)

(def tag #_(-> {:command-args ["git" "describe" "--tags"]
              :dir "./."
              :out :capture}
             b/process
             :out
             str/trim)
  (System/getenv "RELEASE_VERSION"))

(def class-dir "target/classes")

(def basis (b/create-basis {:project "deps.edn"}))

(def uber-file
  (format "target/%s-%s-standalone.jar"
          (str/replace (name lib) #"\." "-")
     tag))


(defn clean [_]
  (b/delete {:path "target"}))


(defn prep [_]
  (b/write-pom {:class-dir class-dir
                :lib       lib
                :version   tag
                :basis     basis
                :src-dirs  ["src"]})
  (b/copy-dir {:src-dirs ["src" "resources" "zrc"]
               :target-dir class-dir}))


(defn uber [_]
  (b/compile-clj {:basis     basis
                  :src-dirs  ["src"]
                  :class-dir class-dir})
  (b/uber {:class-dir class-dir
           :uber-file uber-file
           :basis     basis
           :main      main-ns}))


(defn all [_]
  (clean nil)
  (prep nil)
  (uber nil))
