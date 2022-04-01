(ns zen.fhir.cli
  (:gen-class)
  (:require [cli-matic.core :refer [run-cmd]]
            [cli-matic.utils :as u]
            [zen.core :as zen-core]
            [clojure.java.io :as io]
            [edamame.core]
            [clojure.string :as str]))


(defn parse-edn [file]
  (println "FILE:" file)
  (try (-> file slurp edamame.core/parse-string)
       (catch Exception e (println e))))


(defn read-zen-project [ztx path]
  (let [file (io/file path)]
    (if (.isDirectory file)
      (->> (.listFiles file)
           (filter (fn [file]
                     (and (not= "node_modules" (.getName file))
                          (str/ends-with? (.getName file) ".edn"))))
           (mapv #(read-zen-project ztx (.getPath %))))
      (zen-core/load-ns ztx (parse-edn file))))) ;;Doesn't werks :(


(defn lint-project [path]
  (let [ztx (zen-core/new-context {})]
    (read-zen-project ztx path)
    ztx))


(comment
  (lint-project "/Users/ghrp/prog/zen-samples/tlon-core/zrc")

  )
