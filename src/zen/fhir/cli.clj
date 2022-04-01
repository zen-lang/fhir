(ns zen.fhir.cli
  (:gen-class)
  (:require [cli-matic.core :refer [run-cmd]]
            [cli-matic.utils :as u]
            [zen.core :as zen-core]
            [clojure.java.io :as io]
            [edamame.core]
            [clojure.string :as str]))

(defn parse-edn [file]
  (try (-> file slurp edamame.core/parse-string)
       (catch Exception e (println e))))


(defn read-files-recursive [ztx file]
  (if (.isDirectory file)
    (->> (.listFiles file)
         (filter (fn [file]
                   (or (and (.isDirectory file) (not= "node_modules" (.getName file)))
                       (str/ends-with? (.getName file) ".edn"))))
         (mapv #(read-files-recursive ztx %)))
    (do 
      (println :load file)
      (zen-core/load-ns ztx (parse-edn file)))))

(defn read-zen-project [ztx path]
  (let [file (io/file path)]
    (read-files-recursive ztx file)))


(defn lint-project [path]
  (let [ztx (zen-core/new-context {:paths [path]})]
    (read-zen-project ztx path)
    (zen.core/errors ztx)))


(comment
  (lint-project "/Users/niquola/zen-samples/tlon-core/zrc")

  (def path "/Users/niquola/zen-samples/tlon-core/zrc")

  (def ztx (zen-core/new-context {:paths [path]}))

  (zen.core/read-ns ztx 'tlon.core)
  (zen.core/read-ns ztx 'tlon.core.patient)
  (zen.core/read-ns ztx 'tlon.core.patient)

  (zen.core/errors ztx)
  

  )
