(ns zen.fhir.tooling.cli
  (:gen-class)
  (:require [cli-matic.core :refer [run-cmd]]
            [cli-matic.utils :as u]
            [zen.fhir.tooling.conceptmap-to-ndjson]
            [zen.fhir.tooling.aidbox-standalone]
            [zen.fhir.tooling-packages]
            [zen.fhir.tooling]))

(defmacro cli-output [fn-form]
  `(try  ~fn-form
         (println (str "\u001B[32m" "Done!" "\u001B[0m"))
         (catch Exception e# (println (str "\u001B[31m" "Caught exception: " (.getMessage e#) "\u001B[0m")))))

(def cli-configuration
  {:command      "zenfhir"
   :description  "Test desc"
   :version      "0.0.1"
   :opts         []
   :subcommands  [{:command "zenbnd"
                   :description "Builds zen project from provided IG"
                   :opts [{:option "input" :short "i"
                           :as "Path to node-modules-folder"
                           :type :string}
                          {:option "output" :short "o"
                           :as "Path to resulting zip archive"
                           :type :string}
                          {:option "version" :short "v"
                           :as "Resulting package version"
                           :type :string}
                          {:option "name" :short "n"
                           :as "Resulting package name (optional)"
                           :type :string}]
                   :runs (fn [{:keys [input output version name]}]
                           (cli-output (zen.fhir.tooling/-main
                                        input output version name)))}
                  {:command "stndlp"
                   :description "Builds standalone Aidbox zen project"
                   :opts [{:option "input" :short "i"
                           :as "Path to node-modules-folder"
                           :type :string}
                          {:option "output" :short "o"
                           :as "Path to resulting zip archive"
                           :type :string}
                          {:option "omit-deps"
                           :as "Remove deps from resulting project"
                           :type :with-flag}]
                   :runs (fn [{:keys [input output omit-deps]}]
                           (cli-output (zen.fhir.tooling.aidbox-standalone/-main
                                        input output omit-deps)))}
                  {:command     "cmndj"
                   :description "Converts ConceptMap to .ndjson.gz bundle"
                   :opts [{:option "input" :short "i"
                           :as "Path to conceptmap json file"
                           :type :string}
                          {:option "output" :short "o"
                           :as "Path to resulting ndjson.gz file"
                           :type :string}]
                   :runs (fn [{:keys [input output]}]
                           (cli-output (zen.fhir.tooling.conceptmap-to-ndjson/-main
                                        input output)))}
                  {:command "ig-to-zenpackage"
                   :opts    [{:option "modules" :type :string :short "m" :as "Node modules folder"}
                             {:option "output"  :type :string :short "o" :as "Output directory"}]
                   :runs    (fn [args]
                              (cli-output (zen.fhir.tooling-packages/build
                                           (:modules args)
                                           (:output args))))}]})

(defn -main
  [& args]
  (run-cmd args cli-configuration))
