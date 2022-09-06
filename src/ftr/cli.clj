(ns ftr.cli
  (:gen-class)
  (:require [ftr.core]
            [cli-matic.core]))

(defn parse-ftr-cfg [path]
  (try (read-string (slurp path))
       (catch Exception e {::error (.getMessage e)})))

(def cli-cfg
  {:command "ftr"
   :description "FTR"
   :version "0.0.1"
   :opts []
   :subcommands [{:command "generate"
                  :description "Generate FTR content"
                  :opts [{:option "cfg"
                          :as "Path to config file"
                          :type :string}]
                  :runs (fn [{:keys [cfg]}]
                          (let [{:as parsed-cfg, ::keys [error]}
                                (parse-ftr-cfg cfg)]
                            (if-not error
                              (ftr.core/spit-ftr parsed-cfg)
                              (println (str "\u001B[31m" error "\u001B[0m")))))}]})

(defn -main
  [& args]
  (cli-matic.core/run-cmd args cli-cfg))
