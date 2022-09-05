(ns ftr.cli
  (:require [ftr.extraction.core]
            [cli-matic.core]
            [cli-matic.utils]
            [clojure.java.io :as io]))

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
                          (prn (ftr.extraction.core/extract cfg)))}]})

(comment
  (do
    (when-not (.exists (io/file "/tmp/ftr"))
      (.mkdir (io/file "/tmp/ftr/")))
    (spit "/tmp/ftr/abc"  "content"))
  nil)
