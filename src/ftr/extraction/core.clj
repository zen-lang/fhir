(ns ftr.extraction.core
  (:require [ftr.extraction.flat-table :as flat-table]))

(defn extract [cfg]
  (let [{:keys [source-type]} cfg]
    (condp = source-type
      :csv (flat-table/import-from-cfg cfg))))
