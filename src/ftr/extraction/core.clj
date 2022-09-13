(ns ftr.extraction.core
  (:require [ftr.extraction.flat-table :as flat-table]
            [ftr.extraction.ig]))

(defn extract [cfg]
  (let [{:keys [source-type source-url extractor-options]} cfg
        extractor-cfg (assoc extractor-options :source-url source-url)]
    (condp = source-type
      :flat-table (flat-table/import-from-cfg extractor-cfg)
      :ig (ftr.extraction.ig/import-from-cfg extractor-cfg))))
