(ns zen.fhir.tooling.conceptmap-to-ndjson
  (:gen-class)
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]))

(defn write-line [w x]
  (.write w (cheshire.core/generate-string x))
  (.write w "\n"))

(defn conceptmap-to-ndjson-gz [path-to-conceptmap output-path]
  (let [parsed-conceptmap (json/parse-string (slurp path-to-conceptmap) keyword)
        conceptmap' (dissoc parsed-conceptmap :group)
        maprules (mapcat (fn [group] ;;NOTE: Ungroup group by element
                           (reduce (fn [acc element]
                                     (conj acc (-> group (assoc :element element
                                                                :url (:url conceptmap')
                                                                :resourceType "MapRule"))))
                                   [] (:element group)))
                         (:group parsed-conceptmap))]
    (with-open [gzip (-> output-path
                         (io/output-stream)
                         (java.util.zip.GZIPOutputStream. true)
                         (java.io.OutputStreamWriter.)
                         (java.io.BufferedWriter.))]
      (write-line gzip conceptmap')
      (doseq [m maprules]
        (write-line gzip m)))))

(defn -main [path-to-conceptmap output-path]
  (conceptmap-to-ndjson-gz path-to-conceptmap output-path))
