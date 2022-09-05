(ns ftr.core
  (:require [ftr.extraction.core]
            [clojure.java.io :as io]
            [cheshire.core]
            [cheshire.core :as json]))

(defn create-ftr-dir-if-not-exists!
  [path]
  (let [file (io/file path)]
    (if (.exists file)
      file
      (do (.mkdirs file)
          file))))

(defn spit-ftr [cfg]
  (let [{:keys [value-set code-system concepts]}
        (ftr.extraction.core/extract cfg)
        ftr-dir (create-ftr-dir-if-not-exists! (:ftr-path cfg))
        sorted-concepts (sort-by #(format "%s-%s" (:system :%) (:code %)) concepts)
        newtf (str (.getAbsolutePath ftr-dir) '/ "newtf.ndjson")]

    (spit newtf (str (cheshire.core/generate-string (into (sorted-map) code-system)) \newline) :append true)
    (spit newtf (str (cheshire.core/generate-string (into (sorted-map) value-set)) \newline) :append true)
    (doseq [c sorted-concepts]
      (spit newtf (str (cheshire.core/generate-string c) \newline) :append true))))

(comment
  (def config {:source-url "https://storage.googleapis.com/aidbox-public/documentation/icd10_example_no_header.csv"
               :ftr-path "/tmp/myftr"
               :source-type :csv

               :format      "csv"
               :csv-format  {:delimiter ";"
                             :quote "'"}

               :header   false
               :data-row 0
               :mapping  {:concept {:code    {:column 2}
                                    :display {:column 3}}}

               :code-system {:id "icd10", :url "http://hl7.org/fhir/sid/icd-10"}
               :value-set   {:id "icd10", :url "http://hl7.org/fhir/ValueSet/icd-10"}})

  (spit-ftr config)

  )
