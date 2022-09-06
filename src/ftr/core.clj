(ns ftr.core
  (:require [ftr.extraction.core]
            [ftr.utils.core]
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
        ftr-dir-abs-path (.getAbsolutePath ftr-dir)
        sorted-concepts (sort-by #(format "%s-%s" (:system :%) (:code %)) concepts)
        terminology-file (io/file (str ftr-dir-abs-path '/ "tf.ndjson"))]
    (.delete terminology-file)
    (with-open [fw (io/writer terminology-file :append true)]
      (.write fw (str (cheshire.core/generate-string (into (sorted-map) code-system)) \newline))
      (.write fw (str (cheshire.core/generate-string (into (sorted-map) value-set)) \newline))
      (doseq [c sorted-concepts]
        (.write fw (str (cheshire.core/generate-string c) \newline))))
    (let [sha256 (ftr.utils.core/calculate-sha256 (.getAbsolutePath terminology-file))]
      (.renameTo terminology-file
                 (io/file (str ftr-dir-abs-path '/ "tf." sha256 ".ndjson"))))))

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
