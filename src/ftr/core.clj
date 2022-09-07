(ns ftr.core
  (:require [ftr.extraction.core]
            [ftr.utils.core]
            [clojure.java.io :as io]
            [cheshire.core]
            [cheshire.core :as json]))


(defn generate-ndjson-row [obj]
  (format "%s\n" (cheshire.core/generate-string (into (sorted-map) obj))))


;; TODO:
(defn spit-terminology-file [cfg]
  (let [{:keys [value-set code-system concepts]}
        (ftr.extraction.core/extract cfg)

        ftr-dir
        (ftr.utils.core/create-dir-if-not-exists! (:ftr-path cfg))

        ftr-dir-abs-path
        (.getAbsolutePath ftr-dir)

        sorted-concepts
        (sort-by #(format "%s-%s" (:system :%) (:code %)) concepts)

        output-tf-file-path
        (str ftr-dir-abs-path \/ (ftr.utils.core/gen-uuid))

        {:keys [writer file digest]}
        (ftr.utils.core/make-sha256-gzip-writer output-tf-file-path)]

    (with-open [w writer]
      (.write w (generate-ndjson-row code-system))
      (.write w (generate-ndjson-row value-set))
      (doseq [c sorted-concepts]
        (.write w (generate-ndjson-row c))))

    (let [sha256 (digest)]
      (.renameTo file
                 (io/file (format "%s/tf.%s.ndjson.gz" (.getParent file) sha256))))))

;; TODO
;; this is raw draft
(defn create-ftr-module-layout [cfg]
  (let [module-path (format "%s/%s" (:ftr-path cfg) (:module cfg))
        tags-path (format "%s/tags" module-path)
        vs-dir-path (format "%s/vs" module-path)]
  (ftr.utils.core/create-dir-if-not-exists! tags-path)
  (ftr.utils.core/create-dir-if-not-exists! vs-dir-path)))

;; TODO
;; ftr pipeline:
;; - check repository layout or create
;; - create tf main file
;; - create tf patch file
;; - create or update tag file
;; - create or update tag index file
(defn spit-ftr [cfg]
  (create-ftr-module-layout cfg)
  (spit-terminology-file cfg))


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

  (spit-terminology-file config)

  )
