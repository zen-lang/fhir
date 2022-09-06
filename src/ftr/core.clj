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
        sorted-concepts (sort-by #(format "%s-%s" (:system :%) (:code %)) concepts)
        terminology-file (io/file (str (.getAbsolutePath ftr-dir) '/ "tf.ndjson"))]
    (with-open )
    (spit (get-tf-path "newtf.ndjson") (str (cheshire.core/generate-string (into (sorted-map) code-system)) \newline) :append true)
    (spit (get-tf-path "newtf.ndjson") (str (cheshire.core/generate-string (into (sorted-map) value-set)) \newline) :append true)
    (doseq [c sorted-concepts]
      (spit (get-tf-path "newtf.ndjson") (str (cheshire.core/generate-string c) \newline) :append true))
    (let [sha256 (ftr.utils.core/calculate-sha256 newtf)])))

(comment

  (def f (io/file "/tmp/e"))

  (with-open (io/))

  (let [file-path "/tmp/e"
        sha256 (ftr.utils.core/calculate-sha256 file-path)]
    (.renameTo (io/file file-path)
               (io/file (clojure.string/join
                         "/"
                         (concat
                          (drop-last (clojure.string/split file-path #"/"))
                          (list (str "e" \. sha256 \. "ndjson")))))))

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

  (defn calculate-sha256 [source]
    (let [digest (java.security.MessageDigest/getInstance "SHA-256")]
      (with-open [input-stream  (io/input-stream source)
                  digest-stream (java.security.DigestInputStream. input-stream digest)
                  output-stream (io/output-stream "/dev/null")]
        (io/copy digest-stream output-stream)
        )
      (format "%032x" (BigInteger. 1 (.digest digest)))))

  (copy+sha256 "/tmp/x.nd")

  )
