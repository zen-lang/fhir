(ns ftr.writer.flat-table
  (:require [clojure.java.io :as io]
            [ftr.utils.core]))


(defn create-temp-tf-path! [ftr-path]
  (let [ftr-dir (ftr.utils.core/create-dir-if-not-exists! ftr-path)]
    (format "%s/%s"
            (.getAbsolutePath ftr-dir)
            (ftr.utils.core/gen-uuid))))


(defn spit-tf-file! [writer cs vs c]
  (let [sorted-concepts (sort-by #(format "%s-%s" (:system :%) (:code %)) c)]
    (with-open [w writer]
      (.write w (ftr.utils.core/generate-ndjson-row cs))
      (.write w (ftr.utils.core/generate-ndjson-row vs))
      (doseq [c sorted-concepts]
        (.write w (ftr.utils.core/generate-ndjson-row c))))))


(defn write-terminology-file
  [{:as _ctx,
    {:keys [ftr-path]} :cfg
    {:keys [value-set code-system concepts]} :extraction-result}]
  (let [{:keys [writer file digest]}
        (-> ftr-path
            create-temp-tf-path!
            ftr.utils.core/make-sha256-gzip-writer)

        _ (spit-tf-file! writer code-system value-set concepts)

        sha256
        (digest)

        renamed-file (io/file (format "%s/tf.%s.ndjson.gz" (.getParent file) sha256))]

    (.renameTo file
               renamed-file)

    {:value-set value-set
     :code-system code-system
     :terminology-file renamed-file
     :tf-sha256 sha256}))
