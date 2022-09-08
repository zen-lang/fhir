(ns ftr.writer.flat-table
  (:require [clojure.java.io :as io]
            [ftr.writer.core]
            [ftr.utils.core]))


(defn create-temp-tf-path [ftr-path]
  (let [ftr-dir (ftr.utils.core/create-dir-if-not-exists! ftr-path)]
    (format "%s/%s"
            (.getAbsolutePath ftr-dir)
            (ftr.utils.core/gen-uuid))))


(defmethod ftr.writer.core/write-terminology-file :flat-table
  [{:as _ctx,
    {:keys [ftr-path]} :cfg
    {:keys [value-set code-system concepts]} :extraction-result}]
  (let [temp-tf-path
        (create-temp-tf-path ftr-path)

        {:keys [writer file digest]}
        (ftr.utils.core/make-sha256-gzip-writer temp-tf-path)

        sorted-concepts
        (sort-by #(format "%s-%s" (:system :%) (:code %)) concepts)]
    (with-open [w writer]
      (.write w (ftr.utils.core/generate-ndjson-row code-system))
      (.write w (ftr.utils.core/generate-ndjson-row value-set))
      (doseq [c sorted-concepts]
        (.write w (ftr.utils.core/generate-ndjson-row c))))

    (let [sha256 (digest)]
      (.renameTo file
                 (io/file (format "%s/tf.%s.ndjson.gz" (.getParent file) sha256)))
      {:value-set value-set
       :code-system code-system
       :tf-path temp-tf-path
       :tf-sha256 sha256})))
