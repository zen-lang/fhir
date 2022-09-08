(ns ftr.core
  (:require [clojure.java.io :as io]
            [ftr.extraction.core]
            [ftr.writer.core]
            [ftr.utils.core]
            [ftr.utils.unifn.core :as u]))


(defmethod u/*fn ::create-ftr-module-layout [{:as _ctx, :keys [cfg]}]
  (let [module-path (format "%s/%s" (:ftr-path cfg) (:module cfg))
        tags-path (format "%s/tags" module-path)
        vs-dir-path (format "%s/vs" module-path)]
    (ftr.utils.core/create-dir-if-not-exists! tags-path)
    (ftr.utils.core/create-dir-if-not-exists! vs-dir-path)))


(defmethod u/*fn ::spit-terminology-file [{:as _ctx, :keys [cfg]}]
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
      (.write w (ftr.utils.core/generate-ndjson-row code-system))
      (.write w (ftr.utils.core/generate-ndjson-row value-set))
      (doseq [c sorted-concepts]
        (.write w (ftr.utils.core/generate-ndjson-row c))))

    (let [sha256 (digest)]
      (.renameTo file
                 (io/file (format "%s/tf.%s.ndjson.gz" (.getParent file) sha256))))))


(defmethod u/*fn ::extract-terminology [{:as _ctx, :keys [cfg]}]
  {:extraction-result (ftr.extraction.core/extract cfg)})


(defmethod u/*fn ::write-terminology-file [ctx]
  {:write-result (ftr.writer.core/write-terminology-file ctx)})


(defn apply-cfg [cfg]
  (u/*apply [] cfg))
