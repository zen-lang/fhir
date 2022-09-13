(ns ftr.core
  (:require [clojure.java.io :as io]
            [ftr.extraction.core]
            [ftr.writer.core]
            [ftr.ingestion-coordinator.core]
            [ftr.utils.core]
            [ftr.utils.unifn.core :as u]))


(defmethod u/*fn ::extract-terminology [{:as _ctx, :keys [cfg]}]
  {:extraction-result (ftr.extraction.core/extract cfg)})


(defmethod u/*fn ::write-terminology-file [ctx]
  {:write-result (ftr.writer.core/write-terminology-file ctx)})


(defmethod u/*fn ::shape-ftr-layout [{:as _ctx, :keys [cfg write-result]}]
  (let [tag (:tag cfg)
        module-path (format "%s/%s" (:ftr-path cfg) (:module cfg))
        tags-path (format "%s/tags" module-path)
        vs-dir-path (format "%s/vs" module-path)
        value-set-name (get-in write-result [:value-set :name])
        temp-tf-file (:terminology-file write-result)
        tf-name (.getName temp-tf-file)
        temp-tf-path (.getAbsolutePath temp-tf-file)
        vs-name-path (format "%s/%s" vs-dir-path value-set-name)]

    (doseq [p [tags-path vs-dir-path vs-dir-path vs-name-path]]
      (ftr.utils.core/create-dir-if-not-exists! p))

    {:ftr-layout
     {:module-path module-path
      :tags-path tags-path
      :vs-dir-path vs-dir-path
      :vs-name-path vs-name-path
      :temp-tf-path temp-tf-path
      :tf-path (format "%s/%s" vs-name-path tf-name)
      :tf-tag-path (format "%s/tag.%s.ndjson.gz" vs-name-path tag)
      :tag-index-path (format "%s/%s.ndjson.gz" tags-path tag)}}))


(defmethod u/*fn ::feeder [{:as ctx, :keys [extraction-result]}]
  extraction-result
  (doseq [[_vs-url tf] extraction-result]
    (u/*apply [::write-terminology-file
               ::shape-ftr-layout
               :ftr.ingestion-coordinator.core/ingest-terminology-file]
              (assoc ctx :extraction-result tf))))


(defn apply-cfg [{:as cfg, :keys [source-type]}]
  (condp = source-type
    :flat-table
    (u/*apply [::extract-terminology
               ::write-terminology-file
               ::shape-ftr-layout
               :ftr.ingestion-coordinator.core/ingest-terminology-file] {:cfg cfg})

    :ig
    (u/*apply [::extract-terminology
               ::feeder] {:cfg cfg})))
