(ns ftr.ingestion-coordinator.core
  (:require [ftr.utils.unifn.core :as u]
            [clojure.java.io :as io]
            [ftr.utils.core]
            [ftr.patch-generator.core]))


(defn update-tf-tag! [tf-tag-path new-sha256]
  (let [tf-tag (ftr.utils.core/parse-ndjson-gz tf-tag-path)
        current-sha256 (get-in tf-tag [0 :hash])]
    (->> (-> tf-tag
             (update 0 assoc :hash new-sha256)
             (conj {:from current-sha256 :to new-sha256}))
         (ftr.utils.core/spit-ndjson-gz! tf-tag-path))
    {:ingestion-coordinator {:generate-patch? true
                             :old-sha256 current-sha256}}))


(defn create-tf-tag! [tf-tag-path tag sha256]
  (ftr.utils.core/spit-ndjson-gz! tf-tag-path [{:tag tag :hash sha256}]))


(defmethod u/*fn ::tf-tag-upsert [{:as _ctx,
                                   {:keys [tag]} :cfg
                                   {:keys [tf-sha256]} :write-result
                                   {:keys [tf-tag-path]} :ftr-layout}]
  (if (ftr.utils.core/file-exists? tf-tag-path)
    (update-tf-tag! tf-tag-path tf-sha256)
    (create-tf-tag! tf-tag-path tag tf-sha256)))


(defn update-tag-index! [tag-index-path vs-name module new-sha256]
  (let [tag-index (ftr.utils.core/parse-ndjson-gz tag-index-path)]
    (->> (map (fn [{:as tag-index-entry, :keys [name]}]
                (if (= name (format "%s.%s" module vs-name))
                  (assoc tag-index-entry :hash new-sha256)
                  tag-index-entry))
              tag-index)
         (ftr.utils.core/spit-ndjson-gz! tag-index-path))))


(defn create-tag-index! [tag-index-path vs-name module sha256]
  (->>
    [{:name (format "%s.%s" module vs-name) :hash sha256}]
    (ftr.utils.core/spit-ndjson-gz! tag-index-path)))


(defmethod u/*fn ::tag-index-upsert [{:as _ctx, ;;TODO
                                      {:keys [module]} :cfg
                                      {:keys [tf-sha256 value-set]} :write-result
                                      {:keys [tag-index-path]} :ftr-layout}]
  (if (ftr.utils.core/file-exists? tag-index-path)
    (update-tag-index! tag-index-path (:name value-set) module tf-sha256)
    (create-tag-index! tag-index-path (:name value-set) module tf-sha256)))


(defn generate-tf-name [sha]
  (format "tf.%s.ndjson.gz" sha))


(defmethod u/*fn ::move-terminology-file [{:as _ctx,
                                           {:keys [tf-path vs-name-path]} :ftr-layout
                                           {:keys [terminology-file tf-sha256]} :write-result
                                           {:keys [generate-patch? old-sha256]} :ingestion-coordinator}]
  (let [old-tf-path (format "%s/%s" vs-name-path (generate-tf-name old-sha256))]
    (cond-> {:ingestion-coordinator {:tf-file (ftr.utils.core/move-file! terminology-file tf-path)}}
      generate-patch?
      (->
        (assoc-in [:ftr-layout :old-tf-path] old-tf-path)
        (assoc-in [:ftr-layout :tf-patch-path] (format "%s/patch.%s.%s.ndjson.gz" vs-name-path old-sha256 tf-sha256))
        (assoc-in [:ingestion-coordinator :old-tf-file] (io/file old-tf-path))))))


(defmethod u/*fn ::coordinate-tf-ingestion [ctx]
  (u/*apply [::tf-tag-upsert
             ::tag-index-upsert
             ::move-terminology-file
             :ftr.patch-generator.core/generate-patch] ctx))


(defmethod u/*fn ::ingest-terminology-file
  [{:as ctx,
    {:keys [terminology-file]} :write-result
    {:keys [tf-path]} :ftr-layout}]
  (if (ftr.utils.core/file-exists? tf-path)
    (do (io/delete-file terminology-file)
        {::u/status :stop})
    (u/*apply ::coordinate-tf-ingestion ctx)))
