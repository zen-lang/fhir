(ns ftr.utils.core
  (:require [clojure.java.io :as io]
            [cheshire.core :as json]))


(defn dissoc-when
  ([pred m k]
   (cond-> m
     (and (contains? m k)
          (pred (get m k)))
     (dissoc k)))
  ([pred m k & ks]
   (reduce (partial dissoc-when pred)
           (dissoc-when pred m k)
           ks)))


(defn strip-when [pred m]
  (if-let [ks (seq (keys m))]
    (apply dissoc-when pred m ks)
    m))


(defn strip-nils [m]
  (strip-when nil? m))


(defn create-dir-if-not-exists!
  [path]
  (let [file (io/file path)]
    (if (.exists file)
      file
      (do (.mkdirs file)
          file))))


(defn make-sha256-gzip-writer [output]
  (let [digest (java.security.MessageDigest/getInstance "SHA-256")
        file   (io/file output)]
    {:writer (-> file
                 (java.io.FileOutputStream. true)
                 (java.util.zip.GZIPOutputStream. true)
                 (java.security.DigestOutputStream. digest)
                 (java.io.OutputStreamWriter.)
                 (java.io.BufferedWriter.))
     :file   file
     :digest (fn [] (format "%032x" (BigInteger. 1 (.digest digest))))}))


(defn gen-uuid []
  (str (java.util.UUID/randomUUID)))


(defn rmrf [path]
  (run! io/delete-file (reverse (file-seq (io/file path)))))


(defn parse-ndjson-gz [path]
  (with-open [rdr (-> path
                      (java.io.FileInputStream.)
                      (java.util.zip.GZIPInputStream.)
                      (io/reader))]
    (->> rdr
         line-seq
         (mapv (fn [json-row]
                 (cheshire.core/parse-string json-row keyword))))))


(defn generate-ndjson-row [obj]
  (format "%s\n" (cheshire.core/generate-string (into (sorted-map) obj))))
