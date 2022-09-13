(ns ftr.utils.core
  (:require [clojure.java.io :as io]
            [cheshire.core :as json]
            [clojure.string :as str])
  (:import org.apache.commons.io.input.ReversedLinesFileReader))


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
  (let [file (io/file path)]
    (when (.exists file)
      (run! io/delete-file (reverse (file-seq file))))))


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


(defn file-exists? [path]
  (.exists (io/file path)))


(defn spit-ndjson-gz! [output-path coll]
  (with-open [w (-> output-path
                    (io/file)
                    (java.io.FileOutputStream.)
                    (java.util.zip.GZIPOutputStream. true)
                    (java.io.OutputStreamWriter.)
                    (java.io.BufferedWriter.))]
    (doseq [c coll]
      (.write w (generate-ndjson-row c)))))


(defn move-file! [src dest]
  (java.nio.file.Files/move (.toPath (io/file src))
                            (.toPath (io/file dest))
                            (into-array java.nio.file.CopyOption
                                        [(java.nio.file.StandardCopyOption/ATOMIC_MOVE)
                                         (java.nio.file.StandardCopyOption/REPLACE_EXISTING)])))


(defprotocol NdjsonReader
  (readLine [this] "Reads and parse json line from reader"))


(defn open-ndjson-gz-reader [input]
  (let [ndjson-gz-reader (-> input
                             (java.io.FileInputStream.)
                             (java.util.zip.GZIPInputStream.)
                             (io/reader))]
    (reify NdjsonReader
      (readLine [this] (-> ndjson-gz-reader
                           .readLine
                           (cheshire.core/parse-string keyword))))))
