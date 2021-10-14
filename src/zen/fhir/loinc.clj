(ns zen.fhir.loinc
  (:require [org.httpkit.client :as http]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [cheshire.core :as json]))


(def login (System/getenv "LOINC_LOGIN"))
(def pass (System/getenv "LOINC_PASSWORD"))


(defn fix-codesystem
  "- LOINC has incorrect space in valueSet url"
  [codesystem]
  (update codesystem :valueSet str/trim))


(def loinc-base-url "https://fhir.loinc.org")


(defn get-loinc-codesystem []
  (-> (http/get (str loinc-base-url "/CodeSystem/?url=http://loinc.org")
                {:basic-auth [login pass]})
      deref
      :body
      (json/parse-string keyword)
      :entry first :resource
      fix-codesystem))


(defn get-loinc-valueset [codesystem]
  (let [valueset-reference (:valueSet codesystem)]
    (-> (http/get (str loinc-base-url "/ValueSet/?url=" valueset-reference)
                  {:basic-auth [login pass]})
        deref
        :body
        (json/parse-string keyword)
        :entry first :resource)))


(defn get-loinc-concept-size [codesystem]
  (-> (http/get (str loinc-base-url "/ValueSet/$expand?count=0&url=" (:valueSet codesystem))
                {:basic-auth [login pass]})
      deref
      :body
      (json/parse-string keyword)
      :expansion :total))


(defn get-loinc-concept-page [codesystem page]
  (-> (http/get (str loinc-base-url "/ValueSet/$expand"
                     "?count=" 1000
                     "&offset=" (* page 1000)
                     "&url=" (:valueSet codesystem))
                {:basic-auth [login pass]})
      deref
      :body
      (json/parse-string keyword)
      :expansion :contains))


(defn page-count [total size]
  (int (Math/ceil (/ total size))))


(defn get-loinc-concepts [codesystem]
  (let [size (get-loinc-concept-size codesystem)
        pages (page-count size 1000)
        concepts (pmap (partial get-loinc-concept-page codesystem) (range pages))]
    (vec (apply concat concepts))))


(defn preprocess-concept [vs concept]
  (assoc concept
         :_source "zen.fhir"
         :valueset [(:url vs)]
         :id (str/replace (str (:system concept) "/" (:code concept)) "/" "-")
         :resourceType "Concept"))

(defn preprocess-concepts [vs concepts]
  (pmap (partial preprocess-concept vs) concepts))

(defn preprocess-codesystem [codesystem]
  (assoc codesystem :_source "zen.fhir"))

(defn preprocess-valueset [valueset]
  (assoc valueset :_source "zen.fhir"))


(defn write-line [w x]
  (.write w (cheshire.core/generate-string x))
  (.write w "\n"))


(defn write-ndjson-gz-zip-bundle [target]
  (let [codesystem (preprocess-codesystem (get-loinc-codesystem))
        valueset (preprocess-valueset (get-loinc-valueset codesystem))
        concepts  (preprocess-concepts valueset (get-loinc-concepts codesystem))]
    (with-open [zip (-> target
                        clojure.java.io/file
                        clojure.java.io/output-stream
                        (java.util.zip.ZipOutputStream.))]
      (let [entry (-> "loinc-terminology-bundle.ndjson.gz"
                      (java.util.zip.ZipEntry.))]
        (.putNextEntry zip entry)
        (with-open [gzip (-> zip
                             (java.util.zip.GZIPOutputStream. true)
                             (java.io.OutputStreamWriter.)
                             (java.io.BufferedWriter.))]

          (write-line gzip codesystem)
          (write-line gzip valueset)
          (doseq [c concepts]
            (write-line gzip c)))))))
