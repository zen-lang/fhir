(ns zen.fhir.loinc
  (:require [org.httpkit.client :as http]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [cheshire.core :as json]))


;;;; !!!! NOTE TODO FIXME !!!!
;;;; THIS IS AN INCOMPLETE IMPLEMENTATION


;; FIXME: use env
(def login "")
(def pass "")


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


(defn write-ndjson-gz-bundle [target]
  (let [codesystem (preprocess-codesystem (get-loinc-codesystem))
        valueset (preprocess-valueset (get-loinc-valueset codesystem))
        concept  (preprocess-concepts valueset (get-loinc-concepts codesystem))]
    (with-open [w (-> target
                      clojure.java.io/file
                      clojure.java.io/output-stream
                      (java.util.zip.GZIPOutputStream. true)
                      (java.io.OutputStreamWriter.)
                      (java.io.BufferedWriter.))]
      (write-line w codesystem)
      (write-line w valueset)
      (doseq [c concept]
        (write-line w c)))))

(comment

  (time (write-ndjson-gz-bundle "/tmp/loinc-terminology-bundle.ndjson.gz"))

  )
