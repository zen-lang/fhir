(ns zen.fhir.loinc
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.data.xml :as xml]
            [clojure.string :as str]
            [org.httpkit.client :as http]
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

(def codesystem (get-loinc-codesystem))

(defn get-loinc-valueset []
  (let [valueset-reference (:valueSet codesystem)]
    (-> (http/get (str loinc-base-url "/ValueSet/?url=" valueset-reference)
                  {:basic-auth [login pass]})
        deref
        :body
        (json/parse-string keyword)
        :entry first :resource)))

(def valueset (get-loinc-valueset))

(-> @(http/get (str loinc-base-url "/ValueSet/$expand?url=" (:valueSet codesystem))
               {:basic-auth [login pass]})
    :body
    (json/parse-string keyword))

(defn get-loinc-concept-size []
  (-> (http/get (str loinc-base-url "/ValueSet/$expand?count=0&url=" (:valueSet codesystem))
                {:basic-auth [login pass]})
      deref
      :body
      (json/parse-string keyword)
      :expansion :total))

(defn get-loinc-concept-page [page]
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

(defn get-loinc-concepts []
  (let [size (get-loinc-concept-size)
        pages (page-count size 1000)
        concepts (pmap get-loinc-concept-page (range pages))]
    (vec (apply concat concepts))))

(def loinc-concepts (get-loinc-concepts))
