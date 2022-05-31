(ns zen.fhir.loinc.xml
  "Ad-hoc approach to download LOINC CodeSystem description
  and convert from xml to edn.

  Use `get-loinc-codesystem-edn` function."
  (:require [clojure.java.io :as io]
            [clojure.xml :as xml]))


(defn get-loinc-codesystem-xml [loinc-path]
  (xml/parse (io/input-stream (str loinc-path "/loinc.xml"))))

(defn rename-value [attrs]
  (if (:value attrs)
    (-> attrs
        (assoc :xml/value (:value attrs))
        (dissoc :value))
    attrs))

(defn collect-fields [xml]
  (cond
    (vector? xml) (group-by :xml/tag (mapv collect-fields xml))
    (map? xml) (try
               {:xml/tag (:tag xml)
                :xml/data (merge (rename-value (:attrs xml)) (collect-fields (:content xml)))}
                  (catch Throwable e (println (:attrs xml) (:content xml))))
    :else xml))

(defn collect-data [intermediate]
  (cond
    (vector? intermediate) (mapv collect-data intermediate)

    (and (map? intermediate)
         (:xml/tag intermediate))
    (collect-data (:xml/data intermediate))

    (map? intermediate)
    (into {} (mapv (fn [[key value]] [key (collect-data value)]) intermediate))

    :else intermediate))

(defn unpack-value [intermediate]
  (cond
    (vector? intermediate) (mapv unpack-value intermediate)

    (and (map? intermediate) (:xml/value intermediate))
    (unpack-value (:xml/value intermediate))

    (map? intermediate)
    (into {} (mapv (fn [[key value]] [key (unpack-value value)]) intermediate))

    :else intermediate))


(defn unarray-needed? [key]
  (get #{:description :uri :type :code :id :compositional :status :hierarchyMeaning :valueSet
         :url :caseSensitive :title :name :copyright :experimental :publisher :content
         :system :value :versionNeeded} key))

(defn unarray [intermediate]
  (cond
    (vector? intermediate) (mapv unarray intermediate)

    (map? intermediate)
    (into {} (mapv (fn [[key value]]
                     (if (unarray-needed? key)
                       [key (unarray (first value))]
                       [key (unarray value)]))
                   intermediate))

    :else intermediate))

(defn string->bool [s]
  (case s
    "true" true
    "false" false
    s))

(defn cast-bool [intermediate]
  (cond
    (vector? intermediate) (mapv cast-bool intermediate)

    (map? intermediate)
    (into {} (mapv (fn [[key value]]
                     [key (cast-bool value)])
                   intermediate))

    (string? intermediate)
    (string->bool intermediate)

    :else intermediate))

(defn codesystem-xml->edn [xml]
  (-> xml
      collect-fields collect-data unpack-value cast-bool unarray
      (dissoc :xmlns)
      (assoc :resourceType "CodeSystem")))

(defn get-loinc-codesystem-edn [loinc-path]
  (codesystem-xml->edn (get-loinc-codesystem-xml loinc-path)))
