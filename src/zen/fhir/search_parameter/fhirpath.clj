(ns zen.fhir.search-parameter.fhirpath
  (:require [clojure.java.io :as io]
            [cheshire.core :as json]
            [clojure.string :as str]))


(def expr-exceptions
  {"Patient.deceased.exists() and Patient.deceased != false"
   {"Patient" [["deceased"]]}

   "SubstanceSpecification.code" #_"NOTE: why is this needed? For old FHIR versions?"
   {"SubstanceSpecification" [["type"]]}


   "StructureDefinition.context" #_"NOTE: why is this needed? For old FHIR versions?"
   {"StructureDefinition" [["context" "type"]]}})


(def where-eq-regex #"where\((.*)=\s*'(.*)'\s*\)?")


(def where-ref-regex #"where\(\s*resolve\(\)\s*is\s*(.*)\s*\)?")


(defn extract-where [s]
  (if (str/starts-with? s "where(")
    (if-let [[_ el val] (re-matches where-eq-regex s)]
      {(keyword el) val}
      (if-let [[_ rt] (re-matches where-ref-regex s)]
        {:resourceType rt}
        (println "ERROR:" s)))
    s))


(defn extract-asis [s]
  (if-let [re (re-matches #"^(is|as)\((.*)\)?" s )]
    (let [tp (last re)]
      (cond
        (= tp "DateTime") "dateTime"
        (= tp "Uri") "uri"
        (= tp "Date") "date"
        (= tp "String") "string"
        :else tp))
    s))


(defn remove-exists [s]
  (str/replace s #"\.exists\(\)" ""))


(defn keywordize [x] (if (map? x) x x))


(defn replace-or [s] (str/replace s #" or " " | "))


(defn split-by-pipe [exp]
  (->>
   (str/split (replace-or exp)  #"\|")
   (mapv (fn [s]
           (-> s
               str/trim
               (str/replace #"^\(\s*" "")
               (str/replace #"\s*\)$" ""))))))


(defn capital? [x]
  (= (subs x 0 1)
     (str/upper-case (subs x 0 1))))


(defn unsupported-syntax? [exp]
  (or (str/includes? exp "extension")
      (str/includes? exp "hasExtension")
      (str/includes? exp "[")))


(defn fhirpath->knife [exp]
  (when (and exp (not (unsupported-syntax? exp)))
    (if-let [e (get expr-exceptions exp)]
      e

      (->>
       (remove-exists exp)
       (split-by-pipe)
       (mapv #(->> (str/split % #"(\.|\s+as\s+)")
                   (mapv (fn [s] (str/replace s #"(^\(|\)$)" "")))
                   (mapv extract-asis)
                   (mapv extract-where)
                   (mapv keywordize)
                   (filter (complement nil?))))
       (reduce (fn [acc exp]
                 (let [k (first exp)]
                   (if (capital? k)
                     (update acc k #(conj (or % []) (vec (rest exp))))
                     (update acc :default #(conj (or % []) (vec exp)))))) {})))))


(defn knife-element->jsonpath-element [kel]
  (cond
    (string? kel)  (format ".\"%s\"" kel)
    (integer? kel) (format "[%s]" kel)
    (map? kel)     (let [preds (map (fn [[k v]] (format "@.\"%s\"==\"%s\"" (name k) v))
                                    kel)]
                     (format "?(%s)" (str/join " && " preds)))
    :else (prn :WARN ::knife-element->jsonpath-element "unknown knife element " kel " skipping")))


(defn knife-path->jsonpath [kpath]
  (let [jsonpath-els (map knife-element->jsonpath-element kpath)]
    (format "$%s[*]" (str/join jsonpath-els))))


(defn knife->jsonpath [knife-paths]
  (mapv knife-path->jsonpath knife-paths))
