(ns zen.fhir.search-parameter.fhirpath
  (:require [clojure.stacktrace]
            [clojure.string :as str]))

(def expr-exceptions
  {"Patient.deceased.exists() and Patient.deceased != false"
   {"Patient" [["deceased"]]}

   "Person.deceased.exists() and Person.deceased != false"
   {"Person" [["deceased"]]}

   "Practitioner.deceased.exists() and Practitioner.deceased != false"
   {"Practitioner" [["deceased"]]}

   "SubstanceSpecification.code" #_"NOTE: why is this needed? For old FHIR versions?"
   {"SubstanceSpecification" [["type"]]}

   "StructureDefinition.context" #_"NOTE: why is this needed? For old FHIR versions?"
   {"StructureDefinition" [["context" "type"]]}})

(def where-eq-regex #"where\((.*)=\s*'(.*)'\s*\)?")

(def where-ref-regex #"where\(\s*resolve\(\)\s*is\s*(.*)\s*\)?")

(defn extract-where [s]
  (if (and (string? s) (str/starts-with? s "where("))
    (if-let [[_ el value] (re-matches where-eq-regex s)]
      {(keyword (str/trim el)) value}
      (if-let [[_ rt] (re-matches where-ref-regex s)]
        {:resourceType rt}
        (println "ERROR:" s)))
    s))

(defn extract-asis [s]
  (if-let [re (re-matches #"^(is|as|ofType)\((.*)\)?" s)]
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
  (or #_(str/includes? exp "extension")
   (str/includes? exp "hasExtension")
      #_(str/includes? exp "[")))

(defn quirks [exp]
  (str/replace exp "(start | requestedPeriod.start).first()"
               "Appointment.start | Appointment.requestedPeriod[0].start | Task.requestedPeriod[0].start"))

(defn split-but-ignore-url
  "Split string by ' as ' and '.', ignore dot in url.
  (where clause contains url with dots)"
  [s]
  (str/split s #"\.\s*(?=([^']*'[^']*')*[^']*$)|\s+as\s+|\["))

(defn extract-subscript [s]
  (let [without-last (subs s 0 (dec (count s)))
        last-char (last s)]
    (cond
      (= \] last-char)
      (try (Integer/parseInt without-last)
           (catch Exception _e s))
      :else s)))

(defn fhirpath->knife [exp]
  (when (and exp (not (unsupported-syntax? exp)))
    (try
      (if-let [e (get expr-exceptions exp)]
        e
        (->>
         (remove-exists exp)
         (quirks)
         (split-by-pipe)
         (mapv #(->> (split-but-ignore-url %)
                     (mapv (fn [s] (str/replace s #"(^\(|\)$)" "")))
                     (mapv extract-asis)
                     (mapv extract-subscript)
                     (mapv extract-where)
                     ((fn [a] (println a) a))
                     ((fn [a] (println a) a))
                     (filterv (complement nil?))))

         (reduce (fn [acc exp]
                   (let [k (first exp)]
                     (if (capital? k)
                       (update acc k #(conj (or % []) (vec (rest exp))))
                       (update acc :default #(conj (or % []) (vec exp)))))) {})))

      (catch Exception e (do (clojure.stacktrace/print-stack-trace e) (throw e))))))

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
