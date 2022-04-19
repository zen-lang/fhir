(ns zen.fhir.cpt
  (:require
   [clojure.string :as str]
   [cheshire.core :as json]
   [clojure.java.io]))


(def code-system
  {:resourceType "CodeSystem"
   :id           "cpt"
   :url          "http://www.ama-assn.org/go/cpt"
   :date         "2022-01-01"
   :description  "Current Procedural Terminology (CPT) codes are numbers assigned to each task and service a healthcare provider offers."
   :content      "complete"
   :name         "CPT"
   :status       "active"
   :version      "2022"
   :valueSet     "http://hl7.org/fhir/ValueSet/cpt-all"
   :_source      "zen.fhir"})


(def value-set
  {:resourceType "ValueSet"
   :id           "cpt-all"
   :description  "A value set that includes all CPT codes"
   :version      "0.0.1"
   :compose      {:include [{:system "http://www.ama-assn.org/go/cpt"}]}
   :date         "2022-01-01"
   :name         "All CPT codes"
   :url          "http://hl7.org/fhir/ValueSet/cpt-all"
   :status       "active"
   :_source      "zen.fhir"})


(defn parse-clinical-descriptors [filename]
  (->> filename
       slurp
       str/split-lines
       rest
       (filter not-empty)
       (map #(str/split % #"\t"))
       (reduce
        (fn [acc [c1 c2 c3 c4]]
          (conj acc {:concept-id c1 :cpt-code c2 :cdi c3 :descriptor c4}))
        [])))

(defn preprocess-concept [vs cs concept]
  {:code         (:cpt-code concept)
   :dispaly      (:descriptor concept)
   :_source      "zen.fhir"
   :valueset     [(:url vs)]
   :system       (:url cs)
   :id           (str/replace (str (:url cs) "/" (:cpt-code concept)) "/" "-")
   :resourceType "Concept"})

(defn preprocess-concepts [vs cs concepts]
  (pmap (partial preprocess-concept vs cs) concepts))

(defn write-line [w x]
  (.write w (cheshire.core/generate-string x))
  (.write w "\n"))

(defn spit-ndjson-gz-bundle! [dir filename resources]
  (let [f    (clojure.java.io/file (str dir \/ filename ".ndjson.gz"))
        outs (java.util.zip.GZIPOutputStream. (clojure.java.io/output-stream f) true)]
    (with-open [w (java.io.BufferedWriter. (java.io.OutputStreamWriter. outs))]
      (write-line w code-system)
      (write-line w value-set)
      (doseq [resource resources]
        (.write w (cheshire.core/generate-string resource))
        (.write w "\n")
        (.flush w)))))

(defn create-cpt-bundle! [path]
  (let [all-cpt-codes (parse-clinical-descriptors path)
        concepts      (preprocess-concepts value-set code-system all-cpt-codes)]
    (spit-ndjson-gz-bundle! "/tmp" "cpt-01012022-terminology-bundle" concepts)))
