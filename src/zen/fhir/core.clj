(ns zen.fhir.core
  (:require [zen.core :as zen]
            [cheshire.core]
            [clojure.java.io :as io]
            [fipp.edn]
            [clojure.string :as str]))

;; load resources into memory [rt id]
;; transform to zen (two phase?)
;; elements => nested structure => keys
;; id and namespace based on url


;; min/max => vector? required minItems/maxItems
;; type -> polymorphic, type, profiles
;; references to extensions etc
;; group and analyze slicing
;; analyze valuesets


(def poly-id-terminator "[x]")

(defn drop-poly-name [id poly-name]
  (subs id (count poly-name)))

(defn drop-poly-terminator [id]
  (subs id 0 (- (count id) (count poly-id-terminator))))

(defn rich-parse-path [id]
  (if (str/blank? id)
    []
    (->> (rest (str/split id #"\."))
         (mapcat
           (fn [id-part]
             (let [[key-part slice-part] (str/split id-part #":" 2)]
               (cond
                 (str/ends-with? key-part poly-id-terminator)
                 (let [poly-name (drop-poly-terminator key-part)]
                   (cond-> [{:type :poly :key poly-name}]
                     (some? slice-part) (conj {:type :poly-slice
                                               :key  (drop-poly-name slice-part poly-name)
                                               :poly-name poly-name})))

                 (some? slice-part) [{:key key-part :type :key}
                                     {:key slice-part :type :slice }]
                 :else              [{:key key-part
                                      :type :key}]))))
         vec)))

(rich-parse-path "Patient.identifier:MRI.value[x]:valueCode")

[:els :identifier :slice :MRI :value :els :Quantity :els :value]

(defn build-path [id-path]
  (->> id-path
       (reduce (fn [acc {k :key tp :type}]
                 (let [k (keyword k)]
                   (case tp
                     :key        (conj acc :els k)
                     :slice      (conj acc :slice k)
                     :poly       (conj acc :els k)
                     :poly-slice (conj acc :els k))))
               [])))

;; polymorphic path
;; extensions path
(defn group-elements [els]
  (->> els
       (reduce (fn [acc {id :id pth :path :as el}]
                 (let [id-path (rich-parse-path id)]
                   (if (empty? pth)
                     el
                     (assoc-in acc (build-path id-path) (select-keys el [:id])))))
               {})))

(defn normalize-element [x]
  (-> (dissoc x :mapping :path :constraint :extension :comment :comments :requirements :definition)
      (assoc :path
             (->> (rest (str/split (:path x) #"\."))
                  (mapv (fn [x] (str/replace x #"\[x\]" "")))
                  (into [])
                  (mapv keyword)))))

;; ADD check by http://www.hl7.org/fhir/elementdefinition.html#interpretation
(defn make-elements [res]
  (->> (get-in res [:differential :element])
       (mapv normalize-element)
       (group-elements)))

(defmulti process-on-load (fn [res] (keyword (:resourceType res))))
(defmethod process-on-load :default
  [res]
  #_(println :WARN :no-process-on-load :for (:resourceType res)))

(defmethod process-on-load :StructureDefinition
  [res]
  {:src (dissoc res :text)
   :elements (make-elements res)})

(defn load-json-file [ztx package header f]
  (let [res (-> (cheshire.core/parse-string (slurp f) keyword)
                (assoc :zen.fhir/header header :zen.fhir/package package :zen.fhir/file (.getPath f)))]
    (if-let [rt (:resourceType res)]
      (if-let [url (:url header)]
        (swap! ztx update-in [:fhir rt url]
               (fn [x]
                 (when x (println :WARN :override-resource header))
                 (process-on-load res)))
        (println :WARN :no-url header))
      (println :WARN :no-resource-type header))))

(defn read-json [f]
  (cheshire.core/parse-string (slurp f) keyword))

(defn load-all [ztx package]
  (doseq [pkg-dir (.listFiles (io/file "node_modules"))]
    (when (and (.isDirectory pkg-dir)(not (str/starts-with? (.getName pkg-dir) ".")))
      (println pkg-dir)

      (let [package (read-json (str (.getPath pkg-dir) "/package.json"))
            index (read-json (str (.getPath pkg-dir) "/.index.json"))]
        (doseq [{filename :filename :as header} (:files index)]
          (load-json-file ztx package header (io/file (str (.getPath pkg-dir) "/" filename))))
        (println :loaded (:name package) (count (:files index)))))))





(comment

  (do
    (def ztx (zen/new-context {}))
    (load-all ztx "hl7.fhir.r4.core")
    )

  (get-in @ztx [:fhir "StructureDefinition"
                "http://hl7.org/fhir/us/carin-bb/StructureDefinition/C4BB-ExplanationOfBenefit-Inpatient-Institutional"
                :elements])


  (get-in @ztx [:fhir "StructureDefinition"
                "http://hl7.org/fhir/us/core/StructureDefinition/us-core-patient"
                :elements])

  (get-in @ztx [:fhir "StructureDefinition"
                "http://hl7.org/fhir/us/core/StructureDefinition/us-core-race"
                :elements])

  (get-in @ztx [:fhir "StructureDefinition"
                "http://hl7.org/fhir/us/core/StructureDefinition/us-core-patient"
                :src])

  (keys (get-in @ztx [:fhir "StructureDefinition"]))



  (def def-ks
    (->> (get-in @ztx [:fhir "hl7.fhir.r4.core" "StructureDefinition"])
         (map (fn [[k v]]
                (->
                  (select-keys v [:id :kind :type :derivation :abstract])
                  (assoc :base (when-let [bd (:baseDefinition v)]
                                 (-> (str/split bd #"/")
                                     (last)))))))))


  (def schemas
    (->> (get-in @ztx [:fhir "hl7.fhir.r4.core" "StructureDefinition"])
         (reduce (fn [acc [_ {id :id :as res}]]
                   (assert id "id is required")
                   (-> (update acc (symbol id)
                               process-sd
                               )))
                 {})))

  (time
    (doseq [[id res] schemas]
      (when (contains? (:zen/tags res) 'fhir/specialization)
        (spit (str "/tmp/fhir/" id ".edn")
              (with-out-str (fipp.edn/pprint res))))))



  (->
    (group-by :derivation def-ks)
    (get nil))

  (->>
    def-ks
    (mapv #(select-keys % [:kind :derivation :abstract]))
    (into #{}))

  {:kind       (->
                 (group-by :kind def-ks)
                 (keys))
   ;; :type (->
   ;;         (group-by :type def-ks)
   ;;         (keys))
   :derivation (->
                 (group-by :derivation def-ks)
                 (keys))}

  ;; (zen.fhir.loader/generate-profiles-types-uni-project 'zen-fhir.R4-test type-profiles-bundle resource-profiles-bundle "test-temp-zrc")

  (-> (get-in @ztx [:fhir "hl7.fhir.r4.core" "StructureDefinition" "heartrate" ])
      (dissoc  :extension :mapping :contact))

  (-> (get-in @ztx [:fhir "hl7.fhir.r4.core" "StructureDefinition"])
      (keys))

  (def resource-not-important-attrs
    [:date :meta :snapshot :extension :mapping :contact :_baseDefinition :purpose :fhirVersion :publisher :description])

  (def element-not-important-attrs
    [:constraint :mapping :alias :requirements :definition :comment :id])

  (defn clean-up [res]
    (-> (apply dissoc res resource-not-important-attrs)
        (update-in [:differential :element]
                   (fn [xs]
                     (->> xs
                          (mapv (fn [x]
                                  (cond-> (apply dissoc x element-not-important-attrs)
                                    (:binding x) (update :binding dissoc :extension)))))))))

  (-> (get-in @ztx [:fhir "hl7.fhir.r4.core" "StructureDefinition" "Questionnaire"])
      (clean-up))

  (-> (get-in @ztx [:fhir "hl7.fhir.r4.core" "StructureDefinition" "Age"])
      (dissoc :snapshot :extension :mapping :contact))

  (-> (get-in @ztx [:fhir "hl7.fhir.r4.core" "StructureDefinition" "bmi"])
      (dissoc :snapshot :extension :mapping :contact))

  (-> (get-in @ztx [:fhir "hl7.fhir.r4.core" "StructureDefinition" "Definition"])
      (dissoc :extension :mapping :contact))

  (-> (get-in @ztx [:fhir "hl7.fhir.r4.core" "StructureDefinition" "Event"])
      (dissoc :extension :mapping :contact))

  (-> (get-in @ztx [:fhir "hl7.fhir.r4.core" "StructureDefinition" "Resource"])
      (dissoc :extension :mapping :contact :snapshot))

  (-> (get-in @ztx [:fhir "hl7.fhir.r4.core" "StructureDefinition" "openEHR-test"])
      (dissoc :extension :mapping :contact :snapshot))

  )
;; 1. differential
;; 2. context for
;; * problem polymorphic
;; * prbolem with cardinality
;; * valueset ets
;; * extensions


;;  ig(npm) -> get all deps -> load into memory db
;;  only for this ig  sd=>zen (sp, vs, ???)
;;  publish

;; problem with content reference

;; snapshot.elements ->
;; {:StructureDefinition {"hl7fhir/Patient" {:elements {[:id :duce] {:el :value}
;;                                                      [:id :duce] {:el :value}
;;                                                      [:id :duce] {:el :value}}}
;;                        "fhir/Qest"       {:elements {[:id :duce] {:el :value}
;;                                                      [:id :duce] {:el :value}
;;                                                      [:id :duce] {:el :value}}}}
;;  :ValueSet            {"url" {}}
;;  :SearchParameter     {"url" {}}}



;; {:StructureDefinition {"hl7fhir/Patient" {:elements {:name {:url "hl7fhir/HumanName"}
;;                                                      :communication {:elements {}}
;;                                                      :identifier {:url "hl7/Identifier"}}}
;;                        "fhir/Qest"       {:elements {:item {:elements {:item {:contetRef [:item]}}}}}
;;                        "us-core/Patient" {:elements {:race {:url "us-core/race-ext"}
;;                                                      :identifier {:base ["hl7fhir/Patient" :identfier]
;;                                                                   :slice {:mri {:elements {:system {}}}}}}}}
;;  :ValueSet             {"url" {}}
;;  :SearchParameter      {"url" {}}}

;; (sd-to-ns ctx rt url)
