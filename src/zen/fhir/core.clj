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
(defn group-elements [acc els]
  (->> els
       (reduce (fn [acc {id :id pth :path :as el}]
                 (let [id-path (rich-parse-path id)]
                   (if (empty? id-path)
                     (merge acc (dissoc el :min :max :vector))
                     (assoc-in acc (build-path id-path) el #_(select-keys el [:id :els :polymorphic])))))
               acc)))

(defn reference-profiles [el]
  (let [tp   (first (:type el))
        tpc  (:code tp)
        prof (:targetProfile tp)]

    (if (and (= tpc "Reference") prof)
      (assoc el :profiles (into #{} prof))
      el)))

(defn normalize-polymorphic [el]
  (if (str/ends-with? (:path el) "[x]")
    (-> (assoc el :polymorphic true)
        (dissoc :type)
        (assoc :els (->> (:type el)
                         (reduce (fn [acc {c :code :as tp}]
                                   (assoc acc (keyword c) (-> (reference-profiles {:type [tp]})
                                                              (assoc :type c))))
                                 {})))
        (assoc :types (->> (:type el) (map :code) (into #{}))))
    (if-not (:type el)
      el
      (if (= 1 (count (:type el)))
        (let [tp (first (:type el))
              tpc (:code tp)]
          (-> el (reference-profiles)
              (assoc :type tpc)))
        (throw (Exception. (pr-str el)))))))

(defn normalize-cardinality [{mi :min mx :max :as el}]
  (->
    (cond
      (not (= "1" mx))          (cond-> (assoc el :vector true)
                                  (not (= 0 mi)) (assoc :minItems mi)
                                  (and mx (not (= "*" mx))) (assoc :maxItems (Integer/parseInt mx)))
      (and (= "1" mx) (= 1 mi)) (assoc el :required true)
      :else                     el)
    (dissoc :min :max)))

(defn normalize-binding [el]
  (if-let [bn (:binding el)]
    (cond-> (dissoc el :binding)
      (contains? #{"required" "preferred"} (:strength bn))
      (assoc :binding (dissoc bn :extension)))
    el))

(defn normalize-element [x]
  (-> (dissoc x
              :mapping :constraint :extension :comment :comments :requirements :definition :alias
              :meaningWhenMissing :isModifierReason)
      (normalize-binding)
      (normalize-cardinality)
      (normalize-polymorphic)))

(defn normalize-description [res]
  (-> (dissoc res :description :short)
      (assoc :short (or (:short res) (:description res)))))
;; ADD check by http://www.hl7.org/fhir/elementdefinition.html#interpretation
(defn make-elements [res]
  (->> (get-in res [:differential :element])
       (mapv normalize-element)
       (group-elements (select-keys res [:kind :derivation :baseDefinition :description :fhirVersion]))
       (normalize-description)))

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
