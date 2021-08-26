(ns zen.fhir.core
  (:require [zen.core :as zen]
            [cheshire.core]
            [clojure.java.io :as io]
            [fipp.edn]
            [clojure.string :as str]
            [zen.fhir.utils :as utils]))

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


(defn ^String capitalize-first-letter
  "Converts first character of the string to upper-case, all other characters leaves as is"
  [^CharSequence s]
  (let [s (.toString s)]
    (if (< (count s) 2)
      (.toUpperCase s)
      (str (.toUpperCase (subs s 0 1))
           (subs s 1)))))


(defn build-fhir-poly-keys-mapping [poly-key types]
  (into {}
        (map (fn [el-type]
               (let [fhir-poly-key (str poly-key (capitalize-first-letter el-type))]
                 (-> {(keyword fhir-poly-key)
                      {:key (keyword poly-key)
                       :type el-type}}))))
        types))


;; polymorphic path
;; extensions path
(defn group-elements [acc els]
  (->> els
       (reduce (fn [acc {id :id pth :path :as el}]
                 (let [id-path  (rich-parse-path id)
                       root-el? (empty? id-path)]
                   (if root-el?
                     (merge acc (dissoc el :min :max :vector))
                     (let [last-part      (last id-path)
                           el-path        (build-path id-path)
                           el-parent-path (vec (drop-last 2 el-path))]
                       (cond-> acc
                         (= :poly (:type last-part))
                         (assoc-in
                           (conj el-parent-path :fhir-poly-keys)
                           (build-fhir-poly-keys-mapping (:key last-part) (:types el)))

                         :always
                         (assoc-in el-path el #_(select-keys el [:id :els :polymorphic])))))))
               acc)))

(defn reference-profiles [el]
  (let [tp   (first (:type el))
        tpc  (:code tp)
        prof (:targetProfile tp)]

    (if (and (= tpc "Reference") prof)
      (assoc el :profiles (into #{} prof))
      el)))

(defn normalize-polymorphic [el]
  (if (str/ends-with? (str (:path el)) "[x]")
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

(defn root-element? [el-path]
  (not (str/includes? (str el-path) ".")))

(defn normalize-require [{:as element, el-min :min}]
  (merge element
         {:required (pos? (or el-min 0))}))

(defn normalize-arity
  "The first ElementDefinition (root element) usually has max=* which may be treated as a collection
  but we are treating StructureDefinition as a tool to validate a single resource"
  [{:as element, id :id, el-min :min, el-max :max, {base-max :max} :base}]
  (merge element
         (when (and (not (root-element? id))
                    (or (some? base-max)
                        (some? el-max)))
           (if (and (not= "1" base-max)
                    (not= "0" base-max)
                    (or (some? base-max)
                        (and (not= "1" el-max)
                             (not= "0" el-max))))
             {:vector       true
              :minItems     (when-not (= 0 el-min) el-min)
              :maxItems     (when-not (= "*" el-max) (utils/parse-int el-max))}
             (when (or (= "0" el-max)
                       (and (nil? el-max)
                            (= "0" base-max)))
               {:prohibited true})))))

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
      (normalize-require)
      (normalize-arity)
      (normalize-polymorphic)))

(defn normalize-description [res]
  (-> (dissoc res :description :short)
      (assoc :short (or (:short res) (:description res)))))
;; ADD check by http://www.hl7.org/fhir/elementdefinition.html#interpretation

(defn make-elements [res]
  (->> (get-in res [:differential :element])
       (mapv normalize-element)
       (group-elements (select-keys res [:kind :derivation :baseDefinition :description :fhirVersion :type]))
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

(defn walk-with-base [ztx ctx subj base]
  (->> (:els subj)
       (reduce (fn [acc [k el]]
                 (let [base-el (get-in base [:els k])]
                   (if-not base-el
                     (if-let [base-poly-key (get-in base [:fhir-poly-keys k])]
                       (assoc-in acc
                                 [(:key base-poly-key) :els (keyword (:type base-poly-key))]
                                 (assoc el :base-type (:type base-poly-key)))
                       (do (println :ups (:id el))
                           (assoc acc k (assoc el :error :no-base))))
                     (assoc acc k (if (:els el)
                                    (walk-with-base ztx (update ctx :lvl inc) el base-el)
                                    el)))))
               {})))

(defn process-sd [ztx url subj]
  (if (and (= "constraint" (:derivation subj)) (not (= "Extension" (:type subj))))
    (let [base-url (str "http://hl7.org/fhir/StructureDefinition/" (:type subj))]
      (if (= url base-url)
        subj
        (let [base (get-in @ztx [:fhir "StructureDefinition" base-url :elements])]
          (println :process url (:type subj))
          (assert base (pr-str :WARN :no-base base-url))
          (walk-with-base ztx {:lvl 0} subj base))))
    subj))

(defn process-structure-definitions [ztx]
  (swap! ztx update-in [:fhir "StructureDefinition"]
         (fn [old]
           (->> old
                (reduce (fn [acc [url res]]
                          (assoc acc url (assoc res :elements (process-sd ztx url (:elements res)))))
                        {})))))

(defn process-resources [ztx]
  (process-structure-definitions ztx))

(defn load-all [ztx package]
  (doseq [pkg-dir (.listFiles (io/file "node_modules"))]
    (when (and (.isDirectory pkg-dir)(not (str/starts-with? (.getName pkg-dir) ".")))
      (println pkg-dir)

      (let [package (read-json (str (.getPath pkg-dir) "/package.json"))
            index (read-json (str (.getPath pkg-dir) "/.index.json"))]
        (doseq [{filename :filename :as header} (:files index)]
          (load-json-file ztx package header (io/file (str (.getPath pkg-dir) "/" filename))))
        (println :loaded (:name package) (count (:files index))))))
  (process-resources ztx))






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
