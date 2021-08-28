(ns zen.fhir.core
  (:require [zen.core :as zen]
            [cheshire.core]
            [clojure.java.io :as io]
            [fipp.edn]
            [clojure.string :as str]
            [zen.fhir.utils :as utils]
            [com.rpl.specter :as sp]))

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
                     :key        (conj acc :| k)
                     :slice      (conj acc :slice k)
                     :poly       (conj acc :| k)
                     :poly-slice (conj acc :| k))))
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
                     (-> (merge acc el)
                         (dissoc  :vector :id :path :short :example))
                     (let [last-part      (last id-path)
                           el-path        (build-path id-path)
                           el-root-path   (vec (butlast el-path))
                           el-parent-path (vec (butlast el-root-path))]
                       (cond-> acc
                         (= :poly (:type last-part))
                         (assoc-in (conj el-parent-path :fhir-poly-keys)
                                   (build-fhir-poly-keys-mapping (:key last-part) (:types el)))

                         :always
                         (assoc-in el-path (dissoc el :id :path :example) #_(select-keys el [:id :| :polymorphic])))))))
               acc)))


(defn reference-profiles [el]
  (let [tp   (first (:type el))
        tpc  (:code tp)
        prof (:targetProfile tp)]
    (if (and (= tpc "Reference") prof)
      (assoc el :profiles (into #{} prof) :escalate {:deps {:refs (reduce (fn [acc x] (assoc acc x true)) {} prof)}})
      el)))


(defn normalize-polymorphic [el]
  (if (str/ends-with? (str (or (:path el) (:id el))) "[x]")
    (-> (assoc el :polymorphic true)
        (dissoc :type)
        (assoc :| (->> (:type el)
                         (reduce (fn [acc {c :code :as tp}]
                                   (assoc acc (keyword c) (-> (reference-profiles {:type [tp]  :escalate {:deps {:type {c true}}}})
                                                              (assoc :type c))))
                                 {})))
        (assoc :types (->> (:type el) (map :code) (into #{}))))
    (if-not (:type el)
      el
      (if (= 1 (count (:type el)))
        (let [tp  (first (:type el))
              tpc (:code tp)]
          (-> el (reference-profiles)
              (assoc :type tpc :escalate {:deps {:type {tpc true}}})))
        (throw (Exception. (pr-str el)))))))


(defn root-element? [el-path]
  (not (str/includes? (str el-path) ".")))


(defn normalize-require [{:as element, el-min :min}]
  (if (pos? (or el-min 0))
    (assoc element :required true)
    element))


;; why not use settings of base for arity
(defn fix-arity
  "The first ElementDefinition (root element) usually has max=* which may be treated as a collection
  but we are treating StructureDefinition as a tool to validate a single resource"
  [{:as element el-type :type} {v :vector r :required base-type :type :as _base}]
  (let [tp (or el-type base-type)]
    (cond-> (merge element (utils/strip-nils {:vector v :required r}))
      tp (assoc :type tp :escalate {:deps {:type {tp true}}})
      (not v) (dissoc :maxItems :minItems))))

(defn normalize-arity
  "The first ElementDefinition (root element) usually has max=* which may be treated as a collection
  but we are treating StructureDefinition as a tool to validate a single resource"
  [{:as element, id :id, el-min :min, el-max :max}]
  (->
    (cond-> element
      (and (not (nil? el-max)) (not (contains? #{"1" "0"} el-max)) (not (root-element? id)))
      (assoc :vector true)

      (and (not (nil? el-min)) (not (= 0 el-min)))
      (assoc :minItems el-min)

      (and (not (nil? el-max)) (not (contains? #{"*"} el-max) ))
      (assoc :maxItems (utils/parse-int el-max)))
    (dissoc :min :max)))


(defn normalize-binding [el]
  (if-let [bn (:binding el)]
    (cond-> (dissoc el :binding)
      (contains? #{"required" "preferred"} (:strength bn))
      (assoc :binding (dissoc bn :extension)))
    el))


(defn normalize-content-ref [x]
  (if-let [cr (:contentReference x)]
    (assoc x :escalate  {:recur (->> (rest (str/split cr #"\."))
                                     (mapv keyword))})
    x))

(defn normalize-element [x]
  (-> (dissoc x
              :mapping :constraint :extension :comment :comments :requirements :definition :alias
              :meaningWhenMissing :isModifierReason)
      (normalize-binding)
      (normalize-require)
      (normalize-arity)
      (normalize-polymorphic)
      (normalize-content-ref)))


(defn normalize-description [res]
  (-> (dissoc res :description :short)
      (assoc :short (or (:short res) (:description res)))))
;; ADD check by http://www.hl7.org/fhir/elementdefinition.html#interpretation


(defn *normalize-extension [res]
  (if-let [complex (get-in res [:| :extension :slice])]
    (-> (assoc res :| (->> complex
                           (reduce (fn [acc [k v]]
                                     (assoc acc k (*normalize-extension v)))
                                   {}))
               :fhir/extension (get-in res [:| :url :fixedUri]))
        (dissoc :fhir-poly-keys))
    (let [values (dissoc (:| res) :url)]
      (if (= 1 (count values))
        (merge (dissoc (first (vals values)) :minItems :maxItems :required)
               (select-keys res [:vector :maxItems :minItems :required]))
        {:error :error}))))

(defn normalize-extension [res]
  (if-not (= "Extension" (:type res))
    res
    (*normalize-extension res)))

(defn load-intermidiate [res]
  (->> (get-in res [:differential :element])
       (mapv normalize-element)
       (group-elements (select-keys res [:kind :derivation :baseDefinition :description :fhirVersion :type]))
       (normalize-description)
       (normalize-extension)))



(defmulti process-on-load
  (fn [res] (keyword (:resourceType res))))


(defmethod process-on-load :default
  [res]
  #_(println :WARN :no-process-on-load :for (:resourceType res)))



(defmethod process-on-load
  :StructureDefinition
  [res]
  (load-intermidiate res))


(defn load-definiton [ztx packages header res]
  (if-let [rt (:resourceType res)]
    (if-let [url (:url header)]
      (swap! ztx update-in [:fhir/src rt url]
             (fn [x] (when x (println :WARN :override-resource header)) res))
      (println :WARN :no-url header))
    (println :WARN :no-resource-type header)))

(defn load-json-file [ztx package header f]
  (let [res (-> (cheshire.core/parse-string (slurp f) keyword)
                (assoc :zen.fhir/header header :zen.fhir/package package :zen.fhir/file (.getPath f)))]
    (load-definiton ztx package header res)))


(defn read-json [f] (cheshire.core/parse-string (slurp f) keyword))

(defn base-url [subj]
  (println(:type subj) (pr-str :no-type-in subj))
  (str "http://hl7.org/fhir/StructureDefinition/" (:type subj)))

(defn is-profile? [url subj]
  (and (= "constraint" (:derivation subj))
       (not (= "Extension" (:type subj)))
       (not (= url (base-url subj)))))

(defn get-base [ztx subj]
  (get-in @ztx [:fhir/inter "StructureDefinition" (base-url subj)]))

(defn get-definition [ztx url]
  (get-in @ztx [:fhir/inter "StructureDefinition" url]))

(defn get-original [ztx url]
  (get-in @ztx [:fhir/src "StructureDefinition" url]))

;; one of the most complicated places now
(defn walk-with-base [ztx ctx subj base]
  (update subj :|
          #(reduce (fn [acc [k el]]

                     (if-let [base-el (get-in base [:| k])]
                       (assoc acc k
                              (let [el      (fix-arity el base-el)
                                    new-ctx (-> (update ctx :lvl inc) (update :path conj k))]
                                (if (and (not (:| base-el)) (:| el))
                                  (if-let [tp-base (get-definition ztx (base-url base-el))]
                                    (walk-with-base ztx new-ctx el tp-base)
                                    (assoc el :error {:no-type-jump true}))
                                  (if (:| el) (walk-with-base ztx new-ctx el base-el) el))))

                       (if-let [base-poly-key (get-in base [:fhir-poly-keys k])]
                         (let [fixed-key (:key base-poly-key)
                               base-poly-el (get-in base [:| fixed-key])
                               tp-key       (keyword (:type base-poly-key))
                               base-type-el (get-in base [:| fixed-key :| ])
                               base-el      (merge base-poly-el base-type-el)
                               el           (fix-arity el base-el)]
                           (-> acc
                               (assoc-in [fixed-key :polymorphic] true)
                               (assoc-in [fixed-key :| tp-key]
                                         (if (and (:| el) (not (:| base-el)))
                                           (if-let [tp-base (get-definition ztx (base-url base-poly-key))]
                                             (walk-with-base ztx (-> (update ctx :lvl inc)
                                                                     (update :path conj k))
                                                             (fix-arity el base-el) tp-base)
                                             (throw (Exception. (pr-str "Unexpected" (conj (:path ctx) k)
                                                                        :el el
                                                                        :base-poly-key base-poly-key
                                                                        :base-el base-el))))
                                           (walk-with-base ztx (-> (update ctx :lvl inc)
                                                                   (update :path conj k))
                                                           el base-el)))))
                         (assoc acc k (assoc el :error :no-base))
                         #_(throw (Exception. (pr-str "No base " :el el :path (conj (:path ctx) k)
                                                    :el el
                                                    :top-base base))))))
                   {}
                   %)))


(defn is-extension?
  [_url subj]
  (= "Extension" (:type subj)))

(defn process-extension
  [ztx url subj]
  (println "Process ext")
  subj)

(defn process-sd [ztx url subj]
  (cond
    (is-profile? url subj)
    (let [base (get-base ztx subj)]
      (assert base (pr-str :WARN :no-base url subj))
      (walk-with-base ztx {:lvl 0 :path [url]} subj base))

    (is-extension? url subj)
    (process-extension ztx url subj)

    :else subj))


(defn process-structure-definitions [ztx]
  (swap! ztx update-in [:fhir/inter "StructureDefinition"]
         (fn [old]
           (->> old
                (reduce (fn [acc [url resource]]
                          (assoc acc url (process-sd ztx url resource)))
                        {})))))


(defn preprocess-resources
  ;; this is pure transformation of original resources (i.e. without context)
  [ztx]
  (swap! ztx
         assoc :fhir/inter
         (->> (:fhir/src @ztx)
              (reduce (fn [acc [type resources]]
                        (assoc acc type (->> resources
                                             (reduce (fn [acc' [url resource]]
                                                       (assoc acc' url (process-on-load resource)))
                                                     {}))))
                      {}))))


(defn process-resources
  "this is processing of resources with context"
  [ztx]
  (process-structure-definitions ztx))


(defn load-all [ztx package]
  (doseq [pkg-dir (.listFiles (io/file "node_modules"))
          :when   (and (.isDirectory pkg-dir)(not (str/starts-with? (.getName pkg-dir) ".")))
          :let    [package (read-json (str (.getPath pkg-dir) "/package.json"))
                   index   (read-json (str (.getPath pkg-dir) "/.index.json"))]
          {filename :filename :as header} (:files index)]
    (load-json-file ztx package header (io/file (str (.getPath pkg-dir) "/" filename))))
  (preprocess-resources ztx)
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
