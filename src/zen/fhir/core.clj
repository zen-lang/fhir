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
                     :slice      (conj acc :slicing :slices k)
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

(defn ^String decapitalize-first-letter
  "Converts first character of the string to lower-case, all other characters leaves as is"
  [^CharSequence s]
  (let [s (.toString s)]
    (if (< (count s) 2)
      (.toLowerCase s)
      (str (.toLowerCase (subs s 0 1))
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
      (assoc el :profiles (into #{} prof))
      el)))


(defn extension-profiles [el]
  (if-let [ext-profs (:profile (first (:type el)))]
    (do
      (assert (= 1 (count ext-profs)) (pr-str :unexpected-extension-profiles (:type el)))
      (assoc el :fhir/extension (first ext-profs)))
    el))




(defn get-type-code[{code :code extension :extension}]
  ;; wellknonw bug in FHIR SDs
  ;; StructureDefinition generator has a bug
  ;; instead of id type it uses exension type
  ;; https://chat.fhir.org/#narrow/stream/179283-Da-Vinci/topic/Type.20of.20id/near/232607087
  (or (some-> (utils/code-search :url
                                 ["http://hl7.org/fhir/StructureDefinition/structuredefinition-fhir-type"]
                                 extension)
              (utils/poly-get :value))
      code))

(defn normalize-polymorphic [el]
  (if (str/ends-with? (str (or (:path el) (:id el))) "[x]")
    (-> (assoc el :polymorphic true)
        (dissoc :type)
        (assoc :| (->> (:type el)
                         (reduce (fn [acc {c :code :as tp}]
                                   (assoc acc (keyword c) (-> (reference-profiles {:type [tp]})
                                                              (assoc :type (get-type-code tp)))))
                                 {})))
        (assoc :types (->> (:type el) (map :code) (into #{}))))
    (if-not (:type el)
      el
      (if (= 1 (count (:type el)))
        (let [tp  (first (:type el))
              tpc (get-type-code tp)]
          (-> el
              (reference-profiles)
              (extension-profiles)
              (assoc :type tpc)))
        (throw (Exception. (pr-str el)))))))


(defn root-element? [el-path]
  (not (str/includes? (str el-path) ".")))


(defn normalize-require [{:as element, el-min :min}]
  (if (pos? (or el-min 0))
    (assoc element :required true)
    element))


;; ;; why not use settings of base for arity
;; (defn fix-arity
;;   "The first ElementDefinition (root element) usually has max=* which may be treated as a collection
;;   but we are treating StructureDefinition as a tool to validate a single resource"
;;   [{:as element el-type :type} {v :vector r :required base-type :type :as _base}]
;;   (let [tp (or el-type base-type)]
;;     (cond-> (merge element (utils/strip-nils {:vector v :required r}))
;;       tp (assoc :type tp)
;;       (not v) (dissoc :maxItems :minItems))))

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
    (assoc x :recur (->> (rest (str/split cr #"\."))
                         (mapv keyword)))
    x))

(defn normalize-flags [x]
  (let [flags (cond-> #{}
                (:isModifier x)  (conj :?!)
                (:isSummary x)   (conj :SU)
                (:mustSupport x) (conj :MS))]
    (-> x
        (dissoc :isModifier :isSummary :mustSupport)
        (cond->
            (not (empty? flags)) (assoc :fhir/flags flags)))))

(defn normalize-element [x]
  (-> (dissoc x
              :mapping :constraint :extension :comment :comments :requirements :definition :alias
              :meaningWhenMissing :isModifierReason)
      (normalize-binding)
      (normalize-require)
      (normalize-arity)
      (normalize-polymorphic)
      (normalize-content-ref)
      (normalize-flags)))


(defn normalize-description [res]
  (-> (dissoc res :description :short)
      (assoc :short (or (:short res) (:description res)))))
;; ADD check by http://www.hl7.org/fhir/elementdefinition.html#interpretation


(defn *normalize-extension [ext res]
  (if-let [complex (get-in res [:| :extension :slicing :slices])]
    (-> (assoc res :| (->> complex
                           (reduce (fn [acc [k v]]
                                     (assert (= (name k) (:sliceName v)) (pr-str :slice-name k (:sliceName v)))
                                     (assoc acc k (*normalize-extension ext (dissoc v :sliceName))))
                                   {}))
               :fhir/extension (get-in res [:| :url :fixedUri]))
        (dissoc :fhir-poly-keys))
    (if-let [value (get-in res [:| :value] )]
      (let [types (:types value)]
        (cond
          (= 1 (count types))
          (merge (dissoc res :| :fhir-poly-keys)
                 {:type (first types)})
          (< 1 (count types))
          (merge
            (dissoc res :| :fhir-poly-keys)
            (dissoc value :minItems :maxItems :vector :required :fhir-poly-keys))
          :else (assert false (pr-str :no-types res))))
      (let [values (dissoc (:| res) :url :extension)]
        (if (= 1 (count values))
          (merge (dissoc res :| :fhir-poly-keys)
                 (dissoc (first (vals values)) :minItems :maxItems :required))
          (assert false  (pr-str :extension-values (:url ext) values)))))))

(defn normalize-extension [res]
  (if-not (= "Extension" (:type res))
    res
    (assoc (*normalize-extension res res)
           :fhir/extension (:url res))))


(defn load-intermidiate [res]
  (->> (get-in res [:differential :element])
       (mapv normalize-element)
       (group-elements (select-keys res [:kind :derivation :baseDefinition :description :fhirVersion :type :url]))
       (normalize-description)
       (normalize-extension)
       (merge
         (when-let [package-ns (:zen.fhir/package-ns res)]
           {:zen.fhir/package-ns package-ns
            :zen.fhir/schema-ns (symbol (str (name package-ns) \. (:id res)))}))))


(defmulti process-on-load
  (fn [res] (keyword (:resourceType res))))


(defmethod process-on-load :default
  [res]
  #_(println :WARN :no-process-on-load :for (:resourceType res)))



(defmethod process-on-load :StructureDefinition
  [res]
  (load-intermidiate res))


(defn load-definiton [ztx packages header res]
  (if-let [rt (:resourceType res)]
    (if-let [url (:url header)]
      (swap! ztx update-in [:fhir/src rt url]
             (fn [x] (when x (println :WARN :override-resource header)) res))
      (println :WARN :no-url header))
    (println :WARN :no-resource-type header)))


(defn load-json-file [ztx package header f & [{:keys [params]}]]
  (let [res (-> (cheshire.core/parse-string (slurp f) keyword)
                (assoc :zen.fhir/header header :zen.fhir/package package :zen.fhir/file (.getPath f))
                (merge
                  {:zen.fhir/package-ns (symbol (:name package))}
                  (select-keys params #{:zen.fhir/package-ns})))]
    (load-definiton ztx package header res)))


(defn read-json [f] (cheshire.core/parse-string (slurp f) keyword))


(defn base-url [subj]
  (println(:type subj) (pr-str :no-type-in subj))
  (or (:baseDefinition subj)
      (str "http://hl7.org/fhir/StructureDefinition/" (:type subj))))


(defn get-definition [ztx url]
  (get-in @ztx [:fhir/inter "StructureDefinition" url]))


(defn get-type-definition [ztx type-name]
  (let [tp (if (str/starts-with? type-name "http://hl7.org/fhirpath/System.")
             (str (str/lower-case (subs type-name 31 32)) (subs type-name 32))
             type-name)
        definition (get-definition ztx (str "http://hl7.org/fhir/StructureDefinition/" tp))]
    (assert definition (str "Could not find type definition: " tp))
    definition))


(defn is-profile? [url subj]
  (and (= "constraint" (:derivation subj))
       (not (or (= "Extension" (:type subj))
                (:fhir/extension subj)))
       #_(not (= url (base-url subj)))))


(defn get-bases [ztx subj]
  (loop [base       (:baseDefinition subj)
         base-stack []
         bases      #{}]
    (if (or (nil? base)
            (contains? bases base))
      base-stack
      (let [base-def (get-definition ztx base)]
        (recur (:baseDefinition base-def)
               (conj base-stack base-def)
               (conj bases base))))))


(defn get-original [ztx url]
  (get-in @ztx [:fhir/src "StructureDefinition" url]))


(defn get-base-elements [ztx k el bases]
  (let [elements-stack bases ;;(cons el bases) ;; ????
        base-elements  (keep #(get-in % [:| k]) (reverse elements-stack))
        types          (cond-> (set (keep #(get-in % [:type]) base-elements))
                         (:type el) (conj (:type el)))
        types-defs     (map (partial get-type-definition ztx) types)]
    (not-empty (vec (concat base-elements types-defs)))))


(defn get-base-poly-key [ztx k bases]
  (some #(get-in % [:fhir-poly-keys k]) bases))


(defn enrich-element [el base-els]
  ;; TODO: if vector do min/max items
  ;;       required/prohibited
  ;;       tragetProfile type profile
  (let [v? (some :vector base-els)
        tp (or (:type el)
               (->> base-els
                    (filter (fn [{tp :type}] (and (not (nil? tp))
                                                  (not (= "Element" tp)))))
                    (some :type)))]
    (cond-> el
      v?       (assoc :vector true)
      (not v?) (dissoc :minItems :maxItems)
      tp (assoc :type tp))))


(defn search-base-elements [ztx k el bases]
  (if-let [b-els (get-base-elements ztx k el bases)]
    [k b-els]
    (let [fix-poly-k (keyword (decapitalize-first-letter (name k)))]
      (if-let [b-els (get-base-elements ztx fix-poly-k el bases)]
        [fix-poly-k b-els]
        [k]))))


;; one of the most complicated places now
(defn walk-with-bases [ztx ctx subj bases]
  (let [subj (enrich-element subj bases)]
    (if (empty? (:| subj))
      subj
      (update subj :|
              #(reduce (fn [acc [k el]]
                         (if (and (= :extension k) (not (:element-definition? ctx)))
                           (->> (get-in el [:slicing :slices])
                                (reduce (fn [acc [ext-k ext-el]]
                                          (assert (= ext-k (keyword (:sliceName ext-el))) (pr-str ext-k "!=" (:sliceName ext-el)))
                                          (assoc acc ext-k (dissoc ext-el :type :sliceName)))
                                        acc))
                           (let [[k base-els] (search-base-elements ztx k el bases)]
                             (if base-els
                               (assoc acc k
                                      (let [new-ctx (-> (update ctx :lvl inc) (update :path conj k))]
                                        (walk-with-bases ztx new-ctx el base-els)))
                               (if-let [{poly-key :key, poly-type :type} (get-base-poly-key ztx k bases)]
                                 (let [poly-el  {:| {(keyword poly-type) (assoc el :type poly-type)}}
                                       base-els (get-base-elements ztx poly-key poly-el bases)
                                       new-ctx  (-> (update ctx :lvl inc) (update :path conj poly-key))]
                                   (assoc acc poly-key (walk-with-bases ztx new-ctx poly-el base-els)))
                                 (if (= "specialization" (:derivation ctx))
                                   (assoc acc k
                                          (let [new-ctx (-> (update ctx :lvl inc) (update :path conj k))]
                                            (walk-with-bases ztx new-ctx el [])))
                                   (do
                                     (println :WARN :no-base (conj (:path ctx) k) el)
                                     (assoc acc k
                                            (let [new-ctx (-> (update ctx :lvl inc) (update :path conj k))]
                                              (walk-with-bases ztx new-ctx el []))))))))))
                       {}
                       %)))))


(defn is-extension?
  [_url subj]
  (= "Extension" (:type subj)))


(defn process-extension
  [ztx url subj]
  subj)


(defn collect-extension-profiles [acc path v]
  (if-let [url (:fhir/extension v)]
    (update-in acc [:extensions url] (comp vec distinct concat) [path])
    acc))


(defn collect-types [acc path v]
  (reduce (fn [acc' el-type]
            (update-in acc'
                       [:types (str "http://hl7.org/fhir/StructureDefinition/" el-type)]
                       (comp vec distinct concat) [path]))
          acc
          (cons (:type v) (:types v))))


(defn collect-references [acc path v]
  (reduce (fn [acc' profile-url]
            (update-in acc' [:references profile-url] (comp vec distinct concat) [path]))
          acc
          (:profiles v)))


(defn collect-valuesets [acc path v]
  (let [value-set-url (get-in v [:binding :valueSet])]
    (update-in acc [:value-sets value-set-url] (comp vec distinct concat) [path])))


(defn collect-nested [acc path subj]
  (letfn [(collect-element [path-fn acc [k v]]
            (let [new-path (path-fn path k)]
              (-> acc
                  (collect-nested new-path v)
                  (collect-extension-profiles new-path v)
                  (collect-types new-path v)
                  (collect-references new-path v)
                  (collect-valuesets new-path v))))]
    (as-> acc acc
      (reduce (partial collect-element (fn [path k] (-> (butlast path) vec (conj k))))
              acc
              (:slice subj))
      (reduce (partial collect-element (fn [path k] (conj path k)))
              acc
              (:| subj)))))


(defn collect-deps [sd-processed]
  (-> {:structure-definitions {(:baseDefinition sd-processed) [[]]}}
      (collect-nested [] sd-processed)))


(defn process-sd [ztx url subj]
  (let [processed-sd
        (cond
          (is-extension? url subj)
          (process-extension ztx url subj)

          ;; (is-profile? url subj)
          :else
          (let [bases (get-bases ztx subj)]
            (when (= "constraint" (:derivation subj))
              (println (pr-str :WARN :no-base url)))
            (walk-with-bases ztx {:lvl 0 :path [url] :derivation (:derivation subj)
                                  :element-definition? (= "http://hl7.org/fhir/StructureDefinition/Element" (:url subj))}
                             subj bases)))]
    (assoc processed-sd :deps (collect-deps processed-sd))))


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
  (swap! ztx assoc :fhir/inter
         (sp/transform [sp/MAP-VALS sp/MAP-VALS]
                       process-on-load
                       (:fhir/src @ztx))))


(defn process-resources
  "this is processing of resources with context"
  [ztx]
  (process-structure-definitions ztx))


(defn load-all [ztx package & [{:keys [params]}]]
  (doseq [pkg-dir (.listFiles (io/file "node_modules"))
          :when   (and (.isDirectory pkg-dir)(not (str/starts-with? (.getName pkg-dir) ".")))
          :let    [package (read-json (str (.getPath pkg-dir) "/package.json"))
                   index   (read-json (str (.getPath pkg-dir) "/.index.json"))
                   package-params (get params (:name package))]

          {filename :filename :as header} (:files index)]
    (load-json-file ztx package header
                    (io/file (str (.getPath pkg-dir) "/" filename))
                    {:params package-params}))
  (preprocess-resources ztx)
  (process-resources ztx)
  :done)


;; 1. depency to generate import in zen (profile, type, extension, valuesets)
;; 2. extensions as first class
;; 3. polymoric shortcats - valueQuantity -> value.Quantity
;; 4. P.meta. <- BP <- DomainResource.meta...
;; 5. Slicing on arrays (filter, matcho)

;; Generation
;; 1. load into intermidiate
;; 2. Dump to zen-schema
;; * dump only specific package or dump all
;; * one SD - one ns
;;    * -  us-core.v2.Patient/schema
;;    * -  us-core.v2.Patient/Patient
;;    * -  us-core.v2.Patient/resource
;; *  url of module to namespace (transformation?)
{'ns 'us-core.v2.Patient
 'import #{'fhir/resource 'fhir.r4.Patient/schema}
 'schema {:zen/tags #{'fhir/resource}
          :confirms #{ 'fhir.r4.Patient/schema}
          :keys {:race {:confirms #{'us-core.v2.patient-race/schema}
                        :fhir/extension "http://"}}

          }

}

;; us-core.v2 {imports all us-core}
{'ns 'us-core.v2.ValueSet

 'valueset
 {:zen/tags #{'fhir/valueset}


  }

 }
