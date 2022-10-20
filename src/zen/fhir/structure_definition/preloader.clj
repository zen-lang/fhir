(ns zen.fhir.structure-definition.preloader
  (:require [clojure.string :as str]
            [zen.fhir.utils :as utils]
            [clojure.walk]
            [com.rpl.specter :as sp]))

(declare normalize-slicing)

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
                                     {:key slice-part :type :slice}]
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

(defn build-fhir-poly-keys-mapping [poly-key types]
  (into {}
        (map (fn [el-type]
               (let [fhir-poly-key (str poly-key (capitalize-first-letter el-type))]
                 (-> {(keyword fhir-poly-key)
                      {:key (keyword poly-key)
                       :type el-type}}))))
        types))


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

(defn root-element? [el-path]
  (not (str/includes? (str el-path) ".")))

(defn reference-profiles [el]
  (let [tp       (first (:type el))
        tpc      (:code tp)
        prof     (:targetProfile tp)
        profiles (if (string? prof) [prof] prof)]
    (if (and (= tpc "Reference") profiles)
      (assoc el :profiles (into #{} profiles))
      el)))

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

(defn extension-profiles [el]
  (if-let [ext-profs (:profile (first (:type el)))]
    (let [ext-profs (if (string? ext-profs) [ext-profs] ext-profs)]
      (do
        (assert (= 1 (count ext-profs)) (pr-str :unexpected-extension-profiles (:type el)))
        (assoc el :fhir/extension (first ext-profs))))
    el))

(defn normalize-polymorphic [el & [stu3?]]
  (if (str/ends-with? (str (:id el)) "[x]")
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
      (cond
        stu3?
        ;; In STU3 type is an array of entries each of which can contain
        ;; a `profile` or a `targetProfile` (or both).
        ;; In R4 each type can contain an array of `profile`s
        ;; or an array or `targetProfile`-s (or both)
        (let [profiles (mapv :profile (:type el))
              target-profiles (mapv :targetProfile (:type el))
              type' (-> (first (:type el))
                        (assoc :profile profiles
                               :targetProfile target-profiles)
                        (utils/sanitize-obj))
              el' (assoc el :type [type'])]
          (normalize-polymorphic el'))

        (= 1 (count (:type el)))
        (let [tp  (first (:type el))
              tpc (get-type-code tp)]
          (-> el
              (reference-profiles)
              (extension-profiles)
              (assoc :type tpc)))

        :else ;; NOTE: allowed by FHIR but not implemented.
        (throw (Exception. (pr-str el)))))))

(defn normalize-require [{:as element, el-min :min}]
  (if (pos? (or el-min 0))
    (assoc element :required true)
    element))

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


(defn parse-canonical-url [canonical-url]
  (when-not (str/blank? canonical-url)
    (let [parts   (str/split canonical-url #"\|")
          url     (str/join "|" (cons (first parts) (butlast (rest parts))))
          version (last (rest parts))]
      (not-empty (utils/strip-nils {:url url, :version version})))))

(defn binding->canonical
  "FHIR R4 only allows canonical in valueSet
  In STU3 it is polymorphic with either valueSetUri or valueSetReference field"
  [binding]
  (or (get-in binding [:valueSet])
      (get-in binding [:valueSetUri])
      (get-in binding [:valueSetReference :reference])))

(defn normalize-binding [el]
  (if-let [bn (:binding el)]
    (assoc el :binding (-> bn
                           (dissoc :extension)
                           (dissoc :valueSet :valueSetUri :valueSetReference)
                           (assoc :valueSet (parse-canonical-url (binding->canonical bn)))))
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

(defn normalize-nested [x]
  (if (= "Resource" (:type x))
    (assoc x :nested true)
    x))


(defn normalize-fixed [res]
  (if-let [fixed (utils/poly-get res :fixed)]
    (assoc res :fixed fixed)
    res))

(defn normalize-pattern [res]
  (if-let [pattern (utils/poly-get res :pattern)]
    (assoc res :pattern pattern)
    res))

(defn remove-unhandled-fields
  "Remove fields that could not be handled by zen fhir"
  [structure-definition]
  (dissoc structure-definition
          :mapping :constraint :extension :comment :comments :requirements :definition :alias
          :meaningWhenMissing :isModifierReason))

(defn normalize-element [x & [stu3?]]
  (-> (remove-unhandled-fields x)
      (normalize-binding)
      (normalize-require)
      (normalize-arity)
      (normalize-polymorphic stu3?)
      (normalize-nested)
      (normalize-content-ref)
      (normalize-flags)
      (normalize-fixed)
      (normalize-pattern)))


(defn normalize-description [res]
  (-> (dissoc res :description :short)
      (assoc :text-description (or (:short res) (:description res)))))
;; ADD check by http://www.hl7.org/fhir/elementdefinition.html#interpretation

(defn *normalize-extension [ext res]
  (cond
    (get-in res [:| :extension :slicing :slices]) ;; slices for different extensions
    (-> (assoc res
               :fhir/extension (get-in res [:| :url :fixedUri])
               :| (->> (get-in res [:| :extension :slicing :slices])
                       (reduce (fn [acc [k v]]
                                 (when-not (= k (keyword (:sliceName v)))
                                   (prn "WARN:" (pr-str k "!=" (:sliceName v))))
                                 (assoc acc k (*normalize-extension ext (dissoc v :sliceName))))
                               {})))
        (dissoc :fhir-poly-keys)
        (cond->
          (= "http://hl7.org/fhir/StructureDefinition/Extension" (:baseDefinition res))
          (dissoc :baseDefinition)))

    (= 1 (count (get-in res [:| :value :types]))) ;; value[x] with a single type
    (let [value (get-in res [:| :value])
          tp    (first (:types value))]
      (merge (dissoc res :| :fhir-poly-keys :baseDefinition)
             (dissoc value :| :types :minItems :maxItems :required :polymorphic)
             (dissoc (first (vals (:| value))))
             {:kind "first-class-extension"
              :baseDefinition (str "http://hl7.org/fhir/StructureDefinition/" tp)}))

    (< 1 (count (get-in res [:| :value :types]))) ;; value[x] with multile types
    (merge
      (dissoc res :| :fhir-poly-keys :baseDefinition :minItems :maxItems)
      (dissoc (get-in res [:| :value]) :fhir-poly-keys))

    (and (= 1 (count (dissoc (:| res) :url :extension)))
         (= 1 (count (get-in res [:| :value :|]))))
    (let [value (first (vals (get-in res [:| :value :|])))]
      (merge (dissoc res :| :fhir-poly-keys :baseDefinition) ;; baseDefinition here is http://.../Extension, thus dissoc
             (dissoc value :minItems :maxItems :required :polymorphic :sliceName)
             {:kind "first-class-extension"
              :baseDefinition (str "http://hl7.org/fhir/StructureDefinition/" (:type value))}))

    (and (= 1 (count (dissoc (:| res) :url :extension)))
         (< 1 (count (get-in res [:| :value :|]))))
    (assert false (pr-str :not-supported res))

    (= 1 (count (dissoc (:| res) :url :extension))) ;; extension with a single value
    (let [value (first (vals (dissoc (:| res) :url :extension)))]
      (merge (dissoc res :| :fhir-poly-keys :baseDefinition) ;; baseDefinition here is http://.../Extension, thus dissoc
             (dissoc value :minItems :maxItems :required :polymorphic)
             {:kind "first-class-extension"
              :baseDefinition (str "http://hl7.org/fhir/StructureDefinition/" (:type value))})) ;; making correct baseDefinition

    (and (get-in res [:| :value]) ;; has value[x], but no types in it
         (empty? (get-in res [:| :value :types])))
    (assert false (pr-str :no-types res))

    (and (= "Extension" (:type res)) ;; nested extension
         (contains? res :fhir/extension)
         (empty? (:| res)))
    (dissoc res :type)

    :else
    (assert false (pr-str :extension-values (:url ext) (dissoc (:| res) :url :extension)))))

(defn normalize-extension [res]
  (if (and (= "Extension" (:type res))
           (not= "http://hl7.org/fhir/StructureDefinition/Extension" (:url res)))
    (assoc (*normalize-extension res res)
           :fhir/extension (:url res))
    res))


(def preprocess-slices-by-discriminator-dispatch
  (fn [_slices discriminator]
    (keyword (:type discriminator))))

(defmulti preprocess-slices-by-discriminator #'preprocess-slices-by-discriminator-dispatch)

(defn fix-slices-names [slices]
  (into {}
        (map (fn [[k v]] [(or (:sliceName v) (name k)) v]))
        slices))

(defn preprocess-slicing* [{:as el-with-slicing, :keys [slicing]}]
  (let [slices (reduce preprocess-slices-by-discriminator
                       (fix-slices-names (:slices slicing))
                       (:discriminator slicing))
        processed-slicing (-> slicing
                              (assoc :slices slices)
                              (dissoc :discriminator))]
    (-> el-with-slicing
        (dissoc :slicing)
        (assoc :fhir/slicing processed-slicing))))

(def supported-discriminator-type
  (set (keys (methods preprocess-slices-by-discriminator))))

(defn preprocess-slicing [[k v]]
  (if (and (contains? v :slicing)
           (not= :extension k) #_"NOTE: slicing in extensions is processed differently")
    (if-let [v' (and (seq (get-in v [:slicing :discriminator]))
                     (every? (comp supported-discriminator-type keyword :type)
                             (get-in v [:slicing :discriminator]))
                     (preprocess-slicing* v))]
      [k (normalize-slicing v')]
      (when-let [sliceless-v (not-empty (dissoc v :slicing))]
        [k sliceless-v]))
    [k (normalize-slicing v)]))

(defn normalize-slicing [res]
  (cond-> res
    (contains? res :|) (update :| (partial into {} (keep preprocess-slicing)))
    :always            (as-> $ (utils/dissoc-when empty? $ :|))

    (contains? res :slicing) (update-in [:slicing :slices] (partial into {} (keep preprocess-slicing)))
    :always                  (as-> $ (utils/dissoc-when (comp empty? :slices) $ :slicing))

    (contains? res :fhir/slicing) (update-in [:fhir/slicing :slices] (partial into {} (keep preprocess-slicing)))
    :always                       (as-> $ (utils/dissoc-when (comp empty? :slices) $ :fhir/slicing))))

(defn is-stu3? [structure-definition]
  (some-> (:fhirVersion structure-definition)
          (str/starts-with? "3")))

(defn get-differential
  "Get differential definition which is .differential.element"
  [structure-definition]
  (get-in structure-definition [:differential :element]))

(defn load-intermidiate [res]
  (let [stu3? (is-stu3? res)]
    (->> (get-differential res)
         (mapv #(normalize-element % stu3?))
         (group-elements (select-keys res [:kind :abstract :derivation
                                           :baseDefinition :description :fhirVersion :type :url]))
         (normalize-description)
         (normalize-extension)
         (normalize-slicing)
         (merge
          (when-let [package-ns (:zen.fhir/package-ns res)]
            {:zen.fhir/package-ns package-ns
             :zen.fhir/schema-ns (symbol (str (name package-ns) \. (:id res)))})))))

(defn rich-parse-path-full [id]
  (if (str/blank? id)
    []
    (->> (str/split id #"\.")
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
                                     {:key slice-part :type :slice}]
                 :else              [{:key key-part
                                      :type :key}]))))
         vec)))

(defn pattern->zen-match [k pattern]
  (if (= :patternCanonical k)
    (let [{:keys [url version]} (parse-canonical-url pattern)]
      (if (and url version)
        (list :zen.match/one-of #{url pattern})
        url))
    (clojure.walk/postwalk
      (fn [x]
        (if (and (sequential? x) (not (map-entry? x)))
          (set x)
          x))
      pattern)))

(defn slice-discriminator->match [slices discriminator d-type-key]
  (let [rich-path  (->> (rich-parse-path-full (:path discriminator))
                        (remove (fn [path-el]
                                  (assert (= :key (:type path-el)))
                                  (= "$this" (:key path-el)))))
        inter-path (build-path rich-path)
        path       (mapv (comp keyword :key) rich-path)]
    (sp/transform [sp/MAP-VALS]
                  (fn [v]
                    (if-let [[pattern-k pattern] (some-> (get-in v inter-path) (utils/poly-find d-type-key))]
                      (let [match (cond->> (pattern->zen-match pattern-k pattern)
                                    (seq path)
                                    (assoc-in (:match v) path))]
                        (-> (if (seq path)
                              (update-in v path dissoc pattern-k)
                              (dissoc v pattern-k))
                            (assoc :match match)))
                      v))
                  slices)))

(defmethod preprocess-slices-by-discriminator :pattern [slices discriminator]
  (slice-discriminator->match slices discriminator :pattern))


(defmethod preprocess-slices-by-discriminator :value [slices discriminator]
  (slice-discriminator->match slices discriminator :fixed))
