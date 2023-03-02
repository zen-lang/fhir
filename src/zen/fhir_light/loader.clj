(ns zen.fhir-light.loader
  (:require [clojure.string :as str]
            [zen.v2-validation]
            [zen.fhir.utils :as utils]))


(defn- safe-deep-merge
  ([] nil)
  ([a] a)
  ([a b]
   (cond
     (= a b)
     a

     (and (or (nil? a) (map? a)) (or (nil? b) (map? b)))
     (merge-with safe-deep-merge a b)

     :else
     (throw (ex-info "Can't merge not maps. Overwriting values is not allowed"
                     {:a a
                      :b b}))))
  ([a b & maps]
   (reduce safe-deep-merge
           a
           (cons b maps))))


(def elements-keys-types
  {:zf/loc          #{:id :path}

   :zf/value        #{:type :binding :contentReference :maxLength :base}
   :zf/container    #{:max :min :sliceIsConstraining :sliceName :slicing :base}
   :zf/outer        #{:max :min :condition :base}
   :zf/context      #{:constraint}

   :zf/meta         #{:isModifier :isSummary :mustSupport :representation}
   :zf/description  #{:alias :code :comment :definition :example :isModifierReason
                      :label :mapping :orderMeaning :requirements :short}})


(def elements-poly-keys-types
  {:zf/value #{:fixed :maxValue :minValue :pattern}
   :zf/meta  #{:default}})


(defn- group-keys [element keys-types poly-keys-types]
  (utils/strip-when empty?
                    (merge-with merge
                                (update-vals keys-types
                                             #(select-keys element %))
                                (update-vals poly-keys-types
                                             #(into {}
                                                    (mapcat (fn [pk] (utils/poly-find-all element pk)))
                                                    %)))))


(def ^:private poly-postfix "[x]")


(defn- drop-poly-terminator [id]
  (subs id 0 (- (count id) (count poly-postfix))))


(defn- poly-root? [key-part]
  (str/ends-with? key-part poly-postfix))


(defn- parse-id-part [id-part]
  (let [[key-part slice-part] (str/split id-part #":" 2)]
    (cond
      (poly-root? key-part)
      (let [poly-root (drop-poly-terminator key-part)]
        (if (some? slice-part)
          [{:type :poly-key, :poly-key (keyword slice-part) :poly-root poly-root}]
          [{:type :poly-root, :poly-root poly-root}]))

      (some? slice-part)
      [{:type :key, :key (keyword key-part)}
       {:type :slice, :slice slice-part}]

      :else
      [{:type :key, :key (keyword key-part)}])))


(defn- parse-id [id]
  (if (str/blank? id)
    []
    (let [id-parts (str/split (str id) #"\.")]
      (vec (cons {:type :root, :root (first id-parts)}
                 (mapcat parse-id-part (rest id-parts)))))))


(defn- parsed-id->nested-path [parsed-id]
  (mapcat
    (fn [id-el]
      (case (:type id-el)
        :root      []
        :key       [:els (:key id-el)]
        :poly-root [:poly-roots (:poly-root id-el)]
        :poly-key  [:poly-keys (:poly-key id-el)]
        :slice     [:slicing :slices (:slice id-el)]))
    parsed-id))


(defn- enrich-loc [grouped-element]
  (let [id        (get-in grouped-element [:zf/loc :id])
        parsed-id (parse-id id)]
    (assoc-in grouped-element [:zf/loc :zf/id] parsed-id)))


(defn- parsed-id->nested-path [parsed-id]
  (vec (mapcat (fn [id-el]
                 (case (:type id-el)
                   :root      []
                   :key       [:zf/els (:key id-el)]
                   :poly-root [:zf/poly-roots (:poly-root id-el)]
                   :poly-key  [:zf/poly-keys (:poly-key id-el)]
                   :slice     [:zf/slicing :zf/slices (:slice id-el)]))
               parsed-id)))


(defn- el-part-path [parsed-id elements-keys-type]
  (case elements-keys-type
    :zf/value     (conj (parsed-id->nested-path parsed-id) :zf/value)
    :zf/container (conj (parsed-id->nested-path parsed-id) :zf/container)
    :zf/outer     (let [outer-id   (->> parsed-id
                                        reverse
                                        rest
                                        (drop-while #(not= :key (:type %)))
                                        reverse)
                        outer-path (parsed-id->nested-path outer-id)]
                    (conj outer-path :zf/els-constraints (last parsed-id)))
    :zf/context   [:zf/context parsed-id]
    (conj (parsed-id->nested-path parsed-id) elements-keys-type)))


(defn- strip-el [el & {:keys [keys-to-select keys-to-strip]}]
  (not-empty
    (cond-> el
      (seq keys-to-strip)
      (as-> $ (apply dissoc $ keys-to-strip))

      (seq keys-to-select)
      (select-keys keys-to-select))))


(defn- nest-by-enriched-loc [enriched-elements & {:as params}]
  (:result
   (transduce
     (mapcat (fn [el]
               (-> (strip-el el params)
                   (update-keys #(el-part-path (get-in el [:zf/loc :zf/id]) %)))))
     (completing (fn [acc [path el-part]]
                   (assoc-in acc (cons :result path) el-part)))
     {:result {}}
     enriched-elements)))


(declare nested->zen*)

(defn- els->zen
  "{<key> <nested>}"
  [ctx els]
  (when (some? els)
    (let [keys-acc (update-vals els #(nested->zen* ctx %))

          key-schemas (-> keys-acc
                          (update-vals :zf/schema)
                          utils/strip-nils
                          not-empty)

          rest-acc (->> keys-acc
                        vals
                        (keep #(dissoc % :zf/schema))
                        (apply safe-deep-merge))]
      (cond-> rest-acc
        key-schemas
        (assoc :zf/schema
               {:type 'zen.fhir/element
                :zen.fhir/el
                {:type 'zen/map
                 :keys key-schemas}})))))


(defn- els-constraints->zen
  "{<source :zf/id> #{:max :min :condition :base}}"
  [ctx els-constraints]
  {:zf/schema
   (merge
     (when-let [requires (seq (filter #(some->> (:min (val %))
                                                (< 0))
                                      els-constraints))]
       {:type 'zen.fhir/element
        :zen.fhir/el {:type 'zen/map
                      :require (into #{}
                                     (map #(:key (key %)))
                                     requires)}})

     #_(when-let [forbids (seq (filter #(= "0" (:max (val %)))
                                       els-constraints))]
         {:type 'zen.fhir/element
          :zen.fhir/el {:type 'zen/map
                        :forbid (into #{}
                                      (map #(:key (key %)))
                                      forbids)}}))})


(defn- container->zen
  "#{:max :min :sliceIsConstraining :sliceName :slicing :base}"
  [ctx {min-card :min max-card :max}]
  {:zf/schema
   (merge
     (when (and min-card (< 0 min-card))
       {:type 'zen.fhir/element
        :zen.fhir/min min-card})
     (when max-card
       (if (= "*" max-card)
         {:type 'zen.fhir/element
          :zen.fhir/collection true}
         {:type 'zen.fhir/element
          :zen.fhir/max (parse-long max-card)})))})


(defn- slicing->zen
  "{:slices {\"<slice name>\" <nested>}}"
  [ctx slicing])


(defn- poly-roots->zen
  "{\"<poly root name>\" <nested>}"
  [ctx poly-roots])


(defn- poly-keys->zen
  "{\"<poly key name>\" <nested>}"
  [ctx poly-keys])


(def fhir-sequence->version-mapping
  {"r5" #{"5.0.0-draft-final"
          "5.0.0-snapshot3"
          "5.0.0-ballot"
          "5.0.0-snapshot1"
          "4.6.0"
          "4.5.0"
          "4.4.0"
          "4.2.0"}

   "r4b" #{"4.3.0"
           "4.3.0-snapshot1"
           "4.1.0"}

   "r4" #{"4.0.1"
          "4.0.0"
          "3.5a.0"
          "3.5.0"
          "3.3.0"
          "3.2.0"}

   "stu3" #{"3.0.2"
            "3.0.1"
            "3.0.0"
            "1.8.0"
            "1.6.0"
            "1.4.0"
            "1.2.0"
            "1.1.0"}})


(def fhir-version->sequence-mapping
  (into {}
        (for [[fhir-sequence versions] fhir-sequence->version-mapping
              version                  versions]
          [version fhir-sequence])))


(defn mk-type-binding [fhir-sequence fhir-version type-code]
  (let [sym (symbol (str "zen.fhir.bindings.fhir-" fhir-sequence ".types/" type-code))
        zen-def {:zen/tags #{'zen/schema 'zen/binding 'zen.fhir/type-binding}
                 :fhirSequence fhir-sequence
                 :fhirVersion fhir-version
                 :code type-code}]
    {:zf/symbol sym
     :zf/binding {sym zen-def}}))


(defn- value->zen
  "#{:type :binding :contentReference :maxLength :base}"
  [ctx value]
  (when (some? value)
    (merge
      (when-let [types (seq (:type value))]
        (let [fhir-version  (get-in ctx [:zf/strdef :zf/meta :fhirVersion])
              fhir-sequence (fhir-version->sequence-mapping fhir-version)
              bindings      (->> types
                                 (keep :code)
                                 (map #(mk-type-binding fhir-sequence fhir-version %)))
              binding-syms  (into #{} (map :zf/symbol) bindings)
              binding-defs  (into {} (map :zf/binding) bindings)]
          (when (seq bindings)
            {:zf/bindings binding-defs
             :zf/schema {:type 'zen.fhir/element
                         :zen.fhir/el {:confirms binding-syms}}})))
      #_(when-let [max-length (:maxLength value)]
          {:type 'zen/string
           :maxLength max-length}))))


(defn- context->zen
  "{<source :zf/id> #{:constraint}}"
  [ctx context])


(defn- meta->zen
  "#{:isModifier :isSummary :mustSupport :representation}"
  [ctx meta-data])


(defn- description->zen
  "#{:alias :code :comment :definition :example :isModifierReason
     :label :mapping :orderMeaning :requirements :short}"
  [ctx description])


(defn- nested->zen*
  "#{:zf/description :zf/loc :zf/meta
     :zf/context :zf/els :zf/els-cnstraints
     :zf/slicing :zf/container :zf/value
     :zf/poly-roots :zf/poly-keys}"
  [ctx nested]
  (safe-deep-merge
    (els->zen ctx (:zf/els nested))
    (els-constraints->zen ctx (:zf/els-constraints nested))
    (container->zen ctx (:zf/container nested))
    (slicing->zen ctx (:zf/slicing nested))
    (poly-roots->zen ctx (:zf/poly-roots nested))
    (poly-keys->zen ctx (:zf/poly-keys nested))
    (value->zen ctx (:zf/value nested))
    (context->zen ctx (:zf/context nested))
    (meta->zen ctx (:zf/meta nested))
    (description->zen ctx (:zf/description nested))))


(defn- nested->zen [ctx nested]
  (-> (nested->zen* ctx nested)
      (update :zf/schema merge {:zen/tags #{'zen/schema}})))


(def strdef-keys-types
  {:zf/meta #{:resourceType :url
              :type :kind :derivation :abstract
              :experimental :status
              :fhirVersion  :version :date  :context :contextInvariant}

   :zf/description #{:name :title :description :purpose :useContext
                     :publisher :contact :jurisdiction :copyright
                     :keyword :identifier :mapping}})


(defn symbols-map->zen-nss [symbols-map]
  (->> symbols-map
       (group-by #(namespace (key %)))
       (map (fn [[zen-ns symbols]]
              (into {:ns (symbol zen-ns)
                     :import #{'zen.fhir}}
                    (map (fn [[qsym sym-def]]
                           [(symbol (name qsym))
                            sym-def]))
                    symbols)))))


(defn strdef->zen-ns [strdef]
  (let [grouped-strdef (group-keys strdef strdef-keys-types nil)
        nested-els (->> (get-in strdef [:differential :element])
                        (map #(group-keys %
                                          elements-keys-types
                                          elements-poly-keys-types))
                        (map enrich-loc)
                        nest-by-enriched-loc)
        zen (nested->zen {:zf/strdef grouped-strdef} nested-els)
        zen (assoc zen :zf/bindings-ns (symbols-map->zen-nss (:zf/bindings zen)))]
    zen))
