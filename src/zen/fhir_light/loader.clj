(ns zen.fhir-light.loader
  (:require [clojure.string :as str]
            [zen.v2-validation]
            [zen.fhir.utils :as utils]))


(def elements-keys-types
  {:zf/loc         #{:id :path}
   :zf/validation  #{:base :binding :condition :constraint :contentReference
                     :max :maxLength :min :sliceIsConstraining :sliceName :slicing :type}
   :zf/meta        #{:isModifier :isSummary :mustSupport :representation}
   :zf/description #{:alias :code :comment :definition :example :isModifierReason
                     :label :mapping :orderMeaning :requirements :short}})


(def elements-poly-keys-types
  {:zf/validation  #{:fixed :maxValue :minValue :pattern}
   :zf/meta        #{:default}})


(defn- group-element-keys [element]
  (utils/strip-when empty?
                    (merge-with merge
                                (update-vals elements-keys-types
                                             #(select-keys element %))
                                (update-vals elements-poly-keys-types
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


(def validation-keys-types
  {:zf/value     #{:type :binding :contentReference :maxLength :base}
   :zf/container #{:max :min :sliceIsConstraining :sliceName :slicing :base}
   :zf/outer     #{:max :min :condition :base}
   :zf/context   #{:constraint}})


(defn- validation->zen-schema-parts [validation]
  (->> (update-vals validation-keys-types
                    #(not-empty (select-keys validation %)))
       utils/strip-nils
       not-empty))


(defn- add-schema-parts [grouped-el]
  (utils/assoc-some
    grouped-el
    :schema-parts
    (validation->zen-schema-parts (:zf/validation grouped-el))))


(defn- parsed-id->nested-path [parsed-id]
  (vec (mapcat (fn [id-el]
                 (case (:type id-el)
                   :root      []
                   :key       [:zf/els (:key id-el)]
                   :poly-root [:zf/poly-roots (:poly-root id-el)]
                   :poly-key  [:zf/poly-keys (:poly-key id-el)]
                   :slice     [:zf/slicing :zf/slices (:slice id-el)]))
               parsed-id)))


(defmulti el-part&path
  (fn [_parsed-id [keys-group _el-part]]
    keys-group))


(defmethod el-part&path :default [parsed-id [keys-group el-part]]
  [{:el-part el-part
    :path    (conj (parsed-id->nested-path parsed-id) keys-group)}])


(defmulti schema-part-path
  (fn [_parsed-id [schema-keys-type _schema-part]]
    schema-keys-type))


(defmethod el-part&path :schema-parts [parsed-id [_ schema-parts]]
  (map (fn [e] {:el-part (val e)
                :path (schema-part-path parsed-id e)})
       schema-parts))


(defmethod schema-part-path :zf/value [parsed-id [_ schema-part]]
  (conj (parsed-id->nested-path parsed-id) :zf/value))


(defmethod schema-part-path :zf/container [parsed-id [_ schema-part]]
  (conj (parsed-id->nested-path parsed-id) :zf/container (last parsed-id)))


(defmethod schema-part-path :zf/outer [parsed-id [_ schema-part]]
  (let [outer-id (->> parsed-id
                      reverse
                      rest
                      (drop-while #(not= :key (:type %)))
                      reverse)
        outer-path (parsed-id->nested-path outer-id)]
    (conj outer-path :zf/els-constraints (last parsed-id))))


(defmethod schema-part-path :zf/context [parsed-id [_ schema-part]]
  [:zf/context parsed-id])


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
               (when-let [stripped-el (strip-el el params)]
                 (mapcat #(el-part&path (get-in el [:zf/loc :zf/id]) %)
                         stripped-el))))
     (completing (fn [acc {:keys [el-part path]}]
                   (assoc-in acc (cons :result path) el-part)))
     {:result {}}
     enriched-elements)))


(defn- els-constraints->zen
  "{<source :zf/id> #{:max :min :condition :base}}"
  [els-constraints]
  (merge
    (when-let [requires (seq (filter #(some->> (:min (val %)) (< 0))
                                     els-constraints))]
      {:type 'zen.fhir/element
       :zen.fhir/require (into #{}
                               (map #(:key (key %)))
                               requires)})

    (when-let [forbids (seq (filter #(= 0 (:max (val %)))
                                    els-constraints))]
      {:type 'zen.fhir/element
       :zen.fhir/forbid (into #{}
                              (map #(:key (key %)))
                              forbids)})))


(defn- container->zen
  "#{:max :min :sliceIsConstraining :sliceName :slicing :base}"
  [{min-card :min max-card :max}]
  (merge
    (when (and min-card (< 0 min-card))
      {:type 'zen.fhir/element
       :zen.fhir/min min-card})
    (when max-card
      (if (= "*" max-card)
        {:type 'zen.fhir/element
         :zen.fhir/collection true}
        {:type 'zen.fhir/element
         :zen.fhir/max max-card}))))


(defn- slicing->zen
  "{:slices {\"<slice name>\" <nested>}}"
  [slicing])


(defn- poly-roots->zen
  "{\"<poly root name>\" <nested>}"
  [poly-roots])


(defn- poly-keys->zen
  "{\"<poly key name>\" <nested>}"
  [poly-keys])


(defn- value->zen
  "#{:type :binding :contentReference :maxLength :base}"
  [value]
  (when (some? value)
    (merge (when-let [max-length (:maxLength value)]
             {:type 'zen/string
              :maxLength max-length}))))


(defn- context->zen
  "{<source :zf/id> #{:constraint}}"
  [context])


(defn- meta->zen
  "#{:isModifier :isSummary :mustSupport :representation}"
  [meta-data])


(defn description->zen
  "#{:alias :code :comment :definition :example :isModifierReason
     :label :mapping :orderMeaning :requirements :short}"
  [description])


(defn- nested->zen* [{:as nested
                      meta-data :zf/meta
                      :zf/keys [description loc
                                context container
                                els els-constraints
                                poly-roots poly-keys
                                slicing value]}]
  (merge
    (els-constraints->zen els-constraints)
    (container->zen container)
    (slicing->zen slicing)
    (poly-roots->zen poly-roots)
    (poly-keys->zen poly-keys)
    (value->zen value)
    (context->zen context)
    (meta->zen meta-data)
    (description->zen description)

    (when (some? els)
      (when-let [elements (-> (update-vals els nested->zen*)
                              utils/strip-nils
                              not-empty)]
        {:type 'zen.fhir/element
         :zen.fhir/elements elements}))))


(defn- nested->zen [nested]
  (merge
    {:zen/tags #{'zen/schema}}
    (nested->zen* nested)))


(defn strdef->zen [strdef]
  (->> (get-in strdef [:differential :element])
       (map group-element-keys)
       (map enrich-loc)
       (map add-schema-parts)
       nest-by-enriched-loc
       nested->zen))
