(ns zen.fhir-light.loader
  (:require [clojure.string :as str]
            [zen.v2-validation]
            [zen.fhir.utils :as utils]))


(def elements-keys-types
  {:loc         #{:id :path}
   :validation  #{:base :binding :condition :constraint :contentReference
                  :max :maxLength :min :sliceIsConstraining :sliceName :slicing :type}
   :meta        #{:isModifier :isSummary :mustSupport :representation}
   :description #{:alias :code :comment :definition :example :isModifierReason
                  :label :mapping :orderMeaning :requirements :short}})


(def elements-poly-keys-types
  {:validation  #{:fixed :maxValue :minValue :pattern}
   :meta        #{:default}})


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
  (let [id        (get-in grouped-element [:loc :id])
        parsed-id (parse-id id)]
    (assoc-in grouped-element [:loc ::id] parsed-id)))


(def validation-keys-types
  {:value     #{:type :binding :contentReference :maxLength :base}
   :container #{:max :min :sliceIsConstraining :sliceName :slicing :base}
   :outer     #{:max :min :condition :base}
   :context   #{:constraint}})


(defmulti process-el-key
  (fn [keys-type [el-key _el-val]]
    [keys-type el-key]))


(defmethod process-el-key :default [_ entry] entry)


(defn- validation->zen-schema-parts [validation]
  (not-empty
    (into {}
          (keep (fn [[keys-type el-keys]]
                  (when-let [schema-part
                             (not-empty (into {}
                                              (map #(process-el-key keys-type %))
                                              (select-keys validation el-keys)))]
                    [keys-type schema-part])))
          validation-keys-types)))


(defn- add-schema-parts [grouped-el]
  (utils/assoc-some
    grouped-el
    :schema-parts
    (validation->zen-schema-parts (:validation grouped-el))))


(defmethod process-el-key [:outer :min] [_ [_ min-card]]
  (when (< 0 min-card)
    {::required true}))


(defmethod process-el-key [:container :min] [_ [_ min-card]]
  (when (< 0 min-card)
    {::min min-card}))


(defmethod process-el-key [:outer :max] [_ [_ max-card]]
  (when (= 0 max-card)
    {::forbidden true}))


(defmethod process-el-key [:container :max] [_ [_ max-card]]
  (if (= "*" max-card)
    {::collection true}
    {::max (parse-long max-card)}))


(defmethod process-el-key [:value :maxLength] [_ [_ max-length]]
  {::maxLength max-length})


(defn- parsed-id->nested-path [parsed-id]
  (vec (mapcat (fn [id-el]
                 (case (:type id-el)
                   :root      []
                   :key       [:els (:key id-el)]
                   :poly-root [:poly-roots (:poly-root id-el)]
                   :poly-key  [:poly-keys (:poly-key id-el)]
                   :slice     [:slicing :slices (:slice id-el)]))
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


(defmethod schema-part-path :value [parsed-id [_ schema-part]]
  (conj (parsed-id->nested-path parsed-id) :value))


(defmethod schema-part-path :container [parsed-id [_ schema-part]]
  (conj (parsed-id->nested-path parsed-id) :container (last parsed-id)))


(defmethod schema-part-path :outer [parsed-id [_ schema-part]]
  (let [outer-id (->> parsed-id
                      reverse
                      rest
                      (drop-while #(not= :key (:type %)))
                      reverse)
        outer-path (parsed-id->nested-path outer-id)]
    (conj outer-path :els-constraints (last parsed-id))))


(defmethod schema-part-path :context [parsed-id [_ schema-part]]
  [:context parsed-id])


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
                 (mapcat #(el-part&path (get-in el [:loc ::id]) %)
                         stripped-el))))
     (completing (fn [acc {:keys [el-part path]}]
                   (assoc-in acc (cons :result path) el-part)))
     {:result {}}
     enriched-elements)))


(defn- els-constraints->zen [els-constraints]
  (merge
    (when-let [requires (seq (filter #(::required (val %))
                                     els-constraints))]
      {:type 'zen.fhir/element
       :zen.fhir/require (into #{}
                               (map #(:key (key %)))
                               requires)})

    (when-let [forbids (seq (filter #(::forbidden (val %))
                                    els-constraints))]
      {:type 'zen.fhir/element
       :zen.fhir/forbid (into #{}
                              (map #(:key (key %)))
                              forbids)})))


(defn- container->zen [{min-card ::min
                       max-card ::max
                       collection ::collection}]
  (merge
    (when (some? min-card)
      {:type 'zen.fhir/element
       :zen.fhir/min min-card})
    (when (some? max-card)
      {:type 'zen.fhir/element
       :zen.fhir/max max-card})
    (when (some? collection)
      {:type 'zen.fhir/element
       :zen.fhir/collection collection})))


(defn- value->zen [value]
  (when (some? value)
    (merge (when-let [max-length (::maxLength value)]
             {:type 'zen/string
              :maxLength max-length}))))


(defn- nested->zen* [nested]
  (merge
    (els-constraints->zen (:els-constraints nested))
    (container->zen (:container nested))
    (value->zen (:value nested))

    (when-let [els (:els nested)]
      (when-let [elements (-> (update-vals els nested->zen*)
                              utils/strip-nils
                              not-empty)]
        {:type 'zen.fhir/element
         :zen.fhir/elements elements}))))


(defn- nested->zen [nested]
  (merge {:zen/tags #{'zen/schema}}
         (nested->zen* nested)))


(defn strdef->zen [strdef]
  (->> (get-in strdef [:differential :element])
       (map group-element-keys)
       (map enrich-loc)
       (map add-schema-parts)
       nest-by-enriched-loc
       nested->zen))
