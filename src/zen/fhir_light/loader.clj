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
  {:value     #{:type :binding :contentReference :maxLength #_[:base :path]}
   :container #{:max :min :sliceIsConstraining :sliceName :slicing #_[:base :min] #_[:base :max]}
   :outer     #{:max :min :condition #_[:base :min] #_[:base :max]}
   :context   #{:constraint}})


(defmulti el-keys->zen
  (fn [keys-type [el-key _el-val]]
    [keys-type el-key]))


(defmethod el-keys->zen :default [_ _] nil)


(defn validation->zen-schema-parts [validation]
  (not-empty
    (into {}
          (keep (fn [[keys-type el-keys]]
                  (when-let [schema-part
                             (not-empty (into {}
                                              (map #(el-keys->zen keys-type %))
                                              (select-keys validation el-keys)))]
                    [keys-type schema-part])))
          validation-keys-types)))


(defn- add-schema-parts [grouped-el]
  (utils/assoc-some
    grouped-el
    :schema-parts
    (validation->zen-schema-parts (:validation grouped-el))))


(defmethod el-keys->zen [:outer :min] [_ [_ min-card]]
  (when (< 0 min-card)
    {::required true}))


(defmethod el-keys->zen [:container :min] [_ [_ min-card]]
  (when (< 0 min-card)
    {::min min-card}))


(defmethod el-keys->zen [:outer :max] [_ [_ max-card]]
  (when (= 0 max-card)
    {::forbidden true}))


(defmethod el-keys->zen [:container :max] [_ [_ max-card]]
  (if (= "*" max-card)
    {::collection true}
    {::max (parse-long max-card)}))


(defmethod el-keys->zen [:value :maxLength] [_ [_ max-length]]
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


(defn strip-el [el & {:keys [keys-to-select keys-to-strip]}]
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


(defn nested->zen* [nested]
  (merge #_"TODO: :slicing :poly-roots :poly-keys :context"
    (when-let [requires (->> (:els-constraints nested)
                             (filter (fn [[_ {::keys [required]}]] required))
                             seq)]
      {:type 'zen.fhir/element
       :zen.fhir/require (into #{} (map (comp :key key)) requires)})

    (when-let [forbids (->> (:els-constraints nested)
                            (filter (fn [[_ {::keys [forbidden]}]] forbidden))
                            seq)]
      {:type 'zen.fhir/element
       :zen.fhir/forbid (into #{} (map (comp :key key)) forbids)})

    (when-let [container (:container nested)]
      (merge
        (when-let [min-card (::min container)]
          {:type 'zen.fhir/element
           :zen.fhir/min min-card})
        (when-let [max-card (::max container)]
          {:type 'zen.fhir/element
           :zen.fhir/max max-card})
        (when-let [collection-card (::collection container)]
          {:type 'zen.fhir/element
           :zen.fhir/collection collection-card})))

    (when-let [els (:els nested)]
      (when-let [elements (-> (update-vals els nested->zen*)
                              utils/strip-nils
                              not-empty)]
        {:type 'zen.fhir/element
         :zen.fhir/elements elements}))

    (when-let [value (:value nested)]
      (merge (when-let [max-length (::maxLength value)]
               {:type 'zen/string
                :maxLength max-length})))))


(defn nested->zen [nested]
  (merge {:zen/tags #{'zen/schema}}
         (nested->zen* nested)))


(defmethod zen.v2-validation/compile-type-check 'zen.fhir/element [_ _]
  (with-bindings {#'zen.v2-validation/types-cfg
                  {'zen.fhir/element
                   {:fn (some-fn map? vector?)
                    :to-str "object or 'object[]"}}}
    (zen.v2-validation/type-fn 'zen.fhir/element)))


(defn transpile-key-for-map-or-vector [_ ztx sch-for-map]
  (let [for-map (zen.v2-validation/get-cached ztx sch-for-map false)
        for-vector (zen.v2-validation/get-cached
                     ztx {:type 'zen/vector :every sch-for-map} false)]
    {:when (some-fn map? vector?)
     :rule (fn [vtx data opts]
             (if (vector? data)
               (for-vector vtx data opts)
               (for-map vtx data opts)))}))


(defmethod zen.v2-validation/compile-key :zen.fhir/require
  [k ztx require]
  (transpile-key-for-map-or-vector
    k ztx {:type 'zen/map :require require}))


(defmethod zen.v2-validation/compile-key :zen.fhir/elements
  [k ztx elements]
  (transpile-key-for-map-or-vector
    k ztx {:type 'zen/map :keys elements}))
