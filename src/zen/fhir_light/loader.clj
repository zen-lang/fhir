(ns zen.fhir-light.loader
  (:require [clojure.string :as str]
            [zen.v2-validation]
            [zen.fhir.utils :as utils]))


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
    :zf/container (conj (parsed-id->nested-path parsed-id) :zf/container (last parsed-id))
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
  [els]
  (when (some? els)
    (when-let [elements (-> (update-vals els nested->zen*)
                            utils/strip-nils
                            not-empty)]
      {:type 'zen.fhir/element
       :zen.fhir/elements elements})))


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


(defn- description->zen
  "#{:alias :code :comment :definition :example :isModifierReason
     :label :mapping :orderMeaning :requirements :short}"
  [description])


(defn- nested->zen*
  "#{:zf/description :zf/loc :zf/meta
     :zf/context :zf/els :zf/els-cnstraints
     :zf/slicing :zf/container :zf/value
     :zf/poly-roots :zf/poly-keys}"
  [nested]
  (merge
    (els->zen (:zf/els nested))
    (els-constraints->zen (:zf/els-constraints nested))
    (container->zen (:zf/container nested))
    (slicing->zen (:zf/slicing nested))
    (poly-roots->zen (:zf/poly-roots nested))
    (poly-keys->zen (:zf/poly-keys nested))
    (value->zen (:zf/value nested))
    (context->zen (:zf/context nested))
    (meta->zen (:zf/meta nested))
    (description->zen (:zf/description nested))))


(defn- nested->zen [nested]
  (merge
    {:zen/tags #{'zen/schema}}
    (nested->zen* nested)))


(defn strdef->zen [strdef]
  (->> (get-in strdef [:differential :element])
       (map group-element-keys)
       (map enrich-loc)
       nest-by-enriched-loc
       nested->zen))
