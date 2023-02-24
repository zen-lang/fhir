(ns zen.fhir-light.loader
  (:require [clojure.string :as str]))


(def elements-keys-types
  {:loc         #{:id :path}
   :validation  #{:min :max :type :condition :constraint}
   :meta        #{:mustSupport}
   :description #{:mapping}})


(defn- group-element-keys [element]
  (update-vals elements-keys-types #(select-keys element %)))


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


(defn- enrich-loc [grouped-element]
  (let [id (get-in grouped-element [:loc :id])]
    (assoc-in grouped-element [:loc :zen.fhir-light/id] (parse-id id))))


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


(defn- nest-by-enriched-path [enriched-elements]
  (:result (reduce (fn [acc el]
                     (assoc-in acc
                               (cons :result (parsed-id->nested-path (get-in el [:loc :zen.fhir-light/id])))
                               el))
                   {:result {}}
                   enriched-elements)))
