(ns zen.fhir-light.loader.parse-id
  (:require [clojure.string :as str]))


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


(defn try-parse-primitive-value
  "primitive-types have <type>.value element which
   describes value itself rather a :value key."
  [id-parts {:keys [primitive?]}]
  (when (and primitive? (= 1 (count id-parts)) (= "value" (first id-parts)))
    [{:type :primitive-value}]))


(defn parse-id [id & {:as params}]
  (if (str/blank? id)
    []
    (let [[root-part & rest-parts] (str/split (str id) #"\.")]
      (vec (cons {:type :root, :root root-part}
                 (or (try-parse-primitive-value rest-parts params)
                     (mapcat parse-id-part rest-parts)))))))


(defn enrich-loc [grouped-strdef grouped-element]
  (let [primitive? (= "primitive-type" (get-in grouped-strdef [:zf/meta :kind]))
        id         (get-in grouped-element [:zf/loc :id])
        parsed-id  (parse-id id :primitive? primitive?)]
    (assoc-in grouped-element [:zf/loc :zf/id] parsed-id)))
