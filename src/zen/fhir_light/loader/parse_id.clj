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


(defn parse-id [id]
  (if (str/blank? id)
    []
    (let [id-parts (str/split (str id) #"\.")]
      (vec (cons {:type :root, :root (first id-parts)}
                 (mapcat parse-id-part (rest id-parts)))))))
