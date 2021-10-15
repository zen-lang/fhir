(ns zen.fhir.utils
  (:require [clojure.string :as str]
            [com.rpl.specter :as sp]))


(defn deep-merge
  "efficient deep merge"
  [a b]
  (loop [[[k v :as i] & ks] b, acc a]
    (if (nil? i)
      acc
      (let [av (get a k)]
        (if (= v av)
          (recur ks acc)
          (recur ks (if (and (map? v) (map? av))
                      (assoc acc k (deep-merge av v))
                      (assoc acc k v))))))))


(defn assoc-when-kv
  ([pred m k v]
   (cond-> m (pred [k v]) (assoc k v)))
  ([pred m k v & kvs]
   {:pre [(even? (count kvs))]}
   (reduce (partial apply assoc-when-kv pred)
           (assoc-when-kv pred m k v)
           (partition 2 kvs))))


(defn assoc-when-key
  ([pred m k v]
   (cond-> m (pred k) (assoc k v)))
  ([pred m k v & kvs]
   {:pre [(even? (count kvs))]}
   (reduce (partial apply assoc-when-key pred)
           (assoc-when-key pred m k v)
           (partition 2 kvs))))


(defn assoc-when
  ([pred m k v]
   (cond-> m (pred v) (assoc k v)))
  ([pred m k v & kvs]
   {:pre [(even? (count kvs))]}
   (reduce (partial apply assoc-when pred)
           (assoc-when pred m k v)
           (partition 2 kvs))))


(defn assoc-some [m k v & kvs]
  (apply assoc-when some? m k v kvs))


(defn dissoc-when-kv
  ([pred m k]
   (cond-> m
     (and (contains? m k)
          (pred [k (get m k)]))
     (dissoc k)))
  ([pred m k & ks]
   (reduce (partial dissoc-when-kv pred)
           (dissoc-when-kv pred m k)
           ks)))


(defn dissoc-when-key
  ([pred m k]
   (cond-> m
     (and (contains? m k)
          (pred k))
     (dissoc k)))
  ([pred m k & ks]
   (reduce (partial dissoc-when-key pred)
           (dissoc-when-key pred m k)
           ks)))


(defn dissoc-when
  ([pred m k]
   (cond-> m
     (and (contains? m k)
          (pred (get m k)))
     (dissoc k)))
  ([pred m k & ks]
   (reduce (partial dissoc-when pred)
           (dissoc-when pred m k)
           ks)))


(defn dissoc-nil [m k & ks]
  (apply dissoc-when nil? m k ks))


(defn strip-when-key [pred m]
  (if-let [ks (seq (keys m))]
    (apply dissoc-when-key pred m ks)
    m))


(defn strip-when-kv [pred m]
  (if-let [ks (seq (keys m))]
    (apply dissoc-when-kv pred m ks)
    m))


(defn strip-when [pred m]
  (if-let [ks (seq (keys m))]
    (apply dissoc-when pred m ks)
    m))


(defn strip-nils [m]
  (strip-when nil? m))


(defn parse-int [s]
  (when-let [x (re-matches #"[-+]?\d+" (str s))]
    (Integer/parseInt x)))


(defn poly-get-all
  "Gets all values by fhir polymorphic key[x]"
  [m k]
  (let [key-pat (name k)]
    (sp/select [sp/ALL (comp #(str/starts-with? % key-pat) name first) sp/LAST]
               m)))


(defn poly-get
  "Gets by fhir polymorphic key[x]"
  [m k]
  (first (poly-get-all m k)))


(defn code-search
  "Finds code in vector of hmaps which is equal to one
   of the values provided with descending prioty"
  [code values coll]
  (some (into {} (map (juxt code identity) coll))
        values))

(defn sanitize-obj [obj]
  (cond
    (map? obj) (let [res (reduce (fn [acc [k v]]
                                   (let [v' (sanitize-obj v)]
                                     (if-not (nil? v') (assoc acc k v') acc)))
                                 {} obj)]
                 (if (empty? res) nil res))

    (sequential? obj) (let [res (->> obj (mapv sanitize-obj) (filterv #(not (nil? %))))]
                        (if (empty? res) nil res))

    (string? obj)     (if (str/blank? obj) nil obj)
    :else  obj))

(defn disj-key [m k v]
  (dissoc-when empty? (update m k disj v) k))


(defn safe-merge-with [p f & maps]
  (apply merge-with
         (fn [x y]
           (if (and (p x) (p y))
             (f x y)
             y))
         maps))


(defn safe-merge-with-into [& maps]
  (apply safe-merge-with coll? into maps))


(defn nameable? [obj]
  (or (instance? clojure.lang.Named obj)
      (string? obj)))


(defn index-by [f coll]
  (persistent!
    (reduce
      (fn [ret x]
        (let [k (f x)]
          (assoc! ret k x)))
      (transient {}) coll)))


(def MAP-MAPS
  (sp/recursive-path [] p
   (sp/if-path map?
    (sp/continue-then-stay sp/MAP-VALS p))))
