(ns zen.fhir.value-set-expand
  (:require [zen.core :as zen]
            [cheshire.core]
            [clojure.java.io :as io]
            [fipp.edn]
            [clojure.string :as str]
            [zen.fhir.utils :as utils]
            [com.rpl.specter :as sp]))


(defn vs-compose-system-fn [ztx value-set system version]
  (when (some? system)
    (fn [concept]
      (= system (:system concept)))))


(defn vs-compose-concept-fn [ztx value-set system version concepts]
  (when (seq concepts)
    (let [concept-codes (into #{} (map :code) concepts)]
      (fn [concept]
        (and (= system (:system concept))
             (contains? concept-codes (:code concept)))))))


(defmulti filter-op
  (fn [_ztx _value-set _system _version filter]
    (:op filter)))


(defmethod filter-op :default [_ztx _value-set _system _version _filter]
  (constantly false))


(defmethod filter-op "=" [_ztx _value-set _system _version filter]
  (fn [concept]
    (= (get (:property concept) (:property filter))
       (:value filter))))


(defmethod filter-op "in" [_ztx _value-set _system _version filter]
  (fn [concept]
    (get (into #{} (map str/trim) (str/split (or (:value filter) "") #","))
         (get (:property concept) (:property filter)))))


(defmethod filter-op "not-in" [_ztx _value-set _system _version filter]
  (fn [concept]
    (not (get (into #{} (map str/trim) (str/split (or (:value filter) "") #","))
              (get (:property concept) (:property filter))))))


(defmethod filter-op "exists" [_ztx _value-set _system _version filter]
  (if (= "false" (some-> (:value filter) str/lower-case str/trim))
    (fn [concept] (nil? (get (:property concept) (:property filter))))
    (fn [concept] (some? (get (:property concept) (:property filter))))))


(defmethod filter-op "is-a" [_ztx _value-set _system _version filter]
  (fn [concept]
    (or (= (:code concept) (:value filter))
        (contains? (set (:hierarchy concept)) (:value filter)))))


(defmethod filter-op "descendent-of" [_ztx _value-set _system _version filter]
  (fn [concept]
    (contains? (set (:hierarchy concept)) (:value filter))))


(defmethod filter-op "is-not-a" [_ztx _value-set _system _version filter]
  (fn [concept] ;; TODO: not sure this is correct impl by spec
    (and (not (contains? (set (:hierarchy concept)) (:value filter)))
         (not= (:code concept) (:value filter)))))


(defmethod filter-op "regex" [_ztx _value-set _system _version filter]
  (fn [concept]
    (when-let [prop (get (:property concept) (:property filter))]
      (re-matches (re-pattern (:value filter))
                  (str prop)))))


(defn vs-compose-filter-fn [ztx value-set system version filters]
  (when (seq filters)
    (apply every-pred
           (comp #{system} :system)
           (map (partial filter-op ztx value-set system version)
                filters))))


(declare compose)


(defn vs-compose-value-set-fn [ztx value-set value-set-urls]
  (when-let [composes
             (some->> value-set-urls
                      (keep #(when-let [vs (get-in @ztx [:fhir/inter "ValueSet" %])]
                               (compose ztx vs)))
                      not-empty)]
    (when-let [check-fn
               (some->> composes
                        (keep :compose-fn)
                        not-empty
                        (apply every-pred))]
      {:systems (mapcat :systems composes)
       :check-fn check-fn})))


(defn check-if-concept-is-in-this-compose-el-fn [ztx value-set compose-el]
  (let [code-system-pred (or (vs-compose-concept-fn ztx value-set
                                                    (:system compose-el)
                                                    (:version compose-el)
                                                    (:concept compose-el))
                             (vs-compose-filter-fn ztx value-set
                                                   (:system compose-el)
                                                   (:version compose-el)
                                                   (:filter compose-el))
                             (vs-compose-system-fn ztx value-set
                                                   (:system compose-el)
                                                   (:version compose-el)))

        {value-set-systems :systems, value-set-pred :check-fn}
        (vs-compose-value-set-fn ztx value-set (:valueSet compose-el))

        check-fn (some->> [code-system-pred value-set-pred]
                          (remove nil?)
                          not-empty
                          (apply every-pred))]
    (when check-fn
      {:systems   (conj value-set-systems (:system compose-el))
       :check-fn  check-fn})))


(defn compose [ztx vs]
  (let [includes   (some->> (get-in vs [:compose :include])
                            (keep (partial check-if-concept-is-in-this-compose-el-fn ztx vs))
                            not-empty)
        include-fn (some->> includes
                            (map :check-fn)
                            (apply some-fn))

        excludes   (some->> (get-in vs [:compose :exclude])
                            (keep (partial check-if-concept-is-in-this-compose-el-fn ztx vs))
                            not-empty)
        exclude-fn (some->> excludes
                            (map :check-fn)
                            (apply some-fn)
                            complement)

        includes-and-excludes (concat includes excludes)

        systems    (into #{}
                         (mapcat (comp not-empty :systems))
                         includes-and-excludes)]
    {:systems    (not-empty systems)
     :compose-fn
     (or (some->> [include-fn exclude-fn]
                  (remove nil?)
                  not-empty
                  (apply every-pred))
         #_(assert (some? include-fn) (str "ValueSet.compose.include is required. Value set url is " (:url vs)))
         (constantly false))}))



(defn denormalize-into-concepts [ztx valuesets concepts-map]
  (reduce
    (fn [concepts-acc vs]
      (let [{systems            :systems
             concept-in-vs?     :compose-fn}
            (compose ztx vs)]
        (reduce
          (fn [acc [system concepts]]
            (reduce
              (fn [acc [concept-id concept]]
                (if (concept-in-vs? concept)
                  (update-in acc [system concept-id :valueset]
                             (fnil conj #{})
                             (:url vs))
                  acc))
              acc
              concepts))
          concepts-acc
          (select-keys concepts-acc systems))))
    concepts-map
    valuesets))


(defn denormalize-value-sets-into-concepts [ztx]
  (swap! ztx update-in [:fhir/inter "Concept"]
         (partial denormalize-into-concepts
                  ztx (vals (get-in @ztx [:fhir/inter "ValueSet"])))))
