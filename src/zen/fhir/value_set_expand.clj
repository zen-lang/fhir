(ns zen.fhir.value-set-expand
  (:require [zen.core :as zen]
            [zen.fhir.value-set-expand]
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


(defn vs-compose-concept-fn [ztx value-set system version concepts])


(defn vs-compose-filter-fn [ztx value-set system version filters])


(defn vs-compose-value-set-fn [ztx value-set value-set-urls])


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
        value-set-pred (vs-compose-value-set-fn ztx value-set (:valueSet compose-el))]
    (some->> [code-system-pred value-set-pred]
             (remove nil?)
             not-empty
             (apply every-pred))))


(defn compose [ztx vs]
  (let [include-fn (some->> (get-in vs [:compose :include])
                            (keep (partial check-if-concept-is-in-this-compose-el-fn ztx vs))
                            not-empty
                            (apply some-fn))
        exclude-fn (some->> (get-in vs [:compose :exclude])
                            (keep (partial check-if-concept-is-in-this-compose-el-fn ztx vs))
                            not-empty
                            (apply some-fn)
                            complement)]
    (or (some->> [include-fn exclude-fn]
                 (remove nil?)
                 not-empty
                 (apply every-pred))
        #_(assert (some? include-fn) (str "ValueSet.compose.include is required. Value set url is " (:url vs)))
        (constantly false))))


(defn denormalize-into-concepts [ztx valuesets concepts-map]
  (reduce
    (fn [concepts-acc vs]
      (let [concept-in-vs? (compose ztx vs)]
        (reduce
          (fn [acc [concept-id concept]]
            (if (concept-in-vs? concept)
              (update-in acc [concept-id :valueset]
                         (fnil conj #{})
                         (:url vs))
              acc))
          concepts-acc
          concepts-acc)))
    concepts-map
    valuesets))


(defn denormalize-value-sets [ztx]
  (swap! ztx update-in [:fhir/inter "Concept"]
         (partial denormalize-into-concepts
                  ztx (vals (get-in @ztx [:fhir/inter "ValueSet"])))))