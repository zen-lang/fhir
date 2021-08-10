(ns zen.fhir.generator
  (:require [cheshire.core]
            [clojure.pprint]
            [clojure.java.io :as io]
            [clojure.walk]
            [clojure.string :as str]))

(def types (cheshire.core/parse-string (slurp "fhir/profiles-types.json") keyword))

(def subj (:resource (first (:entry types))))

(defn pp [x]
  (clojure.pprint/pprint x))

(def primitives
  {"dateTime" 'zen/datetime
   "date" 'zen/date
   "id" 'zen/string
   "code" 'zen/string
   "canonical" 'zen/string
   "string" 'zen/string
   "boolean" 'zen/boolean


   })

(defn sd-to-zen [res]
  (let [tp {:zen/tags #{'fhir/primitive 'zen/schema}
            ;:zen/desc (:description res)
            }]
    ;; (println (:name res) (:kind res))
    (if (= "primitive-type" (:kind res))
      (assoc tp
             :type (get primitives (:name res) 'zen/string)
             :zen/tags #{'fhir/primitive-type 'zen/schema})
      (let [base (when-let [b (:baseDefinition res)] (last (str/split b  #"/")))]
        (cond-> 
            (assoc tp
                   :type 'zen/map
                   :zen/tags #{'fhir/complex-type 'zen/schema}
                   :keys (->> (:element (:differential res))
                              (mapv (fn [el]
                                      (->
                                       (select-keys el [:path :short :min :max :type :binding])
                                       (update :path (fn [x] (mapv (fn [e] (keyword (str/replace e #"\[x\]$" "")))
                                                                  (rest (str/split x #"\."))))))))
                              (remove (fn [x] (empty? (:path x))))
                              (reduce (fn [acc x]
                                        (assoc-in acc (interpose :keys (:path x))
                                                  (let [zen-type (get primitives
                                                                      (and (= 1 (count (:type x))) (:code (first (:type x)))))]
                                                    (cond->
                                                        {:zen/desc (:short x)
                                                         :confirms (into #{} (mapv (fn [{c :code}] (symbol c)) (:type x)))}
                                                      (not (= "1" (:max x))) (assoc :coll true)
                                                      (and (:binding x) (= "required" (get-in x [:binding :strength])))
                                                      (assoc :valueset {:name (symbol
                                                                               "fhir.valuesets"
                                                                               (first (str/split (last (str/split (get-in x [:binding :valueSet]) #"/"))
                                                                                                 #"\|")))})
                                                      zen-type (assoc :type zen-type))))) {})
                              (clojure.walk/postwalk (fn [x]
                                                       (if (:coll x)
                                                         {:type 'zen/vector
                                                          :zen/desc (:zen/desc x)
                                                          :every (dissoc x :coll :zen/desc)}
                                                         x)))))
          base (assoc :confirms #{(symbol base)}))))))

(let [edn (with-out-str (->> (:entry types)
                             (reduce (fn [acc {res :resource}]
                                       (if (= "StructureDefinition" (:resourceType res))
                                         (assoc acc (symbol (:name res)) (sd-to-zen res))
                                         acc))
                                     {'ns 'fhir.datatypes}) pp))]
  (spit "zrc/datatypes.edn" edn))

(def resources (cheshire.core/parse-string (slurp "fhir/profiles-resources.json") keyword))

(defn res-to-zen [res]
  (let [tp {'ns 'fhir.smth
            :zen/tags #{'zen/schema}
            :type 'zen/map}]
    tp
    ))

(let [out (->> (:entry resources)
               (reduce (fn [acc {res :resource}]
                         (if (and (= "StructureDefinition" (:resourceType res))
                                  (= "resource" (:kind res)))
                           (assoc acc (symbol (:name res)) (res-to-zen res))
                           acc))
                       {})
               )]
  (doseq [[k v] out]
    (spit (str "zrc/" k ".edn") v)))
