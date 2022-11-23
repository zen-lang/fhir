(ns zen.fhir.structure-definition.loader
  (:require [clojure.string :as str]
            [com.rpl.specter :as sp]))

(defn ^String decapitalize-first-letter
  "Converts first character of the string to lower-case, all other characters leaves as is"
  [^CharSequence s]
  (let [s (.toString s)]
    (if (< (count s) 2)
      (.toLowerCase s)
      (str (.toLowerCase (subs s 0 1))
           (subs s 1)))))

(defn base-url [subj]
  (println(:type subj) (pr-str :no-type-in subj))
  (or (:baseDefinition subj)
      (str "http://hl7.org/fhir/StructureDefinition/" (:type subj))))


(defn get-definition [ztx url]
  (get-in @ztx [:fhir/inter "StructureDefinition" url]))


(defn get-type-definition [ztx subj el type-name]
  (let [tp (if (str/starts-with? type-name "http://hl7.org/fhirpath/System.")
             (str (str/lower-case (subs type-name 31 32)) (subs type-name 32))
             type-name)
        definition (get-definition ztx (str "http://hl7.org/fhir/StructureDefinition/" tp))]
    (when-not definition
      (throw (Exception.
               (str "Could not find type definition: " (pr-str tp) " url " (pr-str (str "http://hl7.org/fhir/StructureDefinition/" tp))
                    " in "    (pr-str (:url  subj))
                    " element " (pr-str el)
                    " file: " (pr-str (get-in subj [:zen/loader :file]))))))
    definition))


(defn is-profile? [url subj]
  (and (= "constraint" (:derivation subj))
       (not (or (= "Extension" (:type subj))
                (:fhir/extension subj)))
       #_(not (= url (base-url subj)))))

(defn get-bases [ztx subj]
  (loop [base       (:baseDefinition subj)
         base-stack []
         bases      #{}]
    (if (or (nil? base)
            (contains? bases base))
      base-stack
      (let [base-def (get-definition ztx base)]
        (recur (:baseDefinition base-def)
               (conj base-stack base-def)
               (conj bases base))))))

(defn get-base-elements [ztx subj k el bases]
  (let [elements-stack bases ;;(cons el bases) ;; ????
        base-elements  (keep #(get-in % [:| k]) (reverse elements-stack))]
    (not-empty (vec base-elements))))


(defn get-type-base-elements [ztx subj k el bases]
  (let [elements-stack bases ;;(cons el bases) ;; ????
        base-elements  (keep #(get-in % [:| k]) (reverse elements-stack))
        types          (cond-> (set (keep #(get-in % [:type]) base-elements))
                         (:type el) (conj (:type el)))
        types-defs     (map (fn [x] (get-type-definition ztx subj el x)) types)]
    (not-empty (vec types-defs))))


(defn get-base-poly-key [ztx k bases]
  (some #(get-in % [:fhir-poly-keys k]) bases))


(defn make-first-class-ext-keys [acc el]
  (->> (get-in el [:slicing :slices])
       (reduce (fn [acc [ext-k ext-el]]
                 (when-not (= ext-k (keyword (:sliceName ext-el)))
                   (prn "WARN:" (pr-str ext-k "!=" (:sliceName ext-el))))
                 (assoc acc ext-k (dissoc ext-el :type :sliceName)))
               acc)))

(defn fhir-primitive? [_el base-els]
  (some #(= "primitive-type" (:kind %))
        base-els))

(defn fix-match-vectors* [slice path acc match-el]
  (if (map? match-el)
    (reduce (fn [out [k v]]
              (let [match-path (conj path k)
                    el-path (vec (interleave (repeat :|) match-path))
                    out' (fix-match-vectors* slice match-path out v)]
                (if (:vector (get-in slice el-path))
                  (update-in out' (conj path k) hash-set)
                  out')))
            acc
            match-el)
    acc))


(defn fix-match-vectors [slice]
  (update slice :match
          #(fix-match-vectors* slice [] % %)))


(defn enrich-slicing [ctx el base-els]
  (update-in el [:fhir/slicing :slices] #(sp/transform [sp/MAP-VALS] fix-match-vectors %)))

(defn enrich-element [ctx el base-els]
  ;; TODO: if vector do min/max items
  ;;       required/prohibited
  ;;       tragetProfile type profile
  (letfn [(make-first-class-extensions [acc [k el]]
            (if (and (= :extension k) (not (:do-not-handle-first-class-ext? ctx)))
              (make-first-class-ext-keys acc el)
              (assoc acc k el)))]
    (let [v? (some :vector (cons el base-els))
          tp (or (:type el)
                 (->> base-els
                      (filter (fn [{tp :type}] (and (not (nil? tp))
                                                    (not (= "Element" tp)))))
                      (some :type)))]
      (cond-> el
        v?                            (assoc :vector true)
        (not v?)                      (dissoc :minItems :maxItems)
        tp                            (assoc :type tp)
        (contains? el :fhir/slicing)  (as-> $ (enrich-slicing ctx $ base-els))
        (seq (:| el))                 (update :| (partial reduce make-first-class-extensions {}))
        (fhir-primitive? el base-els) (assoc :fhir/primitive-attr true)))))

(defn search-base-elements [ztx subj k el bases]
  (if-let [b-els (get-base-elements ztx subj k el bases)]
    {:el-key k, :element el, :base-elements b-els}
    (let [fix-poly-k (keyword (decapitalize-first-letter (name k)))]
      (when-let [b-els (get-base-elements ztx subj fix-poly-k el bases)]
        {:el-key fix-poly-k, :element el, :base-elements b-els}))))


(defn find-poly-base-el [ztx subj k el bases]
  (when-let [{poly-key :key, poly-type :type} (get-base-poly-key ztx k bases)]
    (let [poly-el  {:| {(keyword poly-type) (assoc el :type poly-type)}}
          base-els (get-base-elements ztx subj poly-key poly-el bases)]
      {:el-key        poly-key
       :element       poly-el
       :base-elements base-els})))


(defn find-base-els [ztx subj k el bases]
  (let [{:as   search-result
         :keys [el-key element base-elements]
         :or   {el-key k, element el, base-elements []}}
        (search-base-elements ztx subj k el bases)

        result (if (seq base-elements)
                 search-result
                 (find-poly-base-el ztx subj el-key element bases))

        type-base-elements (get-type-base-elements ztx subj k el bases)]
    (update result :base-elements (fnil into []) type-base-elements)))

(defn primitive-element-key [primitive-k]
  (keyword (str "_" (name primitive-k))))


(defn primitive-element [k primitive]
  (assoc (select-keys primitive [:vector :|])
         :type "Element"
         :original-key k))

(defn extract-_primitive [k el]
  {:primitive {:key k
               :el (dissoc el :|)}
   :_primitive {:key (primitive-element-key k)
                :el (primitive-element k el)}})

;; Profile.element
;; Base.element
(defn walk-with-bases [ztx ctx subj bases]
  (letfn [(walk-with-bases-recursive [acc [k el]]
            (let [required?
                  (get-in subj [:| k :required])

                  {:keys [el-key element base-elements]
                   :or   {el-key k, element el, base-elements []}}
                  (find-base-els ztx subj k el bases)

                  new-ctx
                  (-> (update ctx :lvl inc) (update :path conj el-key))]
              (when (and (not= "specialization" (:derivation ctx))
                         (empty? base-elements))
                (println :no-base-for-element (conj (:path ctx) k) el))

              (let [walked-el-schema (walk-with-bases ztx new-ctx element base-elements)
                    #_"NOTE: assoc of :required is needed because otherwise it is lost for polymorphic keys"
                    el-schema (cond-> walked-el-schema
                                required? (assoc :required true))]
                (assoc acc el-key el-schema))))

          (add-primitive-element-attrs [acc [k el]]
            (if (:fhir/primitive-attr el)
              (let [{:keys [primitive _primitive]} (extract-_primitive k el)]
                (assoc acc
                       (:key primitive) (:el primitive)
                       (:key _primitive) (:el _primitive)))
              (assoc acc k el)))]
    (let [enr-subj (enrich-element ctx subj bases)]
      (cond-> enr-subj
        (seq (:| enr-subj))
        (-> (update :| (partial reduce walk-with-bases-recursive {}))
            (update :| (partial reduce add-primitive-element-attrs {})))

        (seq (get-in enr-subj [:fhir/slicing :slices]))
        (update-in [:fhir/slicing :slices]
                   (fn [slices]
                     (reduce (fn [acc [k v]]
                               (if (seq (:| v))
                                 (assoc acc k (-> v
                                                  (update :| (partial reduce walk-with-bases-recursive {}))
                                                  (update :| (partial reduce add-primitive-element-attrs {}))))
                                 acc))
                             slices
                             slices)))))))

(defn is-extension?
  [_url subj]
  (= "Extension" (:type subj)))

(defn process-extension
  [ztx url subj]
  subj)

(defn process-sd [ztx url subj]
  (if (is-extension? url subj)
    (process-extension ztx url subj)
    (let [bases (get-bases ztx subj)]
      (when (and (= "constraint" (:derivation subj)) (empty? bases))
        (println :no-base-resource (pr-str url)))
      (walk-with-bases ztx {:lvl 0
                            :path [url]
                            :derivation (:derivation subj)
                            :do-not-handle-first-class-ext?
                            (or (= "http://hl7.org/fhir/StructureDefinition/Element" (:url subj))
                                (= "http://hl7.org/fhir/StructureDefinition/DomainResource" (:url subj)))}
                       subj
                       bases))))

(defn process-structure-definitions [ztx]
  (swap! ztx update-in [:fhir/inter "StructureDefinition"]
         (partial reduce (fn [acc [url resource]]
                           (assoc acc url (process-sd ztx url resource)))
                  {})))
