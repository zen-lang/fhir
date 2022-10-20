(ns zen.fhir.structure-definition.loader
  (:require [clojure.string :as str]
            [com.rpl.specter :as sp]))

;; FIXME: unused.
(defn base-url [subj]
  (println(:type subj) (pr-str :no-type-in subj))
  (or (:baseDefinition subj)
      (str "http://hl7.org/fhir/StructureDefinition/" (:type subj))))

(defn ^String decapitalize-first-letter
  "Converts first character of the string to lower-case, all other characters leaves as is"
  [^CharSequence s]
  (let [s (.toString s)]
    (if (< (count s) 2)
      (.toLowerCase s)
      (str (.toLowerCase (subs s 0 1))
           (subs s 1)))))

(defn get-definition
  "Get FHIR StructureDefinition resource by URL"
  [ztx url]
  (get-in @ztx [:fhir/inter "StructureDefinition" url]))

(defn is-fhirpath-type?
  "Check if type code is from fhirpath.
  All types we encountered yet start with 'http://hl7.org/fhirpath/System.'"
  [type-code]
  (str/starts-with? type-code "http://hl7.org/fhirpath/System."))


(defn fhirpath-type-code->typename
  "Convert fhirpath type code to typename.
  All types we encountered yet start with 'http://hl7.org/fhirpath/System.'
  And they correspond to primitive types in FHIR which start with lower case."
  [type-code]
  (let [typename-first-letter-idx 31
        typename-rest-letters-idx (inc typename-first-letter-idx)
        typename-first-letter (subs type-code typename-first-letter-idx typename-rest-letters-idx)
        typename-rest-letters (subs type-code typename-rest-letters-idx)]
    (str (str/lower-case typename-first-letter) typename-rest-letters)))

(defn type-code->typename
  "Get type name from ElementDefinition.type.code value
  The type code is uri.
  Most types are written as-is (e.g. Address).
  Some types are from fhirpath (e.g. string)"
  [type-code]
  (if (is-fhirpath-type? type-code)
    (fhirpath-type-code->typename type-code)
    type-code))

(defn typename->structure-definition-url
  "Only base FHIR is allowed to define data types.
  Therefore all structure definition for types have consistent URLs"
  [typename]
  (str "http://hl7.org/fhir/StructureDefinition/" typename))


(defn get-type-definition [ztx subj el type-name]
  (let [typename (type-code->typename type-name)
        structure-definition-url (typename->structure-definition-url typename)
        definition (get-definition ztx structure-definition-url)]
    (when-not definition
      (throw (Exception.
              (str "Could not find type definition: " (pr-str typename) " url " (pr-str structure-definition-url)
                    " in "    (pr-str (:url  subj))
                    " element " (pr-str el)
                    " file: " (pr-str (get-in subj [:zen/loader :file]))))))
    definition))


(defn is-profile?
  ;; NOTE: seems unused
  [url subj]
  (and (= "constraint" (:derivation subj))
       (not (or (= "Extension" (:type subj))
                (:fhir/extension subj)))))

(defn get-bases
  "Get all StructureDefinition of every ancestor with respect to subtyping relation.
  Note that any FHIR type can have at most one base-type."
  [ztx structure-definition]
  (loop [base       (:baseDefinition structure-definition)
         base-stack []
         bases      #{}]
    (if (or (nil? base)
            ;; NOTE: Why? Circular subtyping is not allowed by FHIR.
            (contains? bases base))
      base-stack
      (let [base-def (get-definition ztx base)]
        (recur (:baseDefinition base-def)
               (conj base-stack base-def)
               (conj bases base))))))

(defn get-base-elements
  [ztx subj k el bases]
  (let [elements-stack bases
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

(defn has-nesting?
  "Check if some (sub)field in preprocessed resource has subfields"
  [element]
  (seq (:| element)))

(defn make-first-class-extensions [acc [field-name element]]
  (if (= :extension field-name)
    (make-first-class-ext-keys acc element)
    (assoc acc field-name element)))

(defn enrich-element [ctx el base-els]
  ;; TODO: if vector do min/max items
  ;;       required/prohibited
  ;;       targetProfile type profile
  (let [is-element-vector? (some :vector (cons el base-els))
        handle-first-class-ext? (not (:do-not-handle-first-class-ext? ctx))
        tp (or (:type el)
               (->> base-els
                    (filter (fn [{tp :type}] (and (not (nil? tp))
                                                  (not (= "Element" tp)))))
                    (some :type)))]
    (cond-> el
      ;; NOTE: Is this condition ever true?
      is-element-vector? (assoc :vector true)
      (not is-element-vector?) (dissoc :minItems :maxItems)
      tp                            (assoc :type tp)
      (contains? el :fhir/slicing)  (as-> $ (enrich-slicing ctx $ base-els))
      (and (has-nesting? el) handle-first-class-ext?) (update :| (partial reduce make-first-class-extensions {}))
      (fhir-primitive? el base-els) (assoc :fhir/primitive-attr true))))

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

(defn add-primitive-element-attrs [acc [k el]]
            (if (:fhir/primitive-attr el)
              (let [{:keys [primitive _primitive]} (extract-_primitive k el)]
                (assoc acc
                       (:key primitive) (:el primitive)
                       (:key _primitive) (:el _primitive)))
              (assoc acc k el)))

(defn walk-with-bases [ztx ctx subj bases]
  (letfn [(walk-with-bases-recursive [acc [k el]]
            (let [{:keys [el-key element base-elements]
                   :or   {el-key k, element el, base-elements []}}
                  (find-base-els ztx subj k el bases)

                  new-ctx
                  (-> (update ctx :lvl inc) (update :path conj el-key))]
              (when (and (not= "specialization" (:derivation ctx))
                         (empty? base-elements))
                (println :no-base-for-element (conj (:path ctx) k) el))
              (assoc acc el-key (walk-with-bases ztx new-ctx element base-elements))))]
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
  "Check if StructureDefinition is for FHIR extension"
  [structure-definition]
  (= "Extension" (:type structure-definition)))

(defn process-extension
  ;; FIXME NOP
  [ztx url subj]
  subj)

(defn has-no-base-but-should?
  "Check if StructureDefinition doesn't have a known base when it should.
  It can be either error in StructureDefinition or in loading base."
  [structure-definition bases]
  (and (= "constraint" (:derivation structure-definition))
       (empty? bases)))

(defn should-ignore-first-class-exts?
  "Check if first class extensions should be ignored for these elements.

  Element and DomainResource are base resources in FHIR 4.0.1.
  They have extension field which should be allowed by zen."
  [structure-definition]
  (or (= "http://hl7.org/fhir/StructureDefinition/Element" (:url structure-definition))
      (= "http://hl7.org/fhir/StructureDefinition/DomainResource" (:url structure-definition))))

(defn process-sd [ztx url subj]
  (if (is-extension? subj)
    (process-extension ztx url subj)
    (let [bases (get-bases ztx subj)]
      (when (has-no-base-but-should? subj bases)
        (println :no-base-resource (pr-str url)))
      (walk-with-bases ztx {:lvl 0
                            :path [url]
                            :derivation (:derivation subj)
                            :do-not-handle-first-class-ext? (should-ignore-first-class-exts? subj)}
                       subj
                       bases))))

(defn process-structure-definitions [ztx]
  (swap! ztx update-in [:fhir/inter "StructureDefinition"]
         (partial reduce (fn [acc [url resource]]
                           (assoc acc url (process-sd ztx url resource)))
                  {})))
