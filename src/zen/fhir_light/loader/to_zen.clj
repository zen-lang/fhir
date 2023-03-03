(ns zen.fhir-light.loader.to-zen
  (:require [clojure.string :as str]
            [zen.fhir-light.utils.merge :as utils.merge]
            [zen.fhir.utils :as utils]))


(def fhir-sequence->version-mapping
  {"r5" #{"5.0.0-draft-final"
          "5.0.0-snapshot3"
          "5.0.0-ballot"
          "5.0.0-snapshot1"
          "4.6.0"
          "4.5.0"
          "4.4.0"
          "4.2.0"}

   "r4b" #{"4.3.0"
           "4.3.0-snapshot1"
           "4.1.0"}

   "r4" #{"4.0.1"
          "4.0.0"
          "3.5a.0"
          "3.5.0"
          "3.3.0"
          "3.2.0"}

   "stu3" #{"3.0.2"
            "3.0.1"
            "3.0.0"
            "1.8.0"
            "1.6.0"
            "1.4.0"
            "1.2.0"
            "1.1.0"}})


(def fhir-version->sequence-mapping
  (into {}
        (for [[fhir-sequence versions] fhir-sequence->version-mapping
              version                  versions]
          [version fhir-sequence])))


(declare nested->zen*)


(defn- els->zen
  "{<key> <nested>}"
  [ctx els]
  (when (some? els)
    (let [keys-acc (update-vals els #(nested->zen* ctx %))

          key-schemas (-> keys-acc
                          (update-vals :zf/schema)
                          utils/strip-nils
                          not-empty)

          rest-acc (->> keys-acc
                        vals
                        (keep #(dissoc % :zf/schema))
                        (apply utils.merge/safe-deep-merge))]
      (cond-> rest-acc
        key-schemas
        (assoc :zf/schema
               {:type 'zen.fhir/element
                :zen.fhir/el
                {:type 'zen/map
                 :keys key-schemas}})))))


(defn- els-constraints->zen
  "{<source :zf/id> #{:max :min :condition :base}}"
  [ctx els-constraints]
  {:zf/schema
   (merge
     (when-let [requires (seq (filter #(some->> (:min (val %))
                                                (< 0))
                                      els-constraints))]
       {:type 'zen.fhir/element
        :zen.fhir/el {:type 'zen/map
                      :require (into #{}
                                     (map #(:key (key %)))
                                     requires)}})

     #_(when-let [forbids (seq (filter #(= "0" (:max (val %)))
                                       els-constraints))]
         {:type 'zen.fhir/element
          :zen.fhir/el {:type 'zen/map
                        :forbid (into #{}
                                      (map #(:key (key %)))
                                      forbids)}}))})


(defn- container->zen
  "#{:max :min :sliceIsConstraining :sliceName :slicing :base}"
  [ctx {min-card :min max-card :max}]
  {:zf/schema
   (merge
     (when (and min-card (< 0 min-card))
       {:type 'zen.fhir/element
        :zen.fhir/min min-card})
     (when max-card
       (if (= "*" max-card)
         {:type 'zen.fhir/element
          :zen.fhir/collection true}
         {:type 'zen.fhir/element
          :zen.fhir/max (parse-long max-card)})))})


(defn- slicing->zen
  "{:slices {\"<slice name>\" <nested>}}"
  [ctx slicing])


(defn- poly-roots->zen
  "{\"<poly root name>\" <nested>}"
  [ctx poly-roots])


(defn- poly-keys->zen
  "{\"<poly key name>\" <nested>}"
  [ctx poly-keys])


(def ^:private fhir-type-url-prefix "http://hl7.org/fhir/StructureDefinition/")
(def ^:private fhirpath-type-url-prefix "http://hl7.org/fhirpath/")


(defn- complex-type-code? [^String type-code]
  (Character/isUpperCase ^Character (first type-code)))


(defn- parse-fhir-type [type-code]
  {:type/code type-code
   :type/url  (str fhir-type-url-prefix type-code)
   :type/kind (if (complex-type-code? type-code)
                "complex"
                "primitive")
   :type/name type-code})


(defn- get-fhirpath-type [fhirpath-type-url]
  (subs fhirpath-type-url
        (count (str fhirpath-type-url-prefix "System."))))


(defn- parse-system-type [fhirpath-type-url]
  {:type/code fhirpath-type-url
   :type/url  fhirpath-type-url
   :type/kind "system"
   :type/name (get-fhirpath-type fhirpath-type-url)})


(defn- parse-type [type-code]
  (if (str/starts-with? type-code "http://")
    (parse-system-type type-code)
    (parse-fhir-type type-code)))


(defn- mk-type-sym [fhir-sequence parsed-type]
  (symbol (str "zen.fhir.bindings.fhir-" fhir-sequence
               "." (:type/kind parsed-type) "-types"
               "/" (:type/name parsed-type))))


(defn- mk-type-binding [fhir-sequence fhir-version type-code]
  (let [{:as parsed-type :type/keys [url code]} (parse-type type-code)

        sym (mk-type-sym fhir-sequence parsed-type)

        zen-def {:zen/tags #{'zen/schema 'zen/binding 'zen.fhir/type-binding}
                 :fhirSequence fhir-sequence
                 :fhirVersion fhir-version
                 :url url
                 :code code}]

    {:zf/symbol sym
     :zf/binding {sym zen-def}}))


(defn- value->zen
  "#{:type :binding :contentReference :maxLength :base}"
  [ctx value]
  (when (some? value)
    (merge
      (when-let [types (seq (:type value))]
        (let [fhir-version  (get-in ctx [:zf/strdef :zf/meta :fhirVersion])
              fhir-sequence (fhir-version->sequence-mapping fhir-version)
              bindings      (->> types
                                 (keep :code)
                                 (map #(mk-type-binding fhir-sequence fhir-version %)))
              binding-syms  (into #{} (map :zf/symbol) bindings)
              binding-defs  (into {} (map :zf/binding) bindings)]
          (when (seq bindings)
            {:zf/bindings binding-defs
             :zf/schema {:type 'zen.fhir/element
                         :zen.fhir/el {:confirms binding-syms}}})))
      #_(when-let [max-length (:maxLength value)]
          {:type 'zen/string
           :maxLength max-length}))))


(defn- context->zen
  "{<source :zf/id> #{:constraint}}"
  [ctx context])


(defn- meta->zen
  "#{:isModifier :isSummary :mustSupport :representation}"
  [ctx meta-data])


(defn- description->zen
  "#{:alias :code :comment :definition :example :isModifierReason
     :label :mapping :orderMeaning :requirements :short}"
  [ctx description])


(defn- nested->zen*
  "#{:zf/description :zf/loc :zf/meta
     :zf/context :zf/els :zf/els-cnstraints
     :zf/slicing :zf/container :zf/value
     :zf/poly-roots :zf/poly-keys}"
  [ctx nested]
  (utils.merge/safe-deep-merge
    (els->zen ctx (:zf/els nested))
    (els-constraints->zen ctx (:zf/els-constraints nested))
    (container->zen ctx (:zf/container nested))
    (slicing->zen ctx (:zf/slicing nested))
    (poly-roots->zen ctx (:zf/poly-roots nested))
    (poly-keys->zen ctx (:zf/poly-keys nested))
    (value->zen ctx (:zf/value nested))
    (context->zen ctx (:zf/context nested))
    (meta->zen ctx (:zf/meta nested))
    (description->zen ctx (:zf/description nested))))


(defn nested->zen
  "Returns #{:zf/schema :zf/bindings}"
  [ctx nested]
  (-> (nested->zen* ctx nested)
      (update :zf/schema merge {:zen/tags #{'zen/schema}})))


(defn symbols-map->zen-nss [symbols-map]
  (->> symbols-map
       (group-by #(namespace (key %)))
       (map (fn [[zen-ns symbols]]
              (into {:ns (symbol zen-ns)
                     :import #{'zen.fhir}}
                    (map (fn [[qsym sym-def]]
                           [(symbol (name qsym))
                            sym-def]))
                    symbols)))))

