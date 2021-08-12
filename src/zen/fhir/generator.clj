(ns zen.fhir.generator
  (:require [cheshire.core]
            [clojure.pprint]
            [clojure.walk]
            [com.rpl.specter :as sp]
            [zen.utils :as utils]
            [clojure.string :as str]
            [zen.validation]))


#_#_(def path "libs/zen-fhir/")


(def types (cheshire.core/parse-string (slurp (str path "fhir/profiles-types.json")) keyword))


(do ;; TODO: move to utils
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
          values)))


(defmulti ed->zen
  "Converts part of ElementDefinition to part of zen schema
  Used to dispatch all of its defmethods
  by their dispatch value as select-keys arg"
  identity)


(remove-all-methods ed->zen)  ;; To remove defmethods erased from code when re-eval a whole buffer in REPL


(defn rich-parse-path [id]
  (if (str/blank? id)
    []
    (->> (str/split id #"\.")
         (mapcat
           (fn [id-part]
             (let [[key-part slice-part] (str/split id-part #":" 2)]
               (cond
                 (str/ends-with? key-part "[x]") (cond-> [{:poly key-part}]
                                                   (some? slice-part)
                                                   (conj {:key slice-part}))
                 (some? slice-part)              [{:key key-part} {:slice slice-part}]
                 :else                           [{:key key-part}]))))
         vec)))


(defn format-rich-path [rich-path]
  (reduce
    (fn [path-acc rich-part]
      (cond
        (:slice rich-part) (str path-acc ":" (:slice rich-part))
        (:poly rich-part)  path-acc
        :else              (str path-acc "." (:key rich-part))))
    (or (:key (first rich-path))
        (:slice (first rich-path)))
    (rest rich-path)))


(defn format-rich-id [rich-path]
  (let [last-idx (dec (count rich-path))]
    (reduce
      (fn [path-acc {:keys [part last?]}]
        (cond
          (:slice part) (str path-acc ":" (str/replace (:slice part) #"@" "_"))
          (:poly part)  (if last?
                          (str path-acc "." (str/replace (:poly part) #"[\[\]]" "_"))
                          path-acc)
          :else         (str path-acc "." (:key part))))
      (:key (first rich-path))
      (mapv (fn [[idx rich-part]]
              {:part  rich-part
               :last? (= last-idx idx)})
            (rest (map-indexed vector rich-path))))))


(defn slice-root? [id]
  (boolean (re-matches #".+(?<!\[x\]):[^.]+?$" (str id))))


(defn poly-root? [id]
  (str/ends-with? (str id) "[x]"))


(defmethod ed->zen #{:id} [{el-id :id}]
  (when-not (str/blank? el-id)
    (let [rich-path (rich-parse-path el-id)
          attr-key  (when-not (or (poly-root? el-id) (slice-root? el-id))
                      (some-> (take-last 1 rich-path)
                              format-rich-path
                              keyword))
          parent    (some-> (butlast rich-path)
                            format-rich-path
                            symbol)]
      (utils/strip-nils
        {::id      (symbol (format-rich-id rich-path))
         ::key     attr-key
         ::slicing (when (:slice (last rich-path))
                     parent)
         ::parent  (when-not (:slice (last rich-path))
                     parent)}))))


(defn root-element?
  "The first ElementDefinition (root element) usually has max=* which may be treated as a collection
  but we are treating StructureDefinition as a tool to validate a single resource"
  [el-path]
  (not (str/includes? (str el-path) ".")))


(defmethod ed->zen #{:min} [{el-min :min}]
  (when el-min
    {::required? (pos? (or el-min 0))}))


(defmethod ed->zen #{:id :min :max :base}
  [{id :id, el-min :min, el-max :max, {base-max :max} :base}]
  (utils/strip-nils
    (when (and (or (some? base-max)
                   (some? el-max))
               (not (root-element? id))
               (not= "1" base-max)
               (or (some? base-max)
                   (not= "1" el-max)))
      {::collection? true
       :minItems     (when-not (= 0 el-min) el-min)
       :maxItems     (when-not (= "*" el-max) (parse-int el-max))})))


(defmethod ed->zen #{} [_]
  {:zen/tags #{'zen/schema}})


(defmethod ed->zen #{:definition :short} [{:keys [short definition]}]
  (let [short      (when-not (str/blank? short) short)
        definition (when-not (str/blank? definition) definition)

        desc       (or short definition)
        #_#_full-desc  (when short definition)]
    (utils/strip-nils
      {:zen/desc  desc
       #_#_:definition full-desc})))


(def fhir-primitive->zen-primitive
  '{boolean zen/boolean

    decimal     zen/number
    integer     zen/integer
    unsignedInt zen/integer
    positiveInt zen/integer

    string       zen/string
    markdown     zen/string
    id           zen/string
    uuid         zen/string
    oid          zen/string
    uri          zen/string
    url          zen/string
    canonical    zen/string
    code         zen/string
    base64Binary zen/string
    xhtml        zen/string

    instant  zen/string
    dateTime zen/datetime
    date     zen/date
    time     zen/string})


;; StructureDefinition snapshot generator has a bug
;; instead of id type it uses exension type
;; https://chat.fhir.org/#narrow/stream/179283-Da-Vinci/topic/Type.20of.20id/near/232607087
(def sd-type-ext-url "http://hl7.org/fhir/StructureDefinition/structuredefinition-fhir-type")


(defn ed-type->zen-type [{:keys [extension code]}]
  (let [type-symbol (symbol (or (some-> (code-search :url [sd-type-ext-url] extension)
                                        (poly-get :value))
                                code))]
    (utils/strip-nils
      {:confirms #{type-symbol}
       :type     (when-let [zen-primitive (fhir-primitive->zen-primitive type-symbol)]
                   zen-primitive)})))

;; TODO: zen: parse namespaced keywords of current ns
;; TODO: generator: generate fhir


(defn ed-types->zen-type [id ed-types poly?]
  (if poly?
    (let [[_ poly-key'] (re-matches #"^.+\.(.+?)\[x\]$" (str id))
          poly-key      (or poly-key' "value")]
      {::poly {:key  (keyword poly-key)
               :keys (into {}
                           (map (fn [{code :code :as poly-type}]
                                  (assert code "Code should be defined for polymorphic type")
                                  {(keyword code) (ed-type->zen-type poly-type)}))
                           ed-types)}})
    (ed-type->zen-type (first ed-types))))


(defmethod ed->zen #{:type :id} [{el-types :type, :keys [id]}]
  (when (seq el-types)
    (ed-types->zen-type id
                        el-types
                        (or (< 1 (count el-types))
                            (str/ends-with? (str id) "[x]")))))


(defmethod ed->zen #{:slicing :path :id} [{path :path, slicing :slicing, id :id}]
  (when slicing
    {::slicing? true}))


;; NOTE: we can't convert sliceName to keyword because
;; by fhir it is string without any constraints
;; this can create invalid edn keywords
(defmethod ed->zen #{:sliceName :type :id} [{slice-name :sliceName, el-type :type, id :id}]
  (let [rich-path (rich-parse-path id)]
    (when (:slice (last rich-path))
      (merge {::slice-name (if (= "@default" slice-name)
                             :slicing/rest
                             slice-name)}
             (when-let [tp (first el-type)]
               {:type              'zen/map
                ::confirms-profile {:urls   (:profile tp)
                                    :symbol (symbol (:code tp))}})))))

(defn pattern->zen [pattern]
  (cond
    (map? pattern)
    {:type 'zen/map
     :validation-type :open
     :keys (into {}
                 (map (fn [[k v]] {k (pattern->zen v)}))
                 pattern)}

    (sequential? pattern)
    {:type 'zen/vector
     :slicing {:slices (into {}
                             (map-indexed (fn [idx pat-el]
                                            {(str "RESERVED-aidbox-array-pattern-slicing-" idx)
                                             {:filter {:engine :zen
                                                       :zen (pattern->zen pat-el)}
                                              :schema {:type 'zen/vector
                                                       :minItems 1}}}))
                             pattern)}}

    :else {:const {:value pattern}}))


#_(defmethod ed->zen #{:binding :id} [{:keys [binding id]}]
  (when (and binding (= "required" (:strength binding)))
    (let [rich-path (rich-parse-path id)
          name      (some-> (format-rich-id rich-path)
                            (str ".valueset")
                            (symbol))]
      {'aidbox-fx/valueset name
       ::binding {:url (first (str/split (:valueSet binding) #"\|" 2))
                  :name name}})))


(defmethod ed->zen #{:poly/pattern} [{:keys [pattern]}]
  (when (some? pattern)
    (pattern->zen pattern)))


(defmethod ed->zen #{:poly/fixed} [{:keys [fixed]}]
  (when (some? fixed)
    {:const {:value fixed}}))


(defn element->zen
  "Converts ElementDefinition to zen schema
  Result is without :keys field for :type zen/map,
  they will be added later after linking"
  [element]
  (into {}
        (mapcat (fn [[ks f]]
                  (let [{poly-ks "poly", rest-ks nil} (group-by (comp #{"poly"} namespace) ks)]
                    (f (into (select-keys element rest-ks)
                             (map (fn [poly-key]
                                    (let [pk (keyword (name poly-key))]
                                      {pk (poly-get element pk)})))
                             poly-ks)))))
        (methods ed->zen)))


(defn link-schemas
  "Links schemas, results in a such map {schema-id {:links #{schema-ids}, schema ...} ...}"
  [schemas]
  (let [links (group-by ::parent schemas)]
    (into {}
          (map (fn [{:as schema ::keys [id]}]
                 {id (assoc schema ::links (->> (get links id) (map ::id) set))}))
          schemas)))


;; TODO: for better solumtion filter.zen.confirms
;; should be separate simple schema without validation only for data distincion
(defn build-slice [schema]
  (let [schema-id      (::id schema)
        element-schema (symbol (str (name schema-id) ".*"))]
    {(::slice-name schema)
     {:filter {:engine :proto.zen.core/zen
               :zen    {:confirms #{element-schema}}}
      :schema {:confirms #{schema-id}}}}))


(defn build-slices [schemas schema]
  (if (::slicing? schema)
    (let [slices  (into {}
                       (comp (filter (comp #{(::id schema)} ::slicing))
                             (map build-slice))
                       (vals schemas))
          slicing {:slices (dissoc slices :slicing/rest)
                   :rest   (:schema (:slicing/rest slices))}]
      (assoc schema :slicing (utils/strip-nils slicing)))
    schema))


(defn build-require [linked-schema]
  (when (::required? linked-schema)
    (if (::poly linked-schema)
      (get-in linked-schema [::poly :key])
      (::key linked-schema))))


(defn build-key [linked-schema]
  (when-not (or (::poly linked-schema)
                (::slice-name linked-schema))
    {(::key linked-schema)
     {:confirms #{(::id linked-schema)}}}))


(defn build-poly [linked-schema]
  (when-let [poly (::poly linked-schema)]
    {(:key poly) {:type 'zen/map
                  :exclusive-keys #{(set (keys (:keys poly)))}
                  :keys (:keys poly)}}))


(def merge-attrs (partial merge-with (partial merge-with into)))


(defn build-schema [schemas [schema-id schema]]
  (cond
    (::poly schema)
    {}

    (::collection? schema)
    (let [collection-keys [::collection? ::slicing? :minItems :maxItems #_:zen/desc] ;; TODO: must be configurable
          schema-id*      (symbol (str (name (::id schema)) ".*"))
          coll            {schema-id
                           (->> (merge (select-keys schema collection-keys)
                                       (select-keys schema [::id])
                                       {:zen/tags #{'zen/schema}
                                        :type     'zen/vector
                                        :every    {:confirms #{schema-id*}}})
                                (build-slices schemas))}
          coll-el         (build-schema schemas [schema-id* (apply dissoc schema collection-keys)])]
      (merge coll coll-el))

    (seq (::links schema))
    (let [linked-schemas (map schemas (::links schema))
          requires       (into #{} (keep build-require) linked-schemas)
          linked-attrs   (into {} (keep build-key) linked-schemas)
          poly-attrs     (into {} (keep build-poly) linked-schemas)
          schema-attrs   (get-in schemas [(::id schema) :keys])
          attrs          (merge-attrs linked-attrs poly-attrs schema-attrs)
          built-schema   (utils/assoc-some
                           schema
                           :type 'zen/map
                           :require        (not-empty requires)
                           :keys           (not-empty attrs))]
      {schema-id built-schema})

    :else
    {schema-id schema}))


#_(defn build-valueset [[schema-id schema]]
  (cond-> {schema-id schema}
    (::binding schema)
    (assoc (get-in schema [::binding :name])
           {:zen/tags #{'aidbox-fx/valueset-definition 'aidbox-fx/legacy-fhir-valueset-definition}
            :engine   :legacy-fhir-valueset
            :valueset-url (get-in schema [::binding :url])})))


(defn build-schemas
  "Builds :keys for each :type zen/map schema & schema.path.* for each :type zen/vector"
  [schemas]
  (into {}
        (comp
          #_(mapcat build-valueset)
          (mapcat (partial build-schema schemas)))
        schemas))

(defn fold-schemas [schemas]
  (->> schemas
       keys
       (sort (comp - compare))
       (reduce
         (fn [acc schema-id]
           (clojure.walk/postwalk
             (fn [x]
               (if (and (map? x) (contains? (:confirms x) schema-id))
                 (merge-with into (update x :confirms disj schema-id) (get acc schema-id))
                 x))
             acc))
         schemas)))


(defn sd->profile-schema
  "Creates zen schema for root resource from StructureDefinition"
  [{:keys [description type url kind baseDefinition]}]
  (let [base (some-> (when-not (str/blank? baseDefinition) baseDefinition)
                     (str/split #"/")
                     last)]
    (utils/strip-nils
      (merge-with into
                  {::collection?       false ;; in some profiles there is * cardinality for the root resource
                   :zen/tags           #{'zen/schema 'fhir/profile}
                   :zen/desc           description
                   :type               'zen/map
                   :format             :aidbox
                   :validation-type    :open
                   :profile-definition url}
                  (when (or (= "DomainResource" base)
                            (= "Element" base))
                    {:confirms (when-not (str/blank? base)
                                 #{(symbol base)})})
                  (when (= "DomainResource" base)
                    {:zen/tags #{'resource}})
                  (when (= "complex-type" kind)
                    {:zen/tags #{'complex-type}})
                  (when (= "resource" kind)
                    {:severity "supported"
                     :resourceType type
                     :keys         {:resourceType {:type 'zen/string, :const {:value type}}}})))))


(defn order-zen-ns [zen-ns-map]
  (let [zen-ns             (get zen-ns-map 'ns)
        zen-import         (get zen-ns-map 'import)
        rest-zen-ns-map    (dissoc zen-ns-map 'ns 'import)
        ordered-zen-ns-map (cond->> (sort-by key rest-zen-ns-map)
                             (some? zen-import) (cons ['import zen-import])
                             (some? zen-ns)     (cons ['ns zen-ns])
                             :always            flatten
                             :always            (apply array-map))]
    ordered-zen-ns-map))


(defn structure-definition->zen-ns
  "Converts StructureDefinition to zen namespace
  :fold-schemas? true -- to inline attributes into single schema
  :elements-mode :snapshot|:differential to choose where elements are located"
  [zen-lib resource & [{:keys [fold-schemas?
                               elements-mode]
                        :or {elements-mode :snapshot
                             fold-schemas? false}}]]
  (let [resource-type   (symbol (:type resource))
        zen-ns          (symbol (str (name zen-lib) "." (:id resource)))
        elements-key    elements-mode
        element-schemas (link-schemas (map element->zen (get-in resource [elements-key :element])))
        resource-schema (sd->profile-schema resource)
        schemas         (update element-schemas resource-type merge resource-schema)] ;; TODO: maybe merge-with into
    (-> schemas
        build-schemas
        (cond-> fold-schemas? (-> fold-schemas (select-keys [resource-type])))
        (assoc 'ns     zen-ns
               'import #{'fhir})
        order-zen-ns)))


(defn get-deps-urls [core-ns]
  (->> core-ns
       :snapshot
       :element
       (mapcat :type)
       (mapcat :profile)
       (remove nil?)
       set))


(defn resolve-confirms-profile [zen-lib deps-resources]
  (fn [{::keys [confirms-profile] :as sch}]
    (if-let [dep (and confirms-profile (some deps-resources (:urls confirms-profile)))]
      (assoc sch :confirms #{(symbol (str (name zen-lib) \. (:id dep) \/ (:symbol confirms-profile)))})
      sch)))


(defn collect-imports [zen-lib core-ns deps-resources]
  (->> core-ns
       (sp/select [sp/MAP-VALS map? ::confirms-profile])
       (map (fn [confirms-profile]
              {:dep-resource (some deps-resources (:urls confirms-profile))
               :symbol       (:symbol confirms-profile)}))
       (filter :dep-resource)
       (map #(symbol (str (name zen-lib) \. (get-in % [:dep-resource :id]))))))


(defn resolve-deps [zen-lib core-ns deps-resources]
  (let [deps-imports (collect-imports zen-lib core-ns deps-resources)
        resolved-ns  (sp/transform [sp/MAP-VALS map? #(contains? % ::confirms-profile)]
                                   (resolve-confirms-profile zen-lib deps-resources)
                                   core-ns)]
    (update resolved-ns 'import into deps-imports)))


(defn remove-gen-keys [zen-projects]
  (let [this-file-ns (namespace ::_)]
    (clojure.walk/postwalk
      (fn [x]
        (when-not (and (instance? clojure.lang.MapEntry x)
                       (instance? clojure.lang.Named (key x))
                       (= this-file-ns (namespace (key x))))
          x))
      zen-projects)))


(defn structure-definitions->zen-project* [zen-lib core-url deps-resources-map & {:as params, :keys [remove-gen-keys? strict-deps]}]
  (when strict-deps
    (assert (contains? deps-resources-map core-url)
            (str "Couldn't find dependency: " core-url)))
  (let [core-resource    (get deps-resources-map core-url)
        core-ns          (structure-definition->zen-ns zen-lib core-resource params)
        deps-sds-urls    (get-deps-urls core-resource)
        deps-projects    (mapcat (fn [url]
                                   (when (some? (get deps-resources-map url))
                                     (structure-definitions->zen-project* zen-lib url deps-resources-map)))
                                 deps-sds-urls)
        resolved-core-ns (resolve-deps zen-lib core-ns deps-resources-map)
        projects         (cons resolved-core-ns deps-projects)]
    (cond->> projects
      remove-gen-keys? (map remove-gen-keys))))


(defn structure-definitions->zen-project [zen-lib core-url deps-resources
                                          & {:keys [remove-gen-keys? strict-deps
                                                    fold-schemas? elements-mode]
                                             :or   {remove-gen-keys? true
                                                    strict-deps      true
                                                    elements-mode    :snapshot
                                                    fold-schemas?    false}}]
  (let [deps-resources-map (sp/transform [sp/MAP-VALS] first (group-by :url deps-resources))]
    (structure-definitions->zen-project* zen-lib core-url deps-resources-map
                                         :remove-gen-keys? remove-gen-keys?
                                         :strict-deps      strict-deps
                                         :fold-schemas?    fold-schemas?
                                         :elements-mode    elements-mode)))


(comment
  (def plannet-pract
    (-> "http://hl7.org/fhir/us/davinci-pdex-plan-net/STU1/StructureDefinition-plannet-Practitioner.json"
        slurp
        (cheshire.core/parse-string keyword)
        (dissoc :differential :text #_:snapshot)))

  (def us-core-pat
    (-> "http://hl7.org/fhir/us/core/StructureDefinition-us-core-patient.json"
        slurp
        (cheshire.core/parse-string keyword)
        (dissoc :differential :text #_:snapshot)))

  (->> plannet-pract :snapshot :element)

  (->> plannet-pract
       (sp/select [:snapshot :element sp/ALL #(-> % :type first :code (= "http://hl7.org/fhirpath/System.String"))])
       (map #(select-keys % [:path :id]))))
