(ns zen.fhir.generator
  (:require [zen.fhir.utils :as utils]
            [zen.fhir.inter-utils]
            [zen.fhir.writer]
            [com.rpl.specter :as sp]
            [clojure.walk]
            [clojure.string :as str]))


(def order-zen-ns            zen.fhir.writer/order-zen-ns)
(def format-zen-ns           zen.fhir.writer/format-zen-ns)
(def spit-zen-schemas        zen.fhir.writer/spit-zen-schemas)
(def spit-ndjson-gz-bundle!  zen.fhir.writer/spit-ndjson-gz-bundle!)
(def spit-terminology-bundle zen.fhir.writer/spit-terminology-bundle)
(def collect-packages        zen.fhir.writer/collect-packages)
(def spit-zen-modules        zen.fhir.writer/spit-zen-modules)
(def spit-zen-npm-modules    zen.fhir.writer/spit-zen-npm-modules)
(def packages-deps-nses      zen.fhir.inter-utils/packages-deps-nses)


(defmulti generate-kind-schema
  (fn [_fhir-inter [_url inter-res]]
    (keyword (:kind inter-res))))


(def fhir-primitive->zen-primitive
  '{"boolean" {:type zen/boolean}

    "decimal"     {:type zen/number}
    "integer"     {:type zen/integer}
    "unsignedInt" {:type zen/integer
                   :min  0}
    "positiveInt" {:type zen/integer
                   :min  1}

    "string"       {:type      zen/string
                    :maxLength 1048576}
    "markdown"     {:type      zen/string
                    :maxLength 1048576}
    "id"           {:type  zen/string
                    :regex "[A-Za-z0-9\\-\\.]{1,64}"}
    "uuid"         {:type  zen/string}
    "oid"          {:type  zen/string
                    :regex "urn:oid:[0-2](\\.(0|[1-9][0-9]*))+"}
    "uri"          {:type zen/string}
    "url"          {:type zen/string}
    "canonical"    {:type zen/string}
    "code"         {:type  zen/string
                    :regex "[^\\s]+(\\s[^\\s]+)*"}
    "base64Binary" {:type  zen/string
                    :regex "(\\s*([0-9a-zA-Z\\+\\=]){4}\\s*)+"}
    "xhtml"        {:type zen/string}

    "instant"  {:type  zen/string
                :regex "([0-9]([0-9]([0-9][1-9]|[1-9]0)|[1-9]00)|[1-9]000)-(0[1-9]|1[0-2])-(0[1-9]|[1-2][0-9]|3[0-1])T([01][0-9]|2[0-3]):[0-5][0-9]:([0-5][0-9]|60)(\\.[0-9]+)?(Z|(\\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))"}
    "dateTime" {:type  zen/string
                :regex "([0-9]([0-9]([0-9][1-9]|[1-9]0)|[1-9]00)|[1-9]000)(-(0[1-9]|1[0-2])(-(0[1-9]|[1-2][0-9]|3[0-1])(T([01][0-9]|2[0-3]):[0-5][0-9]:([0-5][0-9]|60)(\\.[0-9]+)?(Z|(\\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00)))?)?)?"}
    "date"     {:type  zen/string
                :regex "([0-9]([0-9]([0-9][1-9]|[1-9]0)|[1-9]00)|[1-9]000)(-(0[1-9]|1[0-2])(-(0[1-9]|[1-2][0-9]|3[0-1]))?)?"}
    "time"     {:type  zen/string
                :regex "([01][0-9]|2[0-3]):[0-5][0-9]:([0-5][0-9]|60)(\\.[0-9]+)?"}})


(defn confirms-base [fhir-inter [_url inter-res]]
  (let [base-schema (some-> (get-in fhir-inter ["StructureDefinition" (:baseDefinition inter-res) :zen.fhir/schema-ns])
                            name
                            (symbol "schema"))]
    {:confirms (when base-schema #{base-schema})}))


(defmethod generate-kind-schema :primitive-type [fhir-inter [url inter-res]]
  (let [tp         (or (get-in inter-res [:| :value :type])
                       (:type inter-res))
        zen-type-schema   (fhir-primitive->zen-primitive tp)]
    (merge zen-type-schema
           (when (not= "http://hl7.org/fhir/StructureDefinition/Element"
                       (:baseDefinition inter-res))
             (confirms-base fhir-inter [url inter-res])))))


(defn url->symbol [fhir-inter url & [err-ctx]]
  (when-not (nil? url)
    (if-let [ext-ns (get-in fhir-inter ["StructureDefinition" url :zen.fhir/schema-ns])]
      (symbol (name ext-ns) "schema")
      (println :ERROR :unresolved-symbol url err-ctx))))


(defn value-set->symbol [fhir-inter {:keys [url]}]
  (when-let [value-set (get-in fhir-inter ["ValueSet" url])]
    (symbol (str (:zen.fhir/package-ns value-set) ".value-set." (:id value-set)) ;; TODO: use :zen.fhir/schema-ns
            "value-set")))


(declare els-schema)


(defn slice-schema [fhir-inter url [slice-k slice]]
  (let [confirms (when-let [tp (:type slice)]
                   (url->symbol fhir-inter
                                (str "http://hl7.org/fhir/StructureDefinition/" tp)
                                {:type :slice-type :e slice :url url}))
        slice-filter (cond
                       (some? (:match slice))
                       {:engine :match
                        :match (:match slice)})
        slice-schema (merge {:type 'zen/vector}
                            (when-let [min-items (:minItems slice)]
                              {:minItems min-items})
                            (when-let [max-items (:maxItems slice)]
                              {:maxItems max-items})
                            (when (seq (:| slice))
                              {:every (merge (when confirms
                                               {:confirms #{confirms}})
                                             (els-schema fhir-inter [url slice]))}))]
    (if (not slice-filter)
      (prn "WARN: omitting slice without any filter: " url " " slice)
      [slice-k
       {:schema slice-schema
        :filter slice-filter}])))


(defn slicing-schema [fhir-inter [url inter-res]]
  (when-let [slicing (not-empty (:fhir/slicing inter-res))]
    {:slicing {:slices (into {}
                             (keep #(slice-schema fhir-inter url %))
                             (:slices slicing))}}))


(defn fixed-schema [fhir-inter [url el]]
  (when-let [fixed (:fixed el)]
    {:const {:value fixed}}))


(defn pattern-schema [fhir-inter [url el]]
  (when-let [pattern (:pattern el)]
    {:match (clojure.walk/postwalk #(cond-> %
                                      (and (sequential? %)
                                           (not (map-entry? %)))
                                      set)
                                   pattern)}))


(defn el-schema [fhir-inter [url el]]
  (let [sch (merge-with
             into
             {}
             (fixed-schema fhir-inter [url el])
             (pattern-schema fhir-inter [url el])
             (when-let [type-sym (when-let [tp (and (:type el))]
                                   (let [tp-url (str "http://hl7.org/fhir/StructureDefinition/" tp)]
                                     (when (not= url tp-url)
                                       (url->symbol fhir-inter
                                                    (str "http://hl7.org/fhir/StructureDefinition/" tp)
                                                    {:type :element-type :e el :url url}))))]
               {:confirms #{type-sym}})
             (when-let [ext-sym (when-let [ext (:fhir/extension el)]
                                  (url->symbol fhir-inter ext {:type :extension :el el :url url}))]
               {:confirms #{ext-sym}})
             (when-let [ext-uri (and (or (nil? (:type el)) (= "Extension" (:type el)))
                                     (:fhir/extension el))]
               {:fhir/extensionUri ext-uri})
             (when (:polymorphic el)
               (let [types (into #{} (map keyword) (:types el))]
                 (utils/strip-nils
                  {:fhir/polymorphic true
                   :exclusive-keys (when (<= 2 (count types))
                                     #{types})})))
             (when (seq (:| el))
               (els-schema fhir-inter [url el]))
             (when (seq (:fhir/flags el))
               (select-keys el #{:fhir/flags}))
             (when-let [value-set-sym (some->> (get-in el [:binding :valueSet])
                                               (value-set->symbol fhir-inter))]
               {:zen.fhir/value-set {:symbol value-set-sym
                                     :strength (keyword (get-in el [:binding :strength]))}})
             (when (= "Reference" (:type el))
               {:confirms #{'zen.fhir/Reference}
                :zen.fhir/reference
                {:refers (->> (:profiles el)
                              (remove #(= % "http://hl7.org/fhir/StructureDefinition/Resource"))
                              (keep (fn [x] (url->symbol fhir-inter x {:type :reference :el el :url url})))
                              (into #{}))}})
             (when-let [el-recur-sym (get-in el [:recur :symbol])]
               {:confirms #{el-recur-sym}})
             (when (:nested el)
               {:zen.fhir/nested {}})

             (when-let [text (or (:short el) (:definiton el))]
               {:zen/desc text}))
        slicing (when (seq (:fhir/slicing el))
                  (slicing-schema fhir-inter [url el]))]
    (merge slicing
           (if (:vector el)
             (merge {:type 'zen/vector
                     :every sch}
                    (select-keys el [:minItems :maxItems]))
             sch))))


(defn els-schema [fhir-inter [url inter-res]]
  (merge (when-let [els (not-empty (:| inter-res))]
           {:type 'zen/map
            :keys (sp/transform [sp/MAP-VALS]
                                #(el-schema fhir-inter [url %])
                                els)})
         (when-let [requires (->> (:| inter-res)
                                  (into #{}
                                        (comp
                                          (filter (comp :required val))
                                          (map key)))
                                  not-empty)]
           {:require requires})))


(defmethod generate-kind-schema :complex-type [fhir-inter [url inter-res]]
  (merge
    (confirms-base fhir-inter [url inter-res])
    (el-schema fhir-inter [url (dissoc inter-res :fhir/extension)])))


(defmethod generate-kind-schema :resource [fhir-inter [url inter-res]]
  (merge-with
    into
    {:confirms #{'zen.fhir/Resource}}
    (confirms-base fhir-inter [url inter-res])
    (els-schema fhir-inter [url inter-res])))


(defmethod generate-kind-schema :logical [fhir-inter [url inter-res]]
  (merge
    (confirms-base fhir-inter [url inter-res])
    (els-schema fhir-inter [url inter-res])))


(defmethod generate-kind-schema :first-class-extension [fhir-inter [url inter-res]]
  (merge
    (confirms-base fhir-inter [url inter-res])
    (el-schema fhir-inter [url (dissoc inter-res :fhir/extension)]))) ;; Maybe dissoc in core?


(defn generate-zen-schema-dispatch [rt _fhir-inter [_url _inter-res]]
  (keyword rt))


(defmulti generate-zen-schema #'generate-zen-schema-dispatch)


(defmethod generate-zen-schema :default [_rt _fhir-inter [_url _inter-res]])


(defn find-value-set-systems [rt fhir-inter [url inter-res]]
  (-> (:compose inter-res)
      (select-keys [:include :exclude])
      (->> (into #{}
                 (mapcat (fn [[inclusion-status compose-elements]]
                           (mapcat (fn [compose-elem]
                                     (let [nested-systems
                                           (mapcat (fn [nested-vs-url]
                                                     (when-let [nested-vs (get-in fhir-inter ["ValueSet" nested-vs-url])]
                                                       (find-value-set-systems rt fhir-inter [nested-vs-url nested-vs])))
                                                   (:valueSet compose-elem))]
                                       (if-let [system (:system compose-elem)]
                                         (let [content     (if (or (contains? compose-elem :concept)
                                                               (= "complete" (get-in fhir-inter ["CodeSystem" system :content])))
                                                         :bundled
                                                         :not-present)
                                               code-system {:fhir/url         system
                                                            :zen.fhir/content content}]
                                           ;; We don’t want to bundle CodeSystem if its every concept is excluded
                                           (if (and (= inclusion-status :exclude) (= content :bundled))
                                             nested-systems
                                             (cons code-system nested-systems)))
                                         (if (= inclusion-status :exclude)
                                           (filter #(not= :bundled (:zen.fhir/content %)) nested-systems)
                                           nested-systems))))
                                   compose-elements)))))))


(defmethod generate-zen-schema :ValueSet [rt fhir-inter [url inter-res]]
  (let [schema-ns    (:zen.fhir/schema-ns inter-res)
        code-systems (find-value-set-systems rt fhir-inter [url inter-res])]
    {schema-ns
     {'ns     schema-ns
      'import #{'zen.fhir}
      'value-set
      (utils/strip-nils
        {:zen/tags #{'zen.fhir/value-set}
         :zen/desc (:description inter-res)
         :zen.fhir/version (:zen.fhir/version inter-res)
         :fhir/code-systems code-systems
         :uri (:url inter-res)
         :version (:version inter-res)})}}))


(defn path-in-schema [schema keys-path]
  (reduce (fn [path path-el]
            (conj (if (get-in schema (conj path :keys))
                    (conj path :keys)
                    (apply conj path [:every :keys]))
                  path-el))
          []
          keys-path))


(defn path-to-fhir-map-schema [schema keys-path]
  (let [path-to  (path-in-schema schema keys-path)
        ref-node (get-in schema path-to)]
    (if (= (:type ref-node) 'zen/vector)
      (conj (vec path-to) :every)
      path-to)))


(defn extract-recur-referred-schema [_fhir-inter [_url inter-res] zen-ns-map recur-ref]
  (let [ref-ns-sym (:symbol recur-ref)
        ref-name   (name ref-ns-sym)
        ref-sym    (symbol ref-name)

        ref-path (->> (path-to-fhir-map-schema (get zen-ns-map 'schema)
                                               (:path recur-ref))
                      (cons 'schema))

        ref-schema (merge {:zen/tags #{'zen/schema 'zen.fhir/structure-schema}
                           :zen.fhir/version (:zen.fhir/version inter-res)}
                          (get-in zen-ns-map ref-path))]

    (-> zen-ns-map
        (assoc ref-sym ref-schema)
        (assoc-in ref-path {:confirms #{ref-ns-sym}}))))


(defn extract-recur-referred-schemas [_fhir-inter [_url inter-res] schema-ns-map]
  (reduce #(extract-recur-referred-schema _fhir-inter [_url inter-res] %1 %2)
          schema-ns-map
          (sort-by (fn [{:keys [path]}] (- (count path)))
                   (get inter-res :recur-refs))))


(defmethod generate-zen-schema :StructureDefinition [_rt fhir-inter [url inter-res]]
  (let [inter-res   (cond-> inter-res ;; NOTE: should be done in zen.fhir.core when inter-res are generated
                      (and (= "Extension" (:type inter-res))
                           (contains? inter-res :fhir/extension))
                      (dissoc :type))
        schema-ns   (:zen.fhir/schema-ns inter-res)
        imports     (into #{'zen.fhir}
                          (keep (fn [inter-path]
                                  (get-in fhir-inter (conj inter-path :zen.fhir/schema-ns))))
                          (mapcat (fn [[tp urls&reasons]] (map (fn [url] [tp url]) (keys urls&reasons)))
                                  (:deps inter-res)))
        instantiated-resource? (and (not (:abstract inter-res))
                                    (= "resource" (:kind inter-res)))
        severity-tag           (case (and instantiated-resource?
                                          (:derivation inter-res))
                                 "constraint"     'zen.fhir/profile-schema
                                 "specialization" 'zen.fhir/base-schema
                                 'zen.fhir/structure-schema)
        schema-part            (generate-kind-schema fhir-inter [url inter-res])
        this-schema-sym        (symbol (name schema-ns) "schema")
        schema                 (-> (utils/safe-merge-with-into
                                     {:zen/tags            (into #{'zen/schema}
                                                                 (when severity-tag [severity-tag]))
                                      :zen/desc            (:text-description inter-res)
                                      :zen.fhir/type       (:type inter-res)
                                      :zen.fhir/profileUri url
                                      :zen.fhir/version    (:zen.fhir/version inter-res)}
                                     schema-part)
                                   (update :confirms (comp not-empty disj) this-schema-sym)
                                   utils/strip-nils)
        ns-with-all-schemas    (extract-recur-referred-schemas fhir-inter
                                                               [url inter-res]
                                                               {'schema schema})]
    {schema-ns (merge {'ns     schema-ns
                       'import (disj imports schema-ns)}
                      ns-with-all-schemas)}))


(defmethod generate-zen-schema :SearchParameter [_rt fhir-inter [url inter-res]]
  (let [schema-ns (:zen.fhir/schema-ns inter-res)]
    {schema-ns {'ns     schema-ns
                'import #{'zen.fhir}
                'search
                {:zen/tags #{'zen.fhir/search}
                 :fhir/id       (:id inter-res)
                 :fhir/url      (:url inter-res)
                 :fhir/type     (:type inter-res)
                 :name  (:sp-name inter-res)
                 :expr     (:expr inter-res)}}}))


(defn ns-by-package [fhir-inter]
  (let [inter-by-package
        (->> fhir-inter
             (sp/select [sp/MAP-VALS sp/MAP-VALS #(contains? % :zen.fhir/schema-ns)])
             (group-by :zen.fhir/package-ns))]
    (sp/transform [sp/MAP-VALS sp/ALL]
                  :zen.fhir/schema-ns
                  inter-by-package)))


(defn create-resource-quoted-symbol [resource rt]
  (with-meta
    (symbol (name (:zen.fhir/schema-ns resource))
            (case rt
              "StructureDefinition" "schema"
              "ValueSet"            "value-set"
              "SearchParameter"     "search"))
    {:zen/quote true}))


(defn ig-entrypoint-resource-path [url rt resource]
  (let [schema-resource-type (:type resource)

        ig-artifact-type
        (case rt #_"NOTE: if unknown RT then generator will throw exception here. This is intended behavior"
          "ValueSet"        :value-sets
          "SearchParameter" :searches
          "StructureDefinition"
          (cond
            (and (= "Extension" schema-resource-type)
                 (= "constraint" (:derivation resource)))
            :extensions

            (and (= "resource" (:kind resource))
                 (= "constraint" (:derivation resource)))
            :profiles

            (and (= "resource" (:kind resource))
                 (= "specialization" (:derivation resource)))
            :base-schemas

            :else
            :structures))]

    (case ig-artifact-type
      (:profiles :base-schemas)
      [ig-artifact-type schema-resource-type url]

      :searches
      [ig-artifact-type (:name resource) url]

      [ig-artifact-type url])))


(defn symbols-by-package [fhir-inter]
  (reduce-kv (fn [acc rt resources]
               (reduce-kv (fn [acc url resource]
                                (cond-> acc
                                  (contains? resource :zen.fhir/schema-ns)
                                  (assoc-in (cons (:zen.fhir/package-ns resource)
                                                  (ig-entrypoint-resource-path url rt resource))
                                            (create-resource-quoted-symbol resource rt))))
                              acc
                              resources))
                 {}
                 fhir-inter))


(defn root-package-ns [fhir-inter package-ns package-symbols]
  (let [dep-nss (set (concat #{'zen.fhir}
                             (get (zen.fhir.inter-utils/packages-deps-nses
                                    fhir-inter)
                                  package-ns)))

        base-schema-symbols (:base-schemas package-symbols)
        profile-symbols     (:profiles package-symbols)
        extension-symbols   (:extensions package-symbols)
        structure-symbols   (:structures package-symbols)
        value-set-symbols   (:value-sets package-symbols)
        search-symbols      (:searches package-symbols)]

    {package-ns
     (merge-with merge
       {'ns     package-ns
        'import dep-nss

        'ig {:zen/tags #{'zen.fhir/ig}}}

       (when (seq base-schema-symbols)
         {'ig {:base-schemas 'base-schemas}
          'base-schemas {:zen/tags #{'zen.fhir/base-schemas}
                         :schemas base-schema-symbols}})

       (when (seq profile-symbols)
         {'ig {:profiles 'profiles}
          'profiles {:zen/tags #{'zen.fhir/profiles}
                     :schemas profile-symbols}})

       (when (seq extension-symbols)
         {'ig {:extensions 'extensions}
          'extensions {:zen/tags #{'zen.fhir/extensions}
                       :schemas extension-symbols}})

       (when (seq structure-symbols)
         {'ig {:structures 'structures}
          'structures {:zen/tags #{'zen.fhir/structures}
                       :schemas structure-symbols}})

       (when (seq value-set-symbols)
         {'ig {:value-sets 'value-sets}
          'value-sets {:zen/tags #{'zen.fhir/value-sets}
                       :value-sets value-set-symbols}})

       (when (seq search-symbols)
         {'ig {:searches 'searches}
          'searches {:zen/tags #{'zen.fhir/searches}
                     :searches search-symbols}}))}))


(defn generate-root-package-nses [fhir-inter]
  (into {}
        (map #(apply root-package-ns fhir-inter %))
        (symbols-by-package fhir-inter)))


(defn collect-deps [zen-ns]
  (let [used-zen-nss (atom #{})]
    (clojure.walk/postwalk
      (fn [x] (when (qualified-symbol? x)
                (swap! used-zen-nss conj (symbol (namespace x)))))
      zen-ns)
    (update zen-ns 'import
            (fn [imp]
              (-> (or imp #{})
                  (into @used-zen-nss)
                  (disj 'zen))))))


(defn generate-zen-schemas* [fhir-inter]
  (into (generate-root-package-nses fhir-inter)
        (for [[rt inters] fhir-inter
              inter       inters]
          (sp/transform [sp/MAP-VALS]
                        collect-deps
                        (generate-zen-schema rt fhir-inter inter)))))


(defn generate-zen-schemas [ztx]
  (swap! ztx assoc :fhir.zen/ns (generate-zen-schemas* (:fhir/inter @ztx)))
  :done)


;; * resources, types
;;  -> sd (+deps => ctx)
;;    ->  elements
;;      -> element-> zen -> transformed element
;;      -> element -> resolve to symbols
;;      -> aggregate into zen tree - temp keys


;; npm
;; -> temp storage
;;    by resourceType
;;    SD{ulr {res} } => ctx

;; res
;;   -> shaping
;;   1. treeify
;;   2. vectorize (tree algorythm)
