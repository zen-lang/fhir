(ns zen.fhir.generator-test
  (:require
   [zen.fhir.generator :as sut]
   [zen.core]
   [matcho.core :as matcho]
   [clojure.java.io :as io]
   [clojure.test :as t]))

(def qsd   {:name "Quantity",
            :abstract false,
            :type "Quantity",
            :resourceType "StructureDefinition",
            :status "active",
            :id "Quantity",
            :kind "complex-type",
            :url "http://hl7.org/fhir/StructureDefinition/Quantity",
            :differential
            {:element
             [{:constraint
               [{:key "qty-3",
                 :severity "error",
                 :human
                 "If a code for the unit is present, the system SHALL also be present",
                 :expression "code.empty() or system.exists()",
                 :xpath "not(exists(f:code)) or exists(f:system)"}],
               :path "Quantity",
               :min 0,
               :definition
               "A measured amount (or an amount that can potentially be measured). Note that measured amounts include amounts that are not precisely quantified, including amounts involving arbitrary units and floating currencies.",
               :short "A measured or measurable amount",
               :mapping
               [{:identity "v2", :map "SN (see also Range) or CQ"}
                {:identity "rim", :map "PQ, IVL<PQ>, MO, CO, depending on the values"}],
               :extension
               [{:url
                 "http://hl7.org/fhir/StructureDefinition/structuredefinition-standards-status",
                 :valueCode "normative"}
                {:url
                 "http://hl7.org/fhir/StructureDefinition/structuredefinition-normative-version",
                 :valueCode "4.0.0"}],
               :max "*",
               :id "Quantity",
               :comment
               "The context of use may frequently define what kind of quantity this is and therefore what kind of units can be used. The context of use may also restrict the values for the comparator."}
              {:path "Quantity.value",
               :requirements
               "Precision is handled implicitly in almost all cases of measurement.",
               :min 0,
               :definition
               "The value of the measured amount. The value includes an implicit precision in the presentation of the value.",
               :short "Numerical value (with implicit precision)",
               :mapping
               [{:identity "v2", :map "SN.2  / CQ - N/A"}
                {:identity "rim",
                 :map
                 "PQ.value, CO.value, MO.value, IVL.high or IVL.low depending on the value"}],
               :type [{:code "decimal"}],
               :max "1",
               :id "Quantity.value",
               :comment
               "The implicit precision in the value should always be honored. Monetary values have their own rules for handling precision (refer to standard accounting text books).",
               :isSummary true}
              {:path "Quantity.comparator",
               :requirements
               "Need a framework for handling measures where the value is <5ug/L or >400mg/L due to the limitations of measuring methodology.",
               :min 0,
               :definition
               "How the value should be understood and represented - whether the actual value is greater or less than the stated value due to measurement issues; e.g. if the comparator is \"<\" , then the real value is < stated value.",
               :isModifier true,
               :short "< | <= | >= | > - how to understand the value",
               :mapping
               [{:identity "v2", :map "SN.1  / CQ.1"}
                {:identity "rim", :map "IVL properties"}],
               :type [{:code "code"}],
               :meaningWhenMissing
               "If there is no comparator, then there is no modification of the value",
               :binding
               {:extension
                [{:url
                  "http://hl7.org/fhir/StructureDefinition/elementdefinition-bindingName",
                  :valueString "QuantityComparator"}],
                :strength "required",
                :description "How the Quantity should be understood and represented.",
                :valueSet "http://hl7.org/fhir/ValueSet/quantity-comparator|4.0.1"},
               :max "1",
               :id "Quantity.comparator",
               :isModifierReason
               "This is labeled as \"Is Modifier\" because the comparator modifies the interpretation of the value significantly. If there is no comparator, then there is no modification of the value",
               :isSummary true}
              {:path "Quantity.unit",
               :requirements
               "There are many representations for units of measure and in many contexts, particular representations are fixed and required. I.e. mcg for micrograms.",
               :min 0,
               :definition "A human-readable form of the unit.",
               :short "Unit representation",
               :mapping
               [{:identity "v2", :map "(see OBX.6 etc.) / CQ.2"}
                {:identity "rim", :map "PQ.unit"}],
               :type [{:code "string"}],
               :extension
               [{:url
                 "http://hl7.org/fhir/StructureDefinition/elementdefinition-translatable",
                 :valueBoolean true}],
               :max "1",
               :id "Quantity.unit",
               :isSummary true}
              {:path "Quantity.system",
               :requirements
               "Need to know the system that defines the coded form of the unit.",
               :min 0,
               :definition
               "The identification of the system that provides the coded form of the unit.",
               :short "System that defines coded unit form",
               :mapping
               [{:identity "v2", :map "(see OBX.6 etc.) / CQ.2"}
                {:identity "rim", :map "CO.codeSystem, PQ.translation.codeSystem"}],
               :type [{:code "uri"}],
               :max "1",
               :id "Quantity.system",
               :condition ["qty-3"],
               :isSummary true}
              {:path "Quantity.code",
               :requirements
               "Need a computable form of the unit that is fixed across all forms. UCUM provides this for quantities, but SNOMED CT provides many units of interest.",
               :min 0,
               :definition
               "A computer processable form of the unit in some unit representation system.",
               :short "Coded form of the unit",
               :mapping
               [{:identity "v2", :map "(see OBX.6 etc.) / CQ.2"}
                {:identity "rim", :map "PQ.code, MO.currency, PQ.translation.code"}],
               :type [{:code "code"}],
               :max "1",
               :id "Quantity.code",
               :comment
               "The preferred system is UCUM, but SNOMED CT can also be used (for customary units) or ISO 4217 for currency.  The context of use may additionally require a code from a particular system.",
               :isSummary true}]},
            :contact [{:telecom [{:system "url", :value "http://hl7.org/fhir"}]}],
            :baseDefinition "http://hl7.org/fhir/StructureDefinition/Element"})

(def qzs {'Quantity
          {:zen/tags #{'zen/schema 'complex-type 'fhir/profile}
           :zen/desc "A measured or measurable amount",
           :confirms #{'Element} ;; [:baseDefinition]
           #_:effects #_{'fhir/binding {:strength "extensible",
                                        :description "Appropriate units for Duration.",
                                        :valueSet "http://hl7.org/fhir/ValueSet/duration-units"}

                         'fhir/constraint {"qty-3"
                                           {:severity "error",
                                            :human "If a code for the unit is present, the system SHALL also be present",
                                            :expression "code.empty() or system.exists()",}}}
           :type 'zen/map
           :keys {:value {:confirms #{'decimal}
                          :zen/desc "Numerical value (with implicit precision)"}
                  :comparator {:type 'zen/string
                               ;; :fhir/isSummary true ;;TODO
                               ;; :fhir/isModifier true
                               }
                  :unit {:type 'zen/string
                         :zen/desc "Unit representation"}
                  :system {:confirms #{'uri}
                           :zen/desc "System that defines coded unit form"}
                  :code {:confirms #{'code}}}}})


(def patient-sd (read-string (slurp (io/resource "zen/fhir/pt-sd.edn"))))
(def patient-zs (read-string (slurp (io/resource "zen/fhir/pt-zs.edn"))))


(t/deftest differential-schema
  (t/testing "complex-type"
    (matcho/match
      (sut/structure-definitions->zen-project
        'fhir.R4-test
        "http://hl7.org/fhir/StructureDefinition/Quantity"
        [qsd]
        :remove-gen-keys? true
        :fold-schemas? true
        :elements-mode :differential)
      [qzs]))

  (t/testing "resource"
    (matcho/match
      (sut/structure-definitions->zen-project
        'fhir.R4-test
        "http://hl7.org/fhir/StructureDefinition/Patient"
        [patient-sd]
        :remove-gen-keys? true
        :fold-schemas? true
        :elements-mode :differential)
      [{'Patient patient-zs}])))


(t/deftest structure-definition-conversion
  (t/testing "rich parsed path"
    (t/is (= [{:key "Element"}
              {:key "foo"}
              {:slice "bar"}
              {:key "baz"}
              {:poly "quux[x]"}
              {:key "quuxCodeableConcept"}
              {:key "code"}
              {:poly "value[x]"}]
             (sut/rich-parse-path "Element.foo:bar.baz.quux[x]:quuxCodeableConcept.code.value[x]")))

    (t/is (= "Element.foo:bar.baz.quuxCodeableConcept.code"
             (sut/format-rich-path [{:key "Element"}
                                    {:key "foo"}
                                    {:slice "bar"}
                                    {:key "baz"}
                                    {:poly "quux[x]"}
                                    {:key "quuxCodeableConcept"}
                                    {:key "code"}
                                    {:poly "value[x]"}]))))

  (t/testing "id sanitization"
    (t/is (= ["Resource.fieldComplex.valuePrimitive"
              "Resource.fieldComplex.value_x_"
              "Resource.value_x_"
              "Resource.value"
              "Resource"
              nil
              nil]
             (mapv (comp sut/format-rich-id sut/rich-parse-path)
                   ["Resource.field[x]:fieldComplex.value[x]:valuePrimitive"
                    "Resource.field[x]:fieldComplex.value[x]"
                    "Resource.value[x]"
                    "Resource.value"
                    "Resource"
                    ""
                    nil]))))

  (t/testing "various root ids"
    (t/is (= [[0 [true false false]]
              [1 [true false false]]
              [2 [true false false]]
              [3 [false false false]]
              [4 [false false true]]
              [5 [false false false]]
              [6 [false true false]]
              [7 [false false false]]
              [8 [false false false]]
              [9 [false false false]]]
             (->> [nil
                   ""
                   "Element"
                   "Element.foo"
                   "Element.foo:bar"
                   "Element.foo:bar.baz"
                   "Element.foo:bar.baz.quux[x]"
                   "Element.foo:bar.baz.quux[x]:quuxCode"
                   "Element.foo:bar.baz.quux[x]:quuxCodeableConcept"
                   "Element.foo:bar.baz.quux[x]:quuxCodeableConcept.code"]
                  (mapv (juxt sut/root-element? sut/poly-root? sut/slice-root?))
                  (map-indexed vector)))))


  (t/testing "element -> zen"
    (t/testing "pattern -> zen"
      (matcho/match
        (sut/pattern->zen
          {:system "http://hl7.org/fhir/sid/us-npi"
           :type   {:text "foo"}})
        {:type 'zen/map
         :keys {:system {:const {:value "http://hl7.org/fhir/sid/us-npi"}},
                :type   {:type 'zen/map
                         :keys {:text {:const {:value "foo"}}}}}})

      (matcho/match
        (sut/element->zen
          {:patternCodeableConcept
           {:coding
            [{:system "http://terminology.hl7.org/CodeSystem/v2-0203",
              :code "MB"}
             {:system "sys",
              :code "code"}]}})
        '{:type zen/map
          :validation-type :open
          :keys {:coding
                 {:type zen/vector
                  :slicing {:slices {"RESERVED-aidbox-array-pattern-slicing-0"
                                     {:filter {:engine :zen
                                               :zen {:type zen/map
                                                     :validation-type :open
                                                     :keys {:system {:const {:value "http://terminology.hl7.org/CodeSystem/v2-0203"}}
                                                            :code {:const {:value "MB"}}}}}
                                      :schema {:type zen/vector
                                               :minItems 1}}

                                     "RESERVED-aidbox-array-pattern-slicing-1"
                                     {:filter {:engine :zen
                                               :zen {:type zen/map
                                                     :validation-type :open
                                                     :keys {:system {:const {:value "sys"}}
                                                            :code {:const {:value "code"}}}}}
                                      :schema {:type zen/vector
                                               :minItems 1}}}}}}}))

    (t/testing "multimethod"
      (defmethod sut/ed->zen [:foo :baz] [arg] {:zen/foo arg})

      (defmethod sut/ed->zen [:tar] [arg] {:zen/tar arg})

      (matcho/match
        (sut/element->zen {:foo "bar", :baz "taz", :tar "mar"})
        {:zen/foo {:foo "bar", :baz "taz"}
         :zen/tar {:tar "mar"}})

      (remove-method sut/ed->zen [:tar])
      (remove-method sut/ed->zen [:foo :baz])

      (t/testing "fhir primitive types"
        (matcho/match (sut/element->zen {:type [{:code "id"}]})
                      {:confirms #{'id}})

        (matcho/match (sut/element->zen {:type [{:code      "http://hl7.org/fhirpath/System.String"
                                                 :extension [{:url      "http://hl7.org/fhir/StructureDefinition/structuredefinition-fhir-type"
                                                              :valueUrl "uri"}]}]})
                      {:confirms #{'uri}})

        (matcho/match (sut/element->zen {:type [{:code "HumanName"}]})
                      '{:confirms #{HumanName}}))

      (t/testing "fhir polymorphic types"
        (matcho/match (sut/element->zen {:type [{:code "url"} {:code "code"} {:code "id"}]})
                      {::sut/poly [{:key    :valueUrl
                                    :schema {:confirms #{'url}}}
                                   {:key    :valueCode
                                    :schema {:confirms #{'code}}}
                                   {:key    :valueId
                                    :schema {:confirms #{'id}}}]})))


    (t/testing "path & id"
      (matcho/match
        (mapv sut/element->zen
              [{:id "a", :path "a"}
               {:id "a.b", :path "a.b"}
               {:id "a.c", :path "a.c"}
               {:id "a.b.c", :path "a.b.c"}
               {:id "a.c.d", :path "a.c.d"}
               {:id "a.field[x]", :path "a.field[x]"}
               {:id "a.field[x]:fieldCoding", :path "a.field[x]"}
               {:id "a.field[x]:fieldCoding.code", :path "a.field[x].code"}])
        [#::sut{:id 'a, :key :a, :parent nil}
         #::sut{:id 'a.b, :key :b, :parent 'a}
         #::sut{:id 'a.c, :key :c, :parent 'a}
         #::sut{:id 'a.b.c, :key :c, :parent 'a.b}
         #::sut{:id 'a.c.d, :key :d, :parent 'a.c}
         #::sut{:id 'a.field_x_, :key nil?, :parent 'a}
         #::sut{:id 'a.fieldCoding, :key :fieldCoding, :parent 'a}
         #::sut{:id 'a.fieldCoding.code, :key :code, :parent 'a.fieldCoding}]))

    (t/testing "map or vector"
      (matcho/match
        (mapv sut/element->zen
              [{:id "a", :path "a", :min 1, :max "1", :base {:max "1"}}
               {:id "b", :path "b", :min 1, :max "1", :base {:max "*"}}
               {:id "a.c", :path "a.c", :min 1, :max "1", :base {:max "1"}}
               {:id "a.d", :path "a.d", :min 1, :max "1", :base {:max "*"}}
               {:id "a.b", :path "a.b", :min 0, :max "*"}
               {:id "a.b.c", :path "a.b.c", :min 0}
               {:id "a.b.e", :path "a.b.e", :min 0, :max "1"}
               {:id "a.b.d", :path "a.b.d", :min 1, :max "2", :base {:max "*"}}])
        [#::sut{:collection? not, :required? true?}
         #::sut{:collection? not, :required? true?}
         #::sut{:collection? not, :required? true?}
         #::sut{:collection? true?, :required? true?}
         #::sut{:collection? true?, :required? not}
         #::sut{:collection? not, :required? not}
         #::sut{:collection? not, :required? not}
         {::sut/collection? true?, :maxItems 2, :minItems 1}])))


  (t/testing "links"
    (matcho/match (mapv sut/element->zen
                        '[{:id   "Extension"
                           :path "Extension"}
                          {:id   "Extension.url"
                           :path "Extension.url"}
                          {:id   "Extension.extension"
                           :path "Extension.extension"}
                          {:id   "Extension.extension.url"
                           :path "Extension.extension.url"}
                          {:id   "Extension.extension.value[x]"
                           :path "Extension.extension.value[x]"}
                          {:id   "Extension.extension.value[x]:valueCoding"
                           :path "Extension.extension.value[x]"}
                          {:id   "Extension.extension.value[x]:valueCoding.code"
                           :path "Extension.extension.value[x].code"}
                          {:id   "Extension.extension.value[x]:valueCode"
                           :path "Extension.extension.value[x]"}
                          {:id   "Extension.extension:daysOfWeek"
                           :path "Extension.extension"}
                          {:id   "Extension.extension:daysOfWeek.url"
                           :path "Extension.extension.url"}])
                  [#::sut{:id     'Extension
                          :key    :Extension
                          :parent nil}
                   #::sut{:id     'Extension.url
                          :key    :url
                          :parent 'Extension}
                   #::sut{:id     'Extension.extension
                          :key    :extension
                          :parent 'Extension}
                   #::sut{:id     'Extension.extension.url
                          :key    :url
                          :parent 'Extension.extension}
                   #::sut{:id     'Extension.extension.value_x_
                          :key    nil?
                          :parent 'Extension.extension}
                   #::sut{:id     'Extension.extension.valueCoding
                          :key    :valueCoding
                          :parent 'Extension.extension}
                   #::sut{:id     'Extension.extension.valueCoding.code
                          :key    :code
                          :parent 'Extension.extension.valueCoding}
                   #::sut{:id     'Extension.extension.valueCode
                          :key    :valueCode
                          :parent 'Extension.extension}
                   #::sut{:id     'Extension.extension:daysOfWeek
                          :key    nil?
                          :parent nil?}
                   #::sut{:id     'Extension.extension:daysOfWeek.url
                          :key    :url
                          :parent 'Extension.extension:daysOfWeek}]))


  (t/testing "link schemas"
    (matcho/match
      (sut/link-schemas '[#::sut{:id a, :key :a, :parent nil}
                          #::sut{:id a.b, :key :b, :parent a}
                          #::sut{:id a.c, :key :c, :parent a}
                          #::sut{:id a.b.c, :key :c, :parent a.b}
                          #::sut{:id a.c.d, :key :d, :parent a.c}])
      '{a     #::sut{:links #{a.b a.c}},
        a.b   #::sut{:links #{a.b.c}},
        a.c   #::sut{:links #{a.c.d}},
        a.b.c #::sut{:links #{}},
        a.c.d #::sut{:links #{}}}))

  (t/testing "build-schemas"
    (t/testing "build keys"
      (matcho/match (sut/build-schemas
                      '{a     #::sut{:id a :key :a :parent nil :links #{a.b a.c}}
                        a.b   #::sut{:id a.b :key :b :parent a :links #{a.b.c}}
                        a.c   #::sut{:id a.c :key :c :parent a :links #{a.c.d}}
                        a.b.c #::sut{:id a.b.c :key :c :parent a.b :links #{}}
                        a.c.d #::sut{:id a.c.d :key :d :parent a.c :links #{}}})
                    {'a     {:keys {:b {:confirms #{'a.b,}} :c {:confirms #{'a.c}}}},
                     'a.b   {:keys {:c {:confirms #{'a.b.c}}}},
                     'a.c   {:keys {:d {:confirms #{'a.c.d}}}},
                     'a.b.c #(not (contains? % :keys)),
                     'a.c.d #(not (contains? % :keys))}))

    (t/testing "build every & types"
      (matcho/match (sut/build-schemas
                      '{a     #::sut{:id a :key :a :parent nil :collection? false :links #{a.b}}
                        a.b   #::sut{:id a.b :key :b :parent a :collection? true :links #{a.b.c a.b.d}}
                        a.b.c #::sut{:id a.b.c :key :c :parent a.b :collection? false :links #{}}
                        a.b.d #::sut{:id a.b.d :key :d :parent a.b :collection? false :links #{}}})
                    {'a     {:type 'zen/map
                             :keys {:b {:confirms #{'a.b}}}}
                     'a.b   {:type  'zen/vector
                             :every {:confirms #{'a.b.*}}}
                     'a.b.* {:type 'zen/map
                             :keys {:d {:confirms #{'a.b.d}}
                                    :c {:confirms #{'a.b.c}}}}
                     'a.b.c {:type #(not (contains? '#{zen/map zen/vector} %))}
                     'a.b.d {:type #(not (contains? '#{zen/map zen/vector} %))}})))

  (t/testing "remove gen keys"
    (t/is (= '[{ns  foo1
                bar {:baz1 :taz}}
               {ns  foo2
                bar {:baz :taz}}]
             (sut/remove-gen-keys
               '[{ns  foo1
                  bar {:baz1     :taz
                       ::sut/tar :mar}}
                 {ns  foo2
                  bar {:baz      :taz
                       ::sut/tar :mar}}]))))

  (t/testing "get-deps-urls"
    (t/is (= #{"pr"}
             (sut/get-deps-urls
               '{:snapshot
                 {:element
                  [{}
                   {:type [{:code "ContactPoint"}]}
                   {:type [{:code "Extension"}]}
                   {:type [{:code "uri"}]}
                   {:type [{:code "Extension" :profile ["pr"]}]}]}}))))

  (t/testing "resolve-deps"
    (matcho/match (sut/resolve-deps
                    'lib
                    '{ns     lib.foo
                      import #{aidbox}
                      bar    {:confirms #{baz}
                              :foo      :bar}
                      baz    {::sut/confirms-profile {:urls ["pr"] :symbol Extension}}
                      bar2   {:confirms #{baz2}}
                      baz2   {::sut/confirms-profile {:urls ["pr2" "pr3"] :symbol Extension}}
                      bar3   {:confirms #{baz3}}
                      baz3   {::sut/confirms-profile ["pr5"]}}
                    {"pr"  {:url "pr", :id "baz"}
                     "pr2" {:url "pr2" :id "baz2"}
                     "pr3" {:url "pr3" :id "baz3"}
                     "pr4" {:url "pr3" :id "baz3"}})
                  '{ns     lib.foo
                    import #{aidbox lib.baz lib.baz2}
                    baz    {:confirms #{lib.baz/Extension}}
                    baz2   {:confirms #{lib.baz2/Extension}}
                    baz3   {:confirms nil}})))


(t/deftest structure-definition-with-const
  (t/testing "generating zen schema"
    (def plannet-practitioner-zen-project
      (sut/structure-definitions->zen-project
        'plannet.v1
        "http://fhir.domain/plannet-Practitioner"
        [{:id       "plannet-Practitioner"
          :type     "Practitioner"
          :url      "http://fhir.domain/plannet-Practitioner"
          :kind     "resource"
          :snapshot {:element [{:id "Practitioner" :path "Practitioner" :max "*"}]}}]))

    (matcho/match
      plannet-practitioner-zen-project
      '[{ns     plannet.v1.plannet-Practitioner
         import #{fhir}

         Practitioner
         {:zen/tags #{zen/schema fhir/profile}
          :type     zen/map
          :keys     {:resourceType {:type zen/string, :const {:value "Practitioner"}}}}}]))

  (t/testing "validating resource type"
    (def zctx* (zen.core/new-context {:memory-store {'plannet.v1.plannet-Practitioner
                                                     (first plannet-practitioner-zen-project)}}))
    (zen.core/read-ns zctx* 'fhir)
    (run! (partial zen.core/load-ns zctx*) plannet-practitioner-zen-project)

    (matcho/match @zctx* {:errors empty?})

    (matcho/match (zen.core/validate
                    zctx*
                    #{'plannet.v1.plannet-Practitioner/Practitioner}
                    {:meta {:profile ["http://fhir.domain/plannet-Practitioner"]}})
                  {:errors empty?})

    (matcho/match (zen.core/validate
                    zctx*
                    #{'plannet.v1.plannet-Practitioner/Practitioner}
                    {:resourceType "foo"
                     :meta         {:profile ["http://fhir.domain/plannet-Practitioner"]}})
                  {:errors [{:path [:resourceType nil?]}
                            nil?]})

    (matcho/match (zen.core/validate
                    zctx*
                    #{'plannet.v1.plannet-Practitioner/Practitioner}
                    {:resourceType "Practitioner"
                     :meta         {:profile ["http://fhir.domain/plannet-Practitioner"]}})
                  {:errors empty?})))


(t/deftest simple-structure-definition-with-collections
  (t/testing "generating zen schema"
    (def plannet-practitioner-zen-project
      (sut/structure-definitions->zen-project
        'plannet.v1
        "http://fhir.domain/plannet-Practitioner"
        [{:id   "plannet-Practitioner"
          :type "Practitioner"
          :url  "http://fhir.domain/plannet-Practitioner"
          :kind "resource"
          :snapshot
          {:element
           [{:id "Practitioner", :path "Practitioner", :min 0, :max "*"}
            {:id "Practitioner.id", :path "Practitioner.id", :min 1, :max "1", :type [{:code "id"}]}
            {:id "Practitioner.name", :path "Practitioner.name", :min 1, :max "*", :type [{:code "HumanName"}]}
            {:id "Practitioner.name.given", :path "Practitioner.name.given", :min 1, :max "*", :type [{:code "string"}]}
            {:id "Practitioner.meta", :path "Practitioner.meta", :min 0, :max "1", :type [{:code "Meta"}]}
            {:id "Practitioner.meta.profile", :path "Practitioner.meta.profile", :min 0, :max "*", :type [{:code "canonical"}]}]}}]
        :fold-schemas? false))

    (matcho/match
      plannet-practitioner-zen-project
      '[{ns     plannet.v1.plannet-Practitioner
         import #{fhir}

         Practitioner
         {:zen/tags #{zen/schema fhir/profile}
          :type     zen/map
          :require  #{:id :name}
          :keys     {:resourceType {:type zen/string, :const {:value "Practitioner"}}
                     :id           {:confirms #{Practitioner.id}}
                     :name         {:confirms #{Practitioner.name}}
                     :meta         {:confirms #{Practitioner.meta}}}}

         Practitioner.id
         {:zen/tags #{zen/schema}
          :confirms #{id}}

         Practitioner.name
         {:zen/tags #{zen/schema}
          :type     zen/vector
          :minItems 1
          :every    {:confirms #{Practitioner.name.*}}}

         Practitioner.name.*
         {:zen/tags #{zen/schema}
          :type     zen/map
          :require  #{:given}
          :keys     {:given {:confirms #{Practitioner.name.given}}}}

         Practitioner.name.given
         {:type     zen/vector
          :minItems 1
          :every    {:confirms #{Practitioner.name.given.*}}}

         Practitioner.name.given.*
         {:zen/tags #{zen/schema}
          :confirms #{string}}

         Practitioner.meta
         {:zen/tags #{zen/schema}
          :type     zen/map
          :keys     {:profile {:confirms #{Practitioner.meta.profile}}}}

         Practitioner.meta.profile
         {:zen/tags #{zen/schema}
          :type     zen/vector
          :every    {:confirms #{Practitioner.meta.profile.*}}}

         Practitioner.meta.profile.*
         {:zen/tags #{zen/schema}
          :confirms #{canonical}}}]))

  #_(t/testing "validating with generated zen schema"
    (def zctx* (zen.core/new-context {:memory-store {'plannet.v1.plannet-Practitioner
                                                     (first plannet-practitioner-zen-project)}}))
    (zen.core/read-ns zctx* 'fhir)
    (run! (partial zen.core/load-ns zctx*) plannet-practitioner-zen-project)

    (matcho/match @zctx* {:errors empty?})

    (matcho/match (proto.zen.core/validate-applied-profiles
                    {:zen/ctx zctx*}
                    "Practitioner"
                    {:resourceType "Practitioner"
                     :id           "pr-1"
                     :name         [{:given "Aybolit"}]
                     :meta         {:profile ["http://fhir.domain/plannet-Practitioner"]}})
                  {:errors [{:path [:name 0 :given nil?]}
                            {:path [:name 0 :given nil?]}
                            nil?]})

    (matcho/match (proto.zen.core/validate-applied-profiles
                    {:zen/ctx zctx*}
                    "Practitioner"
                    {:resourceType "Practitioner"
                     :id           "pr-1"
                     :name         [{:given ["Aybolit"]}]
                     :meta         {:profile ["http://fhir.domain/plannet-Practitioner"]}})
                  {:errors empty?})))


(t/deftest structure-definition-with-polymorphic-types
  (t/testing "generating zen schema"
    (def plannet-practitioner-zen-project
      (sut/structure-definitions->zen-project
        'plannet.v1
        "http://fhir.domain/qualification-ext"
        [{:id       "qualification-ext"
          :type     "Extension"
          :url      "http://fhir.domain/qualification-ext"
          :kind     "complex-type"
          :snapshot {:element [{:id "Extension", :path "Extension"}
                               {:id   "Extension.value[x]",
                                :path "Extension.value[x]",
                                :min  0,
                                :max  "1",
                                :type [{:code "url"} {:code "code"} {:code "id"}]
                                :base {:path "Extension.value[x]", :min 0, :max "1"}}
                               {:id   "Extension.foo",
                                :path "Extension.foo",
                                :min  0,
                                :max  "*"}
                               {:id   "Extension.foo.value[x]",
                                :path "Extension.foo.value[x]",
                                :min  0,
                                :max  "1",
                                :type [{:code "url"} {:code "code"} {:code "id"}]
                                :base {:path "Extension.value[x]", :min 0, :max "1"}}]}}]))

    (matcho/match
      plannet-practitioner-zen-project
      '[{Extension
         {:type           zen/map
          :exclusive-keys #{#{:valueCode :valueId :valueUrl}}
          :keys           {:valueCode {:confirms #{code}}
                           :valueId   {:confirms #{id}}
                           :valueUrl  {:confirms #{url}}
                           :foo       {:confirms #{Extension.foo}}}}

         Extension.foo
         {:type  zen/vector
          :every {:confirms #{Extension.foo.*}}}

         Extension.foo.*
         {:type zen/map
          :keys {:valueCode {:confirms #{code}}
                 :valueId   {:confirms #{id}}
                 :valueUrl  {:confirms #{url}}}}}]))

  #_(t/testing "validating resource type"
    (def zctx*
      (proto.zen.core/initialize
        {:deps {'plannet.v1.qualification-ext {:edn/data plannet-practitioner-zen-project}}}))

    (matcho/match @zctx* {:errors empty?})

    (matcho/match (zen.core/validate
                    zctx*
                    #{'plannet.v1.qualification-ext/Extension}
                    {:valueUrl "1"
                     :other    [{:valueUrl "1"}
                                {:valueCode "code"}
                                {:valueUrl "1"}]})
                  {:errors empty?})

    (matcho/match (zen.core/validate
                    zctx*
                    #{'plannet.v1.qualification-ext/Extension}
                    {:valueUrl  "1"
                     :valueCode "code"
                     :other     [{:valueUrl "1"}
                                 {:valueUrl  "1"
                                  :valueCode "code"}
                                 {:valueUrl "1"}]})
                  {:errors seq})))


(t/deftest structure-definition-with-complex-types-&-polymorphic-complex-types
  (def us-bmi-obs-zen-project
    (sut/structure-definitions->zen-project
      'us-core.test
      "http://hl7.org/fhir/us/core/StructureDefinition/pediatric-bmi-for-age"
      [{:id       "pediatric-bmi-for-age",
        :type     "Observation",
        :kind     "resource",
        :url      "http://hl7.org/fhir/us/core/StructureDefinition/pediatric-bmi-for-age"
        :snapshot {:element [{:path "Observation",
                              :min  0,
                              :max  "*",
                              :id   "Observation",
                              :base {:path "Observation", :min 0, :max "*"}}
                             {:path "Observation.subject",
                              :min  1,
                              :type
                              [{:code "Reference",
                                :targetProfile
                                ["http://hl7.org/fhir/us/core/StructureDefinition/us-core-patient"]}],
                              :max  "1",
                              :id   "Observation.subject",
                              :base {:path "Observation.subject", :min 0, :max "1"}}
                             {:path      "Observation.value[x]",
                              :min       0,
                              :slicing
                              {:discriminator [{:type "type", :path "$this"}],
                               :ordered       false,
                               :rules         "closed"},
                              :type      [{:code "Quantity"}],
                              :max       "1",
                              :id        "Observation.value[x]",
                              :condition ["obs-7" "vs-2"],
                              :base      {:path "Observation.value[x]", :min 0, :max "1"}}
                             {:path      "Observation.value[x]",
                              :min       0,
                              :type      [{:code "Quantity"}],
                              :sliceName "valueQuantity",
                              :max       "1",
                              :id        "Observation.value[x]:valueQuantity",
                              :condition ["obs-7" "vs-2"],
                              :base      {:path "Observation.value[x]", :min 0, :max "1"}}
                             {:path "Observation.value[x].id",
                              :min  0,
                              :type
                              [{:extension
                                [{:url
                                  "http://hl7.org/fhir/StructureDefinition/structuredefinition-fhir-type",
                                  :valueUrl "string"}],
                                :code "http://hl7.org/fhirpath/System.String"}],
                              :max  "1",
                              :id   "Observation.value[x]:valueQuantity.id",
                              :base {:path "Element.id", :min 0, :max "1"}}
                             {:path "Observation.value[x].extension",
                              :min  0,
                              :slicing
                              {:discriminator [{:type "value", :path "url"}],
                               :description   "Extensions are always sliced by (at least) url",
                               :rules         "open"},
                              :type [{:code "Extension"}],
                              :max  "*",
                              :id   "Observation.value[x]:valueQuantity.extension",
                              :base {:path "Element.extension", :min 0, :max "*"}}
                             {:path "Observation.value[x].value",
                              :min  1,
                              :type [{:code "decimal"}],
                              :max  "1",
                              :id   "Observation.value[x]:valueQuantity.value",
                              :base {:path "Quantity.value", :min 0, :max "1"}}
                             {:path "Observation.value[x].comparator",
                              :min  0,
                              :type [{:code "code"}],
                              :binding
                              {:extension
                               [{:url
                                 "http://hl7.org/fhir/StructureDefinition/elementdefinition-bindingName",
                                 :valueString "QuantityComparator"}],
                               :strength    "required",
                               :description "How the Quantity should be understood and represented.",
                               :valueSet    "http://hl7.org/fhir/ValueSet/quantity-comparator|4.0.1"},
                              :max  "1",
                              :id   "Observation.value[x]:valueQuantity.comparator",
                              :base {:path "Quantity.comparator", :min 0, :max "1"},}
                             {:path "Observation.value[x].unit",
                              :min  1,
                              :type [{:code "string"}],
                              :extension
                              [{:url          "http://hl7.org/fhir/StructureDefinition/elementdefinition-translatable",
                                :valueBoolean true}],
                              :max  "1",
                              :id   "Observation.value[x]:valueQuantity.unit",
                              :base {:path "Quantity.unit", :min 0, :max "1"}}
                             {:path      "Observation.value[x].system",
                              :min       1,
                              :fixedUri  "http://unitsofmeasure.org",
                              :type      [{:code "uri"}],
                              :max       "1",
                              :id        "Observation.value[x]:valueQuantity.system",
                              :condition ["qty-3"],
                              :base      {:path "Quantity.system", :min 0, :max "1"}}
                             {:path      "Observation.value[x].code",
                              :fixedCode "%",
                              :min       1,
                              :type      [{:code "code"}],
                              :max       "1",
                              :id        "Observation.value[x]:valueQuantity.code",
                              :base      {:path "Quantity.code", :min 0, :max "1"}}]}}]
      :strict-deps false))

  (matcho/match
    us-bmi-obs-zen-project
    '[{}])

  #_(t/testing "validating zen schema"
    (def zctx*
      (proto.zen.core/initialize
        {:deps {'us-core.test.pediatric-bmi-for-age {:edn/data us-bmi-obs-zen-project}}}))

    (matcho/match @zctx* {:errors empty?})

    (def bmi-obs
      {:resourceType  "Observation",
       :meta          {:profile ["http://hl7.org/fhir/us/core/StructureDefinition/pediatric-bmi-for-age"]},
       :subject       {:reference "Patient/child-example", :display "Child Example"}
       :valueQuantity {:value  65,
                       :unit   "%",
                       :system "http://unitsofmeasure.org",
                       :code   "%"}})

    (matcho/match (zen.core/validate
                    zctx*
                    #{'us-core.test.pediatric-bmi-for-age/Observation}
                    bmi-obs)
                  {:errors empty?})

    (matcho/match (zen.core/validate
                    zctx*
                    #{'us-core.test.pediatric-bmi-for-age/Observation}
                    (update bmi-obs :valueQuantity dissoc :system))
                  {:errors seq})))
