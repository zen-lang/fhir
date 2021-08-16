(ns zen.fhir.generator-test
  (:require
   [zen.fhir.generator :as sut]
   [zen.core]
   [matcho.core :as matcho]
   [clojure.java.io :as io]
   [clojure.test :as t]))


(defn load-zen-project! [proj]
  (let [proj-map (into {} (map #(-> [(get % 'ns) %])) proj)
        zctx* (zen.core/new-context {:memory-store proj-map})]
    (zen.core/read-ns zctx* 'fhir)
    (run! (partial zen.core/load-ns zctx*) proj)
    zctx*))


(t/deftest structure-definition-conversion
  (t/testing "rich parsed path"
    (t/is (= [{:key "Element"}
              {:key "foo"}
              {:slice "bar"}
              {:key "baz"}
              {:poly "quux"}
              {:key "CodeableConcept", :poly-name "quux"}
              {:key "code"}
              {:poly "value"}]
             (sut/rich-parse-path "Element.foo:bar.baz.quux[x]:quuxCodeableConcept.code.value[x]")))

    (t/is (= "Element.foo:bar.baz.quux.CodeableConcept.code.value"
             (sut/format-rich-path [{:key "Element"}
                                    {:key "foo"}
                                    {:slice "bar"}
                                    {:key "baz"}
                                    {:poly "quux"}
                                    {:key "CodeableConcept", :poly-name "quux"}
                                    {:key "code"}
                                    {:poly "value"}]))))

  (t/testing "id sanitization"
    #_(t/is (= "Resource.field.Complex.value.primitive" ;; NOTE: this assert can't be done without type
               (-> "Resource.field[x]:fieldComplex.value[x]:valuePrimitive"
                   sut/rich-parse-path sut/format-rich-id)))

    (t/is (= "Resource.field.Complex.value"
             (-> "Resource.field[x]:fieldComplex.value[x]"
                 sut/rich-parse-path sut/format-rich-id)))

    (t/is (= "Resource.value" (-> "Resource.value[x]" sut/rich-parse-path sut/format-rich-id)))
    (t/is (= "Resource.value" (-> "Resource.value" sut/rich-parse-path sut/format-rich-id)))
    (t/is (= "Resource"       (-> "Resource" sut/rich-parse-path sut/format-rich-id)))
    (t/is (= nil              (-> "" sut/rich-parse-path sut/format-rich-id)))
    (t/is (= nil              (-> nil sut/rich-parse-path sut/format-rich-id))))

  (t/testing "various root ids"
    (def jxt (juxt sut/root-element? sut/poly-root? sut/slice-root?))
    (t/is (= [true  false false] (jxt nil)))
    (t/is (= [true  false false] (jxt "")))
    (t/is (= [true  false false] (jxt "Element")))
    (t/is (= [false false false] (jxt "Element.foo")))
    (t/is (= [false false true]  (jxt "Element.foo:bar")))
    (t/is (= [false false false] (jxt "Element.foo:bar.baz")))
    (t/is (= [false true  false] (jxt "Element.foo:bar.baz.quux[x]")))
    (t/is (= [false false false] (jxt "Element.foo:bar.baz.quux[x]:quuxCode")))
    (t/is (= [false false false] (jxt "Element.foo:bar.baz.quux[x]:quuxCodeableConcept")))
    (t/is (= [false false false] (jxt "Element.foo:bar.baz.quux[x]:quuxCodeableConcept.code"))))

  (t/testing "element -> zen"
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
        (matcho/match (sut/element->zen {:type [{:code "id"}]}
                                        {::sut/fhir-lib 'fhir.test})
                      {:confirms #{'fhir.test/id}})

        (matcho/match (sut/element->zen {:type [{:code      "http://hl7.org/fhirpath/System.String"
                                                 :extension [{:url      "http://hl7.org/fhir/StructureDefinition/structuredefinition-fhir-type"
                                                              :valueUrl "uri"}]}]}
                                        {::sut/fhir-lib 'fhir.test})
                      {:confirms #{'fhir.test/uri}})

        (matcho/match (sut/element->zen {:type [{:code "HumanName"}]}
                                        {::sut/fhir-lib 'fhir.test})
                      '{:confirms #{fhir.test/HumanName}}))

      (t/testing "fhir polymorphic types"
        (matcho/match (sut/element->zen {:type [{:code "url"} {:code "code"} {:code "id"}]}
                                        {::sut/fhir-lib 'fhir.test})
                      {::sut/poly
                       {:key  :value
                        :keys {:url  {:confirms #{'fhir.test/url}}
                               :code {:confirms #{'fhir.test/code}}
                               :id   {:confirms #{'fhir.test/id}}}}})

        (matcho/match (sut/element->zen {:id "foo.bar.baz[x]"
                                         :type [{:code "type"}]}
                                        {::sut/fhir-lib 'fhir.test})
                      {::sut/poly
                       {:key  :baz
                        :keys {:type {:confirms #{'fhir.test/type}}}}})))

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
          :keys {:coding
                 {:type zen/vector
                  :slicing {:slices {"RESERVED-aidbox-array-pattern-slicing-0"
                                     {:filter {:engine :zen
                                               :zen {:type zen/map
                                                     :keys {:system {:const {:value "http://terminology.hl7.org/CodeSystem/v2-0203"}}
                                                            :code {:const {:value "MB"}}}}}
                                      :schema {:type zen/vector
                                               :minItems 1}}

                                     "RESERVED-aidbox-array-pattern-slicing-1"
                                     {:filter {:engine :zen
                                               :zen {:type zen/map
                                                     :keys {:system {:const {:value "sys"}}
                                                            :code {:const {:value "code"}}}}}
                                      :schema {:type zen/vector
                                               :minItems 1}}}}}}}))


    (t/testing "id"
      (matcho/match
        (mapv sut/element->zen
              [{:id "a", :path "a"}
               {:id "a.b", :path "a.b"}
               {:id "a.c", :path "a.c"}
               {:id "a.b.c", :path "a.b.c"}
               {:id "a.c.d", :path "a.c.d"}
               {:id "a.field[x]", :path "a.field[x]"}
               {:id "a.field[x]:fieldCoding", :path "a.field[x]" :type [{:code "Coding"}]}
               {:id "a.field[x]:fieldCoding.code", :path "a.field[x].code"}
               {:id "a.field[x]:fieldCode", :path "a.field[x]" :type [{:code "code"}]}])
        [#::sut{:id 'a, :key :a, :parent nil}
         #::sut{:id 'a.b, :key :b, :parent 'a}
         #::sut{:id 'a.c, :key :c, :parent 'a}
         #::sut{:id 'a.b.c, :key :c, :parent 'a.b}
         #::sut{:id 'a.c.d, :key :d, :parent 'a.c}
         #::sut{:id 'a.field, :key :field, :parent 'a}
         #::sut{:id 'a.field.Coding, :key :Coding, :parent 'a.field}
         #::sut{:id 'a.field.Coding.code, :key :code, :parent 'a.field.Coding}
         #::sut{:id 'a.field.code, :key :code, :parent 'a.field}]))

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
                           :type [{:code "code"}]
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
                   #::sut{:id     'Extension.extension.value
                          :key    :value
                          :parent 'Extension.extension}
                   #::sut{:id     'Extension.extension.value.Coding
                          :key    :Coding
                          :parent 'Extension.extension.value}
                   #::sut{:id     'Extension.extension.value.Coding.code
                          :key    :code
                          :parent 'Extension.extension.value.Coding}
                   #::sut{:id     'Extension.extension.value.code
                          :key    :code
                          :parent 'Extension.extension.value}
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
         {:zen/tags #{zen/schema fhir/profile fhir/resource}
          :type     zen/map
          :keys     {:resourceType {:type zen/string, :const {:value "Practitioner"}}}}}]))

  (t/testing "validating resource type"
    (def zctx* (load-zen-project! plannet-practitioner-zen-project))

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
         {:zen/tags #{zen/schema fhir/profile fhir/resource}
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
    (def zctx* (load-zen-project! plannet-practitioner-zen-project))

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
                                :min  1,
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
                                :base {:path "Extension.value[x]", :min 0, :max "1"}}]}}]
        :fold-schemas? true))

    (matcho/match
      plannet-practitioner-zen-project
      '[{Extension
         {:type    zen/map
          :format  :aidbox
          :require #{:value}
          :keys    {:value {:type           zen/map
                            :exclusive-keys #{#{:code :id :url}}
                            :keys           {:code {:confirms #{code}}
                                             :id   {:confirms #{id}}
                                             :url  {:confirms #{url}}}}
                    :foo   {:type  zen/vector
                            :every {:type zen/map
                                    :keys {:value {:type zen/map
                                                   :keys {:code {:confirms #{code}}
                                                          :id   {:confirms #{id}}
                                                          :url  {:confirms #{url}}}}}}}}}}]))

  #_(t/testing "validating resource type"
    (def zctx* (load-zen-project! plannet-practitioner-zen-project))

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
      :fold-schemas? true
      :remove-gen-keys? true
      :strict-deps false))

  (matcho/match
    us-bmi-obs-zen-project
    '[{Observation
       {:format :aidbox
        :keys {:value
               {:type zen/map
                :keys {:Quantity
                       {:confirms #{Quantity}
                        :keys {:extension  {:type zen/vector, :every {:confirms #{Extension}}}
                               :comparator {}
                               :id         {}
                               :system     {}
                               :unit       {}
                               :code       {}
                               :value      {}}}}}}}}])

  #_(t/testing "validating zen schema"
    (def zctx* (load-zen-project! us-bmi-obs-zen-project))

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


(t/deftest differential-schema
  (t/testing "complex-type"
    (def qsd (read-string (slurp (io/resource "zen/fhir/quantity-sd.edn"))))

    (matcho/match
      (sut/structure-definitions->zen-project
        'fhir.R4-test
        "http://hl7.org/fhir/StructureDefinition/Quantity"
        [qsd]
        :remove-gen-keys? true
        :fold-schemas?    true
        :elements-mode    :differential
        :fhir-lib         'fhir.R4-test)
      '[{ns fhir.R4-test.Quantity
         import #{fhir}

         Quantity
         {:zen/tags #{zen/schema fhir/complex-type fhir/profile}
          :zen/desc "A measured or measurable amount",
          #_#_:confirms #{fhir.R4-test/Element} ;; TODO: should be namespaced
          :confirms #{Element} ;; [:baseDefinition]
          #_:effects #_{fhir/binding {:strength "extensible",
                                      :description "Appropriate units for Duration.",
                                      :valueSet "http://hl7.org/fhir/ValueSet/duration-units"}

                        fhir/constraint {"qty-3"
                                         {:severity "error",
                                          :human "If a code for the unit is present, the system SHALL also be present",
                                          :expression "code.empty() or system.exists()",}}}
          :type zen/map
          :keys {:value {:confirms #{fhir.R4-test/decimal}
                         :zen/desc "Numerical value (with implicit precision)"}
                 :comparator {:type zen/string
                              ;; :fhir/isSummary true ;;TODO
                              ;; :fhir/isModifier true
                              }
                 :unit {:type zen/string
                        :zen/desc "Unit representation"}
                 :system {:confirms #{fhir.R4-test/uri}
                          :zen/desc "System that defines coded unit form"}
                 :code {:confirms #{fhir.R4-test/code}}}}}])

    (t/testing "duration"
      (def duration-sd (read-string (slurp (io/resource "zen/fhir/duration-sd.edn"))))

      (matcho/match
        (sut/structure-definitions->zen-project
          'fhir.R4-test
          "http://hl7.org/fhir/StructureDefinition/Duration"
          [duration-sd]
          :remove-gen-keys? true
          :fold-schemas? true
          :elements-mode :differential
          :fhir-lib      'fhir.R4-test)
        '[{ns fhir.R4-test.Duration
           import #{fhir}

           Duration
           {:zen/tags #{zen/schema fhir/complex-type fhir/profile}
            :type zen/map
            #_#_:confirms #{fhir.R4-test/Quantity} ;; TODO
            :confirms #{Quantity}}}])))

  (t/testing "resource"
    (def patient-sd (read-string (slurp (io/resource "zen/fhir/pt-sd.edn"))))

    (def patient-proj
      (sut/structure-definitions->zen-project
        'fhir.R4-test
        "http://hl7.org/fhir/StructureDefinition/Patient"
        [patient-sd]
        :remove-gen-keys? true
        :fold-schemas?    true
        :elements-mode    :differential
        :fhir-lib         'fhir.R4-test))

    (matcho/match
      patient-proj
      '[{Patient
         {:zen/tags #{fhir/profile fhir/resource zen/schema}
          :zen/desc "Demographics and other administrative information about an individual or animal receiving care or other health-related services."
          #_"Information about an individual or animal receiving health care services",
          #_#_:confirms #{fhir.R4-test/DomainResource} ;; TODO
          :confirms #{DomainResource}
          :type zen/map
          :keys {:identifier {:type zen/vector
                              #_#_:zen/desc "An identifier for this patient",
                              :every {:confirms #{fhir.R4-test/Identifier}
                                      :zen/desc "An identifier for this patient",}}
                 :active {:type zen/boolean}
                 :name {:type zen/vector
                        :every {:confirms #{fhir.R4-test/HumanName}}}
                 :telecom {:type zen/vector
                           :every {:confirms #{fhir.R4-test/ContactPoint}}}
                 :gender  {:confirms #{fhir.R4-test/code}
                           #_#_:effects {fhir/binding {:strength "required",
                                                       :description "The gender of a person used for administrative purposes.",
                                                       :valueSet "http://hl7.org/fhir/ValueSet/administrative-gender|4.0.1"}}}
                 :birthDate {:confirms #{fhir.R4-test/date}}
                 :deceased {:type zen/map
                            :exclusive-keys #{#{:boolean :dateTime}}
                            :keys {:boolean {:type zen/boolean
                                             :confirms #{fhir.R4-test/boolean}}
                                   :dateTime {:type zen/datetime
                                              :confirms #{fhir.R4-test/dateTime}}}}
                 :contact {:type zen/vector
                           :every {:type zen/map
                                   :keys {:relationship
                                          {:type zen/vector
                                           :every {:confirms #{fhir.R4-test/CodeableConcept}}}
                                          :name {:confirms #{fhir.R4-test/HumanName}}}}}
                 :managingOrganization {:confirms #{fhir.R4-test/Reference}}}}}])

    #_(testing "validating zen"
      (def patient-res (read-string (slurp (io/resource "zen/fhir/pt-res.edn"))))

      (def zctx* (load-zen-project! patient-proj))

      (matcho/match @zctx* {:errors empty?})

      (matcho/match (zen.core/validate
                      zctx*
                      #{'fhir.R4-test.Patient/Patient}
                      patient-res)
                    {:errors empty?}))))
