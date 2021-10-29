(ns zen.fhir.core-test
  (:require [zen.fhir.core :as sut]
            [inspect]
            [clojure.test :as t]
            [zen.core]
            [clojure.pprint]
            [matcho.core :as matcho]))


(defn see-definition-els [ztx url]
  (let [d (sut/get-original ztx url)]
    (->
      (select-keys d [:kind :type :url :baseDefinition :derivation])
      (assoc :els
             (->> (:element (:differential d))
                  (mapv #(select-keys % [:id :min :max :sliceName :binding :fixedUri :type])))))))


(defmacro match-definition [ztx url pattern]
  `(let [res# (sut/get-definition ~ztx ~url)]
     (matcho/match res# ~pattern)
     res#))


(t/deftest element-normalization
  (t/testing "cardinality"
    (matcho/match (-> {:id "a", :min 1, :max "1", :base {:max "1"}}
                      sut/normalize-element)
                  {:required true?, :vector not})

    (matcho/match (-> {:id "b", :min 1, :max "1", :base {:max "*"}}
                      sut/normalize-element)
                  {:required true?, :vector not})

    (matcho/match (-> {:id "a.c", :min 1, :max "1", :base {:max "1"}}
                      sut/normalize-element)
                  {:required true?, :vector not})

    ;; (matcho/match (-> {:id "a.d", :min 1, :max "1", :base {:max "*"}}
    ;;                   sut/normalize-element)
    ;;               {:required true?, :vector true?})

    ;; (matcho/match (-> {:id "a.d", :min 1, :max "1", :base {:max "*"}}
    ;;                   sut/normalize-element)
    ;;               {:required true?, :vector true?})

    (matcho/match (-> {:id "a.b", :min 0, :max "*"}
                      sut/normalize-element)
                  {:required not, :vector true?})

    (matcho/match (-> {:id "a.b.c", :min 0}
                      sut/normalize-element)
                  {:required not, :vector not})

    (matcho/match (-> {:id "a.b.d", :min 0, :max "1"}
                      sut/normalize-element)
                  {:required not, :vector not})

    (matcho/match (-> {:id "a.b.e", :min 1, :max "2", :base {:max "*"}}
                      sut/normalize-element)
                  {:required true?, :vector true?, :minItems 1, :maxItems 2})

    ;; (matcho/match (-> {:id "a.b.f", :max "0", :base {:max "*"}}
    ;;                   sut/normalize-element)
    ;;               {:required not, :vector true?, :maxItems 0})

    ;; (matcho/match (-> {:id "a.b.f", :max "0", :base {:max "1"}}
    ;;                   sut/normalize-element)
    ;;               {:required not, :vector not, :prohibited true?})
    ))


;; 1 use base of base for element
"Profile.meta" "Base" "DomainResource.meta"

;;2.
;; vector should be inherited from base
;; problem that we could not distinct max=1 in Profile is it vector or not
"Profile.attr min/max"  "Base.attr vector"
;; enrich with vector
;; enrich with type

;; 3.
"Profiley.attr.subattr" "Base.attr[Type]" "Type.subattr"
;; enrich with vector
;; enrich with type


;; 4.
;; Polymorics
"Profile.attrType"  "Base.attr [polymorphic]"
;; rename attrType => attr.Type
;; enrich with type
;; polymorphic could not be vector in FHIR!
;; restrict polymorphic

;; 5 first class extensions
;; Extension
;; => Complex Type (extension in extension)
;; => primitive type constraints

;; 6. mount extensions in Profile
;; * give it sliceNames
;; * inline primitive extension constraints ???


;; 7. determine all dependencies ()


(def aztx (zen.core/new-context {}))


(defn load-base [{base-name :name tp :base els :els}]
  (sut/load-definiton
    aztx {}
    {:url (some->> base-name (str "url://"))}
    {:resourceType   "StructureDefinition"
     :url            (some->> base-name (str "url://"))
     :type           tp
     :baseDefinition (some->> tp (str "url://"))
     :derivation     "specialization"
     :differential
     {:element
      (into [{:id base-name}]
            (map (fn [x] (update x :id #(str base-name "." %))))
            els)}}))


(defn load-profile [{prof-name :name base :base els :els}]
  (sut/load-definiton
    aztx {}
    {:url (some->> prof-name (str "url://"))}
    {:resourceType   "StructureDefinition"
     :url            (some->> prof-name (str "url://"))
     :type           base
     :derivation     "constraint"
     :baseDefinition (some->> base (str "url://"))
     :differential
     {:element
      (into [{:id prof-name}]
            (map (fn [x] (update x :id #(str prof-name "." %))))
            els)}}))


(defn load-primitive-type [{type-name :name, els :els}]
  (sut/load-definiton
    aztx {}
    {:url (str "http://hl7.org/fhir/StructureDefinition/" type-name)}
    {:resourceType "StructureDefinition"
     :url          (str "http://hl7.org/fhir/StructureDefinition/" type-name)
     :type         "Element"
     :derivation   "specialization"
     :kind         "primitive-type"
     :differential
     {:element
      (into [{:id type-name}]
            (map (fn [x] (update x :id #(str type-name "." %))))
            els)}}))


(defn load-extension [{ext-name :name els :els}]
  (sut/load-definiton
    aztx {}
    {:url (str "uri://" ext-name)}
    {:resourceType "StructureDefinition"
     :type         "Extension"
     :derivation   "constraint"
     :kind         (if els "complex-type" "primitive")
     :url          (str "uri://" ext-name)
     :differential
     {:element
      (into [{:id ext-name}]
            (map (fn [x] (update x :id #(str ext-name "." %))))
            els)}}))


(defn reload []
  (sut/preprocess-resources aztx)
  (sut/process-resources aztx))


;; * :vector  as or
;; * :required as or
;; * :prohibited as or
;; * no inheritance :minItems
;; * no inheritance :maxItems

;; polymorphic valueString (no other types) if String then
;; polymorphic constraint on type  value[x]:valueString only valueString
;; constraint polymorphic type: []


(t/deftest preprocess-process-resources-test

  (load-primitive-type
    {:name "prim"})

  (load-primitive-type
    {:name "string"})


  (t/testing "inheritence"
    (load-base
      {:name "VectorBase"
       :els  [{:id "attr" :min 0 :max "*" :type [{:code "prim"}]}]})

    (reload)

    (:fhir/inter @aztx)

    (matcho/match
      (sut/get-definition aztx "url://VectorBase")
      {:| {:attr  {:type                "prim"
                   :vector              true
                   :fhir/primitive-attr true}
           :_attr {:type         "Element"
                   :vector       true
                   :original-key :attr}}})

    (load-profile
      {:name "VectorProfile"
       :base "VectorBase"
       :els  [{:id "attr" :max "1"}]})

    (reload)

    (matcho/match
      (sut/get-definition aztx "url://VectorProfile")
      {:| {:attr {:vector true :type "prim"}}}))

  (t/testing "Double inheritece"

    (load-base
      {:name "InhBaseOfBase"
       :els  [{:id "attr" :min 0 :max "*" :type [{:code "prim"}]}]})

    (load-base
      {:name "InhBase"
       :base "InhBaseOfBase"
       :els  []})

    (load-profile
      {:name "InhProfile"
       :base "InhBase"
       :els  [{:id "attr" :max "1"}]})

    (reload)

    (matcho/match
      (sut/get-definition aztx  "url://InhProfile")
      {:| {:attr {:vector true :type "prim"}}}))


  (t/testing "Inherit complex type attrs properties"

    (load-primitive-type
      {:name "ComplexType"
       :els  [{:id "attr" :min 0 :max "*" :type [{:code "prim"}]}]})

    (load-base
      {:name "CBase"
       :els  [{:id "el" :min 0 :max "*" :type [{:code "ComplexType"}]}]})

    (load-profile
      {:name "CProfile"
       :base "CBase"
       :els  [{:id "el" :max "1"}
              {:id "el.attr" :max "1"}]})

    (reload)

    (matcho/match
      (sut/get-definition aztx "url://CProfile")
      {:| {:el {:vector true
                :|      {:attr {:vector true :type "prim"}}}}}))


  (t/testing "Complex type inheritance"
    (load-primitive-type
      {:name "BaseType"
       :els  [{:id "attr" :min 0 :max "*" :type [{:code "prim"}]}]})

    (load-primitive-type
      {:name "InhComplexType"
       :base "BaseType"
       :els  []})

    (load-base
      {:name "InhCBase"
       :els  [{:id "el" :min 0 :max "*" :type [{:code "ComplexType"}]}]})

    (load-profile
      {:name "InhCProfile"
       :base "InhCBase"
       :els  [{:id "el" :max "1"}
              {:id "el.attr" :max "1"}]})

    (reload)

    (matcho/match
      (sut/get-definition aztx "url://InhCProfile")
      {:| {:el {:vector true
                :|      {:attr {:vector true :type "prim"}}}}}))

  (t/testing "Polymorphic shortcut"
    (load-base
      {:name "PBase"
       :els  [{:id "el[x]" :type [{:code "prim"}
                                  {:code "ComplexType"}]}]})

    (load-profile
      {:name "PProfile1"
       :base "PBase"
       :els  [{:id "elPrim"}]})

    (load-profile
      {:name "PProfile2"
       :base "PBase"
       :els  [{:id "elComplexType"}]})

    (load-profile
      {:name "PProfile3"
       :base "PBase"
       :els  [{:id "el[x]:elComplexType"}
              {:id "el[x]:elPrim"}]})

    (load-profile
      {:name "PProfile4"
       :base "PBase"
       :els  [{:id "el[x]:elComplexType.attr", :max "1"}]})


    (load-profile
      {:name "PTProfile"
       :base "PBase"
       :els  [{:id "elComplexType"}
              {:id "elComplexType.attr" :max "1"}]})

    (reload)

    (matcho/match
      (sut/get-definition aztx "url://PProfile1")
      {:| {:el     {:| {:prim        {:type "prim"}
                        :ComplexType nil}}
           :elPrim nil?}})

    (matcho/match
      (sut/get-definition aztx  "url://PProfile2")
      {:| {:el {:| {:ComplexType {:type "ComplexType"}
                    :prim        nil?}}}})

    (matcho/match
      (sut/get-definition aztx "url://PProfile3")
      {:| {:el {:| {:ComplexType {:type "ComplexType"}
                    :prim        {:type "prim"}}}}})

    (matcho/match
      (sut/get-definition aztx "url://PProfile4")
      {:| {:el {:| {:ComplexType {:type "ComplexType"
                                  :|    {:attr {:vector true
                                                :type   "prim"}}}}}}})

    (matcho/match
      (sut/get-definition aztx "url://PTProfile")
      {:| {:el {:| {:ComplexType {:type "ComplexType"
                                  :|    {:attr {:vector true :type "prim"}}}}}}}))

  (t/testing "Dependency escalation"

    (load-primitive-type
      {:name "ComplexType"
       :els  [{:id "attr" :min 0 :max "*" :type [{:code "prim"}]}]})

    (load-primitive-type
      {:name "Reference"
       :els  [{:id "attr" :min 0 :max "*" :type [{:code "prim"}]}]})

    (load-profile
      {:name "BaseResource2"
       :base "DomainResource"
       :els  [{:id  "complexattr" :type [{:code "ComplexType"}]}
              {:id  "complexattr.attr" :type    [{:code "prim"}] :binding {:strength "required", :valueSet "url://valueset"}}
              {:id  "complexattr.attr2" :type    [{:code "prim"}] :binding {:strength "required", :valueSet "url://valueset|ver1"}}
              {:id  "ref" :type [{:code "Reference" :targetProfile ["url://SomeResource"]}]}
              {:id  "extension", :type [{:code "Extension"}] :slicing {:discriminator [{:type "value", :path "url"}]}}
              {:id  "extension:some-ext" :type   [{:code    "Extension" :profile ["url://some-ext"]}] :sliceName "some-ext"}
              {:id  "polyattr[x]" :type  [{:code "prim"} {:code "string"}] :binding {:strength "required", :valueSet "url://valueset"}}]})

    (reload)

    (sut/get-definition aztx "url://BaseResource2")
    (sut/get-base-elements aztx :key {:type "ComplexType"} [])

    (matcho/match
      (sut/get-definition aztx "url://BaseResource2")
      {:deps {"ValueSet"
              {"url://valueset" {nil    [[:complexattr :attr :binding] [:polyattr :binding]]
                                 "ver1" [[:complexattr :attr2 :binding]]}}

              "StructureDefinition"
              {"http://hl7.org/fhir/StructureDefinition/ComplexType" [[:complexattr :type]]
               "http://hl7.org/fhir/StructureDefinition/Reference"   [[:ref :type]]
               "http://hl7.org/fhir/StructureDefinition/prim"        [[:complexattr :attr :type] [:complexattr :attr2 :type] [:polyattr :prim :type]]
               "http://hl7.org/fhir/StructureDefinition/string"      [[:polyattr :string :type]]

               "url://some-ext" [[:some-ext :fhir/extension]]

               "url://SomeResource" [[:ref :profiles]]

               "url://DomainResource" [[:baseDefinition]]}}}))


  (load-extension
    {:name "us-race"
     :els  [{:id        "extension:ombCategory",
             :type      [{:code "Extension"}],
             :sliceName "ombCategory",
             :min       0, :max "5"}
            {:id       "extension:ombCategory.url",
             :type     [{:code "uri"}],
             :min      1, :max "1",
             :fixedUri "ombCategory"}
            {:id      "extension:ombCategory.valueCoding",
             :type    [{:code "Coding"}],
             :min     1, :max "1",
             :binding {:strength "required",
                       :valueSet "http://hl7.org/fhir/us/core/ValueSet/omb-race-category"}}
            {:id        "extension:detailed",
             :type      [{:code "Extension"}],
             :sliceName "detailed",
             :min       0, :max "*"}
            {:id       "extension:detailed.url",
             :type     [{:code "uri"}],
             :min      1, :max "1",
             :fixedUri "detailed"}
            {:id      "extension:detailed.valueCoding",
             :type    [{:code "Coding"}],
             :min     1, :max "1",
             :binding {:strength "required",
                       :valueSet "http://hl7.org/fhir/us/core/ValueSet/detailed-race"}}
            {:id        "extension:text",
             :type      [{:code "Extension"}],
             :sliceName "text",
             :min       1, :max "1"}
            {:id       "extension:text.url",
             :type     [{:code "uri"}],
             :min      1, :max "1",
             :fixedUri "text"}
            {:id   "extension:text.valueString",
             :type [{:code "string"}],
             :min  1, :max "1"}
            {:id       "url",
             :min      1, :max "1",
             :fixedUri "http://hl7.org/fhir/us/core/StructureDefinition/us-core-race"}
            {:id "value[x]", :min 0, :max "0"}]})

  (load-extension
    {:name "pt-nation"
     :els  [{:id "Extension", :min 0, :max "*"}
            {:id "extension:code", :min 0, :max "1", :sliceName "code", :type [{:code "Extension"}]}
            {:id "extension:code.extension", :max "0"}
            {:id "extension:code.url", :fixedUri "code", :type [{:code "uri"}]}
            {:id "extension:code.value[x]", :min 1, :type [{:code "CodeableConcept"}]}
            {:id "extension:period", :min 1, :max "1", :sliceName "period", :type [{:code "Extension"}]}
            {:id "extension:period.extension", :max "0"}
            {:id "extension:period.url", :fixedUri "period", :type [{:code "uri"}]}
            {:id "extension:period.value[x]", :min 1, :type [{:code "Period"}]}
            {:id "url", :fixedUri "http://hl7.org/fhir/StructureDefinition/patient-nationality"}
            {:id "value[x]", :min 0, :max "0"}]})


  (load-extension
    {:name "due-to"
     :els
     [{:id "extension", :max "0"}
      {:id "url", :fixedUri "http://hl7.org/fhir/StructureDefinition/condition-dueTo"}
      {:id   "value[x]", :min 1,
       :type [{:code "CodeableConcept"}
              {:code "Reference",}]}]})

  (load-extension
    {:name "simple-ext"
     :els
     [{:id "url", :fixedUri "http://hl7.org/fhir/StructureDefinition/structuredefinition-xml-type"}
      {:id "valueString", :type [{:code "string"}]}]})

  (reload)

  (matcho/match
    (sut/get-definition aztx "uri://us-race")
    {:kind           "complex-type"
     :derivation     "constraint"
     :fhir/extension string?
     :type           "Extension"
     :|              {:ombCategory
                      {:type     "Coding"
                       :binding  {:strength "required"
                                  :valueSet {:url "http://hl7.org/fhir/us/core/ValueSet/omb-race-category"}}
                       :vector   true
                       :maxItems 5}
                      :detailed {:type    "Coding"
                                 :binding {:strength "required"
                                           :valueSet {:url "http://hl7.org/fhir/us/core/ValueSet/detailed-race"}}
                                 :vector  true}
                      :text     {:type     "string"
                                 :required true}}})

  (matcho/match
    (sut/get-definition aztx "uri://pt-nation")
    {:kind           "complex-type"
     :derivation     "constraint"
     :type           "Extension"
     :fhir/extension string?
     :|              {:code   {:required    nil?
                               :polymorphic nil?
                               :types       nil?
                               :type        "CodeableConcept"
                               :maxItems    1}
                      :period {:required true
                               :type     "Period"}}})

  (matcho/match
    (sut/get-definition aztx "uri://due-to")
    {:derivation     "constraint"
     :kind           "complex-type"
     :url            "uri://due-to"
     :type           "Extension"
     :baseDefinition nil?
     :fhir/extension string?
     :polymorphic    true
     :types          #{"CodeableConcept" "Reference"}
     :fhir-poly-keys nil? ;; will be determined while mount
     :|              {:CodeableConcept {:type "CodeableConcept"} :Reference {:type "Reference"}}})

  (matcho/match
    (sut/get-definition aztx "uri://simple-ext")
    {:derivation     "constraint"
     :kind           "first-class-extension"
     :url            "uri://simple-ext"
     :fhir/extension string?
     :type           "string"}))


(t/deftest deps
  (matcho/match
    (sut/collect-deps
      '{:fhir/extension
        "http://hl7.org/fhir/us/core/StructureDefinition/us-core-birthsex",
        :binding
        {:strength    "required",
         :description "Code for sex assigned at birth",
         :valueSet    {:url "http://hl7.org/fhir/us/core/ValueSet/birthsex"}},
        :url            "http://hl7.org/fhir/us/core/StructureDefinition/us-core-birthsex",
        :baseDefinition "http://hl7.org/fhir/StructureDefinition/code"})
    {"StructureDefinition"
     {"http://hl7.org/fhir/StructureDefinition/code" [[:baseDefinition]]}
     "ValueSet"
     {"http://hl7.org/fhir/us/core/ValueSet/birthsex" {nil [[:binding]]}}}))
