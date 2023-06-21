(ns zen.fhir.search-parameter.loader-test
  (:require [zen.fhir.search-parameter.loader :as sut]
            [clojure.test :as t]
            [matcho.core :as matcho]))

(def raw-sp
  {:resourceType "SearchParameter"
   :id "individual-given"
   :url "http://hl7.org/fhir/SearchParameter/individual-given"
   :version "4.0.1"
   :name "given"
   :status "draft"
   :experimental false
   :date "2019-11-01T09:29:23+11:00"
   :code "given"
   :base ["Patient" "Practitioner"]
   :type "string"
   :expression "Patient.name.given | Practitioner.name.given | name.abc"})

(def zen-fhir-meta
  {:zen.fhir/package-ns "hl7-fhir-r4-core"})

(def read-sp (merge raw-sp zen-fhir-meta))

(def loaded-sp {:zen.fhir/package-ns "hl7-fhir-r4-core",
                :date "2019-11-01T09:29:23+11:00",
                :expression "Patient.name.given | Practitioner.name.given | name.abc",
                :name "given",
                :type "string",
                :experimental false,
                :resourceType "SearchParameter",
                :base-resource-types ["Patient" "Practitioner"],
                :zen.fhir/schema-ns 'hl7-fhir-r4-core.search.individual-given,
                :status "draft",
                :id "individual-given",
                :url "http://hl7.org/fhir/SearchParameter/individual-given",
                :code "given",
                :base ["Patient" "Practitioner"],
                :version "4.0.1",
                :sp-name "given"})

(def loaded-composite-sp {:zen.fhir/package-ns 'fhir-r5,
                          :expression "Observation | Observation.component",
                          :multipleOr false,
                          :name "combo-code-value-quantity",
                          :type "composite",
                          :resourceType "SearchParameter",
                          :component
                          [{:definition "http://hl7.org/fhir/SearchParameter/Observation-combo-code",
                            :expression "code"}
                           {:definition
                            "http://hl7.org/fhir/SearchParameter/Observation-combo-value-quantity",
                            :expression "value.ofType(Quantity)"}],
                          :base-resource-types ["Observation"],
                          :extension
                          [{:url
                            "http://hl7.org/fhir/StructureDefinition/structuredefinition-standards-status",
                            :valueCode "normative"}],
                          :zen.fhir/schema-ns 'fhir-r5.search.Observation-combo-code-value-quantity,
                          :status "active",
                          :id "Observation-combo-code-value-quantity",
                          :processingMode "normal",
                          :url
                          "http://hl7.org/fhir/SearchParameter/Observation-combo-code-value-quantity",
                          :code "combo-code-value-quantity",
                          :base ["Observation"],
                          :version "5.0.0",
                          :_source "zen.fhir",
                          :sp-name "combo-code-value-quantity"})


(def structure-definition-ir-fixture
  {"http://hl7.org/fhir/StructureDefinition/Patient"
   {:| {:name {:| {:given {:type "string"}
                   :abc {:type "string"}}}}}
   "http://hl7.org/fhir/StructureDefinition/Practitioner"
   {:| {:name {:| {:given {:type "string"}
                   :abc {:type "string"}}}}}
   "http://hl7.org/fhir/StructureDefinition/Observation"
   {:| {:code {:type "CodeableConcept"}
        :value {:| {:Quantity {:type "Quantity"}}}}}})

(def ztx
  (atom {:fhir/inter {"StructureDefinition" structure-definition-ir-fixture
                      "SearchParameter"
                      {"http://hl7.org/fhir/SearchParameter/individual-given" loaded-sp
                       "http://hl7.org/fhir/SearchParameter/Observation-combo-code-value-quantity" loaded-composite-sp}}}))

(t/deftest process-on-load-test
  (t/is (= loaded-sp
           (sut/process-on-load read-sp))))

(t/deftest process-search-parameter-test
  (matcho/match (sut/process-search-parameter ztx loaded-sp)
                {:zen.fhir/package-ns "hl7-fhir-r4-core"
                 :date "2019-11-01T09:29:23+11:00"
                 :name "given"
                 :type "string"
                 :experimental false
                 :resourceType "SearchParameter"
                 :base-resource-types ["Patient" "Practitioner"]
                 :zen.fhir/schema-ns 'hl7-fhir-r4-core.search.individual-given
                 :expr
                 {:Patient
                  {:knife [["name" "given"] ["name" "abc"]]
                   :data-types #{{:type "string" :polymorphic? false}}
                   :template :string}
                  :Practitioner
                  {:knife [["name" "given"] ["name" "abc"]]
                   :data-types #{{:type "string" :polymorphic? false}}
                   :template :string}}
                 :status "draft"
                 :id "individual-given"
                 :url "http://hl7.org/fhir/SearchParameter/individual-given"
                 :code "given"
                 :base ["Patient" "Practitioner"]
                 :version "4.0.1"
                 :sp-name "given"}))

(t/deftest process-composite-search-parameter-test
  (matcho/match (sut/process-search-parameter ztx user/composite-sp)
                {:zen.fhir/package-ns 'fhir-r5,
                 :multipleOr false,
                 :name "combo-code-value-quantity",
                 :type "composite",
                 :resourceType "SearchParameter",
                 :component
                 [{:definition "http://hl7.org/fhir/SearchParameter/Observation-combo-code",
                   :expression "code"}
                  {:definition
                   "http://hl7.org/fhir/SearchParameter/Observation-combo-value-quantity"
                   :expression "value.ofType(Quantity)"}],
                 :base-resource-types ["Observation"],
                 :extension
                 [{:url
                   "http://hl7.org/fhir/StructureDefinition/structuredefinition-standards-status",
                   :valueCode "normative"}],
                 :zen.fhir/schema-ns 'fhir-r5.search.Observation-combo-code-value-quantity,
                 :expr
                 {"Observation"
                  {:knife
                   [[["code"]
                     ["value" "Quantity"]]
                    [["component" "code"]
                     ["component" "value" "Quantity"]]],
                   :jsonpath
                   [["$.\"code\"[*]" "$.\"value\".\"Quantity\"[*]"]
                    ["$.\"component\".\"code\"[*]" "$.\"component\".\"value\".\"Quantity\"[*]"]]
                   :data-types
                   #{{:type "Quantity", :polymorphic? false}
                     {:type "CodeableConcept", :polymorphic? false}}
                   :template :composite}},
                 :status "active",
                 :id "Observation-combo-code-value-quantity",
                 :processingMode "normal",
                 :url
                 "http://hl7.org/fhir/SearchParameter/Observation-combo-code-value-quantity",
                 :code "combo-code-value-quantity",
                 :base ["Observation"],
                 :version "5.0.0",
                 :_source "zen.fhir",
                 :sp-name "combo-code-value-quantity"}))
