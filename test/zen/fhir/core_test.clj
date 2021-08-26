(ns zen.fhir.core-test
  (:require [zen.fhir.core :as sut]
            [clojure.test :as t]
            [zen.core]
            [clojure.pprint]
            [matcho.core :as matcho]
            [clojure.java.io :as io]))


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

    (matcho/match (-> {:id "a.d", :min 1, :max "1", :base {:max "*"}}
                      sut/normalize-element)
                  {:required true?, :vector true?})

    (matcho/match (-> {:id "a.d", :min 1, :max "1", :base {:max "*"}}
                      sut/normalize-element)
                  {:required true?, :vector true?})

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

    (matcho/match (-> {:id "a.b.f", :max "0", :base {:max "*"}}
                      sut/normalize-element)
                  {:required not, :vector true?, :maxItems 0})

    (matcho/match (-> {:id "a.b.f", :max "0", :base {:max "1"}}
                      sut/normalize-element)
                  {:required not, :vector not, :prohibited true?})))


(t/deftest fhir-aidbox-poly-keys-mapping
  (def ztx (zen.core/new-context {}))
  (sut/load-all ztx "hl7.fhir.r4.core")

  (def observation (get-in @ztx [:fhir "StructureDefinition" "http://hl7.org/fhir/StructureDefinition/Observation"]))

  (matcho/match
    (:elements observation)
    {:baseDefinition "http://hl7.org/fhir/StructureDefinition/DomainResource"
     :kind           "resource",
     :type           "Observation"
     :derivation     "specialization",
     :fhir-poly-keys {:valueQuantity {:key :value, :type "Quantity"}
                      :valueBoolean  {:key :value, :type "boolean"}}
     :els {:value {:els {:boolean {:type "boolean"}
                         :Quantity {:type "Quantity"}}}
           :component {:fhir-poly-keys {:valueQuantity {:key :value, :type "Quantity"}
                                        :valueBoolean  {:key :value, :type "boolean"}}
                       :els {:value {:els {:boolean {:type "boolean"}
                                           :Quantity {:type "Quantity"}}}}}}}))

(t/deftest test-zen-transformation
  (def ztx (zen.core/new-context {}))
  (sut/load-all ztx "hl7.fhir.r4.core")

  (def pres (get-in @ztx [:fhir "StructureDefinition"
                          "http://hl7.org/fhir/StructureDefinition/Patient"]))




  (spit "/tmp/pres.edn" (with-out-str (clojure.pprint/pprint (:elements pres))))

  (matcho/match
    (:elements pres)
    {:id             "Patient"
     :kind           "resource"
     :derivation     "specialization",
     :baseDefinition "http://hl7.org/fhir/StructureDefinition/DomainResource"

     ;; :deps {:valuesets {}
     ;;        :types {}
     ;;        :extensions {}
     ;;        :profiles {}}

     :short          "Information about an individual or animal receiving health care services"
     :els            {:address             {:short     "An address for the individual"
                                            :type      "Address"
                                            :vector    true
                                            :id        "Patient.address"
                                            :isSummary true}
                      :multipleBirth
                      {:els         {:boolean {:type "boolean"}
                                     :integer {:type "integer"}}
                       :types       #{"boolean" "integer"}
                       :polymorphic true}
                      :link                {:type   "BackboneElement"
                                            :vector true
                                            :els    {:other {:type      "Reference"
                                                             :required  true
                                                             :isSummary true}
                                                     :type  {:path      "Patient.link.type"
                                                             :binding   {:strength "required"
                                                                         :valueSet "http://hl7.org/fhir/ValueSet/link-type|4.0.1"}
                                                             :required  true
                                                             :isSummary true}}}
                      :generalPractitioner {:vector   true
                                            :type     "Reference"
                                            :profiles #{"http://hl7.org/fhir/StructureDefinition/Organization"
                                                        "http://hl7.org/fhir/StructureDefinition/Practitioner"
                                                        "http://hl7.org/fhir/StructureDefinition/PractitionerRole"}}}})


  (def ares (get-in @ztx [:fhir "StructureDefinition"
                          "http://hl7.org/fhir/StructureDefinition/Address"]))

  (spit "/tmp/ares.edn" (with-out-str (clojure.pprint/pprint (:elements ares))))

  (matcho/match
    (:elements ares)
    {})


  (def poly-prof-res (get-in @ztx [:fhir "StructureDefinition" "http://hl7.org/fhir/us/core/StructureDefinition/pediatric-bmi-for-age"]))

  (get-in @ztx [:fhir "StructureDefinition"
                "http://hl7.org/fhir/StructureDefinition/Observation" :elements])


  (:src poly-prof-res)

  (matcho/match
    (:elements poly-prof-res)

    {:baseDefinition "http://hl7.org/fhir/StructureDefinition/vitalsigns"
     :kind           "resource",
     :type           "Observation"
     :derivation     "constraint",
     :els {:value
           {:polymorphic true
            :els {:Quantity
                  {:els
                   {:value {:required true}}}}}}}
    )

  (spit "/tmp/poly.edn" (with-out-str (clojure.pprint/pprint (:elements poly-prof-res))))


  (def qres (get-in @ztx [:fhir "StructureDefinition"
                          "http://hl7.org/fhir/StructureDefinition/Questionnaire"
                          :elements]))



  (spit "/tmp/qres.edn" (with-out-str (clojure.pprint/pprint qres)))

  (matcho/match
    qres
    {:els
     {:description     {:id "Questionnaire.description"},
      :subjectType     {:id "Questionnaire.subjectType"},
      :date            {:id "Questionnaire.date"},
      :publisher       {:id "Questionnaire.publisher"},
      :approvalDate    {:id "Questionnaire.approvalDate"},
      :jurisdiction    {:id "Questionnaire.jurisdiction"},
      :derivedFrom     {:id "Questionnaire.derivedFrom"},
      :purpose         {:id "Questionnaire.purpose"},
      :name            {:id "Questionnaire.name"},
      :item
      {:id "Questionnaire.item",
       :els
       {:enableBehavior {:id "Questionnaire.item.enableBehavior"},
        :definition     {:id "Questionnaire.item.definition"},
        :linkId         {:id "Questionnaire.item.linkId"},
        :repeats        {:id "Questionnaire.item.repeats"},
        :item           {:id "Questionnaire.item.item"},
        :type           {:id "Questionnaire.item.type"},
        :enableWhen
        {:id "Questionnaire.item.enableWhen",
         :els
         {:question {:id "Questionnaire.item.enableWhen.question"},
          :operator {:id "Questionnaire.item.enableWhen.operator"},
          :answer   {:id "Questionnaire.item.enableWhen.answer[x]"}}},
        :answerOption
        {:id "Questionnaire.item.answerOption",
         :els
         {:value {:id          "Questionnaire.item.answerOption.value[x]"
                  :polymorphic true
                  :els         {:string {}}}
          :initialSelected
          {:id "Questionnaire.item.answerOption.initialSelected"}}},
        :prefix         {:id "Questionnaire.item.prefix"},
        :readOnly       {:id "Questionnaire.item.readOnly"},
        :answerValueSet {:id "Questionnaire.item.answerValueSet"},
        :code           {:id "Questionnaire.item.code"},
        :initial
        {:id  "Questionnaire.item.initial",
         :els {:value {:id "Questionnaire.item.initial.value[x]"}}},
        :maxLength      {:id "Questionnaire.item.maxLength"},
        :required       {:id "Questionnaire.item.required"},
        :text           {:id "Questionnaire.item.text"}}},
      :useContext      {:id "Questionnaire.useContext"},
      :copyright       {:id "Questionnaire.copyright"},
      :experimental    {:id "Questionnaire.experimental"},
      :title           {:id "Questionnaire.title"},
      :status          {:id "Questionnaire.status"},
      :url             {:id "Questionnaire.url"},
      :code            {:id "Questionnaire.code"},
      :identifier      {:id "Questionnaire.identifier"},
      :lastReviewDate  {:id "Questionnaire.lastReviewDate"},
      :version         {:id "Questionnaire.version"},
      :contact         {:id "Questionnaire.contact"},
      :effectivePeriod {:id "Questionnaire.effectivePeriod"}},})


  ;; (get-in @ztx [:fhir "StructureDefinition" "Questionnaire"])

  ;; (sut/process-sd quest)

  ;; min/max => vector? required minItems/maxItems
  ;; type -> polymorphic, type, profiles
  ;; references to extensions etc
  ;; group and analyze slicing
  ;; analyze valuesets


  ;; packeg (deps) => index


  ;; {
  ;;  <rt> <url> {
  ;;              :lookup-idx {}
  ;;              :fhir       {}
  ;;              :elements   {}
  ;;              :transforms {}
  ;;              }
  ;;  }

  ;; => ????

  ;; -> rich -> generate
  ;;         -> assumptins check

  ;;may be no vector as a first step
  #_(matcho/match
      (sut/group-elements ztx quest)

      ;; (sut/load-structure-definition ztx quest)

      {
       :original      {}
       :tree-elements {}
       :elements      {}
       :deps          {:base       {}
                       :extensions {}
                       :valuesets  {}}

       }

      {:elements
       {:description {:type [{:code "markdown"}]},
        :subjectType {:vector true
                      :type   [{:code "code"}]},
        :derivedFrom {:vector true
                      :type   [{:code "canonical"}]},
        :name        {:type [{:code "string"}]},
        :code        {:vector true
                      :type   [{:code "Coding"}]},
        :item        {:vector true
                      :elements
                      {:enableBehavior {:type [{:code "code"}]},
                       :definition     {:type [{:code "uri"}]},
                       :item           {:vector true
                                        :recur  [:item]},
                       :enableWhen
                       {:vector true
                        :elements
                        {:question {:type [{:code "string"}]},
                         :operator {:type [{:code "code"}]},
                         :answer   {:polymorphic true
                                    :elements    {:boolean   {}
                                                  :decimal   {}
                                                  :integer   {}
                                                  :date      {}
                                                  :dateTime  {}
                                                  :time      {}
                                                  :string    {:type [{:code "string"}]}
                                                  :Coding    {}
                                                  :Quantity  {}
                                                  :Reference {:type [{:code "Reference", :targetProfile ["http://hl7.org/fhir/StructureDefinition/Resource"]}]}}}}}}}}})





  )
