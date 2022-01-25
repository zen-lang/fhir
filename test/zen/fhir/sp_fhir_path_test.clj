(ns zen.fhir.sp-fhir-path-test
  (:require [zen.fhir.sp-fhir-path :as sut]
            [clojure.test :as t]
            [zen.core]
            [clojure.pprint]
            [matcho.core :as matcho]))


#_"TODO: test these paths parsing:
Patient.active
Patient.address.city | Person.address.city
Patient.deceased.exists() and Patient.deceased != false
Patient.name.where(use='nickname')
Organization.extension.where(url='http://hl7.org/fhir/us/davinci-pdex-plan-net/StructureDefinition/location-reference')
Person.link.target.where(resolve() is Patient)
PlanDefinition.relatedArtifact.where(type='composed-of').resource
Substance.code | (Substance.ingredient.substance as CodeableConcept)
(ConceptMap.source as uri)
(Group.characteristic.value as CodeableConcept) | (Group.characteristic.value as boolean)
QuestionnaireResponse.item.where(hasExtension('http://hl7.org/fhir/StructureDefinition/questionnaireresponse-isSubject')).answer.value.ofType(Reference)
Patient.extension.where(url = 'http://hl7.org/fhir/us/core/StructureDefinition/us-core-race').extension.value.code
ExplanationOfBenefit.billablePeriod.start | Patient.a | ExplanationOfBenefit.item.servicedDate | ExplanationOfBenefit.item.servicedPeriod.start
name | alias
Bundle.entry[0].resource
"


(t/deftest test-sp-parsing
  (t/is (= {"MedicationRequest" [["medication" "Reference"] ["medication" "string"]]}
           (sut/parse-expression "(MedicationRequest.medication as Reference) | (MedicationRequest.medication as string) ")))

  (t/is (= {"Person" [["link" "target" {:resourceType "Patient"}]]}
           (sut/parse-expression "Person.link.target.where(resolve() is Patient)")))

  (t/is (= {"Person" [["link" "target" {:sys "ups"}]]}
           (sut/parse-expression "Person.link.target.where(sys='ups')")))

  (t/is (= {"Medication" [["medication" "Reference"]]}
           (sut/parse-expression "Medication.medication.as(Reference)")))

  (t/is (= {"Observation" [["value" "CodeableConcept"]]}
           (sut/parse-expression "(Observation.value as CodeableConcept)")))

  (t/is (= {:default [["name"] ["alias"]]}
           (sut/parse-expression "name | alias")))


  (t/is (= {"Patient" [["active"]]}
           (sut/parse-expression "Patient.active")))

  (t/is (= {"Patient" [["address" "city"]]
            "Person" [["address" "city"]]}
           (sut/parse-expression "Patient.address.city | Person.address.city")))

  (t/is (= {"Patient" [["deceased"]]}
           (sut/parse-expression "Patient.deceased.exists() and Patient.deceased != false")))

  (t/is (= {"Patient" [["name" {:use "nickname"}]]}
           (sut/parse-expression "Patient.name.where(use='nickname')")))

  (t/is (= {"Person" [["link" "target" {:resourceType "Patient"}]]}
           (sut/parse-expression "Person.link.target.where(resolve() is Patient)")))

  (t/is (= {"PlanDefinition" [["relatedArtifact" {:type "composed-of"} "resource"]]}
           (sut/parse-expression "PlanDefinition.relatedArtifact.where(type='composed-of').resource")))

  (t/is (= {"Substance" [["code"]
                         ["ingredient" "substance" "CodeableConcept"]]}
           (sut/parse-expression "Substance.code | (Substance.ingredient.substance as CodeableConcept)")))

  (t/is (= {"ConceptMap" [["source" "uri"]]}
           (sut/parse-expression "(ConceptMap.source as uri)")))

  (t/is (= {"Group" [["characteristic" "value" "CodeableConcept"]
                     ["characteristic" "value" "boolean"]]}
           (sut/parse-expression "(Group.characteristic.value as CodeableConcept) | (Group.characteristic.value as boolean)")))

  (t/is (= {"Condition" [["onset" "dateTime"]
                         ["onset" "Period"]]}
           (sut/parse-expression "Condition.onset.as(dateTime) | Condition.onset.as(Period)")))

  (t/is (= {"Observation" [["value" "dateTime"]
                           ["value" "Period"]]}
           (sut/parse-expression "(Observation.value as dateTime) | (Observation.value as Period)")))

  (t/is (= {"ExplanationOfBenefit" [["billablePeriod" "start"]
                                    ["item" "servicedDate"]
                                    ["item" "servicedPeriod" "start"]]}
           (sut/parse-expression "ExplanationOfBenefit.billablePeriod.start | ExplanationOfBenefit.item.servicedDate | ExplanationOfBenefit.item.servicedPeriod.start")))

  (t/is (= {:default [["name"] ["alias"]]}
           (sut/parse-expression "name | alias")))

  (t/is (= #_{"Organization" [["extension" {:url "http://hl7.org/fhir/us/davinci-pdex-plan-net/StructureDefinition/location-reference"}]]}
           nil
           (sut/parse-expression "Organization.extension.where(url='http://hl7.org/fhir/us/davinci-pdex-plan-net/StructureDefinition/location-reference')")))


  (t/is (= #_{"Patient" [["extension" {:url "http://hl7.org/fhir/us/core/StructureDefinition/us-core-race"}
                          "extension"
                          "value"
                          "code"]]}
           nil
           (sut/parse-expression "Patient.extension.where(url = 'http://hl7.org/fhir/us/core/StructureDefinition/us-core-race').extension.value.code")))

  (t/is (= #_{"QuestionnaireResponse" [["item"
                                        {:extension [{:url "http://hl7.org/fhir/StructureDefinition/questionnaireresponse-isSubject"}]}
                                        "answer"
                                        "value"
                                        "Reference"]]}
           nil
           (sut/parse-expression "QuestionnaireResponse.item.where(hasExtension('http://hl7.org/fhir/StructureDefinition/questionnaireresponse-isSubject')).answer.value.ofType(Reference)")))


  (t/is (= #_{"Bundle" [["entry" 0 "resource"]]}
           nil
           (sut/parse-expression "Bundle.entry[0].resource"))))
