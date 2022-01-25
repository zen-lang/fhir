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
Bundle.entry[0].resource"


(t/deftest test-sp-parsing
  (matcho/match
    (sut/parse-expression "(MedicationRequest.medication as Reference) | (MedicationRequest.medication as string) ")
    {"MedicationRequest" [["medication" "Reference"] ["medication" "string"]]})

  (matcho/match
    (sut/parse-expression "Person.link.target.where(resolve() is Patient)")
    {"Person" [["link" "target" {:resourceType "Patient"}]]})

  (matcho/match
    (sut/parse-expression "Person.link.target.where(sys='ups')")
    {"Person" [["link" "target" {:sys "ups"}]]})

  (matcho/match
    (sut/parse-expression "Medication.medication.as(Reference)")
    {"Medication" [["medication" "Reference"]]})

  (matcho/match
    (sut/parse-expression "(Observation.value as CodeableConcept)")
    {"Observation" [["value" "CodeableConcept"]]})

  (matcho/match
    (sut/parse-expression "name | alias")
    {:default [["name"] ["alias"]]}))
