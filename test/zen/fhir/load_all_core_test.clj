(ns zen.fhir.load-all-core-test
  (:require [zen.fhir.core :as sut]
            [clojure.test :as t]
            [zen.core]
            [clojure.java.io :as io]
            [matcho.core :as matcho]))


(defonce used-urls (atom {}))
(reset! used-urls {})


(defmacro match-inter [ztx rt url pattern]
  `(let [res# (get-in @~ztx [:fhir/inter ~rt ~url])]
     (swap! used-urls update-in [~rt ~url] (fnil inc 0))
     (matcho/match res# ~pattern)
     res#))


(defonce ztx (zen.core/new-context {}))


(defn collect-deps-recursively
  ([deps]
   (collect-deps-recursively deps (atom #{})))
  ([deps processed-urls]
   (let [deps-acc (into {} (map (fn [[rt urls]] [rt (set (remove nil? (keys urls)))])) deps)
         urls     (set (for [[rt urls] deps
                             [url _]   urls
                             :when     (some? url)]
                         {:rt rt, :url url}))]
     (transduce
       (map (fn [{:keys [rt url] :as u}]
              (when-not (contains? @processed-urls u)
                (swap! processed-urls conj u)
                (collect-deps-recursively (get-in @ztx [:fhir/inter rt url :deps])
                                          processed-urls))))
       (partial merge-with into)
       deps-acc
       urls))))


(def deps
  #_(collect-deps-recursively @used-urls)
  {"StructureDefinition"
   #{"http://hl7.org/fhir/StructureDefinition/Group"
     "http://hl7.org/fhir/StructureDefinition/Age"
     "http://hl7.org/fhir/StructureDefinition/string"
     "http://hl7.org/fhir/StructureDefinition/Questionnaire"
     "http://hl7.org/fhir/StructureDefinition/markdown"
     "http://hl7.org/fhir/StructureDefinition/Provenance"
     "http://hl7.org/fhir/StructureDefinition/date"
     "http://hl7.org/fhir/StructureDefinition/Goal"
     "http://hl7.org/fhir/StructureDefinition/ProductShelfLife"
     "http://hl7.org/fhir/StructureDefinition/AppointmentResponse"
     "http://hl7.org/fhir/StructureDefinition/ResearchStudy"
     "http://hl7.org/fhir/StructureDefinition/Duration"
     "http://hl7.org/fhir/StructureDefinition/Dosage"
     "http://hl7.org/fhir/StructureDefinition/Encounter"
     "http://hl7.org/fhir/StructureDefinition/ClinicalImpression"
     "http://hl7.org/fhir/StructureDefinition/url"
     "http://hl7.org/fhir/StructureDefinition/DiagnosticReport"
     "http://hl7.org/fhir/StructureDefinition/Substance"
     "http://hl7.org/fhir/StructureDefinition/Slot"
     "http://hl7.org/fhir/StructureDefinition/DeviceMetric"
     "http://hl7.org/fhir/StructureDefinition/DomainResource"
     "http://hl7.org/fhir/StructureDefinition/Endpoint"
     "http://hl7.org/fhir/StructureDefinition/RelatedPerson"
     "http://hl7.org/fhir/StructureDefinition/ImagingStudy"
     "http://hl7.org/fhir/StructureDefinition/Practitioner"
     "http://hl7.org/fhir/StructureDefinition/SampledData"
     "http://hl7.org/fhir/us/core/StructureDefinition/us-core-patient"
     "http://hl7.org/fhir/StructureDefinition/ProdCharacteristic"
     "http://hl7.org/fhir/StructureDefinition/CommunicationRequest"
     "http://hl7.org/fhir/StructureDefinition/integer"
     "http://hl7.org/fhir/StructureDefinition/base64Binary"
     "http://hl7.org/fhir/StructureDefinition/Contract"
     "http://hl7.org/fhir/StructureDefinition/Media"
     "http://hl7.org/fhir/StructureDefinition/DeviceDefinition"
     "http://hl7.org/fhir/StructureDefinition/PlanDefinition"
     "http://hl7.org/fhir/StructureDefinition/Extension"
     "http://hl7.org/fhir/StructureDefinition/Location"
     "http://hl7.org/fhir/StructureDefinition/Ratio"
     "http://hl7.org/fhir/StructureDefinition/ImmunizationRecommendation"
     "http://hl7.org/fhir/StructureDefinition/Count"
     "http://hl7.org/fhir/StructureDefinition/ParameterDefinition"
     "http://hl7.org/fhir/StructureDefinition/instant"
     "http://hl7.org/fhir/StructureDefinition/ContactDetail"
     "http://hl7.org/fhir/us/core/StructureDefinition/us-core-birthsex"
     "http://hl7.org/fhir/StructureDefinition/EpisodeOfCare"
     "http://hl7.org/fhir/StructureDefinition/Address"
     "http://hl7.org/fhir/StructureDefinition/Coding"
     "http://hl7.org/test-plannet/StructureDefinition/plannet-FromNetwork-extension"
     "http://hl7.org/fhir/StructureDefinition/Reference"
     "http://hl7.org/fhir/StructureDefinition/Procedure"
     "http://hl7.org/fhir/StructureDefinition/Period"
     "http://hl7.org/fhir/StructureDefinition/xhtml"
     "http://hl7.org/fhir/StructureDefinition/HumanName"
     "http://hl7.org/fhir/us/core/StructureDefinition/pediatric-bmi-for-age"
     "http://hl7.org/fhir/StructureDefinition/DeviceRequest"
     "http://hl7.org/fhir/StructureDefinition/Composition"
     "http://hl7.org/test-plannet/StructureDefinition/plannet-PractitionerRole"
     "http://hl7.org/fhir/StructureDefinition/RelatedArtifact"
     "http://hl7.org/fhir/StructureDefinition/Schedule"
     "http://hl7.org/fhir/StructureDefinition/Expression"
     "http://hl7.org/fhir/StructureDefinition/uuid"
     "http://hl7.org/fhir/StructureDefinition/id"
     "http://hl7.org/fhir/StructureDefinition/DetectedIssue"
     "http://hl7.org/fhir/StructureDefinition/unsignedInt"
     "http://hl7.org/fhir/StructureDefinition/RiskAssessment"
     "http://hl7.org/fhir/StructureDefinition/MolecularSequence"
     "http://hl7.org/fhir/StructureDefinition/Observation"
     "http://hl7.org/fhir/StructureDefinition/Signature"
     "http://hl7.org/fhir/us/core/StructureDefinition/us-core-race"
     "http://hl7.org/fhir/StructureDefinition/Coverage"
     "http://hl7.org/fhir/StructureDefinition/Resource"
     "http://hl7.org/fhir/StructureDefinition/NutritionOrder"
     "http://hl7.org/fhir/StructureDefinition/Device"
     "http://hl7.org/fhir/StructureDefinition/FamilyMemberHistory"
     "http://hl7.org/fhir/StructureDefinition/CareTeam"
     "http://hl7.org/fhir/StructureDefinition/Contributor"
     "http://hl7.org/fhir/StructureDefinition/HealthcareService"
     "http://hl7.org/fhir/StructureDefinition/ServiceRequest"
     "http://hl7.org/fhir/StructureDefinition/UsageContext"
     "http://hl7.org/fhir/StructureDefinition/VisionPrescription"
     "http://hl7.org/fhir/StructureDefinition/ClaimResponse"
     "http://hl7.org/fhir/StructureDefinition/canonical"
     "http://hl7.org/fhir/StructureDefinition/Meta"
     "http://hl7.org/test-plannet/StructureDefinition/plannet-Network"
     "http://hl7.org/fhir/StructureDefinition/CarePlan"
     "http://hl7.org/fhir/StructureDefinition/QuestionnaireResponse"
     "http://hl7.org/fhir/StructureDefinition/Patient"
     "http://hl7.org/fhir/StructureDefinition/code"
     "http://hl7.org/fhir/StructureDefinition/Condition"
     "http://hl7.org/fhir/StructureDefinition/Distance"
     "http://hl7.org/fhir/StructureDefinition/MedicationDispense"
     "http://hl7.org/fhir/StructureDefinition/Quantity"
     "http://hl7.org/fhir/StructureDefinition/MedicationAdministration"
     "http://hl7.org/fhir/StructureDefinition/oid"
     "http://hl7.org/fhir/StructureDefinition/Task"
     "http://hl7.org/fhir/StructureDefinition/"
     "http://hl7.org/fhir/StructureDefinition/Appointment"
     "http://hl7.org/fhir/us/core/StructureDefinition/us-core-ethnicity"
     "http://hl7.org/fhir/StructureDefinition/Medication"
     "http://hl7.org/fhir/StructureDefinition/ContactPoint"
     "http://hl7.org/fhir/StructureDefinition/Annotation"
     "http://hl7.org/fhir/StructureDefinition/Specimen"
     "http://hl7.org/fhir/StructureDefinition/vitalsigns"
     "http://hl7.org/fhir/StructureDefinition/Account"
     "http://hl7.org/fhir/StructureDefinition/MedicationStatement"
     "http://hl7.org/fhir/StructureDefinition/Attachment"
     "http://hl7.org/fhir/StructureDefinition/DocumentReference"
     "http://hl7.org/fhir/StructureDefinition/boolean"
     "http://hl7.org/fhir/StructureDefinition/PractitionerRole"
     "http://hl7.org/fhir/StructureDefinition/Element"
     "http://hl7.org/fhir/StructureDefinition/Narrative"
     "http://hl7.org/test-plannet/StructureDefinition/newpatients"
     "http://hl7.org/fhir/StructureDefinition/Organization"
     "http://hl7.org/fhir/StructureDefinition/AllergyIntolerance"
     "http://hl7.org/fhir/StructureDefinition/SimpleQuantity"
     "http://hl7.org/fhir/StructureDefinition/InsurancePlan"
     "http://hl7.org/fhir/StructureDefinition/time"
     "http://hl7.org/fhir/StructureDefinition/Claim"
     "http://hl7.org/fhir/StructureDefinition/Immunization"
     "http://hl7.org/fhir/StructureDefinition/TriggerDefinition"
     "http://hl7.org/fhir/StructureDefinition/Range"
     "http://hl7.org/fhir/StructureDefinition/BackboneElement"
     "http://hl7.org/fhir/StructureDefinition/CodeableConcept"
     "http://hl7.org/fhir/StructureDefinition/DataRequirement"
     "http://hl7.org/fhir/StructureDefinition/Money"
     "http://hl7.org/fhir/StructureDefinition/MedicationRequest"
     "http://hl7.org/fhir/StructureDefinition/Identifier"
     "http://hl7.org/fhir/StructureDefinition/dateTime"
     "http://hl7.org/fhir/StructureDefinition/RequestGroup"
     "http://hl7.org/fhir/StructureDefinition/uri"
     "http://hl7.org/fhir/StructureDefinition/decimal"
     "http://hl7.org/fhir/StructureDefinition/Timing"
     "http://hl7.org/fhir/StructureDefinition/positiveInt"
     "http://hl7.org/fhir/StructureDefinition/ImmunizationEvaluation"},
   "ValueSet"
   #{"http://hl7.org/fhir/ValueSet/note-type"
     "http://hl7.org/fhir/us/mcode/ValueSet/mcode-cancer-staging-system-vs"
     "http://hl7.org/fhir/ValueSet/questionnaire-answers-status"
     "http://hl7.org/fhir/ValueSet/medication-admin-category"
     "http://loinc.org/vs/LL379-9"
     "http://hl7.org/fhir/us/core/ValueSet/detailed-race"
     "http://hl7.org/fhir/ValueSet/medicationrequest-status"
     "http://hl7.org/fhir/us/core/ValueSet/omb-race-category"
     "http://hl7.org/fhir/ValueSet/encounter-location-status"
     "http://hl7.org/fhir/ValueSet/care-plan-intent"
     "http://hl7.org/fhir/ValueSet/questionnaire-enable-behavior"
     "http://hl7.org/fhir/ValueSet/list-mode"
     "http://hl7.org/fhir/ValueSet/care-team-status"
     "http://hl7.org/fhir/ValueSet/strand-type"
     "http://hl7.org/fhir/ValueSet/metric-calibration-type"
     "http://terminology.hl7.org/ValueSet/v2-0116"
     "http://hl7.org/fhir/ValueSet/list-order"
     "http://hl7.org/fhir/ValueSet/udi-entry-type"
     "http://hl7.org/fhir/ValueSet/contract-status"
     "http://hl7.org/fhir/ValueSet/questionnaire-enable-operator"
     "http://hl7.org/fhir/us/core/ValueSet/omb-ethnicity-category"
     "http://hl7.org/fhir/ValueSet/goal-priority"
     "http://hl7.org/fhir/ValueSet/currencies"
     "http://hl7.org/fhir/ValueSet/condition-clinical"
     "http://hl7.org/fhir/ValueSet/allergy-intolerance-category"
     "http://hl7.org/fhir/ValueSet/narrative-status"
     "http://hl7.org/fhir/ValueSet/vision-base-codes"
     "http://hl7.org/fhir/ValueSet/event-status"
     "http://hl7.org/fhir/ValueSet/sort-direction"
     "http://hl7.org/fhir/ValueSet/encounter-special-courtesy"
     "http://hl7.org/fhir/ValueSet/history-status"
     "http://hl7.org/fhir/ValueSet/imagingstudy-status"
     "http://hl7.org/fhir/ValueSet/c80-doc-typecodes"
     "http://hl7.org/fhir/ValueSet/orientation-type"
     "http://hl7.org/fhir/ValueSet/trigger-type"
     "http://terminology.hl7.org/ValueSet/v3-ConfidentialityClassification"
     "http://hl7.org/fhir/us/core/ValueSet/birthsex"
     "http://hl7.org/fhir/ValueSet/quality-type"
     "http://hl7.org/fhir/ValueSet/days-of-week"
     "http://hl7.org/fhir/ValueSet/formatcodes"
     "http://hl7.org/fhir/ValueSet/diagnosis-role"
     "http://hl7.org/fhir/ValueSet/action-participant-type"
     "http://hl7.org/fhir/ValueSet/sequence-type"
     "http://hl7.org/fhir/ValueSet/action-relationship-type"
     "http://hl7.org/fhir/ValueSet/action-precheck-behavior"
     "http://hl7.org/fhir/ValueSet/detectedissue-severity"
     "http://hl7.org/fhir/ValueSet/group-type"
     "http://hl7.org/fhir/ValueSet/composition-attestation-mode"
     "http://hl7.org/fhir/ValueSet/units-of-time"
     "http://hl7.org/fhir/ValueSet/mimetypes"
     "http://hl7.org/fhir/ValueSet/medicationrequest-intent"
     "http://hl7.org/fhir/ValueSet/name-use"
     "http://hl7.org/fhir/ValueSet/research-study-status"
     "http://hl7.org/fhir/ValueSet/document-reference-status"
     "http://hl7.org/fhir/ValueSet/allergy-intolerance-type"
     "http://hl7.org/fhir/ValueSet/slotstatus"
     "http://hl7.org/fhir/ValueSet/reaction-event-severity"
     "http://hl7.org/fhir/ValueSet/encounter-reason"
     "http://hl7.org/fhir/ValueSet/device-nametype"
     "http://hl7.org/fhir/ValueSet/account-status"
     "http://hl7.org/fhir/ValueSet/performer-role"
     "http://hl7.org/fhir/ValueSet/participantrequired"
     "http://hl7.org/fhir/ValueSet/task-intent"
     "http://hl7.org/fhir/ValueSet/medicationdispense-status"
     "http://hl7.org/fhir/ValueSet/timing-abbreviation"
     "http://hl7.org/fhir/ValueSet/metric-color"
     "http://hl7.org/fhir/ValueSet/relatedperson-relationshiptype"
     "http://hl7.org/fhir/ValueSet/condition-ver-status"
     "http://hl7.org/fhir/ValueSet/composition-status"
     "http://hl7.org/fhir/ValueSet/insuranceplan-applicability"
     "http://hl7.org/fhir/ValueSet/related-artifact-type"
     "http://hl7.org/fhir/ValueSet/request-intent"
     "http://hl7.org/fhir/ValueSet/list-empty-reason"
     "http://hl7.org/fhir/ValueSet/claim-use"
     "http://hl7.org/fhir/ValueSet/research-study-objective-type"
     "http://hl7.org/test-plannet/ValueSet/AcceptingPatientsVS"
     "http://hl7.org/fhir/ValueSet/allergy-intolerance-criticality"
     "http://hl7.org/fhir/ValueSet/operation-parameter-use"
     "http://hl7.org/fhir/ValueSet/repository-type"
     "http://hl7.org/fhir/ValueSet/contact-point-system"
     "http://hl7.org/fhir/ValueSet/referencerange-meaning"
     "http://hl7.org/fhir/ValueSet/medication-status"
     "http://hl7.org/fhir/ValueSet/coverage-type"
     "http://hl7.org/fhir/ValueSet/device-status"
     "http://hl7.org/fhir/ValueSet/action-grouping-behavior"
     "http://hl7.org/fhir/ValueSet/provenance-entity-role"
     "http://hl7.org/fhir/ValueSet/administrative-gender"
     "http://hl7.org/fhir/ValueSet/all-types"
     "http://hl7.org/fhir/ValueSet/goal-achievement"
     "http://hl7.org/fhir/ValueSet/fm-status"
     "http://hl7.org/fhir/ValueSet/task-status"
     "http://hl7.org/fhir/ValueSet/request-status"
     "http://hl7.org/fhir/ValueSet/immunization-evaluation-status"
     "http://hl7.org/fhir/ValueSet/action-selection-behavior"
     "http://hl7.org/fhir/ValueSet/inactive"
     "http://hl7.org/fhir/ValueSet/identifier-use"
     "http://hl7.org/fhir/ValueSet/devicemetric-type"
     "http://hl7.org/fhir/ValueSet/action-condition-kind"
     "http://hl7.org/fhir/ValueSet/event-timing"
     "http://hl7.org/fhir/ValueSet/device-action"
     "http://hl7.org/fhir/ValueSet/address-type"
     "http://hl7.org/fhir/ValueSet/metric-category"
     "http://hl7.org/fhir/ValueSet/contract-signer-type"
     "http://hl7.org/fhir/ValueSet/medication-statement-category"
     "http://hl7.org/fhir/ValueSet/immunization-status"
     "http://hl7.org/fhir/ValueSet/practitioner-specialty"
     "http://hl7.org/fhir/ValueSet/contact-point-use"
     "http://hl7.org/fhir/ValueSet/location-mode"
     "http://hl7.org/fhir/ValueSet/vision-eye-codes"
     "http://hl7.org/fhir/ValueSet/detectedissue-mitigation-action"
     "http://hl7.org/fhir/ValueSet/allergyintolerance-verification"
     "http://hl7.org/fhir/ValueSet/medicationdispense-category"
     "http://hl7.org/fhir/ValueSet/link-type"
     "http://terminology.hl7.org/ValueSet/v2-0276"
     "http://hl7.org/fhir/ValueSet/metric-operational-status"
     "http://hl7.org/fhir/ValueSet/condition-severity"
     "http://hl7.org/fhir/ValueSet/substance-status"
     "http://hl7.org/fhir/ValueSet/care-plan-activity-status"
     "http://hl7.org/fhir/ValueSet/encounter-special-arrangements"
     "http://hl7.org/fhir/ValueSet/allergyintolerance-clinical"
     "http://hl7.org/fhir/ValueSet/metric-calibration-state"
     "http://hl7.org/fhir/ValueSet/detectedissue-category"
     "http://hl7.org/fhir/ValueSet/contributor-type"
     "http://hl7.org/fhir/ValueSet/signature-type"
     "http://hl7.org/fhir/ValueSet/episode-of-care-status"
     "http://hl7.org/fhir/ValueSet/observation-status"
     "http://hl7.org/fhir/ValueSet/encounter-admit-source"
     "http://hl7.org/fhir/ValueSet/encounter-status"
     "http://hl7.org/fhir/ValueSet/endpoint-status"
     "http://hl7.org/fhir/ValueSet/medication-admin-status"
     "http://hl7.org/fhir/ValueSet/appointmentstatus"
     "http://hl7.org/fhir/ValueSet/c80-practice-codes"
     "http://hl7.org/fhir/ValueSet/ucum-vitals-common"
     "http://hl7.org/fhir/ValueSet/location-status"
     "http://hl7.org/fhir/ValueSet/request-priority"
     "http://hl7.org/fhir/ValueSet/document-relationship-type"
     "http://hl7.org/fhir/ValueSet/diagnostic-report-status"
     "http://hl7.org/fhir/ValueSet/address-use"
     "http://hl7.org/fhir/ValueSet/report-codes"
     "http://hl7.org/fhir/us/core/ValueSet/detailed-ethnicity"
     "http://hl7.org/fhir/ValueSet/item-type"
     "http://hl7.org/fhir/ValueSet/remittance-outcome"
     "http://hl7.org/fhir/ValueSet/clinicalimpression-status"
     "http://hl7.org/fhir/ValueSet/quantity-comparator"
     "http://hl7.org/fhir/ValueSet/action-cardinality-behavior"
     "http://hl7.org/fhir/ValueSet/care-plan-activity-kind"
     "http://hl7.org/fhir/ValueSet/languages"
     "http://hl7.org/fhir/ValueSet/action-required-behavior"
     "http://hl7.org/fhir/ValueSet/resource-types"
     "http://hl7.org/fhir/ValueSet/observation-category"
     "http://hl7.org/fhir/ValueSet/specimen-status"
     "http://hl7.org/fhir/ValueSet/participationstatus"
     "http://hl7.org/fhir/ValueSet/doc-typecodes"
     "http://hl7.org/fhir/ValueSet/medication-statement-status"
     "http://hl7.org/fhir/ValueSet/goal-status"
     "http://hl7.org/fhir/ValueSet/publication-status"
     "http://hl7.org/fhir/ValueSet/use-context"
     "http://hl7.org/fhir/ValueSet/contract-publicationstatus"},
   "CodeSystem"
   #{"http://hl7.org/fhir/link-type"
     "http://hl7.org/fhir/practitioner-specialty"
     "http://terminology.hl7.org/CodeSystem/v3-ActMood"
     "http://hl7.org/fhir/administrative-gender"},
   "Concept"
   #{"http:--hl7.org-fhir-administrative-gender-other"
     "http:--terminology.hl7.org-CodeSystem-v3-ActMood-EVN.CRT"
     "http:--terminology.hl7.org-CodeSystem-v3-ActMood-RSK"
     "http:--terminology.hl7.org-CodeSystem-v3-ActMood-PRMS.CRT"
     "http:--terminology.hl7.org-CodeSystem-v3-ActMood-GOL"
     "http:--terminology.hl7.org-CodeSystem-umls-C4683555"
     "http:--terminology.hl7.org-CodeSystem-v3-ActMood-RQO.CRT"
     "http:--hl7.org-fhir-link-type-seealso"
     "http:--terminology.hl7.org-CodeSystem-v3-ActMood-CRT"
     "http:--snomed.info-sct-444256004"
     "http:--terminology.hl7.org-CodeSystem-v3-ActMood-RSK.CRT"
     "http:--terminology.hl7.org-CodeSystem-v3-ActMood-GOL.CRT"
     "http:--terminology.hl7.org-CodeSystem-v3-ActMood-INT.CRT"
     "http:--terminology.hl7.org-CodeSystem-v3-ActMood-OPT"
     "http:--hl7.org-fhir-practitioner-specialty-dietary"
     "http:--terminology.hl7.org-CodeSystem-v3-ActMood-EXPEC"}})


(t/use-fixtures :once
  (fn [t]
    (reset! ztx @(zen.core/new-context {}))

    (do ;; 'nested-extensions test fixtures
      (def from-network-extension    (-> "zen/fhir/plannet_fromnetwork_stripped.edn" io/resource slurp read-string))
      (def new-patients-extension    (-> "zen/fhir/plannet_newpatients_stripped.edn" io/resource slurp read-string))
      (def practitioner-role-profile (-> "zen/fhir/plannet_practitionerrole_stripped.edn" io/resource slurp read-string))

      (sut/load-definiton ztx nil {:url (:url practitioner-role-profile)} practitioner-role-profile)
      (sut/load-definiton ztx nil {:url (:url new-patients-extension)} new-patients-extension)
      (sut/load-definiton ztx nil {:url (:url from-network-extension)} from-network-extension))

    (sut/load-all ztx "hl7.fhir.r4.core" {:params {:whitelist deps}})

    (t)))


(t/deftest fhir-aidbox-poly-keys-mapping
  (match-inter ztx "StructureDefinition" "http://hl7.org/fhir/us/core/StructureDefinition/us-core-patient"
    {:derivation     "constraint"
     :type           "Patient"
     :kind           "resource"
     :url            "http://hl7.org/fhir/us/core/StructureDefinition/us-core-patient"
     :baseDefinition "http://hl7.org/fhir/StructureDefinition/Patient"
     :fhirVersion    "4.0.1"
     :|              {:address       {:vector     true
                                      :type       "Address"
                                      :fhir/flags #{:MS}
                                      :|          {:line       {:vector true :fhir/flags #{:MS} :type "string"}
                                                   :city       {:fhir/flags #{:MS} :type "string"}
                                                   :state      {:fhir/flags #{:MS} :type "string"}
                                                   :postalCode {:short "US Zip Codes" :fhir/flags #{:MS} :type "string"}
                                                   :period     {:fhir/flags #{:MS} :type "Period"}}}
                      :race          {
                                      ;; :maxItems   nil?
                                      ;; :extension-profiles nil?
                                      :fhir/flags #{:MS}
                                      :fhir/extension "http://hl7.org/fhir/us/core/StructureDefinition/us-core-race"}
                      :name          {:fhir/flags #{:MS}
                                      :type       "HumanName"
                                      :vector     true
                                      :minItems   1
                                      :required   true
                                      :| {:family {:type "string" :condition ["us-core-8"] :fhir/flags #{:MS}}
                                          :given  {:type "string" :condition ["us-core-8"] :vector true :fhir/flags #{:MS}}}}
                      :birthDate     {:type "date" :fhir/flags #{:MS}}
                      :ethnicity     {:fhir/flags #{:MS}
                                      :fhir/extension "http://hl7.org/fhir/us/core/StructureDefinition/us-core-ethnicity"}
                      :communication {:vector     true
                                      :type       "BackboneElement"
                                      :fhir/flags #{:MS}
                                      :|          {:language {:type "CodeableConcept" :fhir/flags #{:MS} :required true}}}
                      :identifier    {:type       "Identifier"
                                      :vector     true
                                      :fhir/flags #{:MS}
                                      :minItems   1
                                      :required   true
                                      :|          {:system {:type "uri" :fhir/flags #{:MS} :required true}
                                                   :value {:short      "The value that is unique within the system."
                                                           :type       "string"
                                                           :fhir/flags #{:MS}
                                                           :required   true}}}
                      :telecom       {:vector     true
                                      :fhir/flags #{:MS}
                                      :type       "ContactPoint"
                                      :| {:system {:type       "code"
                                                   :binding    {:strength    "required"
                                                                :description "Telecommunications form for contact point."
                                                                :valueSet    {:url "http://hl7.org/fhir/ValueSet/contact-point-system"}}
                                                   :fhir/flags #{:MS}
                                                   :required   true}
                                          :value  {:required true :fhir/flags #{:MS} :type "string"}
                                          :use    {:binding    {:strength "required"
                                                                :valueSet {:url "http://hl7.org/fhir/ValueSet/contact-point-use"}}
                                                :fhir/flags #{:MS}
                                                :type       "code"}}}
                      :gender {:type       "code"
                               :fhir/flags #{:MS}
                               :required   true
                               :binding {:strength "required"
                                         :valueSet {:url "http://hl7.org/fhir/ValueSet/administrative-gender"}}}
                      :birthsex {:fhir/extension "http://hl7.org/fhir/us/core/StructureDefinition/us-core-birthsex"
                                 :fhir/flags #{:MS}
                                 :binding    {:strength    "required"
                                              :description "Code for sex assigned at birth"
                                              :valueSet    {:url "http://hl7.org/fhir/us/core/ValueSet/birthsex"}}}}
     })

  (match-inter ztx "StructureDefinition" "http://hl7.org/fhir/StructureDefinition/Observation"
    {:baseDefinition "http://hl7.org/fhir/StructureDefinition/DomainResource"
     :kind           "resource",
     :type           "Observation"
     :derivation     "specialization",
     :fhir-poly-keys {:valueQuantity {:key :value, :type "Quantity"}
                      :valueBoolean  {:key :value, :type "boolean"}}
     :|              {:value     {:polymorphic true
                                  :|           {:boolean  {:type "boolean"}
                                                :Quantity {:type "Quantity"}}}
                      :component {:fhir-poly-keys {:valueQuantity {:key :value, :type "Quantity"}
                                                   :valueBoolean  {:key :value, :type "boolean"}}
                                  :|              {:value {:polymorphic true
                                                           :|           {:boolean  {:type "boolean"}
                                                                         :Quantity {:type "Quantity"}}}}}}})

  (match-inter ztx "StructureDefinition" "http://hl7.org/fhir/us/core/StructureDefinition/pediatric-bmi-for-age"
    {:baseDefinition "http://hl7.org/fhir/StructureDefinition/vitalsigns"
     :kind           "resource",
     :type           "Observation"
     :derivation     "constraint",
     :|              {:value
                      {
                       ;;TODO: check this case
                       ;; :polymorphic true
                       :| {:Quantity
                           {:| {:value {:required true}}}}}}})

  (match-inter ztx "StructureDefinition" "http://hl7.org/fhir/StructureDefinition/Patient"
    {:kind           "resource"
     :derivation     "specialization",
     :baseDefinition "http://hl7.org/fhir/StructureDefinition/DomainResource"
     :|              {:address             {:short  "An address for the individual"
                                            :type   "Address"
                                            :vector true}
                      :multipleBirth       {:|           {:boolean {:type "boolean"}
                                                          :integer {:type "integer"}}
                                            :types       #{"boolean" "integer"}
                                            :polymorphic true}
                      :link                {:type   "BackboneElement"
                                            :vector true
                                            :|      {:other {:type     "Reference"
                                                             :required true}
                                                     :type  {:binding  {:strength "required"
                                                                        :valueSet {:url "http://hl7.org/fhir/ValueSet/link-type"
                                                                                   :version "4.0.1"}}
                                                             :required true}}}
                      :generalPractitioner {:vector   true
                                            :type     "Reference"
                                            :profiles #{"http://hl7.org/fhir/StructureDefinition/Organization"
                                                        "http://hl7.org/fhir/StructureDefinition/Practitioner"
                                                        "http://hl7.org/fhir/StructureDefinition/PractitionerRole"}}}})

  (match-inter ztx "StructureDefinition" "http://hl7.org/fhir/StructureDefinition/Questionnaire"
    {:| {:description {}
         :subjectType {}
         :item
         {:| {:item       {:recur [:item]}
              :enableWhen {:| {:question {}
                               :operator {}
                               :answer   {}}}}}}})

  (comment
    (inspect/inspect "/tmp/pres.html" (get-in @ztx [:fhir/inter "StructureDefinition"]) {:closed true})

    (inspect/inspect "/tmp/us-core-pt.html" (sut/get-definition ztx "http://hl7.org/fhir/us/core/StructureDefinition/us-core-patient"))

    (see-definition-els ztx "http://hl7.org/fhir/us/carin-bb/StructureDefinition/C4BB-ExplanationOfBenefit-Inpatient-Institutional")

    (sut/get-original ztx "http://hl7.org/fhir/us/carin-bb/StructureDefinition/C4BB-ExplanationOfBenefit-Inpatient-Institutional")

    (inspect/inspect "/tmp/slice.html"
                     (sut/get-definition ztx "http://hl7.org/fhir/us/carin-bb/StructureDefinition/C4BB-ExplanationOfBenefit-Inpatient-Institutional")
                     #_(->> (get-in @ztx [:fhir/inter "StructureDefinition"])
                          (filter (fn [[k v]] (str/includes? (str v) ":slicing")))
                          (take 20)
                          (into {})))

    (see-definition-els  ztx  "http://hl7.org/fhir/StructureDefinition/patient-nationality")
    (see-definition-els  ztx "http://hl7.org/fhir/StructureDefinition/condition-dueTo")
    (see-definition-els  ztx "http://hl7.org/fhir/StructureDefinition/structuredefinition-xml-type")
    (see-definition-els  ztx "http://hl7.org/fhir/us/core/StructureDefinition/us-core-patient")
    (sut/get-definition  ztx "http://hl7.org/fhir/us/core/StructureDefinition/us-core-patient")

    ;; (->> (:element (:differential (sut/get-original ztx)))
    ;;      (mapv #(select-keys % [:id :min :max :sliceName :binding :fixedUri :type])))

    (def ares (sut/get-definition ztx "http://hl7.org/fhir/StructureDefinition/Address"))))


(t/deftest nested-extensions
  (match-inter ztx "StructureDefinition" "http://hl7.org/test-plannet/StructureDefinition/plannet-PractitionerRole"
    {:|
     {:newpatients
      {:fhir/extension
       "http://hl7.org/test-plannet/StructureDefinition/newpatients"}}})

  (match-inter ztx "StructureDefinition" "http://hl7.org/test-plannet/StructureDefinition/newpatients"
    {:|
     {:acceptingPatients {}
      :fromnetwork {:fhir/extension "http://hl7.org/test-plannet/StructureDefinition/plannet-FromNetwork-extension"}}})

  (match-inter ztx "StructureDefinition" "http://hl7.org/test-plannet/StructureDefinition/plannet-FromNetwork-extension"
    {:type "Reference"
     :baseDefinition "http://hl7.org/fhir/StructureDefinition/Reference"}))


(t/deftest value-sets
  (t/testing "value set processing"
    (match-inter ztx "ValueSet" "http://hl7.org/fhir/ValueSet/administrative-gender"
      {:url "http://hl7.org/fhir/ValueSet/administrative-gender"
       :zen.fhir/package-ns 'hl7-fhir-r4-core
       :zen.fhir/schema-ns 'hl7-fhir-r4-core.value-set.administrative-gender
       :zen.fhir/resource
       {:url "http://hl7.org/fhir/ValueSet/administrative-gender"
        :resourceType "ValueSet"
        :id "administrative-gender"
        :zen.fhir/header nil?
        :zen.fhir/package nil?
        :zen.fhir/package-ns nil?
        :zen.fhir/schema-ns nil?}}))

  (t/testing "code system contained concepts extract"
    (match-inter ztx "CodeSystem" "http://hl7.org/fhir/administrative-gender"
      {:zen.fhir/resource
       {:url "http://hl7.org/fhir/administrative-gender"
        :resourceType "CodeSystem"
        :id "administrative-gender"
        :zen.fhir/header nil?
        :zen.fhir/package nil?
        :zen.fhir/package-ns nil?
        :zen.fhir/schema-ns nil?}
       :url "http://hl7.org/fhir/administrative-gender"
       :concept nil?
       :zen.fhir/package-ns 'hl7-fhir-r4-core
       :fhir/concepts
       {"http:--hl7.org-fhir-administrative-gender-male"
        {:id "http:--hl7.org-fhir-administrative-gender-male"
         :code "male"
         :display "Male"
         :definition "Male."}
        "http:--hl7.org-fhir-administrative-gender-female"
        {:id "http:--hl7.org-fhir-administrative-gender-female"
         :code "female"
         :display "Female"
         :definition "Female."}
        "http:--hl7.org-fhir-administrative-gender-other"
        {:id "http:--hl7.org-fhir-administrative-gender-other"
         :code "other"
         :display "Other"
         :definition "Other."}
        "http:--hl7.org-fhir-administrative-gender-unknown"
        {:id "http:--hl7.org-fhir-administrative-gender-unknown"
         :code "unknown"
         :display "Unknown"
         :definition "Unknown."}}})

    (match-inter ztx "Concept" "http:--hl7.org-fhir-administrative-gender-other"
      {:id         "http:--hl7.org-fhir-administrative-gender-other"
       :code       "other"
       :system     "http://hl7.org/fhir/administrative-gender"
       :display    "Other"
       :definition "Other."
       :valueset   #(contains? % "http://hl7.org/fhir/ValueSet/administrative-gender")
       :_source "zen.fhir"
       :zen.fhir/package-ns 'hl7-fhir-r4-core
       :zen.fhir/resource
       {:resourceType "Concept"
        :id           "http:--hl7.org-fhir-administrative-gender-other"
        :code         "other"
        :system       "http://hl7.org/fhir/administrative-gender"
        :_source      "zen.fhir"
        :valueset     #(and (vector? %)
                            (contains? (set %) "http://hl7.org/fhir/ValueSet/administrative-gender"))
        :zen.fhir/header     nil?
        :zen.fhir/package    nil?
        :zen.fhir/package-ns nil?
        :zen.fhir/schema-ns  nil?}})

    (t/testing "hierarchy & property extract"
      (match-inter ztx "Concept" "http:--terminology.hl7.org-CodeSystem-v3-ActMood-EXPEC"
        {:code      "EXPEC"
         :system    "http://terminology.hl7.org/CodeSystem/v3-ActMood"
         :hierarchy ["_ActMoodPredicate" nil]
         :property  nil?})

      (match-inter ztx "Concept" "http:--terminology.hl7.org-CodeSystem-v3-ActMood-GOL.CRT"
        {:code      "GOL.CRT"
         :system    "http://terminology.hl7.org/CodeSystem/v3-ActMood"
         :hierarchy ["_ActMoodPredicate" "CRT" nil]
         :property  {"status" "retired"}})))

  (t/testing "value set contained concepts extract"
    (match-inter ztx "ValueSet" "http://hl7.org/fhir/us/mcode/ValueSet/mcode-cancer-staging-system-vs"
      {:url "http://hl7.org/fhir/us/mcode/ValueSet/mcode-cancer-staging-system-vs"
       :zen.fhir/package-ns 'hl7-fhir-us-mcode
       :zen.fhir/schema-ns 'hl7-fhir-us-mcode.value-set.mcode-cancer-staging-system-vs
       :fhir/concepts
       {"http:--terminology.hl7.org-CodeSystem-umls-C4683555"
        {:id      "http:--terminology.hl7.org-CodeSystem-umls-C4683555"
         :code    "C4683555"
         :system  "http://terminology.hl7.org/CodeSystem/umls"
         :display "Ann Arbor Stage"}

        "http:--snomed.info-sct-444256004"
        {:id "http:--snomed.info-sct-444256004"
         :system "http://snomed.info/sct"
         :code "444256004"
         :display string?}}}))

  (t/testing "compose"
    (t/testing "include.system"
      (match-inter ztx "ValueSet" "http://hl7.org/fhir/ValueSet/link-type"
                   {:compose {:include [{:system "http://hl7.org/fhir/link-type"}]}})

      (match-inter ztx "CodeSystem" "http://hl7.org/fhir/link-type"
                   {:fhir/concepts {"http:--hl7.org-fhir-link-type-seealso" {:code "seealso"}}})

      (match-inter ztx "Concept" "http:--hl7.org-fhir-link-type-seealso"
        {:id       "http:--hl7.org-fhir-link-type-seealso"
         :valueset #{"http://hl7.org/fhir/ValueSet/link-type"}}))

    (t/testing "include.concept"
      (match-inter ztx "Concept" "http:--terminology.hl7.org-CodeSystem-umls-C4683555"
        {:id       "http:--terminology.hl7.org-CodeSystem-umls-C4683555"
         :code     "C4683555"
         :display  "Ann Arbor Stage"
         :system   "http://terminology.hl7.org/CodeSystem/umls"
         :valueset #{"http://hl7.org/fhir/us/mcode/ValueSet/mcode-cancer-staging-system-vs"}
         :zen.fhir/resource {:valueset ["http://hl7.org/fhir/us/mcode/ValueSet/mcode-cancer-staging-system-vs"]}})

      (match-inter ztx "Concept" "http:--snomed.info-sct-444256004"
        {:id       "http:--snomed.info-sct-444256004"
         :code     "444256004"
         :display  string?
         :system   "http://snomed.info/sct"
         :valueset #(contains? % "http://hl7.org/fhir/us/mcode/ValueSet/mcode-cancer-staging-system-vs")}))

    (t/testing "include.valueSet"
      (match-inter ztx "ValueSet" "http://hl7.org/fhir/ValueSet/use-context"
                   {:compose
                    {:include #(->> %
                                    (mapcat :valueSet)
                                    (filter #{"http://hl7.org/fhir/ValueSet/practitioner-specialty"})
                                    seq
                                    boolean)}})

      (match-inter ztx "ValueSet" "http://hl7.org/fhir/ValueSet/practitioner-specialty"
                   {:compose {:include [{:system "http://hl7.org/fhir/practitioner-specialty"}]}})

      (match-inter ztx "CodeSystem" "http://hl7.org/fhir/practitioner-specialty"
                   {:fhir/concepts
                    {"http:--hl7.org-fhir-practitioner-specialty-dietary"
                     {:code "dietary"}}})

      (match-inter ztx "Concept" "http:--hl7.org-fhir-practitioner-specialty-dietary"
        {:valueset #{"http://hl7.org/fhir/ValueSet/practitioner-specialty"
                     "http://hl7.org/fhir/ValueSet/use-context"}}))

    (t/testing "include.filter"
      (t/testing "descendent-of"
        (match-inter ztx "ValueSet" "http://hl7.org/fhir/ValueSet/inactive"
                     {:compose {:include [{:system "http://terminology.hl7.org/CodeSystem/v3-ActMood",
                                           :filter
                                           [{:property "concept",
                                             :op "descendent-of",
                                             :value "_ActMoodPredicate"}]}]}})

        (match-inter ztx "CodeSystem" "http://terminology.hl7.org/CodeSystem/v3-ActMood"
                     {:fhir/concepts
                      #(clojure.set/subset? #{"RSK" "RQO.CRT" "CRT"
                                              "EXPEC" "PRMS.CRT" "OPT"
                                              "EVN.CRT" "INT.CRT" "GOL.CRT"
                                              "RSK.CRT" "GOL"}
                                            (set (map (comp :code val) %)))})

        (doseq [code ["RSK" "GOL" "CRT" "OPT" "EXPEC" "EVN.CRT" "PRMS.CRT" "RQO.CRT" "RSK.CRT" "GOL.CRT" "INT.CRT"]]
          (match-inter ztx "Concept" (str "http:--terminology.hl7.org-CodeSystem-v3-ActMood-" code)
                       {:valueset #(contains? % "http://hl7.org/fhir/ValueSet/inactive")}))))))
