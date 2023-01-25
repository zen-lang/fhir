(ns zen.fhir.generator-test
  (:require
   [zen.fhir.generator :as sut]
   [zen.fhir.core]
   [zen.core]
   [zen.package]
   [matcho.core :as matcho]
   [clojure.test :as t]
   [clojure.java.io :as io]
   [cheshire.core :as json]
   [clojure.java.shell :as sh]
   [clojure.set]))


(def zen-fhir-version (slurp (clojure.java.io/resource "zen-fhir-version")))


(defonce ztx (zen.core/new-context {}))


(t/use-fixtures :once
  (fn generate&spit-project [t]
    (reset! ztx @(zen.core/new-context {}))

    (zen.fhir.core/init-ztx ztx)

    (do ;; 'nested-extension test fixtures
      (def from-network-extension (-> "zen/fhir/plannet_fromnetwork_stripped.edn" io/resource slurp read-string))
      (def new-patients-extension (-> "zen/fhir/plannet_newpatients_stripped.edn" io/resource slurp read-string))
      (def practitioner-role-profile (-> "zen/fhir/plannet_practitionerrole_stripped.edn" io/resource slurp read-string))
      (def multiple-same-url-includes-excludes-vs (-> "zen/fhir/ig-fiction_multiple-same-url-includes-excludes.edn" io/resource slurp read-string))
      (zen.fhir.core/load-definiton ztx {:url (:url practitioner-role-profile)} (assoc practitioner-role-profile :zen.fhir/package-ns "plannet"))
      (zen.fhir.core/load-definiton ztx {:url (:url new-patients-extension)} (assoc new-patients-extension :zen.fhir/package-ns "plannet"))
      (zen.fhir.core/load-definiton ztx {:url (:url from-network-extension)} (assoc from-network-extension :zen.fhir/package-ns "plannet"))
      (zen.fhir.core/load-definiton ztx {:url (:url multiple-same-url-includes-excludes-vs)} (assoc multiple-same-url-includes-excludes-vs :zen.fhir/package-ns "ig-fiction")))

    (zen.fhir.core/load-all ztx nil
                            {:params {"hl7.fhir.r4.core" {:zen.fhir/package-ns 'fhir-r4}
                                      "hl7.fhir.us.core" {:zen.fhir/package-ns 'us-core}}
                             :whitelist {"ValueSet" #{"http://hl7.org/fhir/ValueSet/administrative-gender"
                                                      "http://cts.nlm.nih.gov/fhir/ValueSet/2.16.840.1.113762.1.4.1"
                                                      "http://cts.nlm.nih.gov/fhir/ValueSet/2.16.840.1.113762.1.4.1021.103"
                                                      "http://cts.nlm.nih.gov/fhir/ValueSet/2.16.840.1.113883.1.11.14914"
                                                      "http://cts.nlm.nih.gov/fhir/ValueSet/2.16.840.1.113883.3.2074.1.1.3"
                                                      "http://hl7.org/fhir/us/core/ValueSet/birthsex"
                                                      "http://hl7.org/fhir/us/core/ValueSet/detailed-race"
                                                      "http://hl7.org/fhir/ValueSet/c80-practice-codes"
                                                      "http://hl7.org/fhir/us/davinci-pdex-plan-net/ValueSet/SpecialtiesVS"
                                                      "http://hl7.org/fhir/us/davinci-pdex-plan-net/ValueSet/IndividualAndGroupSpecialtiesVS"
                                                      "http://hl7.org/fhir/us/davinci-pdex-plan-net/ValueSet/NonIndividualSpecialtiesVS"}}})

    (sut/generate-zen-schemas ztx)

    (t)))


(t/deftest generate-project-integration
  (def schemas-match
    {'fhir-r4
     {'ns 'fhir-r4
      'import #{'zen.fhir}

      'ig
      {:zen/tags   #{'zen.fhir/ig}
       :profiles   'profiles
       :extensions 'extensions
       :value-sets 'value-sets
       :searches   'searches}

      'base-schemas
      {:zen/tags #{'zen.fhir/base-schemas}
       :schemas  {"DomainResource"
                  {"http://hl7.org/fhir/StructureDefinition/DomainResource"
                   (with-meta 'fhir-r4.DomainResource/schema {:zen/quote true})}

                  "Patient"
                  {"http://hl7.org/fhir/StructureDefinition/Patient"
                   (with-meta 'fhir-r4.Patient/schema {:zen/quote true})}

                  "Practitioner"
                  {"http://hl7.org/fhir/StructureDefinition/Practitioner"
                   (with-meta 'fhir-r4.Practitioner/schema {:zen/quote true})}}}

      'profiles
      {:zen/tags #{'zen.fhir/profiles}
       :schemas  {"ServiceRequest"
                  {"http://hl7.org/fhir/StructureDefinition/servicerequest-genetics"
                   (with-meta 'fhir-r4.servicerequest-genetics/schema {:zen/quote true})}}}

      'extensions
      {:zen/tags #{'zen.fhir/extensions}
       :schemas  {"http://hl7.org/fhir/StructureDefinition/patient-nationality"
                  (with-meta 'fhir-r4.patient-nationality/schema {:zen/quote true})}}

      'structures
      {:zen/tags #{'zen.fhir/structures}
       :schemas {"http://hl7.org/fhir/StructureDefinition/string"
                 (with-meta 'fhir-r4.string/schema {:zen/quote true})

                 "http://hl7.org/fhir/StructureDefinition/Element"
                 (with-meta 'fhir-r4.Element/schema {:zen/quote true})

                 "http://hl7.org/fhir/StructureDefinition/Resource"
                 (with-meta 'fhir-r4.Resource/schema {:zen/quote true})}}

      'value-sets
      {:zen/tags   #{'zen.fhir/value-sets}
       :value-sets {"http://hl7.org/fhir/ValueSet/administrative-gender"
                    (with-meta 'fhir-r4.value-set.administrative-gender/value-set {:zen/quote true})}}

      'searches
      {:zen/tags #{'zen.fhir/searches}
       :searches {"phone"
                  {"http://hl7.org/fhir/SearchParameter/OrganizationAffiliation-phone"
                   'fhir-r4.search.OrganizationAffiliation-phone/search,
                   "http://hl7.org/fhir/SearchParameter/individual-phone"
                   'fhir-r4.search.individual-phone/search}}}}

     'us-core
     {'ns 'us-core
      'import (partial clojure.set/subset?
                       #{'zen.fhir 'fhir-r4})

      'ig {:zen/tags   #{'zen.fhir/ig}
           :profiles   'profiles
           :extensions 'extensions
           :value-sets 'value-sets
           :searches   'searches}
      'profiles   {}
      'extensions {}
      'value-sets {}
      'searches   {}}

     'fhir-r4.string
     {'ns     'fhir-r4.string
      'schema {:zen/tags #{'zen/schema 'zen.fhir/structure-schema}
               :confirms #(not (contains? % 'fhir-r4.Element/schema))
               :type 'zen/string
               :zen.fhir/version zen-fhir-version}}

     'fhir-r4.Extension
     {'ns     'fhir-r4.Extension
      'schema {:zen/tags #{'zen/schema 'zen.fhir/structure-schema}
               :confirms #{'fhir-r4.Element/schema}
               :zen.fhir/version zen-fhir-version
               :type     'zen/map
               :keys     {:url {:confirms #{'fhir-r4.uri/schema}}
                          :value {:type 'zen/map
                                  :exclusive-keys (comp not empty?)
                                  :keys {:uri {:confirms #{'fhir-r4.uri/schema}}
                                         :url {:confirms #{'fhir-r4.url/schema}}
                                         :string {:confirms #{'fhir-r4.string/schema}}
                                         :Reference {:confirms #{'fhir-r4.Reference/schema 'zen.fhir/Reference}}}}}}}

     'fhir-r4.Element
     {'ns     'fhir-r4.Element
      'schema {:zen/tags #{'zen/schema 'zen.fhir/structure-schema}
               :confirms empty?
               :zen.fhir/version zen-fhir-version
               :type     'zen/map
               :keys     {:id        {:confirms #{'fhir-r4.string/schema}}
                          :extension {:type  'zen/vector
                                      :every {:confirms #{'fhir-r4.Extension/schema}}}}}}

     'fhir-r4.Resource
     {'ns     'fhir-r4.Resource
      'schema {:zen/tags #{'zen/schema 'zen.fhir/structure-schema}
               :confirms #{'zen.fhir/Resource}
               :type 'zen/map
               :keys {:id            {:confirms #{'fhir-r4.string/schema}
                                      :fhir/flags #{:SU}}
                      :meta          {:confirms #{'fhir-r4.Meta/schema}
                                      :fhir/flags #{:SU}}
                      :implicitRules {:confirms #{'fhir-r4.uri/schema}
                                      :fhir/flags #{:SU :?!}}
                      :language      {:confirms #{'fhir-r4.code/schema}
                                      :fhir/flags nil?}}}}

     'fhir-r4.DomainResource
     {'ns     'fhir-r4.DomainResource
      'import #(contains? % 'fhir-r4.Resource)
      'schema {:zen/tags #{'zen/schema 'zen.fhir/structure-schema}
               :confirms #{'fhir-r4.Resource/schema 'zen.fhir/Resource}
               :type 'zen/map
               :keys {:text              {:confirms #{'fhir-r4.Narrative/schema}}
                      :contained         {:type  'zen/vector
                                          :every {:confirms #{'fhir-r4.Resource/schema}}}
                      :extension         {:type  'zen/vector
                                          :every {:confirms #{'fhir-r4.Extension/schema}}}
                      :modifierExtension {:type  'zen/vector
                                          :every {:confirms #{'fhir-r4.Extension/schema}}}}}}

     'fhir-r4.value-set.administrative-gender
     {'ns 'fhir-r4.value-set.administrative-gender
      'import #(contains? % 'zen.fhir)
      'value-set {:zen/tags #{'zen.fhir/value-set}
                  :uri "http://hl7.org/fhir/ValueSet/administrative-gender"
                  :fhir/code-systems #{{:fhir/url "http://hl7.org/fhir/administrative-gender"
                                        :zen.fhir/content :bundled}}
                  :zen.fhir/version zen-fhir-version}}

     'fhir-r4.HumanName
     {'ns 'fhir-r4.HumanName
      'schema {:zen/tags #{'zen/schema 'zen.fhir/structure-schema}
               :confirms #{'fhir-r4.Element/schema}
               :type 'zen/map
               :zen.fhir/type "HumanName"
               :zen.fhir/profileUri "http://hl7.org/fhir/StructureDefinition/HumanName"
               :keys {:family  {:confirms #{'fhir-r4.string/schema}}
                      :_family {:confirms #{'fhir-r4.Element/schema}}

                      :given  {:type  'zen/vector
                               :every {:confirms #{'fhir-r4.string/schema}}}
                      :_given {:type  'zen/vector
                               :every {:confirms #{'fhir-r4.Element/schema}}}}}}

     'fhir-r4.Patient
     {'ns     'fhir-r4.Patient
      'import #(and (contains? % 'fhir-r4.DomainResource)
                    (contains? % 'zen.fhir)
                    (contains? % 'fhir-r4.value-set.administrative-gender))
      'schema {:zen/tags #{'zen/schema 'zen.fhir/base-schema}
               :confirms #{'fhir-r4.DomainResource/schema 'zen.fhir/Resource}
               :zen.fhir/version zen-fhir-version
               :type 'zen/map
               :zen.fhir/type "Patient"
               :zen.fhir/profileUri "http://hl7.org/fhir/StructureDefinition/Patient"
               :keys {:name {:type 'zen/vector
                             :every {:confirms #{'fhir-r4.HumanName/schema}}}
                      :active {:confirms #{'fhir-r4.boolean/schema}}
                      :deceased {:type 'zen/map
                                 :fhir/polymorphic true
                                 :exclusive-keys #{#{:boolean :dateTime}}
                                 :keys {:boolean {:confirms #{'fhir-r4.boolean/schema}}
                                        :dateTime {:confirms #{'fhir-r4.dateTime/schema}}}}
                      :managingOrganization {:zen/desc "Organization that is the custodian of the patient record"
                                             :confirms #{'fhir-r4.Reference/schema 'zen.fhir/Reference}
                                             :zen.fhir/reference {:refers #{'fhir-r4.Organization/schema}}}
                      :gender {:confirms #{'fhir-r4.code/schema}
                               :zen.fhir/value-set {:symbol 'fhir-r4.value-set.administrative-gender/value-set
                                                    :strength :required}}
                      :link {:type 'zen/vector
                             :every {:require #{:other :type}}}}}}

     'fhir-r4.Appointment
     {'schema {:require #{:status :participant}
               :keys {:status {:confirms #{'fhir-r4.code/schema}}
                      :participant {:type     'zen/vector
                                    :minItems 1
                                    :every    {:confirms #{'fhir-r4.BackboneElement/schema}
                                               :type 'zen/map
                                               :require #{:status}
                                               :keys {:type     {:type 'zen/vector
                                                                 :every {:confirms #{'fhir-r4.CodeableConcept/schema}}}
                                                      :actor    {:confirms #{'zen.fhir/Reference 'fhir-r4.Reference/schema}
                                                                 :zen.fhir/reference {:refers set?}}
                                                      :required {:confirms #{'fhir-r4.code/schema}}
                                                      :status   {:confirms #{'fhir-r4.code/schema}}
                                                      :period   {:confirms #{'fhir-r4.Period/schema}}}}}}}}

     'fhir-r4.ServiceRequest
     {'ns     'fhir-r4.ServiceRequest
      'schema {:keys {:supportingInfo {:every {:confirms #{'fhir-r4.Reference/schema 'zen.fhir/Reference}
                                               :zen.fhir/reference {:refers #{}}}}}}}

     'fhir-r4.Practitioner
     {'ns     'fhir-r4.Practitioner
      'import #(contains? % 'fhir-r4.value-set.administrative-gender)
      'schema {:keys {:gender {:confirms #{'fhir-r4.code/schema}
                               :zen.fhir/value-set {:symbol 'fhir-r4.value-set.administrative-gender/value-set
                                                    :strength :required}}}}}

     'us-core.us-core-patient
     {'ns     'us-core.us-core-patient
      'import #(and (contains? % 'fhir-r4.Patient)
                    (contains? % 'zen.fhir))
      'schema {:zen/tags #{'zen/schema 'zen.fhir/profile-schema}
               :zen/desc string?
               :confirms #{'fhir-r4.Patient/schema 'zen.fhir/Resource}
               :type 'zen/map
               :zen.fhir/type "Patient"
               :zen.fhir/profileUri "http://hl7.org/fhir/us/core/StructureDefinition/us-core-patient"
               :zen.fhir/version zen-fhir-version
               :require #{:name :gender :identifier}
               :keys {:race      {:confirms #{'us-core.us-core-race/schema}
                                  :fhir/extensionUri "http://hl7.org/fhir/us/core/StructureDefinition/us-core-race"}
                      :ethnicity {:confirms #{'us-core.us-core-ethnicity/schema}
                                  :fhir/extensionUri "http://hl7.org/fhir/us/core/StructureDefinition/us-core-ethnicity"}
                      :birthsex  {:confirms #{'us-core.us-core-birthsex/schema}
                                  :fhir/extensionUri "http://hl7.org/fhir/us/core/StructureDefinition/us-core-birthsex"}
                      :genderIdentity {:confirms #{'us-core.us-core-genderIdentity/schema}
                                       :fhir/extensionUri "http://hl7.org/fhir/us/core/StructureDefinition/us-core-genderIdentity"}
                      :identifier {:type     'zen/vector
                                   :minItems 1
                                   :every    {:confirms nil?
                                              :type 'zen/map
                                              :keys {:system {:confirms nil?}
                                                     :value  {:zen/desc "The value that is unique within the system."
                                                              :confirms nil?}}}}}}}

     'fhir-r4.patient-genderIdentity
     {'ns 'fhir-r4.patient-genderIdentity
      'schema {:zen.fhir/type "CodeableConcept"
               :confirms #{'fhir-r4.CodeableConcept/schema}
               :zen.fhir/profileUri "http://hl7.org/fhir/StructureDefinition/patient-genderIdentity"}}

     'us-core.us-core-genderIdentity
     {'ns 'us-core.us-core-genderIdentity
      'schema {:zen.fhir/type "CodeableConcept"
               :confirms #{'fhir-r4.patient-genderIdentity/schema}
               :zen.fhir/profileUri "http://hl7.org/fhir/us/core/StructureDefinition/us-core-genderIdentity"}}

     'us-core.value-set.birthsex
     {'ns 'us-core.value-set.birthsex
      'import #{'zen.fhir}

      'value-set
      {:uri "http://hl7.org/fhir/us/core/ValueSet/birthsex"
       :zen.fhir/version zen-fhir-version
       :fhir/code-systems #{{:fhir/url "http://terminology.hl7.org/CodeSystem/v3-AdministrativeGender"
                             :zen.fhir/content :bundled}
                            {:fhir/url "http://terminology.hl7.org/CodeSystem/v3-NullFlavor"
                             :zen.fhir/content :bundled}}}}

     'us-core.us-core-birthsex
     {'ns 'us-core.us-core-birthsex
      'import #(and (contains? % 'us-core.value-set.birthsex)
                    (contains? % 'zen.fhir))

      'schema
      {:zen.fhir/version zen-fhir-version
       :zen.fhir/value-set {:symbol 'us-core.value-set.birthsex/value-set
                            :strength :required}}}

     'us-core.value-set.detailed-race
     {'ns 'us-core.value-set.detailed-race
      'import #{'zen.fhir}

      'value-set
      {:uri "http://hl7.org/fhir/us/core/ValueSet/detailed-race"
       :zen.fhir/version zen-fhir-version
       :fhir/code-systems #{{:fhir/url "urn:oid:2.16.840.1.113883.6.238",
                             :zen.fhir/content :not-present}
                            {:fhir/url "http://terminology.hl7.org/CodeSystem/v3-NullFlavor",
                             :zen.fhir/content :bundled}}}}

     'fhir-r4.MolecularSequence
     {'schema
      {:keys {:structureVariant {:every {:keys {:variantType {:zen.fhir/value-set nil?}}}}}}}

     'hl7-fhir-us-davinci-pdex-plan-net.org-description
     {'ns 'hl7-fhir-us-davinci-pdex-plan-net.org-description
      'import #{'zen.fhir 'fhir-r4.string}

      'schema
      {:zen/tags #{'zen/schema 'zen.fhir/structure-schema}
       :zen/desc "An extension to provide a human-readable description of an organization."
       :confirms #{'fhir-r4.string/schema}
       :zen.fhir/type "string"
       :zen.fhir/profileUri "http://hl7.org/fhir/us/davinci-pdex-plan-net/StructureDefinition/org-description"}}

     'fhir-r4.condition-dueTo
     {'schema
      {:zen/tags #{'zen/schema 'zen.fhir/structure-schema}
       :type 'zen/map
       :confirms nil?
       :zen.fhir/profileUri "http://hl7.org/fhir/StructureDefinition/condition-dueTo"
       :fhir/polymorphic true
       :keys {:CodeableConcept {:confirms #{'fhir-r4.CodeableConcept/schema}}
              :Reference {:confirms #{'fhir-r4.Reference/schema 'zen.fhir/Reference}}}}}

     'hl7-fhir-us-davinci-pdex-plan-net.practitioner-qualification
     {'schema
      {:zen/tags #{'zen/schema 'zen.fhir/structure-schema}
       :confirms empty?
       :keys {:status {:confirms #{'fhir-r4.code/schema}}
              :whereValid {:type 'zen/vector
                           :every {:type 'zen/map
                                   :fhir/polymorphic true
                                   :exclusive-keys #{#{:CodeableConcept :Reference}}
                                   :keys {:CodeableConcept {:confirms #{'fhir-r4.CodeableConcept/schema}}
                                          :Reference {:confirms #{'fhir-r4.Reference/schema 'zen.fhir/Reference}}}}}}}}


     'hl7-fhir-us-davinci-pdex-plan-net.plannet-Practitioner
     {'schema
      {:zen/tags #{'zen/schema 'zen.fhir/profile-schema}
       :keys {:qualification {:type 'zen/vector
                              :every {:type 'zen/map
                                      :keys {:practitioner-qualification
                                             {:confirms #{'hl7-fhir-us-davinci-pdex-plan-net.practitioner-qualification/schema}
                                              :fhir/extensionUri
                                              "http://hl7.org/fhir/us/davinci-pdex-plan-net/StructureDefinition/practitioner-qualification"}}}}}}}


     'fhir-r4.HealthcareService
     {'schema
      {:zen/tags #{'zen/schema 'zen.fhir/base-schema}
       :keys {:specialty
              {:every {:zen.fhir/value-set
                       {:symbol 'fhir-r4.value-set.c80-practice-codes/value-set
                        :strength :preferred}}}}}}

     'hl7-fhir-us-davinci-pdex-plan-net.value-set.SpecialtiesVS
     {'value-set
      {:uri "http://hl7.org/fhir/us/davinci-pdex-plan-net/ValueSet/SpecialtiesVS"
       :fhir/code-systems #{{:fhir/url "http://nucc.org/provider-taxonomy"
                             :zen.fhir/content :bundled}}}}

     'hl7-fhir-us-davinci-pdex-plan-net.plannet-HealthcareService
     {'schema
      {:zen/tags #{'zen/schema 'zen.fhir/profile-schema}
       :keys {:specialty
              {:every {:zen.fhir/value-set
                       {:symbol 'hl7-fhir-us-davinci-pdex-plan-net.value-set.SpecialtiesVS/value-set
                        :strength :required}}}}}}

     'us-core.us-core-smokingstatus
     {'schema
      {:zen/tags #{'zen/schema 'zen.fhir/profile-schema}
       :require  #{:category :effective :subject :value}
       :keys     {:effective
                  {:type    'zen/map,
                   :keys
                   {:dateTime  {:confirms nil?, :fhir/flags #{:MS}},
                    :_dateTime nil?},
                   :require #{:dateTime}}}}}

     ;; Check that a CodeSystem present multiple times
     ;; in compose.include and compose.exclude
     ;; is not duplicated.
     'ig-fiction.value-set.multiple-same-url-includes-excludes
     {'value-set
      {:zen/tags #{'zen.fhir/value-set}
       :fhir/code-systems
       #{{:fhir/url "http://example.com",
          :zen.fhir/content :not-present}}}}

     'fhir-r4.base64Binary
     {'schema (merge
               (get sut/fhir-primitive->zen-primitive "base64Binary")
               '{:zen/tags      #{zen/schema zen.fhir/structure-schema}
                 :zen.fhir/type "base64Binary"})}})

  (t/testing "Intermediate representation is correct for"
    (t/testing "polymorphic keys defined on an element"
      (matcho/match
       (get-in (:fhir/inter @ztx)
               ["StructureDefinition" "http://hl7.org/fhir/StructureDefinition/Observation"])
       {:fhir-poly-keys
        {:valueTime            {:key :value, :type "time"},
         :valueQuantity        {:key :value, :type "Quantity"},
         :valueString          {:key :value, :type "string"},
         :valueRatio           {:key :value, :type "Ratio"},
         :valueBoolean         {:key :value, :type "boolean"},
         :valueDateTime        {:key :value, :type "dateTime"},
         :valueSampledData     {:key :value, :type "SampledData"},
         :effectiveDateTime    {:key :effective, :type "dateTime"},
         :effectiveTiming      {:key :effective, :type "Timing"},
         :valueCodeableConcept {:key :value, :type "CodeableConcept"},
         :valuePeriod          {:key :value, :type "Period"},
         :effectiveInstant     {:key :effective, :type "instant"},
         :valueRange           {:key :value, :type "Range"},
         :valueInteger         {:key :value, :type "integer"},
         :effectivePeriod      {:key :effective, :type "Period"}}}))

    (t/testing "inherited polymorphic keys"
      (matcho/match
       (get-in (:fhir/inter @ztx)
               ["StructureDefinition" "http://hl7.org/fhir/us/core/StructureDefinition/us-core-smokingstatus"])
       {:|
        {:effective
         {:|
          {:dateTime  {:required            true,
                       :fhir/flags          #{:MS},
                       :type                "dateTime",
                       :fhir/primitive-attr true},
           :_dateTime {:type         "Element",
                       :original-key :dateTime}}
          :required true}
         :effectiveDateTime nil}})))

  (t/testing "Generated schmemas are correct"
    (matcho/match
     (select-keys (:fhir.zen/ns @ztx) (keys schemas-match))
     schemas-match)))


(t/deftest zen-schemas-validation
  (def zctx (zen.core/new-context
             {:package-paths ["zen.fhir"]
              :memory-store (:fhir.zen/ns @ztx)}))

  (run! (fn [zen-ns]
          (zen.core/load-ns zctx (get-in @zctx [:memory-store zen-ns])))
        '#{fhir-r4
           us-core
           hl7.fhir.us.carin-bb
           hl7.fhir.us.Davinci-drug-formulary
           hl7.fhir.us.davinci-pdex
           hl7.fhir.us.davinci-pdex-plan-net
           hl7.fhir.us.dme-orders
           hl7.fhir.us.mcode
           hl7.fhir.uv.sdc})

  (t/is (empty? (:errors @zctx)))

  (t/is (every? #(not (contains? (:ns @zctx) %))
                ['us-core.us-core-patient
                 'fhir-r4.Patient]))

  (-> (zen.core/get-symbol zctx 'fhir-r4/ig)
      :base-schemas
      (->> (zen.core/get-symbol zctx) :schemas)
      (get-in ["Patient" "http://hl7.org/fhir/StructureDefinition/Patient"])
      namespace
      symbol
      (#(zen.core/load-ns zctx (get-in @zctx [:memory-store %]))))

  (-> (zen.core/get-symbol zctx 'us-core/ig)
      :profiles
      (->> (zen.core/get-symbol zctx) :schemas)
      (get-in ["Patient" "http://hl7.org/fhir/us/core/StructureDefinition/us-core-patient"])
      namespace
      symbol
      (#(zen.core/load-ns zctx (get-in @zctx [:memory-store %]))))

  (t/is (empty? (:errors @zctx)))

  (t/is (every? #(contains? (:ns @zctx) %)
                ['us-core.us-core-patient
                 'fhir-r4.Patient]))

  (t/is (empty? (:errors (zen.core/validate zctx '#{fhir-r4.Patient/schema} {}))))

  (def fhir-pat (read-string (slurp (io/resource "zen/fhir/aidbox-fhir-r4-patient-example.edn"))))

  (t/is (empty? (:errors (zen.core/validate zctx '#{fhir-r4.Patient/schema} fhir-pat))))

  (matcho/match
   (:errors (zen.core/validate zctx '#{us-core.us-core-patient/schema} {}))
    [{:path [:name], :type "require"}
     {:path [:identifier], :type "require"}
     {:path [:gender], :type "require"}
     nil]))


(t/deftest nested-extension
  (t/testing "Nested extensions correct namespaces & symbol links are created"
    (t/is (= :done (sut/generate-zen-schemas ztx)))

    (matcho/match
     (-> (:fhir.zen/ns @ztx)
         (select-keys '[plannet.plannet-PractitionerRole
                        plannet.newpatients
                        plannet.plannet-FromNetwork-extension]))
      {'plannet.plannet-PractitionerRole
       {'ns 'plannet.plannet-PractitionerRole
        'import #(contains? % 'plannet.newpatients)

        'schema {:zen.fhir/version zen-fhir-version
                 :keys {:newpatients {:every {:confirms #{'plannet.newpatients/schema}}}}}}

       'plannet.newpatients
       {'ns 'plannet.newpatients
        'import #(contains? % 'plannet.plannet-FromNetwork-extension)

        'schema {:require #{:acceptingPatients}
                 :zen.fhir/version zen-fhir-version
                 :keys {:acceptingPatients {:confirms #{'fhir-r4.CodeableConcept/schema}
                                            :fhir/flags #{:MS}}
                        :fromnetwork {:confirms #{'plannet.plannet-FromNetwork-extension/schema}}}}}

       'plannet.plannet-FromNetwork-extension
       {'ns 'plannet.plannet-FromNetwork-extension

        'schema {:zen/tags          #{'zen/schema 'zen.fhir/structure-schema}
                 :zen/desc          "A reference to a healthcare provider insurance network (plannet-Network) for which the entity is/isn’t accepting new patients. This is a component of the NewPatients extension."
                 :confirms          #{'zen.fhir/Reference}
                 :zen.fhir/version zen-fhir-version
                 :zen.fhir/type       "Reference"
                 :zen.fhir/profileUri "http://hl7.org/test-plannet/StructureDefinition/plannet-FromNetwork-extension"
                 :fhir/flags        #{:MS}}}}))

  (t/testing "Generated zen schemas are correct"
    (def zctx (zen.core/new-context {:package-paths ["zen.fhir"]
                                     :memory-store (:fhir.zen/ns @ztx)}))

    (zen.core/load-ns zctx (get (:memory-store @zctx) 'plannet.plannet-PractitionerRole))

    (t/is (empty? (:errors @zctx)))

    (zen.core/get-symbol zctx 'zen.fhir/version)

    (matcho/match
     (zen.core/validate zctx '#{plannet.plannet-PractitionerRole/schema} {})
     {:errors empty?})

    (matcho/match
     (zen.core/validate
      zctx '#{plannet.plannet-PractitionerRole/schema}
      {:newpatients
       [{:acceptingPatients {:coding [{:code "foo"}]
                             :text "foo"}
         :fromnetwork {:resourceType "Network"
                       :id "some-plannet-network"}}]})
      {:errors empty?})

    (matcho/match
     (zen.core/validate
      zctx '#{plannet.plannet-PractitionerRole/schema}
      {:newpatients
       {:acceptingPatients {:coding [{:code "foo"}]
                            :text "foo"}
        :fromnetwork {:resourceType "Network"
                      :id "some-plannet-network"}}})
     {:errors [{:path [:newpatients]
                :schema ['plannet.plannet-PractitionerRole/schema :newpatients]}
               {:path [:newpatients some?],
                :type "unknown-key"}
               {:path [:newpatients some?],
                :type "unknown-key"}
               nil]})))

(t/deftest base64Binary-schema-test
  (t/testing "base64 correct short input"
    (t/is (empty? (:errors
                   (zen.core/validate
                    zctx
                    #{'fhir-r4.base64Binary/schema}
                    "aGVsbG8gd29ybGQ=")))))

  (t/testing "base64 correct long input"
    (t/is (empty? (:errors
                   (zen.core/validate
                    zctx
                    #{'fhir-r4.base64Binary/schema}
                    "eyAidHlwZSI6ICJGZWF0dXJlIiwgInByb3BlcnRpZXMiOiB7ICJHRU9fSUQiOiAiMDQwMDAwMFVTMDkiLCAiU1RBVEUiOiAiMDkiLCAiTkFNRSI6ICJDb25uZWN0aWN1dCIsICJMU0FEIjogIiIsICJDRU5TVVNBUkVBIjogNDg0Mi4zNTUwMDAgfSwgImdlb21ldHJ5IjogeyAidHlwZSI6ICJNdWx0aVBvbHlnb24iLCAiY29vcmRpbmF0ZXMiOiBbIFsgWyBbIC03MS44NTk1NzAsIDQxLjMyMjM5OSBdLCBbIC03MS44NjgyMzUsIDQxLjMzMDk0MSBdLCBbIC03MS44ODYzMDIsIDQxLjMzNjQxMCBdLCBbIC03MS45MTY3MTAsIDQxLjMzMjIxNyBdLCBbIC03MS45MjIwOTIsIDQxLjMzNDUxOCBdLCBbIC03MS45MjMyODIsIDQxLjMzNTExMyBdLCBbIC03MS45MzYyODQsIDQxLjMzNzk1OSBdLCBbIC03MS45NDU2NTIsIDQxLjMzNzc5OSBdLCBbIC03MS45NTY3NDcsIDQxLjMyOTg3MSBdLCBbIC03MS45NzA5NTUsIDQxLjMyNDUyNiBdLCBbIC03MS45Nzk0NDcsIDQxLjMyOTk4NyBdLCBbIC03MS45ODIxOTQsIDQxLjMyOTg2MSBdLCBbIC03MS45ODgxNTMsIDQxLjMyMDU3NyBdLCBbIC03Mi4wMDAyOTMsIDQxLjMxOTIzMiBdLCBbIC03Mi4wMDUxNDMsIDQxLjMwNjY4NyBdLCBbIC03Mi4wMTA4MzgsIDQxLjMwNzAzMyBdLCBbIC03Mi4wMjE4OTgsIDQxLjMxNjgzOCBdLCBbIC03Mi4wODQ0ODcsIDQxLjMxOTYzNCBdLCBbIC03Mi4wOTQ0NDMsIDQxLjMxNDE2NCBdLCBbIC03Mi4wOTk4MjAsIDQxLjMwNjk5OCBdLCBbIC03Mi4xMTE4MjAsIDQxLjI5OTA5OCBdLCBbIC03Mi4xMzQyMjEsIDQxLjI5OTM5OCBdLCBbIC03Mi4xNjE1ODAsIDQxLjMxMDI2MiBdLCBbIC03Mi4xNzM5MjIsIDQxLjMxNzU5NyBdLCBbIC03Mi4xNzc2MjIsIDQxLjMyMjQ5NyBdLCBbIC03Mi4xODQxMjIsIDQxLjMyMzk5NyBdLCBbIC03Mi4xOTEwMjIsIDQxLjMyMzE5NyBdLCBbIC03Mi4yMDE0MjIsIDQxLjMxNTY5NyBdLCBbIC03Mi4yMDMwMjIsIDQxLjMxMzE5NyBdLCBbIC03Mi4yMDQwMjIsIDQxLjI5OTA5NyBdLCBbIC03Mi4yMDE0MDAsIDQxLjI4ODQ3MCBdLCBbIC03Mi4yMDUxMDksIDQxLjI4NTE4NyBdLCBbIC03Mi4yMDk5OTIsIDQxLjI4NjA2NSBdLCBbIC03Mi4yMTI5MjQsIDQxLjI5MTM2NSBdLCBbIC03Mi4yMjUyNzYsIDQxLjI5OTA0NyBdLCBbIC03Mi4yMzU1MzEsIDQxLjMwMDQxMyBdLCBbIC03Mi4yNDgxNjEsIDQxLjI5OTQ4OCBdLCBbIC03Mi4yNTE4OTUsIDQxLjI5ODYyMCBdLCBbIC03Mi4yNTA1MTUsIDQxLjI5NDM4NiBdLCBbIC03Mi4yNTEzMjMsIDQxLjI4OTk5NyBdLCBbIC03Mi4yNjE0ODcsIDQxLjI4MjkyNiBdLCBbIC03Mi4zMTc3NjAsIDQxLjI3Nzc4MiBdLCBbIC03Mi4zMjc1OTUsIDQxLjI3ODQ2MCBdLCBbIC03Mi4zMzM4OTQsIDQxLjI4MjkxNiBdLCBbIC03Mi4zNDg2NDMsIDQxLjI3NzQ0NiBdLCBbIC03Mi4zNDgwNjgsIDQxLjI2OTY5OCBdLCBbIC03Mi4zODY2MjksIDQxLjI2MTc5OCBdLCBbIC03Mi4zOTg2ODgsIDQxLjI3ODE3MiBdLCBbIC03Mi40MDU5MzAsIDQxLjI3ODM5OCBdLCBbIC03Mi40NTE5MjUsIDQxLjI3ODg4NSBdLCBbIC03Mi40NzI1MzksIDQxLjI3MDEwMyBdLCBbIC03Mi40ODU2OTMsIDQxLjI3MDg4MSBdLCBbIC03Mi40OTk1MzQsIDQxLjI2NTg2NiBdLCBbIC03Mi41MDY2MzQsIDQxLjI2MDA5OSBdLCBbIC03Mi41MTg2NjAsIDQxLjI2MTI1MyBdLCBbIC03Mi41MjEzMTIsIDQxLjI2NTYwMCBdLCBbIC03Mi41Mjk0MTYsIDQxLjI2NDQyMSBdLCBbIC03Mi41MzMyNDcsIDQxLjI2MjY5MCBdLCBbIC03Mi41MzY3NDYsIDQxLjI1NjIwNyBdLCBbIC03Mi41NDcyMzUsIDQxLjI1MDQ5OSBdLCBbIC03Mi41NzExMzYsIDQxLjI2ODA5OCBdLCBbIC03Mi41ODMzMzYsIDQxLjI3MTY5OCBdLCBbIC03Mi41OTgwMzYsIDQxLjI2ODY5OCBdLCBbIC03Mi42MTcyMzcsIDQxLjI3MTk5OCBdLCBbIC03Mi42NDE1MzgsIDQxLjI2Njk5OCBdLCBbIC03Mi42NTM4MzgsIDQxLjI2NTg5NyBdLCBbIC03Mi42NjI4MzgsIDQxLjI2OTE5NyBdLCBbIC03Mi42NzIzMzksIDQxLjI2Njk5NyBdLCBbIC03Mi42ODQ5MzksIDQxLjI1NzU5NyBdLCBbIC03Mi42ODU1MzksIDQxLjI1MTI5NyBdLCBbIC03Mi42OTA0MzksIDQxLjI0NjY5NyBdLCBbIC03Mi42OTQ3NDQsIDQxLjI0NDk3MCBdLCBbIC03Mi43MTA1OTUsIDQxLjI0NDQ4MCBdLCBbIC03Mi43MTM2NzQsIDQxLjI0OTAwNyBdLCBbIC03Mi43MTEyMDgsIDQxLjI1MTAxOCBdLCBbIC03Mi43MTI0NjAsIDQxLjI1NDE2NyBdLCBbIC03Mi43MjI0MzksIDQxLjI1OTEzOCBdLCBbIC03Mi43MzI4MTMsIDQxLjI1NDcyNyBdLCBbIC03Mi43NTQ0NDQsIDQxLjI2NjkxMyBdLCBbIC03Mi43NTc0NzcsIDQxLjI2NjkxMyBdLCBbIC03Mi43ODYxNDIsIDQxLjI2NDc5NiBdLCBbIC03Mi44MTg3MzcsIDQxLjI1MjI0NCBdLCBbIC03Mi44MTkzNzIsIDQxLjI1NDA2MSBdLCBbIC03Mi44MjY4ODMsIDQxLjI1Njc1NSBdLCBbIC03Mi44NDc3NjcsIDQxLjI1NjY5MCBdLCBbIC03Mi44NTAyMTAsIDQxLjI1NTU0NCBdLCBbIC03Mi44NTQwNTUsIDQxLjI0Nzc0MCBdLCBbIC03Mi44NjEzNDQsIDQxLjI0NTI5NyBdLCBbIC03Mi44ODE0NDUsIDQxLjI0MjU5NyBdLCBbIC03Mi44OTU0NDUsIDQxLjI0MzY5NyBdLCBbIC03Mi45MDQzNDUsIDQxLjI0NzI5NyBdLCBbIC03Mi45MDUyNDUsIDQxLjI0ODI5NyBdLCBbIC03Mi45MDMwNDUsIDQxLjI1Mjc5NyBdLCBbIC03Mi44OTQ3NDUsIDQxLjI1NjE5NyBdLCBbIC03Mi44OTM4NDUsIDQxLjI1OTg5NyBdLCBbIC03Mi45MDgyMDAsIDQxLjI4MjkzMiBdLCBbIC03Mi45MTY4MjcsIDQxLjI4MjAzMyBdLCBbIC03Mi45MjAwNjIsIDQxLjI4MDA1NiBdLCBbIC03Mi45MjA4NDYsIDQxLjI2ODg5NyBdLCBbIC03Mi45MzU2NDYsIDQxLjI1ODQ5NyBdLCBbIC03Mi45NjIwNDcsIDQxLjI1MTU5NyBdLCBbIC03Mi45ODYyNDcsIDQxLjIzMzQ5NyBdLCBbIC03Mi45OTc5NDgsIDQxLjIyMjY5NyBdLCBbIC03My4wMDc1NDgsIDQxLjIxMDE5NyBdLCBbIC03My4wMTQ5NDgsIDQxLjIwNDI5NyBdLCBbIC03My4wMjAxNDksIDQxLjIwNDA5NyBdLCBbIC03My4wMjA0NDksIDQxLjIwNjM5NyBdLCBbIC03My4wMjI1NDksIDQxLjIwNzE5NyBdLCBbIC03My4wNTA2NTAsIDQxLjIxMDE5NyBdLCBbIC03My4wNTkzNTAsIDQxLjIwNjY5NyBdLCBbIC03My4wNzk0NTAsIDQxLjE5NDAxNSBdLCBbIC03My4xMDU0OTMsIDQxLjE3MjE5NCBdLCBbIC03My4xMDc5ODcsIDQxLjE2ODczOCBdLCBbIC03My4xMTAzNTIsIDQxLjE1OTY5NyBdLCBbIC03My4xMDk5NTIsIDQxLjE1Njk5NyBdLCBbIC03My4xMDgzNTIsIDQxLjE1MzcxOCBdLCBbIC03My4xMTEwNTIsIDQxLjE1MDc5NyBdLCBbIC03My4xMzAyNTMsIDQxLjE0Njc5NyBdLCBbIC03My4xNzAwNzQsIDQxLjE2MDUzMiBdLCBbIC03My4xNzA3MDEsIDQxLjE2NDk0NSBdLCBbIC03My4xNzc3NzQsIDQxLjE2NjY5NyBdLCBbIC03My4yMDI2NTYsIDQxLjE1ODA5NiBdLCBbIC03My4yMjgyOTUsIDQxLjE0MjYwMiBdLCBbIC03My4yMzUwNTgsIDQxLjE0Mzk5NiBdLCBbIC03My4yNDc5NTgsIDQxLjEyNjM5NiBdLCBbIC03My4yNjIzNTgsIDQxLjExNzQ5NiBdLCBbIC03My4yODY3NTksIDQxLjEyNzg5NiBdLCBbIC03My4yOTYzNTksIDQxLjEyNTY5NiBdLCBbIC03My4zMTE4NjAsIDQxLjExNjI5NiBdLCBbIC03My4zMzA2NjAsIDQxLjEwOTk5NiBdLCBbIC03My4zNzIyOTYsIDQxLjEwNDAyMCBdLCBbIC03My4zOTIxNjIsIDQxLjA4NzY5NiBdLCBbIC03My40MDAxNTQsIDQxLjA4NjI5OSBdLCBbIC03My40MTM2NzAsIDQxLjA3MzI1OCBdLCBbIC03My40MzUwNjMsIDQxLjA1NjY5NiBdLCBbIC03My40NTAzNjQsIDQxLjA1NzA5NiBdLCBbIC03My40NjgyMzksIDQxLjA1MTM0NyBdLCBbIC03My40NzczNjQsIDQxLjAzNTk5NyBdLCBbIC03My40OTMzMjcsIDQxLjA0ODE3MyBdLCBbIC03My41MTY5MDMsIDQxLjAzODczOCBdLCBbIC03My41MTY3NjYsIDQxLjAyOTQ5NyBdLCBbIC03My41MjI2NjYsIDQxLjAxOTI5NyBdLCBbIC03My41Mjg4NjYsIDQxLjAxNjM5NyBdLCBbIC03My41MzExNjksIDQxLjAyMTkxOSBdLCBbIC03My41MzAxODksIDQxLjAyODc3NiBdLCBbIC03My41MzI3ODYsIDQxLjAzMTY3MCBdLCBbIC03My41MzUzMzgsIDQxLjAzMTkyMCBdLCBbIC03My41NTE0OTQsIDQxLjAyNDMzNiBdLCBbIC03My41NjE5NjgsIDQxLjAxNjc5NyBdLCBbIC03My41Njc2NjgsIDQxLjAxMDg5NyBdLCBbIC03My41NzAwNjgsIDQxLjAwMTU5NyBdLCBbIC03My41ODM5NjgsIDQxLjAwMDg5NyBdLCBbIC03My41ODQ5ODgsIDQxLjAxMDUzNyBdLCBbIC03My41OTU2OTksIDQxLjAxNTk5NSBdLCBbIC03My42MDM5NTIsIDQxLjAxNTA1NCBdLCBbIC03My42NDM0NzgsIDQxLjAwMjE3MSBdLCBbIC03My42NTExNzUsIDQwLjk5NTIyOSBdLCBbIC03My42NTczMzYsIDQwLjk4NTE3MSBdLCBbIC03My42NTk2NzEsIDQwLjk4NzkwOSBdLCBbIC03My42NTg3NzIsIDQwLjk5MzQ5NyBdLCBbIC03My42NTkzNzIsIDQwLjk5OTQ5NyBdLCBbIC03My42NTU1NzEsIDQxLjAwNzY5NyBdLCBbIC03My42NTQ2NzEsIDQxLjAxMTY5NyBdLCBbIC03My42NTUzNzEsIDQxLjAxMjc5NyBdLCBbIC03My42NjI2NzIsIDQxLjAyMDQ5NyBdLCBbIC03My42NzA0NzIsIDQxLjAzMDA5NyBdLCBbIC03My42Nzk5NzMsIDQxLjA0MTc5NyBdLCBbIC03My42ODcxNzMsIDQxLjA1MDY5NyBdLCBbIC03My42OTQyNzMsIDQxLjA1OTI5NiBdLCBbIC03My43MTY4NzUsIDQxLjA4NzU5NiBdLCBbIC03My43MjI1NzUsIDQxLjA5MzU5NiBdLCBbIC03My43Mjc3NzUsIDQxLjEwMDY5NiBdLCBbIC03My42Mzk2NzIsIDQxLjE0MTQ5NSBdLCBbIC03My42MzIxNTMsIDQxLjE0NDkyMSBdLCBbIC03My41NjQ5NDEsIDQxLjE3NTE3MCBdLCBbIC03My41MTQ2MTcsIDQxLjE5ODQzNCBdLCBbIC03My41MDk0ODcsIDQxLjIwMDgxNCBdLCBbIC03My40ODI3MDksIDQxLjIxMjc2MCBdLCBbIC03My41MTgzODQsIDQxLjI1NjcxOSBdLCBbIC03My41NTA5NjEsIDQxLjI5NTQyMiBdLCBbIC03My41NDg5MjksIDQxLjMwNzU5OCBdLCBbIC03My41NDk1NzQsIDQxLjMxNTkzMSBdLCBbIC03My41NDg5NzMsIDQxLjMyNjI5NyBdLCBbIC03My41NDQ3MjgsIDQxLjM2NjM3NSBdLCBbIC03My41NDM0MjUsIDQxLjM3NjYyMiBdLCBbIC03My41NDExNjksIDQxLjQwNTk5NCBdLCBbIC03My41Mzc2NzMsIDQxLjQzMzkwNSBdLCBbIC03My41Mzc0NjksIDQxLjQzNTg5MCBdLCBbIC03My41MzY5NjksIDQxLjQ0MTA5NCBdLCBbIC03My41MzYwNjcsIDQxLjQ1MTMzMSBdLCBbIC03My41MzU5ODYsIDQxLjQ1MzA2MCBdLCBbIC03My41MzU4ODUsIDQxLjQ1NTIzNiBdLCBbIC03My41MzU4NTcsIDQxLjQ1NTcwOSBdLCBbIC03My41MzU3NjksIDQxLjQ1NzE1OSBdLCBbIC03My41MzQzNjksIDQxLjQ3NTg5NCBdLCBbIC03My41MzQyNjksIDQxLjQ3NjM5NCBdLCBbIC03My41MzQyNjksIDQxLjQ3NjkxMSBdLCBbIC03My41MzQxNTAsIDQxLjQ3ODA2MCBdLCBbIC03My41MzQwNTUsIDQxLjQ3ODk2OCBdLCBbIC03My41MzM5NjksIDQxLjQ3OTY5MyBdLCBbIC03My41MzAwNjcsIDQxLjUyNzE5NCBdLCBbIC03My41MjEwNDEsIDQxLjYxOTc3MyBdLCBbIC03My41MjAwMTcsIDQxLjY0MTE5NyBdLCBbIC03My41MTY3ODUsIDQxLjY4NzU4MSBdLCBbIC03My41MTE5MjEsIDQxLjc0MDk0MSBdLCBbIC03My41MTA5NjEsIDQxLjc1ODc0OSBdLCBbIC03My41MDUwMDgsIDQxLjgyMzc3MyBdLCBbIC03My41MDQ5NDQsIDQxLjgyNDI4NSBdLCBbIC03My41MDE5ODQsIDQxLjg1ODcxNyBdLCBbIC03My40OTgzMDQsIDQxLjg5MjUwOCBdLCBbIC03My40OTY1MjcsIDQxLjkyMjM4MCBdLCBbIC03My40OTI5NzUsIDQxLjk1ODUyNCBdLCBbIC03My40ODk2MTUsIDQyLjAwMDA5MiBdLCBbIC03My40ODczMTQsIDQyLjA0OTYzOCBdLCBbIC03My40MzI4MTIsIDQyLjA1MDU4NyBdLCBbIC03My4yOTQ0MjAsIDQyLjA0Njk4NCBdLCBbIC03My4yOTMwOTcsIDQyLjA0Njk0MCBdLCBbIC03My4yMzEwNTYsIDQyLjA0NDk0NSBdLCBbIC03My4yMjk3OTgsIDQyLjA0NDg3NyBdLCBbIC03My4wNTMyNTQsIDQyLjAzOTg2MSBdLCBbIC03Mi45OTk1NDksIDQyLjAzODY1MyBdLCBbIC03Mi44NjM3MzMsIDQyLjAzNzcxMCBdLCBbIC03Mi44NjM2MTksIDQyLjAzNzcwOSBdLCBbIC03Mi44NDcxNDIsIDQyLjAzNjg5NCBdLCBbIC03Mi44MTM1NDEsIDQyLjAzNjQ5NCBdLCBbIC03Mi44MTY3NDEsIDQxLjk5NzU5NSBdLCBbIC03Mi43NjY3MzksIDQyLjAwMjk5NSBdLCBbIC03Mi43NjYxMzksIDQyLjAwNzY5NSBdLCBbIC03Mi43NjMyNjUsIDQyLjAwOTc0MiBdLCBbIC03Mi43NjMyMzgsIDQyLjAxMjc5NSBdLCBbIC03Mi43NjEyMzgsIDQyLjAxNDU5NSBdLCBbIC03Mi43NTk3MzgsIDQyLjAxNjk5NSBdLCBbIC03Mi43NjEzNTQsIDQyLjAxODE4MyBdLCBbIC03Mi43NjIzMTAsIDQyLjAxOTc3NSBdLCBbIC03Mi43NjIxNTEsIDQyLjAyMTUyNyBdLCBbIC03Mi43NjA1NTgsIDQyLjAyMTg0NiBdLCBbIC03Mi43NTgxNTEsIDQyLjAyMDg2NSBdLCBbIC03Mi43NTc0NjcsIDQyLjAyMDk0NyBdLCBbIC03Mi43NTQwMzgsIDQyLjAyNTM5NSBdLCBbIC03Mi43NTE3MzgsIDQyLjAzMDE5NSBdLCBbIC03Mi43NTM1MzgsIDQyLjAzMjA5NSBdLCBbIC03Mi43NTc1MzgsIDQyLjAzMzI5NSBdLCBbIC03Mi43NTU4MzgsIDQyLjAzNjE5NSBdLCBbIC03Mi42OTU5MjcsIDQyLjAzNjc4OCBdLCBbIC03Mi42NDMxMzQsIDQyLjAzMjM5NSBdLCBbIC03Mi42MDc5MzMsIDQyLjAzMDc5NSBdLCBbIC03Mi42MDY5MzMsIDQyLjAyNDk5NSBdLCBbIC03Mi41OTAyMzMsIDQyLjAyNDY5NSBdLCBbIC03Mi41ODIzMzIsIDQyLjAyNDY5NSBdLCBbIC03Mi41NzMyMzEsIDQyLjAzMDE0MSBdLCBbIC03Mi41MjgxMzEsIDQyLjAzNDI5NSBdLCBbIC03Mi40NTY2ODAsIDQyLjAzMzk5OSBdLCBbIC03Mi4zMTcxNDgsIDQyLjAzMTkwNyBdLCBbIC03Mi4yNDk1MjMsIDQyLjAzMTYyNiBdLCBbIC03Mi4xMzU2ODcsIDQyLjAzMDI0NSBdLCBbIC03Mi4wNjM0OTYsIDQyLjAyNzM0NyBdLCBbIC03MS45ODczMjYsIDQyLjAyNjg4MCBdLCBbIC03MS44OTA3ODAsIDQyLjAyNDM2OCBdLCBbIC03MS44MDA2NTAsIDQyLjAyMzU2OSBdLCBbIC03MS43OTkyNDIsIDQyLjAwODA2NSBdLCBbIC03MS43OTc5MjIsIDQxLjkzNTM5NSBdLCBbIC03MS43OTQxNjEsIDQxLjg0MTEwMSBdLCBbIC03MS43OTQxNjEsIDQxLjg0MDE0MSBdLCBbIC03MS43OTI3ODYsIDQxLjgwODY3MCBdLCBbIC03MS43OTI3NjcsIDQxLjgwNzAwMSBdLCBbIC03MS43OTEwNjIsIDQxLjc3MDI3MyBdLCBbIC03MS43ODk2NzgsIDQxLjcyNDczNCBdLCBbIC03MS43ODY5OTQsIDQxLjY1NTk5MiBdLCBbIC03MS43ODkzNTYsIDQxLjU5NjkxMCBdLCBbIC03MS43OTc2ODMsIDQxLjQxNjcwOSBdLCBbIC03MS44MTgzOTAsIDQxLjQxOTU5OSBdLCBbIC03MS44Mzk2NDksIDQxLjQxMjExOSBdLCBbIC03MS44NDI1NjMsIDQxLjQwOTg1NSBdLCBbIC03MS44NDM0NzIsIDQxLjQwNTgzMCBdLCBbIC03MS44NDIxMzEsIDQxLjM5NTM1OSBdLCBbIC03MS44MzM0NDMsIDQxLjM4NDUyNCBdLCBbIC03MS44MzE2MTMsIDQxLjM3MDg5OSBdLCBbIC03MS44Mzc3MzgsIDQxLjM2MzUyOSBdLCBbIC03MS44MzU5NTEsIDQxLjM1MzkzNSBdLCBbIC03MS44Mjk1OTUsIDQxLjM0NDU0NCBdLCBbIC03MS44MzkwMTMsIDQxLjMzNDA0MiBdLCBbIC03MS44NjA1MTMsIDQxLjMyMDI0OCBdLCBbIC03MS44NTk1NzAsIDQxLjMyMjM5OSBdIF0gXSwgWyBbIFsgLTczLjQyMjE2NSwgNDEuMDQ3NTYyIF0sIFsgLTczLjQwMzYxMCwgNDEuMDYyNjg3IF0sIFsgLTczLjM2Nzg1OSwgNDEuMDg4MTIwIF0sIFsgLTczLjM1MjA1MSwgNDEuMDg4MTIwIF0sIFsgLTczLjM4NTczNSwgNDEuMDU5MjUwIF0sIFsgLTczLjQyMjE2NSwgNDEuMDQ3NTYyIF0gXSBdIF0gfSB9"
                    )))))

  (t/testing "base64 incorrect short input"
    (t/is (= 1 (count (:errors
                       (zen.core/validate
                        zctx
                        #{'fhir-r4.base64Binary/schema}
                        "aGVsbG8gd29ybGQ-="))))))

  (t/testing "base64 incorrect long input"
    (t/is (= 1 (count (:errors
                       (zen.core/validate
                        zctx
                        #{'fhir-r4.base64Binary/schema}
                        "eyAidHlwZSI6ICJGZWF0dXJlIiwgInByb3BlcnRpZXMiOiB7ICJHRU9fSUQiOiAiMDQwMDAwMFVTMDkiLCAiU1RBVEUiOiAiMDkiLCAiTkFNRSI6ICJDb25uZWN0aWN1dCIsICJMU0FEIjogIiIsICJDRU5TVVNBUkVBIjogNDg0Mi4zNTUwMDAgfSwgImdlb21ldHJ5IjogeyAidHlwZSI6ICJNdWx0aVBvbHlnb24iLCAiY29vcmRpbmF0ZXMiOiBbIFsgWyBbIC03MS44NTk1NzAsIDQxLjMyMjM5OSBdLCBbIC03MS44NjgyMzUsIDQxLjMzMDk0MSBdLCBbIC03MS44ODYzMDIsIDQxLjMzNjQxMCBdLCBbIC03MS45MTY3MTAsIDQxLjMzMjIxNyBdLCBbIC03MS45MjIwOTIsIDQxLjMzNDUxOCBdLCBbIC03MS45MjMyODIsIDQxLjMzNTExMyBdLCBbIC03MS45MzYyODQsIDQxLjMzNzk1OSBdLCBbIC03MS45NDU2NTIsIDQxLjMzNzc5OSBdLCBbIC03MS45NTY3NDcsIDQxLjMyOTg3MSBdLCBbIC03MS45NzA5NTUsIDQxLjMyNDUyNiBdLCBbIC03MS45Nzk0NDcsIDQxLjMyOTk4NyBdLCBbIC03MS45ODIxOTQsIDQxLjMyOTg2MSBdLCBbIC03MS45ODgxNTMsIDQxLjMyMDU3NyBdLCBbIC03Mi4wMDAyOTMsIDQxLjMxOTIzMiBdLCBbIC03Mi4wMDUxNDMsIDQxLjMwNjY4NyBdLCBbIC03Mi4wMTA4MzgsIDQxLjMwNzAzMyBdLCBbIC03Mi4wMjE4OTgsIDQxLjMxNjgzOCBdLCBbIC03Mi4wODQ0ODcsIDQxLjMxOTYzNCBdLCBbIC03Mi4wOTQ0NDMsIDQxLjMxNDE2NCBdLCBbIC03Mi4wOTk4MjAsIDQxLjMwNjk5OCBdLCBbIC03Mi4xMTE4MjAsIDQxLjI5OTA5OCBdLCBbIC03Mi4xMzQyMjEsIDQxLjI5OTM5OCBdLCBbIC03Mi4xNjE1ODAsIDQxLjMxMDI2MiBdLCBbIC03Mi4xNzM5MjIsIDQxLjMxNzU5NyBdLCBbIC03Mi4xNzc2MjIsIDQxLjMyMjQ5NyBdLCBbIC03Mi4xODQxMjIsIDQxLjMyMzk5NyBdLCBbIC03Mi4xOTEwMjIsIDQxLjMyMzE5NyBdLCBbIC03Mi4yMDE0MjIsIDQxLjMxNTY5NyBdLCBbIC03Mi4yMDMwMjIsIDQxLjMxMzE5NyBdLCBbIC03Mi4yMDQwMjIsIDQxLjI5OTA5NyBdLCBbIC03Mi4yMDE0MDAsIDQxLjI4ODQ3MCBdLCBbIC03Mi4yMDUxMDksIDQxLjI4NTE4NyBdLCBbIC03Mi4yMDk5OTIsIDQxLjI4NjA2NSBdLCBbIC03Mi4yMTI5MjQsIDQxLjI5MTM2NSBdLCBbIC03Mi4yMjUyNzYsIDQxLjI5OTA0NyBdLCBbIC03Mi4yMzU1MzEsIDQxLjMwMDQxMyBdLCBbIC03Mi4yNDgxNjEsIDQxLjI5OTQ4OCBdLCBbIC03Mi4yNTE4OTUsIDQxLjI5ODYyMCBdLCBbIC03Mi4yNTA1MTUsIDQxLjI5NDM4NiBdLCBbIC03Mi4yNTEzMjMsIDQxLjI4OTk5NyBdLCBbIC03Mi4yNjE0ODcsIDQxLjI4MjkyNiBdLCBbIC03Mi4zMTc3NjAsIDQxLjI3Nzc4MiBdLCBbIC03Mi4zMjc1OTUsIDQxLjI3ODQ2MCBdLCBbIC03Mi4zMzM4OTQsIDQxLjI4MjkxNiBdLCBbIC03Mi4zNDg2NDMsIDQxLjI3NzQ0NiBdLCBbIC03Mi4zNDgwNjgsIDQxLjI2OTY5OCBdLCBbIC03Mi4zODY2MjksIDQxLjI2MTc5OCBdLCBbIC03Mi4zOTg2ODgsIDQxLjI3ODE3MiBdLCBbIC03Mi40MDU5MzAsIDQxLjI3ODM5OCBdLCBbIC03Mi40NTE5MjUsIDQxLjI3ODg4NSBdLCBbIC03Mi40NzI1MzksIDQxLjI3MDEwMyBdLCBbIC03Mi40ODU2OTMsIDQxLjI3MDg4MSBdLCBbIC03Mi40OTk1MzQsIDQxLjI2NTg2NiBdLCBbIC03Mi41MDY2MzQsIDQxLjI2MDA5OSBdLCBbIC03Mi41MTg2NjAsIDQxLjI2MTI1MyBdLCBbIC03Mi41MjEzMTIsIDQxLjI2NTYwMCBdLCBbIC03Mi41Mjk0MTYsIDQxLjI2NDQyMSBdLCBbIC03Mi41MzMyNDcsIDQxLjI2MjY5MCBdLCBbIC03Mi41MzY3NDYsIDQxLjI1NjIwNyBdLCBbIC03Mi41NDcyMzUsIDQxLjI1MDQ5OSBdLCBbIC03Mi41NzExMzYsIDQxLjI2ODA5OCBdLCBbIC03Mi41ODMzMzYsIDQxLjI3MTY5OCBdLCBbIC03Mi41OTgwMzYsIDQxLjI2ODY5OCBdLCBbIC03Mi42MTcyMzcsIDQxLjI3MTk5OCBdLCBbIC03Mi42NDE1MzgsIDQxLjI2Njk5OCBdLCBbIC03Mi42NTM4MzgsIDQxLjI2NTg5NyBdLCBbIC03Mi42NjI4MzgsIDQxLjI2OTE5NyBdLCBbIC03Mi42NzIzMzksIDQxLjI2Njk5NyBdLCBbIC03Mi42ODQ5MzksIDQxLjI1NzU5NyBdLCBbIC03Mi42ODU1MzksIDQxLjI1MTI5NyBdLCBbIC03Mi42OTA0MzksIDQxLjI0NjY5NyBdLCBbIC03Mi42OTQ3NDQsIDQxLjI0NDk3MCBdLCBbIC03Mi43MTA1OTUsIDQxLjI0NDQ4MCBdLCBbIC03Mi43MTM2NzQsIDQxLjI0OTAwNyBdLCBbIC03Mi43MTEyMDgsIDQxLjI1MTAxOCBdLCBbIC03Mi43MTI0NjAsIDQxLjI1NDE2NyBdLCBbIC03Mi43MjI0MzksIDQxLjI1OTEzOCBdLCBbIC03Mi43MzI4MTMsIDQxLjI1NDcyNyBdLCBbIC03Mi43NTQ0NDQsIDQxLjI2NjkxMyBdLCBbIC03Mi43NTc0NzcsIDQxLjI2NjkxMyBdLCBbIC03Mi43ODYxNDIsIDQxLjI2NDc5NiBdLCBbIC03Mi44MTg3MzcsIDQxLjI1MjI0NCBdLCBbIC03Mi44MTkzNzIsIDQxLjI1NDA2MSBdLCBbIC03Mi44MjY4ODMsIDQxLjI1Njc1NSBdLCBbIC03Mi44NDc3NjcsIDQxLjI1NjY5MCBdLCBbIC03Mi44NTAyMTAsIDQxLjI1NTU0NCBdLCBbIC03Mi44NTQwNTUsIDQxLjI0Nzc0MCBdLCBbIC03Mi44NjEzNDQsIDQxLjI0NTI5NyBdLCBbIC03Mi44ODE0NDUsIDQxLjI0MjU5NyBdLCBbIC03Mi44OTU0NDUsIDQxLjI0MzY5NyBdLCBbIC03Mi45MDQzNDUsIDQxLjI0NzI5NyBdLCBbIC03Mi45MDUyNDUsIDQxLjI0ODI5NyBdLCBbIC03Mi45MDMwNDUsIDQxLjI1Mjc5NyBdLCBbIC03Mi44OTQ3NDUsIDQxLjI1NjE5NyBdLCBbIC03Mi44OTM4NDUsIDQxLjI1OTg5NyBdLCBbIC03Mi45MDgyMDAsIDQxLjI4MjkzMiBdLCBbIC03Mi45MTY4MjcsIDQxLjI4MjAzMyBdLCBbIC03Mi45MjAwNjIsIDQxLjI4MDA1NiBdLCBbIC03Mi45MjA4NDYsIDQxLjI2ODg5NyBdLCBbIC03Mi45MzU2NDYsIDQxLjI1ODQ5NyBdLCBbIC03Mi45NjIwNDcsIDQxLjI1MTU5NyBdLCBbIC03Mi45ODYyNDcsIDQxLjIzMzQ5NyBdLCBbIC03Mi45OTc5NDgsIDQxLjIyMjY5NyBdLCBbIC03My4wMDc1NDgsIDQxLjIxMDE5NyBdLCBbIC03My4wMTQ5NDgsIDQxLjIwNDI5NyBdLCBbIC03My4wMjAxNDksIDQxLjIwNDA5NyBdLCBbIC03My4wMjA0NDksIDQxLjIwNjM5NyBdLCBbIC03My4wMjI1NDksIDQxLjIwNzE5NyBdLCBbIC03My4wNTA2NTAsIDQxLjIxMDE5NyBdLCBbIC03My4wNTkzNTAsIDQxLjIwNjY5NyBdLCBbIC03My4wNzk0NTAsIDQxLjE5NDAxNSBdLCBbIC03My4xMDU0OTMsIDQxLjE3MjE5NCBdLCBbIC03My4xMDc5ODcsIDQxLjE2ODczOCBdLCBbIC03My4xMTAzNTIsIDQxLjE1OTY5NyBdLCBbIC03My4xMDk5NTIsIDQxLjE1Njk5NyBdLCBbIC03My4xMDgzNTIsIDQxLjE1MzcxOCBdLCBbIC03My4xMTEwNTIsIDQxLjE1MDc5NyBdLCBbIC03My4xMzAyNTMsIDQxLjE0Njc5NyBdLCBbIC03My4xNzAwNzQsIDQxLjE2MDUzMiBdLCBbIC03My4xNzA3MDEsIDQxLjE2NDk0NSBdLCBbIC03My4xNzc3NzQsIDQxLjE2NjY5NyBdLCBbIC03My4yMDI2NTYsIDQxLjE1ODA5NiBdLCBbIC03My4yMjgyOTUsIDQxLjE0MjYwMiBdLCBbIC03My4yMzUwNTgsIDQxLjE0Mzk5NiBdLCBbIC03My4yNDc5NTgsIDQxLjEyNjM5NiBdLCBbIC03My4yNjIzNTgsIDQxLjExNzQ5NiBdLCBbIC03My4yODY3NTksIDQxLjEyNzg5NiBdLCBbIC03My4yOTYzNTksIDQxLjEyNTY5NiBdLCBbIC03My4zMTE4NjAsIDQxLjExNjI5NiBdLCBbIC03My4zMzA2NjAsIDQxLjEwOTk5NiBdLCBbIC03My4zNzIyOTYsIDQxLjEwNDAyMCBdLCBbIC03My4zOTIxNjIsIDQxLjA4NzY5NiBdLCBbIC03My40MDAxNTQsIDQxLjA4NjI5OSBdLCBbIC03My40MTM2NzAsIDQxLjA3MzI1OCBdLCBbIC03My40MzUwNjMsIDQxLjA1NjY5NiBdLCBbIC03My40NTAzNjQsIDQxLjA1NzA5NiBdLCBbIC03My40NjgyMzksIDQxLjA1MTM0NyBdLCBbIC03My40NzczNjQsIDQxLjAzNTk5NyBdLCBbIC03My40OTMzMjcsIDQxLjA0ODE3MyBdLCBbIC03My41MTY5MDMsIDQxLjAzODczOCBdLCBbIC03My41MTY3NjYsIDQxLjAyOTQ5NyBdLCBbIC03My41MjI2NjYsIDQxLjAxOTI5NyBdLCBbIC03My41Mjg4NjYsIDQxLjAxNjM5NyBdLCBbIC03My41MzExNjksIDQxLjAyMTkxOSBdLCBbIC03My41MzAxODksIDQxLjAyODc3NiBdLCBbIC03My41MzI3ODYsIDQxLjAzMTY3MCBdLCBbIC03My41MzUzMzgsIDQxLjAzMTkyMCBdLCBbIC03My41NTE0OTQsIDQxLjAyNDMzNiBdLCBbIC03My41NjE5NjgsIDQxLjAxNjc5NyBdLCBbIC03My41Njc2NjgsIDQxLjAxMDg5NyBdLCBbIC03My41NzAwNjgsIDQxLjAwMTU5NyBdLCBbIC03My41ODM5NjgsIDQxLjAwMDg5NyBdLCBbIC03My41ODQ5ODgsIDQxLjAxMDUzNyBdLCBbIC03My41OTU2OTksIDQxLjAxNTk5NSBdLCBbIC03My42MDM5NTIsIDQxLjAxNTA1NCBdLCBbIC03My42NDM0NzgsIDQxLjAwMjE3MSBdLCBbIC03My42NTExNzUsIDQwLjk5NTIyOSBdLCBbIC03My42NTczMzYsIDQwLjk4NTE3MSBdLCBbIC03My42NTk2NzEsIDQwLjk4NzkwOSBdLCBbIC03My42NTg3NzIsIDQwLjk5MzQ5NyBdLCBbIC03My42NTkzNzIsIDQwLjk5OTQ5NyBdLCBbIC03My42NTU1NzEsIDQxLjAwNzY5NyBdLCBbIC03My42NTQ2NzEsIDQxLjAxMTY5NyBdLCBbIC03My42NTUzNzEsIDQxLjAxMjc5NyBdLCBbIC03My42NjI2NzIsIDQxLjAyMDQ5NyBdLCBbIC03My42NzA0NzIsIDQxLjAzMDA5NyBdLCBbIC03My42Nzk5NzMsIDQxLjA0MTc5NyBdLCBbIC03My42ODcxNzMsIDQxLjA1MDY5NyBdLCBbIC03My42OTQyNzMsIDQxLjA1OTI5NiBdLCBbIC03My43MTY4NzUsIDQxLjA4NzU5NiBdLCBbIC03My43MjI1NzUsIDQxLjA5MzU5NiBdLCBbIC03My43Mjc3NzUsIDQxLjEwMDY5NiBdLCBbIC03My42Mzk2NzIsIDQxLjE0MTQ5NSBdLCBbIC03My42MzIxNTMsIDQxLjE0NDkyMSBdLCBbIC03My41NjQ5NDEsIDQxLjE3NTE3MCBdLCBbIC03My41MTQ2MTcsIDQxLjE5ODQzNCBdLCBbIC03My41MDk0ODcsIDQxLjIwMDgxNCBdLCBbIC03My40ODI3MDksIDQxLjIxMjc2MCBdLCBbIC03My41MTgzODQsIDQxLjI1NjcxOSBdLCBbIC03My41NTA5NjEsIDQxLjI5NTQyMiBdLCBbIC03My41NDg5MjksIDQxLjMwNzU5OCBdLCBbIC03My41NDk1NzQsIDQxLjMxNTkzMSBdLCBbIC03My41NDg5NzMsIDQxLjMyNjI5NyBdLCBbIC03My41NDQ3MjgsIDQxLjM2NjM3NSBdLCBbIC03My41NDM0MjUsIDQxLjM3NjYyMiBdLCBbIC03My41NDExNjksIDQxLjQwNTk5NCBdLCBbIC03My41Mzc2NzMsIDQxLjQzMzkwNSBdLCBbIC03My41Mzc0NjksIDQxLjQzNTg5MCBdLCBbIC03My41MzY5NjksIDQxLjQ0MTA5NCBdLCBbIC03My41MzYwNjcsIDQxLjQ1MTMzMSBdLCBbIC03My41MzU5ODYsIDQxLjQ1MzA2MCBdLCBbIC03My41MzU4ODUsIDQxLjQ1NTIzNiBdLCBbIC03My41MzU4NTcsIDQxLjQ1NTcwOSBdLCBbIC03My41MzU3NjksIDQxLjQ1NzE1OSBdLCBbIC03My41MzQzNjksIDQxLjQ3NTg5NCBdLCBbIC03My41MzQyNjksIDQxLjQ3NjM5NCBdLCBbIC03My41MzQyNjksIDQxLjQ3NjkxMSBdLCBbIC03My41MzQxNTAsIDQxLjQ3ODA2MCBdLCBbIC03My41MzQwNTUsIDQxLjQ3ODk2OCBdLCBbIC03My41MzM5NjksIDQxLjQ3OTY5MyBdLCBbIC03My41MzAwNjcsIDQxLjUyNzE5NCBdLCBbIC03My41MjEwNDEsIDQxLjYxOTc3MyBdLCBbIC03My41MjAwMTcsIDQxLjY0MTE5NyBdLCBbIC03My41MTY3ODUsIDQxLjY4NzU4MSBdLCBbIC03My41MTE5MjEsIDQxLjc0MDk0MSBdLCBbIC03My41MTA5NjEsIDQxLjc1ODc0OSBdLCBbIC03My41MDUwMDgsIDQxLjgyMzc3MyBdLCBbIC03My41MDQ5NDQsIDQxLjgyNDI4NSBdLCBbIC03My41MDE5ODQsIDQxLjg1ODcxNyBdLCBbIC03My40OTgzMDQsIDQxLjg5MjUwOCBdLCBbIC03My40OTY1MjcsIDQxLjkyMjM4MCBdLCBbIC03My40OTI5NzUsIDQxLjk1ODUyNCBdLCBbIC03My40ODk2MTUsIDQyLjAwMDA5MiBdLCBbIC03My40ODczMTQsIDQyLjA0OTYzOCBdLCBbIC03My40MzI4MTIsIDQyLjA1MDU4NyBdLCBbIC03My4yOTQ0MjAsIDQyLjA0Njk4NCBdLCBbIC03My4yOTMwOTcsIDQyLjA0Njk0MCBdLCBbIC03My4yMzEwNTYsIDQyLjA0NDk0NSBdLCBbIC03My4yMjk3OTgsIDQyLjA0NDg3NyBdLCBbIC03My4wNTMyNTQsIDQyLjAzOTg2MSBdLCBbIC03Mi45OTk1NDksIDQyLjAzODY1MyBdLCBbIC03Mi44NjM3MzMsIDQyLjAzNzcxMCBdLCBbIC03Mi44NjM2MTksIDQyLjAzNzcwOSBdLCBbIC03Mi44NDcxNDIsIDQyLjAzNjg5NCBdLCBbIC03Mi44MTM1NDEsIDQyLjAzNjQ5NCBdLCBbIC03Mi44MTY3NDEsIDQxLjk5NzU5NSBdLCBbIC03Mi43NjY3MzksIDQyLjAwMjk5NSBdLCBbIC03Mi43NjYxMzksIDQyLjAwNzY5NSBdLCBbIC03Mi43NjMyNjUsIDQyLjAwOTc0MiBdLCBbIC03Mi43NjMyMzgsIDQyLjAxMjc5NSBdLCBbIC03Mi43NjEyMzgsIDQyLjAxNDU5NSBdLCBbIC03Mi43NTk3MzgsIDQyLjAxNjk5NSBdLCBbIC03Mi43NjEzNTQsIDQyLjAxODE4MyBdLCBbIC03Mi43NjIzMTAsIDQyLjAxOTc3NSBdLCBbIC03Mi43NjIxNTEsIDQyLjAyMTUyNyBdLCBbIC03Mi43NjA1NTgsIDQyLjAyMTg0NiBdLCBbIC03Mi43NTgxNTEsIDQyLjAyMDg2NSBdLCBbIC03Mi43NTc0NjcsIDQyLjAyMDk0NyBdLCBbIC03Mi43NTQwMzgsIDQyLjAyNTM5NSBdLCBbIC03Mi43NTE3MzgsIDQyLjAzMDE5NSBdLCBbIC03Mi43NTM1MzgsIDQyLjAzMjA5NSBdLCBbIC03Mi43NTc1MzgsIDQyLjAzMzI5NSBdLCBbIC03Mi43NTU4MzgsIDQyLjAzNjE5NSBdLCBbIC03Mi42OTU5MjcsIDQyLjAzNjc4OCBdLCBbIC03Mi42NDMxMzQsIDQyLjAzMjM5NSBdLCBbIC03Mi42MDc5MzMsIDQyLjAzMDc5NSBdLCBbIC03Mi42MDY5MzMsIDQyLjAyNDk5NSBdLCBbIC03Mi41OTAyMzMsIDQyLjAyNDY5NSBdLCBbIC03Mi41ODIzMzIsIDQyLjAyNDY5NSBdLCBbIC03Mi41NzMyMzEsIDQyLjAzMDE0MSBdLCBbIC03Mi41MjgxMzEsIDQyLjAzNDI5NSBdLCBbIC03Mi40NTY2ODAsIDQyLjAzMzk5OSBdLCBbIC03Mi4zMTcxNDgsIDQyLjAzMTkwNyBdLCBbIC03Mi4yNDk1MjMsIDQyLjAzMTYyNiBdLCBbIC03Mi4xMzU2ODcsIDQyLjAzMDI0NSBdLCBbIC03Mi4wNjM0OTYsIDQyLjAyNzM0NyBdLCBbIC03MS45ODczMjYsIDQyLjAyNjg4MCBdLCBbIC03MS44OTA3ODAsIDQyLjAyNDM2OCBdLCBbIC03MS44MDA2NTAsIDQyLjAyMzU2OSBdLCBbIC03MS43OTkyNDIsIDQyLjAwODA2NSBdLCBbIC03MS43OTc5MjIsIDQxLjkzNTM5NSBdLCBbIC03MS43OTQxNjEsIDQxLjg0MTEwMSBdLCBbIC03MS43OTQxNjEsIDQxLjg0MDE0MSBdLCBbIC03MS43OTI3ODYsIDQxLjgwODY3MCBdLCBbIC03MS43OTI3NjcsIDQxLjgwNzAwMSBdLCBbIC03MS43OTEwNjIsIDQxLjc3MDI3MyBdLCBbIC03MS43ODk2NzgsIDQxLjcyNDczNCBdLCBbIC03MS43ODY5OTQsIDQxLjY1NTk5MiBdLCBbIC03MS43ODkzNTYsIDQxLjU5NjkxMCBdLCBbIC03MS43OTc2ODMsIDQxLjQxNjcwOSBdLCBbIC03MS44MTgzOTAsIDQxLjQxOTU5OSBdLCBbIC03MS44Mzk2NDksIDQxLjQxMjExOSBdLCBbIC03MS44NDI1NjMsIDQxLjQwOTg1NSBdLCBbIC03MS44NDM0NzIsIDQxLjQwNTgzMCBdLCBbIC03MS44NDIxMzEsIDQxLjM5NTM1OSBdLCBbIC03MS44MzM0NDMsIDQxLjM4NDUyNCBdLCBbIC03MS44MzE2MTMsIDQxLjM3MDg5OSBdLCBbIC03MS44Mzc3MzgsIDQxLjM2MzUyOSBdLCBbIC03MS44MzU5NTEsIDQxLjM1MzkzNSBdLCBbIC03MS44Mjk1OTUsIDQxLjM0NDU0NCBdLCBbIC03MS44MzkwMTMsIDQxLjMzNDA0MiBdLCBbIC03MS44NjA1MTMsIDQxLjMyMDI0OCBdLCBbIC03MS44NTk1NzAsIDQxLjMyMjM5OSBdIF0gXSwgWyBbIFsgLTczLjQyMjE2NSwgNDEuMDQ3NTYyIF0sIFsgLTczLjQwMzYxMCwgNDEuMDYyNjg3IF0sIFsgLTczLjM2Nzg1OSwgNDEuMDg4MTIwIF0sIFsgLTczLjM1MjA1MSwgNDEuMDg4MTIwIF0sIFsgLTczLjM4NTczNSwgNDEuMDU5MjUwIF0sIFsgLTczLjQyMjE2NSwgNDEuMDQ3NTYyIF0gXSBdIF0gfSB9-"
                        ))))))


  (t/testing "base64 empty input"
    (t/is (= 1 (count (:errors
                       (zen.core/validate
                        zctx
                        #{'fhir-r4.base64Binary/schema}
                        ""))))))

  (t/testing "base64 so short input"
    (t/is (= 1 (count (:errors
                       (zen.core/validate
                        zctx
                        #{'fhir-r4.base64Binary/schema}
                        "123"))))))

  (t/testing "base64 min length input"
    (t/is (empty? (:errors
                   (zen.core/validate
                    zctx
                    #{'fhir-r4.base64Binary/schema}
                    "1234")))))

  )
