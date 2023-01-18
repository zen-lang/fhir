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
          :zen.fhir/content :not-present}}}}})

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
