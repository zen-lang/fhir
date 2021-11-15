(ns zen.fhir.generator-test
  (:require
   [zen.fhir.generator :as sut]
   [zen.fhir.core]
   [zen.core]
   [matcho.core :as matcho]
   [clojure.test :as t]
   [clojure.java.io :as io]
   [cheshire.core :as json]
   [clojure.string :as str]
   [clojure.set]))


(def memory-store
  '{zen.fhir
    {ns zen.fhir

     version
     {:zen/tags #{zen/schema zen/tag zen.fhir/version}
      :type zen/map
      :validation-type :open
      :zen.fhir/version "0.3.3-1"
      :require #{:zen.fhir/version}
      :keys {:zen.fhir/version {:type zen/string
                                :const {:value "0.3.3-1"}}}}}

    zenbox
    {ns zenbox
     import #{zen.fhir}

     Resource
     {:zen/tags #{zen/tag zen/schema}
      :type zen/map
      :keys {:resourceType {:type zen/string}
             :id {:type zen/string}
             :meta {:type zen/map :values {:type zen/any}}}}

     Reference
     {:zen/tags #{zen/schema}
      :zen/desc "reference datatype"
      :type zen/map
      :keys {:id {:type zen/string}
             :resourceType {:type zen/string}
             :display {:type zen/string}}}

     value-set
     {:zen/tags #{zen/schema zen/tag}
      :zen/desc "Value set"
      :confirms #{zen.fhir/version}
      :type zen/map
      :keys {:uri {:type zen/string}
             :version {:type zen/string}}}

     nested-schema
     {:zen/tags #{zen/schema}
      :type zen/map
      :keys {:fhir/flags {:type zen/set}
             :fhir/extensionUri {:type zen/string}
             :fhir/polymorphic {:type zen/boolean}
             :zenbox/reference {:type zen/map
                                :keys {:refers {:type zen/set
                                                :every {:type zen/symbol
                                                        #_#_:tags #{#{zenbox/base-schema zenbox/profile-schema}}}}}} ;; TODO
             :zenbox/value-set {:type zen/map
                                :keys {:symbol {:type zen/symbol}
                                       :strength {:type zen/keyword
                                                  :enum [{:value :required}
                                                         {:value :extensible}
                                                         {:value :preferred}
                                                         {:value :example}]}}}
             :keys {:type zen/map
                    :values {:confirms #{nested-schema}}}
             :every {:confirms #{nested-schema}}}}

     structure-schema
     {:zen/tags #{zen/schema zen/tag}
      :confirms #{nested-schema zen.fhir/version}
      :type     zen/map
      :keys     {:zenbox/type {:type zen/string}
                 :zenbox/profileUri {:type zen/string}}}

     base-schema
     {:zen/tags #{zen/schema zen/tag}
      :zen/desc "This schema should be used to validate all resources of its type"
      :confirms #{structure-schema}
      :type     zen/map
      :require  #{:zenbox/type}}

     profile-schema
     {:zen/tags #{zen/schema zen/tag}
      :zen/desc "This schema should be used only when mentioned in meta.profile"
      :confirms #{structure-schema}
      :type     zen/map
      :require  #{:zenbox/profileUri}}}})


(defn delete-directory-recursive
  [^java.io.File file]
  (when (.isDirectory file)
    (doseq [file-in-dir (.listFiles file)]
      (delete-directory-recursive file-in-dir)))
  (io/delete-file file true))


(defn read-ndjson-bundle [path]
  (with-open [^java.io.InputStream src (java.io.FileInputStream. path)
              gz (java.util.zip.GZIPInputStream. src)
              rd (clojure.java.io/reader gz)]
    (loop [acc []]
      (if-let [l (.readLine rd)]
        (recur (conj acc (cheshire.core/parse-string l keyword)))
        acc))))


(defonce ztx (zen.core/new-context {}))


(t/use-fixtures :once
  (fn generate&spit-project [t]
    (try
      (reset! ztx @(zen.core/new-context {}))

      (do ;; 'nested-extension test fixtures
        (def from-network-extension (-> "zen/fhir/plannet_fromnetwork_stripped.edn" io/resource slurp read-string))
        (def new-patients-extension (-> "zen/fhir/plannet_newpatients_stripped.edn" io/resource slurp read-string))
        (def practitioner-role-profile (-> "zen/fhir/plannet_practitionerrole_stripped.edn" io/resource slurp read-string))
        (zen.fhir.core/load-definiton ztx {:url (:url practitioner-role-profile)} (assoc practitioner-role-profile :zen.fhir/package-ns "plannet"))
        (zen.fhir.core/load-definiton ztx {:url (:url new-patients-extension)} (assoc new-patients-extension :zen.fhir/package-ns "plannet"))
        (zen.fhir.core/load-definiton ztx {:url (:url from-network-extension)} (assoc from-network-extension :zen.fhir/package-ns "plannet")))

      (zen.fhir.core/load-all ztx nil
                              {:params {"hl7.fhir.r4.core" {:zen.fhir/package-ns 'fhir-r4}
                                        "hl7.fhir.us.core" {:zen.fhir/package-ns 'us-core-v3}}
                               :whitelist {"ValueSet" #{"http://hl7.org/fhir/ValueSet/administrative-gender"
                                                        "http://hl7.org/fhir/us/core/ValueSet/birthsex"
                                                        "http://hl7.org/fhir/ValueSet/c80-practice-codes"
                                                        "http://hl7.org/fhir/us/davinci-pdex-plan-net/ValueSet/SpecialtiesVS"
                                                        "http://hl7.org/fhir/us/davinci-pdex-plan-net/ValueSet/IndividualAndGroupSpecialtiesVS"
                                                        "http://hl7.org/fhir/us/davinci-pdex-plan-net/ValueSet/NonIndividualSpecialtiesVS"}}})

      (sut/generate-zen-schemas ztx)

      (catch Exception e
        (throw e))
      (finally (t)))))


(t/deftest generate-project-integration
  (def schemas-match
    {'fhir-r4
     {'ns 'fhir-r4
      'import (partial clojure.set/subset?
                       #{'fhir-r4.string
                         'fhir-r4.Element
                         'fhir-r4.Resource
                         'fhir-r4.DomainResource
                         'fhir-r4.value-set.administrative-gender
                         'fhir-r4.Patient
                         'fhir-r4.Practitioner})}

     'us-core-v3
     {'ns 'us-core-v3
      'import (partial clojure.set/subset?
                       #{'fhir-r4
                         'us-core-v3.us-core-patient
                         'us-core-v3.value-set.birthsex
                         'us-core-v3.us-core-birthsex})}

     'fhir-r4.string
     {'ns     'fhir-r4.string
      'schema {:zen/tags #{'zen/schema 'zenbox/structure-schema}
               :confirms #(not (contains? % 'fhir-r4.Element/schema))
               :type 'zen/string}}

     'fhir-r4.Extension
     {'ns     'fhir-r4.Extension
      'schema {:zen/tags #{'zen/schema 'zenbox/structure-schema}
               :confirms #{'fhir-r4.Element/schema}
               :type     'zen/map
               :keys     {:url {:confirms #{'fhir-r4.uri/schema}}
                          :value {:type 'zen/map
                                  :exclusive-keys (comp not empty?)
                                  :keys {:uri {:confirms #{'fhir-r4.uri/schema}}
                                         :url {:confirms #{'fhir-r4.url/schema}}
                                         :string {:confirms #{'fhir-r4.string/schema}}
                                         :Reference {:confirms #{'fhir-r4.Reference/schema 'zenbox/Reference}}}}}}}

     'fhir-r4.Element
     {'ns     'fhir-r4.Element
      'schema {:zen/tags #{'zen/schema 'zenbox/structure-schema}
               :confirms empty?
               :type     'zen/map
               :keys     {:id        {:confirms #{'fhir-r4.string/schema}}
                          :extension {:type  'zen/vector
                                      :every {:confirms #{'fhir-r4.Extension/schema}}}}}}

     'fhir-r4.Resource
     {'ns     'fhir-r4.Resource
      'schema {:zen/tags #{'zen/schema 'zenbox/structure-schema}
               :confirms #{'zenbox/Resource}
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
      'schema {:zen/tags #{'zen/schema 'zenbox/structure-schema}
               :confirms #{'fhir-r4.Resource/schema 'zenbox/Resource}
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
      'import #(contains? % 'zenbox)
      'value-set {:zen/tags #{'zenbox/value-set}
                  :uri "http://hl7.org/fhir/ValueSet/administrative-gender"
                  :fhir/code-systems #{"http://hl7.org/fhir/administrative-gender"}}}

     'fhir-r4.HumanName
     {'ns 'fhir-r4.HumanName
      'schema {:zen/tags #{'zen/schema 'zenbox/structure-schema}
               :confirms #{'fhir-r4.Element/schema}
               :type 'zen/map
               :zenbox/type "HumanName"
               :zenbox/profileUri "http://hl7.org/fhir/StructureDefinition/HumanName"
               :keys {:family  {:confirms #{'fhir-r4.string/schema}}
                      :_family {:confirms #{'fhir-r4.Element/schema}}

                      :given  {:type  'zen/vector
                               :every {:confirms #{'fhir-r4.string/schema}}}
                      :_given {:type  'zen/vector
                               :every {:confirms #{'fhir-r4.Element/schema}}}}}}

     'fhir-r4.Patient
     {'ns     'fhir-r4.Patient
      'import #(and (contains? % 'fhir-r4.DomainResource)
                    (contains? % 'zenbox)
                    (contains? % 'fhir-r4.value-set.administrative-gender))
      'schema {:zen/tags #{'zen/schema 'zenbox/base-schema}
               :confirms #{'fhir-r4.DomainResource/schema 'zenbox/Resource}
               :type 'zen/map
               :zenbox/type "Patient"
               :zenbox/profileUri "http://hl7.org/fhir/StructureDefinition/Patient"
               :keys {:name {:type 'zen/vector
                             :every {:confirms #{'fhir-r4.HumanName/schema}}}
                      :active {:confirms #{'fhir-r4.boolean/schema}}
                      :deceased {:type 'zen/map
                                 :fhir/polymorphic true
                                 :exclusive-keys #{#{:boolean :dateTime}}
                                 :keys {:boolean {:confirms #{'fhir-r4.boolean/schema}}
                                        :dateTime {:confirms #{'fhir-r4.dateTime/schema}}}}
                      :managingOrganization {:zen/desc "Organization that is the custodian of the patient record"
                                             :confirms #{'fhir-r4.Reference/schema 'zenbox/Reference}
                                             :zenbox/reference {:refers #{'fhir-r4.Organization/schema}}}
                      :gender {:confirms #{'fhir-r4.code/schema}
                               :zenbox/value-set {:symbol 'fhir-r4.value-set.administrative-gender/value-set
                                                  :strength :required}}
                      :link {:type 'zen/vector
                             :every {:require #{:other :type}}}}}}

     'fhir-r4.ServiceRequest
     {'ns     'fhir-r4.ServiceRequest
      'schema {:keys {:supportingInfo {:every {:confirms #{'fhir-r4.Reference/schema 'zenbox/Reference}
                                               :zenbox/reference {:refers #{}}}}}}}

     'fhir-r4.Practitioner
     {'ns     'fhir-r4.Practitioner
      'import #(contains? % 'fhir-r4.value-set.administrative-gender)
      'schema {:keys {:gender {:confirms #{'fhir-r4.code/schema}
                               :zenbox/value-set {:symbol 'fhir-r4.value-set.administrative-gender/value-set
                                                  :strength :required}}}}}

     'us-core-v3.us-core-patient
     {'ns     'us-core-v3.us-core-patient
      'import #(and (contains? % 'fhir-r4.Patient)
                    (contains? % 'zenbox))
      'schema {:zen/tags #{'zen/schema 'zenbox/profile-schema}
               :zen/desc "Defines constraints and extensions on the patient resource for the minimal set of data to query and retrieve patient demographic information."
               :confirms #{'fhir-r4.Patient/schema 'zenbox/Resource}
               :type 'zen/map
               :zenbox/type "Patient"
               :zenbox/profileUri "http://hl7.org/fhir/us/core/StructureDefinition/us-core-patient"
               :require #{:name :gender :identifier}
               :keys {:race      {:confirms #{'us-core-v3.us-core-race/schema}
                                  :fhir/extensionUri "http://hl7.org/fhir/us/core/StructureDefinition/us-core-race"}
                      :ethnicity {:confirms #{'us-core-v3.us-core-ethnicity/schema}
                                  :fhir/extensionUri "http://hl7.org/fhir/us/core/StructureDefinition/us-core-ethnicity"}
                      :birthsex  {:confirms #{'us-core-v3.us-core-birthsex/schema}
                                  :fhir/extensionUri "http://hl7.org/fhir/us/core/StructureDefinition/us-core-birthsex"}
                      :identifier {:type     'zen/vector
                                   :minItems 1
                                   :every    {:confirms #{'fhir-r4.Identifier/schema}
                                              :type 'zen/map
                                              :keys {:system {:confirms #{'fhir-r4.uri/schema}}
                                                     :value  {:zen/desc "The value that is unique within the system."
                                                              :confirms #{'fhir-r4.string/schema}}}}}}}}

     'us-core-v3.value-set.birthsex
     {'ns 'us-core-v3.value-set.birthsex
      'import #{'zenbox}

      'value-set
      {:uri "http://hl7.org/fhir/us/core/ValueSet/birthsex"
       :fhir/code-systems #{"http://terminology.hl7.org/CodeSystem/v3-AdministrativeGender"
                            "http://terminology.hl7.org/CodeSystem/v3-NullFlavor"}}}

     'us-core-v3.us-core-birthsex
     {'ns 'us-core-v3.us-core-birthsex
      'import #(and (contains? % 'us-core-v3.value-set.birthsex)
                    (contains? % 'zenbox))

      'schema
      {:zenbox/value-set {:symbol 'us-core-v3.value-set.birthsex/value-set
                          :strength :required}}}

     'fhir-r4.MolecularSequence
     {'schema
      {:keys {:structureVariant {:every {:keys {:variantType {:zenbox/value-set nil?}}}}}}}

     'hl7-fhir-us-davinci-pdex-plan-net.org-description
     {'ns 'hl7-fhir-us-davinci-pdex-plan-net.org-description
      'import #{'zenbox 'fhir-r4.string}

      'schema
      {:zen/tags #{'zen/schema 'zenbox/structure-schema}
       :zen/desc "An extension to provide a human-readable description of an organization."
       :confirms #{'fhir-r4.string/schema}
       :zenbox/type "string"
       :zenbox/profileUri "http://hl7.org/fhir/us/davinci-pdex-plan-net/StructureDefinition/org-description"
       }}

     'fhir-r4.condition-dueTo
     {'schema
      {:zen/tags #{'zen/schema 'zenbox/structure-schema}
       :type 'zen/map
       :confirms nil?
       :zenbox/profileUri "http://hl7.org/fhir/StructureDefinition/condition-dueTo"
       :fhir/polymorphic true
       :keys {:CodeableConcept {:confirms #{'fhir-r4.CodeableConcept/schema}}
              :Reference {:confirms #{'fhir-r4.Reference/schema 'zenbox/Reference}}}}}

     'hl7-fhir-us-davinci-pdex-plan-net.practitioner-qualification
     {'schema
      {:zen/tags #{'zen/schema 'zenbox/structure-schema}
       :confirms empty?
       :keys {:status {:confirms #{'fhir-r4.code/schema}}
              :whereValid {:type 'zen/vector
                           :every {:type 'zen/map
                                   :fhir/polymorphic true
                                   :exclusive-keys #{#{:CodeableConcept :Reference}}
                                   :keys {:CodeableConcept {:confirms #{'fhir-r4.CodeableConcept/schema}}
                                          :Reference {:confirms #{'fhir-r4.Reference/schema 'zenbox/Reference}}}}}}}}


     'hl7-fhir-us-davinci-pdex-plan-net.plannet-Practitioner
     {'schema
      {:zen/tags #{'zen/schema 'zenbox/profile-schema}
       :keys {:qualification {:type 'zen/vector
                              :every {:type 'zen/map
                                      :keys {:practitioner-qualification
                                             {:confirms #{'hl7-fhir-us-davinci-pdex-plan-net.practitioner-qualification/schema}
                                              :fhir/extensionUri
                                              "http://hl7.org/fhir/us/davinci-pdex-plan-net/StructureDefinition/practitioner-qualification"}}}}}}}


     'fhir-r4.HealthcareService
     {'schema
      {:zen/tags #{'zen/schema 'zenbox/base-schema}
       :keys {:specialty
              {:every {:zenbox/value-set
                       {:symbol 'fhir-r4.value-set.c80-practice-codes/value-set
                        :strength :preferred}}}}}}

     'hl7-fhir-us-davinci-pdex-plan-net.value-set.SpecialtiesVS
     {'value-set
      {:uri "http://hl7.org/fhir/us/davinci-pdex-plan-net/ValueSet/SpecialtiesVS"
       :fhir/code-systems #{"http://nucc.org/provider-taxonomy"}}}

     'hl7-fhir-us-davinci-pdex-plan-net.plannet-HealthcareService
     {'schema
      {:zen/tags #{'zen/schema 'zenbox/profile-schema}
       :keys {:specialty
              {:every {:zenbox/value-set
                       {:symbol 'hl7-fhir-us-davinci-pdex-plan-net.value-set.SpecialtiesVS/value-set
                        :strength :required}}}}}}})

  (matcho/match
    (select-keys (:fhir.zen/ns @ztx) (keys schemas-match))
    schemas-match))


(t/deftest project-write
  (def og-ztx @ztx)

  (def tested-nses '#{fhir-r4.Element us-core-v3.us-core-patient})

  (def ns-deps '#{fhir-r4.Address
                  fhir-r4.BackboneElement
                  fhir-r4.CodeableConcept
                  fhir-r4.ContactPoint
                  fhir-r4.HumanName
                  fhir-r4.Identifier
                  fhir-r4.Patient
                  fhir-r4.Period
                  fhir-r4.code
                  fhir-r4.date
                  fhir-r4.string
                  fhir-r4.uri
                  fhir-r4.value-set.administrative-gender
                  fhir-r4.value-set.contact-point-system
                  fhir-r4.value-set.contact-point-use
                  us-core-v3.us-core-birthsex
                  us-core-v3.us-core-ethnicity
                  us-core-v3.us-core-race
                  us-core-v3.value-set.birthsex

                  fhir-r4.Attachment
                  fhir-r4.Coding
                  fhir-r4.DomainResource
                  fhir-r4.Extension
                  fhir-r4.Organization
                  fhir-r4.Practitioner
                  fhir-r4.PractitionerRole
                  fhir-r4.Reference
                  fhir-r4.RelatedPerson
                  fhir-r4.boolean
                  fhir-r4.dateTime
                  fhir-r4.integer
                  fhir-r4.positiveInt
                  fhir-r4.value-set.address-type
                  fhir-r4.value-set.address-use
                  fhir-r4.value-set.identifier-use
                  fhir-r4.value-set.languages
                  fhir-r4.value-set.link-type
                  fhir-r4.value-set.name-use
                  us-core-v3.value-set.detailed-ethnicity
                  us-core-v3.value-set.detailed-race
                  us-core-v3.value-set.omb-ethnicity-category
                  us-core-v3.value-set.omb-race-category

                  fhir-r4.Age
                  fhir-r4.Annotation
                  fhir-r4.ContactDetail
                  fhir-r4.Contributor
                  fhir-r4.Count
                  fhir-r4.DataRequirement
                  fhir-r4.Distance
                  fhir-r4.Dosage
                  fhir-r4.Duration
                  fhir-r4.Endpoint
                  fhir-r4.Expression
                  fhir-r4.HealthcareService
                  fhir-r4.Location
                  fhir-r4.Meta
                  fhir-r4.Money
                  fhir-r4.Narrative
                  fhir-r4.ParameterDefinition
                  fhir-r4.Quantity
                  fhir-r4.Range
                  fhir-r4.Ratio
                  fhir-r4.RelatedArtifact
                  fhir-r4.Resource
                  fhir-r4.SampledData
                  fhir-r4.Signature
                  fhir-r4.Timing
                  fhir-r4.TriggerDefinition
                  fhir-r4.UsageContext
                  fhir-r4.base64Binary
                  fhir-r4.canonical
                  fhir-r4.decimal
                  fhir-r4.id
                  fhir-r4.instant
                  fhir-r4.markdown
                  fhir-r4.oid
                  fhir-r4.time
                  fhir-r4.unsignedInt
                  fhir-r4.url
                  fhir-r4.uuid
                  fhir-r4.value-set.c80-practice-codes
                  fhir-r4.value-set.days-of-week
                  fhir-r4.value-set.mimetypes
                  fhir-r4.value-set.relatedperson-relationshiptype

                  fhir-r4.Device
                  fhir-r4.Group
                  fhir-r4.InsurancePlan
                  fhir-r4.PlanDefinition
                  fhir-r4.ResearchStudy
                  fhir-r4.Schedule
                  fhir-r4.SimpleQuantity
                  fhir-r4.value-set.all-types
                  fhir-r4.value-set.contributor-type
                  fhir-r4.value-set.currencies
                  fhir-r4.value-set.endpoint-status
                  fhir-r4.value-set.event-timing
                  fhir-r4.value-set.location-mode
                  fhir-r4.value-set.location-status
                  fhir-r4.value-set.narrative-status
                  fhir-r4.value-set.operation-parameter-use
                  fhir-r4.value-set.quantity-comparator
                  fhir-r4.value-set.related-artifact-type
                  fhir-r4.value-set.signature-type
                  fhir-r4.value-set.sort-direction
                  fhir-r4.value-set.timing-abbreviation
                  fhir-r4.value-set.trigger-type
                  fhir-r4.value-set.units-of-time
                  fhir-r4.value-set.v2-0116
                  fhir-r4.xhtml

                  fhir-r4.DeviceDefinition
                  fhir-r4.Medication
                  fhir-r4.Substance
                  fhir-r4.value-set.action-cardinality-behavior
                  fhir-r4.value-set.action-condition-kind
                  fhir-r4.value-set.action-grouping-behavior
                  fhir-r4.value-set.action-participant-type
                  fhir-r4.value-set.action-precheck-behavior
                  fhir-r4.value-set.action-relationship-type
                  fhir-r4.value-set.action-required-behavior
                  fhir-r4.value-set.action-selection-behavior
                  fhir-r4.value-set.device-nametype
                  fhir-r4.value-set.device-status
                  fhir-r4.value-set.goal-priority
                  fhir-r4.value-set.group-type
                  fhir-r4.value-set.insuranceplan-applicability
                  fhir-r4.value-set.publication-status
                  fhir-r4.value-set.request-priority
                  fhir-r4.value-set.research-study-objective-type
                  fhir-r4.value-set.research-study-status
                  fhir-r4.value-set.udi-entry-type

                  fhir-r4.ProdCharacteristic
                  fhir-r4.ProductShelfLife
                  fhir-r4.value-set.medication-status
                  fhir-r4.value-set.substance-status})

  (def _ (swap! ztx update :fhir.zen/ns select-keys
                (into tested-nses ns-deps)))

  (t/testing "zen-npm-modules"
    (t/testing "spit single module"
      (delete-directory-recursive (io/file "test-temp-zrc"))

      (t/is (= :done (sut/spit-zen-npm-modules ztx "test-temp-zrc/node_modules/" "0.0.1-test" "fhir-r4")))

      (t/is (.exists (io/file "test-temp-zrc/node_modules/fhir-r4/fhir-r4/Element.edn")))
      (t/is (and (.exists (io/file "test-temp-zrc/node_modules/fhir-r4/package.json"))
                 (let [package (-> "test-temp-zrc/node_modules/fhir-r4/package.json"
                                   io/file
                                   slurp
                                   (json/parse-string keyword))]
                   (matcho/match package
                                 {:name "@zen-lang/fhir-r4"
                                  :version "0.0.1-test"}))))

      (t/is (not (.exists (io/file "test-temp-zrc/node_modules/us-core-v3/us-core-v3/us-core-patient.edn")))))

    (t/testing "spit all modules"
      (delete-directory-recursive (io/file "test-temp-zrc"))

      (t/is (= :done (sut/spit-zen-npm-modules ztx "test-temp-zrc/node_modules/" "0.0.1-test")))

      (t/is (.exists (io/file "test-temp-zrc/node_modules/fhir-r4/fhir-r4/Element.edn")))
      (t/is (.exists (io/file "test-temp-zrc/node_modules/us-core-v3/us-core-v3/us-core-patient.edn")))
      (t/is (and (.exists (io/file "test-temp-zrc/node_modules/us-core-v3/package.json"))
                 (let [package (-> "test-temp-zrc/node_modules/us-core-v3/package.json"
                                   io/file
                                   slurp
                                   (json/parse-string))]
                   (matcho/match package
                                 {"name" "@zen-lang/us-core-v3"
                                  "version" "0.0.1-test"
                                  "dependencies" {"@zen-lang/fhir-r4" "0.0.1-test"}}))))

      (t/is (and (.exists (io/file "test-temp-zrc/node_modules/fhir-r4/fhir-r4-terminology-bundle.ndjson.gz"))
                 (let [bundle (->> "test-temp-zrc/node_modules/fhir-r4/fhir-r4-terminology-bundle.ndjson.gz"
                                   (read-ndjson-bundle)
                                   (group-by (juxt :resourceType :id)))]
                   (matcho/match bundle
                                 {["ValueSet" "administrative-gender"]
                                  [{:url "http://hl7.org/fhir/ValueSet/administrative-gender"
                                    :_source "zen.fhir"
                                    :zen.fhir/header nil?
                                    :zen.fhir/package nil?
                                    :zen.fhir/package-ns nil?
                                    :zen.fhir/schema-ns nil?}]

                                  ["CodeSystem" "administrative-gender"]
                                  [{:url "http://hl7.org/fhir/administrative-gender"
                                    :concept nil?
                                    :_source "zen.fhir"

                                    :zen.fhir/header nil?
                                    :zen.fhir/package nil?
                                    :zen.fhir/package-ns nil?
                                    :zen.fhir/schema-ns nil?}]

                                  ["Concept" "http:--hl7.org-fhir-administrative-gender-other"]
                                  [{:code       "other"
                                    :display    "Other"
                                    :definition "Other."
                                    :system     "http://hl7.org/fhir/administrative-gender"
                                    :valueset   ["http://hl7.org/fhir/ValueSet/administrative-gender"]
                                    :_source "zen.fhir"
                                    :zen.fhir/header nil?
                                    :zen.fhir/package nil?
                                    :zen.fhir/package-ns nil?
                                    :zen.fhir/schema-ns nil?}]}))))))

  (t/testing "read zen npm modules"
    (def zctx (zen.core/new-context
                {:paths ["test-temp-zrc/"]
                 :memory-store memory-store}))

    (def _ (zen.core/read-ns zctx 'us-core-v3.us-core-patient))

    (t/is (empty? (:errors @zctx)))
    #_(sort (distinct (map :missing-ns (filter (comp #(clojure.string/starts-with? % "No file for ns") :message)(:errors @zctx)))))

    (t/is (every? #(contains? (:ns @zctx) %)
                  ['us-core-v3.us-core-patient
                   'fhir-r4.Patient])))

  (def _ (reset! ztx og-ztx))

  :done)


(t/deftest zen-schemas-validation
  (def zctx (zen.core/new-context
              {:memory-store
               (merge (:fhir.zen/ns @ztx)
                      memory-store)}))

  (run! (fn [zen-ns]
          (zen.core/load-ns zctx (get-in @zctx [:memory-store zen-ns])))
        '#{fhir-r4
           us-core-v3
           hl7-fhir-us-carin-bb
           hl7-fhir-us-Davinci-drug-formulary
           hl7-fhir-us-davinci-hrex
           hl7-fhir-us-davinci-pdex
           hl7-fhir-us-davinci-pdex-plan-net
           hl7-fhir-us-mcode})

  (t/is (->> (:errors @zctx)
             (remove ;; FIXME
               #{{:message "Invalid token: :",
                  :file "test-temp-zrc/node_modules/fhir-r4/fhir-r4/familymemberhistory-genetic.edn",
                  :ns 'fhir-r4.familymemberhistory-genetic}})
             empty?))

  (t/is (every? #(contains? (:ns @zctx) %)
                ['us-core-v3.us-core-patient
                 'fhir-r4.Patient]))

  (t/is (empty? (:errors (zen.core/validate zctx '#{fhir-r4.Patient/schema} {}))))

  (def fhir-pat (read-string (slurp (io/resource "zen/fhir/aidbox-fhir-r4-patient-example.edn"))))

  (t/is (empty? (:errors (zen.core/validate zctx '#{fhir-r4.Patient/schema} fhir-pat))))

  (matcho/match
    (:errors (zen.core/validate zctx '#{us-core-v3.us-core-patient/schema} {}))
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

       'schema {:keys {:newpatients {:every {:confirms #{'plannet.newpatients/schema}}}}}}

      'plannet.newpatients
      {'ns 'plannet.newpatients
       'import #(contains? % 'plannet.plannet-FromNetwork-extension)

       'schema {:require #{:acceptingPatients}
                :keys {:acceptingPatients {:confirms #{'fhir-r4.CodeableConcept/schema}
                                           :fhir/flags #{:MS}}
                       :fromnetwork {:confirms #{'plannet.plannet-FromNetwork-extension/schema}}}}}

      'plannet.plannet-FromNetwork-extension
      {'ns 'plannet.plannet-FromNetwork-extension

       'schema {:zen/tags          #{'zen/schema 'zenbox/structure-schema}
                :zen/desc          "A reference to a healthcare provider insurance network (plannet-Network) for which the entity is/isn’t accepting new patients. This is a component of the NewPatients extension."
                :confirms          #{'fhir-r4.Reference/schema 'zenbox/Reference}
                :zenbox/type       "Reference"
                :zenbox/profileUri "http://hl7.org/test-plannet/StructureDefinition/plannet-FromNetwork-extension"
                :fhir/flags        #{:MS}}}}))

  (t/testing "Generated zen schemas are correct"
    (swap! ztx assoc :memory-store (merge (:fhir.zen/ns @ztx) memory-store))

    (zen.core/load-ns ztx (get (:fhir.zen/ns @ztx) 'plannet.plannet-PractitionerRole))

    (t/is (empty? (:errors @ztx)))

    (matcho/match
     (zen.core/validate ztx '#{plannet.plannet-PractitionerRole/schema} {})
     {:errors empty?})

    (matcho/match
     (zen.core/validate
      ztx '#{plannet.plannet-PractitionerRole/schema}
      {:newpatients
       [{:acceptingPatients {:coding [{:code "foo"}]
                             :text "foo"}
         :fromnetwork {:resourceType "Network"
                       :id "some-plannet-network"}}]})
     {:errors empty?})

    (matcho/match
     (zen.core/validate
      ztx '#{plannet.plannet-PractitionerRole/schema}
      {:newpatients
       {:acceptingPatients {:coding [{:code "foo"}]
                            :text "foo"}
        :fromnetwork {:resourceType "Network"
                      :id "some-plannet-network"}}})
     {:errors [{:type "type"
                :path [:newpatients]
                :schema ['plannet.plannet-PractitionerRole/schema :newpatients]}
               nil]})))
