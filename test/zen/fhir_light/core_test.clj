(ns zen.fhir-light.core-test
  (:require [cheshire.core :as json]
            [clojure.test :as t]
            [matcho.core :as matcho]
            [zen.core]
            [zen.fhir-light.core :as sut]))


(t/deftest convert-single-strdef-test
  (def dir (System/getProperty "user.dir"))

  (def fhir-core-ig-dir
    (str dir "/node_modules/hl7.fhir.r4.core"))

  (def us-core-ig-dir
    (str dir "/node_modules/hl7.fhir.us.core"))

  (def us-core-patient-str-def
    (json/parse-string (slurp (str us-core-ig-dir "/StructureDefinition-us-core-patient.json"))
                       keyword))
  #_(dissoc us-core-patient-str-def :text :snapshot)

  (def ztx (zen.core/new-context {}))

  #_(t/testing "errors because no base context"
    (def error-res (sut/structure-definition->zen-schemas ztx us-core-patient-str-def))

    (def common-errors-data
      {:structure-definition/url          "http://hl7.org/fhir/us/core/StructureDefinition/us-core-patient"
       :structure-definition/resourceType "StructureDefinition"
       :structure-definition/type         "Patient"
       :structure-definition/version      "5.0.1"
       :structure-definition/fhirVersion  "4.0.1"})

    (matcho/match error-res
                  {:errors (mapv #(merge common-errors-data %)
                                 [{:error/code :missing-fhir-core
                                   :error/source [:fhirVersion]}
                                  {:error/code :missing-base-definition
                                   :error/source [:baseDefinition]}
                                  {:error/code :missing-profile
                                   :error/source []
                                   :missing-profile/type "Extension"
                                   :missing-profile/url "http://hl7.org/fhir/us/core/StructureDefinition/us-core-race"}
                                  {:error/code :missing-profile
                                   :missing-profile/type "Extension"
                                   :missing-profile/url "http://hl7.org/fhir/us/core/StructureDefinition/us-core-ethnicity"}

                                  ],
                                 )}))

  #_"NOTE: for .extension elements we can assume such slicing by default
     https://build.fhir.org/profiling.html
     Note that extensions are always sliced by the url element, though they may be resliced on additional elements where required."
  #_#_:slicing
  {:discriminator [{:type "value", :path "url"}]
   :ordered false,
   :rules "open"}

  #_"NOTE: A structure definition is not required to designate any discriminator at all for a slice, but those that don't identify discriminators are describing content that is very difficult to process, and so this is discouraged.
  https://build.fhir.org/profiling.html"

  #_"NOTE: profiles can be applied partially. Not critical as we didn't find any usage of this
 https://build.fhir.org/profiling.html#partial"

  #_"NOTE: reslicing refers to sliceNames (and thus to slicing rules) of a baseDefinition, and makes reslicing not possible to be evaluated without dependency
the only usage we found: http://hl7.org/fhir/us/core/StructureDefinition/us-core-observation-social-history "

  #_"NOTE: slicing.type = pattern
This code means the same as value, and is retained for backwards compatibility reasons "

  #_"NOTE: If the type is value, or pattern, then the element definition must use either:

ElementDefinition.fixed[x], or
ElementDefinition.pattern[x], or
if the element has a terminology binding, a required binding with a Value Set that enumerates the list of possible codes in the value set (\"formally called an 'Extensional' value set\")"

  #_"NOTE: slicing can rely on indexes, type=position. We couldn't find any usage of this"
  #_"NOTE: slicing can rely on presence of data, type=exists. We couldn't find any usage of this"
  #_"NOTE: slicing on profile is very rare, we found only one usage example"
  #_"NOTE: spec theorethically allows to slice with type=type not on polymorphics. We couldn't find any usage of this
Context=List.entry	type=type	path=item.resolve()	interpretation=Entries are differentiated by the type of the target element that the reference points to"


  (t/testing "pure convertation, no context"
    (def res (sut/structure-definition->inter-repr ztx us-core-patient-str-def))

    (matcho/match res
                  {:schemas
                   {:root
                    {:structure-meta
                     {:url              "http://hl7.org/fhir/us/core/StructureDefinition/us-core-patient"
                      :resourceType     "StructureDefinition"
                      :type             "Patient"
                      :version          "5.0.1"
                      :fhirVersion      "4.0.1"
                      :date             "2022-04-20T15:02:49-07:00"
                      :kind             "resource"
                      :abstract         false
                      :experimental     false
                      :status           "active"
                      :derivation       "constraint"
                      :context          nil
                      :contextInvariant nil}

                     :dependencies
                     {:fhir-core {:version "4.0.1"}
                      :IGs nil}

                     :structure
                     {:elements {:extension {:slicing {:descriminator [{:type :value, :path [:url], :deduced true}]
                                                       :slices
                                                       {:race
                                                        {:structure {:min 0
                                                                     :max "1"
                                                                     :type [{:code "Extension"
                                                                             :profile ["http://hl7.org/fhir/us/core/StructureDefinition/us-core-race"]}]}
                                                         :description {:mustSupport false
                                                                       :mapping [{:identity "argonaut-dq-dstu2", :map "Patient.extension"}]}}
                                                        :ethnicity
                                                        {:min 0
                                                         :max "1"
                                                         :type [{:code "Extension"
                                                                 :profile ["http://hl7.org/fhir/us/core/StructureDefinition/us-core-ethnicity"]}]
                                                         :description {}}
                                                        :birthsex
                                                        {:min 0
                                                         :max "1"
                                                         :type [{:code "Extension"
                                                                 :profile ["http://hl7.org/fhir/us/core/StructureDefinition/us-core-birthsex"]}]
                                                         :description {}}
                                                        :genderIdentity
                                                        {:min 0
                                                         :max "1"
                                                         :type [{:code "Extension"
                                                                 :profile ["http://hl7.org/fhir/us/core/StructureDefinition/us-core-genderIdentity"]}]
                                                         :description {}}}}}
                                 }}

                     :description
                     {:name         "USCorePatientProfile"
                      :title        "US Core Patient Profile"
                      :useContext   nil
                      :purpose      nil
                      :keyword      nil
                      :identifier   nil
                      :description  nil
                      :mapping      [{:identity "rim", :uri "http://hl7.org/v3", :name "RIM Mapping"}
                                     {:identity "cda", :uri "http://hl7.org/v3/cda", :name "CDA (R2)"}
                                     {:identity "w5",
                                      :uri      "http://hl7.org/fhir/fivews",
                                      :name     "FiveWs Pattern Mapping"}
                                     {:identity "v2", :uri "http://hl7.org/v2", :name "HL7 v2 Mapping"}
                                     {:identity "loinc",
                                      :uri      "http://loinc.org",
                                      :name     "LOINC code for the element"}]
                      :publisher    "HL7 International - Cross-Group Projects"
                      :contact      [{:name    "HL7 International - Cross-Group Projects",
                                      :telecom [{:system "url", :value "http://www.hl7.org/Special/committees/cgp"}
                                                {:system "email", :value "cgp@lists.HL7.org"}]}]
                      :jurisdiction [{:coding [{:system "urn:iso:std:iso:3166", :code "US"}]}]
                      :copyright    "Used by permission of HL7 International, all rights reserved Creative Commons License"}}}})))


(t/deftest convert-many-strdef-test
  (t/is true))


(t/deftest convert-ig-package-test
  (t/is true))
