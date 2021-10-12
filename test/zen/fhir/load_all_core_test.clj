(ns zen.fhir.load-all-core-test
  (:require [zen.fhir.core :as sut]
            [clojure.test :as t]
            [zen.core]
            [clojure.java.io :as io]
            [matcho.core :as matcho]))


(defmacro match-definition [ztx url pattern]
  `(let [res# (sut/get-definition ~ztx ~url)]
     (matcho/match res# ~pattern)
     res#))


(defonce ztx (zen.core/new-context {}))


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

    (sut/load-all ztx "hl7.fhir.r4.core")

    (t)))


(t/deftest fhir-aidbox-poly-keys-mapping
  (match-definition
    ztx "http://hl7.org/fhir/us/core/StructureDefinition/us-core-patient"
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

  (match-definition
    ztx "http://hl7.org/fhir/StructureDefinition/Observation"
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

  (match-definition
    ztx "http://hl7.org/fhir/us/core/StructureDefinition/pediatric-bmi-for-age"
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

  (match-definition
    ztx "http://hl7.org/fhir/StructureDefinition/Patient"
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

  (match-definition
    ztx "http://hl7.org/fhir/StructureDefinition/Questionnaire"
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
  (matcho/match
    (zen.fhir.core/get-definition ztx "http://hl7.org/test-plannet/StructureDefinition/plannet-PractitionerRole")
    {:|
     {:newpatients
      {:fhir/extension
       "http://hl7.org/test-plannet/StructureDefinition/newpatients"}}})

  (matcho/match
    (zen.fhir.core/get-definition ztx "http://hl7.org/test-plannet/StructureDefinition/newpatients")
    {:|
     {:acceptingPatients {}
      :fromnetwork {:fhir/extension "http://hl7.org/test-plannet/StructureDefinition/plannet-FromNetwork-extension"}}})

  (matcho/match
    (zen.fhir.core/get-definition ztx "http://hl7.org/test-plannet/StructureDefinition/plannet-FromNetwork-extension")
    {:type "Reference"
     :baseDefinition "http://hl7.org/fhir/StructureDefinition/Reference"}))


(t/deftest value-sets
  (t/testing "value set processing"
    (matcho/match
      (get-in @ztx [:fhir/inter "ValueSet" "http://hl7.org/fhir/ValueSet/administrative-gender"])
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
    (matcho/match
      (get-in @ztx [:fhir/inter "CodeSystem" "http://hl7.org/fhir/administrative-gender"])
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

    (matcho/match
      (get-in @ztx [:fhir/inter "Concept" "http:--hl7.org-fhir-administrative-gender-other"])
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
      (matcho/match
        (get-in @ztx [:fhir/inter "Concept" "http:--terminology.hl7.org-CodeSystem-v3-ActMood-EXPEC"])
        {:code      "EXPEC"
         :system    "http://terminology.hl7.org/CodeSystem/v3-ActMood"
         :hierarchy ["_ActMoodPredicate" nil]
         :property  nil?})

      (matcho/match
        (get-in @ztx [:fhir/inter "Concept" "http:--terminology.hl7.org-CodeSystem-v3-ActMood-GOL.CRT"])
        {:code      "GOL.CRT"
         :system    "http://terminology.hl7.org/CodeSystem/v3-ActMood"
         :hierarchy ["_ActMoodPredicate" "CRT" nil]
         :property  {"status" "retired"}})))

  (t/testing "value set contained concepts extract"
    (matcho/match
      (get-in @ztx [:fhir/inter "ValueSet" "http://hl7.org/fhir/us/mcode/ValueSet/mcode-cancer-staging-system-vs"])
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
      (matcho/match
        (get-in @ztx [:fhir/inter "Concept" "http:--hl7.org-fhir-link-type-seealso"])
        {:id       "http:--hl7.org-fhir-link-type-seealso"
         :valueset #{"http://hl7.org/fhir/ValueSet/link-type"}}))

    (t/testing "include.concept"
      (matcho/match
        (get-in @ztx [:fhir/inter "Concept" "http:--terminology.hl7.org-CodeSystem-umls-C4683555"])
        {:id       "http:--terminology.hl7.org-CodeSystem-umls-C4683555"
         :code     "C4683555"
         :display  "Ann Arbor Stage"
         :system   "http://terminology.hl7.org/CodeSystem/umls"
         :valueset #{"http://hl7.org/fhir/us/mcode/ValueSet/mcode-cancer-staging-system-vs"}
         :zen.fhir/resource {:valueset ["http://hl7.org/fhir/us/mcode/ValueSet/mcode-cancer-staging-system-vs"]}})

      (matcho/match
        (get-in @ztx [:fhir/inter "Concept" "http:--snomed.info-sct-444256004"])
        {:id       "http:--snomed.info-sct-444256004"
         :code     "444256004"
         :display  string?
         :system   "http://snomed.info/sct"
         :valueset #(contains? % "http://hl7.org/fhir/us/mcode/ValueSet/mcode-cancer-staging-system-vs")}))

    (t/testing "include.valueSet"
      #_(get-in @ztx [:fhir/inter "ValueSet" "http://hl7.org/fhir/ValueSet/use-context"])
      #_(get-in @ztx [:fhir/inter "ValueSet" "http://hl7.org/fhir/ValueSet/practitioner-specialty"])
      #_(get-in @ztx [:fhir/inter "CodeSystem" "http://hl7.org/fhir/practitioner-specialty"])

      (matcho/match
        (get-in @ztx [:fhir/inter "Concept" "http:--hl7.org-fhir-practitioner-specialty-dietary"])
        {:valueset #{"http://hl7.org/fhir/ValueSet/practitioner-specialty"
                     "http://hl7.org/fhir/ValueSet/use-context"}}))

    (t/testing "include.filter" ;; TODO
      (t/testing "descendent-of"
        #_(get-in @ztx [:fhir/inter "ValueSet" "http://hl7.org/fhir/ValueSet/inactive"])
        #_(get-in @ztx [:fhir/inter "CodeSystem" "http://terminology.hl7.org/CodeSystem/v3-ActMood"])
        (t/is (clojure.set/subset? ;; TODO, NOTE: should be clojure.core/=, not clojure.core/subset?, but codes
                #{"CRT" "EXPEC" "GOL" "RSK" "OPT"}
                (->> (get-in @ztx [:fhir/inter "Concept"])
                     vals
                     (filter #(contains? (:valueset %) "http://hl7.org/fhir/ValueSet/inactive"))
                     (map :code)
                     set)))))))