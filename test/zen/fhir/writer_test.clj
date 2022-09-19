(ns zen.fhir.writer-test
  (:require [zen.fhir.writer :as sut]
            [zen.fhir.generator]
            [zen.fhir.loader]
            [zen.package]
            [zen.core]
            [matcho.core :as matcho]
            [clojure.test :as t]
            [clojure.java.io :as io]
            [cheshire.core :as json]
            [clojure.java.shell :as sh]))


(def zen-fhir-version (slurp (clojure.java.io/resource "zen-fhir-version")))


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

      (zen.fhir.loader/init-ztx ztx)

      (do ;; 'nested-extension test fixtures
        (def from-network-extension (-> "zen/fhir/plannet_fromnetwork_stripped.edn" io/resource slurp read-string))
        (def new-patients-extension (-> "zen/fhir/plannet_newpatients_stripped.edn" io/resource slurp read-string))
        (def practitioner-role-profile (-> "zen/fhir/plannet_practitionerrole_stripped.edn" io/resource slurp read-string))
        (zen.fhir.loader/load-definiton ztx {:url (:url practitioner-role-profile)} (assoc practitioner-role-profile :zen.fhir/package-ns "plannet"))
        (zen.fhir.loader/load-definiton ztx {:url (:url new-patients-extension)} (assoc new-patients-extension :zen.fhir/package-ns "plannet"))
        (zen.fhir.loader/load-definiton ztx {:url (:url from-network-extension)} (assoc from-network-extension :zen.fhir/package-ns "plannet")))

      (zen.fhir.loader/load-all ztx nil
                                {:params {"hl7.fhir.r4.core" {:zen.fhir/package-ns 'fhir-r4}
                                          "hl7.fhir.us.core" {:zen.fhir/package-ns 'us-core-v3}}
                                 :whitelist {"ValueSet" #{"http://hl7.org/fhir/ValueSet/administrative-gender"
                                                          "http://hl7.org/fhir/us/core/ValueSet/birthsex"
                                                          "http://hl7.org/fhir/ValueSet/c80-practice-codes"
                                                          "http://hl7.org/fhir/us/davinci-pdex-plan-net/ValueSet/SpecialtiesVS"
                                                          "http://hl7.org/fhir/us/davinci-pdex-plan-net/ValueSet/IndividualAndGroupSpecialtiesVS"
                                                          "http://hl7.org/fhir/us/davinci-pdex-plan-net/ValueSet/NonIndividualSpecialtiesVS"}}})

      (zen.fhir.generator/generate-zen-schemas ztx)

      (catch Exception e
        (throw e))
      (finally (t)))))



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
                :package-paths ["zen.fhir/"]}))

    (def _ (zen.core/read-ns zctx 'us-core-v3.us-core-patient))

    (t/is (empty? (:errors @zctx)))
    #_(sort (distinct (map :missing-ns (filter (comp #(clojure.string/starts-with? % "No file for ns") :message) (:errors @zctx)))))

    (t/is (every? #(contains? (:ns @zctx) %)
                  ['us-core-v3.us-core-patient
                   'fhir-r4.Patient])))

  (def _ (reset! ztx og-ztx))

  :done)


(t/deftest zen-package-project-write

  (def test-dir "/tmp/zen-fhir-package-write-test/test-zen-modules")

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

  (t/testing "zen-modules"
    (t/testing "spit single module"
      (delete-directory-recursive (io/file test-dir))
      (sh/sh "mkdir" "-p" test-dir)

      (t/is (= :done (sut/spit-zen-packages ztx {:out-dir        test-dir
                                                 :package        "fhir-r4"
                                                 :git-url-format (str test-dir "/%s")})))

      (t/is (.exists (io/file (str test-dir "/fhir-r4/zrc/fhir-r4/Element.edn"))))
      (t/is (.exists (io/file (str test-dir "/fhir-r4/.git/"))))
      (t/is (and (.exists (io/file (str test-dir "/fhir-r4/zen-package.edn")))
                 (let [package (-> (str test-dir "/fhir-r4/zen-package.edn")
                                   io/file
                                   slurp
                                   read-string)]
                   (matcho/match package
                                 {:deps {'zen.fhir string?}}))))

      (t/is (not (.exists (io/file (str test-dir "/us-core-v3/zrc/us-core-v3/us-core-patient.edn"))))))

    (t/testing "spit all modules"
      (delete-directory-recursive (io/file test-dir))
      (sh/sh "mkdir" "-p" test-dir)

      (t/is (= :done (sut/spit-zen-packages ztx {:out-dir        test-dir
                                                 :git-url-format (str test-dir "/%s")})))

      (t/is (.exists (io/file (str test-dir "/fhir-r4/zrc/fhir-r4/Element.edn"))))
      (t/is (.exists (io/file (str test-dir "/us-core-v3/zrc/us-core-v3/us-core-patient.edn"))))
      (t/is (and (.exists (io/file (str test-dir "/us-core-v3/zen-package.edn")))
                 (let [package (-> (str test-dir "/us-core-v3/zen-package.edn")
                                   io/file
                                   slurp
                                   read-string)]
                   (matcho/match package
                                 {:deps {'fhir-r4 (str test-dir "/fhir-r4")}}))))

      (t/is (and (.exists (io/file (str test-dir "/fhir-r4/fhir-r4-terminology-bundle.ndjson.gz")))
                 (let [bundle (->> (str test-dir "/fhir-r4/fhir-r4-terminology-bundle.ndjson.gz")
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


  (t/testing "read zen package with deps"

   (def package-dir (str test-dir "/us-core-v3"))

   (zen.package/zen-init-deps! package-dir)
   (def zctx (zen.core/new-context
              {:package-paths ["zen.fhir" package-dir]}))

    (def _ (zen.core/read-ns zctx 'us-core-v3.us-core-patient))

    (t/is (empty? (:errors @zctx)))
    #_(sort (distinct (map :missing-ns (filter (comp #(clojure.string/starts-with? % "No file for ns") :message) (:errors @zctx)))))

    (t/is (every? #(contains? (:ns @zctx) %)
                  ['us-core-v3.us-core-patient
                   'fhir-r4.Patient])))

  (def _ (reset! ztx og-ztx))

  :done)

