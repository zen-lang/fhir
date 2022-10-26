(ns zen.fhir.writer-test
  (:require [zen.fhir.writer :as sut]
            [zen.fhir.generator]
            [zen.fhir.loader]
            [zen.package]
            [zen.core]
            [matcho.core :as matcho]
            [clojure.test :as t]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [cheshire.core :as json]
            [clojure.java.shell :as sh]
            [ftr.zen-package]))


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

(defn generate&spit-project [& [opts]]
  (reset! ztx @(zen.core/new-context {}))

  (zen.fhir.loader/init-ztx ztx)

  (zen.fhir.loader/load-all ztx nil
                            (merge {:params {"hl7.fhir.r4.core" {:zen.fhir/package-ns 'fhir-r4}
                                             "hl7.fhir.us.core" {:zen.fhir/package-ns 'us-core}}
                                    :whitelist {"ValueSet" #{"http://hl7.org/fhir/ValueSet/administrative-gender"
                                                             "http://hl7.org/fhir/us/core/ValueSet/birthsex"
                                                             "http://hl7.org/fhir/ValueSet/c80-practice-codes"
                                                             "http://cts.nlm.nih.gov/fhir/ValueSet/2.16.840.1.113762.1.4.1021.32"
                                                             "http://cts.nlm.nih.gov/fhir/ValueSet/2.16.840.1.113762.1.4.1114.17"
                                                             "http://cts.nlm.nih.gov/fhir/ValueSet/2.16.840.1.113762.1.4.1021.101"
                                                             "http://terminology.hl7.org/CodeSystem/v3-NullFlavor"
                                                             "http://hl7.org/fhir/us/davinci-pdex-plan-net/ValueSet/SpecialtiesVS"
                                                             "http://hl7.org/fhir/us/davinci-pdex-plan-net/ValueSet/IndividualAndGroupSpecialtiesVS"
                                                             "http://hl7.org/fhir/us/davinci-pdex-plan-net/ValueSet/NonIndividualSpecialtiesVS"}}}
                                   opts))

  (zen.fhir.generator/generate-zen-schemas ztx))


(t/deftest project-write
  (generate&spit-project)

  (def og-ztx @ztx)

  (def tested-nses '#{fhir-r4.Element us-core.us-core-patient})

  (def ns-deps '#{us-core.us-core-birthsex
                  us-core.us-core-ethnicity
                  us-core.us-core-race
                  us-core.us-core-genderIdentity
                  us-core.value-set.birthsex
                  us-core.value-set.detailed-ethnicity
                  us-core.value-set.detailed-race
                  us-core.value-set.omb-ethnicity-category
                  us-core.value-set.omb-race-category
                  us-nlm-vsac.value-set.2.16.840.1.113762.1.4.1021.32

                  fhir-r4.Address
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

      (t/is (= :done (sut/spit-zen-npm-modules ztx "test-temp-zrc/node_modules/" zen-fhir-version "fhir-r4")))

      (t/is (.exists (io/file "test-temp-zrc/node_modules/fhir-r4/fhir-r4/Element.edn")))
      (t/is (and (.exists (io/file "test-temp-zrc/node_modules/fhir-r4/package.json"))
                 (let [package (-> "test-temp-zrc/node_modules/fhir-r4/package.json"
                                   io/file
                                   slurp
                                   (json/parse-string keyword))]
                   (matcho/match package
                     {:name "@zen-lang/fhir-r4"
                      :version zen-fhir-version}))))

      (t/is (not (.exists (io/file "test-temp-zrc/node_modules/us-core/us-core/us-core-patient.edn")))))

    (t/testing "spit all modules"
      (delete-directory-recursive (io/file "test-temp-zrc"))

      (t/is (= :done (sut/spit-zen-npm-modules ztx "test-temp-zrc/node_modules/" zen-fhir-version)))

      (t/is (.exists (io/file "test-temp-zrc/node_modules/fhir-r4/fhir-r4/Element.edn")))
      (t/is (.exists (io/file "test-temp-zrc/node_modules/us-core/us-core/us-core-patient.edn")))
      (t/is (and (.exists (io/file "test-temp-zrc/node_modules/us-core/package.json"))
                 (let [package (-> "test-temp-zrc/node_modules/us-core/package.json"
                                   io/file
                                   slurp
                                   (json/parse-string))]
                   (matcho/match package
                     {"name" "@zen-lang/us-core"
                      "version" zen-fhir-version
                      "dependencies" {"@zen-lang/fhir-r4" zen-fhir-version}}))))

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

    (def _ (zen.core/read-ns zctx 'us-core.us-core-patient))

    (t/is (empty? (:errors @zctx)))
    #_(sort (distinct (map :missing-ns (filter (comp #(clojure.string/starts-with? % "No file for ns") :message) (:errors @zctx)))))

    (t/is (every? #(contains? (:ns @zctx) %)
                  ['us-core.us-core-patient
                   'fhir-r4.Patient])))

  (def _ (reset! ztx og-ztx))

  :done)


(defn fs-tree->tree-map [path]
  (let [splitted-path (drop 1 (str/split path #"/"))
        tree-map (reduce
                   (fn [store path] (assoc-in store path {}))
                   {}
                   (map (fn [f] (drop 1 (str/split (str f) #"/"))) (file-seq (io/file path))))]
    (get-in tree-map splitted-path)))


(t/deftest zen-package-project-write
  (generate&spit-project {:skip-concept-processing true})

  (def test-dir "/tmp/zen-fhir-package-write-test/test-zen-packages")

  (def og-ztx @ztx)

  (def tested-nses '#{fhir-r4.Element us-core.us-core-patient})

  (def ns-deps '#{us-core.us-core-birthsex
                  us-core.us-core-ethnicity
                  us-core.us-core-race
                  us-core.us-core-genderIdentity
                  us-core.value-set.birthsex
                  us-core.value-set.detailed-ethnicity
                  us-core.value-set.detailed-race
                  us-core.value-set.omb-ethnicity-category
                  us-core.value-set.omb-race-category
                  us-nlm-vsac.value-set.2.16.840.1.113762.1.4.1021.32

                  fhir-r4.Address
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

  (t/testing "zen-packages"
    (t/testing "filter packages"

      (matcho/match
       (filter
        (partial sut/filter-zen-packages ztx {:package "fhir-r4"})
        (sut/collect-packages ztx))
       ["fhir-r4" nil])

      (matcho/match
       (filter
        (partial sut/filter-zen-packages ztx {})
        (sut/collect-packages ztx))
        [string? string? #_etc]))

    (t/testing "generate-package-config"
      (matcho/match
       (sut/generate-package-config
        ztx
        {:out-dir "/tmp"
         :package "fhir-r4"
         :git-url-format "https://github.com/zen-fhir/%s"
         :zen-fhir-lib-url (str (System/getProperty "user.dir") "/zen.fhir/")}
        "fhir-r4")
       {:out-dir "/tmp"
        :package "fhir-r4"
        :package-dir "/tmp/fhir-r4/"
        :package-git-url "https://github.com/zen-fhir/fhir-r4"
        :package-file-path "/tmp/fhir-r4/zen-package.edn"
        :package-file
        {:deps {'zen.fhir string?}}}))

    (t/testing "existing repo clone-zen-package"
      (delete-directory-recursive (io/file test-dir))
      (sh/sh "mkdir" "-p" test-dir)

      (matcho/match
       (sut/clone-zen-package
        {:package-git-url "https://github.com/HealthSamurai/zen-samples.git"
         :package-dir (str test-dir "/test-clone")})
       {:cloned? true}))

    (t/testing "non-existing repo clone-zen-package"
      (delete-directory-recursive (io/file test-dir))
      (sh/sh "mkdir" "-p" test-dir)

      (matcho/match
       (sut/clone-zen-package
        ;; TODO: username and password hack to avoid entering creds
        {:package-git-url "https://:@github.com/HealthSamurai/non-existing-i-hope.git"
         :package-dir (str test-dir "/test-clone")})
       {:cloned? false}))

    (t/testing "create-repo! post request"
      (require 'org.httpkit.client)

      (def module-dir (str test-dir "/fhir-r4"))

      (swap! ztx assoc-in [:env :github-token] "hello-world")
      (swap! ztx assoc :org-name "hs")

      (t/testing "create repo ok"
        (delete-directory-recursive (io/file test-dir))
        (sh/sh "mkdir" "-p" test-dir)

        (with-redefs [org.httpkit.client/post
                    (fn [url options]
                      (println url (:headers options))
                      (future (if (and (= url "https://api.github.com/orgs/hs/repos")
                                       (= (get-in options [:headers "Authorization"]) "token hello-world"))
                                {:status 200}
                                {:status 401
                                 :body "You shall not pass"})))]
          (->> {:cloned? false
                :out-dir test-dir
                :package "fhir-r4"
                :package-git-url "https://github.com/hs/fhir-r4"
                :package-dir (str test-dir "/fhir-r4")}
               (sut/init-zen-repo! ztx)
               (sut/create-remote! ztx)))

        (t/is (.exists (io/file module-dir)))
        (t/is
         "main"
         (->> (zen.package/sh! "git" "branch" "--show-current" :dir module-dir)
              :out
              str/trim-newline))

        (t/is (= 1 (->> (zen.package/sh! "git" "log" "--oneline" :dir module-dir)
                        :out
                        str/split-lines
                        (filter (partial re-find #"Init commit"))
                        count)))

        (t/is (= "https://github.com/hs/fhir-r4"
                 (->> (zen.package/sh! "git" "remote" "get-url" "origin" :dir module-dir)
                      :out
                      str/trim-newline))))

      (t/testing "create repo error"
        (delete-directory-recursive (io/file test-dir))
        (sh/sh "mkdir" "-p" test-dir)

        (swap! ztx assoc :org-name "nothing")

        (matcho/match
         (with-redefs [org.httpkit.client/post
                       (fn [url options]
                         (println url)
                         (future {:status 401
                                  :body "You shall not pass"}))]
           (unreduced (->> {:cloned? false
                            :out-dir test-dir
                            :package "fhir-r4"
                            :package-git-url (str test-dir "/fhir-r4")
                            :package-dir (str test-dir "/fhir-r4")}
                           (sut/init-zen-repo! ztx)
                           (sut/create-remote! ztx))))
         {:error {:status 401
                  :body "You shall not pass"}
          :cloned? false
          :out-dir string?
          :package string?
          :package-git-url string?
          :package-dir string?})))

    (t/testing "spit data in package"
      (delete-directory-recursive (io/file test-dir))
      (sh/sh "mkdir" "-p" test-dir)

      (def package-dir (str test-dir "/fhir-r4"))

      (sh/sh "mkdir" "-p" package-dir)
      (sh/sh "mkdir" "-p" (str package-dir "/zrc"))

      (let [config (sut/generate-package-config
                     ztx
                     {:out-dir test-dir
                      :package "fhir-r4"
                      :git-url-format (str "/tmp" "/%s")
                      :zen-fhir-lib-url (str (System/getProperty "user.dir") "/zen.fhir/")}
                     "fhir-r4")

            _ (sut/produce-ftr-manifests ztx config)]

        (sut/spit-data ztx config))

      (t/is (.exists (io/file (str test-dir "/fhir-r4/zrc/fhir-r4/Element.edn"))))
      (t/is (and (.exists (io/file (str test-dir "/fhir-r4/zen-package.edn")))
                 (let [package (-> (str test-dir "/fhir-r4/zen-package.edn")
                                   io/file
                                   slurp
                                   read-string)]
                   (matcho/match package
                                 {:deps {'zen.fhir string?}}))))

      (t/is (not (.exists (io/file (str test-dir "/us-core/zrc/us-core/us-core-patient.edn")))))

      (t/testing "FTR shaped correctly"
        (matcho/match
          (fs-tree->tree-map package-dir)
          {"fhir-r4-terminology-bundle.ndjson.gz" nil
           "ftr"
           {"ig"
            {"vs" {}
             "tags" {"init.ndjson.gz" {}}}}})))

    (t/testing "spit all packages data"
      (delete-directory-recursive (io/file test-dir))
      (sh/sh "mkdir" "-p" test-dir)

      (doseq [package-name (sut/collect-packages ztx)]
        (let [package (sut/generate-package-config
                        ztx
                        {:create-remote? false
                         :out-dir test-dir
                         :cloned? false
                         :git-url-format (str test-dir "/%s")
                         :zen-fhir-lib-url (str (System/getProperty "user.dir") "/zen.fhir/")}
                        package-name)]
          (->> package
               sut/clone-zen-package
               (sut/init-zen-repo! ztx)
               (sut/spit-data ztx)
               (sut/commit-zen-changes))))


      (t/is (.exists (io/file (str test-dir "/fhir-r4/.git"))))
      (t/is (.exists (io/file (str test-dir "/fhir-r4/zrc/fhir-r4/Element.edn"))))
      (t/is (and (.exists (io/file (str test-dir "/fhir-r4/zen-package.edn")))
                 (let [package (-> (str test-dir "/fhir-r4/zen-package.edn")
                                   io/file
                                   slurp
                                   read-string)]
                   (matcho/match package
                                 {:deps {'zen.fhir string?}}))))

      (t/is (.exists (io/file (str test-dir "/us-core/.git"))))
      (t/is (.exists (io/file (str test-dir "/us-core/zrc/us-core/us-core-patient.edn"))))
      (t/is (and (.exists (io/file (str test-dir "/us-core/zen-package.edn")))
                 (let [package (-> (str test-dir "/us-core/zen-package.edn")
                                   io/file
                                   slurp
                                   read-string)]
                   (matcho/match package
                                 {:deps {'zen.fhir string?
                                         'fhir-r4 (str test-dir "/fhir-r4")}})))))

    #_(t/testing "release packages" #_"NOTE: don't know how to test release, it's just git push"
      #_(delete-directory-recursive (io/file test-dir))
      #_(sh/sh "mkdir" "-p" test-dir)

      (swap! ztx assoc :org-name "hs")
      (swap! ztx assoc-in [:env :github-token] "hello-world")

      #_(with-redefs [org.httpkit.client/post
                    (fn [url options]
                      (future {:status 200}))]
        (sut/release-packages
         ztx
         {:out-dir test-dir
          :package "fhir-r4"
          :git-url-format (str test-dir "/%s")
          :zen-fhir-lib-url (str (System/getProperty "user.dir") "/zen.fhir/")}))))


  (t/testing "read zen package with deps"

    (def package-dir (str test-dir "/us-core"))

    (zen.package/zen-init-deps! package-dir)
    (def zctx (zen.core/new-context
               {:package-paths ["zen.fhir" package-dir]}))

    (def _ (zen.core/read-ns zctx 'us-core.us-core-patient))

    (t/is (empty? (:errors @zctx)))

    (t/is (every? #(contains? (:ns @zctx) %)
                  ['us-core.us-core-patient
                   'fhir-r4.Patient])))

  (t/testing "vs validation works"
    (ftr.zen-package/build-ftr-index zctx)

    (matcho/match (ftr.zen-package/validate zctx #{'fhir-r4.Patient/schema} {:gender "incorrect-value"})
                  {:errors [{:type ":zen.fhir/value-set"
                             :path [:gender nil]}
                            nil]})

    (matcho/match (ftr.zen-package/validate zctx #{'fhir-r4.Patient/schema} {:gender "male"})
                  {:errors empty?}))

  (def _ (reset! ztx og-ztx))

  :done)

