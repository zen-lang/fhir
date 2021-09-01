(ns zen.fhir.generator-test
  (:require
   [zen.fhir.generator :as sut]
   [zen.fhir.core]
   [zen.core]
   [matcho.core :as matcho]
   [clojure.test :as t]
   [clojure.java.io :as io]
   [cheshire.core :as json]))


(defn delete-directory-recursive
  [^java.io.File file]
  (when (.isDirectory file)
    (doseq [file-in-dir (.listFiles file)]
      (delete-directory-recursive file-in-dir)))
  (io/delete-file file true))


(t/deftest ^:kaocha/pending generate-project-integration
  (def ztx  (zen.core/new-context {}))

  (zen.fhir.core/load-all ztx "hl7.fhir.us.core"
                          {:params {"hl7.fhir.r4.core" {:zen.fhir/package-ns 'fhir-r4}
                                    "hl7.fhir.us.core" {:zen.fhir/package-ns 'us-core-v3}
                                    "hl7.fhir.us.carin-bb" {:zen.fhir/package-ns 'carin-bb-v1}}})

  (get-in @ztx [:fhir/inter "StructureDefinition" "http://hl7.org/fhir/StructureDefinition/patient-nationality"])
  (get-in @ztx [:fhir/inter "StructureDefinition" "http://hl7.org/fhir/StructureDefinition/string"])

  (t/is (= :done (sut/generate-zen-schemas ztx)))

  (matcho/match
    (:fhir.zen/ns @ztx)
    {'fhir-r4.string
     {'ns     'fhir-r4.string
      'schema {:zen/tags #{'zen/schema}
               :confirms #(not (contains? % 'fhir-r4.Element/schema))
               :type 'zen/string}}

     'fhir-r4.Element
     {'ns     'fhir-r4.Element
      'schema {:zen/tags #{'zen/schema}
               :type     'zen/map
               :keys     {:id        {:confirms #{'fhir-r4.string/schema}}
                          :extension {:type  'zen/vector
                                      :every {:confirms #{'fhir-r4.Extension/schema}}}}}}

     'fhir-r4.Resource
     {'ns     'fhir-r4.Resource
      'schema {:zen/tags #{'zen/schema}
               :type 'zen/map
               :keys {:id            {:confirms #{'fhir-r4.string/schema}}
                      :meta          {:confirms #{'fhir-r4.Meta/schema}}
                      :implicitRules {:confirms #{'fhir-r4.uri/schema}}
                      :language      {:confirms #{'fhir-r4.code/schema}}}}}

     'fhir-r4.DomainResource
     {'ns     'fhir-r4.DomainResource
      'import #(contains? % 'fhir-r4.Resource)
      'schema {:confirms #{'fhir-r4.Resource/schema}
               :type 'zen/map
               :keys {:text              {:confirms #{'fhir-r4.Narrative/schema}}
                      :contained         {:type  'zen/vector
                                          :every {:confirms #{'fhir-r4.Resource/schema}}}
                      :extension         {:type  'zen/vector
                                          :every {:confirms #{'fhir-r4.Extension/schema}}}
                      :modifierExtension {:type  'zen/vector
                                          :every {:confirms #{'fhir-r4.Extension/schema}}}}}}

     'fhir-r4.Patient
     {'ns     'fhir-r4.Patient
      'import #(and (contains? % 'fhir-r4.DomainResource)
                    (contains? % 'zenbox))
      'schema {:zen/tags #{'zen/schema 'zenbox/base-schema}
               :confirms #{'fhir-r4.DomainResource/schema}
               :type 'zen/map
               :zenbox/resourceType "Patient"
               :zenbox/profileUrl "http://hl7.org/fhir/StructureDefinition/Patient"
               :keys {:name {:type 'zen/vector
                             :every {:confirms #{'fhir-r4.HumanName/schema}}}
                      :active {:confirms #{'fhir-r4.boolean/schema}}
                      :deceased {:type 'zen/map
                                 :keys {:boolean {:confirms #{'fhir-r4.boolean/schema}}
                                        :dateTime {:confirms #{'fhir-r4.dateTime/schema}}}}}}}

     'us-core-v3.us-core-patient
     {'ns     'us-core-v3.us-core-patient
      'import #(and (contains? % 'fhir-r4.Patient)
                    (contains? % 'zenbox))
      'schema {:zen/tags #{'zen/schema 'zenbox/profile-schema}
               :confirms  #{'fhir-r4.Patient/schema}
               :type 'zen/map
               :zenbox/resourceType "Patient"
               :zenbox/profileUrl "http://hl7.org/fhir/us/core/StructureDefinition/us-core-patient"
               :keys {:race      {:confirms #{'us-core-v3.us-core-race/schema}}
                      :ethnicity {:confirms #{'us-core-v3.us-core-ethnicity/schema}}
                      :birthsex  {:confirms #{'us-core-v3.us-core-birthsex/schema}}
                      :identifier {:type     'zen/vector
                                   :minItems 1
                                   :every    {:confirms #{'fhir-r4.Identifier/schema}
                                              :type 'zen/map
                                              :keys {:system {:confirms #{'fhir-r4.uri/schema}}
                                                     :value  {:confirms #{'fhir-r4.string/schema}}}}}}}}})


  (delete-directory-recursive (io/file "test-temp-zrc"))

  (t/is (= :done (sut/spit-zen-schemas ztx "test-temp-zrc/")))

  (t/is (.exists (io/file "test-temp-zrc/fhir-r4/Element.edn")))
  (t/is (.exists (io/file "test-temp-zrc/fhir-r4/Resource.edn")))
  (t/is (.exists (io/file "test-temp-zrc/fhir-r4/DomainResource.edn")))
  (t/is (.exists (io/file "test-temp-zrc/fhir-r4/Patient.edn")))
  (t/is (.exists (io/file "test-temp-zrc/us-core-v3/us-core-patient.edn")))


  (t/is (= :done (sut/spit-zen-npm-modules ztx "test-temp-zrc/node_modules/")))

  (t/is (.exists (io/file "test-temp-zrc/node_modules/fhir-r4/Element.edn")))
  (t/is (and (.exists (io/file "test-temp-zrc/node_modules/fhir-r4/package.json"))
             (let [package (-> "test-temp-zrc/node_modules/fhir-r4/package.json"
                               io/file
                               slurp
                               (json/parse-string keyword))]
               (= "fhir-r4" (:name package)))))


  (t/is (.exists (io/file "test-temp-zrc/node_modules/us-core-v3/us-core-patient.edn")))
  (t/is (and (.exists (io/file "test-temp-zrc/node_modules/us-core-v3/package.json"))
             (let [package (-> "test-temp-zrc/node_modules/us-core-v3/package.json"
                               io/file
                               slurp
                               (json/parse-string keyword))]
               (= "us-core-v3" (:name package)))))

  (def ztx (zen.core/new-context
             {:paths "test-temp-zrc/"
              :memory-store '{zenbox
                              {ns zenbox

                               resource-schema
                               {:zen/tags #{zen/schema}
                                :type     zen/map
                                :keys     {:zenbox/resourceType {:type zen/string}
                                           :zenbox/profileUrl {:type zen/string}}}

                               base-schema
                               {:zen/tags #{zen/schema zen/tag}
                                :zen/desc "This schema should be used to validate all resources of its type"
                                :confirms #{resource-schema}}

                               profile-schema
                               {:zen/tags #{zen/schema zen/tag}
                                :zen/desc "This schema should be used only when mentioned in meta.profile"
                                :confirms #{resource-schema}}}}}))

  (zen.core/read-ns ztx 'us-core-v3.us-core-patient)

  (t/is (empty? (:errors @ztx)))

  (t/is (empty? (:errors (zen.core/validate ztx '#{fhir-r4.Patient/schema} {}))))

  (def fhir-pat (read-string (slurp (io/resource "zen/fhir/aidbox-fhir-r4-patient-example.edn"))))

  (t/is (empty? (:errors (zen.core/validate ztx '#{fhir-r4.Patient/schema} fhir-pat))))

  (t/is (empty? (:errors (zen.core/validate ztx '#{us-core-v3.us-core-patient/schema} {}))))

  )
