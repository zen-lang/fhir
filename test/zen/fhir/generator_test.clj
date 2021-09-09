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


(t/deftest generate-project-integration
  (def ztx  (zen.core/new-context {}))

  (t/testing "generating zen"
    (zen.fhir.core/load-all ztx "hl7.fhir.us.core"
                            {:params {"hl7.fhir.r4.core" {:zen.fhir/package-ns 'fhir-r4}
                                      "hl7.fhir.us.core" {:zen.fhir/package-ns 'us-core-v3}
                                      "hl7.fhir.us.carin-bb" {:zen.fhir/package-ns 'carin-bb-v1}}})

    #_(get-in @ztx [:fhir/inter "StructureDefinition" "http://hl7.org/fhir/StructureDefinition/Resource"])

    (t/is (= :done (sut/generate-zen-schemas ztx)))

    (matcho/match
      (:fhir.zen/ns @ztx)
      {'fhir-r4.string
       {'ns     'fhir-r4.string
        'schema {:zen/tags #{'zen/schema 'zenbox/structure-schema}
                 :confirms #(not (contains? % 'fhir-r4.Element/schema))
                 :type 'zen/string}}

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

       'fhir-r4.Patient
       {'ns     'fhir-r4.Patient
        'import #(and (contains? % 'fhir-r4.DomainResource)
                      (contains? % 'zenbox))
        'schema {:zen/tags #{'zen/schema 'zenbox/base-schema}
                 :confirms #{'fhir-r4.DomainResource/schema 'zenbox/Resource}
                 :type 'zen/map
                 :zenbox/type "Patient"
                 :zenbox/profileUri "http://hl7.org/fhir/StructureDefinition/Patient"
                 :keys {:name {:type 'zen/vector
                               :every {:confirms #{'fhir-r4.HumanName/schema}}}
                        :active {:confirms #{'fhir-r4.boolean/schema}}
                        :deceased {:type 'zen/map
                                   :keys {:boolean {:confirms #{'fhir-r4.boolean/schema}}
                                          :dateTime {:confirms #{'fhir-r4.dateTime/schema}}}}
                        :managingOrganization {:confirms #{'fhir-r4.Reference/schema 'zenbox/Reference}
                                               :zenbox/refers #{'fhir-r4.Organization/schema}}
                        :link {:type 'zen/vector
                               :every {:require #{:other :type}}}}}}

       'us-core-v3.us-core-patient
       {'ns     'us-core-v3.us-core-patient
        'import #(and (contains? % 'fhir-r4.Patient)
                      (contains? % 'zenbox))
        'schema {:zen/tags #{'zen/schema 'zenbox/profile-schema}
                 :confirms #{'fhir-r4.Patient/schema 'zenbox/Resource}
                 :type 'zen/map
                 :zenbox/type "Patient"
                 :zenbox/profileUri "http://hl7.org/fhir/us/core/StructureDefinition/us-core-patient"
                 :require #{:name :gender :identifier}
                 :keys {:race      {:confirms #{'us-core-v3.us-core-race/schema}}
                        :ethnicity {:confirms #{'us-core-v3.us-core-ethnicity/schema}}
                        :birthsex  {:confirms #{'us-core-v3.us-core-birthsex/schema}}
                        :identifier {:type     'zen/vector
                                     :minItems 1
                                     :every    {:confirms #{'fhir-r4.Identifier/schema}
                                                :type 'zen/map
                                                :keys {:system {:confirms #{'fhir-r4.uri/schema}}
                                                       :value  {:confirms #{'fhir-r4.string/schema}}}}}}}}}))

  (t/testing "spit"
    (t/testing "zen-schemas"
      (delete-directory-recursive (io/file "test-temp-zrc"))

      (t/is (= :done (sut/spit-zen-schemas ztx "test-temp-zrc/")))

      (t/is (.exists (io/file "test-temp-zrc/fhir-r4/Element.edn")))
      (t/is (.exists (io/file "test-temp-zrc/fhir-r4/Resource.edn")))
      (t/is (.exists (io/file "test-temp-zrc/fhir-r4/DomainResource.edn")))
      (t/is (.exists (io/file "test-temp-zrc/fhir-r4/Patient.edn")))
      (t/is (.exists (io/file "test-temp-zrc/us-core-v3/us-core-patient.edn"))))


    (t/testing "zen-npm-modules"
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

      (t/is (not (.exists (io/file "test-temp-zrc/node_modules/us-core-v3/us-core-v3/us-core-patient.edn"))))

      (t/is (= :done (sut/spit-zen-npm-modules ztx "test-temp-zrc/node_modules/" "0.0.1-test")))

      (t/is (.exists (io/file "test-temp-zrc/node_modules/fhir-r4/fhir-r4/Element.edn")))
      (t/is (.exists (io/file "test-temp-zrc/node_modules/us-core-v3/us-core-v3/us-core-patient.edn")))
      (t/is (and (.exists (io/file "test-temp-zrc/node_modules/us-core-v3/package.json"))
                 (let [package (-> "test-temp-zrc/node_modules/us-core-v3/package.json"
                                   io/file
                                   slurp
                                   (json/parse-string keyword))]
                   (matcho/match package
                                 {:name "@zen-lang/us-core-v3"
                                  :version "0.0.1-test"}))))))

  (t/testing "zen validation"
    (def ztx (zen.core/new-context
               {:paths ["test-temp-zrc/"]
                :memory-store
                '{zenbox
                  {ns zenbox

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

                   nested-schema
                   {:zen/tags #{zen/schema}
                    :type zen/map
                    :keys {:fhir/flags {:type zen/set}
                           :zenbox/refers {:type zen/set
                                           :every {:type zen/symbol
                                                   #_#_:tags #{#{zenbox/base-schema zenbox/profile-schema}}}} ;; TODO
                           :keys {:type zen/map
                                  :values {:confirms #{nested-schema}}}
                           :every {:confirms #{nested-schema}}}}

                   structure-schema
                   {:zen/tags #{zen/schema zen/tag}
                    :type     zen/map
                    :keys     {:zenbox/type {:type zen/string}
                               :zenbox/profileUri {:type zen/string}
                               :keys {:type zen/map
                                      :values {:confirms #{nested-schema}}}}}

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
                    :require  #{:zenbox/profileUri}}}}}))

    (zen.core/read-ns ztx 'us-core-v3.us-core-patient)

    #_(zen.core/get-symbol ztx 'us-core-v3.us-core-patient/schema)

    (t/is (empty? (:errors @ztx)))

    (t/is (every? #(contains? (:ns @ztx) %)
                  ['us-core-v3.us-core-patient
                   'fhir-r4.Patient]))

    (t/is (empty? (:errors (zen.core/validate ztx '#{fhir-r4.Patient/schema} {}))))

    (def fhir-pat (read-string (slurp (io/resource "zen/fhir/aidbox-fhir-r4-patient-example.edn"))))

    (t/is (empty? (:errors (zen.core/validate ztx '#{fhir-r4.Patient/schema} fhir-pat))))

    (matcho/match
      (:errors (zen.core/validate ztx '#{us-core-v3.us-core-patient/schema} {}))
      [{:path [:name], :type "require"}
       {:path [:identifier], :type "require"}
       {:path [:gender], :type "require"}
       nil])))
