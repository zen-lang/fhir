(ns zen.fhir-light.loader-test
  (:require [cheshire.core :as json]
            [clojure.test :as t]
            [matcho.core :as matcho]
            [zen.core]
            [zen.fhir-light.loader :as sut]))


(defmethod zen.v2-validation/compile-type-check 'zen.fhir/element [_ _]
  (with-bindings {#'zen.v2-validation/types-cfg
                  {'zen.fhir/element
                   {:fn (some-fn map? vector?)
                    :to-str "object or 'object[]"}}}
    (zen.v2-validation/type-fn 'zen.fhir/element)))


(defn transpile-key-for-map-or-vector [_ ztx sch-for-map]
  (let [for-map (zen.v2-validation/get-cached ztx sch-for-map false)
        for-vector (zen.v2-validation/get-cached
                     ztx {:type 'zen/vector :every sch-for-map} false)]
    {:when (some-fn map? vector?)
     :rule (fn [vtx data opts]
             (if (vector? data)
               (for-vector vtx data opts)
               (for-map vtx data opts)))}))


(defmethod zen.v2-validation/compile-key :zen.fhir/require
  [k ztx require]
  (transpile-key-for-map-or-vector
    k ztx {:type 'zen/map :require require}))


(defmethod zen.v2-validation/compile-key :zen.fhir/elements
  [k ztx elements]
  (transpile-key-for-map-or-vector
    k ztx {:type 'zen/map :keys elements}))


(t/deftest ^:kaocha/pending convert-single-strdef-test
  (def dir (System/getProperty "user.dir"))

  (def fhir-core-ig-dir
    (str dir "/node_modules/hl7.fhir.r4.core"))

  (def us-core-ig-dir
    (str dir "/node_modules/hl7.fhir.us.core"))

  #_(json/parse-string (slurp (str us-core-ig-dir "/StructureDefinition-us-core-observation-sdoh-assessment.json"))
                       keyword)
  (def us-core-patient-str-def
    (json/parse-string (slurp (str us-core-ig-dir "/StructureDefinition-us-core-patient.json"))
                       keyword))
  #_(dissoc us-core-patient-str-def :text :snapshot)

  (t/testing "pure convertation, no context"
    (t/testing "keys grouping"
      (def el-res (map #'sut/group-element-keys (get-in us-core-patient-str-def [:differential :element])))

      (matcho/match
        el-res
        [{:loc {:id "Patient"                          :path "Patient"}                   :validation {:constraint [{} nil]}}
         {:loc {:id "Patient.extension:race"           :path "Patient.extension"}         :validation {:min 0 :max "1" :type [{:code "Extension" :profile [string?]}]}}
         {:loc {:id "Patient.extension:ethnicity"      :path "Patient.extension"}         :validation {:min 0 :max "1" :type [{:code "Extension" :profile [string?]}]}}
         {:loc {:id "Patient.extension:birthsex"       :path "Patient.extension"}         :validation {:min 0 :max "1" :type [{:code "Extension" :profile [string?]}]}}
         {:loc {:id "Patient.extension:genderIdentity" :path "Patient.extension"}         :validation {:min 0 :max "1" :type [{:code "Extension" :profile [string?]}]}}
         {:loc {:id "Patient.identifier"               :path "Patient.identifier"}        :validation {:min 1} :meta {:mustSupport true} :description {:mapping [{}]}}
         {:loc {:id "Patient.identifier.system"        :path "Patient.identifier.system"} :validation {:min 1}}
         {:loc {:id "Patient.identifier.value"         :path "Patient.identifier.value"}  :validation {:min 1}}
         {:loc {:id "Patient.name"                     :path "Patient.name"}              :validation {:min 1}}
         {:loc {:id "Patient.name.use"                 :path "Patient.name.use"}          :validation empty?}
         {:loc {:id "Patient.name.family"              :path "Patient.name.family"}       :validation {:condition ["us-core-6"]}}
         {:loc {:id "Patient.name.given"               :path "Patient.name.given"}        :validation {:condition ["us-core-6"]}}
         {:loc {:id "Patient.name.suffix"              :path "Patient.name.suffix"}       :validation empty?}]))

    (t/testing "parse id"
      (def enriched-res (map #'sut/enrich-loc el-res))

      (matcho/match
        enriched-res
        [{:loc {:id "Patient"                   ::sut/id [{:root "Patient" :type :root}]}}
         {:loc {:id "Patient.extension:race"    ::sut/id [{:root "Patient"}
                                                          {:key :extension :type :key}
                                                          {:slice "race"   :type :slice}]}}
         {} {} {} {}
         {:loc {:id "Patient.identifier.system" ::sut/id [{:root "Patient"}
                                                          {:key :identifier :type :key}
                                                          {:key :system     :type :key}]}}]))

    (t/testing "generate schema parts"
      (def schema-parts-res (map #'sut/add-schema-parts enriched-res)))

    (t/testing "nesting"
      (def nested-res (#'sut/nest-by-enriched-loc
                        schema-parts-res
                        :keys-to-strip #{:loc :description :meta :validation}))

      (matcho/match
        nested-res
        {:els {:extension {:slicing {:slices {"race" {}}}}
               :identifier {:els {:system {}}}
               :telecom {:els {:system {}}}}}))

    (t/testing "to zen"
      (def zen-sch (#'sut/nested->zen nested-res))

      (assert (= zen-sch (sut/strdef->zen us-core-patient-str-def)))

      (def ztx (zen.core/new-context {}))

      (zen.core/load-ns ztx '{:ns zen.fhir
                              element {:zen/tags #{zen/schema zen/is-type}
                                       :type zen/map}
                              elements {:zen/tags #{zen/is-key}
                                        :for #{element}
                                        :type zen/map
                                        :key {:type zen/keyword}
                                        :values {:confirms #{zen/schema}}}
                              max {:zen/tags #{zen/is-key}
                                   :for #{element}
                                   :type zen/integer
                                   :min 0}
                              min {:zen/tags #{zen/is-key}
                                   :for #{element}
                                   :type zen/integer
                                   :min 0}
                              collection {:zen/tags #{zen/is-key}
                                          :for #{element}
                                          :type zen/boolean}
                              forbid {:zen/tags #{zen/is-key}
                                      :for #{element}
                                      :type zen/set
                                      :every {:type zen/keyword}}
                              require {:zen/tags #{zen/is-key}
                                       :for #{element}
                                       :type zen/set
                                       :every {:type zen/keyword}}})

      (zen.core/load-ns ztx {:ns 'test-patient
                             :import #{'zen.fhir}
                             'schema zen-sch})

      (swap! ztx
             dissoc
             :errors
             :zen.v2-validation/compiled-schemas
             :zen.v2-validation/prop-schemas)

      (matcho/match (zen.core/errors ztx) empty?)

      (matcho/match (zen.core/validate ztx
                                       #{'test-patient/schema}
                                       "hello")
                    {:errors [{} nil]})

      (matcho/match (zen.core/validate ztx
                                       #{'test-patient/schema}
                                       [{}])
                    {:errors [{} {} {}]})

      (matcho/match (zen.core/validate ztx
                                       #{'test-patient/schema}
                                       {})
                    {:errors [{} {} {}]})

      (matcho/match (zen.core/validate ztx
                                       #{'test-patient/schema}
                                       {:name {:given ["1"]}
                                        :gender "unknown"
                                        :identifier [{:system "sys"
                                                      :value "val"}]
                                        :telecom [{:system "sys"
                                                   :value "val"}]})
                    {:errors empty?}))))


(t/deftest convert-many-strdef-test
  (t/is true))


(t/deftest convert-ig-package-test
  (t/is true))
