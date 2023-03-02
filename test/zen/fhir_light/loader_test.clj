(ns zen.fhir-light.loader-test
  (:require [cheshire.core :as json]
            [clojure.test :as t]
            [matcho.core :as matcho]
            [zen.core]
            [zen.v2-validation]
            [zen.fhir-light.loader :as sut]))


(def zen-fhir-ns
  '{:ns zen.fhir
    type-binding {:zen/tags #{zen/schema zen/tag}
                  :require #{:fhirSequence :code}
                  :keys {:fhirVersion {:type zen/string}
                         :fhirSequence {:type zen/string}
                         :code {:type zen/string}}}
    element {:zen/tags #{zen/schema zen/is-type}
             :type zen/map}
    el {:zen/tags #{zen/is-key}
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



#_"NOTE: emptiness check as per https://hl7.org/fhir/json.html"
(defmethod zen.v2-validation/compile-type-check 'zen.fhir/element [tp _]
  (fn [vtx data _opts]
    (if (and (seqable? data) (empty? data))
      (let [error-msg (cond (string? data) "string"
                            (map? data)    "object"
                            :else          "collection")
            error {:message (str "'" tp " " error-msg "s can not be empty")
                   :type    "type"}]
        (zen.v2-validation/add-err vtx :type error))
      vtx)))


(defn transpile-key-for-map-or-vector [_ ztx sch-for-scalar]
  (let [for-scalar (zen.v2-validation/get-cached ztx sch-for-scalar false)
        for-vector (zen.v2-validation/get-cached
                     ztx {:type 'zen/vector :every sch-for-scalar} false)]
    {:rule (fn [vtx data opts]
             (if (vector? data)
               (for-vector vtx data opts)
               (for-scalar vtx data opts)))}))


(defmethod zen.v2-validation/compile-key :zen.fhir/el
  [k ztx elements]
  (transpile-key-for-map-or-vector
    k ztx elements))


(t/deftest convert-single-strdef-test
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
      (def el-res (map #(#'sut/group-keys % sut/elements-keys-types sut/elements-poly-keys-types)
                       (get-in us-core-patient-str-def [:differential :element])))

      (matcho/match
        el-res
        [{:zf/loc {:path string? :id "Patient"}}
         {:zf/loc {:path string? :id "Patient.extension:race"}}
         {:zf/loc {:path string? :id "Patient.extension:ethnicity"}}
         {:zf/loc {:path string? :id "Patient.extension:birthsex"}}
         {:zf/loc {:path string? :id "Patient.extension:genderIdentity"}}
         {:zf/loc {:path string? :id "Patient.identifier"}}
         {:zf/loc {:path string? :id "Patient.identifier.system"}}
         {:zf/loc {:path string? :id "Patient.identifier.value"}}
         {:zf/loc {:path string? :id "Patient.name"}}
         {:zf/loc {:path string? :id "Patient.name.use"}}
         {:zf/loc {:path string? :id "Patient.name.family"}}
         {:zf/loc {:path string? :id "Patient.name.given"}}
         {:zf/loc {:path string? :id "Patient.name.suffix"}}]))

    (t/testing "parse id"
      (def enriched-res (map #'sut/enrich-loc el-res))

      (matcho/match
        enriched-res
        [{:zf/loc {:id "Patient"                   :zf/id [{:root "Patient" :type :root}]}}
         {:zf/loc {:id "Patient.extension:race"    :zf/id [{:root "Patient"}
                                                           {:key :extension :type :key}
                                                           {:slice "race"   :type :slice}]}}
         {} {} {} {}
         {:zf/loc {:id "Patient.identifier.system" :zf/id [{:root "Patient"}
                                                           {:key :identifier :type :key}
                                                           {:key :system     :type :key}]}}]))

    (t/testing "nesting"
      (def nested-res (#'sut/nest-by-enriched-loc
                        enriched-res
                        :keys-to-strip #{:zf/loc :zf/description :zf/meta}))

      (matcho/match
        nested-res
        {:zf/els {:extension {:zf/slicing {:zf/slices {"race" {}}}}
                  :identifier {:zf/els {:system {}}}
                  :telecom {:zf/els {:system {}}}}}))

    (t/testing "to zen"
      (def zen-sch (#'sut/nested->zen {:zf/strdef
                                       (#'sut/group-keys us-core-patient-str-def
                                                         sut/strdef-keys-types
                                                         nil)}
                                      nested-res))

      (t/is (= (:zf/schema zen-sch)
               (:zf/schema (sut/strdef->zen-ns us-core-patient-str-def)))))))


(t/deftest convert-fhir-base-strdef-test
  (def dir (System/getProperty "user.dir"))

  (def fhir-core-ig-dir
    (str dir "/node_modules/hl7.fhir.r4.core"))

  (def us-core-ig-dir
    (str dir "/node_modules/hl7.fhir.us.core"))

  (def fhir-patient-str-def
    (json/parse-string (slurp (str fhir-core-ig-dir "/StructureDefinition-Patient.json"))
                       keyword))

  (def us-core-patient-str-def
    (json/parse-string (slurp (str us-core-ig-dir "/StructureDefinition-us-core-patient.json"))
                       keyword))

  (def fhir-patient-sch (sut/strdef->zen-ns fhir-patient-str-def))

  #_{:type 'zen/map
   :keys {:given {:type 'zen.fhir/element
                  :zen.fhir/collection true
                  :zen.fhir/el
                  {:type 'zen/string}}}}

  (matcho/match
    fhir-patient-sch
    {:zf/bindings
     {'zen.fhir.bindings.fhir-r4.types/HumanName
      {:zen/tags #{'zen/schema 'zen/binding 'zen.fhir/type-binding}
       :fhirVersion "4.0.1"
       :fhirSequence "r4"
       :code "HumanName"}}
     :zf/schema
     {:type 'zen.fhir/element
      :zen.fhir/el
      {:type 'zen/map
       :keys {:name {:type 'zen.fhir/element
                     :zen.fhir/collection true
                     :zen.fhir/el {:confirms
                                   #{'zen.fhir.bindings.fhir-r4.types/HumanName}}}}}}})

  (def us-patient-sch (sut/strdef->zen-ns us-core-patient-str-def))

  (matcho/match
    us-patient-sch
    {:zf/schema
     {:type 'zen.fhir/element
      :zen.fhir/el
      {:type 'zen/map
       :require #(contains? % :name)
       :keys {:name {:type 'zen.fhir/element
                     :zen.fhir/min 1}}}}})

  (def ztx (zen.core/new-context {}))

  (zen.core/load-ns ztx zen-fhir-ns)

  (doseq [bindings-ns (:zf/bindings-ns us-patient-sch)]
    (zen.core/load-ns ztx bindings-ns))

  (zen.core/load-ns ztx {:ns 'test-patient
                         :import (into #{'zen.fhir}
                                       (map :ns)
                                       (:zf/bindings us-patient-sch))
                         'schema (:zf/schema us-patient-sch)})

  (swap! ztx
         dissoc
         :errors
         :zen.v2-validation/compiled-schemas
         :zen.v2-validation/prop-schemas)

  (matcho/match (zen.core/errors ztx) [{:type :unbound-binding} nil])

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
                                   {:name {}
                                    :gender "unknown"
                                    :identifier [{:system "sys"
                                                  :value "val"}]
                                    :telecom [{:system "sys"
                                               :value "val"}]})
                {:errors [{:path [:name nil]}
                          nil]}))


(comment
  (def strdefs
    (->> (concat (file-seq (clojure.java.io/file us-core-ig-dir))
                 (file-seq (clojure.java.io/file fhir-core-ig-dir)))
         (filter #(clojure.string/starts-with? (.getName %)
                                               "StructureDefinition"))
         (map slurp)
         (keep #(try (json/parse-string % keyword)
                     (catch Exception _)))
         (filter #(= "StructureDefinition" (:resourceType %)))))


  (def schemas
    (into {}
          (map (juxt :url sut/strdef->zen-ns))
          strdefs)))


(t/deftest convert-many-strdef-test
  (t/is true))


(t/deftest convert-ig-package-test
  (t/is true))
