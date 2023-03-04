(ns zen.fhir-light.loader-test
  (:require [cheshire.core :as json]
            [clojure.test :as t]
            [matcho.core :as matcho]
            [zen.core]
            [zen.fhir-light.loader :as sut]
            [zen.fhir-light.loader.group :as sut.group]
            [zen.fhir-light.loader.nest :as sut.nest]
            [zen.fhir-light.loader.parse-id :as sut.parse-id]
            [zen.fhir-light.loader.to-zen :as sut.to-zen]
            [zen.v2-validation]))


(def zen-fhir-ns
  '{:ns zen.fhir
    type-binding {:zen/tags #{zen/schema zen/tag}
                  :require #{:fhirSequence :code}
                  :keys {:fhirVersion {:type zen/string}
                         :fhirSequence {:type zen/string}
                         :url {:type zen/string}
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
    (def ctx
      {:zf/strdef
       (#'sut.group/group-keys us-core-patient-str-def
                               sut.group/strdef-keys-types
                               nil)})

    (t/testing "keys grouping"
      (def el-res (map #(#'sut.group/group-keys % sut.group/elements-keys-types sut.group/elements-poly-keys-types)
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
      (def enriched-res (map #(sut.parse-id/enrich-loc ctx %) el-res))

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
      (def nested-res (#'sut.nest/nest-by-enriched-loc
                        enriched-res
                        :keys-to-strip #{:zf/loc :zf/description :zf/meta}))

      (matcho/match
        nested-res
        {:zf/els {:extension {:zf/slicing {:zf/slices {"race" {}}}}
                  :identifier {:zf/els {:system {}}}
                  :telecom {:zf/els {:system {}}}}}))))


(t/deftest convert-fhir-base-strdef-test
  (def dir (System/getProperty "user.dir"))

  (def fhir-core-ig-dir
    (str dir "/node_modules/hl7.fhir.r4.core"))

  (def us-core-ig-dir
    (str dir "/node_modules/hl7.fhir.us.core"))

  (def fhir-patient-str-def
    (json/parse-string (slurp (str fhir-core-ig-dir "/StructureDefinition-Patient.json"))
                       keyword))

  (def boolean-str-def
    (json/parse-string (slurp (str fhir-core-ig-dir "/StructureDefinition-boolean.json"))
                       keyword))

  (def us-core-patient-str-def
    (json/parse-string (slurp (str us-core-ig-dir
                                   "/StructureDefinition-us-core-patient.json"))
                       keyword))

  (t/testing "primitive type"
    (def boolean-sch (sut/strdef->zen-ns boolean-str-def))

    (matcho/match
      boolean-sch
      {'zen.fhir.bindings.system-type
       {'Boolean
        {:url "http://hl7.org/fhirpath/System.Boolean"
         :code "http://hl7.org/fhirpath/System.Boolean"}}

       'zen.fhir.bindings.fhir-r4.primitive-type
       {'boolean
        {:url "http://hl7.org/fhir/StructureDefinition/boolean"
         :code "boolean"}}

       'zen.fhir.fhir-r4.primitive-type.boolean
       {'schema
        {:zen/binding 'zen.fhir.bindings.fhir-r4.primitive-type/boolean
         :type 'zen.fhir/element
         :zen.fhir/el {:confirms #{'zen.fhir.bindings.system-type/Boolean}}}}}))

  (t/testing "base schema"
    (def fhir-patient-sch (sut/strdef->zen-ns fhir-patient-str-def))

    (matcho/match
      fhir-patient-sch
      {'zen.fhir.bindings.fhir-r4.complex-type
       {'HumanName
        {:zen/tags #{'zen/schema 'zen/binding 'zen.fhir/type-binding}
         :fhirVersion "4.0.1"
         :fhirSequence "r4"
         :url "http://hl7.org/fhir/StructureDefinition/HumanName"
         :code "HumanName"}}

       'zen.fhir.bindings.fhir-r4.structure
       {'Patient
        {:url "http://hl7.org/fhir/StructureDefinition/Patient"}}

       'zen.fhir.fhir-r4.resource.Patient
       {'schema
        {:zen/binding 'zen.fhir.bindings.fhir-r4.structure/Patient
         :type 'zen.fhir/element
         :zen.fhir/el
         {:type 'zen/map
          :keys
          {:name
           {:type 'zen.fhir/element
            :zen.fhir/collection true
            :zen.fhir/el {:confirms
                          #{'zen.fhir.bindings.fhir-r4.complex-type/HumanName}}}}}}}}))

  (t/testing "profile"
    (def us-patient-sch (sut/strdef->zen-ns us-core-patient-str-def))

    (matcho/match
      us-patient-sch
      {'zen.fhir.profiles.USCorePatientProfile.v5-0-1
       {'schema
        {:type 'zen.fhir/element
         :zen.fhir/el
         {:type 'zen/map
          :require #(contains? % :name)
          :keys {:name {:type 'zen.fhir/element
                        :zen.fhir/min 1}}}}}}))

  (t/testing "validaton"
    (def ztx (zen.core/new-context {}))

    (zen.core/load-ns ztx zen-fhir-ns)

    (doseq [bindings-ns (vals us-patient-sch)]
      (zen.core/load-ns ztx bindings-ns))

    (swap! ztx
           dissoc
           :errors
           :zen.v2-validation/compiled-schemas
           :zen.v2-validation/prop-schemas)

    (matcho/match
      (zen.core/errors ztx)
      [{:type :unbound-binding}
       {:type :unbound-binding}
       nil])

    (matcho/match
      (zen.core/validate ztx
                         #{'zen.fhir.profiles.USCorePatientProfile.v5-0-1/schema}
                         "hello")
      {:errors [{} nil]})

    (matcho/match
      (zen.core/validate ztx
                         #{'zen.fhir.profiles.USCorePatientProfile.v5-0-1/schema}
                         [{}])
      {:errors [{} {} {}]})

    (matcho/match
      (zen.core/validate ztx
                         #{'zen.fhir.profiles.USCorePatientProfile.v5-0-1/schema}
                         {})
      {:errors [{} {} {}]})

    (matcho/match
      (zen.core/validate ztx
                         #{'zen.fhir.profiles.USCorePatientProfile.v5-0-1/schema}
                         {:name {}
                          :gender "unknown"
                          :identifier [{:system "sys"
                                        :value "val"}]
                          :telecom [{:system "sys"
                                     :value "val"}]})
      {:errors [{:path [:name nil]}
                nil]})))


(comment

  (defn- merge-with* [path f & maps]
    (when (some identity maps)
      (let [merge-entry (fn [m e]
			                    (let [k (key e) v (val e)]
			                      (if (contains? m k)
			                        (assoc m k (f (conj path k) (get m k) v))
			                        (assoc m k v))))
            merge2 (fn [m1 m2]
		                 (reduce merge-entry (or m1 {}) (seq m2)))]
        (reduce merge2 maps))))

  (defn- safe-deep-merge*
    [path a b]
    (cond
      (= a b)
      a

      (and (set? a) (set? b))
      (clojure.set/union a b)

      (and (map? a) (map? b)
           (= "4.0.0" (:fhirVersion a)) (= "4.0.1" (:fhirVersion b)))
      b

      (and (map? a) (map? b)
           (= "4.0.1" (:fhirVersion a)) (= "4.0.0" (:fhirVersion b)))
      a

      (and (or (nil? a) (map? a)) (or (nil? b) (map? b)))
      (merge-with* path safe-deep-merge* a b)

      :foo
      b

      :else
      (throw (ex-info "Can't merge not maps. Overwriting values is not allowed"
                      {:path path
                       :a a
                       :b b}))))

  (defn- safe-deep-merge
    ([] nil)
    ([a] a)
    ([a b] (safe-deep-merge* [] a b))
    ([a b & maps]
     (reduce safe-deep-merge
             a
             (cons b maps))))

  (def strdefs
    (->> (concat (file-seq (clojure.java.io/file us-core-ig-dir))
                 (file-seq (clojure.java.io/file fhir-core-ig-dir)))
         (filter #(clojure.string/starts-with? (.getName %)
                                               "StructureDefinition"))
         (map slurp)
         (keep #(try (json/parse-string % keyword)
                     (catch Exception _)))
         (filter #(= "StructureDefinition" (:resourceType %)))))

  (def zen-namespaces
    (map sut/strdef->zen-ns strdefs))

  (def merged
    (apply safe-deep-merge zen-namespaces))

  (do
    (def gztx (zen.core/new-context {}))

    (doseq [zen-ns (vals merged)]
      (zen.core/load-ns ztx zen-ns))

    (swap! ztx dissoc :errors)

    (doseq [zen-ns (vals merged)]
      (zen.core/load-ns ztx zen-ns)))

  (get (group-by :url strdefs)
       "http://hl7.org/fhir/StructureDefinition/boolean")

  (get (group-by :url strdefs)
       "http://hl7.org/fhir/StructureDefinition/MedicinalProductPackaged")

  (get (group-by :url strdefs)
       "http://hl7.org/fhir/StructureDefinition/ProductShelfLife")

  nil)


(t/deftest convert-many-strdef-test
  (t/is true))


(t/deftest convert-ig-package-test
  (t/is true))
