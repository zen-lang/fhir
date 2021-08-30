(ns zen.fhir.core-test
  (:require [zen.fhir.core :as sut]
            [clojure.test :as t]
            [zen.core]
            [clojure.pprint]
            [matcho.core :as matcho]
            [hiccup.core :as hiccup]
            [clojure.java.io :as io]))

(defn inspect [file data]
  (spit file (with-out-str (clojure.pprint/pprint data))))

(defmulti render-data (fn [x & [opts]] (type x)))

(defmethod render-data :default
  [x & [opts]]
  [:span (with-out-str (clojure.pprint/pprint x))])

(defn primitive? [x]
  (or (number? x) (string? x) (keyword? x) (boolean? x) (set? x)))

(defn render-map [x & [opts]]
  (into [:div.block]
        (for [[k v] (sort-by first x)]
          (if (primitive? v)
            [:div [:b.key (str k)] (render-data v)]
            [:details (cond-> {:style "display: flex;"}
                        (and (or (= k :|) (:| v))
                             (not (:closed opts))) (assoc :open "open"))
             [:summary [:b.key (str k)]]
             (render-data v)]))))

(defmethod render-data
  clojure.lang.PersistentArrayMap
  [x & [opts]] (render-map x opts))

(defmethod render-data
  clojure.lang.PersistentHashMap
  [x & [opts]] (render-map x opts))

(defmethod render-data
  clojure.lang.PersistentVector
  [x & [opts]]
  (into [:div.block "[" (count x)]
        (->> x
             (map-indexed
               (fn [i v]
                 [:details
                  [:summary [:b i]]
                  (render-data v)])))))




;; (defn inspect [file data]
;;   (spit file (with-out-str (clojure.pprint/pprint data))))

(def css
  "
body {font-family: Geneva, Arial, Helvetica, sans-serif; background-color: #282a36; color: #bfcd70;}
.block {padding-left: 1rem;}
.key {color: #fe7ac6; padding-right: 0.5rem; cursor: pointer;}
.key:hover {color: white;}"
  )

(defn inspect [file data & [opts]]
  (spit file (hiccup/html
               [:html [:head [:style css]]
                [:body (render-data data opts)]])))

(t/deftest element-normalization
  (t/testing "cardinality"
    (matcho/match (-> {:id "a", :min 1, :max "1", :base {:max "1"}}
                      sut/normalize-element)
                  {:required true?, :vector not})

    (matcho/match (-> {:id "b", :min 1, :max "1", :base {:max "*"}}
                      sut/normalize-element)
                  {:required true?, :vector not})

    (matcho/match (-> {:id "a.c", :min 1, :max "1", :base {:max "1"}}
                      sut/normalize-element)
                  {:required true?, :vector not})

    ;; (matcho/match (-> {:id "a.d", :min 1, :max "1", :base {:max "*"}}
    ;;                   sut/normalize-element)
    ;;               {:required true?, :vector true?})

    ;; (matcho/match (-> {:id "a.d", :min 1, :max "1", :base {:max "*"}}
    ;;                   sut/normalize-element)
    ;;               {:required true?, :vector true?})

    (matcho/match (-> {:id "a.b", :min 0, :max "*"}
                      sut/normalize-element)
                  {:required not, :vector true?})

    (matcho/match (-> {:id "a.b.c", :min 0}
                      sut/normalize-element)
                  {:required not, :vector not})

    (matcho/match (-> {:id "a.b.d", :min 0, :max "1"}
                      sut/normalize-element)
                  {:required not, :vector not})

    (matcho/match (-> {:id "a.b.e", :min 1, :max "2", :base {:max "*"}}
                      sut/normalize-element)
                  {:required true?, :vector true?, :minItems 1, :maxItems 2})

    ;; (matcho/match (-> {:id "a.b.f", :max "0", :base {:max "*"}}
    ;;                   sut/normalize-element)
    ;;               {:required not, :vector true?, :maxItems 0})

    ;; (matcho/match (-> {:id "a.b.f", :max "0", :base {:max "1"}}
    ;;                   sut/normalize-element)
    ;;               {:required not, :vector not, :prohibited true?})
    ))


(t/deftest arity-test
  (def aztx (zen.core/new-context {}))

  (sut/load-definiton
    aztx {}
    {:url "http://hl7.org/fhir/StructureDefinition/Complex"}
    {:resourceType "StructureDefinition"
     :type         "Complex"
     :derivation   "specialization"
     :kind "complex-type"
     :differential
     {:element
      [{:id "Complex"}
       {:id "Complex.attr" :min 0 :max "1" :type [{:code "prim"}]}
       {:id "Complex.array" :min 0 :max "*" :type [{:code "prim"}]}
       {:id "Complex.nested" :min 0 :max "1"}
       {:id "Complex.nested.attr" :min 0 :max "1" :type [{:code "prim"}]}]}})

  (sut/load-definiton
    aztx {}
    {:url "http://hl7.org/fhir/StructureDefinition/TestBaseResourceType"}
    {:resourceType "StructureDefinition"
     :url          "http://hl7.org/fhir/StructureDefinition/TestBaseResourceType"
     :type         "TestBaseResourceType"
     :derivation   "specialization"
     :differential
     {:element
      [{:id "TestBaseResourceType"}
       {:id "TestBaseResourceType.singular" :min 0 :max "1"}
       {:id "TestBaseResourceType.singular.array" :min 0 :max "*"}
       {:id "TestBaseResourceType.type" :min 0 :max "*" :type [{:code "Complex"}]}
       {:id "TestBaseResourceType.poly[x]" :min 0 :max "1" :type [{:code "Complex"}]}
       ;; {:id "TestBaseResourceType.poly[x]:polyComplex" :min 0 :max "1"}
       ;; {:id "TestBaseResourceType.poly[x]:polyComplex.array" :min 0 :max "*"}
       ]}})

  (sut/load-definiton
    aztx {}
    {:url "http://hl7.org/fhir/StructureDefinition/TestConstraint"}
    {:resourceType  "StructureDefinition"
     :url           "http://hl7.org/fhir/StructureDefinition/TestConstraint"
     :baseDefiniton "http://hl7.org/fhir/StructureDefinition/SomeOtherTestConstraint"
     :type          "TestBaseResourceType"
     :derivation    "constraint"
     :differential
     {:element
      [{:id "TestConstraint"}
       {:id "TestConstraint.singular"}
       {:id "TestConstraint.singular.array" :min 1 :max "1"}
       {:id "TestConstraint.type"}
       {:id "TestConstraint.type.array" :max "1"}
       {:id "TestConstraint.polyComplex"}
       {:id "TestConstraint.polyComplex.array" :min 1 :max "1"}]}})


  (sut/load-definiton
    aztx {}
    {:url "race"}
    {:type "Extension",
     :kind "complex-type",
     :url  "race"
     :id   "us-core-race",
     :resourceType "StructureDefinition"
     :differential
     {:element
      [{:id "Extension", :path "Extension", :min 0, :max "1"}
       {:id        "Extension.extension:ombCategory",
        :type      [{:code "Extension"}],
        :sliceName "ombCategory",
        :min       0, :max       "5"}
       {:id       "Extension.extension:ombCategory.url",
        :type     [{:code "uri"}],
        :min      1, :max      "1",
        :fixedUri "ombCategory"}
       {:id   "Extension.extension:ombCategory.valueCoding",
        :type [{:code "Coding"}],
        :min  1, :max  "1",
        :binding {:strength "required",
         :valueSet "http://hl7.org/fhir/us/core/ValueSet/omb-race-category"}}
       {:id        "Extension.extension:detailed",
        :type      [{:code "Extension"}],
        :sliceName "detailed",
        :min       0, :max       "*"}
       {:id       "Extension.extension:detailed.url",
        :type     [{:code "uri"}],
        :min      1, :max      "1",
        :fixedUri "detailed"}
       {:id   "Extension.extension:detailed.valueCoding",
        :type [{:code "Coding"}],
        :min  1, :max  "1",
        :binding {:strength "required",
                  :valueSet "http://hl7.org/fhir/us/core/ValueSet/detailed-race"}}
       {:id        "Extension.extension:text",
        :type      [{:code "Extension"}],
        :sliceName "text",
        :min       1, :max       "1"}
       {:id       "Extension.extension:text.url",
        :type     [{:code "uri"}],
        :min      1, :max      "1",
        :fixedUri "text"}
       {:id   "Extension.extension:text.valueString",
        :type [{:code "string"}],
        :min  1, :max  "1"}
       {:id       "Extension.url",
        :min      1, :max      "1",
        :fixedUri "http://hl7.org/fhir/us/core/StructureDefinition/us-core-race"}
       {:id "Extension.value[x]", :path "Extension.value[x]", :min 0, :max "0"}]}})


  (sut/preprocess-resources aztx)
  (sut/process-resources aztx)

  ;; (keys (get-in (:fhir/src @aztx) ["StructureDefinition"]))

  ;; (select-keys @aztx [:fhir/src :fhir/inter])

  (matcho/match
    (sut/get-definition aztx  "http://hl7.org/fhir/StructureDefinition/TestBaseResourceType")
    {:| {:singular {:required not
                    :vector   not
                    :|        {:array {:required not
                                       :vector   true}}}
         :type     {:type     "Complex"
                    :vector   true
                    :escalate {:deps {:type {"Complex" true}}}}
         :poly     {:required not
                    :vector   not
                    :|        {:Complex {:required not
                                         :vector   not}}}}})

  (matcho/match (sut/get-definition aztx "http://hl7.org/fhir/StructureDefinition/TestConstraint")
                {:| {:singular {:required not
                                :vector   not
                                :|        {:array {:minItems 1
                                                   :maxItems 1
                                                   :required true
                                                   :vector   true}}}
                     :type     {:type     "Complex"
                                :vector   true
                                :escalate {:deps {:type {"Complex" true}}}
                                :|        {:array {:type "prim"
                                            :maxItems 1
                                            :vector true}}}
                     :poly     {:required not
                                :vector   not
                                :|        {:Complex {:required not
                                                     ;;TODO: uncomment
                                                     :type "Complex"
                                                     :escalate {:deps {:type {"Complex" true}}}
                                                     :vector   not
                                                     :|        {:array {:minItems 1
                                                                        :maxItems 1
                                                                        :type "prim"
                                                                        :escalate {:deps {:type {"prim" true}}}
                                                                        :required true
                                                                        :vector   true
                                                                        }}}}}}})

  (matcho/match
    (sut/get-definition aztx  "race")
    {:kind           "complex-type"
     :type           "Extension"
     :fhir/extension "http://hl7.org/fhir/us/core/StructureDefinition/us-core-race"
     :|              {:ombCategory {:vector   true
                                    :maxItems 5
                                    :escalate {:deps {:type {"Coding" true}}}
                                    :type     "Coding"
                                    :binding  {:strength "required"
                                               :valueSet "http://hl7.org/fhir/us/core/ValueSet/omb-race-category"}}
                      :detailed    {:vector    true
                                    :type      "Coding"
                                    :binding   {:strength "required"
                                                :valueSet "http://hl7.org/fhir/us/core/ValueSet/detailed-race"}
                                    :escalate  {:deps {:type {"Coding" true}}}}
                      :text        {:type     "string"
                                    :escalate {:deps {:type {"string" true}}}
                                    :required true}}})


  )


(t/deftest fhir-aidbox-poly-keys-mapping)

#_(t/deftest fhir-aidbox-poly-keys-mapping
    (def ztx (zen.core/new-context {}))
    (sut/load-all ztx "hl7.fhir.r4.core")

    (-> (sut/get-original ztx "http://hl7.org/fhir/us/core/StructureDefinition/us-core-race")
        (dissoc :snapshot)
        (select-keys [:type :kind :id :differential])
        (update-in [:differential :element]
                   (fn [xs]
                     (->> xs
                          (mapv (fn [x]
                                  (select-keys x [:id :path :type :sliceName :min :max :fixedUri :binding])))))))

    (t/testing "base type poly keys"
      (def observation (sut/get-definition ztx "http://hl7.org/fhir/StructureDefinition/Observation"))

      (matcho/match
        observation
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
                                                                             :Quantity {:type "Quantity"}}}}}}}))

    (t/testing "constraint poly keys fixing"
      (def poly-prof-res (sut/get-definition ztx "http://hl7.org/fhir/us/core/StructureDefinition/pediatric-bmi-for-age"))

      (matcho/match
        poly-prof-res

        {:baseDefinition "http://hl7.org/fhir/StructureDefinition/vitalsigns"
         :kind           "resource",
         :type           "Observation"
         :derivation     "constraint",
         :|              {:value
                          {:polymorphic true
                           :|           {:Quantity
                                         {:|
                                          {:value {:required true}}}}}}}))


    (t/testing "test-zen-transformation"

      (def pres (sut/get-definition ztx "http://hl7.org/fhir/StructureDefinition/Patient"))


      (comment
        (inspect "/tmp/pres.html" (get-in @ztx [:fhir/inter "StructureDefinition"]) {:closed true})
        )


      (matcho/match
        pres
        {:kind           "resource"
         :derivation     "specialization",
         :baseDefinition "http://hl7.org/fhir/StructureDefinition/DomainResource"

         ;; :deps {:valuesets {}
         ;;        :types {}
         ;;        :extensions {}
         ;;        :profiles {}}

         :| {:address             {:short     "An address for the individual"
                                   :type      "Address"
                                   :escalate  {:deps {:type {"Address" true}}}
                                   :vector    true
                                   :isSummary true}
             :multipleBirth
             {:|           {:boolean {:type "boolean"}
                            :integer {:type "integer"}}
              :types       #{"boolean" "integer"}
              :polymorphic true}
             :link                {:type   "BackboneElement"
                                   :vector true
                                   :|      {:other {:type      "Reference"
                                                    :required  true
                                                    :isSummary true}
                                            :type  {:binding  {:strength "required"
                                                               :valueSet "http://hl7.org/fhir/ValueSet/link-type|4.0.1"}
                                                    :required true}}}
             :generalPractitioner {:vector   true
                                   :type     "Reference"
                                   :profiles #{"http://hl7.org/fhir/StructureDefinition/Organization"
                                               "http://hl7.org/fhir/StructureDefinition/Practitioner"
                                               "http://hl7.org/fhir/StructureDefinition/PractitionerRole"}}}})


      (def ares (sut/get-definition ztx "http://hl7.org/fhir/StructureDefinition/Address"))

      ;; (spit "/tmp/ares.edn" (with-out-str (clojure.pprint/pprint (:elements ares))))

      (matcho/match ares {})

      (def qres (sut/get-definition ztx "http://hl7.org/fhir/StructureDefinition/Questionnaire"))

      ;; (spit "/tmp/qres.edn" (with-out-str (clojure.pprint/pprint qres)))

      (matcho/match
        qres
        {:|
         {:description {}
          :subjectType {}
          :item
          {:|
           {:item {:escalate {:recur [:item]}}
            :enableWhen
            {:| {:question {}
                 :operator {}
                 :answer   {}}}}}}})


      ;; (get-in @ztx [:fhir "StructureDefinition" "Questionnaire"])

      ;; (sut/process-sd quest)

      ;; min/max => vector? required minItems/maxItems
      ;; type -> polymorphic, type, profiles
      ;; references to extensions etc
      ;; group and analyze slicing
      ;; analyze valuesets


      ;; packeg (deps) => index


      ;; {
      ;;  <rt> <url> {
      ;;              :lookup-idx {}
      ;;              :fhir       {}
      ;;              :elements   {}
      ;;              :transforms {}
      ;;              }
      ;;  }

      ;; => ????

      ;; -> rich -> generate
      ;;         -> assumptins check

      ;;may be no vector as a first step
      #_(matcho/match
          (sut/group-elements ztx quest)

          ;; (sut/load-structure-definition ztx quest)

          {
           :original      {}
           :tree-elements {}
           :elements      {}
           :deps          {:base       {}
                           :extensions {}
                           :valuesets  {}}

           }

          {:elements
           {:description {:type [{:code "markdown"}]},
            :subjectType {:vector true
                          :type   [{:code "code"}]},
            :derivedFrom {:vector true
                          :type   [{:code "canonical"}]},
            :name        {:type [{:code "string"}]},
            :code        {:vector true
                          :type   [{:code "Coding"}]},
            :item        {:vector true
                          :elements
                          {:enableBehavior {:type [{:code "code"}]},
                           :definition     {:type [{:code "uri"}]},
                           :item           {:vector true
                                            :recur  [:item]},
                           :enableWhen
                           {:vector true
                            :elements
                            {:question {:type [{:code "string"}]},
                             :operator {:type [{:code "code"}]},
                             :answer   {:polymorphic true
                                        :elements    {:boolean   {}
                                                      :decimal   {}
                                                      :integer   {}
                                                      :date      {}
                                                      :dateTime  {}
                                                      :time      {}
                                                      :string    {:type [{:code "string"}]}
                                                      :Coding    {}
                                                      :Quantity  {}
                                                      :Reference {:type [{:code "Reference", :targetProfile ["http://hl7.org/fhir/StructureDefinition/Resource"]}]}}}}}}}}})





      ))
