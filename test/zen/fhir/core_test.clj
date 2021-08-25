(ns zen.fhir.core-test
  (:require [zen.fhir.core :as sut]
            [clojure.test :as t]
            [zen.core]
            [matcho.core :as matcho]
            [clojure.java.io :as io]))

(t/deftest test-zen-transformation

  (def ztx (zen.core/new-context {}))

  ;; elements => nested

  (def quest (read-string (slurp (io/resource "zen/fhir/questionnair.edn"))))


  (sut/process-sd quest)

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
  (matcho/match
    (sut/group-elements ztx quest)

    ;; (sut/load-structure-definition ztx quest)

    {
     :original {}
     :tree-elements {}
     :elements {}
     :deps {:base {}
            :extensions {}
            :valuesets  {}}

     }

    {:elements
     {:description {:type [{:code "markdown"}]},
      :subjectType {:vector true
                    :type [{:code "code"}]},
      :derivedFrom {:vector true
                    :type [{:code "canonical"}]},
      :name        {:type [{:code "string"}]},
      :code        {:vector true
                    :type [{:code "Coding"}]},
      :item        {:vector true
                    :elements
                    {:enableBehavior {:type [{:code "code"}]},
                     :definition     {:type [{:code "uri"}]},
                     :item           {:vector true
                                      :recur [:item]},
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





  )
