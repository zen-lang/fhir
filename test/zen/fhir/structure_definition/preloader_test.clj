(ns zen.fhir.structure-definition.preloader-test
  (:require [zen.fhir.structure-definition.preloader :as sut]
            [matcho.core :as matcho]
            [clojure.test :as t]))

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
