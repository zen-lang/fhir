(ns zen.fhir.value-set-expand-test
  (:require [zen.fhir.value-set-expand :as sut]
            [clojure.test :as t]))


(t/deftest filter-unit-test
  (t/testing "wrong sys"
    (t/is (not ((sut/vs-compose-filter-fn nil nil "sys" nil [{}])
                {:system "sus"}))))

  (t/testing "op ="
    (t/is ((sut/vs-compose-filter-fn
            nil nil "sys" nil
            [{:property "prop"
              :op       "="
              :value    "abc"}])
           {:code     "c"
            :system   "sys"
            :property {"prop" "abc"}}))

    (t/is (not ((sut/vs-compose-filter-fn
                 nil nil "sys" nil
                 [{:property "prop"
                   :op       "="
                   :value    "abc"}])
                {:code     "c"
                 :system   "sys"
                 :property {"prop" "xyz"}})))

    (t/is (not ((sut/vs-compose-filter-fn
                 nil nil "sys" nil
                 [{:property "prop"
                   :op       "="
                   :value    "abc"}])
                {:code   "c"
                 :system "sys"}))))

  (t/testing "in"
    (t/is ((sut/vs-compose-filter-fn
            nil nil "sys" nil
            [{:property "p"
              :op       "in"
              :value    "a,b,c"}])
           {:code     "c"
            :system   "sys"
            :property {"p" "b"}}))

    (t/is (not ((sut/vs-compose-filter-fn
                 nil nil "sys" nil
                 [{:property "p"
                   :op       "in"
                   :value    "a,b,c"}])
                {:code     "c"
                 :system   "sys"
                 :property {"p" "y"}})))

    (t/is (not ((sut/vs-compose-filter-fn
                 nil nil "sys" nil
                 [{:property "p"
                   :op       "in"
                   :value    "a,b,c"}])
                {:code   "c"
                 :system "sys"}))))

  (t/testing "not-in"
    (t/is ((sut/vs-compose-filter-fn
            nil nil "sys" nil
            [{:property "p"
              :op       "not-in"
              :value    "a,b,c"}])
           {:code     "c"
            :system   "sys"
            :property {"p" "y"}}))

    (t/is (not ((sut/vs-compose-filter-fn
                 nil nil "sys" nil
                 [{:property "p"
                   :op       "not-in"
                   :value    "a,b,c"}])
                {:code     "c"
                 :system   "sys"
                 :property {"p" "b"}})))

    (t/is ((sut/vs-compose-filter-fn
            nil nil "sys" nil
            [{:property "p"
              :op       "not-in"
              :value    "a,b,c"}])
           {:code   "c"
            :system "sys"})))

  (t/testing "exists"
    (t/testing "true"
      (t/is ((sut/vs-compose-filter-fn
              nil nil "sys" nil
              [{:property "p"
                :op       "exists"
                :value    "true"}])
             {:code     "c"
              :system   "sys"
              :property {"p" "a"}}))

      (t/is (not ((sut/vs-compose-filter-fn
                   nil nil "sys" nil
                   [{:property "p"
                     :op       "exists"
                     :value    "true"}])
                  {:code   "c"
                   :system "sys"}))))

    (t/testing "false"
      (t/is ((sut/vs-compose-filter-fn
              nil nil "sys" nil
              [{:property "p"
                :op       "exists"
                :value    "false"}])
             {:code   "c"
              :system "sys"}))

      (t/is (not ((sut/vs-compose-filter-fn
                   nil nil "sys" nil
                   [{:property "p"
                     :op       "exists"
                     :value    "false"}])
                  {:code     "c"
                   :system   "sys"
                   :property {"p" "a"}})))))

  (t/testing "is-a"
    (t/is ((sut/vs-compose-filter-fn
            nil nil "sys" nil
            [{:property "concept"
              :op       "is-a"
              :value    "proto_c"}])
           {:code      "proto_c"
            :system    "sys"
            :hierarchy ["proto_proto_c"]}))

    (t/is ((sut/vs-compose-filter-fn
            nil nil "sys" nil
            [{:property "concept"
              :op       "is-a"
              :value    "proto_c"}])
           {:code      "c"
            :system    "sys"
            :hierarchy ["proto_proto_c" "proto_c"]}))

    (t/is (not ((sut/vs-compose-filter-fn
                 nil nil "sys" nil
                 [{:property "concept"
                   :op       "is-a"
                   :value    "proto_c"}])
                {:code      "c1"
                 :system    "sys"
                 :hierarchy ["proto_proto_c1" "proto_c1"]}))))

  (t/testing "descendent-of"
    (t/is (not ((sut/vs-compose-filter-fn
                 nil nil "sys" nil
                 [{:property "concept"
                   :op       "descendent-of"
                   :value    "proto_c"}])
                {:code      "proto_c"
                 :system    "sys"
                 :hierarchy ["proto_proto_c"]})))

    (t/is ((sut/vs-compose-filter-fn
            nil nil "sys" nil
            [{:property "concept"
              :op       "descendent-of"
              :value    "proto_c"}])
           {:code      "c"
            :system    "sys"
            :hierarchy ["proto_proto_c" "proto_c"]}))

    (t/is (not ((sut/vs-compose-filter-fn
                 nil nil "sys" nil
                 [{:property "concept"
                   :op       "descendent-of"
                   :value    "proto_c"}])
                {:code      "c1"
                 :system    "sys"
                 :hierarchy ["proto_proto_c1" "proto_c1"]}))))

  (t/testing "is-not-a"
    (t/is (not ((sut/vs-compose-filter-fn
                 nil nil "sys" nil
                 [{:property "concept"
                   :op       "is-not-a"
                   :value    "proto_c"}])
                {:code      "proto_c"
                 :system    "sys"
                 :hierarchy ["proto_proto_c"]})))

    (t/is (not ((sut/vs-compose-filter-fn
                 nil nil "sys" nil
                 [{:property "concept"
                   :op       "is-not-a"
                   :value    "proto_c"}])
                {:code      "c"
                 :system    "sys"
                 :hierarchy ["proto_proto_c" "proto_c"]})))

    (t/is ((sut/vs-compose-filter-fn
            nil nil "sys" nil
            [{:property "concept"
              :op       "is-not-a"
              :value    "proto_c"}])
           {:code      "c1"
            :system    "sys"
            :hierarchy ["proto_proto_c1" "proto_c1"]})))

  (t/testing "regex"
    (t/is ((sut/vs-compose-filter-fn
            nil nil "sys" nil
            [{:property "p"
              :op       "regex"
              :value    "a.c"}])
           {:code     "c"
            :system   "sys"
            :property {"p" "abc"}}))

    (t/is (not ((sut/vs-compose-filter-fn
                 nil nil "sys" nil
                 [{:property "p"
                   :op       "regex"
                   :value    "a.c"}])
                {:code     "c"
                 :system   "sys"
                 :property {"p" "xyz"}})))

    (t/is (not ((sut/vs-compose-filter-fn
                 nil nil "sys" nil
                 [{:property "p"
                   :op       "regex"
                   :value    "a.c"}])
                {:code   "c"
                 :system "sys"})))

    (t/is (not ((sut/vs-compose-filter-fn
                 nil nil "sys" nil
                 [{:property "p"
                   :op       "regex"
                   :value    ".*"}])
                {:code   "c"
                 :system "sys"})))))
