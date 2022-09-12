(ns ftr.core-test
  (:require [ftr.core :as sut]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :as t]
            [matcho.core :as matcho]
            [ftr.utils.core]))


(defn fs-tree->tree-map [path]
  (reduce
    (fn [store path] (assoc-in store path {}))
    {}
    (map (fn [f] (drop 1 (str/split (str f) #"/"))) (file-seq (io/file path)))))

{"icd10"
           {"vs"
            {"icd10.accidents"
             {"tag.v1.ndjson.gz" {},
              "tf..ndjson.gz"
              {}}},
            "tags" {"v1.ndjson.gz" {}}}}
(def test-env-cfg {:csv-source-initial "/tmp/ftr-fixtures/icd10initial.csv"
                   :csv-source-updated "/tmp/ftr-fixtures/icd10updated.csv"
                   :ftr-path "/tmp/ftr"
                   :expected-tf-sha256 "70c1225a2ddd108c869a18a87a405c029f48a30c0401268869f19959a4723976"
                   :expected-tf-filename "tf.70c1225a2ddd108c869a18a87a405c029f48a30c0401268869f19959a4723976.ndjson.gz"
                   :expected-updated-tf-sha256 "476b3dbcdab7fe4e8522451f7495c185317acb3530178b31c91a888e173f94f5"
                   :expected-updated-tf-filename "tf.476b3dbcdab7fe4e8522451f7495c185317acb3530178b31c91a888e173f94f5.ndjson.gz"
                   :expected-patch-filename "patch.70c1225a2ddd108c869a18a87a405c029f48a30c0401268869f19959a4723976.476b3dbcdab7fe4e8522451f7495c185317acb3530178b31c91a888e173f94f5.ndjson.gz"})


(def user-cfg {:module            "icd10"
               :source-url        (:csv-source-initial test-env-cfg)
               :ftr-path          (:ftr-path test-env-cfg)
               :tag               "v1"
               :source-type       :flat-table
               :extractor-options {:format "csv"
                                   :csv-format      {:delimiter ";"
                                                     :quote     "'"}
                                   :header      false
                                   :data-row    0
                                   :mapping     {:concept {:code    {:column 2}
                                                           :display {:column 3}}}
                                   :code-system {:id  "icd10"
                                                 :url "http://hl7.org/fhir/sid/icd-10"}
                                   :value-set   {:id   "icd10"
                                                 :name "icd10.accidents"
                                                 :url  "http://hl7.org/fhir/ValueSet/icd-10"}}})


(defn prepare-test-env! [{:as _cfg, :keys [csv-source-initial
                                           csv-source-updated
                                           ftr-path]}]
  (let [fixture-file (io/file csv-source-initial)
        fixture-file-2 (io/file csv-source-updated)]

    (io/make-parents fixture-file)
    (spit
      fixture-file
      "10344;20;XX;External causes of morbidity and mortality;;;1;
16003;2001;V01-X59;Accidents;10344;;1;
15062;20012;W00-X59;Other external causes of accidental injury;16003;;1;10/07/2020")

    (io/make-parents fixture-file-2)
    (spit
      fixture-file-2
      "10343;766;AA;loh and mortality;;;1;
10343;666;X;morbidity and mortality;;;1;
10344;20;XX;External causes of morbidity and mortality;;;1;
16003;2001;V01-X59;Updated accidents;10344;;1;")

    (ftr.utils.core/rmrf ftr-path)))

(defn clean-up-test-env! [{:as _cfg, :keys [csv-source-initial
                                            csv-source-updated
                                            ftr-path]}]
  (ftr.utils.core/rmrf csv-source-initial)
  (ftr.utils.core/rmrf csv-source-updated)
  (ftr.utils.core/rmrf ftr-path))

(t/deftest generate-repository-layout-from-config
  (t/testing "User provides config for CSV"
    (prepare-test-env! test-env-cfg)

    (let [{:as user-cfg, :keys [module ftr-path tag]
           {{value-set-name :name} :value-set} :extractor-options}
          user-cfg

          tf-tag-file-name
          (format "tag.%s.ndjson.gz" tag)

          _
          (sut/apply-cfg user-cfg)

          ftr-tree
          (get-in (fs-tree->tree-map ftr-path) (str/split (subs ftr-path 1) #"/"))]

      (t/testing "sees generated repository layout, tf sha is correct"
        (matcho/match
          ftr-tree
          {module
           {"tags"
            {(format "%s.ndjson.gz" tag) {}}
            "vs"
            {value-set-name
             {(:expected-tf-filename test-env-cfg) {}
              tf-tag-file-name                     {}}}}}))

      (t/testing "sees tag ingex content"
        (matcho/match
          (ftr.utils.core/parse-ndjson-gz (format "%s/icd10/tags/%s.ndjson.gz" (:ftr-path test-env-cfg) (:tag user-cfg)))
          [{:name (format "%s.%s" module value-set-name) :hash (:expected-tf-sha256 test-env-cfg)}
           nil?]))

      (t/testing "sees terminology tag file"
        (matcho/match
          (ftr.utils.core/parse-ndjson-gz (format "%s/icd10/vs/%s/%s" ftr-path value-set-name tf-tag-file-name))
          [{:tag tag :hash (:expected-tf-sha256 test-env-cfg)}
           nil?])))

    )

  (t/testing "User provides updated config for CSV"
    (let [{:as user-cfg, :keys [module ftr-path tag]
           {{value-set-name :name} :value-set} :extractor-options}
          (assoc user-cfg :source-url (:csv-source-updated test-env-cfg))

          tf-tag-file-name
          (format "tag.%s.ndjson.gz" tag)

          _
          (sut/apply-cfg user-cfg)

          ftr-tree
          (get-in (fs-tree->tree-map ftr-path) (str/split (subs ftr-path 1) #"/"))

          _
          (def a ftr-tree)]

      (t/testing "sees updated repository layout, new tf sha is correct, patch file created"
        (matcho/match
         ftr-tree
         {module
          {"tags"
           {(format "%s.ndjson.gz" tag) {}}
           "vs"
           {value-set-name
            {(:expected-tf-filename test-env-cfg)         {}
             (:expected-updated-tf-filename test-env-cfg) {}
             (:expected-patch-filename test-env-cfg)      {}
             tf-tag-file-name                             {}}}}})
        )

      (t/testing "sees tag ingex content"
        (matcho/match
         (ftr.utils.core/parse-ndjson-gz (format "%s/icd10/tags/%s.ndjson.gz" (:ftr-path test-env-cfg) (:tag user-cfg)))
         [{:name (format "%s.%s" module value-set-name) :hash (:expected-updated-tf-sha256 test-env-cfg)}
          nil?]))

      (t/testing "sees terminology tag file"
        (matcho/match
          (ftr.utils.core/parse-ndjson-gz (format "%s/icd10/vs/%s/%s" ftr-path value-set-name tf-tag-file-name))
          [{:tag tag :hash (:expected-updated-tf-sha256 test-env-cfg)}
           {:from (:expected-tf-sha256 test-env-cfg) :to (:expected-updated-tf-sha256 test-env-cfg)}
           nil?]))

      (t/testing "sees terminology patch file"
        (matcho/match
          (sort-by :code (ftr.utils.core/parse-ndjson-gz (format "%s/icd10/vs/%s/%s" ftr-path value-set-name (:expected-patch-filename test-env-cfg))))
          [{:name value-set-name}
           {:code "AA" :op "add"}
           {:code "V01-X59" :op "update"}
           {:code "W00-X59" :op "remove"}
           {:code "X" :op "add"}
           nil?]))))

  (clean-up-test-env! test-env-cfg))
