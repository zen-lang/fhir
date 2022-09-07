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

(def test-env-cfg {:csv-source "/tmp/ftr-fixtures/cfg.csv"
                   :ftr-path "/tmp/ftr"
                   :expected-tf-sha256 "3da7d3b590373ea920efeec1d79ab33c17d6cb3d3a2eead521ce5a4239e7c850"
                   :expected-tf-filename "tf.3da7d3b590373ea920efeec1d79ab33c17d6cb3d3a2eead521ce5a4239e7c850.ndjson.gz"})

(defn prepare-test-env! [{:as _cfg, :keys [csv-source ftr-path]}]
  (let [fixture-file (io/file csv-source)]
    (io/make-parents fixture-file)
    (spit
     fixture-file
     "10344;20;XX;External causes of morbidity and mortality;;;1;
16003;2001;V01-X59;Accidents;10344;;1;
15062;20012;W00-X59;Other external causes of accidental injury;16003;;1;10/07/2020
11748;2001203;W50-W64;Exposure to animate mechanical forces;15062;;1;
11870;2001203W64;W64;Exposure to other and unspecified animate mechanical forces;11748;;1;
11871;2001203W640;W64.0;Exposure to other and unspecified animate mechanical forces home while engaged in sports activity;11870;;1;
11872;2001203W641;W64.00;Exposure to other and unspecified animate mechanical forces, home, while engaged in sports activity;11871;;1;
11873;2001203W641;W64.01;Exposure to other and unspecified animate mechanical forces, home, while engaged in leisure activity;11871;;1;")
    (ftr.utils.core/rmrf ftr-path)))

(defn clean-up-test-env! [{:as _cfg, :keys [csv-source ftr-path]}]
  (ftr.utils.core/rmrf csv-source)
  (ftr.utils.core/rmrf ftr-path))

(t/deftest generate-repository-layout-from-config
  (t/testing "User provides config for CSV"
    (prepare-test-env! test-env-cfg)

    (let [{:as user-cfg, :keys [module ftr-path tag]
           {{value-set-name :name} :value-set} :extractor-options}

          {:module            "icd10"
           :source-url        (:csv-source test-env-cfg)
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
                                             :url  "http://hl7.org/fhir/ValueSet/icd-10"}}}

          tf-tag-file-name
          (format "tag.%s.ndjson.gz" tag)

          _
          (sut/spit-ftr user-cfg)

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

    (clean-up-test-env! test-env-cfg)))
