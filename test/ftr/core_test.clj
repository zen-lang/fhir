(ns ftr.core-test
  (:require [ftr.core :as sut]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :as t]
            [matcho.core :as matcho]))

(defn fs-tree->tree-map [path]
  (reduce
   (fn [store path] (assoc-in store path {}))
   {}
   (map (fn [f] (drop 1 (str/split (str f) #"/"))) (file-seq (io/file path)))))

(def test-env-cfg {
                   :csv-source "/tmp/ftr-fixtures/cfg.csv"
                   :ftr-path "/tmp/ftr"
                   :expected-sha256 "123"
                   })

(defn prepare-test-env []
  (let [fixture-file (io/file (:csv-source test-env-cfg))]
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
11873;2001203W641;W64.01;Exposure to other and unspecified animate mechanical forces, home, while engaged in leisure activity;11871;;1;")))


(t/deftest generate-repository-layout-from-config
  (t/testing "User provides config for CSV"
    (prepare-test-env)
    (def user-cfg {:module "icd10"
                   :source-url (:csv-source test-env-cfg)
                   :ftr-path (:ftr-path test-env-cfg)
                   :source-type :csv
                   :extractor-options {:format  {:delimiter ";"
                                                 :quote "'"}
                                       :header   false
                                       :data-row 0
                                       :mapping  {:concept {:code    {:column 2}
                                                            :display {:column 3}}}
                                       :code-system {:id "icd10"
                                                     :url "http://hl7.org/fhir/sid/icd-10"}
                                       :value-set   {:id "icd10"
                                                     :name "icd10"
                                                     :url "http://hl7.org/fhir/ValueSet/icd-10"}}})
    (sut/spit-terminology-file user-cfg)
    (t/testing "sees generated repository layout"
      (matcho/match
       (fs-tree->tree-map (:ftr-path test-env-cfg))
        {"tmp"
         {"ftr"
          {"icd10"
           {"vs"
            {(format "tf.%s.ndjson.gz" (:expected-sha256 test-env-cfg)) {}}}}}}))))

(comment

  (str f)

  f
  (def f "/tmp/")

  (spit "abc" f)

  (matcho/str)


  (fs-tree->tree-map (:ftr-path test-env-cfg))
  nil)
