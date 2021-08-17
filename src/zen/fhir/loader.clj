(ns zen.fhir.loader
  (:require [clojure.pprint]
            [clojure.string :as str]
            [clojure.java.io]
            [cheshire.core]
            [com.rpl.specter :as sp]
            [zen.fhir.generator]))


(defn format-zen-ns [zen-ns-map]
  (clojure.pprint/write (zen.fhir.generator/order-zen-ns zen-ns-map) :stream nil))


(defn generate-profiles-types-uni-project [fhir-lib type-profiles-bundle zrc-path]
  (let [type-profiles     (sp/select [sp/ALL :resource] (:entry type-profiles-bundle))
        type-urls         (sp/select [sp/ALL :url] type-profiles)
        type-proj         (-> (zen.fhir.generator/structure-definitions->uni-zen-project
                                fhir-lib
                                type-urls
                                type-profiles
                                {:remove-gen-keys?     true
                                 :fold-schemas?        true
                                 :elements-mode        :differential
                                 :fhir-lib             fhir-lib
                                 :drop-out-current-ns? true})
                              format-zen-ns)
        project-file-path (str (str/join "/" (cons zrc-path (str/split (name fhir-lib) #"\."))) ".edn")]
    (clojure.java.io/make-parents project-file-path)
    (spit project-file-path type-proj)
    :done))
