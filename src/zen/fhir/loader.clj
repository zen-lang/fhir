(ns zen.fhir.loader
  (:require [clojure.pprint]
            [clojure.string :as str]
            [clojure.java.io]
            [cheshire.core]
            [com.rpl.specter :as sp]
            [zen.fhir.generator]))


(defn format-zen-ns [zen-ns-map]
  (clojure.pprint/write (zen.fhir.generator/order-zen-ns zen-ns-map) :stream nil))


(defn patch-fhir->aidbox-format [zen-fhir-ns]
  (-> zen-fhir-ns
      (assoc-in ['Meta :keys :createdAt]
                {:confirms #{'instant},
                 :type     'zen/string,
                 :zen/desc "When the resource was created"})
      (assoc-in ['Reference :keys :id]
                {:confirms #{'id},
                 :type     'zen/string,
                 :zen/desc "ID of the referred resource"})
      (assoc-in ['Reference :keys :resourceType]
                {:confirms #{'uri},
                 :type     'zen/string,
                 :zen/desc "Type the reference refers to"})))


(defn generate-profiles-types-uni-project [fhir-lib resource-profiles-bundle type-profiles-bundle zrc-path]
  (let [type-profiles     (sp/select [sp/ALL :resource #(= "StructureDefinition" (:resourceType %))]
                                     (concat (:entry type-profiles-bundle)
                                             (:entry resource-profiles-bundle)))
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
                              patch-fhir->aidbox-format
                              format-zen-ns)
        project-file-path (str (str/join "/" (cons zrc-path (str/split (name fhir-lib) #"\."))) ".edn")]
    (clojure.java.io/make-parents project-file-path)
    (spit project-file-path type-proj)
    :done))
