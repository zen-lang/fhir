(ns zen.fhir.search-parameter.loader
  (:require [zen.fhir.utils :as utils]

            [zen.fhir.search-parameter.template :as template]
            [zen.fhir.search-parameter.fhirpath :as fhirpath]

            #_[clojure.string :as str]
            [com.rpl.specter :as sp]))

(defn process-on-load [res]
  (cond
    (nil? (:expression res))
    (println :search-parameter/no-expression (:url res))

    (= "composite" (:type res))
    (println :search-parameter/composite-not-supported (:url res))

    :else
    (merge res
           (when-let [package-ns (:zen.fhir/package-ns res)]
             {:zen.fhir/schema-ns (symbol (str (name package-ns) ".search." (:id res)))})
           {:id (:id res)
            :url (:url res)
            :type (:type res)
            :sp-name (:code res)
            :base-resource-types (:base res)
            :expression (:expression res)})))

(defn get-type-by-knife [ztx inter base-rt knife]
  (let [inter-path (->> (remove map? knife)
                        (map keyword)
                        (interleave (repeat :|)))
        rt-inter (get-in @ztx
                         [:fhir/inter
                          "StructureDefinition"
                          (str "http://hl7.org/fhir/StructureDefinition/" base-rt)])
        el (get-in rt-inter inter-path)]
    (if-let [tp (:type el)]
      #{{:type tp
         :polymorphic? false}}
      (into #{} (map (fn [tp] {:type tp
                               :polymorphic? true}))
            (:types el)))))

(defn process-search-parameter [ztx inter]
  (let [knife-paths (fhirpath/fhirpath->knife (:expression inter))
        expr        (into {}
                          (map (fn [base-rt]
                                 (let [knifes      (get knife-paths base-rt)
                                       default-knifes (get knife-paths :default)
                                       all-knifes (into [] (concat knifes default-knifes))
                                       jsonpath    (fhirpath/knife->jsonpath all-knifes)
                                       sp-template (keyword (:type inter))
                                       types       (into #{}
                                                         (mapcat (partial get-type-by-knife ztx inter base-rt))
                                                         knifes)]
                                   {(keyword base-rt)
                                    (utils/strip-nils
                                     {:knife      all-knifes
                                      :jsonpath   jsonpath
                                      :data-types types
                                      :template   sp-template
                                      :sql        (template/expand sp-template types jsonpath)})})))
                          (:base inter))]
    (-> inter
        (dissoc :expression)
        (assoc :expr expr))))

(defn process-search-parameters [ztx]
  (swap! ztx update-in [:fhir/inter "SearchParameter"]
         #(sp/transform [sp/MAP-VALS]
                        (partial process-search-parameter ztx)
                        %)))
