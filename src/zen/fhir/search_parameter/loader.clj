(ns zen.fhir.search-parameter.loader
  (:require [zen.fhir.utils :as utils]
            [zen.fhir.search-parameter.template :as template]
            [zen.fhir.search-parameter.fhirpath :as fhirpath]
            [com.rpl.specter :as sp]))

(defn process-on-load [res]
  (cond
    (nil? (:expression res))
    (println :search-parameter/no-expression (:url res))

    (= "composite" (:type res))
    (merge res
           (when-let [package-ns (:zen.fhir/package-ns res)]
             {:zen.fhir/schema-ns (symbol (str (name package-ns) ".search." (:id res)))})
           {:sp-name (:code res)
            :base-resource-types (:base res)})

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
    

(defn get-type-by-knife [ztx _inter base-rt knife]
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

(defn process-expression [ztx inter base-rt]
  (let [knife-paths (fhirpath/fhirpath->knife (:expression inter))
        knifes      (get knife-paths base-rt)
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
         :sql        (template/expand sp-template types jsonpath)})}))

(defn merge-with-base-paths [base-paths components]
  (mapv
   (fn [base-path]
     (mapv
      (fn [c]
        (mapv
         (fn [knife-path]
           (into base-path knife-path))
         c))
      components))
   base-paths))

(defn components-search-types [ztx components]
  (mapv
   (fn [component]
     (let [definition (:definition component)]
       (-> @ztx
           :fhir/inter
           (get "SearchParameter")
           (get definition)
           :type
           keyword)))
   components))

(defn process-composite-expression [ztx inter base-rt base-paths components]
  (let [paths (map (comp :default fhirpath/fhirpath->knife :expression) components)
        search-types (components-search-types ztx components)
        full-paths (merge-with-base-paths base-paths paths)
        jsonpath   (mapv (partial mapv fhirpath/knife->jsonpath) full-paths)
        types      (reduce
                    #(into %1 (mapcat (partial get-type-by-knife ztx inter base-rt)) %2)
                    #{}
                    paths)]
     (utils/strip-nils
      {:knife      full-paths
       :jsonpath   jsonpath
       :data-types types
       :search-types search-types
       :template   :composite
       :sql        (template/expand :composite types jsonpath)})))

(defn process-composite-search-parameter [ztx inter]
  (let [knife-paths (fhirpath/fhirpath->knife (:expression inter))]
    (reduce-kv
     (fn [inter base-rt paths]
       (assoc-in inter [:expr (keyword base-rt)]
                 (process-composite-expression ztx inter base-rt paths (:component inter))))
     inter
     knife-paths)))

(defn process-common-search-parameter [ztx inter]
  (->> inter
       :base
       (into {} (map (partial process-expression ztx inter)))
       (assoc inter :expr)))

(defn process-search-parameter [ztx inter]
  (->
   (if (= "composite" (:type inter))
    (process-composite-search-parameter ztx inter)
    (process-common-search-parameter ztx inter))
   (dissoc :expression)))

(defn process-search-parameters [ztx]
  (swap! ztx update-in [:fhir/inter "SearchParameter"]
         #(sp/transform [sp/MAP-VALS]
                        (partial process-search-parameter ztx)
                        %)))
