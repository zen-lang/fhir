(ns zen.fhir.search-parameter.loader
  (:require
   [clojure.string :as str]
   [zen.fhir.utils :as utils]
   [zen.fhir.search-parameter.template :as template]
   [zen.fhir.search-parameter.fhirpath :as fhirpath]))

(defn process-on-load [res]
  (cond
    (nil? (:expression res))
    (println :search-parameter/no-expression (:url res))

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

(defn remove-fhirpath-references [fhirpath]
  (str/replace fhirpath #"%(resource|rootResource|context)\." ""))

(defn component->knife-path [component]
  (-> component
      :expression
      remove-fhirpath-references
      fhirpath/fhirpath->knife
      :default))

(defn process-composite-expression [ztx inter base-rt base-paths]
  (let [components (:component inter)
        base-jsonpath (fhirpath/knife->jsonpath base-paths)
        components-paths (mapv component->knife-path components)
        components-jsonpaths (mapv fhirpath/knife->jsonpath components-paths)
        search-types (components-search-types ztx components)
        types      (reduce
                    #(into %1 (mapcat (partial get-type-by-knife ztx inter base-rt)) %2)
                    #{}
                    components-paths)]
    (if (some nil? search-types)
      nil
      (utils/strip-nils
        {:base-knife base-paths
         :base-jsonpath base-jsonpath
         :component-knife components-paths
         :component-jsonpath components-jsonpaths
         :data-types types
         :search-types search-types
         :template   :composite
         :sql        (template/expand :composite types base-rt)}))))

(defn process-composite-search-parameter [ztx inter]
  (let [knife-paths (fhirpath/fhirpath->knife (:expression inter))]
    (reduce-kv
     (fn [inter base-rt paths]
       (if-let [expr (process-composite-expression ztx inter base-rt paths)]
         (assoc-in inter [:expr (keyword base-rt)] expr)
         (reduced nil)))
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
         #(reduce-kv
           (fn [search-params sp-name sp]
             (if-let [sp (process-search-parameter ztx sp)]
               (assoc search-params sp-name sp)
               search-params))
           {} %)))
