(ns zen.fhir.nictiz
  (:require [zen.fhir.core :as c]
            [clojure.java.io :as io]
            [cheshire.core]
            [clojure.string :as str]
            [com.rpl.specter :as sp]))

(def blacklist
  {"StructureDefinition"
   #{"http://hl7.org/fhir/StructureDefinition/allergyintolerance-substanceExposureRisk"
     "http://hl7.org/fhir/StructureDefinition/cqif-measureInfo"
     "http://hl7.org/fhir/StructureDefinition/cqif-questionnaire"
     "http://hl7.org/fhir/StructureDefinition/diagnosticreport-genetics"
     "http://hl7.org/fhir/StructureDefinition/elementdefinition-de"
     "http://hl7.org/fhir/StructureDefinition/familymemberhistory-genetic"
     "http://hl7.org/fhir/StructureDefinition/observation-genetics"
     "http://hl7.org/fhir/StructureDefinition/patient-clinicalTrial"
     "http://hl7.org/fhir/StructureDefinition/procedurerequest-genetics"
     "http://hl7.org/fhir/StructureDefinition/procedurerequest-geneticsItem"}})


(defn load-definiton [ztx packages header res]
  (if-let [rt (:resourceType res)]
    (if-let [url (:url header)]
      (swap! ztx update-in [:fhir/src rt url]
             (fn [x] (when x (println :WARN :override-resource header)) res))
      (println :WARN :no-url header))
    (println :WARN :no-resource-type header)))


(defn load-json-file [ztx package header f & [{:keys [params]}]]
  (let [res (-> (cheshire.core/parse-string (str/replace (slurp f) \ufeff \space) keyword)
                (assoc :zen.fhir/header header :zen.fhir/package package :zen.fhir/file (.getPath f))
                (merge
                  {:zen.fhir/package-ns (some-> package :name (str/replace #"\." "-") symbol)
                   :_source "zen.fhir"}
                  (select-keys params #{:zen.fhir/package-ns})))]
    (load-definiton ztx package header res)))


(defn preprocess-resources
  ;; this is pure transformation of original resources (i.e. without context)
  [ztx]
  (swap! ztx assoc :fhir/inter
         (sp/transform [sp/MAP-VALS sp/MAP-VALS]
                       c/process-on-load
                       (:fhir/src @ztx))))


(defn load-all [ztx package & [{:keys [params node-modules-folder whitelist]
                                :or {node-modules-folder "node_modules"}}]]
  (doseq [pkg-dir (->> [(io/file node-modules-folder)
                        (io/file (str node-modules-folder "/node_modules"))]
                       (mapcat (fn [dir] (when (c/dir? dir) (cons dir (.listFiles dir)))))
                       (mapcat (fn [x] (if (and (c/dir? x) (str/starts-with? (.getName x) "@"))
                                         (.listFiles x)
                                         [x])))
                       (filter c/dir?)
                       distinct)
          :when   (.exists (io/file (str (.getPath pkg-dir) "/.index.json")))
          :let    [package (c/read-json (str (.getPath pkg-dir) "/package.json"))
                   index   (c/read-json (str (.getPath pkg-dir) "/.index.json"))
                   package-params (get params (:name package))]
          header (:files index)
          :let   [rt-whitelist (get whitelist (:resourceType header))
                  rt-blacklist (get blacklist (:resourceType header))]
          :when  (and
                  (or (nil? rt-blacklist)
                      (not (contains? rt-blacklist (:url header))))
                  (or (nil? rt-whitelist)
                      (contains? rt-whitelist (:url header))))]
    (load-json-file ztx package header
                    (io/file (str (.getPath pkg-dir) "/" (:filename header)))
                    {:params package-params}))
  (preprocess-resources ztx)
  (swap! ztx assoc-in [:fhir/inter
                       "StructureDefinition"
                       "http://nictiz.nl/fhir/StructureDefinition/zib-NutritionAdvice"
                       :|
                       :orderer
                       :|
                       :extension
                       :slicing
                       :slices]
         {:practitionerrolereference
          {:sliceName "practitionerrolereference",
           :type "Extension",
           :maxItems 1,
           :fhir/extension
           "http://nictiz.nl/fhir/StructureDefinition/practitionerrole-reference"}})
  (c/process-resources ztx)
  :done)
