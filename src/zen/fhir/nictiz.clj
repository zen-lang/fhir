(ns zen.fhir.nictiz
  (:require [zen.fhir.core :as c]
            [clojure.java.io :as io]
            [clojure.string :as str]))


(defn load-all [ztx package & [{:keys [params node-modules-folder whitelist blacklist]
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
    (prn :file (str (.getPath pkg-dir) "/" (:filename header)))
    (c/load-json-file ztx package header
                      (io/file (str (.getPath pkg-dir) "/" (:filename header)))
                      {:params package-params}))
  (c/preprocess-resources ztx)
#_  (get-in @ztx [:fhir/inter
                "StructureDefinition"
                "http://nictiz.nl/fhir/StructureDefinition/zib-NutritionAdvice"
                :|
                :orderer
                :|
                :extension
                :slicing
                :slices])

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

 #_ (get-in @ztx [:fhir/inter
                "StructureDefinition"
                "http://nictiz.nl/fhir/StructureDefinition/zib-NutritionAdvice"
#_#_#_#_#_#_                :|
                :orderer
                :|
                :extension
                :slicing
                :slices])

#_  (c/preprocess-resources ztx)
  (c/process-resources ztx)
  :done)
