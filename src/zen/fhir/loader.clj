(ns zen.fhir.loader
  (:require [zen.fhir.search-parameter.loader :as search-parameter.loader]
            [zen.fhir.structure-definition.loader :as structure-definition.loader]
            [zen.fhir.structure-definition.preloader :as structure-definition.preloader]
            [zen.core :as zen]
            [zen.fhir.value-set-expand]
            [cheshire.core]
            [clojure.java.io :as io]
            [fipp.edn]
            [clojure.string :as str]
            [zen.fhir.utils :as utils]
            [clojure.walk]
            [edamame.core :as edamame]
            [com.rpl.specter :as sp]))

(defmulti process-on-load
  (fn [res] (keyword (:resourceType res))))


(defmethod process-on-load :SearchParameter [res]
  (search-parameter.loader/process-on-load res))


(defmethod process-on-load :default
  [res]
  #_(println :WARN :no-process-on-load :for (:resourceType res)))


(defn build-designation [ds]
  (reduce (fn [acc d]
            (assoc-in acc [(or (get-in d [:use :code]) "display")
                           (or (:language d) "en")]
                      (:value d)))
          {} ds))


(defn get-value [m]
  (let [k (->> (keys m)
               (filter #(str/starts-with? (name %) "value"))
               (first))]
    (get m k)))


(defn build-property [ps]
  (reduce (fn [acc p]
            (assoc acc (:code p) (get-value p)))
          {} ps))


(defn reduce-concept [acc id-fn sys parents c]
  (let [con (-> c
                (select-keys [:code :display :definition])
                (assoc :id (id-fn c)
                       :system sys
                       :_source "zen.fhir"
                       :resourceType "Concept")
                (cond-> (:designation c) (assoc :designation (build-designation (:designation c)))
                        (seq parents) (assoc :hierarchy parents)
                        (:property c) (assoc :property (build-property (:property c)))))
        acc (conj acc con)]
    (if-let [cs (:concept c)]
      (reduce (fn [acc c']
                (reduce-concept acc id-fn sys (conj parents (:code con)) c'))
              acc cs)
      acc)))


(defn extract-concepts [inter-part id-fn sys concept-parts]
  (->> concept-parts
       (reduce (fn [acc c] (reduce-concept acc id-fn sys [] c))
               [])
       (map (fn [concept]
              (-> concept
                  (merge inter-part)
                  (assoc :zen.fhir/resource concept))))))


(def loader-keys
  #{:zen/loader
    :zen.fhir/loader
    :zen.fhir/package
    :zen.fhir/package-ns
    :zen.fhir/schema-ns
    :zen.fhir/file
    :zen.fhir/header
    :zen.fhir/version})


(defmethod process-on-load :ValueSet
  [res]
  (merge
    res
    (when-let [package-ns (:zen.fhir/package-ns res)]
      {:zen.fhir/package-ns package-ns
       :zen.fhir/schema-ns (symbol (str (name package-ns) \. "value-set" \. (:id res)))
       :zen.fhir/resource (apply dissoc res loader-keys)
       :fhir/concepts (let [inter-part (select-keys res loader-keys)
                            concepts (->> (select-keys (:compose res) [:include :exclude])
                                          vals
                                          (apply concat)
                                          (filter :concept))
                            concepts (or (some->> [:expansion :contains]
                                                  (get-in res)
                                                  not-empty
                                                  (map #(assoc % :valueset [(:url res)]))
                                                  (group-by :system)
                                                  (reduce-kv (fn [acc system concepts]
                                                               (conj acc {:system system :concept concepts}))
                                                             [])
                                                  (into concepts))
                                         concepts)]
                        (->> concepts
                             (mapcat (fn [{:keys [system concept]}]
                                       (extract-concepts inter-part
                                                         (fn [{:keys [code]}] (str/replace (str system \/ code) \/ \-))
                                                         system
                                                         concept)))))})))


(defmethod process-on-load :CodeSystem
  [res]
  (merge
   (dissoc res :concept)
   {:fhir/concepts (extract-concepts (select-keys res loader-keys)
                                     (fn [{:keys [code]}] (str/replace (str (:url res) \/ code) \/ \-))
                                     (:url res)
                                     (:concept res))}
   {:zen.fhir/resource (apply dissoc res :concept loader-keys)}))


(defmethod process-on-load :StructureDefinition
  [res]
  (structure-definition.preloader/load-intermidiate res))


;; TODO filter by resource type
(defn load-definiton [ztx opts res]
  (let [rt (:resourceType res)
        url (or (:url res) (:url opts))]
    (if (and rt url)
      (when-let [processed-res (process-on-load res)]
        (swap! ztx update-in [:fhir/inter rt url]
               (fn [x] (when x (println :override-resource url))
                 (merge processed-res
                        {:_source "zen.fhir"
                         :zen.fhir/version (:zen.fhir/version @ztx)}
                        (select-keys res (conj loader-keys :_source))))))
      (println :skip-resource "no url or rt" (get-in res [:zen/loader :file])))))


(defn read-json [f] (cheshire.core/parse-string (slurp f) keyword))

(defn collect-concepts [ztx]
  (let [code-systems (vals (get-in @ztx [:fhir/inter "CodeSystem"]))
        value-sets (vals (get-in @ztx [:fhir/inter "ValueSet"]))
        concepts (transduce (comp (mapcat :fhir/concepts)
                                  (map (fn [concept]
                                         {:path [(:system concept)
                                                 (:id concept)]
                                          :value concept})))
                            (completing
                              (fn [acc {:keys [path value]}]
                                (update-in acc path merge value)))
                            {}
                            (concat value-sets code-systems))]
    (swap! ztx assoc-in [:fhir/inter "Concept"] concepts)))


(defn process-concept [_ztx concept]
  (-> concept
      (assoc-in [:zen.fhir/resource :valueset]
                (vec (:valueset concept)))))


(defn process-concepts [ztx]
  (collect-concepts ztx)
  (zen.fhir.value-set-expand/denormalize-value-sets-into-concepts ztx)
  (swap! ztx update-in [:fhir/inter "Concept"]
         #(sp/transform [sp/MAP-VALS]
                        (partial process-concept ztx)
                        (reduce merge (vals %)))))

(defn process-resources
  "this is processing of resources with context"
  [ztx]
  (structure-definition.loader/process-structure-definitions ztx)
  (search-parameter.loader/process-search-parameters ztx)
  (process-concepts ztx))


(defn dir? [^java.io.File file]
  (and (.isDirectory file)
       (not (str/starts-with? (.getName file) "."))))


;; TODO write test with all corner cases of npm dir organization
(defn find-packages [project-root]
  (->> [(io/file project-root)
        (io/file (str project-root "/node_modules"))]
       (mapcat (fn [dir] (when (dir? dir) (cons dir (.listFiles dir)))))
       (mapcat (fn [x] (if (and (dir? x) (str/starts-with? (.getName x) "@"))
                         (.listFiles x)
                         [x])))
       (filter dir?)
       distinct
       (filter (fn [f] (.exists (io/file (str (.getPath f) "/package.json")))))))



(def package-blacklist
  #{"hl7.fhir.r2.examples"
    "hl7.fhir.r2b.examples"
    "hl7.fhir.r3.examples"
    "hl7.fhir.r4.examples"})


(defn blacklisted-package? [package]
  (contains? package-blacklist (:name package)))


(defn do-load-file [ztx {:as opts :keys [whitelist blacklist params]} package f]
  (let [file-name (.getName f)
        content (cond
                  (str/ends-with? file-name ".json")
                  (try (cheshire.core/parse-string (str/replace (slurp f) \ufeff \space) keyword)
                       (catch Exception e
                         (println :WARN :invalid-json (.getName f) (.getMessage e))))

                  (str/ends-with? file-name ".edn")
                  (edamame/parse-string (slurp f)))
        rt-whitelist (get whitelist (:resourceType content))
        rt-blacklist (get blacklist (:resourceType content))]
    (when (and (not (blacklisted-package? package))
               content
               (or (nil? rt-blacklist)
                   (not (contains? rt-blacklist (:url content))))
               (or (nil? rt-whitelist)
                   (contains? rt-whitelist (:url content))))
      (load-definiton ztx opts (assoc content
                                      :_source "zen.fhir"
                                      :zen.fhir/version (:zen.fhir/version @ztx)
                                      :zen/loader {:package package :file (.getPath f)}
                                      :zen.fhir/package package
                                      :zen.fhir/file (.getPath f)
                                      :zen.fhir/package-ns (or (:zen.fhir/package-ns params)
                                                               (some-> package :name (str/replace #"\." "-") symbol)))))))

(comment
  (def b (init-ztx))

  (def a (do-load-file b {} {:name "abc"} (clojure.java.io/file "/tmp/aaa.json")))

  (zen.fhir.generator/generate-zen-schemas b)
 (:fhir.zen/ns @b)

  (def aaa (get-in a [:fhir/inter "StructureDefinition" ]))

  )

(defn init-ztx
  ([]
   (init-ztx (zen.core/new-context)))

  ([ztx]
   (swap! ztx assoc :zen.fhir/version (slurp (io/resource "zen-fhir-version")))
   ztx))

(defn preload-all [ztx & [{:keys [params node-modules-folder whitelist blacklist]
                           :or {node-modules-folder "node_modules"}}]]
  (println node-modules-folder)
  (init-ztx ztx)
  (doseq [pkg-dir  (find-packages node-modules-folder)]
    (let [package (read-json (str (.getPath pkg-dir) "/package.json"))
          package-params (get params (:name package))]
      (println "HELLO")
      (assert package (str "No package for " pkg-dir))
      (doseq [f (.listFiles pkg-dir)]
        (do-load-file ztx
                      {:params package-params
                       :whitelist whitelist
                       :blacklist (merge-with merge
                                              {"StructureDefinition" #{"http://hl7.org/fhir/StructureDefinition/familymemberhistory-genetic"
                                                                       "http://hl7.org/fhir/uv/sdc/StructureDefinition/parameters-questionnaireresponse-extract-in"}}
                                              blacklist)}
                      package
                      f)))))


(defn load-all [ztx _ & [params]]
  (preload-all ztx params)
  (process-resources ztx)
  :done)

(comment

  (def ztx (zen/new-context))

  (def node-folder "/home/hs/fhir/node_modules")

  (find-packages node-folder)


  (preload-all ztx {:node-modules-folder node-folder})

  (keys (get (:fhir/inter @ztx) "SearchParameter"))

  (structure-definition.loader/process-structure-definitions ztx)

  (def zz (atom @ztx))


  (search-parameter.loader/process-search-parameters ztx)


  (keys (get (:fhir/inter @ztx) "SearchParameter"))

  @ztx

  (get (get (:fhir/inter @ztx) "StructureDefinition") "http://hl7.org/fhir/StructureDefinition/HumanName")


  )
