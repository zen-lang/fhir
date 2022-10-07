(ns zen.fhir.loader
  (:require [zen.fhir.search-parameter.loader :as search-parameter.loader]
            [zen.fhir.structure-definition.loader :as structure-definition.loader]
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


(def poly-id-terminator "[x]")


(defn drop-poly-name [id poly-name]
  (subs id (count poly-name)))


(defn drop-poly-terminator [id]
  (subs id 0 (- (count id) (count poly-id-terminator))))


(defn rich-parse-path-full [id]
  (if (str/blank? id)
    []
    (->> (str/split id #"\.")
         (mapcat
           (fn [id-part]
             (let [[key-part slice-part] (str/split id-part #":" 2)]
               (cond
                 (str/ends-with? key-part poly-id-terminator)
                 (let [poly-name (drop-poly-terminator key-part)]
                   (cond-> [{:type :poly :key poly-name}]
                     (some? slice-part) (conj {:type :poly-slice
                                               :key  (drop-poly-name slice-part poly-name)
                                               :poly-name poly-name})))

                 (some? slice-part) [{:key key-part :type :key}
                                     {:key slice-part :type :slice}]
                 :else              [{:key key-part
                                      :type :key}]))))
         vec)))


(defn rich-parse-path [id]
  (if (str/blank? id)
    []
    (->> (rest (str/split id #"\."))
         (mapcat
           (fn [id-part]
             (let [[key-part slice-part] (str/split id-part #":" 2)]
               (cond
                 (str/ends-with? key-part poly-id-terminator)
                 (let [poly-name (drop-poly-terminator key-part)]
                   (cond-> [{:type :poly :key poly-name}]
                     (some? slice-part) (conj {:type :poly-slice
                                               :key  (drop-poly-name slice-part poly-name)
                                               :poly-name poly-name})))

                 (some? slice-part) [{:key key-part :type :key}
                                     {:key slice-part :type :slice}]
                 :else              [{:key key-part
                                      :type :key}]))))
         vec)))


(defn build-path [id-path]
  (->> id-path
       (reduce (fn [acc {k :key tp :type}]
                 (let [k (keyword k)]
                   (case tp
                     :key        (conj acc :| k)
                     :slice      (conj acc :slicing :slices k)
                     :poly       (conj acc :| k)
                     :poly-slice (conj acc :| k))))
               [])))


(defn ^String capitalize-first-letter
  "Converts first character of the string to upper-case, all other characters leaves as is"
  [^CharSequence s]
  (let [s (.toString s)]
    (if (< (count s) 2)
      (.toUpperCase s)
      (str (.toUpperCase (subs s 0 1))
           (subs s 1)))))


(defn build-fhir-poly-keys-mapping [poly-key types]
  (into {}
        (map (fn [el-type]
               (let [fhir-poly-key (str poly-key (capitalize-first-letter el-type))]
                 (-> {(keyword fhir-poly-key)
                      {:key (keyword poly-key)
                       :type el-type}}))))
        types))


;; polymorphic path
;; extensions path
(defn group-elements [acc els]
  (->> els
       (reduce (fn [acc {id :id pth :path :as el}]
                 (let [id-path  (rich-parse-path id)
                       root-el? (empty? id-path)]
                   (if root-el?
                     (-> (merge acc el)
                         (dissoc  :vector :id :path :short :example))
                     (let [last-part      (last id-path)
                           el-path        (build-path id-path)
                           el-root-path   (vec (butlast el-path))
                           el-parent-path (vec (butlast el-root-path))]
                       (cond-> acc
                         (= :poly (:type last-part))
                         (assoc-in (conj el-parent-path :fhir-poly-keys)
                                   (build-fhir-poly-keys-mapping (:key last-part) (:types el)))

                         :always
                         (assoc-in el-path (dissoc el :id :path :example) #_(select-keys el [:id :| :polymorphic])))))))
               acc)))


(defn reference-profiles [el]
  (let [tp       (first (:type el))
        tpc      (:code tp)
        prof     (:targetProfile tp)
        profiles (if (string? prof) [prof] prof)]
    (if (and (= tpc "Reference") profiles)
      (assoc el :profiles (into #{} profiles))
      el)))


(defn extension-profiles [el]
  (if-let [ext-profs (:profile (first (:type el)))]
    (let [ext-profs (if (string? ext-profs) [ext-profs] ext-profs)]
      (do
        (assert (= 1 (count ext-profs)) (pr-str :unexpected-extension-profiles (:type el)))
        (assoc el :fhir/extension (first ext-profs))))
    el))


(defn get-type-code[{code :code extension :extension}]
  ;; wellknonw bug in FHIR SDs
  ;; StructureDefinition generator has a bug
  ;; instead of id type it uses exension type
  ;; https://chat.fhir.org/#narrow/stream/179283-Da-Vinci/topic/Type.20of.20id/near/232607087
  (or (some-> (utils/code-search :url
                                 ["http://hl7.org/fhir/StructureDefinition/structuredefinition-fhir-type"]
                                 extension)
              (utils/poly-get :value))
      code))


(defn normalize-polymorphic [el & [stu3?]]
  (if (str/ends-with? (str (:id el)) "[x]")
    (-> (assoc el :polymorphic true)
        (dissoc :type)
        (assoc :| (->> (:type el)
                         (reduce (fn [acc {c :code :as tp}]
                                   (assoc acc (keyword c) (-> (reference-profiles {:type [tp]})
                                                              (assoc :type (get-type-code tp)))))
                                 {})))
        (assoc :types (->> (:type el) (map :code) (into #{}))))
    (if-not (:type el)
      el
      (cond
        stu3?
        ;; In STU3 type is an array of entries each of which can contain
        ;; a `profile` or a `targetProfile` (or both).
        ;; In R4 each type can contain an array of `profile`s
        ;; or an array or `targetProfile`-s (or both)
        (let [profiles (mapv :profile (:type el))
              target-profiles (mapv :targetProfile (:type el))
              type' (-> (first (:type el))
                        (assoc :profile profiles
                               :targetProfile target-profiles)
                        (utils/sanitize-obj))
              el' (assoc el :type [type'])]
          (normalize-polymorphic el'))

        (= 1 (count (:type el)))
        (let [tp  (first (:type el))
              tpc (get-type-code tp)]
          (-> el
              (reference-profiles)
              (extension-profiles)
              (assoc :type tpc)))

        :else ;; NOTE: allowed by FHIR but not implemented.
        (throw (Exception. (pr-str el)))))))


(defn root-element? [el-path]
  (not (str/includes? (str el-path) ".")))


(defn normalize-require [{:as element, el-min :min}]
  (if (pos? (or el-min 0))
    (assoc element :required true)
    element))


;; ;; why not use settings of base for arity
;; (defn fix-arity
;;   "The first ElementDefinition (root element) usually has max=* which may be treated as a collection
;;   but we are treating StructureDefinition as a tool to validate a single resource"
;;   [{:as element el-type :type} {v :vector r :required base-type :type :as _base}]
;;   (let [tp (or el-type base-type)]
;;     (cond-> (merge element (utils/strip-nils {:vector v :required r}))
;;       tp (assoc :type tp)
;;       (not v) (dissoc :maxItems :minItems))))

(defn normalize-arity
  "The first ElementDefinition (root element) usually has max=* which may be treated as a collection
  but we are treating StructureDefinition as a tool to validate a single resource"
  [{:as element, id :id, el-min :min, el-max :max}]
  (->
    (cond-> element
      (and (not (nil? el-max)) (not (contains? #{"1" "0"} el-max)) (not (root-element? id)))
      (assoc :vector true)

      (and (not (nil? el-min)) (not (= 0 el-min)))
      (assoc :minItems el-min)

      (and (not (nil? el-max)) (not (contains? #{"*"} el-max) ))
      (assoc :maxItems (utils/parse-int el-max)))
    (dissoc :min :max)))


(defn parse-canonical-url [canonical-url]
  (when-not (str/blank? canonical-url)
    (let [parts   (str/split canonical-url #"\|")
          url     (str/join "|" (cons (first parts) (butlast (rest parts))))
          version (last (rest parts))]
      (not-empty (utils/strip-nils {:url url, :version version})))))


(defn normalize-binding [el]
  (if-let [bn (:binding el)]
    (assoc el :binding (-> bn
                           (dissoc :extension)
                           (dissoc :valueSet :valueSetUri :valueSetReference)
                           (assoc :valueSet (parse-canonical-url
                                              (or (get-in bn [:valueSet])
                                                  (get-in bn [:valueSetUri])
                                                  (get-in bn [:valueSetReference :reference]))))))
    el))


(defn normalize-content-ref [x]
  (if-let [cr (:contentReference x)]
    (assoc x :recur (->> (rest (str/split cr #"\."))
                         (mapv keyword)))
    x))


(defn normalize-flags [x]
  (let [flags (cond-> #{}
                (:isModifier x)  (conj :?!)
                (:isSummary x)   (conj :SU)
                (:mustSupport x) (conj :MS))]
    (-> x
        (dissoc :isModifier :isSummary :mustSupport)
        (cond->
            (not (empty? flags)) (assoc :fhir/flags flags)))))

(defn normalize-nested [x]
  (if (= "Resource" (:type x))
    (assoc x :nested true)
    x))


(defn normalize-fixed [res]
  (if-let [fixed (utils/poly-get res :fixed)]
    (assoc res :fixed fixed)
    res))


(defn normalize-pattern [res]
  (if-let [pattern (utils/poly-get res :pattern)]
    (assoc res :pattern pattern)
    res))


(defn normalize-element [x & [stu3?]]
  (-> (dissoc x
              :mapping :constraint :extension :comment :comments :requirements :definition :alias
              :meaningWhenMissing :isModifierReason)
      (normalize-binding)
      (normalize-require)
      (normalize-arity)
      (normalize-polymorphic stu3?)
      (normalize-nested)
      (normalize-content-ref)
      (normalize-flags)
      (normalize-fixed)
      (normalize-pattern)))


(defn normalize-description [res]
  (-> (dissoc res :description :short)
      (assoc :text-description (or (:short res) (:description res)))))
;; ADD check by http://www.hl7.org/fhir/elementdefinition.html#interpretation


(defn *normalize-extension [ext res]
  (cond
    (get-in res [:| :extension :slicing :slices]) ;; slices for different extensions
    (-> (assoc res
               :fhir/extension (get-in res [:| :url :fixedUri])
               :| (->> (get-in res [:| :extension :slicing :slices])
                       (reduce (fn [acc [k v]]
                                 (when-not (= k (keyword (:sliceName v)))
                                   (prn "WARN:" (pr-str k "!=" (:sliceName v))))
                                 (assoc acc k (*normalize-extension ext (dissoc v :sliceName))))
                               {})))
        (dissoc :fhir-poly-keys)
        (cond->
          (= "http://hl7.org/fhir/StructureDefinition/Extension" (:baseDefinition res))
          (dissoc :baseDefinition)))

    (= 1 (count (get-in res [:| :value :types]))) ;; value[x] with a single type
    (let [value (get-in res [:| :value])
          tp    (first (:types value))]
      (merge (dissoc res :| :fhir-poly-keys :baseDefinition)
             (dissoc value :| :types :minItems :maxItems :required :polymorphic)
             (dissoc (first (vals (:| value))))
             {:kind "first-class-extension"
              :baseDefinition (str "http://hl7.org/fhir/StructureDefinition/" tp)}))

    (< 1 (count (get-in res [:| :value :types]))) ;; value[x] with multile types
    (merge
      (dissoc res :| :fhir-poly-keys :baseDefinition :minItems :maxItems)
      (dissoc (get-in res [:| :value]) :fhir-poly-keys))

    (and (= 1 (count (dissoc (:| res) :url :extension)))
         (= 1 (count (get-in res [:| :value :|]))))
    (let [value (first (vals (get-in res [:| :value :|])))]
      (merge (dissoc res :| :fhir-poly-keys :baseDefinition) ;; baseDefinition here is http://.../Extension, thus dissoc
             (dissoc value :minItems :maxItems :required :polymorphic :sliceName)
             {:kind "first-class-extension"
              :baseDefinition (str "http://hl7.org/fhir/StructureDefinition/" (:type value))}))

    (and (= 1 (count (dissoc (:| res) :url :extension)))
         (< 1 (count (get-in res [:| :value :|]))))
    (assert false (pr-str :not-supported res))

    (= 1 (count (dissoc (:| res) :url :extension))) ;; extension with a single value
    (let [value (first (vals (dissoc (:| res) :url :extension)))]
      (merge (dissoc res :| :fhir-poly-keys :baseDefinition) ;; baseDefinition here is http://.../Extension, thus dissoc
             (dissoc value :minItems :maxItems :required :polymorphic)
             {:kind "first-class-extension"
              :baseDefinition (str "http://hl7.org/fhir/StructureDefinition/" (:type value))})) ;; making correct baseDefinition

    (and (get-in res [:| :value]) ;; has value[x], but no types in it
         (empty? (get-in res [:| :value :types])))
    (assert false (pr-str :no-types res))

    (and (= "Extension" (:type res)) ;; nested extension
         (contains? res :fhir/extension)
         (empty? (:| res)))
    (dissoc res :type)

    :else
    (assert false (pr-str :extension-values (:url ext) (dissoc (:| res) :url :extension)))))


(defn normalize-extension [res]
  (if (and (= "Extension" (:type res))
           (not= "http://hl7.org/fhir/StructureDefinition/Extension" (:url res)))
    (assoc (*normalize-extension res res)
           :fhir/extension (:url res))
    res))


(declare normalize-slicing)


(def preprocess-slices-by-discriminator-dispatch
  (fn [_slices discriminator]
    (keyword (:type discriminator))))


(defmulti preprocess-slices-by-discriminator #'preprocess-slices-by-discriminator-dispatch)


(defn pattern->zen-match [k pattern]
  (if (= :patternCanonical k)
    (let [{:keys [url version]} (parse-canonical-url pattern)]
      (if (and url version)
        (list :zen.match/one-of #{url pattern})
        url))
    (clojure.walk/postwalk
      (fn [x]
        (if (and (sequential? x) (not (map-entry? x)))
          (set x)
          x))
      pattern)))


(defn slice-discriminator->match [slices discriminator d-type-key]
  (let [rich-path  (->> (rich-parse-path-full (:path discriminator))
                        (remove (fn [path-el]
                                  (assert (= :key (:type path-el)))
                                  (= "$this" (:key path-el)))))
        inter-path (build-path rich-path)
        path       (mapv (comp keyword :key) rich-path)]
    (sp/transform [sp/MAP-VALS]
                  (fn [v]
                    (if-let [[pattern-k pattern] (some-> (get-in v inter-path) (utils/poly-find d-type-key))]
                      (let [match (cond->> (pattern->zen-match pattern-k pattern)
                                    (seq path)
                                    (assoc-in (:match v) path))]
                        (-> (if (seq path)
                              (update-in v path dissoc pattern-k)
                              (dissoc v pattern-k))
                            (assoc :match match)))
                      v))
                  slices)))

(defmethod preprocess-slices-by-discriminator :pattern [slices discriminator]
  (slice-discriminator->match slices discriminator :pattern))


(defmethod preprocess-slices-by-discriminator :value [slices discriminator]
  (slice-discriminator->match slices discriminator :fixed))


(defn fix-slices-names [slices]
  (into {}
        (map (fn [[k v]] [(or (:sliceName v) (name k)) v]))
        slices))


(defn preprocess-slicing* [{:as el-with-slicing, :keys [slicing]}]
  (let [slices (reduce preprocess-slices-by-discriminator
                       (fix-slices-names (:slices slicing))
                       (:discriminator slicing))
        processed-slicing (-> slicing
                              (assoc :slices slices)
                              (dissoc :discriminator))]
    (-> el-with-slicing
        (dissoc :slicing)
        (assoc :fhir/slicing processed-slicing))))


(def supported-discriminator-type
  (set (keys (methods preprocess-slices-by-discriminator))))


(defn preprocess-slicing [[k v]]
  (if (and (contains? v :slicing)
           (not= :extension k) #_"NOTE: slicing in extensions is processed differently")
    (if-let [v' (and (seq (get-in v [:slicing :discriminator]))
                     (every? (comp supported-discriminator-type keyword :type)
                             (get-in v [:slicing :discriminator]))
                     (preprocess-slicing* v))]
      [k (normalize-slicing v')]
      (when-let [sliceless-v (not-empty (dissoc v :slicing))]
        [k sliceless-v]))
    [k (normalize-slicing v)]))


(defn normalize-slicing [res]
  (cond-> res
    (contains? res :|) (update :| (partial into {} (keep preprocess-slicing)))
    :always            (as-> $ (utils/dissoc-when empty? $ :|))

    (contains? res :slicing) (update-in [:slicing :slices] (partial into {} (keep preprocess-slicing)))
    :always                  (as-> $ (utils/dissoc-when (comp empty? :slices) $ :slicing))

    (contains? res :fhir/slicing) (update-in [:fhir/slicing :slices] (partial into {} (keep preprocess-slicing)))
    :always                       (as-> $ (utils/dissoc-when (comp empty? :slices) $ :fhir/slicing))))


(defn load-intermidiate [res]
  (let [#_#_res (if (or (= "Element" (:id res)) ;; NOTE: fix FHIR bug - element missed derivation
                    (= "Resource" (:id res))
                    (and (= "logical" (:kind res))
                         (nil? (:derivation res))))
              (update res :derivation (fn [x] (or x "specialization")))
              res)]
    #_(assert (:derivation res) (str ":derivation is required " (pr-str (:url res))))
    (let [stu3? ((fnil str/starts-with? "") (:fhirVersion res) "3")]
      (->> (get-in res [:differential :element])
           (mapv #(normalize-element % stu3?))
           (group-elements (select-keys res [:kind :abstract :derivation
                                             :baseDefinition :description :fhirVersion :type :url]))
           (normalize-description)
           (normalize-extension)
           (normalize-slicing)
           (merge
             (when-let [package-ns (:zen.fhir/package-ns res)]
               {:zen.fhir/package-ns package-ns
                :zen.fhir/schema-ns (symbol (str (name package-ns) \. (:id res)))}))))))


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
  (load-intermidiate res))


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
  (init-ztx ztx)
  (doseq [pkg-dir  (find-packages node-modules-folder)]
    (let [package (read-json (str (.getPath pkg-dir) "/package.json"))
          package-params (get params (:name package))]
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
