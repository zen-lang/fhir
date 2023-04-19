(ns zen.fhir.loader
  (:require [zen.fhir.search-parameter.loader :as search-parameter.loader]
            [zen.fhir.structure-definition.loader :as structure-definition.loader]
            [zen.core :as zen]
            [ftr.extraction.ig.core]
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
                         (some? (:recur el))
                         (update :recur-refs #(conj (set %) (:recur el)))

                         (= :poly (:type last-part))
                         (update-in (conj el-parent-path :fhir-poly-keys)
                                    merge
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
      (if (= 1 (count ext-profs))
          (assoc el :fhir/extension (first ext-profs))
          (assoc el :fhir/element-profiles ext-profs)))
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
  (if (or (str/ends-with? (str (:id el)) "[x]")
          (>= (count (:type el)) 2))
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


(defn deduce-ref-type [content-reference]
  (cond
    (nil? content-reference)
    :none

    (str/starts-with? content-reference "#")
    :local-ref

    (str/starts-with? content-reference "http://")
    :external-ref

    :else
    :unknown))


(defn normalize-content-ref [x parent-resource]
  (case (deduce-ref-type (:contentReference x))

    :none
    x

    :local-ref
    (let [package-ns (:zen.fhir/package-ns parent-resource)
          schema-ns  (when package-ns
                       (str (name package-ns) "." (:id parent-resource)))
          el-path    (-> (:contentReference x)
                         (str/split #"\.")
                         rest
                         (->> (map keyword)))
          cr-name    (str/join "-" (concat (map name el-path) ["schema"]))
          cr-symbol  (symbol schema-ns cr-name)]
      (assoc x :recur {:symbol cr-symbol
                       :path el-path}))

    :external-ref
    (do (println :unsupported-content-reference-type (:contentReference x))
        x)

    :unknown
    (do (println :unknown-content-reference-type (:contentReference x))
        x)))


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


(defn normalize-element [x parent-resource & [stu3?]]
  (-> (dissoc x
              :mapping :constraint :extension :comment :comments :requirements :definition :alias
              :meaningWhenMissing :isModifierReason)
      (normalize-binding)
      (normalize-require)
      (normalize-arity)
      (normalize-polymorphic stu3?)
      (normalize-nested)
      (normalize-content-ref parent-resource)
      (normalize-flags)
      (normalize-fixed)
      (normalize-pattern)))


(defn normalize-description [res]
  (-> (dissoc res :description :short)
      (assoc :text-description (or (:short res) (:description res)))))
;; ADD check by http://www.hl7.org/fhir/elementdefinition.html#interpretation


(defn deduce-extension-base-definition [res value]
  (let [tp (or (first (:types value))
               (:type value))]
    (cond
      (and (some? (:baseDefinition res))
           (not= "http://hl7.org/fhir/StructureDefinition/Extension"
                 (:baseDefinition res)))
      {:baseDefinition (:baseDefinition res)}

      (and (some? tp) (not= "Extension" tp))
      {:baseDefinition (str "http://hl7.org/fhir/StructureDefinition/" tp)})))


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
        (dissoc :baseDefinition)
        (merge (deduce-extension-base-definition res nil)))

    (= 1 (count (get-in res [:| :value :types]))) ;; value[x] with a single type
    (let [value (get-in res [:| :value])]
      (merge (dissoc res :| :fhir-poly-keys :baseDefinition)
             (dissoc value :| :types :minItems :maxItems :required :polymorphic)
             (dissoc (first (vals (:| value))))
             {:kind "first-class-extension"}
             (deduce-extension-base-definition res value)))

    (< 1 (count (get-in res [:| :value :types]))) ;; value[x] with multile types
    (merge
      (dissoc res :| :fhir-poly-keys :baseDefinition :minItems :maxItems)
      (dissoc (get-in res [:| :value]) :fhir-poly-keys))

    (and (= 1 (count (dissoc (:| res) :url :extension)))
         (= 1 (count (get-in res [:| :value :|]))))
    (let [value (first (vals (get-in res [:| :value :|])))]
      (merge (dissoc res :| :fhir-poly-keys :baseDefinition) ;; baseDefinition here is http://.../Extension, thus dissoc
             (dissoc value :minItems :maxItems :required :polymorphic :sliceName)
             {:kind "first-class-extension"}
             (deduce-extension-base-definition res value)))

    (and (= 1 (count (dissoc (:| res) :url :extension)))
         (< 1 (count (get-in res [:| :value :|]))))
    (assert false (pr-str :not-supported res))

    (= 1 (count (dissoc (:| res) :url :extension))) ;; extension with a single value
    (let [value (first (vals (dissoc (:| res) :url :extension)))]
      (merge (dissoc res :| :fhir-poly-keys :baseDefinition) ;; baseDefinition here is http://.../Extension, thus dissoc
             (dissoc value :minItems :maxItems :required :polymorphic)
             {:kind "first-class-extension"}
             (deduce-extension-base-definition res value)))

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
           (mapv #(normalize-element % res stu3?))
           (group-elements (select-keys res [:kind :abstract :derivation
                                             :baseDefinition :description :fhirVersion :type :url]))
           (normalize-description)
           (normalize-extension)
           (normalize-slicing)
           (merge
             (when-let [package-ns (:zen.fhir/package-ns res)]
               {:zen.fhir/package-ns package-ns
                :zen.fhir/schema-ns (symbol (str (name package-ns) \. (:id res)))}))))))


(def process-on-load ftr.extraction.ig.core/process-on-load)


(defmethod ftr.extraction.ig.core/process-on-load :SearchParameter [res]
  (search-parameter.loader/process-on-load res))


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


(def reduce-concept ftr.extraction.ig.core/reduce-concept)


(def extract-concepts ftr.extraction.ig.core/extract-concepts)


(def loader-keys ftr.extraction.ig.core/loader-keys)


(defmethod ftr.extraction.ig.core/process-on-load :StructureDefinition
  [res]
  (load-intermidiate res))


(def load-definiton ftr.extraction.ig.core/load-definition)
(def load-definition ftr.extraction.ig.core/load-definition)


(def read-json ftr.extraction.ig.core/read-json)


(def collect-concepts  ftr.extraction.ig.core/collect-concepts)
(def process-concept   ftr.extraction.ig.core/process-concept)
(def process-concepts  ftr.extraction.ig.core/process-concepts)


(def process-resources ftr.extraction.ig.core/process-resources)


(defmethod ftr.extraction.ig.core/process-resource-type :StructureDefinition [_ ztx & [_params]]
  (structure-definition.loader/process-structure-definitions ztx))


(defmethod ftr.extraction.ig.core/process-resource-type :SearchParameter [_ ztx & [_params]]
  (search-parameter.loader/process-search-parameters ztx))


(defn dir? [^java.io.File file]
  (and (.isDirectory file)
       (not (str/starts-with? (.getName file) "."))))


;; TODO write test with all corner cases of npm dir organization
(def find-packages ftr.extraction.ig.core/find-packages)


(def package-blacklist ftr.extraction.ig.core/package-blacklist)


(def blacklisted-package? ftr.extraction.ig.core/blacklisted-package?)


(def do-load-file ftr.extraction.ig.core/do-load-file)


(comment
  (def b (init-ztx))

  (def a (do-load-file b {} {:name "abc"} (clojure.java.io/file "/tmp/aaa.json")))

  (zen.fhir.generator/generate-zen-schemas b)
 (:fhir.zen/ns @b)

  (def aaa (get-in a [:fhir/inter "StructureDefinition" ]))

  )

(def init-ztx ftr.extraction.ig.core/init-ztx)


(def preload-all ftr.extraction.ig.core/preload-all)


(def load-all ftr.extraction.ig.core/load-all)
