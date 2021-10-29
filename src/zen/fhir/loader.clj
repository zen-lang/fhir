(ns zen.fhir.loader
  (:require [zen.core :as zen]
            [zen.fhir.value-set-expand]
            [cheshire.core]
            [clojure.java.io :as io]
            [fipp.edn]
            [clojure.string :as str]
            [zen.fhir.utils :as utils]
            [edamame.core :as edamame]
            [com.rpl.specter :as sp]))

(def poly-id-terminator "[x]")

(defn drop-poly-name [id poly-name]
  (subs id (count poly-name)))

(defn drop-poly-terminator [id]
  (subs id 0 (- (count id) (count poly-id-terminator))))

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
                                     {:key slice-part :type :slice }]
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

(defn ^String decapitalize-first-letter
  "Converts first character of the string to lower-case, all other characters leaves as is"
  [^CharSequence s]
  (let [s (.toString s)]
    (if (< (count s) 2)
      (.toLowerCase s)
      (str (.toLowerCase (subs s 0 1))
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
  (let [tp   (first (:type el))
        tpc  (:code tp)
        prof (:targetProfile tp)]
    (if (and (= tpc "Reference") prof)
      (assoc el :profiles (into #{} prof))
      el)))


(defn extension-profiles [el]
  (if-let [ext-profs (:profile (first (:type el)))]
    (do
      (assert (= 1 (count ext-profs)) (pr-str :unexpected-extension-profiles (:type el)))
      (assoc el :fhir/extension (first ext-profs)))
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
  (if (str/ends-with? (str (or (:path el) (:id el))) "[x]")
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
    (cond-> (dissoc el :binding)
      (contains? #{"required" "preferred"} (:strength bn))
      (assoc :binding (-> bn
                          (dissoc :extension)
                          (update :valueSet parse-canonical-url))))
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

(defn normalize-element [x & [stu3?]]
  (-> (dissoc x
              :mapping :constraint :extension :comment :comments :requirements :definition :alias
              :meaningWhenMissing :isModifierReason)
      (normalize-binding)
      (normalize-require)
      (normalize-arity)
      (normalize-polymorphic stu3?)
      (normalize-content-ref)
      (normalize-flags)))


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
                                     (assert (= (name k) (:sliceName v)) (pr-str :slice-name k (:sliceName v)))
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


(defn load-intermidiate [res]
  ;; fix FHIR bug - element missed derivation
  (let [res (if (= "Element" (:id res))
              (update res :derivation (fn [x] (or x "specialization")))
              res)]
    (assert (:derivation res) (str ":derivation is required " (pr-str (:url res))))
    (let [stu3? ((fnil str/starts-with? "") (:fhirVersion res) "3")]
      (->> (get-in res [:differential :element])
           (mapv #(normalize-element % stu3?))
           (group-elements (select-keys res [:kind :abstract :derivation
                                             :baseDefinition :description :fhirVersion :type :url]))
           (normalize-description)
           (normalize-extension)
           (merge
            (when-let [package-ns (:zen.fhir/package-ns res)]
              {:zen.fhir/package-ns package-ns
               :zen.fhir/schema-ns (symbol (str (name package-ns) \. (:id res)))}))))))


(defmulti process-on-load
  (fn [res] (keyword (:resourceType res))))


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


(defmethod process-on-load :ValueSet
  [res]
  (merge
    res
    (when-let [package-ns (:zen.fhir/package-ns res)]
      {:zen.fhir/package-ns package-ns
       :zen.fhir/schema-ns (symbol (str (name package-ns) \. "value-set" \. (:id res) ))
       :zen.fhir/resource (dissoc res :zen.fhir/file :zen.fhir/package :zen.fhir/package-ns :zen.fhir/header)
       :fhir/concepts (let [inter-part (select-keys res [:zen.fhir/file :zen.fhir/package :zen.fhir/package-ns :zen.fhir/header])]
                        (->> (select-keys (:compose res) [:include :exclude])
                             vals
                             (apply concat)
                             (filter :concept)
                             (map (fn [{:keys [system concept]}]
                                    (into {}
                                          (map (juxt :id identity))
                                          (extract-concepts inter-part
                                                            (fn [{:keys [code]}] (str/replace (str system \/ code) \/ \-))
                                                            system
                                                            concept))))
                             (apply utils/safe-merge-with map? merge)))})))


(defmethod process-on-load :CodeSystem
  [res]
  (merge
   (dissoc res :concept)
   {:fhir/concepts (into {} (map (juxt :id identity))
                         (extract-concepts (select-keys res [:zen.fhir/file :zen.fhir/package :zen.fhir/package-ns :zen.fhir/header])
                                           (fn [{:keys [code]}] (str/replace (str (:url res) \/ code) \/ \-))
                                           (:url res)
                                           (:concept res)))}
   {:zen.fhir/resource (dissoc res :concept :zen.fhir/file :zen.fhir/package :zen.fhir/package-ns :zen.fhir/header)}))


(defmethod process-on-load :StructureDefinition
  [res]
  (load-intermidiate res))

;; TODO filter by resource type
(defn load-definiton [ztx opts res]
  (let [rt (:resourceType res)
        url (:url res)]
    (if (and rt url)
      (swap! ztx update-in [:fhir/inter rt url]
             (fn [x] (when x (println :override-resource url))
               (assoc (process-on-load res) :zen/loader (:zen/loader res))))
      (println :skip-resource "no url or rt" (get-in res [:zen/loader :file])))))

(defn read-json [f] (cheshire.core/parse-string (slurp f) keyword))

(defn base-url [subj]
  (println(:type subj) (pr-str :no-type-in subj))
  (or (:baseDefinition subj)
      (str "http://hl7.org/fhir/StructureDefinition/" (:type subj))))

(defn get-definition [ztx url]
  (get-in @ztx [:fhir/inter "StructureDefinition" url]))

(defn get-type-definition [ztx subj el type-name]
  (let [tp (if (str/starts-with? type-name "http://hl7.org/fhirpath/System.")
             (str (str/lower-case (subs type-name 31 32)) (subs type-name 32))
             type-name)
        definition (get-definition ztx (str "http://hl7.org/fhir/StructureDefinition/" tp))]
    (when-not definition
      (throw (Exception.
              (str "Could not find type definition: " (pr-str tp) " url " (pr-str (str "http://hl7.org/fhir/StructureDefinition/" tp))
                   " in "    (pr-str (:url  subj))
                   " element " (pr-str el)
                   " file: " (pr-str (get-in subj [:zen/loader :file]))))))
    definition))


(defn is-profile? [url subj]
  (and (= "constraint" (:derivation subj))
       (not (or (= "Extension" (:type subj))
                (:fhir/extension subj)))
       #_(not (= url (base-url subj)))))


(defn get-bases [ztx subj]
  (loop [base       (:baseDefinition subj)
         base-stack []
         bases      #{}]
    (if (or (nil? base)
            (contains? bases base))
      base-stack
      (let [base-def (get-definition ztx base)]
        (recur (:baseDefinition base-def)
               (conj base-stack base-def)
               (conj bases base))))))

(defn get-base-elements [ztx subj k el bases]
  (let [elements-stack bases ;;(cons el bases) ;; ????
        base-elements  (keep #(get-in % [:| k]) (reverse elements-stack))
        types          (cond-> (set (keep #(get-in % [:type]) base-elements))
                         (:type el) (conj (:type el)))
        types-defs     (map (fn [x] (get-type-definition ztx subj el x)) types)]
    (not-empty (vec (concat base-elements types-defs)))))


(defn get-base-poly-key [ztx k bases]
  (some #(get-in % [:fhir-poly-keys k]) bases))


(defn make-first-class-ext-keys [acc el]
  (->> (get-in el [:slicing :slices])
       (reduce (fn [acc [ext-k ext-el]]
                 (assert (= ext-k (keyword (:sliceName ext-el))) (pr-str ext-k "!=" (:sliceName ext-el)))
                 (assoc acc ext-k (dissoc ext-el :type :sliceName)))
               acc)))


(defn fhir-primitive? [_el base-els]
  (some #(= "primitive-type" (:kind %))
        base-els))


(defn enrich-element [ctx el base-els]
  ;; TODO: if vector do min/max items
  ;;       required/prohibited
  ;;       tragetProfile type profile
  (letfn [(make-first-class-extensions [acc [k el]]
            (if (and (= :extension k) (not (:do-not-handle-first-class-ext? ctx)))
              (make-first-class-ext-keys acc el)
              (assoc acc k el)))]
    (let [v? (some :vector base-els)
          tp (or (:type el)
                 (->> base-els
                      (filter (fn [{tp :type}] (and (not (nil? tp))
                                                    (not (= "Element" tp)))))
                      (some :type)))]
      (cond-> el
        v?                            (assoc :vector true)
        (not v?)                      (dissoc :minItems :maxItems)
        tp                            (assoc :type tp)
        (seq (:| el))                 (update :| (partial reduce make-first-class-extensions {}))
        (fhir-primitive? el base-els) (assoc :fhir/primitive-attr true)))))


(defn search-base-elements [ztx subj k el bases]
  (if-let [b-els (get-base-elements ztx subj k el bases)]
    {:el-key k, :element el, :base-elements b-els}
    (let [fix-poly-k (keyword (decapitalize-first-letter (name k)))]
      (when-let [b-els (get-base-elements ztx subj fix-poly-k el bases)]
        {:el-key fix-poly-k, :element el, :base-elements b-els}))))


(defn find-poly-base-el [ztx subj k el bases]
  (when-let [{poly-key :key, poly-type :type} (get-base-poly-key ztx k bases)]
    (let [poly-el  {:| {(keyword poly-type) (assoc el :type poly-type)}}
          base-els (get-base-elements ztx subj poly-key poly-el bases)]
      {:el-key        poly-key
       :element       poly-el
       :base-elements base-els})))


(defn find-base-els [ztx subj k el bases]
  (let [{:as   search-result
         :keys [el-key element base-elements]
         :or   {el-key k, element el, base-elements []}}
        (search-base-elements ztx subj k el bases)]
    (if (seq base-elements)
      search-result
      (find-poly-base-el ztx subj el-key element bases))))

(defn primitive-element-key [primitive-k]
  (keyword (str "_" (name primitive-k))))

(defn primitive-element [primitive]
  (dissoc primitive :fhir/primitive-attr :type))



;; Profile.element
;; Base.element
(defn walk-with-bases [ztx ctx subj bases]
  (let [enr-subj (enrich-element ctx subj bases)]
    (cond-> enr-subj
      (seq (:| enr-subj))
      (update :| (fn [els]
                   (->> els
                        (reduce (fn [acc [k el]]
                                  (let [{:keys [el-key element base-elements]
                                         :or   {el-key k, element el, base-elements []}}
                                        (find-base-els ztx subj k el bases)

                                        new-ctx (-> (update ctx :lvl inc) (update :path conj el-key))]
                                    (when (and (not= "specialization" (:derivation ctx)) (empty? base-elements))
                                      (println :no-base-for-element (conj (:path ctx) k) el))
                                    (assoc acc el-key (walk-with-bases ztx new-ctx element base-elements))))
                                {})
                        (reduce (fn [acc [k el]]
                                  (if (:fhir/primitive-attr el)
                                    (let [element-key (primitive-element-key k)
                                          element-attr (assoc (select-keys el [:vector])
                                                              :type "Element"
                                                              :original-key k)]
                                      (assoc acc k el, element-key element-attr))
                                    (assoc acc k el)))
                                {})))))))


(defn is-extension?
  [_url subj]
  (= "Extension" (:type subj)))


(defn process-extension
  [ztx url subj]
  subj)


(defn collect-extension-profiles [acc path v]
  (if-let [url (:fhir/extension v)]
    (update-in acc ["StructureDefinition" url] (comp vec distinct concat) [(conj path :fhir/extension)])
    acc))


(defn collect-types [acc path v]
  (reduce (fn [acc' el-type]
            (update-in acc'
                       ["StructureDefinition" (str "http://hl7.org/fhir/StructureDefinition/" el-type)]
                       (comp vec distinct concat) [(conj path :type)]))
          acc
          (cons (:type v) (:types v))))


(defn collect-references [acc path v]
  (reduce (fn [acc' profile-url]
            (update-in acc' ["StructureDefinition" profile-url] (comp vec distinct concat) [(conj path :profiles)]))
          acc
          (:profiles v)))


(defn collect-valuesets [acc path v]
  (if-let [{:keys [url version]} (get-in v [:binding :valueSet])]
    (update-in acc ["ValueSet" url version] (comp vec distinct concat) [(conj path :binding)])
    acc))


(declare collect-nested)


(defn collect-element [path-fn acc [k v]]
  (let [new-path (path-fn k)]
    (-> acc
        (collect-nested new-path v)
        (collect-extension-profiles new-path v)
        (collect-types new-path v)
        (collect-references new-path v)
        (collect-valuesets new-path v))))


(defn collect-nested [acc path subj]
  (as-> acc acc
    (reduce (partial collect-element (fn [k] (-> (butlast path) vec (conj k))))
            acc
            (:slice subj))
    (reduce (partial collect-element (fn [k] (conj path k)))
            acc
            (:| subj))))


(defn collect-deps [sd-processed]
  (as-> {"StructureDefinition" {(:baseDefinition sd-processed) [[:baseDefinition]]}} acc
    (collect-element (constantly []) acc [nil sd-processed])))


(defn process-sd [ztx url subj]
  (let [processed-sd
        (if (is-extension? url subj)
          (process-extension ztx url subj)
          (let [bases (get-bases ztx subj)]
            (when (and (= "constraint" (:derivation subj)) (empty? bases))
              (println :no-base-resource (pr-str url)))
            (walk-with-bases ztx {:lvl 0
                                  :path [url]
                                  :derivation (:derivation subj)
                                  :do-not-handle-first-class-ext?
                                  (or (= "http://hl7.org/fhir/StructureDefinition/Element" (:url subj))
                                      (= "http://hl7.org/fhir/StructureDefinition/DomainResource" (:url subj)))}
                             subj
                             bases)))]
    (assoc processed-sd :deps (collect-deps processed-sd))))


(defn process-structure-definitions [ztx]
  (swap! ztx update-in [:fhir/inter "StructureDefinition"]
         (partial reduce (fn [acc [url resource]]
                           (assoc acc url (process-sd ztx url resource)))
                  {})))

(defn collect-concepts [ztx]
  (let [code-systems (vals (get-in @ztx [:fhir/inter "CodeSystem"]))
        value-sets (vals (get-in @ztx [:fhir/inter "ValueSet"]))
        concepts (into {} (mapcat :fhir/concepts) (concat value-sets code-systems))]
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
                        %)))

(defn process-resources
  "this is processing of resources with context"
  [ztx]
  (process-structure-definitions ztx)
  (process-concepts ztx))


(defn dir? [^java.io.File file]
  (and (.isDirectory file)
       (not (str/starts-with? (.getName file) "."))))

;; TODO write test with all corner cases of npm dir organization
(defn find-packages [project-root]
  (->> [(io/file project-root)
        (io/file (str project-root "/node_modules"))]
       (mapcat (fn [dir] (when (dir? dir) (cons dir (.listFiles dir)))))
       (mapcat (fn [x] (if (and (dir? x) (str/starts-with? (.getName x) "@")) (.listFiles x) [x])))
       (filter dir?)
       distinct
       (filter (fn [f] (.exists (io/file (str (.getPath f) "/package.json")))))))


(defn do-load-file [ztx opts package f]
  (let [file-name (.getName f)
        content (cond
                  (str/ends-with? file-name ".json")
                  (cheshire.core/parse-string (str/replace (slurp f) \ufeff \space) keyword)

                  (str/ends-with? file-name ".edn")
                  (edamame/parse-string (slurp f)))]
    (when content
      (load-definiton ztx opts (assoc content
                                      :zen/loader {:package package :file (.getPath f)}
                                      :zen.fhir/package-ns (some-> package :name (str/replace #"\." "-") symbol))))))

(defn load-all [ztx opts project-dir]
  (doseq [pkg-dir  (find-packages project-dir)]
    (let [package (read-json (str (.getPath pkg-dir) "/package.json"))]
      (assert package (str "No package for " pkg-dir))
      (doseq [f (.listFiles pkg-dir)]
        (do-load-file ztx opts package f))))
  (process-resources ztx)
  :done)
