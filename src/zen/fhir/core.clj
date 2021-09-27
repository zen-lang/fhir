(ns zen.fhir.core
  (:require [zen.core :as zen]
            [cheshire.core]
            [clojure.java.io :as io]
            [fipp.edn]
            [clojure.string :as str]
            [zen.fhir.utils :as utils]
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

(defn normalize-polymorphic [el]
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
      (if (= 1 (count (:type el)))
        (let [tp  (first (:type el))
              tpc (get-type-code tp)]
          (-> el
              (reference-profiles)
              (extension-profiles)
              (assoc :type tpc)))
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

(defn normalize-element [x]
  (-> (dissoc x
              :mapping :constraint :extension :comment :comments :requirements :definition :alias
              :meaningWhenMissing :isModifierReason)
      (normalize-binding)
      (normalize-require)
      (normalize-arity)
      (normalize-polymorphic)
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
        (dissoc :fhir-poly-keys))

    (= 1 (count (get-in res [:| :value :types]))) ;; value[x] with a single type
    (merge (dissoc res :| :fhir-poly-keys)
           {:type (first (get-in res [:| :value :types]))})

    (< 1 (count (get-in res [:| :value :types]))) ;; value[x] with multile types
    (merge
      (dissoc res :| :fhir-poly-keys)
      (dissoc (get-in res [:| :value]) :minItems :maxItems :vector :required :fhir-poly-keys))

    (and (get-in res [:| :value]) ;; has value[x], but no types in it
         (empty? (get-in res [:| :value :types])))
    (assert false (pr-str :no-types res))

    (= 1 (count (dissoc (:| res) :url :extension))) ;; extension with a single value
    (let [value (first (vals (dissoc (:| res) :url :extension)))]
      (merge (dissoc res :| :fhir-poly-keys :baseDefinition) ;; baseDefinition here is http://.../Extension, thus dissoc
             (dissoc value :minItems :maxItems :required)
             {:kind "first-class-extension"
              :baseDefinition (str "http://hl7.org/fhir/StructureDefinition/" (:type value))})) ;; making correct baseDefinition

    (and (= "Extension" (:type res)) ;; nested extension
         (contains? res :fhir/extension)
         (empty? (:| res)))
    (dissoc res :type)

    :else
    (assert false (pr-str :extension-values (:url ext) (dissoc (:| res) :url :extension)))))


(defn normalize-extension [res]
  (if (= "Extension" (:type res))
    (assoc (*normalize-extension res res)
           :fhir/extension (:url res))
    res))


(defn load-intermidiate [res]
  (->> (get-in res [:differential :element])
       (mapv normalize-element)
       (group-elements (select-keys res [:kind :abstract :derivation
                                         :baseDefinition :description :fhirVersion :type :url]))
       (normalize-description)
       (normalize-extension)
       (merge
         (when-let [package-ns (:zen.fhir/package-ns res)]
           {:zen.fhir/package-ns package-ns
            :zen.fhir/schema-ns (symbol (str (name package-ns) \. (:id res)))}))))


(defmulti process-on-load
  (fn [res] (keyword (:resourceType res))))


(defmethod process-on-load :default
  [res]
  #_(println :WARN :no-process-on-load :for (:resourceType res)))


(defmethod process-on-load :ValueSet
  [res]
  (merge
   res
   (when-let [package-ns (:zen.fhir/package-ns res)]
     {:zen.fhir/package-ns package-ns
      :zen.fhir/schema-ns (symbol (str (name package-ns) \. "value-set" \. (:id res) ))
      :zen.fhir/resource (dissoc res :zen.fhir/file :zen.fhir/package :zen.fhir/package-ns :zen.fhir/header)})))


(defn extract-concepts [codesystem]
  (let [zen-fhir-keys (select-keys codesystem [:zen.fhir/file :zen.fhir/package :zen.fhir/package-ns :zen.fhir/header])
        concept-part (-> {:resourceType "Concept"
                          :system       (:url codesystem)
                          :valueset     (some-> (:valueSet codesystem) vector)}
                         utils/strip-nils)]
    (map (fn [concept]
           (let [res (-> (merge concept-part concept)
                         (assoc :id (str (:id codesystem) "/" (:code concept))))]
             (assoc (merge res zen-fhir-keys) :zen.fhir/resource res)))
         (:concept codesystem))))


(defmethod process-on-load :CodeSystem
  [res]
  (merge
   (dissoc res :concept)
   {:fhir/concepts (into {} (map (juxt :id identity)) (extract-concepts res))}
   {:zen.fhir/resource (dissoc res :concept :zen.fhir/file :zen.fhir/package :zen.fhir/package-ns :zen.fhir/header)}))


(defmethod process-on-load :StructureDefinition
  [res]
  (load-intermidiate res))


(defn load-definiton [ztx packages header res]
  (if-let [rt (:resourceType res)]
    (if-let [url (:url header)]
      (swap! ztx update-in [:fhir/src rt url]
             (fn [x] (when x (println :WARN :override-resource header)) res))
      (println :WARN :no-url header))
    (println :WARN :no-resource-type header)))


(defn load-json-file [ztx package header f & [{:keys [params]}]]
  (let [res (-> (cheshire.core/parse-string (slurp f) keyword)
                (assoc :zen.fhir/header header :zen.fhir/package package :zen.fhir/file (.getPath f))
                (merge
                 {:zen.fhir/package-ns (some-> package :name (str/replace #"\." "-") symbol)}
                 (select-keys params #{:zen.fhir/package-ns})))]
    (load-definiton ztx package header res)))


(defn read-json [f] (cheshire.core/parse-string (slurp f) keyword))


(defn base-url [subj]
  (println(:type subj) (pr-str :no-type-in subj))
  (or (:baseDefinition subj)
      (str "http://hl7.org/fhir/StructureDefinition/" (:type subj))))


(defn get-definition [ztx url]
  (get-in @ztx [:fhir/inter "StructureDefinition" url]))


(defn get-type-definition [ztx type-name]
  (let [tp (if (str/starts-with? type-name "http://hl7.org/fhirpath/System.")
             (str (str/lower-case (subs type-name 31 32)) (subs type-name 32))
             type-name)
        definition (get-definition ztx (str "http://hl7.org/fhir/StructureDefinition/" tp))]
    (assert definition (str "Could not find type definition: " tp))
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


(defn get-original [ztx url]
  (get-in @ztx [:fhir/src "StructureDefinition" url]))


(defn get-base-elements [ztx k el bases]
  (let [elements-stack bases ;;(cons el bases) ;; ????
        base-elements  (keep #(get-in % [:| k]) (reverse elements-stack))
        types          (cond-> (set (keep #(get-in % [:type]) base-elements))
                         (:type el) (conj (:type el)))
        types-defs     (map (partial get-type-definition ztx) types)]
    (not-empty (vec (concat base-elements types-defs)))))


(defn get-base-poly-key [ztx k bases]
  (some #(get-in % [:fhir-poly-keys k]) bases))


(defn make-first-class-ext-keys [acc el]
  (->> (get-in el [:slicing :slices])
       (reduce (fn [acc [ext-k ext-el]]
                 (assert (= ext-k (keyword (:sliceName ext-el))) (pr-str ext-k "!=" (:sliceName ext-el)))
                 (assoc acc ext-k (dissoc ext-el :type :sliceName)))
               acc)))


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
        v?            (assoc :vector true)
        (not v?)      (dissoc :minItems :maxItems)
        tp            (assoc :type tp)
        (seq (:| el)) (update :| (partial reduce make-first-class-extensions {}))))))


(defn search-base-elements [ztx k el bases]
  (if-let [b-els (get-base-elements ztx k el bases)]
    {:el-key k, :element el, :base-elements b-els}
    (let [fix-poly-k (keyword (decapitalize-first-letter (name k)))]
      (when-let [b-els (get-base-elements ztx fix-poly-k el bases)]
        {:el-key fix-poly-k, :element el, :base-elements b-els}))))


(defn find-poly-base-el [ztx k el bases]
  (when-let [{poly-key :key, poly-type :type} (get-base-poly-key ztx k bases)]
    (let [poly-el  {:| {(keyword poly-type) (assoc el :type poly-type)}}
          base-els (get-base-elements ztx poly-key poly-el bases)]
      {:el-key        poly-key
       :element       poly-el
       :base-elements base-els})))


(defn find-base-els [ztx k el bases]
  (let [{:as   search-result
         :keys [el-key element base-elements]
         :or   {el-key k, element el, base-elements []}}
        (search-base-elements ztx k el bases)]
    (if (seq base-elements)
      search-result
      (find-poly-base-el ztx el-key element bases))))


(defn walk-with-bases [ztx ctx subj bases]
  (letfn [(walk-with-bases-recursive [acc [k el]]
            (let [{:keys [el-key element base-elements]
                   :or   {el-key k, element el, base-elements []}}
                  (find-base-els ztx k el bases)

                  new-ctx
                  (-> (update ctx :lvl inc) (update :path conj el-key))]
              (when (and (not= "specialization" (:derivation ctx))
                         (empty? base-elements))
                (println :WARN :no-base (conj (:path ctx) k) el))
              (assoc acc el-key (walk-with-bases ztx new-ctx element base-elements))))]
    (let [enr-subj (enrich-element ctx subj bases)]
      (cond-> enr-subj
        (seq (:| enr-subj))
        (update :| (partial reduce walk-with-bases-recursive {}))))))


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
            (when (= "constraint" (:derivation subj))
              (println (pr-str :WARN :no-base url)))
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


(defn preprocess-resources
  ;; this is pure transformation of original resources (i.e. without context)
  [ztx]
  (swap! ztx assoc :fhir/inter
         (sp/transform [sp/MAP-VALS sp/MAP-VALS]
                       process-on-load
                       (:fhir/src @ztx))))


(defn process-concepts [ztx]
  (let [codesystems (get-in @ztx [:fhir/inter "CodeSystem"])
        concepts (into {} (mapcat (comp :fhir/concepts val)) codesystems)]
    (swap! ztx assoc-in [:fhir/inter "Concept"] concepts)))


(defn process-resources
  "this is processing of resources with context"
  [ztx]
  (process-structure-definitions ztx)
  (process-concepts ztx))


(defn dir? [^java.io.File file]
  (and (.isDirectory file)
       (not (str/starts-with? (.getName file) "."))))


(defn load-all [ztx package & [{:keys [params node-modules-folder]
                                :or {node-modules-folder "node_modules"}}]]
  (doseq [pkg-dir (->> [(io/file node-modules-folder)
                        (io/file (str node-modules-folder "/node_modules"))]
                       (mapcat (fn [dir] (when (dir? dir) (cons dir (.listFiles dir)))))
                       (mapcat (fn [x] (if (and (dir? x) (str/starts-with? (.getName x) "@"))
                                         (.listFiles x)
                                         [x])))
                       (filter dir?)
                       distinct)
          :when   (.exists (io/file (str (.getPath pkg-dir) "/.index.json")))
          :let    [package (read-json (str (.getPath pkg-dir) "/package.json"))
                   index   (read-json (str (.getPath pkg-dir) "/.index.json"))
                   package-params (get params (:name package))]
          {filename :filename :as header} (:files index)]
    (load-json-file ztx package header
                    (io/file (str (.getPath pkg-dir) "/" filename))
                    {:params package-params}))
  (preprocess-resources ztx)
  (process-resources ztx)
  :done)
