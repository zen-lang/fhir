(ns zen.fhir-light.loader.nest)


(defn- parsed-id->nested-path [parsed-id]
  (mapcat
    (fn [id-el]
      (case (:type id-el)
        :root      []
        :key       [:els (:key id-el)]
        :poly-root [:poly-roots (:poly-root id-el)]
        :poly-key  [:poly-keys (:poly-key id-el)]
        :slice     [:slicing :slices (:slice id-el)]))
    parsed-id))


(defn- parsed-id->nested-path [parsed-id]
  (vec (mapcat (fn [id-el]
                 (case (:type id-el)
                   :root      []
                   :key       [:zf/els (:key id-el)]
                   :poly-root [:zf/poly-roots (:poly-root id-el)]
                   :poly-key  [:zf/poly-keys (:poly-key id-el)]
                   :slice     [:zf/slicing :zf/slices (:slice id-el)]))
               parsed-id)))


(defn- mk-parent-id [parsed-id]
  (->> parsed-id
       reverse
       rest
       (drop-while #(not= :key (:type %)))
       reverse))


(defn- el-part-path [parsed-id elements-keys-type]
  (case elements-keys-type
    :zf/value     (conj (parsed-id->nested-path parsed-id) :zf/value)
    :zf/container (conj (parsed-id->nested-path parsed-id) :zf/container)
    :zf/outer     (let [outer-id   (mk-parent-id parsed-id)
                        outer-path (parsed-id->nested-path outer-id)]
                    (conj outer-path :zf/els-constraints (last parsed-id)))
    :zf/context   [:zf/context parsed-id]
    (conj (parsed-id->nested-path parsed-id) elements-keys-type)))


(defn- strip-el [el & {:keys [keys-to-select keys-to-strip]}]
  (not-empty
    (cond-> el
      (seq keys-to-strip)
      (as-> $ (apply dissoc $ keys-to-strip))

      (seq keys-to-select)
      (select-keys keys-to-select))))


(defn nest-by-enriched-loc [enriched-elements & {:as params}]
  (:result
   (transduce
     (mapcat (fn [el]
               (-> (strip-el el params)
                   (update-keys #(el-part-path (get-in el [:zf/loc :zf/id]) %)))))
     (completing (fn [acc [path el-part]]
                   (assoc-in acc (cons :result path) el-part)))
     {:result {}}
     enriched-elements)))
