(ns zen.fhir-light.utils.merge)


(defn safe-deep-merge
  ([] nil)
  ([a] a)
  ([a b]
   (cond
     (= a b)
     a

     (and (or (nil? a) (map? a)) (or (nil? b) (map? b)))
     (merge-with safe-deep-merge a b)

     :else
     (throw (ex-info "Can't merge not maps. Overwriting values is not allowed"
                     {:a a
                      :b b}))))
  ([a b & maps]
   (reduce safe-deep-merge
           a
           (cons b maps))))
