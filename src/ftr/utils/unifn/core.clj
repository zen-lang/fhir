(ns ftr.utils.unifn.core
  (:require [clojure.stacktrace :as stacktrace]))

(defn deep-merge [a b]
  (loop [[[k v :as i] & ks] b
         acc a]
    (if (nil? i)
      acc
      (let [av (get acc k)]
        (cond
          (= v av)
          (recur ks acc)

          (and (map? v) (= :replace (::action v)))
          (recur ks (assoc acc k (dissoc v ::action)))

          (and (map? v) (map? av))
          (recur ks (assoc acc k (deep-merge av v)))

          :else (recur ks (assoc acc k v)))))))

(defmulti *fn (fn [arg] (::fn arg)))

(defmethod *fn ::identity
  [arg] {})

(defmethod *fn :default
  [arg]
  (if (::safe? arg)
    {::status :error
     ::message (str "Could not resolve " (::fn arg))}
    (throw (Exception. (str "Could not resolve " (::fn arg))))))

(declare *apply)

;; looks like tracers are not used
(defn *apply-impl [{st ::status inter ::intercept f-name ::fn tracers ::tracers :as arg}]
  (if (and (contains? #{:error :stop} st) (not (= inter :all)))
    arg
    (let [arg (dissoc arg ::intercept)
          trace-ev {::fn f-name ::phase :enter}]
      (doseq [t tracers] (*apply t {:event trace-ev :arg arg}))
      (let [patch (if (::safe? arg)
                    (try (*fn arg)
                         (catch Exception e
                           {::status :error
                            ::message (.getMessage e)
                            ::exception e
                            ::stacktrace (with-out-str (stacktrace/print-stack-trace e))}))
                    (*fn arg))
            patch (cond (map? patch) patch
                        (nil? patch) {}
                        :else {::value patch})
            ;; ??? effects?
            ;; if response contains ::fx map we apply effects to result
            ;; you can override fx for tests by passing ::u/override-fx map
            patch (->> (::fx patch)
                       (reduce (fn [acc [k {*fn ::fn :as u}]]
                                 (let [*fn (get-in arg [::override-fx *fn] *fn)]
                                   (assoc acc k (*apply (assoc u ::fn *fn) {}))))
                               (dissoc patch ::fx)))

            res (deep-merge arg patch)]
        (doseq [t tracers]
          (*apply t {:event patch :arg res}))
        res))))

(defn *apply [f arg]
  ;; validate f
  (cond
    (keyword? f) (*apply-impl (assoc arg ::fn f))
    (string? f)  (*apply-impl (assoc arg ::fn (keyword f)))
    (map? f)     (*apply-impl (deep-merge arg f))
    (sequential? f) (loop [[f & fs] f, arg arg]
                      (if (nil? f)
                        arg
                        (recur fs (*apply f arg))))
    (var? f) (*apply (var-get f) arg)
    :else (throw (Exception. (str "I don't know how to apply " (pr-str f))))))
