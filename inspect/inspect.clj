(ns inspect
  (:require [clojure.pprint]
            [hiccup.core :as hiccup]))

(defn inspect [file data]
  (spit file (with-out-str (clojure.pprint/pprint data))))

(defmulti render-data (fn [x & [opts]] (type x)))

(defmethod render-data :default
  [x & [opts]]
  [:span (with-out-str (clojure.pprint/pprint x))])

(defn primitive? [x]
  (or (number? x) (string? x) (keyword? x) (boolean? x) (set? x)))

(defn render-map [x & [opts]]
  (into [:div.block]
        (for [[k v] (sort-by first x)]
          (if (primitive? v)
            [:div [:b.key (str k)] (render-data v)]
            [:details (cond-> {:style "display: flex;"}
                        (and (or (= k :|) (:| v))
                             (not (:closed opts))) (assoc :open "open"))
             [:summary [:b.key (str k)]]
             (render-data v)]))))

(defmethod render-data
  clojure.lang.PersistentArrayMap
  [x & [opts]] (render-map x opts))

(defmethod render-data
  clojure.lang.PersistentHashMap
  [x & [opts]] (render-map x opts))

(defmethod render-data
  clojure.lang.PersistentVector
  [x & [opts]]
  (into [:div.block (pr-str x)]))




;; (defn inspect [file data]
;;   (spit file (with-out-str (clojure.pprint/pprint data))))

(def css
  "
body {font-family: Geneva, Arial, Helvetica, sans-serif; background-color: #282a36; color: #bfcd70;}
.block {padding-left: 1rem;}
.key {color: #fe7ac6; padding-right: 0.5rem; cursor: pointer;}
.key:hover {color: white;}"
  )

(defn inspect [file data & [opts]]
  (spit file (hiccup/html
               [:html [:head [:style css]]
                [:body (render-data data opts)]])))
