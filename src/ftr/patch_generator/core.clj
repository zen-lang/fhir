(ns ftr.patch-generator.core
  (:require [ftr.utils.unifn.core :as u]
            [ftr.utils.core]))


(defn make-code&system [concept]
  (format "%s-%s" (:system concept) (:code concept)))


(defn generate-patch! [old-tf new-tf]
  (let [o (ftr.utils.core/open-ndjson-gz-reader old-tf)
        n (ftr.utils.core/open-ndjson-gz-reader new-tf)
        rend (last (ftr.utils.core/parse-ndjson-gz new-tf))]

    ;;Skip header lines
    (doto o (.readLine) (.readLine))
    (doto n (.readLine) (.readLine))

    (loop [c1 (.readLine o)
           c2 (.readLine n)
           diff-acc []]
      (cond
        (and (nil? c1) (nil? c2))
        diff-acc

        (nil? c1)
        (recur c1 (.readLine n) (conj diff-acc (merge {:op "add"} c2)))


        (= (make-code&system c1)
           (make-code&system c2))
        (if (not= c1 c2)
          (recur (.readLine o) (.readLine n) (conj diff-acc (merge {:op "update"} c2)))
          (recur (.readLine o) (.readLine n) diff-acc))

        (neg?
          (compare (make-code&system c1)
                   (make-code&system c2)))
        (recur (.readLine o) c2 (conj diff-acc (merge {:op "remove"} c1)))

        (pos?
          (compare (make-code&system c1)
                   (make-code&system c2)))
        (recur c1 (.readLine n) (conj diff-acc (merge {:op "add"} c2)))

        :else
        diff-acc))))


(defmethod u/*fn ::generate-patch [{:as _ctx,
                                    {:keys [tf-patch-path]} :ftr-layout
                                    {:keys [value-set]} :write-result
                                    {:keys [generate-patch? tf-file old-tf-file]} :ingestion-coordinator}]
  (when generate-patch?
    (->>
      (generate-patch! (str old-tf-file) (str tf-file))
      (into [{:name (:name value-set)}])
      (ftr.utils.core/spit-ndjson-gz! tf-patch-path))))


(comment
  (compare "V01-X59" "XX")

  )
