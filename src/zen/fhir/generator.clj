(ns zen.fhir.generator)


;; * resources, types
;;  -> sd (+deps => ctx)
;;    ->  elements
;;      -> element-> zen -> transformed element
;;      -> element -> resolve to symbols
;;      -> aggregate into zen tree - temp keys


;; npm
;; -> temp storage
;;    by resourceType
;;    SD{ulr {res} } => ctx

;; res
;;   -> shaping
;;   1. treeify
;;   2. vectorize (tree algorythm)
