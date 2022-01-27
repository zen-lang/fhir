(ns zen.fhir.search-parameter.template)

(defmulti expand
  (fn [template-type _jsonpath] template-type))

(defmethod expand :string
  [_type jsonpath]
  {:where (into [:or]
                (for [jp jsonpath]
                  [:ilike
                   [:pg/cast
                    [:pg/jsonb-path-query-array
                     [:pg/sql "{{table}}.resource"]
                     [:pg/cast jp :jsonpath]]
                    :text]
                   [:pg/sql "{{param}}"]]))
   :parameter-format "%?%"})

(defmethod expand :reference
  [_type _jp]
  {:where ["@@"]})

(defmethod expand :default
  [_type _jp]
  {:where :pg/false})
