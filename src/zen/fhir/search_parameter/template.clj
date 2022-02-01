(ns zen.fhir.search-parameter.template)


(defn expand-dispatch [template-type _types _jsonpath]
  template-type)


(defmulti expand #'expand-dispatch)


(defmethod expand :string
  [_type _types jsonpaths]
  {:where (into [:or]
                (for [jp jsonpaths]
                  [:ilike
                   [:pg/cast
                    [:pg/jsonb-path-query-array
                     [:pg/sql "{{table}}.resource"]
                     [:pg/cast (str "(" jp ")" ".** ? (@.type() == \"string\")") :jsonpath]]
                    :text]
                   [:pg/sql "{{param}}"]]))
   :parameter-format "%\"?%"})


(defmethod expand :reference
  [_type _types _jp]
  nil #_{:where ["@@"]})


(defmethod expand :token
  [_type _types jsonpaths]
  #_{:token
   {:only-code   (into [:or]
                       (for [jp jsonpaths]
                         ["@?::jp"
                          [:pg/sql "{{table}}.resource"]
                          [:pg/sql (format "%s ? (@.code)" jp)]]))
    :no-system   "<SQL query for parameter=|{{param.code}}>",
    :only-system "<SQL query for parameter={{param.system}}|>",
    :both        "<SQL query for parameter={{param.system}}|{{param.code}}>",
    :text        "<SQL query for parameter:text={{param.text}}>",
    :text-format "<format string {{param.text}}>"}})


(defmethod expand :default
  [_type _types _jp]
  nil #_{:where :pg/false})
