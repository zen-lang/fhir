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


(defmethod expand :date
  [_type _types jsonpaths]
  {:where (into [:or]
                (for [jp jsonpaths]
                  [:and
                   [:>= [:pg/call :max_text_date_bound [:pg/sql "{{param}}"]]
                    [:pg/call :jsonpath_extract_max_timestamptz [:pg/sql "{{table}}.resource"] jp]]
                   [:<= [:pg/call :min_text_date_bound [:pg/sql "{{param}}"]]
                    [:pg/call :jsonpath_extract_min_timestamptz [:pg/sql "{{table}}.resource"] jp]]]))})


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
