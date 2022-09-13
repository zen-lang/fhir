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

(defmulti date-expr (fn [_jp type] (keyword (:type type))))

(defmethod date-expr :Period
  [jsonpaths {polymorphic? :polymorphic?}]
  (let [max-param [:pg/call :max_text_date_bound [:pg/sql "{{param}}"]]
        min-param [:pg/call :min_text_date_bound [:pg/sql "{{param}}"]]
        extract-max (fn [jp] [:pg/call :jsonpath_extract_max_timestamptz [:pg/sql "{{table}}.resource"] jp])
        extract-min (fn [jp] [:pg/call :jsonpath_extract_min_timestamptz [:pg/sql "{{table}}.resource"] jp])]
    (for [jp jsonpaths
          :let [jp (cond-> jp polymorphic? (str ".Period"))
                jp-start (str jp ".start")
                jp-end (str jp ".end")]]
      [:or
       [:and
        [:>= max-param (extract-max jp-start)]
        [:<= min-param (extract-min jp-end)]]
       [:and
        [:>= max-param (extract-max jp-start)]
        [:is (extract-max jp-end) nil]]
       [:and
        [:<= min-param (extract-min jp-end)]
        [:is (extract-min jp-start) nil]]])))

(defmethod date-expr :Timing
  [jsonpaths {polymorphic? :polymorphic?}]
  (let [max-param [:pg/call :max_text_date_bound [:pg/sql "{{param}}"]]
        min-param [:pg/call :min_text_date_bound [:pg/sql "{{param}}"]]
        extract-max (fn [jp] [:pg/call :jsonpath_extract_max_timestamptz [:pg/sql "{{table}}.resource"] jp])
        extract-min (fn [jp] [:pg/call :jsonpath_extract_min_timestamptz [:pg/sql "{{table}}.resource"] jp])]
    (for [jp jsonpaths
          :let [jp (cond-> jp polymorphic? (str ".Timing"))
                jp (str jp ".event")]]
      [:and
       [:>= max-param (extract-max jp)]
       [:<= min-param (extract-min jp)]])))

(defmethod date-expr :default
  [jsonpaths {polymorphic? :polymorphic? type :type}]
  (let [max-param [:pg/call :max_text_date_bound [:pg/sql "{{param}}"]]
        min-param [:pg/call :min_text_date_bound [:pg/sql "{{param}}"]]
        extract-max (fn [jp] [:pg/call :jsonpath_extract_max_timestamptz [:pg/sql "{{table}}.resource"] jp])
        extract-min (fn [jp] [:pg/call :jsonpath_extract_min_timestamptz [:pg/sql "{{table}}.resource"] jp])]
    (for [jp jsonpaths
          :let [jp (cond-> jp polymorphic? (str "." (name type)))]]
      [:and
       [:>= max-param (extract-max jp)]
       [:<= min-param (extract-min jp)]])))


(defmethod expand :date
  [_type types jsonpaths]
  {:where (into [:or]
                (mapcat (partial date-expr jsonpaths))
                types)})

(defmulti number-expr (fn [_jp type] (keyword (:type type))))

(defmethod number-expr :decimal [jsonpaths {polymorhic? :polymorphic? type :type}]
  (for [jp jsonpaths
        :let [jp (cond-> jp polymorhic? (str "." (name type)))]]
    [:and
     [:>= [:pg/call :jsonpath_extract_max_numeric [:pg/sql "{{table}}.resource"] jp]
      [:pg/cast [:pg/sql "{{param}}"] :numeric]]
     [:<= [:pg/call :jsonpath_extract_min_numeric [:pg/sql "{{table}}.resource"] jp]
      [:pg/cast [:pg/sql "{{param}}"] :numeric]]]))

(defmethod number-expr :default [jsonpaths {polymorhic? :polymorphic? type :type}]
  (for [jp jsonpaths
        :let [jp (cond-> jp polymorhic? (str "." (name type)))]]
    [:and
     [:>= [:pg/call :jsonpath_extract_max_numeric [:pg/sql "{{table}}.resource"] jp]
      [:pg/cast [:pg/sql "{{param}}"] :numeric]]
     [:<= [:pg/call :jsonpath_extract_min_numeric [:pg/sql "{{table}}.resource"] jp]
      [:pg/cast [:pg/sql "{{param}}"] :numeric]]]))

(defmethod number-expr :Range [jsonpaths {polymorhic? :polymorphic? type :type}]
  (for [jp jsonpaths
        :let [jp (cond-> jp polymorhic? (str "." (name type)))
              jp-low (str jp ".low.value")
              jp-high (str jp ".high.value")]]
    [:and
     [:<= [:pg/call :jsonpath_extract_max_numeric [:pg/sql "{{table}}.resource"] jp-high]
      [:pg/cast [:pg/sql "{{param}}"] :numeric]]
     [:>= [:pg/call :jsonpath_extract_min_numeric [:pg/sql "{{table}}.resource"] jp-low]
      [:pg/cast [:pg/sql "{{param}}"] :numeric]]]))


(defmethod expand :number
  [_type types jsonpaths]
  {:where (into [:or]
                (mapcat (partial number-expr jsonpaths))
                types)})


(defmethod expand :quantity
  [_type _types jsonpaths]
  {:where (into [:or]
                (for [jp jsonpaths]
                  [:and
                   [:>= [:pg/call :jsonpath_extract_max_numeric [:pg/sql "{{table}}.resource"]
                         (str jp ".\"value\"")]
                    [:pg/cast [:pg/sql "{{param}}"] :numeric]]
                   [:<= [:pg/call :jsonpath_extract_min_numeric [:pg/sql "{{table}}.resource"]
                         (str jp ".\"value\"")]
                    [:pg/cast [:pg/sql "{{param}}"] :numeric]]]))})


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
