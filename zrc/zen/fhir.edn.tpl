{ns zen.fhir

 version
 {:zen/tags #{zen/schema zen/tag version}
  :type zen/map
  :zen.fhir/version "${ZEN_FHIR_VERSION}"
  :require #{:zen.fhir/version}
  :keys {:zen.fhir/version {:type zen/string
                            :const {:value "${ZEN_FHIR_VERSION}"}}}}

 Resource
 {:zen/tags #{zen/tag zen/schema}
  :type zen/map
  :keys {:resourceType {:type zen/string}
         :id {:type zen/string}
         :meta {:type zen/map :values {:type zen/any}}}}

 Reference
 {:zen/tags #{zen/schema}
  :zen/desc "reference datatype"
  :type zen/map
  :keys {:id {:type zen/string}
         :resourceType {:type zen/string}
         :display {:type zen/string}
         :localRef {:type zen/string}
         :uri {:type zen/string}}}

 value-set
 {:zen/tags #{zen/schema zen/tag}
  :zen/desc "Value set"
  :confirms #{version}
  :type zen/map
  :keys {:uri {:type zen/string}
         :fhir/code-systems {:type zen/set
                             :every {:type zen/map
                                     :require #{:fhir/url}
                                     :keys {:fhir/url {:type zen/string}
                                            :zen.fhir/content {:type zen/keyword
                                                               :enum [{:value :bundled}
                                                                      {:value :not-present}]}}}}
         :version {:type zen/string}}}

 nested-schema
 {:zen/tags #{zen/schema}
  :type zen/map
  :keys {:fhir/flags {:type zen/set}
         :fhir/extensionUri {:type zen/string}
         :fhir/polymorphic {:type zen/boolean}
         :zen.fhir/reference {:type zen/map
                              :keys {:refers {:type zen/set
                                              :every {:type zen/symbol
                                                      #_#_:tags #{#{zen.fhir/base-schema zen.fhir/profile-schema}}}}}} ;; TODO
         :zen.fhir/value-set {:type zen/map
                              :keys {:symbol {:type zen/symbol}
                                     :strength {:type zen/keyword
                                                :enum [{:value :required}
                                                       {:value :extensible}
                                                       {:value :preferred}
                                                       {:value :example}]}}}
         :keys {:type zen/map
                :values {:confirms #{nested-schema}}}
         :every {:confirms #{nested-schema}}}}

 structure-schema
 {:zen/tags #{zen/schema zen/tag}
  :confirms #{nested-schema version}
  :type     zen/map
  :keys     {:zen.fhir/type {:type zen/string}
             :zen.fhir/profileUri {:type zen/string}}}

 base-schema
 {:zen/tags #{zen/schema zen/tag}
  :zen/desc "This schema should be used to validate all resources of its type"
  :confirms #{structure-schema}
  :type     zen/map
  :require  #{:zen.fhir/type}}

 profile-schema
 {:zen/tags #{zen/schema zen/tag}
  :zen/desc "This schema should be used only when mentioned in meta.profile"
  :confirms #{structure-schema}
  :type     zen/map
  :require  #{:zen.fhir/profileUri}}}
