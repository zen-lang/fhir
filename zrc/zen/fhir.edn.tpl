{ns zen.fhir

 version
 {:zen/tags #{zen/schema zen/tag zen.fhir/version}
  :type zen/map
  :validation-type :open
  :zen.fhir/version "${ZEN_FHIR_VERSION}"
  :require #{:zen.fhir/version}
  :keys {:zen.fhir/version {:type zen/string
                            :const {:value "${ZEN_FHIR_VERSION}"}}}}}
