(ns zen.fhir.generator-test
  (:require
   [zen.fhir.generator :as sut]
   [zen.fhir.core]
   [zen.core]
   [matcho.core :as matcho]
   [clojure.test :as t]))


(t/deftest generate-project-integration

  (def ztx  (zen.core/new-context {}))

  (zen.fhir.core/load-all ztx "hl7.fhir.us.core"
                          {:params {"hl7.fhir.r4.core" {:zen.fhir/package-ns 'fhir.r4}
                                    "hl7.fhir.us.core" {:zen.fhir/package-ns 'us-core.v3}}})

  (get-in @ztx [:fhir/inter "StructureDefinition" "http://hl7.org/fhir/StructureDefinition/patient-nationality"])

  (sut/generate-zen-schemas ztx)

  (matcho/match
    (:fhir.zen/ns @ztx)
    {'fhir.r4.Element
     {'ns     'fhir.r4.Element
      'schema {}}

     'fhir.r4.Resource
     {'ns     'fhir.r4.Resource
      'schema {}}

     'fhir.r4.DomainResource
     {'ns     'fhir.r4.DomainResource
      'import #(contains? % 'fhir.r4.Resource)
      'schema {:confirms #(contains? % 'fhir.r4.Resource/schema)}}

     'fhir.r4.Patient
     {'ns     'fhir.r4.Patient
      'import #(contains? % 'fhir.r4.DomainResource)
      'schema {:confirms #(contains? % 'fhir.r4.DomainResource/schema)}}

     'us-core.v3.us-core-patient
     {'ns     'us-core.v3.us-core-patient
      'import #(contains? % 'fhir.r4.Patient)
      'schema {:confirms #(contains? % 'fhir.r4.Patient/schema)}}}))
