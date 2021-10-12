(ns zen.fhir.icd10
  (:require [org.httpkit.client :as http]
            [clojure.java.io :as io]
            [cheshire.core :as json]
            [clojure.string :as str]))


(def token-endpoint "https://icdaccessmanagement.who.int/connect/token")
(def client-id "")
(def client-secret "")
(def scope "icdapi_access")
(def grant-type "client_credentials")
(def base-uri "https://id.who.int/")


(defn get-auth-token []
  (-> (http/post token-endpoint
                 {:form-params {"client_id" client-id
                                "client_secret" client-secret
                                "scope" scope
                                "grant_type" grant-type}})
      deref
      :body
      (json/parse-string keyword)
      :access_token))


(def request-headers
  {:headers
   {"Authorization" (str "Bearer "(get-auth-token))
    "Accept" "application/json"
    "Accept-language" "en"
    "API-Version" "v2"}})


(defn get-icd-10-latest-release-uri []
  (-> (http/get (str base-uri "icd/release/10")
                request-headers)
      deref
      :body
      (json/parse-string keyword)
      :latestRelease))


(defn get-all-codes [codes uri]
  (let [res (-> (http/get uri request-headers)
                deref
                :body
                (json/parse-string keyword))]
    (cond
      (and (contains? res :parent)
           (or (= (:classKind res) "category")
               (= (:classKind res) "modifiedcategory"))
           (not (contains? res :child)))
      (conj codes {:definition (get-in res [:title (keyword "@value")])
                   :code  (:code res)})

      (and (contains? res :parent)
           (contains? res :child))
      (let [children (:child res)
            codes'   (cond-> (->> children
                                  (map (partial get-all-codes []))
                                  flatten)
                       (= (:classKind res) "chapter")
                       (conj {:definition (get-in res [:title (keyword "@value")])
                              :code  (:code res)})

                       (= (:classKind res) "block")
                       (conj {:definition (get-in res [:title (keyword "@value")])
                              :code  (:code res)})

                       (= (:classKind res) "category")
                       (conj {:definition (get-in res [:title (keyword "@value")])
                              :code  (:code res)}))]
        (into codes codes'))

      (not (contains? res :parent))
      (let [children (:child res)
            codes'   (->> children
                          (pmap (partial get-all-codes []))
                          flatten)]
        (into codes codes'))

      :else codes)))


(defn preprocess-concept [vs concept]
  (assoc concept
         :_source "zen.fhir"
         :valueset [(:url vs)]
         :system (:url vs)
         :id (str/replace (str (:url vs) "/" (:code concept)) "/" "-")
         :resourceType "Concept"))


(defn preprocess-concepts [vs concepts]
  (pmap (partial preprocess-concept vs) concepts))


(def code-system
  {:resourceType "CodeSystem"
   :id "icd-10"
   :url "ICD-10"
   :date "2019"
   :description "International Classification of Diseases"
   :content "complete"
   :name "ICD-10-CM"
   :status "active"
   :version "2019"
   :_source "zen.fhir"})


(def value-set
  {:resourceType "ValueSet",
   :id "icd-10",
   :description "This value set includes all ICD-10 codes.",
   :version "0.0.1"
   :compose {:include [{:system "ICD-10"}]}
   :date "2019-01-01",
   :name "ICD-10",
   :url "icd-10"
   :status "active"
   :_source "zen.fhir"})


(defn write-line [w x]
  (.write w (cheshire.core/generate-string x))
  (.write w "\n"))


(defn write-ndjson-gz-bundle [target]
  (let [all-icd-codes (get-all-codes [] (get-icd-10-latest-release-uri))
        concepts  (preprocess-concepts value-set all-icd-codes)]
    (with-open [w (-> target
                      clojure.java.io/file
                      clojure.java.io/output-stream
                      (java.util.zip.GZIPOutputStream. true)
                      (java.io.OutputStreamWriter.)
                      (java.io.BufferedWriter.))]
      (write-line w code-system)
      (write-line w value-set)
      (doseq [c concepts]
        (write-line w c)))))

(comment

  (time (write-ndjson-gz-bundle "/tmp/icd-10-terminology-bundle.ndjson.gz"))

  )
