(ns zen.fhir.icd10
  (:require [org.httpkit.client :as http]
            [clojure.java.io :as io]
            [cheshire.core :as json]
            [clojure.string :as str]))


(def token-endpoint "https://icdaccessmanagement.who.int/connect/token")
(def client-id (System/getenv "ICD_10_CLIENT_ID"))
(def client-secret (System/getenv "ICD_10_CLIENT_SECRET"))
(def scope "icdapi_access")
(def grant-type "client_credentials")
(def base-uri "https://id.who.int/")


(defn get-auth-token []
  (-> (http/post token-endpoint
                 {:form-params {"client_id"     client-id
                                "client_secret" client-secret
                                "scope"         scope
                                "grant_type"    grant-type}})
      deref
      :body
      (json/parse-string keyword)
      :access_token))


(def request-headers
  {:headers
   {"Authorization"   (str "Bearer "(get-auth-token))
    "Accept"          "application/json"
    "Accept-language" "en"
    "API-Version"     "v2"}})


(defn get-icd-10-latest-release-uri []
  (-> (http/get (str base-uri "icd/release/10")
                request-headers)
      deref
      :body
      (json/parse-string keyword)
      :latestRelease))


(defn get-all-codes [codes path uri]
  (let [res (-> (http/get uri request-headers)
                deref
                :body
                (json/parse-string keyword))]
    (cond
      (and (:parent res) (not (:child res))
           (#{"category" "modifiedcategory"} (:classKind res)))
      (conj codes {:definition (get-in res [:title (keyword "@value")])
                   :hierarchy  path
                   :code       (:code res)})

      (and (:parent res) (:child res))
      (let [codes'  (->> (:child res)
                         (map #(get-all-codes [] (conj path (:code res)) %))
                         flatten)
            codes'' (cond-> codes'
                      (#{"chapter" "block" "category"} (:classKind res))
                      (conj {:definition (get-in res [:title (keyword "@value")])
                             :hierarchy  path
                             :code       (:code res)}))]
        (into codes codes''))

      (not (:parent res))
      (let [codes' (->> (:child res)
                        (pmap #(get-all-codes [] [] %))
                        flatten)]
        (into codes codes'))
      :else codes)))


(defn preprocess-concept [vs cs concept]
  (-> concept
      (assoc
       :_source "zen.fhir"
       :valueset [(:url vs)]
       :system (:url cs)
       :id (str/replace (str (:url cs) "/" (:code concept)) "/" "-")
       :resourceType "Concept")))

(defn preprocess-concepts [vs cs concepts]
  (pmap (partial preprocess-concept vs cs) concepts))

(def code-system
  {:resourceType "CodeSystem" :id           "icd-10" :url          "http://hl7.org/fhir/sid/icd-10"
   :date         "2019"
   :description  "International Classification of Diseases"
   :content      "complete"
   :name         "ICD-10-CM"
   :status       "active"
   :version      "2019"
   :valueSet     "http://hl7.org/fhir/ValueSet/icd-10"
   :_source      "zen.fhir"})


(def value-set
  {:resourceType "ValueSet",
   :id           "icd-10",
   :description  "This value set includes all ICD-10 codes.",
   :version      "0.0.1"
   :compose      {:include [{:system "http://hl7.org/fhir/sid/icd-10"}]}
   :date         "2019-01-01",
   :name         "ICD-10Codes",
   :url          "http://hl7.org/fhir/ValueSet/icd-10"
   :status       "active"
   :_source      "zen.fhir"})


(defn write-line [w x]
  (.write w (cheshire.core/generate-string x))
  (.write w "\n"))


(defn write-ndjson-gz-zip-bundle [target]
  (let [all-icd-codes (get-all-codes [] [] (get-icd-10-latest-release-uri) )
        concepts      (preprocess-concepts value-set code-system all-icd-codes)]
    (with-open [zip (-> target
                        clojure.java.io/file
                        clojure.java.io/output-stream
                        (java.util.zip.ZipOutputStream.))]
      (let [entry (-> "icd-10-terminology-bundle.ndjson.gz"
                      (java.util.zip.ZipEntry.))]
        (.putNextEntry zip entry)
        (with-open [gzip (-> zip
                             (java.util.zip.GZIPOutputStream. true)
                             (java.io.OutputStreamWriter.)
                             (java.io.BufferedWriter.))]

          (write-line gzip code-system)
          (write-line gzip value-set)
          (doseq [c concepts]
            (write-line gzip c)))))))

(comment

  (-> (http/get "http://id.who.int/icd/release/10/2019/A00.9" request-headers)
      deref
      :body
      (json/parse-string keyword))

  )
