(ns ftr.extraction.flat-table
  (:require [clojure.java.io :as io]
            [ftr.utils.unifn.core :as u]
            [ftr.utils.core]
            [clojure.data.csv :as csv]
            [clojure.string :as str]
            [cheshire.core]))

(defmethod u/*fn ::create-value-set [cfg]
  {::result {:value-set
             (-> (:value-set cfg)
                 (assoc :resourceType "ValueSet")
                 (->> (merge {:status  "unknown"
                              :compose {:include [{:system (get-in cfg [:code-system :url])}]}})))}})

(defmethod u/*fn ::create-code-system [cfg]
  {::result {:code-system (-> (:code-system cfg)
                              (assoc :resourceType "CodeSystem")
                              (->> (merge {:status   "unknown"
                                           :content  "not-present"
                                           :valueSet (get-in cfg [:value-set :url])})))}})

(defmulti parse-flat-table-row
  (fn [params _s] (keyword (:format params))))

(def lf    (int \newline))
(def cr    (int \return))
(def eof   -1)
(def space (int \space))
(def tab   (int \tab))

(defn trim-left-read [^java.io.PushbackReader reader whitespaces]
  (->> (iteration (fn [_] (.read reader)))
       (drop-while #(contains? whitespaces %))
       first))

(defn read-cell [^java.io.PushbackReader reader ^java.lang.StringBuilder sb sep quote & {:keys [trim-left?]}]
  (let [first-ch (if trim-left?
                   (trim-left-read reader (set (remove #(= sep %) [space tab])))
                   (.read reader))]
    (if (== first-ch quote)
      [(#'csv/read-quoted-cell reader sb sep quote) true]
      (loop [ch first-ch]
        (condp == ch
          sep [:sep false]
          lf  [:eol false]
          cr  (let [next-ch (.read reader)]
               (when (not= next-ch lf)
                 (.unread reader next-ch))
               [:eol])
          eof [:eof false]
          (do (.append sb (char ch))
              (recur (.read reader))))))))

(defn read-record [reader sep quote]
  (loop [record (transient [])]
    (let [cell               (java.lang.StringBuilder.)
          [sentinel quoted?] (read-cell reader cell sep quote :trim-left? true)
          str-res            (cond-> (str cell)
                               (not quoted?) str/trimr)]
      (if (= sentinel :sep)
        (recur (conj! record str-res))
        [(persistent! (conj! record str-res)) sentinel]))))

(defmethod parse-flat-table-row :csv [params reader]
  (let [delimiter (int (first (get-in params [:csv-format :delimiter])))
        quotation (int (first (get-in params [:csv-format :quote])))

        pb-reader (java.io.PushbackReader. reader)
        [row _]   (read-record pb-reader delimiter quotation)]
    row))

(defn prepare-row [row & {:keys [header]}]
  (if (some? header)
    (into {} (zipmap header row))
    (into {} (zipmap (range) row))))

(defn extract-mapped-value [mapping source]
  (update-vals mapping
               (fn [el]
                 (let [v (get source (:column el))]
                   (cond-> el
                     (not (str/blank? v))
                     (assoc :value v))))))

(defn extract-mapping [prepared-row mapping]
  (update-vals mapping #(extract-mapped-value % prepared-row)))

(defn format-concept-resource [extracted-mapping {:keys [system valueset]}]
  (let [code             (get-in extracted-mapping [:concept :code :value])
        display          (get-in extracted-mapping [:concept :display :value])
        status           (get-in extracted-mapping [:concept :deprecated? :value])
        deprecation-mark (get-in extracted-mapping [:concept :deprecated? :true-values])
        parent-id        (get-in extracted-mapping [:concept :parent-id :value])
        hierarchy-id     (get-in extracted-mapping [:concept :hierarchy-id :value])]
    (ftr.utils.core/strip-nils
      {:resourceType "Concept"
       :id           (-> (str system \- code)
                         (java.net.URLEncoder/encode "UTF-8")
                         (str/replace #"%" "-"))
       :system       system
       :valueset     [valueset]
       :code         code
       :deprecated   (if (some #(= status %) deprecation-mark) true nil)
       :display      display
       :parent-id    parent-id
       :hierarchy-id hierarchy-id
       :property     (-> (:property extracted-mapping)
                         (update-vals :value)
                         ftr.utils.core/strip-nils
                         not-empty)})))

(defn process-row [params row {:keys [mapping header system valueset]}]
  (-> (parse-flat-table-row params (java.io.StringReader. row))
      (prepare-row :header header)
      (extract-mapping mapping)
      (format-concept-resource {:system system :valueset valueset})
      cheshire.core/generate-string))

(defn process-reader-rows [reader {:as params :keys [mapping header system valueset]}]
  (let [br (java.io.BufferedReader. reader)]
    (->> br
         line-seq
         (map (fn [str-row]
                (process-row params
                             str-row
                             {:mapping mapping
                              :header header
                              :system system
                              :valueset valueset}))))))

(defn skip-buffered-reader-rows! [^java.io.BufferedReader reader rows-n]
  (dotimes [_ rows-n]
    (.readLine reader)))

(defmethod u/*fn ::import [cfg]
  (let [reader (io/reader (:source-url cfg))
        header (when (:header cfg)
                 (skip-buffered-reader-rows! reader (:header-row cfg))
                 (parse-flat-table-row cfg reader))

        rows-to-skip-until-data (if header
                                  (- (:data-row cfg)
                                     (inc (:header-row cfg)))
                                  (:data-row cfg))
        _ (skip-buffered-reader-rows! reader rows-to-skip-until-data)

        concepts-seq (process-reader-rows
                       reader (merge cfg
                                     {:header   header
                                      :system   (get-in cfg [:code-system :url])
                                      :valueset (get-in cfg [:value-set :url])
                                      :mapping  (:mapping cfg)}))]
    {::result {:concepts concepts-seq}}))

(defmethod u/*fn ::check-header&data-rows-params [cfg]
  (let [{:keys [data-row header-row]} cfg
        header-before-data? (if (some? header-row)
                              (< header-row data-row)
                              true)]
    (when-not header-before-data?
      {::u/status :error
       ::error {:message    "Header row must be before data"
                :header-row header-row
                :data-row   data-row}})))

(defn import-from-cfg [cfg]
  (::result (u/*apply [::check-header&data-rows-params
                       ::create-value-set
                       ::create-code-system
                       ::import] cfg)))


(comment
  (def config
    {:source-url "https://storage.googleapis.com/aidbox-public/documentation/icd10_example_no_header.csv"
     :format      "csv"
     :csv-format  {:delimiter ";"
                   :quote "'"}

     :header   false
     :data-row 0
     :mapping  {:concept {:code    {:column 2}
                          :display {:column 3}}}

     :code-system {:id "icd10", :url "http://hl7.org/fhir/sid/icd-10"}
     :value-set   {:id "icd10", :url "http://hl7.org/fhir/ValueSet/icd-10"}})

  (import-from-cfg config)


  )
