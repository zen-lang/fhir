(ns zen.fhir.test-utils
  (:require  [clojure.test :as t]
             [clojure.java.io :as io]
             [org.httpkit.server]
             [ring.middleware.params]
             [ring.middleware.keyword-params]))

;;;;; mock server utils ;;;;;

(defn mock-cloud-storage-handler [{:as _req, :keys [uri]}]
  {:status 200
   :body (io/input-stream (-> (subs uri 1) ;; Remove slash at the beginning
                              (io/resource)
                              (.getPath)))})


(def mock-server-opts {:port 7654})


(defn start-mock-server [& [opts]]
  (org.httpkit.server/run-server (-> #'mock-cloud-storage-handler
                                     ring.middleware.keyword-params/wrap-keyword-params
                                     ring.middleware.params/wrap-params)
                                 (merge mock-server-opts opts)))


(defn mock-server-url [& [port]]
  (format "http://localhost:%s"
          (or port (:port mock-server-opts))))
