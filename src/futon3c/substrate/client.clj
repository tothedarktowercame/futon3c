(ns futon3c.substrate.client
  "Semantic HTTP client for the authoritative substrate graph.

  FUTON_SUBSTRATE_URL is canonical. FUTON1A_URL remains a compatibility input
  while the historical variable name is retired."
  (:require [babashka.http-client :as http]
            [clojure.edn :as edn]
            [clojure.string :as str])
  (:import [java.net URLEncoder]))

(defn configured-url []
  (-> (or (System/getenv "FUTON_SUBSTRATE_URL")
          (System/getenv "FUTON1A_URL")
          "http://127.0.0.1:7071")
      (str/replace #"/+$" "")
      (str/replace #"/api/alpha$" "")))

(defn- encode [x]
  (URLEncoder/encode (if (keyword? x) (subs (str x) 1) (str x)) "UTF-8"))

(defn- get-edn! [url timeout-ms]
  (let [response (http/get url {:headers {"Accept" "application/edn"}
                                :timeout timeout-ms
                                :throw false})
        body (try (edn/read-string (:body response))
                  (catch Throwable _ (:body response)))]
    (if (= 200 (:status response))
      body
      (throw (ex-info "authoritative substrate read failed"
                      {:url url :status (:status response) :body body})))))

(defn hyperedges-by-type
  ([type] (hyperedges-by-type type {}))
  ([type {:keys [limit timeout-ms] :or {limit 10000 timeout-ms 60000}}]
   (:hyperedges
    (get-edn! (str (configured-url) "/api/alpha/hyperedges?type=" (encode type)
                   "&limit=" (long limit))
              timeout-ms))))

(defn hyperedges-by-end
  ([end] (hyperedges-by-end end {}))
  ([end {:keys [limit timeout-ms] :or {limit 10000 timeout-ms 60000}}]
   (:hyperedges
    (get-edn! (str (configured-url) "/api/alpha/hyperedges?end=" (encode end)
                   "&limit=" (long limit))
              timeout-ms))))
