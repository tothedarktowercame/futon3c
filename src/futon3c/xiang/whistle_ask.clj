(ns futon3c.xiang.whistle-ask
  "One typed, bounded HTTP boundary for animal model whistles."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.net SocketTimeoutException]))

(def failure-kinds
  #{"model/quota" "model/timeout" "model/http" "model/parse"
    "model/budget" "model/empty-response" "model/truncated-reply"
    "model/error" "model/no-reader" "model/unparsed-reply"})

(def ^:dynamic *whistle-url* "http://127.0.0.1:7070/api/alpha/whistle")
(def ^:dynamic *whistle-read-slack-ms* 15000)

(defn model-failure
  ([kind]
   {:failure_kind (if (contains? failure-kinds kind) kind "model/error")})
  ([kind http-status error]
   {:failure_kind (if (contains? failure-kinds kind) kind "model/error")
    :http_status http-status :error error})
  ([kind http-status error details]
   (merge (model-failure kind http-status error) details)))

(defn ask
  "Ask one Agency whistle. Returns its response string or a typed failure map."
  [{:keys [seat prompt timeout-ms caller]}]
  (try
    (let [conn (doto ^java.net.HttpURLConnection
                (.openConnection (java.net.URL. *whistle-url*))
                 (.setRequestMethod "POST")
                 (.setDoOutput true)
                 (.setConnectTimeout 5000)
                 (.setReadTimeout (int (+ (or timeout-ms 60000) *whistle-read-slack-ms*)))
                 (.setRequestProperty "Content-Type" "application/json"))]
      (with-open [w (io/writer (.getOutputStream conn))]
        (.write w (json/generate-string {:agent-id seat :prompt prompt
                                         :timeout-ms (or timeout-ms 60000)
                                         :caller caller})))
      (let [status (.getResponseCode conn)
            stream (if (<= 200 status 299) (.getInputStream conn) (.getErrorStream conn))
            raw (if stream (slurp stream) "")
            body (try (json/parse-string raw true) (catch Throwable _ ::invalid-json))]
        (cond
          (= status 429)
          (model-failure "model/quota" status
                         (when-not (= ::invalid-json body)
                           (some-> (or (:error body) (:message body)) str)))

          (and (= ::invalid-json body) (not (<= 200 status 299)))
          (model-failure "model/http" status "non-JSON HTTP failure")

          (= ::invalid-json body)
          (model-failure "model/parse" status "invalid JSON response")

          (and (<= 200 status 299) (:ok body) (some? (:response body)))
          (str (:response body))

          :else
          (let [typed (some-> (or (:failure-kind body) (:failure_kind body)) str
                              (str/replace-first #"^:" ""))
                error (some-> (or (:error body) (:message body)) str)
                error-data (or (:error-data body) (:error_data body))
                kind (cond
                       (contains? #{"model/budget" "model/empty-response"} typed) typed
                       (= typed "provider/quota") "model/quota"
                       (contains? #{"timeout" "provider/timeout"} typed) "model/timeout"
                       (= "timeout" error) "model/timeout"
                       ;; Whistle currently exposes provider quota only as prose.
                       (re-find #"(?i)quota|rate[ ._-]?limit|HTTP 429" (or error "")) "model/quota"
                       :else "model/http")]
            (model-failure kind status error
                           (when (map? error-data)
                             {:diagnostics (select-keys error-data
                                                        [:http_status :stop_reason :content_block_types
                                                         :usage :bound-usd :max-usd])}))))))
    (catch SocketTimeoutException e
      (model-failure "model/timeout" nil (.getMessage e)))
    (catch Throwable e
      (model-failure "model/error" nil (.getMessage e)))))
