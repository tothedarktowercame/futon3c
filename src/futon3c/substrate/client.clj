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

(defn- response-body
  [response]
  (try (edn/read-string (:body response))
       (catch Throwable _ (:body response))))

(defn- request-headers
  [trace-id]
  (cond-> {"Accept" "application/edn"}
    trace-id (assoc "X-Trace-Id" trace-id)))

(defn- get-edn!
  ([url timeout-ms] (get-edn! url timeout-ms nil))
  ([url timeout-ms trace-id]
   (let [response (http/get url {:headers (request-headers trace-id)
                                 :timeout timeout-ms
                                 :throw false})
         body (response-body response)]
     (if (= 200 (:status response))
       body
       (throw (ex-info "authoritative substrate read failed"
                       {:url url :status (:status response) :body body}))))))

(defn- post-edn!
  [url payload timeout-ms trace-id]
  (let [response (http/post url
                            {:headers (assoc (request-headers trace-id)
                                             "Content-Type" "application/edn")
                             :body (pr-str payload)
                             :timeout timeout-ms
                             :throw false})
        body (response-body response)]
    (if (= 200 (:status response))
      body
      (throw (ex-info "authoritative substrate read failed"
                      {:url url :status (:status response) :body body})))))

(defn hyperedges-by-type
  ([type] (hyperedges-by-type type {}))
  ([type {:keys [limit timeout-ms valid-as-of system-as-of]
          :or {limit 10000 timeout-ms 60000}}]
   (:hyperedges
    (get-edn! (str (configured-url) "/api/alpha/hyperedges?type=" (encode type)
                   "&limit=" (long limit)
                   (when valid-as-of
                     (str "&valid-as-of=" (encode valid-as-of)))
                   (when system-as-of
                     (str "&system-as-of=" (encode system-as-of))))
              timeout-ms))))

(defn hyperedges-by-end
  ([end] (hyperedges-by-end end {}))
  ([end {:keys [type limit timeout-ms valid-as-of system-as-of]
         :or {limit 10000 timeout-ms 60000}}]
   (let [url (str (configured-url) "/api/alpha/hyperedges?end=" (encode end)
                  (when type (str "&type=" (encode type)))
                  "&limit=" (long limit)
                  (when valid-as-of
                    (str "&valid-as-of=" (encode valid-as-of)))
                  (when system-as-of
                    (str "&system-as-of=" (encode system-as-of))))]
     (:hyperedges (get-edn! url timeout-ms)))))

(defn hyperedge-by-id
  ([id] (hyperedge-by-id id {}))
  ([id {:keys [timeout-ms] :or {timeout-ms 60000}}]
   (get-edn! (str (configured-url) "/api/alpha/hyperedge/" (encode id))
             timeout-ms)))

(defn evidence-text-search
  ([query] (evidence-text-search query {}))
  ([query {:keys [limit timeout-ms trace-id]
           :or {limit 10 timeout-ms 60000}}]
   (get-edn! (str (configured-url) "/api/alpha/evidence/text-search?q="
                  (encode query) "&limit=" (long limit))
             timeout-ms trace-id)))

(defn memory-projection
  "Fetch compact edge/evidence components for several memory endpoints.

  Current reads use the server's coherent revisioned projection; explicit
  bitemporal reads use its bounded database path. Callers retain responsibility
  for validating the shared memory contract."
  ([endpoints] (memory-projection endpoints {}))
  ([endpoints {:keys [limit timeout-ms trace-id valid-as-of system-as-of]
               :or {limit 10 timeout-ms 60000}}]
   (post-edn!
    (str (configured-url) "/api/alpha/memory/projection")
    (cond-> {:endpoints (vec endpoints)
             :limit (long limit)}
      valid-as-of (assoc :valid-as-of valid-as-of)
      system-as-of (assoc :system-as-of system-as-of))
    timeout-ms trace-id)))
