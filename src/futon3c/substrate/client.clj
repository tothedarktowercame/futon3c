(ns futon3c.substrate.client
  "Semantic HTTP client for the authoritative substrate graph.

  FUTON_SUBSTRATE_URL is canonical. FUTON1A_URL remains a compatibility input
  while the historical variable name is retired."
  (:require [babashka.http-client :as http]
            [clojure.edn :as edn]
            [clojure.string :as str])
  (:import [java.net URLEncoder]))

(defn configured-url []
  ;; Fallback 7073, the standalone futon1b-server (two-JVM standard,
  ;; 2026-08-10). The old 7071 fallback was the retired futon1a port: any
  ;; env-less CLI invocation (fresh login shell, ssh command, cron) resolved
  ;; to a dead port and reported store-unavailable.
  (-> (or (System/getenv "FUTON_SUBSTRATE_URL")
          (System/getenv "FUTON1A_URL")
          "http://127.0.0.1:7073")
      (str/replace #"/+$" "")
      (str/replace #"/api/alpha$" "")))

(defn- encode [x]
  (URLEncoder/encode (if (keyword? x) (subs (str x) 1) (str x)) "UTF-8"))

(def hyperedge-page-limit
  "Maximum accepted by the authoritative hyperedges endpoint."
  1000)

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

;; Keep one page comfortably inside short live-view deadlines. The substrate
;; permits 1,000, but several 1,000-row hydrations measured at 4-5 seconds,
;; leaving no room inside cascade-real's 5-second per-request timeout.
;;
;; Re-derived 2026-08-23 by the same criterion, after the mission-scope/pattern
;; layer was repaired from 115 edges to 971. Those edges are ~2.5 KB each, so
;; per-page cost is dominated by payload rather than row count, and 250 stopped
;; fitting: measured against the live store with one writer active,
;;
;;     limit=50  2.7s   limit=100 2.6s   limit=150 4.2s   limit=250 4.9-7.0s
;;
;; i.e. ~2.5s fixed overhead plus a per-row term. At 250 the first page alone
;; blew the 5s budget and threw, which 500s the WHOLE cascade graph -- fixing
;; the cascade is what made it unfetchable.
;;
;; 100 restores the margin this constant exists to provide. Note the budget was
;; deliberately NOT raised instead: the 5-second deadline is the only signal
;; that distinguishes a big page from a slow substrate, and inflating it to fit
;; would spend that signal to buy nothing.
(def ^:private substrate-page-size 100)
(def default-request-budget 50)
(def ^:private admission-retries 3)

(defn partial-result?
  "True when a bounded substrate walk stopped before the server was exhausted."
  [rows]
  (true? (:partial? (meta rows))))

(defn- get-page!
  [url timeout-ms remaining-budget]
  (loop [attempt 0]
    (let [result (try
                   {:body (get-edn! url timeout-ms)}
                   (catch clojure.lang.ExceptionInfo e {:error e}))
          error (:error result)
          {:keys [status body]} (some-> error ex-data)]
      (if (and error
               (= 503 status)
               (= :expensive-read-busy (:error body))
               (< attempt admission-retries)
               (< (inc attempt) remaining-budget))
        (do (Thread/sleep (* 100 (bit-shift-left 1 attempt)))
            (recur (inc attempt)))
        (if error
          (throw error)
          (assoc result :requests (inc attempt)))))))

(defn- paged-hyperedges
  [url-fn {:keys [limit timeout-ms request-budget]
           :or {limit 10000 timeout-ms 60000
                request-budget default-request-budget}}]
  (let [target (long limit)
        budget (long request-budget)]
    (when-not (and (pos? target) (pos? budget))
      (throw (ex-info "substrate pagination requires positive limit and request budget"
                      {:limit target :request-budget budget})))
    (loop [after nil
           requests 0
           rows []]
      (let [remaining (- target (count rows))]
        (if (or (not (pos? remaining)) (>= requests budget))
          (with-meta (vec rows)
            {:partial? (boolean after)
             :next-cursor after
             :requests requests
             :request-budget budget})
          (let [page-limit (min substrate-page-size remaining)
                page-result (get-page! (url-fn page-limit after) timeout-ms
                                       (- budget requests))
                body (:body page-result)
                rows' (into rows (:hyperedges body))
                next-cursor (:next-cursor body)
                requests' (+ requests (:requests page-result))]
            (cond
              next-cursor
              (recur next-cursor requests' rows')

              ;; The endpoint form currently cannot emit a cursor. A full page
              ;; is therefore not evidence of exhaustion and must stay marked.
              (= page-limit (count (:hyperedges body)))
              (with-meta (vec rows')
                {:partial? true
                 :reason :server-page-full-without-cursor
                 :requests requests'
                 :request-budget budget})

              :else
              (vec rows'))))))))

(defn hyperedges-by-type
  ([type] (hyperedges-by-type type {}))
  ([type {:keys [valid-as-of system-as-of] :as options}]
   (paged-hyperedges
    (fn [page-limit after]
      (str (configured-url) "/api/alpha/hyperedges?type=" (encode type)
           "&limit=" page-limit
           "&include-total=false"
           (when after (str "&after=" (encode after)))
           (when valid-as-of
             (str "&valid-as-of=" (encode valid-as-of)))
           (when system-as-of
             (str "&system-as-of=" (encode system-as-of)))))
    options)))

(defn hyperedges-by-end
  ([end] (hyperedges-by-end end {}))
  ([end {:keys [type valid-as-of system-as-of] :as options}]
   (paged-hyperedges
    (fn [page-limit after]
      (str (configured-url) "/api/alpha/hyperedges?end=" (encode end)
           (when type (str "&type=" (encode type)))
           "&limit=" page-limit
           "&include-total=false"
           (when after (str "&after=" (encode after)))
           (when valid-as-of
             (str "&valid-as-of=" (encode valid-as-of)))
           (when system-as-of
             (str "&system-as-of=" (encode system-as-of)))))
    options)))

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
