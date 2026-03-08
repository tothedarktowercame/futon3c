(ns futon3c.transport.http
  "HTTP REST adapter — thin Ring handler wired to the social pipeline.

   Adapter as boundary — not business logic. The handler only translates
   between HTTP requests/responses and pipeline function calls. No routing
   logic, mode classification, or evidence emission lives here.

   Routes:
     POST /dispatch  — classify + dispatch, return receipt or error
     POST /presence  — verify presence, return record or error
     GET  /session/:id — retrieve session by ID
     GET  /api/alpha/evidence — query evidence entries
     GET  /api/alpha/evidence/count — count evidence entries by filters
     GET  /api/alpha/evidence/:id — retrieve single evidence entry
     GET  /api/alpha/evidence/:id/chain — retrieve ancestor reply chain
     GET  /api/alpha/invoke/jobs — list recent invoke jobs
     GET  /api/alpha/invoke/jobs/:id — retrieve invoke job details
     POST /api/alpha/bell — asynchronous fire-and-forget invoke (returns job-id immediately)
     POST /api/alpha/whistle — synchronous invoke (or NDJSON stream when stream=true)
     POST /api/alpha/whistle-stream — NDJSON streaming invoke with heartbeats
     POST /api/alpha/irc/send — post a line to IRC via server relay
     POST /api/alpha/invoke-delivery — record invoke delivery receipt metadata
     GET  /api/alpha/reflect/namespaces — list loaded Clojure namespaces
     GET  /api/alpha/reflect/ns/:ns — public vars in a namespace
     GET  /api/alpha/reflect/ns/:ns/full — all vars (public + private)
     GET  /api/alpha/reflect/var/:ns/:var — full var metadata (envelope)
     GET  /api/alpha/reflect/deps/:ns — namespace dependency graph
     GET  /api/alpha/reflect/java/:class — Java class reflection
     GET  /api/alpha/enrich/file?path=... — composite enrichment for a source file
     POST /api/alpha/todo — lightweight todo management (add/list/done)
     POST /api/alpha/portfolio/step — run one AIF portfolio step
     POST /api/alpha/portfolio/heartbeat — weekly heartbeat with bid/clear
     GET  /api/alpha/portfolio/state — current portfolio belief state
     GET  /health    — liveness check with agent/session counts

   Pattern references:
   - realtime/verify-after-start (L4, L7): start-server! probes port after
     start to confirm it is actually listening. Async startup can hide binding
     failures (port in use, permission denied).
   - realtime/request-param-resilience (L1, L3): delegates param extraction
     to protocol/extract-params for consistency across HTTP and WS."
  (:require [futon3c.transport.protocol :as proto]
            [futon3c.transport.encyclopedia :as enc]
            [futon3c.evidence.store :as estore]
            [futon3c.agency.registry :as reg]
            [futon3c.agency.federation :as federation]
            [futon3c.social.mode :as mode]
            [futon3c.social.dispatch :as dispatch]
            [futon3c.social.presence :as presence]
            [futon3c.social.persist :as persist]
            [futon3c.social.whistles :as whistles]
            [futon3c.mission-control.service :as mcs]
            [futon3c.peripheral.mission-control-backend :as mcb]
            [futon3c.portfolio.core :as portfolio]
            [futon3c.reflection.core :as reflection]
            [futon3c.enrichment.query :as enrich]
            [meme.schema :as meme-schema]
            [meme.core :as meme-core]
            [meme.arrow :as meme-arrow]
            [next.jdbc :as meme-jdbc]
            [next.jdbc.result-set :as meme-rs]
            [futon3c.cyder :as cyder]
            [cheshire.core :as json]
            [cheshire.generate :as json-gen]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as cset]
            [clojure.string :as str]
            [org.httpkit.server :as hk])
  (:import [java.time Instant]
           [java.net Socket InetSocketAddress]
           [java.nio.channels AsynchronousCloseException ClosedChannelException ClosedSelectorException]
           [java.util UUID]
           [java.util.concurrent Executors ExecutorService]
           [org.httpkit.logger ContextLogger]))

;; =============================================================================
;; JSON encoders for java.time types (Cheshire doesn't handle them natively)
;; =============================================================================

(json-gen/add-encoder Instant
  (fn [val jg]
    (.writeString jg (str val))))

;; =============================================================================
;; Invoke thread pool — keeps long-running agent invocations off http-kit threads
;; =============================================================================

(defonce ^ExecutorService invoke-executor
  (Executors/newFixedThreadPool
   4
   (let [ctr (atom 0)]
     (reify java.util.concurrent.ThreadFactory
       (newThread [_ r]
         (doto (Thread. r (str "invoke-worker-" (swap! ctr inc)))
           (.setDaemon true)))))))

;; =============================================================================
;; Internal helpers
;; =============================================================================

(defn- read-body
  "Read request body as a string. Handles both InputStream and String."
  [request]
  (when-let [body (:body request)]
    (if (string? body) body (slurp body))))

(defn- json-response
  "Build a Ring response with JSON content-type.
   If serialization fails, returns a 500 with the error message
   rather than letting the exception propagate and wedge the server."
  [status body]
  {:status status
   :headers {"Content-Type" "application/json"}
   :body (if (string? body)
           body
           (try
             (json/generate-string body)
             (catch Exception e
               (json/generate-string
                {:ok false
                 :error "json-serialization-failed"
                 :message (.getMessage e)}))))})

(defn- error?
  "True if result is a SocialError (has :error/code key)."
  [result]
  (and (map? result) (contains? result :error/code)))

(defn- error-response
  "Convert a SocialError into a Ring response with mapped HTTP status."
  [social-error]
  (let [{:keys [status body]} (proto/render-error social-error)]
    {:status status
     :headers {"Content-Type" "application/json"}
     :body body}))

(defn- parse-query-params
  "Parse request query string into map of string -> string."
  [request]
  (let [query (:query-string request)]
    (if (or (nil? query) (str/blank? query))
      {}
      (->> (str/split query #"&")
           (remove str/blank?)
           (map (fn [pair]
                  (let [[k v] (str/split pair #"=" 2)]
                    [(enc/decode-uri-component k)
                     (enc/decode-uri-component (or v ""))])))
           (into {})))))

(defn- encyclopedia-opts
  "Resolve encyclopedia config options for handlers."
  [config]
  {:corpus-root (or (get-in config [:encyclopedia :corpus-root])
                    (System/getenv "FUTON3C_PLANETMATH_ROOT"))})

(defn- parse-int
  "Parse integer string to int; return nil when invalid."
  [s]
  (when (and (string? s) (not (str/blank? s)))
    (try
      (int (Long/parseLong s))
      (catch Exception _ nil))))

(defn- parse-bool
  "Parse boolean query values.
   Accepts true/false, 1/0, yes/no."
  [s]
  (when (string? s)
    (let [v (str/lower-case (str/trim s))]
      (cond
        (#{"true" "1" "yes"} v) true
        (#{"false" "0" "no"} v) false
        :else nil))))

(defonce ^:private !invoke-jobs-ledger (atom nil))

(declare invoke-execution-evidence)
(declare record-invoke-job-delivery!)

(defn- resolve-delivery-recorder
  "Best-effort resolver for futon3c.dev/record-invoke-delivery!.
   Returns nil when dev ns is not loaded."
  []
  (try
    (when-let [dev-ns (find-ns 'futon3c.dev)]
      (ns-resolve dev-ns 'record-invoke-delivery!))
    (catch Throwable _ nil)))

(def ^:dynamic *resolve-delivery-recorder*
  "Indirection for delivery-recorder lookup (test seam)."
  resolve-delivery-recorder)

(defn reset-invoke-jobs!
  "Test/dev helper: clear in-memory invoke-job ledger so next access reloads from disk."
  []
  (reset! !invoke-jobs-ledger nil))

(defn- invoke-jobs-store-path
  []
  (or (System/getenv "FUTON3C_INVOKE_JOBS_FILE")
      "/tmp/futon3c-invoke-jobs.edn"))

(defn- default-invoke-jobs-ledger
  []
  {:version 1
   :next-seq 0
   :job-order []
   :trace->job {}
   :jobs {}})

(defn- persist-invoke-jobs-ledger!
  [ledger]
  (try
    (spit (invoke-jobs-store-path) (pr-str ledger))
    (catch Throwable t
      (println (str "[invoke-jobs] persist failed: " (.getMessage t)))
      (flush)))
  ledger)

(defn- load-invoke-jobs-ledger
  []
  (let [f (io/file (invoke-jobs-store-path))]
    (if (.exists f)
      (try
        (let [parsed (edn/read-string (slurp f))]
          (if (map? parsed)
            (merge (default-invoke-jobs-ledger) parsed)
            (default-invoke-jobs-ledger)))
        (catch Throwable t
          (println (str "[invoke-jobs] load failed: " (.getMessage t)))
          (flush)
          (default-invoke-jobs-ledger)))
      (default-invoke-jobs-ledger))))

(defn- append-job-event
  [job event-type payload]
  (let [seq-num (inc (long (or (:event-seq job) 0)))
        event (merge {:seq seq-num
                      :type event-type
                      :at (str (Instant/now))}
                     payload)]
    (-> job
        (assoc :event-seq seq-num)
        (update :events (fnil conj []) event))))

(defn- recover-inflight-jobs
  [ledger]
  (let [failed-at (str (Instant/now))
        recover-one (fn [job]
                      (if (#{"queued" "running"} (str (:state job)))
                        (-> job
                            (assoc :state "failed"
                                   :finished-at failed-at
                                   :terminal-code "worker-lost-on-restart"
                                   :terminal-message "job did not reach terminal state before restart")
                            (append-job-event "failed" {:code "worker-lost-on-restart"
                                                        :message "recovered on startup"}))
                        job))
        recovered-jobs (into {}
                             (map (fn [[jid job]] [jid (recover-one job)]))
                             (:jobs ledger))]
    (assoc ledger :jobs recovered-jobs)))

(defn- ensure-invoke-jobs-ledger!
  []
  (when (nil? @!invoke-jobs-ledger)
    (let [loaded (-> (load-invoke-jobs-ledger) recover-inflight-jobs)]
      (reset! !invoke-jobs-ledger loaded)
      (persist-invoke-jobs-ledger! loaded)))
  @!invoke-jobs-ledger)

(defn- update-invoke-jobs-ledger!
  [f]
  (ensure-invoke-jobs-ledger!)
  (let [updated (swap! !invoke-jobs-ledger f)]
    (persist-invoke-jobs-ledger! updated)))

(defn- next-invoke-job-id
  [ledger]
  (let [next-seq (inc (long (or (:next-seq ledger) 0)))
        rand-sfx (subs (str (UUID/randomUUID)) 0 8)]
    [(str "invoke-" (System/currentTimeMillis) "-" next-seq "-" rand-sfx)
     next-seq]))

(defn- invoke-job-mode
  [prompt]
  (let [p (str (or prompt ""))]
  (cond
    (or (re-find #"(?i)\bmode:\s*task\b" p)
        (re-find #"(?i)\b(task assignment|fm-\d{3}|falsify|prove|counterexample|state of play)\b" p))
    "work"

    :else
    "brief")))

(defn- extract-trace-id
  [invoke-meta]
  (some (fn [k]
          (let [v (or (get invoke-meta k) (get invoke-meta (name k)))]
            (when (and (string? v) (not (str/blank? v)))
              v)))
        [:invoke-trace-id :invoke_trace_id :invokeTraceId]))

(def ^:private artifact-ref-re
  #"(?ix)
    (https?://github\.com/\S+/(?:pull|issues)/\d+)
    |
    (\bPR\s*#\d+\b)
    |
    (\b[0-9a-f]{7,40}\b)
    |
    ((?:/|\.{1,2}/|~?/)[^\s]+?\.(?:clj|cljs|cljc|el|md|txt|sh|py|js|ts|tsx|java|go|rs|tex|json|edn)\b))")

(defn- first-artifact-ref
  [text]
  (when (string? text)
    (some->> (re-find artifact-ref-re text)
             rest
             (remove nil?)
             first
             str/trim)))

(defn- summarize-result-text
  [text]
  (let [t (str/trim (str/replace (str (or text "")) #"\s+" " "))]
    (if (<= (count t) 220)
      t
      (str (subs t 0 217) "..."))))

(defn- http-delivery-surface?
  [surface]
  (let [s (str/lower-case (str/trim (str (or surface ""))))]
    (or (str/blank? s)
        (= s "http")
        (str/starts-with? s "http "))))

(defn- record-http-delivery!
  [{:keys [agent-id caller surface result]}]
  (when (http-delivery-surface? surface)
    (when-let [trace-id (extract-trace-id (:invoke-meta result))]
      (let [receipt {:surface "http"
                     :destination (str "caller " (or caller "http-caller"))
                     :delivered? true
                     :note "http-direct-response"}]
        (record-invoke-job-delivery! trace-id receipt)
        (when-let [record-fn (*resolve-delivery-recorder*)]
          (try
            (record-fn (str agent-id) (str trace-id)
                       {:surface "http"
                        :destination (str "caller " (or caller "http-caller"))
                        :delivered? true
                        :note "http-direct-response"})
            (catch Throwable _)))))))

(defn- classify-terminal
  [result no-evidence?]
  (cond
    no-evidence?
    ["failed" "no-execution-evidence" "codex task-mode reply had no execution evidence"]

    (:ok result)
    ["done" nil nil]

    :else
    (let [err (:error result)
          code (if (map? err) (:error/code err) :invoke-failed)
          msg (if (map? err) (:error/message err) (str err))]
      [(if (= :timeout code) "timeout" "failed")
       (name code)
       msg])))

(defn- create-invoke-job!
  [{:keys [requested-job-id agent-id prompt caller surface]}]
  (let [created-id (atom nil)]
    (update-invoke-jobs-ledger!
     (fn [ledger]
       (let [requested (some-> requested-job-id str str/trim)
             ;; Dedup: if the requested job-id already exists and is non-terminal,
             ;; reuse it instead of creating a duplicate.
             existing (when (seq requested)
                        (get-in ledger [:jobs requested]))
             reuse? (and existing
                         (#{"queued" "running"} (str (:state existing))))]
         (if reuse?
           (do (reset! created-id requested)
               ledger)  ;; no mutation — return existing job
           (let [[auto-id next-seq] (next-invoke-job-id ledger)
                 usable-requested (and (seq requested)
                                       (not (contains? (:jobs ledger) requested))
                                       requested)
                 job-id (or usable-requested auto-id)
                 created-at (str (Instant/now))
                 mode (invoke-job-mode prompt)
                 job {:job-id job-id
                      :agent-id (str agent-id)
                      :caller (str (or caller "http-caller"))
                      :surface (str (or surface "http"))
                      :mode mode
                      :state "queued"
                      :created-at created-at
                      :started-at nil
                      :finished-at nil
                      :terminal-code nil
                      :terminal-message nil
                      :session-id nil
                      :trace-id nil
                      :result-summary nil
                      :artifact-ref nil
                      :execution {:executed? false :tool-events 0 :command-events 0}
                      :delivery {:status "pending"}
                      :event-seq 0
                      :events []}]
             (reset! created-id job-id)
             (-> ledger
                 (assoc :next-seq next-seq)
                 (update :job-order (fnil conj []) job-id)
                 (assoc-in [:jobs job-id] (append-job-event job "accepted" {}))))))))
    @created-id))

(defn- mark-invoke-job-running!
  [job-id]
  (update-invoke-jobs-ledger!
   (fn [ledger]
     (if-let [job (get-in ledger [:jobs job-id])]
       (assoc-in ledger [:jobs job-id]
                 (-> job
                     (assoc :state "running"
                            :started-at (or (:started-at job) (str (Instant/now))))
                     (append-job-event "running" {})))
       ledger))))

(defn- finalize-invoke-job!
  [job-id terminal-state terminal-code terminal-message result sid]
  (let [invoke-meta (:invoke-meta result)
        execution (invoke-execution-evidence result)
        trace-id (extract-trace-id invoke-meta)
        result-text (when (string? (:result result)) (:result result))
        summary (when result-text (summarize-result-text result-text))
        artifact-ref (or (first-artifact-ref result-text)
                         (first-artifact-ref summary))]
    (update-invoke-jobs-ledger!
     (fn [ledger]
       (if-let [job (get-in ledger [:jobs job-id])]
         (let [finished-at (str (Instant/now))
               updated-job (-> job
                               (assoc :state terminal-state
                                      :finished-at finished-at
                                      :terminal-code terminal-code
                                      :terminal-message terminal-message
                                      :session-id sid
                                      :trace-id trace-id
                                      :result-summary summary
                                      :artifact-ref artifact-ref
                                      :execution execution)
                               (append-job-event terminal-state
                                                 {:code terminal-code
                                                  :message terminal-message}))]
           (cond-> (assoc-in ledger [:jobs job-id] updated-job)
             (and (string? trace-id) (not (str/blank? trace-id)))
             (assoc-in [:trace->job trace-id] job-id)))
         ledger)))))

(defn- record-invoke-job-delivery!
  [invoke-trace-id {:keys [surface destination delivered? note]}]
  (when (and (string? invoke-trace-id) (not (str/blank? invoke-trace-id)))
    (update-invoke-jobs-ledger!
     (fn [ledger]
       (if-let [job-id (get-in ledger [:trace->job invoke-trace-id])]
         (if-let [job (get-in ledger [:jobs job-id])]
           (let [delivered (boolean delivered?)
                 receipt {:status (if delivered "delivered" "delivery-failed")
                          :surface (str (or surface "unknown"))
                          :destination (str (or destination "unknown"))
                          :recorded-at (str (Instant/now))
                          :note (str (or note ""))}
                 updated-job (-> job
                                 (assoc :delivery receipt)
                                 (append-job-event "delivery-recorded" receipt))]
             (assoc-in ledger [:jobs job-id] updated-job))
           ledger)
         ledger)))))

(defn- record-invoke-job-delivery-by-job-id!
  [job-id {:keys [surface destination delivered? note]}]
  (when (and (string? job-id) (not (str/blank? job-id)))
    (update-invoke-jobs-ledger!
     (fn [ledger]
       (if-let [job (get-in ledger [:jobs job-id])]
         (let [delivered (boolean delivered?)
               receipt {:status (if delivered "delivered" "delivery-failed")
                        :surface (str (or surface "unknown"))
                        :destination (str (or destination "unknown"))
                        :recorded-at (str (Instant/now))
                        :note (str (or note ""))}
               updated-job (-> job
                               (assoc :delivery receipt)
                               (append-job-event "delivery-recorded" receipt))]
           (assoc-in ledger [:jobs job-id] updated-job))
         ledger)))))

(defn- invoke-job-public-view
  [job]
  (select-keys job [:job-id :agent-id :caller :surface :mode :state
                    :created-at :started-at :finished-at
                    :terminal-code :terminal-message
                    :session-id :trace-id
                    :result-summary :artifact-ref
                    :execution :delivery :events]))

(defn- get-invoke-job
  [job-id]
  (ensure-invoke-jobs-ledger!)
  (get-in @!invoke-jobs-ledger [:jobs (str job-id)]))

(defn- recent-invoke-jobs
  [limit]
  (ensure-invoke-jobs-ledger!)
  (let [ledger @!invoke-jobs-ledger
        ids (take-last (max 1 (int limit)) (:job-order ledger))]
    (->> ids
         reverse
         (map #(get-in ledger [:jobs %]))
         (remove nil?))))

(def ^:const stale-job-threshold-ms
  "Jobs running longer than this are considered stale and will be reaped."
  (* 35 60 1000))  ;; 35 minutes

(defn reap-stale-invoke-jobs!
  "Finalize jobs stuck in 'running' state past the stale threshold.
   Returns the count of reaped jobs."
  ([] (reap-stale-invoke-jobs! stale-job-threshold-ms))
  ([threshold-ms]
   (ensure-invoke-jobs-ledger!)
   (let [now-ms (System/currentTimeMillis)
         reaped (atom 0)]
     (update-invoke-jobs-ledger!
      (fn [ledger]
        (let [jobs (:jobs ledger)
              updated-jobs
              (reduce-kv
               (fn [acc jid job]
                 (if (and (= "running" (str (:state job)))
                          (let [started (:started-at job)]
                            (when (string? started)
                              (try
                                (let [started-ms (.toEpochMilli (Instant/parse started))
                                      age-ms (- now-ms started-ms)]
                                  (> age-ms threshold-ms))
                                (catch Exception _ false)))))
                   (do
                     (swap! reaped inc)
                     (let [finished-at (str (Instant/now))]
                       (assoc acc jid
                              (-> job
                                  (assoc :state "failed"
                                         :finished-at finished-at
                                         :terminal-code "stale-job-reaped"
                                         :terminal-message (str "Job stuck in running state; reaped after "
                                                                (/ threshold-ms 60000) " minutes"))
                                  (append-job-event "failed" {:code "stale-job-reaped"})))))
                   (assoc acc jid job)))
               {}
               jobs)]
          (assoc ledger :jobs updated-jobs))))
     (let [n @reaped]
       (when (pos? n)
         (println (str "[invoke-jobs] Reaped " n " stale job(s)"))
         (flush))
       n))))

(defonce ^:private !stale-job-reaper
  (let [running (atom true)
        thread (Thread.
                (fn []
                  (while @running
                    (try
                      (Thread/sleep 300000) ;; 5 minutes
                      (reap-stale-invoke-jobs!)
                      (catch InterruptedException _
                        (reset! running false))
                      (catch Throwable t
                        (println (str "[invoke-jobs] Reaper error: " (.getMessage t)))
                        (flush)))))
                "invoke-job-reaper")]
    (.setDaemon thread true)
    (.start thread)
    {:stop (fn [] (reset! running false) (.interrupt thread))}))

(defn- parse-keyword
  "Parse an API parameter into a keyword (accepts optional leading :)."
  [s]
  (when (and (string? s) (not (str/blank? s)))
    (let [v (str/trim s)
          raw (if (str/starts-with? v ":") (subs v 1) v)]
      (when (seq raw)
        (keyword raw)))))

(defn- parse-tags
  "Parse comma-separated tag list to vector of keywords."
  [s]
  (when (and (string? s) (not (str/blank? s)))
    (->> (str/split s #",")
         (map str/trim)
         (remove str/blank?)
         (map parse-keyword)
         (remove nil?)
         vec)))

(defn- parse-subject
  "Parse subject params into ArtifactRef."
  [params]
  (let [subject-type (parse-keyword (get params "subject-type"))
        subject-id (get params "subject-id")]
    (when (and subject-type (string? subject-id) (not (str/blank? subject-id)))
      {:ref/type subject-type
       :ref/id (str subject-id)})))

(defn- evidence-store-for-config
  "Resolve configured evidence store from runtime config."
  [config]
  (or (:evidence-store config)
      (get-in config [:registry :peripheral-config :evidence-store])))

(defn- expected-http-kit-shutdown-close?
  "True when `http-kit` accept-loop emitted an expected close exception while stopping."
  [msg ex]
  (and (= "accept incoming request" msg)
       (or (instance? ClosedChannelException ex)
           (instance? ClosedSelectorException ex)
           (instance? AsynchronousCloseException ex))))

(defn- expected-http-kit-stop-race?
  "True when `http-kit` stop hit the known JDK selector close race (NPE in removeKey)."
  [ex]
  (and (instance? NullPointerException ex)
       (let [msg (some-> ex .getMessage)
             top (first (.getStackTrace ex))]
         (and (some? top)
              (= "java.nio.channels.spi.AbstractSelectableChannel" (.getClassName ^StackTraceElement top))
              (= "removeKey" (.getMethodName ^StackTraceElement top))
              (or (nil? msg)
                  (str/includes? msg "this.keys"))))))

(defn- make-http-kit-error-logger
  "Create an error logger that suppresses expected shutdown close races only."
  [!server]
  (fn [msg ex]
    (let [status (some-> @!server hk/server-status)
          suppress? (and (not= :running status)
                         (expected-http-kit-shutdown-close? msg ex))]
      (when-not suppress?
        (.log ContextLogger/ERROR_PRINTER msg ex)))))

(defn- normalize-artifact-ref
  "Normalize subject map (string or keyword :ref/type accepted)."
  [subject]
  (when (map? subject)
    (let [subject-type (or (parse-keyword (get subject :ref/type))
                           (parse-keyword (get subject "ref/type")))
          subject-id (or (get subject :ref/id)
                         (get subject "ref/id"))]
      (when (and subject-type (some? subject-id) (not (str/blank? (str subject-id))))
        {:ref/type subject-type
         :ref/id (str subject-id)}))))

(defn- normalize-tags
  "Normalize vector/list of tags to keyword vector."
  [tags]
  (when (coll? tags)
    (->> tags
         (map #(cond
                 (keyword? %) %
                 (string? %) (parse-keyword %)
                 :else nil))
         (remove nil?)
         vec)))

(defn- non-blank-string
  "Return trimmed string when non-blank; otherwise nil."
  [s]
  (when (and (string? s) (not (str/blank? s)))
    (str/trim s)))

;; =============================================================================
;; Route handlers
;; =============================================================================

(defn- live-registry
  "Build a registry that merges the static config snapshot with live-registered agents.
   HTTP-registered and federated agents appear in the live registry atom but not
   in the startup config snapshot. Merge ensures dispatch can reach them."
  [config]
  (let [static-reg (:registry config)
        live-status (reg/registry-status)
        live-agents (into {}
                         (map (fn [[id {:keys [capabilities type]}]]
                                [id (cond-> {:capabilities (vec capabilities)}
                                      (some? type) (assoc :type type))]))
                         (:agents live-status))
        merged-agents (merge (:agents static-reg) live-agents)]
    (assoc static-reg :agents merged-agents)))

(defn- handle-dispatch
  "POST /dispatch — parse JSON body, classify message, dispatch to agent.
   Returns 200 + DispatchReceipt JSON or error status + SocialError JSON.
   Uses live registry (merged with config snapshot) so federated agents are reachable."
  [request config]
  (let [body (read-body request)
        parsed (proto/parse-dispatch-request (or body ""))]
    (if (error? parsed)
      (error-response parsed)
      (let [classified (mode/classify parsed (:patterns config))
            registry (live-registry config)]
        (if (error? classified)
          (error-response classified)
          (let [result (dispatch/dispatch classified registry)]
            (if (error? result)
              (error-response result)
              (json-response 200 (proto/render-receipt result)))))))))

(defn- handle-presence
  "POST /presence — parse JSON body, verify agent presence via S-presence.
   Returns 200 + PresenceRecord JSON or error status + SocialError JSON.
   Uses live registry so federated agents are verifiable."
  [request config]
  (let [body (read-body request)
        parsed (proto/parse-presence-request (or body ""))]
    (if (error? parsed)
      (error-response parsed)
      (let [result (presence/verify parsed (live-registry config))]
        (if (error? result)
          (error-response result)
          (json-response 200 (proto/render-ws-frame result)))))))

(defn- handle-get-session
  "GET /session/:id — retrieve session by ID from persist store.
   Returns 200 + SessionRecord JSON or 404."
  [request _config]
  (let [uri (:uri request)
        session-id (when (str/starts-with? (str uri) "/session/")
                     (subs uri (count "/session/")))]
    (if (or (nil? session-id) (str/blank? session-id))
      (json-response 400 {"error" true
                          "code" "invalid-request"
                          "message" "Missing session ID in path"})
      (let [result (persist/get-session session-id)]
        (if (error? result)
          (error-response result)
          (json-response 200
            {"session_id" (:session/id result)
             "agent_id" (when-let [aid (:session/agent-id result)]
                          {"value" (:id/value aid)
                           "type" (name (:id/type aid))})
             "state" (:session/state result)
             "at" (str (:session/at result))}))))))

(defn- bridge-health-file
  "Path to the ngircd bridge health JSON file."
  []
  (let [runtime-dir (or (System/getenv "XDG_RUNTIME_DIR") "/tmp")]
    (io/file runtime-dir "ngircd-bridge-health.json")))

(defn- read-bridge-health
  "Read the ngircd bridge health file. Returns a map with at minimum
   {:status \"ok\"|\"stale\"|\"absent\"|\"error\"}."
  []
  (let [f (bridge-health-file)]
    (if (.exists f)
      (try
        (let [data (json/parse-string (slurp f) true)
              updated-at (:updated_at data)
              age-seconds (when updated-at
                            (try
                              (.getSeconds
                                (java.time.Duration/between
                                  (Instant/parse updated-at)
                                  (Instant/now)))
                              (catch Exception _ nil)))
              stale? (or (nil? age-seconds) (> age-seconds 90))]
          (assoc data
                 :status (if stale? "stale" "ok")
                 :stale stale?
                 :age_seconds age-seconds))
        (catch Exception e
          {"status" "error" "error" (.getMessage e)}))
      {"status" "absent"})))

(defn- handle-health
  "GET /health — return agent, session, and evidence counts.
   Reads both live registry and config snapshot, reports the larger count.
   Live registry reflects HTTP-registered and federated agents;
   config snapshot reflects agents wired at startup."
  [config started-at]
  (let [now (Instant/now)
        uptime-seconds (max 0 (.getSeconds (java.time.Duration/between started-at now)))
        live-status (reg/registry-status)
        live-count (:count live-status)
        config-count (count (get-in config [:registry :agents]))
        agent-summary (into {}
                            (map (fn [[id info]]
                                   [id {:type (:type info)
                                        :last-active (:last-active info)
                                        :capabilities (:capabilities info)
                                        :invoke-route (:invoke-route info)
                                        :invoke-ready? (:invoke-ready? info)}]))
                            (:agents live-status))
        evidence-store (evidence-store-for-config config)
        evidence-count (count (estore/query* evidence-store {}))
        irc-send-base (some-> (:irc-send-base config) str str/trim not-empty)
        irc-relay-configured? (fn? (:irc-send-fn config))
        bridge (read-bridge-health)]
    (json-response 200 {"status" "ok"
                         "agents" (max live-count config-count)
                         "sessions" (count (persist/list-sessions {}))
                         "agent-summary" agent-summary
                         "irc-relay-configured" irc-relay-configured?
                         "irc-send-base" irc-send-base
                         "evidence" evidence-count
                         "started-at" (str started-at)
                         "uptime-seconds" uptime-seconds
                         "bridge" bridge})))

(defn- handle-encyclopedia-corpuses
  "GET /fulab/encyclopedia/corpuses — list available corpuses."
  [config]
  (json-response 200 {:ok true
                      :corpuses (enc/list-corpuses (encyclopedia-opts config))}))

(defn- handle-encyclopedia-entries
  "GET /fulab/encyclopedia/:corpus/entries — list paginated entry summaries."
  [request config corpus-name]
  (let [params (parse-query-params request)
        limit (enc/parse-int (get params "limit") 100)
        offset (enc/parse-int (get params "offset") 0)
        opts (encyclopedia-opts config)]
    (if-let [page (enc/page-entries opts corpus-name limit offset)]
      (json-response 200 (assoc page :ok true))
      (json-response 404 {:ok false :err "corpus-not-found" :corpus corpus-name}))))

(defn- handle-encyclopedia-entry
  "GET /fulab/encyclopedia/:corpus/entry/:id — fetch full entry."
  [config corpus-name entry-id]
  (let [opts (encyclopedia-opts config)]
    (if-let [entry (enc/find-entry opts corpus-name entry-id)]
      (json-response 200 {:ok true :entry entry})
      (json-response 404 {:ok false
                          :err "entry-not-found"
                          :corpus corpus-name
                          :entry-id entry-id}))))

(defn- handle-evidence-query
  "GET /api/alpha/evidence — query evidence entries."
  [request config]
  (let [params (parse-query-params request)
        limit (parse-int (get params "limit"))
        subject (parse-subject params)
        evidence-type (parse-keyword (get params "type"))
        claim-type (parse-keyword (get params "claim-type"))
        include-ephemeral? (parse-bool (get params "include-ephemeral?"))
        pattern-id (parse-keyword (get params "pattern-id"))
        tags (parse-tags (get params "tag"))
        author (non-blank-string (get params "author"))
        session-id (non-blank-string (get params "session-id"))
        query (cond-> {}
                subject (assoc :query/subject subject)
                evidence-type (assoc :query/type evidence-type)
                claim-type (assoc :query/claim-type claim-type)
                (get params "since") (assoc :query/since (get params "since"))
                (some? include-ephemeral?)
                (assoc :query/include-ephemeral? include-ephemeral?)
                (seq tags) (assoc :query/tags tags))
        evidence-store (evidence-store-for-config config)
        entries (cond->> (estore/query* evidence-store query)
                  true
                  (filter (fn [entry]
                            (and
                             (or (nil? author)
                                 (= author (:evidence/author entry)))
                             (or (nil? session-id)
                                 (= session-id (:evidence/session-id entry)))
                             (or (nil? pattern-id)
                                 (= pattern-id (:evidence/pattern-id entry))))))
                  (and (int? limit) (pos? limit))
                  (take limit)
                  true
                  vec)]
    (json-response 200 {:ok true
                        :count (count entries)
                        :entries entries})))

(defn- handle-evidence-count
  "GET /api/alpha/evidence/count — return filtered evidence count."
  [request config]
  (let [params (parse-query-params request)
        subject (parse-subject params)
        evidence-type (parse-keyword (get params "type"))
        claim-type (parse-keyword (get params "claim-type"))
        include-ephemeral? (parse-bool (get params "include-ephemeral?"))
        pattern-id (parse-keyword (get params "pattern-id"))
        tags (parse-tags (get params "tag"))
        author (non-blank-string (get params "author"))
        session-id (non-blank-string (get params "session-id"))
        query (cond-> {}
                subject (assoc :query/subject subject)
                evidence-type (assoc :query/type evidence-type)
                claim-type (assoc :query/claim-type claim-type)
                (get params "since") (assoc :query/since (get params "since"))
                (some? include-ephemeral?)
                (assoc :query/include-ephemeral? include-ephemeral?)
                (seq tags) (assoc :query/tags tags))
        evidence-store (evidence-store-for-config config)
        count* (->> (estore/query* evidence-store query)
                    (filter (fn [entry]
                              (and
                               (or (nil? author)
                                   (= author (:evidence/author entry)))
                               (or (nil? session-id)
                                   (= session-id (:evidence/session-id entry)))
                               (or (nil? pattern-id)
                                   (= pattern-id (:evidence/pattern-id entry))))))
                    count)]
    (json-response 200 {:ok true
                        :count count*})))

(defn- handle-evidence-entry
  "GET /api/alpha/evidence/:id — fetch one entry."
  [config evidence-id]
  (let [evidence-store (evidence-store-for-config config)
        entry (estore/get-entry* evidence-store evidence-id)]
    (if entry
      (json-response 200 {:ok true
                          :entry entry})
      (json-response 404 {:ok false
                          :err "evidence-not-found"
                          :evidence-id evidence-id}))))

(defn- handle-evidence-chain
  "GET /api/alpha/evidence/:id/chain — fetch ordered ancestor chain."
  [config evidence-id]
  (let [evidence-store (evidence-store-for-config config)
        entry (estore/get-entry* evidence-store evidence-id)]
    (if (nil? entry)
      (json-response 404 {:ok false
                          :err "evidence-not-found"
                          :evidence-id evidence-id})
      (let [chain (estore/get-reply-chain* evidence-store evidence-id)]
        (json-response 200 {:ok true
                            :evidence-id evidence-id
                            :chain chain})))))

(defn- parse-json-map
  "Parse BODY as JSON map; return nil on parse/shape failure."
  [body]
  (try
    (let [parsed (json/parse-string (or body "") true)]
      (when (map? parsed)
        parsed))
    (catch Exception _ nil)))

(defn- append-error-status
  "Map evidence append error codes to HTTP status."
  [code]
  (case code
    :duplicate-id 409
    :reply-not-found 409
    :fork-not-found 409
    :invalid-entry 400
    :invalid-input 400
    400))

(defn- normalize-evidence-payload
  "Normalize write payload (string fields to keywords where needed)."
  [payload]
  (let [subject (or (normalize-artifact-ref (:subject payload))
                    (let [subject-type (parse-keyword (or (:subject-type payload) (get payload "subject-type")))
                          subject-id (or (:subject-id payload) (get payload "subject-id"))]
                      (when (and subject-type (some? subject-id) (not (str/blank? (str subject-id))))
                        {:ref/type subject-type
                         :ref/id (str subject-id)})))
        entry {:evidence-id (or (:evidence-id payload) (get payload "evidence-id"))
               :subject subject
               :type (parse-keyword (or (:type payload) (get payload "type")))
               :claim-type (parse-keyword (or (:claim-type payload) (get payload "claim-type")))
               :author (or (:author payload) (get payload "author"))
               :body (or (:body payload) (get payload "body"))
               :pattern-id (parse-keyword (or (:pattern-id payload) (get payload "pattern-id")))
               :session-id (or (:session-id payload) (get payload "session-id"))
               :in-reply-to (or (:in-reply-to payload) (get payload "in-reply-to"))
               :fork-of (or (:fork-of payload) (get payload "fork-of"))
               :conjecture? (or (:conjecture? payload) (get payload "conjecture?"))
               :ephemeral? (or (:ephemeral? payload) (get payload "ephemeral?"))
               :tags (or (normalize-tags (:tags payload))
                         (normalize-tags (get payload "tags")))}]
    (-> entry
        (cond-> (or (nil? (:evidence-id entry))
                    (str/blank? (str (:evidence-id entry))))
          (dissoc :evidence-id))
        (cond-> (nil? (:subject entry))
          (dissoc :subject))
        (cond-> (nil? (:pattern-id entry))
          (dissoc :pattern-id))
        (cond-> (or (nil? (:session-id entry))
                    (str/blank? (str (:session-id entry))))
          (dissoc :session-id))
        (cond-> (or (nil? (:in-reply-to entry))
                    (str/blank? (str (:in-reply-to entry))))
          (dissoc :in-reply-to))
        (cond-> (or (nil? (:fork-of entry))
                    (str/blank? (str (:fork-of entry))))
          (dissoc :fork-of))
        (cond-> (nil? (:conjecture? entry))
          (dissoc :conjecture?))
        (cond-> (nil? (:ephemeral? entry))
          (dissoc :ephemeral?))
        (cond-> (empty? (:tags entry))
          (dissoc :tags)))))

(defn- handle-evidence-create
  "POST /api/alpha/evidence — append one evidence entry."
  [request config]
  (let [payload (parse-json-map (read-body request))]
    (if (nil? payload)
      (json-response 400 {:ok false
                          :err "invalid-json"
                          :message "Request body must be a JSON object"})
      (let [evidence-store (evidence-store-for-config config)
            normalized (normalize-evidence-payload payload)
            result (estore/append* evidence-store normalized)]
        (if (:ok result)
          (json-response 201 {:ok true
                              :evidence/id (get-in result [:entry :evidence/id])
                              :entry (:entry result)})
          (json-response (append-error-status (:error/code result))
                         {:ok false
                          :err (name (:error/code result))
                          :error result}))))))

;; =============================================================================
;; Agent registration endpoints
;; =============================================================================

(def ^:private default-capabilities
  {:claude [:explore :edit :test :coordination/execute]
   :codex  [:edit :test :coordination/execute]
   :tickle [:mission-control :discipline :coordination/execute]})

(defn- handle-agents-register
  "POST /api/alpha/agents — register an agent via HTTP.
   Body: {\"agent-id\": \"codex-1\", \"type\": \"codex\",
          \"origin-url\": \"http://...\", \"proxy\": true,
          \"ws-bridge\": true}
   Local registration (no origin-url): default no-op invoke-fn.
   Set ws-bridge=true to register with no local invoke-fn and use WS fallback.
   Proxy registration (with origin-url): invoke-fn forwards to origin Agency."
  [request _config]
  (let [payload (parse-json-map (read-body request))]
    (if (nil? payload)
      (json-response 400 {:ok false :err "invalid-json"
                          :message "Request body must be a JSON object"})
      (let [agent-id (or (:agent-id payload) (get payload "agent-id"))
            agent-type-str (or (:type payload) (get payload "type"))
            agent-type (parse-keyword agent-type-str)
            origin-url (or (:origin-url payload) (get payload "origin-url"))
            proxy? (or (:proxy payload) (get payload "proxy") (some? origin-url))
            ws-bridge? (boolean (or (:ws-bridge payload)
                                    (get payload "ws-bridge")
                                    (:ws_bridge payload)
                                    (get payload "ws_bridge")))
            caps-raw (or (:capabilities payload) (get payload "capabilities"))
            capabilities (if (sequential? caps-raw)
                           (mapv keyword caps-raw)
                           (get default-capabilities agent-type []))]
        (cond
          (or (nil? agent-id) (str/blank? (str agent-id)))
          (json-response 400 {:ok false :err "missing-agent-id"
                              :message "agent-id is required"})

          (nil? agent-type)
          (json-response 400 {:ok false :err "missing-type"
                              :message "type is required (claude, codex, tickle, mock)"})

          :else
          (let [invoke-fn (cond
                            (and proxy? origin-url (not (str/blank? origin-url)))
                            (federation/make-proxy-invoke-fn origin-url agent-id)

                            ws-bridge?
                            nil

                            :else
                            (fn [_prompt _session-id]
                              {:result "registered-via-http" :session-id nil}))
                result (reg/register-agent!
                        {:agent-id {:id/value (str agent-id) :id/type :continuity}
                         :type agent-type
                         :invoke-fn invoke-fn
                         :capabilities capabilities
                         :metadata (cond-> {}
                                     proxy? (assoc :proxy? true)
                                     ws-bridge? (assoc :ws-bridge? true)
                                     origin-url (assoc :origin-url origin-url))})]
            (if (and (map? result) (= false (:ok result)))
              (json-response 409 {:ok false
                                  :err "duplicate-registration"
                                  :message (str "Agent already registered: " agent-id)
                                  :detail result})
              (if (and (map? result) (:agent/id result))
                (json-response 201 {:ok true
                                    :agent-id (get-in result [:agent/id :id/value])
                                    :type (name (:agent/type result))
                                    :proxy proxy?
                                    :ws-bridge ws-bridge?})
                (json-response 409 {:ok false
                                    :err "registration-failed"
                                    :message (str "Could not register: " agent-id)
                                    :detail result})))))))))

(defn- handle-agents-auto-register
  "POST /api/alpha/agents/auto — allocate and register next available agent.
   Body: {\"type\": \"claude\"} — type is required.
   Finds the next unused ID (e.g. claude-2 if claude-1 exists) and registers it
   with a real invoke-fn (resolved from dev.clj's make-claude-invoke-fn).
   Each call creates a new, independent agent (I-1: one agent = one identity)."
  [request _config]
  (let [payload (parse-json-map (read-body request))]
    (if (nil? payload)
      (json-response 400 {:ok false :err "invalid-json"})
      (let [agent-type-str (or (:type payload) (get payload "type"))
            agent-type (parse-keyword agent-type-str)]
        (if (nil? agent-type)
          (json-response 400 {:ok false :err "missing-type"
                              :message "type is required"})
          (let [prefix (name agent-type)
                existing (reg/registered-agents)
                matching-ids (->> existing
                                  (map :id/value)
                                  (filter #(str/starts-with? (str %) (str prefix "-")))
                                  set)
                next-n (loop [n 1]
                         (if (contains? matching-ids (str prefix "-" n))
                           (recur (inc n))
                           n))
                agent-id (str prefix "-" next-n)
                ;; Build invoke-fn: resolve make-claude-invoke-fn from dev ns
                emacs-socket (or (:emacs-socket payload) (get payload "emacs-socket"))
                invoke-fn (when (= agent-type :claude)
                            (try
                              (require 'futon3c.dev)
                              (when-let [make-fn (resolve 'futon3c.dev/make-claude-invoke-fn)]
                                (let [sf (format "/tmp/futon-session-id-%s" agent-id)
                                      sid-atom (atom (when (.exists (java.io.File. sf))
                                                       (str/trim (slurp sf))))]
                                  (@make-fn (cond-> {:agent-id agent-id
                                                     :session-file sf
                                                     :session-id-atom sid-atom}
                                              emacs-socket (assoc :emacs-socket emacs-socket)))))
                              (catch Throwable _ nil)))
                result (reg/register-agent!
                        {:agent-id {:id/value agent-id :id/type :continuity}
                         :type agent-type
                         :invoke-fn invoke-fn
                         :capabilities (get default-capabilities agent-type [])
                         :metadata (cond-> {:auto-registered? true}
                                     emacs-socket (assoc :emacs-socket emacs-socket))})]
            (when (and invoke-fn (map? result) (:agent/id result))
              (reg/update-agent! agent-id :agent/invoke-fn invoke-fn))
            (if (and (map? result) (:agent/id result))
              (json-response 201 {:ok true
                                  :agent-id agent-id
                                  :type (name agent-type)})
              (json-response 409 {:ok false
                                  :err "registration-failed"
                                  :message (str "Could not register: " agent-id)}))))))))

(defn- handle-agent-rebind
  "POST /api/alpha/agents/:id/rebind — update an agent's invoke-fn socket.
   Body: {\"emacs-socket\": \"workspace1\"}
   Recreates the invoke-fn with the new emacs-socket so blackboard calls
   go to the correct Emacs daemon."
  [_config agent-id request]
  (let [payload (parse-json-map (read-body request))
        emacs-socket (or (:emacs-socket payload) (get payload "emacs-socket"))]
    (if (str/blank? emacs-socket)
      (json-response 400 {:ok false :err "missing-emacs-socket"})
      (let [agent (reg/get-agent agent-id)]
        (if (nil? agent)
          (json-response 404 {:ok false :err "not-found"
                              :message (str "No agent: " agent-id)})
          (try
            (require 'futon3c.dev)
            (if-let [make-fn (resolve 'futon3c.dev/make-claude-invoke-fn)]
              (let [sf (format "/tmp/futon-session-id-%s" agent-id)
                    existing-sid (:agent/session-id agent)
                    sid-atom (atom (or existing-sid
                                       (when (.exists (java.io.File. sf))
                                         (str/trim (slurp sf)))))
                    invoke-fn (@make-fn {:agent-id agent-id
                                         :session-file sf
                                         :session-id-atom sid-atom
                                         :emacs-socket emacs-socket})]
                (reg/update-agent! agent-id
                                   :agent/invoke-fn invoke-fn
                                   :agent/metadata (assoc (or (:agent/metadata agent) {})
                                                          :emacs-socket emacs-socket))
                (json-response 200 {:ok true :agent-id agent-id
                                    :emacs-socket emacs-socket}))
              (json-response 500 {:ok false :err "make-fn-not-found"}))
            (catch Throwable e
              (json-response 500 {:ok false :err "rebind-failed"
                                  :message (.getMessage e)}))))))))

(defn- emit-invoke-evidence!
  "Emit a forum-post evidence entry for an invoke prompt or response.
   Mirrors the pattern used by the IRC transport so chat messages from
   all surfaces are queryable in the same evidence landscape.
   When mission-id is provided, tags evidence with that mission subject.
   Fire-and-forget: runs in a future so HTTP handler threads are never
   blocked by XTDB indexing delays."
  [evidence-store author text session-id & {:keys [mission-id]}]
  (when evidence-store
    (future
      (try
        (let [subject (if mission-id
                        {:ref/type :mission :ref/id mission-id}
                        {:ref/type :thread :ref/id "emacs/chat"})
              tags (cond-> [:emacs :chat :transport/emacs-chat]
                     mission-id (conj :mission-focused))]
          (estore/append* evidence-store
                          {:evidence/id (str "e-" (UUID/randomUUID))
                           :evidence/subject subject
                           :evidence/type :forum-post
                           :evidence/claim-type :observation
                           :evidence/author author
                           :evidence/at (str (Instant/now))
                           :evidence/body {:channel "emacs-chat"
                                           :text text
                                           :from author
                                           :transport :emacs-chat}
                           :evidence/tags tags
                           :evidence/session-id (or session-id "pending")}))
        (catch Exception e
          (println (str "[invoke] evidence emit warning: " (.getMessage e))))))))

(defn- emit-review-snapshot!
  "Emit a portfolio snapshot evidence entry after a successful review.
   Stores compact mission id+status pairs for diffing between reviews.
   Fire-and-forget: runs in a future so HTTP handler threads are never
   blocked by XTDB indexing delays."
  [evidence-store author review-result]
  (when evidence-store
    (future
      (try
        (let [missions (get review-result "portfolio/missions"
                         (:portfolio/missions review-result))
              summary (get review-result "portfolio/summary"
                        (:portfolio/summary review-result))
              coverage (get review-result "portfolio/coverage"
                         (:portfolio/coverage review-result))
              ;; Compact form: just id + status per mission
              compact-missions (vec (for [m (or missions [])]
                                     (let [mid (or (get m "mission/id")
                                                   (:mission/id m) "?")
                                           status (or (get m "mission/status")
                                                      (:mission/status m) "unknown")]
                                       {:mission/id mid :mission/status status})))]
          (estore/append* evidence-store
                          {:evidence/id (str "e-review-" (UUID/randomUUID))
                           :evidence/subject {:ref/type :portfolio :ref/id "global"}
                           :evidence/type :coordination
                           :evidence/claim-type :observation
                           :evidence/author (or author "mission-control")
                           :evidence/at (str (Instant/now))
                           :evidence/body {:portfolio/missions compact-missions
                                           :portfolio/summary summary
                                           :portfolio/coverage coverage}
                           :evidence/tags [:review :portfolio-snapshot]}))
        (catch Exception e
          (println (str "[review] snapshot emit warning: " (.getMessage e))))))))

(defn- wrap-surface-header
  "Prepend an authoritative surface header to PROMPT when SURFACE is non-nil.
   This ensures the agent sees a consistent, unambiguous surface declaration
   on every turn — even when session history has messages from other surfaces."
  [prompt surface caller]
  (if (and surface (not (str/blank? (str surface))))
    (str "--- CURRENT TURN ---\n"
         "Surface: " surface "\n"
         (when (and caller (not (str/blank? (str caller))))
           (str "Caller: " caller "\n"))
         "---\n\n"
         prompt)
    prompt))

(def ^:private task-mode-re
  #"(?i)\bmode:\s*task\b")

(def ^:private mission-work-re
  #"(?i)\b(task assignment|fm-\d{3}|falsify|prove|counterexample|state of play)\b")

(def ^:private planning-only-re
  #"(?i)\b(planning-only|not started|need clarification|need more context|cannot execute yet|blocked)\b")

(defn- invoke-execution-evidence
  "Extract execution evidence map from invoke result metadata."
  [result]
  (let [invoke-meta (:invoke-meta result)
        execution (or (:execution invoke-meta) (get invoke-meta "execution"))
        raw-executed (or (:executed? execution) (get execution "executed?")
                         (:executed execution) (get execution "executed"))
        executed (cond
                   (boolean? raw-executed) raw-executed
                   (string? raw-executed) (boolean (parse-bool raw-executed))
                   :else (boolean raw-executed))
        tool-events (long (or (:tool-events execution) (get execution "tool-events") 0))
        command-events (long (or (:command-events execution) (get execution "command-events") 0))]
    {:executed executed
     :tool-events tool-events
     :command-events command-events}))

(defn- codex-task-no-execution?
  "True when a codex task-mode reply reports no execution evidence and isn't planning-only."
  [agent-id prompt result]
  (let [aid (some-> agent-id str str/lower-case)
        text (some-> (:result result) str str/trim)
        {:keys [executed tool-events command-events]} (invoke-execution-evidence result)]
    (and (string? aid)
         (str/starts-with? aid "codex")
         (string? prompt)
         (or (boolean (re-find task-mode-re prompt))
             (boolean (re-find mission-work-re prompt)))
         (string? text)
         (not (str/blank? text))
         (not (boolean (re-find planning-only-re text)))
         (not executed)
         (zero? tool-events)
         (zero? command-events))))

(defn- build-invoke-response
  "Run a direct invoke and convert it to a Ring response map."
  [{:keys [payload agent-id prompt evidence-store]}]
  (let [caller (or (some-> payload :caller str)
                   (some-> payload (get "caller") str)
                   "http-caller")
        surface (or (some-> payload :surface str)
                    (some-> payload (get "surface") str))
        requested-job-id (or (:job-id payload) (get payload "job-id")
                             (:job_id payload) (get payload "job_id"))
        mission-id (or (:mission-id payload) (get payload "mission-id"))
        timeout-ms (some-> (or (:timeout-ms payload) (get payload "timeout-ms"))
                           long)
        ev-opts (when mission-id [:mission-id mission-id])
        job-id (create-invoke-job! {:requested-job-id requested-job-id
                                    :agent-id agent-id
                                    :prompt prompt
                                    :caller caller
                                    :surface surface})]
    (try
      (mark-invoke-job-running! job-id)
      (let [effective-prompt (wrap-surface-header prompt surface caller)
            result (reg/invoke-agent! (str agent-id) effective-prompt timeout-ms)
            sid (:session-id result)
            no-evidence? (and (:ok result)
                              (codex-task-no-execution? agent-id effective-prompt result))
            [terminal-state terminal-code terminal-message]
            (classify-terminal result no-evidence?)]
        (apply emit-invoke-evidence! evidence-store caller (str prompt) sid
               (or ev-opts []))
        (finalize-invoke-job! job-id terminal-state terminal-code terminal-message result sid)
        (record-http-delivery! {:agent-id agent-id
                                :caller caller
                                :surface surface
                                :result result})
        (if no-evidence?
          (json-response 502 {:ok false
                              :error "invoke-no-execution-evidence"
                              :message "codex task-mode reply had no execution evidence"
                              :session-id sid
                              :job-id job-id
                              :invoke-meta (:invoke-meta result)})
          (if (:ok result)
            (do
              (apply emit-invoke-evidence! evidence-store (str agent-id) (str (:result result)) sid
                     (or ev-opts []))
              (json-response 200 (cond-> {:ok true
                                          :job-id job-id
                                          :result (:result result)
                                          :session-id sid}
                                   (:invoke-meta result)
                                   (assoc :invoke-meta (:invoke-meta result)))))
            (let [err (:error result)
                  code (if (map? err) (:error/code err) :invoke-failed)
                  msg (if (map? err) (:error/message err) (str err))]
              (json-response (if (= :agent-not-found code) 404 502)
                             {:ok false
                              :job-id job-id
                              :error (name code)
                              :message msg})))))
      (catch Throwable t
        (finalize-invoke-job! job-id "failed" "invoke-error" (.getMessage t) {:ok false} nil)
        (json-response 500 {:ok false
                            :job-id job-id
                            :error "invoke-error"
                            :message (.getMessage t)})))))

(defn- run-invoke-job!
  "Execute a queued invoke job to terminal state.
   Used by async bell worker and can be reused by other async surfaces."
  [{:keys [job-id agent-id prompt caller surface timeout-ms mission-id evidence-store]}]
  (let [ev-opts (when mission-id [:mission-id mission-id])]
    (try
      (mark-invoke-job-running! job-id)
      (let [effective-prompt (wrap-surface-header prompt surface caller)
            result (reg/invoke-agent! (str agent-id) effective-prompt timeout-ms)
            sid (:session-id result)
            no-evidence? (and (:ok result)
                              (codex-task-no-execution? agent-id effective-prompt result))
            [terminal-state terminal-code terminal-message]
            (classify-terminal result no-evidence?)]
        (apply emit-invoke-evidence! evidence-store caller (str prompt) sid
               (or ev-opts []))
        (finalize-invoke-job! job-id terminal-state terminal-code terminal-message result sid)
        ;; HTTP direct responses are auto-delivered to caller; bell responses are not.
        (record-http-delivery! {:agent-id agent-id
                                :caller caller
                                :surface surface
                                :result result})
        (when (and (:ok result) (not no-evidence?))
          (apply emit-invoke-evidence! evidence-store (str agent-id) (str (:result result)) sid
                 (or ev-opts [])))
        {:ok true
         :job-id job-id
         :result result
         :session-id sid
         :no-execution-evidence? no-evidence?
         :terminal-state terminal-state
         :terminal-code terminal-code
         :terminal-message terminal-message})
      (catch Throwable t
        (finalize-invoke-job! job-id "failed" "invoke-error" (.getMessage t) {:ok false} nil)
        {:ok false
         :job-id job-id
         :error "invoke-error"
         :message (.getMessage t)}))))

(defn- handle-invoke
  "POST /api/alpha/invoke — invoke a registered agent directly.
   Body: {\"agent-id\": \"claude-1\", \"prompt\": \"hello\",
          \"surface\": \"irc\", \"caller\": \"joe\"}
   Returns the invoke-fn result: {\"ok\": true, \"result\": \"...\", \"session-id\": \"...\"}
   or error: {\"ok\": false, \"error\": \"...\", \"message\": \"...\"}

   When `surface` is provided, an authoritative surface header is prepended
   to the prompt so the agent always knows which surface the current turn
   came from — regardless of what previous turns in the session said.

   This is the direct invocation endpoint — the futon3c equivalent of futon3's
   POST /agency/page. Both Emacs and IRC peripherals route through this.
   Emits evidence entries for both prompt and response (parity with IRC transport)."
  [request config]
  (let [payload (parse-json-map (read-body request))]
    (if (nil? payload)
      (json-response 400 {:ok false :err "invalid-json"
                          :message "Request body must be a JSON object"})
      (let [agent-id (or (:agent-id payload) (get payload "agent-id"))
            prompt (or (:prompt payload) (get payload "prompt"))
            evidence-store (evidence-store-for-config config)
            invoke-opts {:payload payload
                         :agent-id agent-id
                         :prompt prompt
                         :evidence-store evidence-store}]
        (cond
          (or (nil? agent-id) (str/blank? (str agent-id)))
          (json-response 400 {:ok false :err "missing-agent-id"
                              :message "agent-id is required"})

          (nil? prompt)
          (json-response 400 {:ok false :err "missing-prompt"
                              :message "prompt is required"})

          (:async-channel request)
          (let [invoked? (atom false)]
            (hk/as-channel request
              {:on-open
               (fn [channel]
                 (when (compare-and-set! invoked? false true)
                   (try
                     (.submit invoke-executor
                              ^Runnable
                              (fn []
                                (try
                                  (hk/send! channel (build-invoke-response invoke-opts))
                                  (catch Throwable t
                                    (println (str "[invoke] async error: " (.getMessage t)))
                                    (flush)
                                    (hk/send! channel
                                              (json-response 500 {:ok false
                                                                  :error "invoke-error"
                                                                  :message (.getMessage t)}))))))
                     (catch Throwable t
                       (println (str "[invoke] async submit error: " (.getMessage t)))
                       (flush)
                       (hk/send! channel
                                 (json-response 503 {:ok false
                                                     :error "invoke-submit-failed"
                                                     :message (.getMessage t)}))))))}))

          :else
          (try
            (build-invoke-response invoke-opts)
            (catch Throwable t
              (println (str "[invoke] sync error: " (.getMessage t)))
              (flush)
              (json-response 500 {:ok false
                                  :error "invoke-error"
                                  :message (.getMessage t)})))))))

(defn- handle-bell
  "POST /api/alpha/bell — asynchronous fire-and-forget invoke.
   Body: {\"agent-id\":\"codex-1\",\"prompt\":\"...\",\"timeout-ms\":1800000}
   Returns immediately with accepted job-id while execution proceeds on invoke-executor."
  [request config]
  (let [payload (parse-json-map (read-body request))]
    (if (nil? payload)
      (json-response 400 {:ok false :err "invalid-json"
                          :message "Request body must be a JSON object"})
      (let [agent-id (or (:agent-id payload) (get payload "agent-id"))
            prompt (or (:prompt payload) (get payload "prompt"))
            caller (or (some-> payload :caller str)
                       (some-> payload (get "caller") str)
                       "http-caller")
            surface (or (some-> payload :surface str)
                        (some-> payload (get "surface") str)
                        "bell")
            requested-job-id (or (:job-id payload) (get payload "job-id")
                                 (:job_id payload) (get payload "job_id"))
            mission-id (or (:mission-id payload) (get payload "mission-id"))
            timeout-ms (some-> (or (:timeout-ms payload) (get payload "timeout-ms"))
                               long)
            evidence-store (evidence-store-for-config config)]
        (cond
          (or (nil? agent-id) (str/blank? (str agent-id)))
          (json-response 400 {:ok false :err "missing-agent-id"
                              :message "agent-id is required"})

          (nil? prompt)
          (json-response 400 {:ok false :err "missing-prompt"
                              :message "prompt is required"})

          :else
          (let [job-id (create-invoke-job! {:requested-job-id requested-job-id
                                            :agent-id agent-id
                                            :prompt prompt
                                            :caller caller
                                            :surface surface})]
            (try
              (.submit invoke-executor
                       ^Runnable
                       (fn []
                         (let [result (run-invoke-job! {:job-id job-id
                                                        :agent-id agent-id
                                                        :prompt prompt
                                                        :caller caller
                                                        :surface surface
                                                        :timeout-ms timeout-ms
                                                        :mission-id mission-id
                                                        :evidence-store evidence-store})]
                           ;; Bell delivery means caller can obtain terminal result via canonical job query.
                           (record-invoke-job-delivery-by-job-id!
                            job-id
                            {:surface "bell"
                             :destination (str "caller " caller " via /api/alpha/invoke/jobs/" job-id)
                             :delivered? true
                             :note (if (:ok result) "bell-job-ready" "bell-job-error")}))))
              (json-response 202 {:ok true
                                  :accepted true
                                  :job-id job-id
                                  :state "queued"
                                  :mode (invoke-job-mode prompt)
                                  :status-url (str "/api/alpha/invoke/jobs/" job-id)})
              (catch Throwable t
                (finalize-invoke-job! job-id "failed" "invoke-submit-failed" (.getMessage t) {:ok false} nil)
                (json-response 503 {:ok false
                                    :job-id job-id
                                    :error "invoke-submit-failed"
                                    :message (.getMessage t)})))))))))

(defn- handle-invoke-stream
  "POST /api/alpha/invoke-stream — streaming invoke via NDJSON.
   Same request body as /invoke. Returns application/x-ndjson with chunked events:
     {\"type\":\"text\",\"text\":\"...\"}
     {\"type\":\"tool_use\",\"tools\":[\"Read\"]}
     {\"type\":\"done\",\"ok\":true,\"result\":\"...\",\"session-id\":\"...\"}
   The event sink is installed on the agent registry so the NDJSON parse loop
   in make-claude-invoke-fn emits events as they arrive."
  [request config]
  (let [payload (parse-json-map (read-body request))]
    (if (nil? payload)
      (json-response 400 {:ok false :err "invalid-json"
                          :message "Request body must be a JSON object"})
      (let [agent-id (or (:agent-id payload) (get payload "agent-id"))
            prompt (or (:prompt payload) (get payload "prompt"))]
        (cond
          (or (nil? agent-id) (str/blank? (str agent-id)))
          (json-response 400 {:ok false :err "missing-agent-id"
                              :message "agent-id is required"})

          (nil? prompt)
          (json-response 400 {:ok false :err "missing-prompt"
                              :message "prompt is required"})

          :else
          (hk/with-channel request channel
            ;; Send initial response with a keepalive comment to start chunked stream
            (hk/send! channel
              {:status 200
               :headers {"Content-Type" "application/x-ndjson"
                         "Cache-Control" "no-cache"
                         "X-Accel-Buffering" "no"}
               :body (str (json/generate-string {:type "started"}) "\n")}
              false)
            (let [aid (str agent-id)
                  ;; Create sink-fn that writes NDJSON lines to the channel
                  sink-fn (fn [event]
                            (try
                              (hk/send! channel
                                (str (json/generate-string event) "\n")
                                false)
                              (catch Throwable _)))]
              ;; Install event sink on the agent
              (reg/set-invoke-event-sink! aid sink-fn)
              ;; Clean up on client disconnect
              (hk/on-close channel
                (fn [_status]
                  (reg/clear-invoke-event-sink! aid)))
              ;; Run invoke on executor thread
              (.submit invoke-executor
                ^Runnable
                (fn []
                  (try
                    (let [caller (or (some-> payload :caller str)
                                     (some-> payload (get "caller") str)
                                     "http-caller")
                          surface (or (some-> payload :surface str)
                                      (some-> payload (get "surface") str))
                          mission-id (or (:mission-id payload) (get payload "mission-id"))
                          timeout-ms (some-> (or (:timeout-ms payload) (get payload "timeout-ms"))
                                             long)
                          evidence-store (evidence-store-for-config config)
                          ev-opts (when mission-id [:mission-id mission-id])
                          effective-prompt (wrap-surface-header prompt surface caller)
                          result (reg/invoke-agent! (str agent-id) effective-prompt timeout-ms)
                          sid (:session-id result)]
                      ;; Emit evidence (same as handle-invoke)
                      (apply emit-invoke-evidence! evidence-store caller (str prompt) sid
                             (or ev-opts []))
                      (if (:ok result)
                        (do
                          (apply emit-invoke-evidence! evidence-store (str agent-id) (str (:result result)) sid
                                 (or ev-opts []))
                          ;; Send done event and close
                          (try
                            (hk/send! channel
                              (str (json/generate-string
                                     (cond-> {:type "done"
                                              :ok true
                                              :result (:result result)
                                              :session-id sid}
                                       (:invoke-meta result)
                                       (assoc :invoke-meta (:invoke-meta result))))
                                   "\n")
                              false)
                            (catch Throwable _)))
                        (let [err (:error result)
                              code (if (map? err) (:error/code err) :invoke-failed)
                              msg (if (map? err) (:error/message err) (str err))]
                          (try
                            (hk/send! channel
                              (str (json/generate-string
                                     {:type "done"
                                      :ok false
                                      :error (name code)
                                      :message msg})
                                   "\n")
                              false)
                            (catch Throwable _)))))
                    (catch Throwable t
                      (try
                        (hk/send! channel
                          (str (json/generate-string
                                 {:type "done"
                                  :ok false
                                  :error "invoke-error"
                                  :message (.getMessage t)})
                               "\n")
                          false)
                        (catch Throwable _)))
                    (finally
                      (reg/clear-invoke-event-sink! aid)
                      (hk/close channel))))))))))))

(defn- invoke-job-terminal-state?
  [state]
  (not (#{"queued" "running"} (str state))))

(defn- stream-flag?
  [payload]
  (let [v (or (:stream payload) (get payload "stream"))]
    (cond
      (boolean? v) v
      (string? v) (true? (parse-bool v))
      :else false)))

(defn- send-ndjson!
  [channel event]
  (try
    (hk/send! channel (str (json/generate-string event) "\n") false)
    true
    (catch Throwable _
      false)))

(defn- sanitize-ms
  [v default-ms min-ms max-ms]
  (let [n (cond
            (integer? v) (long v)
            (number? v) (long v)
            (string? v) (parse-long v)
            :else nil)]
    (if (and (number? n) (pos? n))
      (max min-ms (min max-ms n))
      default-ms)))

(defn- handle-whistle-stream*
  "Long-running modem-style whistle with progressive NDJSON updates.
   Emits: started, job-event, heartbeat, done."
  [request config payload]
  (let [agent-id (or (:agent-id payload) (get payload "agent-id"))
        prompt (or (:prompt payload) (get payload "prompt"))
        timeout-ms (some-> (or (:timeout-ms payload) (get payload "timeout-ms")) long)
        caller (or (some-> payload :caller str)
                   (some-> payload (get "caller") str)
                   "http-caller")
        requested-job-id (or (:job-id payload) (get payload "job-id")
                             (:job_id payload) (get payload "job_id"))
        mission-id (or (:mission-id payload) (get payload "mission-id"))
        poll-ms (sanitize-ms (or (:poll-ms payload) (get payload "poll-ms")) 1000 100 10000)
        heartbeat-ms (sanitize-ms (or (:heartbeat-ms payload) (get payload "heartbeat-ms")) 5000 500 60000)
        evidence-store (evidence-store-for-config config)]
    (cond
      (or (nil? agent-id) (str/blank? (str agent-id)))
      (json-response 400 {:ok false :err "missing-agent-id"
                          :message "agent-id is required"})

      (nil? prompt)
      (json-response 400 {:ok false :err "missing-prompt"
                          :message "prompt is required"})

      :else
      (let [job-id (create-invoke-job! {:requested-job-id requested-job-id
                                        :agent-id agent-id
                                        :prompt prompt
                                        :caller caller
                                        :surface "whistle"})
            mode (invoke-job-mode prompt)
            started-ms (System/currentTimeMillis)]
        (hk/with-channel request channel
          (let [closed? (atom false)
                delivery-recorded? (atom false)
                mark-delivery!
                (fn [delivered? note]
                  (when (compare-and-set! delivery-recorded? false true)
                    (record-invoke-job-delivery-by-job-id!
                     job-id
                     {:surface "whistle-stream"
                      :destination (str "caller " caller " (stream)")
                      :delivered? (boolean delivered?)
                      :note (str note)})))
                close-channel!
                (fn []
                  (try
                    (hk/close channel)
                    (catch Throwable _)))]
            (hk/on-close channel
              (fn [_]
                (reset! closed? true)
                (mark-delivery! false "whistle-stream-client-closed")))
            (when-not (send-ndjson! channel
                                    {:type "started"
                                     :ok true
                                     :job-id job-id
                                     :state "queued"
                                     :mode mode
                                     :poll-ms poll-ms
                                     :heartbeat-ms heartbeat-ms
                                     :status-url (str "/api/alpha/invoke/jobs/" job-id)})
              (mark-delivery! false "whistle-stream-start-send-failed"))
            (.submit invoke-executor
                     ^Runnable
                     (fn []
                       (run-invoke-job! {:job-id job-id
                                         :agent-id agent-id
                                         :prompt prompt
                                         :caller caller
                                         :surface "whistle"
                                         :timeout-ms timeout-ms
                                         :mission-id mission-id
                                         :evidence-store evidence-store})))
            (.submit invoke-executor
                     ^Runnable
                     (fn []
                       (loop [last-seq 0
                              last-heartbeat-ms started-ms]
                         (if @closed?
                           (close-channel!)
                           (let [job (get-invoke-job job-id)
                                 public-job (when job (invoke-job-public-view job))
                                 state (or (:state public-job) "unknown")
                                 events (:events public-job)
                                 new-events (->> events
                                                 (filter #(> (long (or (:seq %) 0)) (long last-seq)))
                                                 vec)
                                 next-seq (if (seq new-events)
                                            (long (or (:seq (last new-events)) last-seq))
                                            (long last-seq))]
                             (doseq [evt new-events]
                               (when-not (send-ndjson! channel
                                                       {:type "job-event"
                                                        :job-id job-id
                                                        :state state
                                                        :event evt})
                                 (reset! closed? true)))
                             (let [now (System/currentTimeMillis)
                                   terminal? (invoke-job-terminal-state? state)
                                   heartbeat-due? (and (not terminal?)
                                                       (>= (- now last-heartbeat-ms) heartbeat-ms))
                                   sent-heartbeat? (when heartbeat-due?
                                                     (send-ndjson!
                                                      channel
                                                      {:type "heartbeat"
                                                       :job-id job-id
                                                       :state state
                                                       :elapsed-ms (- now started-ms)
                                                       :last-event-seq next-seq}))
                                   next-heartbeat-ms (if heartbeat-due? now last-heartbeat-ms)]
                               (cond
                                 terminal?
                                 (do
                                   (if (send-ndjson! channel
                                                     {:type "done"
                                                      :ok (= "done" state)
                                                      :job-id job-id
                                                      :job public-job})
                                     (mark-delivery! true "whistle-stream-response")
                                     (mark-delivery! false "whistle-stream-send-failed"))
                                   (close-channel!))

                                 (false? sent-heartbeat?)
                                 (do
                                   (reset! closed? true)
                                   (mark-delivery! false "whistle-stream-heartbeat-send-failed")
                                   (close-channel!))

                                 :else
                                 (do
                                   (Thread/sleep poll-ms)
                                   (recur next-seq next-heartbeat-ms)))))))))))))
        ))

(defn- handle-whistle-stream
  "POST /api/alpha/whistle-stream — NDJSON streaming whistle endpoint."
  [request config]
  (let [payload (parse-json-map (read-body request))]
    (if (nil? payload)
      (json-response 400 {:ok false :err "invalid-json"
                          :message "Request body must be a JSON object"})
      (handle-whistle-stream* request config payload))))

(defn- handle-whistle
  "POST /api/alpha/whistle — synchronous request-response to a registered agent.
   Body: {\"agent-id\": \"codex-1\", \"prompt\": \"...\", \"timeout-ms\": 1800000}
   Delegates to whistles/whistle! which wraps invoke-agent! with evidence."
  [request config]
  (let [payload (parse-json-map (read-body request))]
    (if (nil? payload)
      (json-response 400 {:ok false :err "invalid-json"
                          :message "Request body must be a JSON object"})
      (let [agent-id (or (:agent-id payload) (get payload "agent-id"))
            prompt (or (:prompt payload) (get payload "prompt"))
            timeout-ms (some-> (or (:timeout-ms payload) (get payload "timeout-ms"))
                               long)
            caller (or (some-> payload :caller str)
                       (some-> payload (get "caller") str)
                       "http-caller")
            evidence-store (evidence-store-for-config config)]
        (if (stream-flag? payload)
          (handle-whistle-stream* request config payload)
          (cond
            (or (nil? agent-id) (str/blank? (str agent-id)))
            (json-response 400 {:ok false :err "missing-agent-id"
                                :message "agent-id is required"})

            (nil? prompt)
            (json-response 400 {:ok false :err "missing-prompt"
                                :message "prompt is required"})

            :else
            (hk/with-channel request channel
              (.submit invoke-executor
                       ^Runnable
                       (fn []
                         (try
                           (let [result (whistles/whistle!
                                         {:agent-id (str agent-id)
                                          :prompt prompt
                                          :author caller
                                          :timeout-ms timeout-ms
                                          :evidence-store evidence-store})]
                             (if (:whistle/ok result)
                               (hk/send! channel
                                         (json-response 200 (cond-> {:ok true
                                                                     :response (:whistle/response result)
                                                                     :agent-id (:whistle/agent-id result)
                                                                     :session-id (:whistle/session-id result)}
                                                              (:whistle/invoke-trace-id result)
                                                              (assoc :invoke-trace-id (:whistle/invoke-trace-id result)))))
                               (let [err (:whistle/error result)]
                                 (hk/send! channel
                                           (json-response
                                            (if (and (string? err) (.contains ^String err "not registered")) 404 502)
                                            (cond-> {:ok false
                                                     :error err
                                                     :agent-id (:whistle/agent-id result)}
                                              (:whistle/invoke-trace-id result)
                                              (assoc :invoke-trace-id (:whistle/invoke-trace-id result))))))))
                           (catch Throwable t
                             (println (str "[whistle] async error: " (.getMessage t)))
                             (flush)
                             (hk/send! channel
                                       (json-response 500 {:ok false
                                                           :error "whistle-error"
                                                           :message (.getMessage t)})))))))))))))

(defn- handle-irc-send
  "POST /api/alpha/irc/send — send a one-line IRC message via configured relay.
   Body: {\"channel\": \"#futon\", \"text\": \"...\", \"from\": \"codex\"}."
  [request config]
  (let [payload (parse-json-map (read-body request))
        send-fn (:irc-send-fn config)]
    (cond
      (nil? payload)
      (json-response 400 {:ok false :err "invalid-json"
                          :message "Request body must be a JSON object"})

      (not (fn? send-fn))
      (json-response 503 {:ok false :err "irc-unavailable"
                          :message "IRC relay is not configured on this node"})

      :else
      (let [channel (or (:channel payload) (get payload "channel"))
            text (or (:text payload) (get payload "text"))
            from (or (:from payload) (get payload "from") "codex")]
        (cond
          (or (nil? channel) (str/blank? (str channel)))
          (json-response 400 {:ok false :err "missing-channel"
                              :message "channel is required"})

          (or (nil? text) (str/blank? (str text)))
          (json-response 400 {:ok false :err "missing-text"
                              :message "text is required"})

          :else
          (try
            (send-fn (str channel) (str from) (str text))
            (json-response 200 {:ok true
                                :channel (str channel)
                                :from (str from)
                                :text (str text)})
            (catch Exception e
              (json-response 502 {:ok false :err "irc-send-failed"
                                  :message (.getMessage e)}))))))))

(defn- handle-invoke-jobs
  "GET /api/alpha/invoke/jobs?limit=N — list recent invoke jobs."
  [request]
  (let [params (parse-query-params request)
        limit (or (parse-int (get params "limit")) 20)
        jobs (->> (recent-invoke-jobs limit)
                  (mapv invoke-job-public-view))]
    (json-response 200 {:ok true
                        :count (count jobs)
                        :jobs jobs})))

(defn- handle-invoke-job
  "GET /api/alpha/invoke/jobs/:id — return one invoke-job snapshot."
  [job-id]
  (if-let [job (get-invoke-job job-id)]
    (json-response 200 {:ok true
                        :job (invoke-job-public-view job)})
    (json-response 404 {:ok false
                        :error "invoke-job-not-found"
                        :job-id (str job-id)})))

(defn- handle-invoke-delivery
  "POST /api/alpha/invoke-delivery — record where an invoke reply was delivered.
   Body: {\"agent-id\":\"codex-1\",\"invoke-trace-id\":\"invoke-...\",\"surface\":\"irc\",
          \"destination\":\"#futon as <codex>\",\"delivered\":true,\"note\":\"...\"}."
  [request _config]
  (let [payload (parse-json-map (read-body request))]
    (if (nil? payload)
      (json-response 400 {:ok false :err "invalid-json"
                          :message "Request body must be a JSON object"})
      (let [agent-id (or (:agent-id payload) (get payload "agent-id"))
            invoke-trace-id (or (:invoke-trace-id payload) (get payload "invoke-trace-id")
                                (:invoke_trace_id payload) (get payload "invoke_trace_id"))
            surface (or (:surface payload) (get payload "surface"))
            destination (or (:destination payload) (get payload "destination"))
            note (or (:note payload) (get payload "note"))
            delivered (let [v (or (:delivered payload) (get payload "delivered"))]
                        (cond
                          (boolean? v) v
                          (string? v) (if-some [b (parse-bool v)] b true)
                          :else true))
            receipt {:surface (str (or surface "unknown"))
                     :destination (str (or destination "unknown"))
                     :delivered? delivered
                     :note (str (or note ""))}]
        (cond
          (or (nil? agent-id) (str/blank? (str agent-id)))
          (json-response 400 {:ok false :err "missing-agent-id"
                              :message "agent-id is required"})

          (or (nil? invoke-trace-id) (str/blank? (str invoke-trace-id)))
          (json-response 400 {:ok false :err "missing-invoke-trace-id"
                              :message "invoke-trace-id is required"})

          :else
          (do
            (record-invoke-job-delivery! (str invoke-trace-id) receipt)
            (if-let [record-fn (*resolve-delivery-recorder*)]
            (try
              (let [recorded? (boolean
                               (record-fn (str agent-id) (str invoke-trace-id)
                                          receipt))]
                (if recorded?
                  (json-response 200 {:ok true
                                      :agent-id (str agent-id)
                                      :invoke-trace-id (str invoke-trace-id)
                                      :recorded true})
                  (json-response 502 {:ok false
                                      :err "invoke-delivery-record-failed"
                                      :message "delivery receipt could not be written"})))
              (catch Throwable t
                (json-response 502 {:ok false
                                    :err "invoke-delivery-record-failed"
                                    :message (.getMessage t)})))
            (json-response 503 {:ok false
                                :err "invoke-delivery-unavailable"
                                :message "invoke delivery recorder not available on this node"}))))))))

(defn- handle-irc-history
  "GET /api/alpha/irc/history — recent IRC messages in chat-friendly format.
   Query params: channel (default #futon), limit (default 50, max 200), since (ISO)."
  [request config]
  (let [params (parse-query-params request)
        channel (or (non-blank-string (get params "channel")) "#futon")
        limit (min (or (parse-int (get params "limit")) 50) 200)
        since (non-blank-string (get params "since"))
        evidence-store (evidence-store-for-config config)
        query (cond-> {:query/subject {:ref/type :thread
                                       :ref/id (str "irc/" channel)}
                       :query/type :forum-post
                       :query/limit limit}
                since (assoc :query/since since))
        entries (estore/query* evidence-store query)
        messages (mapv (fn [e]
                         {:nick (:evidence/author e)
                          :text (get-in e [:evidence/body :text])
                          :at (:evidence/at e)
                          :channel (get-in e [:evidence/body :channel])})
                       entries)]
    (json-response 200 {:ok true
                        :channel channel
                        :count (count messages)
                        :messages messages})))

(defn- handle-agents-list
  "GET /api/alpha/agents — list all registered agents."
  [_config]
  (let [status (reg/registry-status)]
    (json-response 200 {:ok true
                        :count (:count status)
                        :agents (:agents status)})))

(defn- handle-agent-get
  "GET /api/alpha/agents/:id — return a single agent's details."
  [_config agent-id]
  (let [status (reg/registry-status)
        agent (get-in status [:agents agent-id])]
    (if agent
      (json-response 200 {:ok true :agent-id agent-id :agent agent})
      (json-response 404 {:ok false :error (str "Agent not found: " agent-id)}))))

(defn- handle-agent-delete
  "DELETE /api/alpha/agents/:id — deregister an agent."
  [_config agent-id]
  (let [result (reg/unregister-agent! agent-id)]
    (if (:ok result)
      (json-response 200 {:ok true :agent-id agent-id :deregistered true})
      (let [err (:error result)
            code (if (map? err) (:error/code err) :deregister-failed)]
        (json-response (if (= :agent-not-found code) 404 502)
                       {:ok false
                        :error (if (= :agent-not-found code)
                                 (str "Agent not found: " agent-id)
                                 (if (map? err)
                                   (:error/message err)
                                   (str err)))})))))

(defn- handle-agent-reset-session
  "POST /api/alpha/agents/:id/reset-session — clear an agent's session so the
   next invoke starts a fresh conversation. Useful when a session becomes
   poisoned (e.g. invalid tool-use in conversation history)."
  [_config agent-id]
  (let [result (reg/reset-session! agent-id)]
    (if (:ok result)
      (json-response 200 {:ok true
                          :agent-id (:agent-id result)
                          :old-session-id (:old-session-id result)})
      (let [err (:error result)
            code (if (map? err) (:error/code err) :reset-failed)]
        (json-response (if (= :agent-not-found code) 404 502)
                       {:ok false
                        :error (if (= :agent-not-found code)
                                 (str "Agent not found: " agent-id)
                                 (if (map? err)
                                   (:error/message err)
                                   (str err)))})))))

;; =============================================================================
;; CYDER process endpoints
;; =============================================================================

(defn- handle-processes-list
  "GET /api/alpha/processes — list all registered processes."
  [_config]
  (let [status (cyder/registry-status)]
    (json-response 200 {:ok true
                        :count (:count status)
                        :processes (:processes status)})))

(defn- handle-process-get
  "GET /api/alpha/processes/:id — inspect a single process."
  [_config process-id]
  (let [result (cyder/inspect process-id)]
    (if (:ok result)
      (json-response 200 result)
      (json-response 404 result))))

(defn- handle-process-delete
  "DELETE /api/alpha/processes/:id — stop and deregister a process."
  [_config process-id]
  (let [result (cyder/stop! process-id)]
    (if (:ok result)
      (json-response 200 {:ok true :id process-id :stopped true})
      (json-response 404 result))))

(defn- handle-process-step
  "POST /api/alpha/processes/:id/step — single-step a REPL-like process."
  [_config process-id]
  (let [result (cyder/step! process-id)]
    (if (:ok result)
      (json-response 200 result)
      (json-response (if (re-find #"not registered" (str (:error result))) 404 400)
                     result))))

;; =============================================================================
;; Mission-control endpoints
;; =============================================================================

(defn- compute-portfolio-diff
  "Compute diff between two portfolio snapshots (newest first).
   Returns {:added [...] :removed [...] :changed [...] :summary {...}}."
  [new-snapshot old-snapshot]
  (let [new-missions (:portfolio/missions (:evidence/body new-snapshot))
        old-missions (:portfolio/missions (:evidence/body old-snapshot))
        new-by-id (into {} (map (juxt :mission/id identity)) new-missions)
        old-by-id (into {} (map (juxt :mission/id identity)) old-missions)
        new-ids (set (keys new-by-id))
        old-ids (set (keys old-by-id))
        added (vec (for [id (sort (cset/difference new-ids old-ids))]
                     (get new-by-id id)))
        removed (vec (for [id (sort (cset/difference old-ids new-ids))]
                       (get old-by-id id)))
        changed (vec (for [id (sort (cset/intersection new-ids old-ids))
                           :let [new-status (:mission/status (get new-by-id id))
                                 old-status (:mission/status (get old-by-id id))]
                           :when (not= new-status old-status)]
                       {:mission/id id
                        :old-status old-status
                        :new-status new-status}))
        new-summary (:portfolio/summary (:evidence/body new-snapshot))
        old-summary (:portfolio/summary (:evidence/body old-snapshot))
        new-coverage (:portfolio/coverage (:evidence/body new-snapshot))
        old-coverage (:portfolio/coverage (:evidence/body old-snapshot))]
    {:added added
     :removed removed
     :changed changed
     :new-count (count new-missions)
     :old-count (count old-missions)
     :new-summary new-summary
     :old-summary old-summary
     :new-coverage new-coverage
     :old-coverage old-coverage}))

(defn- handle-mission-control
  "POST /api/alpha/mission-control — multiplexed mission-control RPC.
   Actions: review, status, sessions, step, start, stop, diff.
   Wraps futon3c.mission-control.service functions."
  [request config]
  (let [payload (parse-json-map (read-body request))]
    (if (nil? payload)
      (json-response 400 {:ok false :err "invalid-json"
                          :message "Request body must be a JSON object"})
      (let [action (or (:action payload) (get payload "action"))
            evidence-store (evidence-store-for-config config)]
        (case (str action)
          "review"
          (let [author (or (:author payload) (get payload "author"))
                session-id (or (:session-id payload) (get payload "session-id"))
                close? (boolean (or (:close payload) (get payload "close")))
                result (mcs/run-review!
                        {:session-id session-id
                         :author author
                         :close? close?})]
            ;; Emit portfolio snapshot on successful review
            (when (and (:ok result) evidence-store)
              (let [lr (:last-result result)
                    review-data (if (map? lr) (:result lr) {})]
                (when (map? review-data)
                  (emit-review-snapshot! evidence-store author review-data))))
            (json-response 200 result))

          "diff"
          (if-not evidence-store
            (json-response 500 {:ok false :err "no-evidence-store"
                                :message "Evidence store not configured"})
            (let [snapshots (estore/query* evidence-store
                                          {:query/subject {:ref/type :portfolio
                                                           :ref/id "global"}
                                           :query/type :coordination
                                           :query/limit 100})
                  ;; Filter to portfolio-snapshot tagged entries
                  snapshots (->> snapshots
                                 (filter (fn [e]
                                           (let [tags (set (:evidence/tags e))]
                                             (and (contains? tags :review)
                                                  (contains? tags :portfolio-snapshot)))))
                                 (take 2)
                                 vec)]
              (if (< (count snapshots) 2)
                (json-response 200 {:ok true
                                    :diff nil
                                    :message "Not enough review history — run !mc review at least twice"})
                (let [diff (compute-portfolio-diff (first snapshots) (second snapshots))]
                  (json-response 200 {:ok true :diff diff})))))

          "status"
          (json-response 200 (mcs/status))

          "sessions"
          (json-response 200 {:ok true :sessions (mcs/list-sessions)})

          "step"
          (let [session-id (or (:session-id payload) (get payload "session-id"))
                tool (parse-keyword (or (:tool payload) (get payload "tool")))
                args (or (:args payload) (get payload "args") [])]
            (if (or (nil? session-id) (nil? tool))
              (json-response 400 {:ok false :err "missing-params"
                                  :message "step requires session-id and tool"})
              (json-response 200 (mcs/step! session-id tool (vec args)))))

          "start"
          (let [session-id (or (:session-id payload) (get payload "session-id"))
                author (or (:author payload) (get payload "author"))]
            (json-response 200 (mcs/start-session!
                                (cond-> {}
                                  session-id (assoc :session-id session-id)
                                  author (assoc :author author)))))

          "stop"
          (let [session-id (or (:session-id payload) (get payload "session-id"))
                reason (or (:reason payload) (get payload "reason") "stopped via API")]
            (if (nil? session-id)
              (json-response 400 {:ok false :err "missing-session-id"
                                  :message "stop requires session-id"})
              (json-response 200 (mcs/stop-session! session-id reason))))

          ;; default
          (json-response 400 {:ok false :err "unknown-action"
                              :message (str "Unknown action: " action
                                            ". Valid: review, status, sessions, step, start, stop, diff")}))))))

(defn- load-mission-wiring-edn
  "Load a per-mission wiring diagram from holes/missions/.
   Convention: M-foo → foo-wiring.edn or M-foo-wiring.edn"
  [mission-id]
  (let [cwd (System/getProperty "user.dir")
        ;; Try {name}-wiring.edn (strip M- prefix)
        short-name (str/replace (str mission-id) #"^M-" "")
        candidates [(io/file cwd "holes" "missions" (str short-name "-wiring.edn"))
                    (io/file cwd "holes" "missions" (str "M-" short-name "-wiring.edn"))
                    (io/file cwd "holes" "missions" (str mission-id "-wiring.edn"))]]
    (some (fn [^java.io.File f]
            (when (.exists f)
              (try (edn/read-string (slurp f))
                   (catch Exception _ nil))))
          candidates)))

(defn- handle-missions
  "GET /api/alpha/missions — cross-repo mission inventory."
  [_request _config]
  (let [missions (mcb/build-inventory)]
    (json-response 200 {:ok true
                        :missions missions
                        :count (count missions)})))

(defn- handle-mission-detail
  "GET /api/alpha/missions/:id — single mission info + wiring diagram."
  [_config mission-id]
  (let [missions (mcb/build-inventory)
        mission (first (filter #(= mission-id (:mission/id %)) missions))
        wiring (load-mission-wiring-edn mission-id)]
    (if mission
      (json-response 200 {:ok true
                          :mission mission
                          :wiring wiring})
      (json-response 404 {:ok false
                          :err "mission-not-found"
                          :message (str "No mission found: " mission-id)}))))

(defn- handle-mission-wiring
  "GET /api/alpha/missions/:id/wiring — per-mission wiring diagram."
  [_config mission-id]
  (let [wiring (load-mission-wiring-edn mission-id)]
    (if wiring
      (json-response 200 {:ok true :wiring wiring})
      (json-response 404 {:ok false
                          :err "not-found"
                          :message (str "No wiring diagram for mission " mission-id)}))))

;; =============================================================================
;; Todo endpoints — lightweight task management via evidence entries
;; =============================================================================

(defn- handle-todo
  "POST /api/alpha/todo — lightweight todo management.
   Actions: add, list, done.
   Todos are stored as evidence entries with tags [:todo :pending]."
  [request config]
  (let [payload (parse-json-map (read-body request))]
    (if (nil? payload)
      (json-response 400 {:ok false :err "invalid-json"
                          :message "Request body must be a JSON object"})
      (let [action (or (:action payload) (get payload "action"))
            evidence-store (evidence-store-for-config config)]
        (case (str action)
          "add"
          (let [text (or (:text payload) (get payload "text"))
                author (or (:author payload) (get payload "author") "anonymous")
                todo-id (str "todo-" (java.util.UUID/randomUUID))
                entry {:subject {:ref/type :task :ref/id todo-id}
                       :type :coordination
                       :claim-type :goal
                       :author author
                       :body {:text text}
                       :tags [:todo :pending]}
                result (estore/append* evidence-store entry)]
            (if (and (map? result) (:evidence/id result))
              (json-response 201 {:ok true
                                  :id todo-id
                                  :evidence-id (:evidence/id result)
                                  :text text})
              (if (:ok result)
                (json-response 201 {:ok true
                                    :id todo-id
                                    :evidence-id (get-in result [:entry :evidence/id])
                                    :text text})
                (json-response 400 {:ok false :err "append-failed"
                                    :error result}))))

          "list"
          (let [author (or (:author payload) (get payload "author"))
                all-goals (estore/query* evidence-store {:query/claim-type :goal})
                pending (->> all-goals
                             (filter (fn [e]
                                       (let [tags (set (:evidence/tags e))]
                                         (and (contains? tags :todo)
                                              (contains? tags :pending)))))
                             (filter (fn [e]
                                       (or (nil? author)
                                           (= author (:evidence/author e))))))
                done-ids (->> (estore/query* evidence-store {:query/claim-type :conclusion})
                              (filter (fn [e]
                                        (let [tags (set (:evidence/tags e))]
                                          (and (contains? tags :todo)
                                               (contains? tags :done)))))
                              (map (fn [e] (get-in e [:evidence/subject :ref/id])))
                              set)
                todos (->> pending
                           (remove (fn [e] (done-ids (get-in e [:evidence/subject :ref/id]))))
                           (map (fn [e]
                                  {:id (get-in e [:evidence/subject :ref/id])
                                   :text (get-in e [:evidence/body :text])
                                   :author (:evidence/author e)
                                   :at (:evidence/at e)}))
                           (sort-by :at)
                           vec)]
            (json-response 200 {:ok true :todos todos :count (count todos)}))

          "done"
          (let [todo-id (or (:id payload) (get payload "id"))
                author (or (:author payload) (get payload "author") "anonymous")]
            (if (or (nil? todo-id) (str/blank? (str todo-id)))
              (json-response 400 {:ok false :err "missing-id"
                                  :message "done requires id"})
              (let [entry {:subject {:ref/type :task :ref/id (str todo-id)}
                           :type :coordination
                           :claim-type :conclusion
                           :author author
                           :body {:completed true}
                           :tags [:todo :done]}
                    result (estore/append* evidence-store entry)]
                (if (or (:ok result) (:evidence/id result))
                  (json-response 200 {:ok true :id (str todo-id)})
                  (json-response 400 {:ok false :err "append-failed"
                                      :error result})))))

          ;; default
          (json-response 400 {:ok false :err "unknown-action"
                              :message (str "Unknown action: " action ". Valid: add, list, done")}))))))

;; =============================================================================
;; Reflection endpoints — Clojure runtime introspection
;; =============================================================================

(defn- handle-reflect-namespaces
  "GET /api/alpha/reflect/namespaces — list loaded namespaces."
  [request]
  (let [pattern (get-in request [:query-params "pattern"])
        result (if pattern
                 (reflection/list-namespaces pattern)
                 (reflection/list-namespaces))]
    (json-response 200 {:ok true :namespaces result :count (count result)})))

(defn- handle-reflect-ns
  "GET /api/alpha/reflect/ns/:ns — public vars in a namespace."
  [ns-str]
  (let [ns-sym (symbol ns-str)
        result (reflection/reflect-ns ns-sym)]
    (if (:error result)
      (json-response 404 {:ok false :error (:error result)})
      (json-response 200 {:ok true :vars result :count (count result)}))))

(defn- handle-reflect-ns-full
  "GET /api/alpha/reflect/ns/:ns/full — all vars (public + private)."
  [ns-str]
  (let [ns-sym (symbol ns-str)
        result (reflection/reflect-ns-full ns-sym)]
    (if (:error result)
      (json-response 404 {:ok false :error (:error result)})
      (json-response 200 {:ok true :vars result :count (count result)}))))

(defn- handle-reflect-var
  "GET /api/alpha/reflect/var/:ns/:var — full metadata for one var."
  [ns-str var-str]
  (let [ns-sym (symbol ns-str)
        var-sym (symbol var-str)
        result (reflection/reflect-var ns-sym var-sym)]
    (if (:error result)
      (json-response 404 {:ok false :error (:error result)})
      (json-response 200 {:ok true :envelope result}))))

(defn- handle-reflect-deps
  "GET /api/alpha/reflect/deps/:ns — namespace dependency graph."
  [ns-str]
  (let [ns-sym (symbol ns-str)
        result (reflection/reflect-deps ns-sym)]
    (if (:error result)
      (json-response 404 {:ok false :error (:error result)})
      (json-response 200 {:ok true :deps result}))))

(defn- handle-reflect-java-class
  "GET /api/alpha/reflect/java/:class — reflect on a Java class."
  [class-name]
  (let [result (reflection/reflect-java-class class-name)]
    (if (:error result)
      (json-response 404 {:ok false :error (:error result)})
      (json-response 200 {:ok true :class result}))))

;; =============================================================================
;; Enrichment endpoint
;; =============================================================================

(defn- handle-enrich-file
  "GET /api/alpha/enrich/file?path=... — composite enrichment for a source file.
   Queries futon1a hyperedge store for all enrichment layers and returns
   missions, patterns, evidence counts, tensions, and deps per symbol."
  [request config]
  (let [params (parse-query-params request)
        path (or (get params "path") (get params :path))]
    (if (or (nil? path) (str/blank? (str path)))
      (json-response 400 {:ok false :error "missing-path"
                           :message "path query parameter is required"})
      (let [futon1a-url (or (System/getenv "FUTON1A_URL") "http://localhost:7071")
            result (enrich/enrich-file (str path) {:futon1a-url futon1a-url})]
        (json-response 200 (assoc result :ok true))))))

;; =============================================================================
;; Mission control data endpoints
;; =============================================================================

(defn- handle-mc-backfill
  "POST /api/alpha/mc/backfill — backfill per-mission evidence entries.
   Creates one evidence entry per scanned mission, tagged [:mission :backfill :snapshot].
   Idempotent: skips entries whose IDs already exist."
  [config]
  (let [evidence-store (evidence-store-for-config config)]
    (if-not evidence-store
      (json-response 500 {:ok false :error "no-evidence-store"
                          :message "Evidence store not configured"})
      (try
        (let [entries (mcb/backfill-inventory)
              results (mapv (fn [entry]
                              (let [result (estore/append* evidence-store entry)]
                                {:id (:evidence/id entry)
                                 :ok (boolean (:ok result))
                                 :skipped (= :duplicate-id (:error/code result))}))
                            entries)
              created (count (filter :ok results))
              skipped (count (filter :skipped results))]
          (json-response 200
            {:ok true
             :count (count entries)
             :created created
             :skipped skipped
             :sample (vec (take 3 entries))}))
        (catch Exception e
          (json-response 500 {:ok false :error "backfill-failed"
                              :message (.getMessage e)}))))))

(defn- handle-mc-tensions
  "GET /api/alpha/mc/tensions — export structured tension data.
   Returns typed tension entries pre-shaped for hyperedge creation."
  [config]
  (try
    (let [result (mcb/build-tension-export)]
      (json-response 200 {:ok true
                          :tensions (:tensions result)
                          :detected-at (:detected-at result)
                          :summary (:summary result)}))
    (catch Exception e
      (json-response 500 {:ok false :error "tension-export-failed"
                          :message (.getMessage e)}))))

(defn- handle-mc-devmaps
  "GET /api/alpha/mc/devmaps — export devmap structural summaries.
   Returns all devmap prototypes with component/edge/port counts."
  [_config]
  (try
    (let [devmaps (mcb/read-all-devmaps
                   (get mcb/default-repo-roots :futon5)
                   mcb/default-repo-roots)]
      (json-response 200 {:ok true
                          :devmaps devmaps
                          :count (count devmaps)}))
    (catch Exception e
      (json-response 500 {:ok false :error "devmap-export-failed"
                          :message (.getMessage e)}))))

(defn- handle-mc-trace
  "GET /api/alpha/mc/trace — trace all tensions through the gate chain.
   Returns per-tension paths with gate results and aggregate stats.
   Optional ?devmap=X to filter to a single devmap."
  [request _config]
  (try
    (let [params (parse-query-params request)
          dm-filter (get params "devmap")
          result (mcb/trace-all-tensions)
          paths (if dm-filter
                  (filterv (fn [p]
                             (let [dm (get-in p [:tension :tension/devmap])]
                               (= dm-filter (when dm (name dm)))))
                           (:paths result))
                  (:paths result))]
      (json-response 200 {:ok true
                          :paths paths
                          :summary (if dm-filter
                                     {:total (count paths)
                                      :filter dm-filter}
                                     (:summary result))
                          :detected-at (:detected-at result)}))
    (catch Exception e
      (json-response 500 {:ok false :error "trace-failed"
                          :message (.getMessage e)}))))

;; =============================================================================
;; Portfolio inference handlers
;; =============================================================================

(defn- handle-portfolio-step
  "POST /api/alpha/portfolio/step — run one AIF step, return recommendation."
  [request config]
  (let [evidence-store (evidence-store-for-config config)
        payload (or (parse-json-map (read-body request)) {})
        opts (cond-> {}
               (:emit-evidence payload)
               (assoc :emit-evidence? (boolean (:emit-evidence payload))))]
    (try
      (let [result (portfolio/portfolio-step! evidence-store opts)]
        (json-response 200
          {:ok true
           :recommendation (portfolio/format-recommendation result)
           :action (some-> (:action result) name)
           :diagnostics (:diagnostics result)
           :abstain (get-in result [:policy :abstain?])
           :structure (:structure result)}))
      (catch Exception e
        (json-response 500 {:ok false :error "portfolio-step-failed"
                            :message (.getMessage e)})))))

(defn- handle-portfolio-heartbeat
  "POST /api/alpha/portfolio/heartbeat — run heartbeat with bid/clear data."
  [request config]
  (let [payload (parse-json-map (read-body request))]
    (if (nil? payload)
      (json-response 400 {:ok false :error "invalid-json"
                          :message "Request body must be a JSON object with heartbeat data"})
      (let [evidence-store (evidence-store-for-config config)
            ;; Parse heartbeat-data from payload, keywordizing effort/outcome/mode
            heartbeat-data
            (cond-> {}
              (:bids payload)
              (assoc :bids (mapv (fn [b]
                                  (cond-> {:action (keyword (:action b))
                                           :mission (:mission b)
                                           :effort (keyword (:effort b))}))
                                (:bids payload)))
              (:clears payload)
              (assoc :clears (mapv (fn [c]
                                    (cond-> {:action (keyword (:action c))
                                             :mission (:mission c)
                                             :effort (keyword (:effort c))
                                             :outcome (keyword (:outcome c))}))
                                  (:clears payload)))
              (:mode-prediction payload)
              (assoc :mode-prediction (keyword (:mode-prediction payload)))
              (:mode-observed payload)
              (assoc :mode-observed (keyword (:mode-observed payload))))
            opts {}]
        (try
          (let [result (portfolio/portfolio-heartbeat! evidence-store heartbeat-data opts)]
            (json-response 200
              {:ok true
               :recommendation (portfolio/format-recommendation result)
               :action (some-> (:action result) name)
               :diagnostics (:diagnostics result)
               :heartbeat (:heartbeat result)}))
          (catch Exception e
            (json-response 500 {:ok false :error "portfolio-heartbeat-failed"
                                :message (.getMessage e)})))))))

(defn- handle-portfolio-state
  "GET /api/alpha/portfolio/state — return current belief state."
  [_config]
  (let [state @portfolio/!state
        mu (:mu state)
        prec (:prec state)]
    (json-response 200
      {:ok true
       :state {:mu mu
               :prec prec
               :mode (:mode mu)
               :urgency (:urgency mu)
               :tau (:tau prec)
               :step-count (:step-count state)
               :pending (:pending state)}})))

;; =============================================================================
;; Concept Graph (meme store)
;; =============================================================================

(defn- meme-datasource
  "Get the meme store datasource, or nil if meme.db doesn't exist."
  []
  (let [path (meme-schema/db-path)]
    (when (.exists (io/file path))
      (meme-schema/datasource path))))

(defn- handle-concepts
  "GET /api/alpha/concepts?q=... — query concept graph from meme store.
   Returns entities matching the query with their arrows."
  [request]
  (let [params (:query-string request)
        q (some-> params
                  (str/split #"&")
                  (->> (some (fn [p]
                               (when (str/starts-with? p "q=")
                                 (enc/decode-uri-component (subs p 2)))))))]
    (if-not q
      (json-response 400 {:ok false :error "Missing ?q= parameter"})
      (if-let [ds (meme-datasource)]
        (let [entity (meme-core/find-entity-by-name ds q)
              entities (if entity
                         [entity]
                         ;; Fuzzy: search by LIKE
                         (let [like-pat (str "%" q "%")]
                           (try
                             (meme-jdbc/execute! ds
                               ["SELECT * FROM entities WHERE name LIKE ? LIMIT 20" like-pat]
                               {:builder-fn meme-rs/as-unqualified-maps})
                             (catch Exception _ []))))
              result (mapv (fn [ent]
                             {:entity (select-keys ent [:id :name :kind :description])
                              :arrows (mapv (fn [a] (select-keys a [:id :source_id :target_id :mode :rationale :confidence]))
                                            (meme-arrow/arrows-from ds (:id ent)))
                              :bridges []})
                           entities)]
          (json-response 200 {:ok true :query q :count (count result) :results result}))
        (json-response 503 {:ok false :error "meme.db not available"})))))

(defn- handle-concepts-health
  "GET /api/alpha/concepts/health — meme store health check."
  []
  (if-let [ds (meme-datasource)]
    (let [entity-count (-> (meme-jdbc/execute-one! ds ["SELECT count(*) as cnt FROM entities"]
                             {:builder-fn meme-rs/as-unqualified-maps})
                           :cnt)
          arrow-count (-> (meme-jdbc/execute-one! ds ["SELECT count(*) as cnt FROM arrows"]
                            {:builder-fn meme-rs/as-unqualified-maps})
                          :cnt)]
      (json-response 200 {:ok true
                           :entities entity-count
                           :arrows arrow-count
                           :db-path (meme-schema/db-path)}))
    (json-response 503 {:ok false :error "meme.db not available"})))

;; =============================================================================
;; Public API
;; =============================================================================

(defn make-handler
  "Create an HTTP request handler wired to the social pipeline.

   config:
     :registry  — AgentRegistryShape (may include :peripheral-config)
     :patterns  — PatternLibrary

   Returns a Ring handler fn that routes to the social pipeline."
  [config]
  (let [started-at (Instant/now)]
    (fn [request]
      (let [method (:request-method request)
            uri (:uri request)]
        (cond
          (and (= :post method) (= "/dispatch" uri))
          (handle-dispatch request config)

          (and (= :post method) (= "/presence" uri))
          (handle-presence request config)

          (and (= :get method) (string? uri) (str/starts-with? uri "/session/"))
          (handle-get-session request config)

          (and (= :post method) (= "/api/alpha/evidence" uri))
          (handle-evidence-create request config)

          (and (= :get method) (= "/api/alpha/evidence" uri))
          (handle-evidence-query request config)

          (and (= :get method) (= "/api/alpha/evidence/count" uri))
          (handle-evidence-count request config)

          (and (= :get method) (re-matches #"/api/alpha/evidence/(.+)/chain" uri))
          (let [[_ raw-id] (re-find #"/api/alpha/evidence/(.+)/chain" uri)
                evidence-id (enc/decode-uri-component raw-id)]
            (handle-evidence-chain config evidence-id))

          (and (= :get method) (re-matches #"/api/alpha/evidence/(.+)" uri))
          (let [[_ raw-id] (re-find #"/api/alpha/evidence/(.+)" uri)
                evidence-id (enc/decode-uri-component raw-id)]
            (handle-evidence-entry config evidence-id))

          (and (= :post method) (= "/api/alpha/invoke" uri))
          (handle-invoke request config)

          (and (= :post method) (= "/api/alpha/bell" uri))
          (handle-bell request config)

          (and (= :get method) (= "/api/alpha/invoke/jobs" uri))
          (handle-invoke-jobs request)

          (and (= :get method) (re-matches #"/api/alpha/invoke/jobs/(.+)" uri))
          (let [[_ raw-id] (re-find #"/api/alpha/invoke/jobs/(.+)" uri)
                job-id (enc/decode-uri-component raw-id)]
            (handle-invoke-job job-id))

          (and (= :post method) (= "/api/alpha/invoke/jobs/reap" uri))
          (let [n (reap-stale-invoke-jobs!)]
            (json-response 200 {:ok true :reaped n}))

          (and (= :post method) (= "/api/alpha/invoke-stream" uri))
          (handle-invoke-stream request config)

          (and (= :post method) (= "/api/alpha/whistle" uri))
          (handle-whistle request config)

          (and (= :post method) (= "/api/alpha/whistle-stream" uri))
          (handle-whistle-stream request config)

          (and (= :post method) (= "/api/alpha/irc/send" uri))
          (handle-irc-send request config)

          (and (= :post method) (= "/api/alpha/invoke-delivery" uri))
          (handle-invoke-delivery request config)

          (and (= :get method) (= "/api/alpha/irc/history" uri))
          (handle-irc-history request config)

          (and (= :post method) (= "/api/alpha/mission-control" uri))
          (handle-mission-control request config)

          (and (= :post method) (= "/api/alpha/mc/backfill" uri))
          (handle-mc-backfill config)

          (and (= :get method) (= "/api/alpha/mc/tensions" uri))
          (handle-mc-tensions config)

          (and (= :get method) (= "/api/alpha/mc/devmaps" uri))
          (handle-mc-devmaps config)

          (and (= :get method) (= "/api/alpha/mc/trace" uri))
          (handle-mc-trace request config)

          (and (= :post method) (= "/api/alpha/todo" uri))
          (handle-todo request config)

          ;; Portfolio inference
          (and (= :post method) (= "/api/alpha/portfolio/step" uri))
          (handle-portfolio-step request config)

          (and (= :post method) (= "/api/alpha/portfolio/heartbeat" uri))
          (handle-portfolio-heartbeat request config)

          (and (= :get method) (= "/api/alpha/portfolio/state" uri))
          (handle-portfolio-state config)

          (and (= :get method) (= "/api/alpha/missions" uri))
          (handle-missions request config)

          (and (= :get method) (string? uri)
               (str/starts-with? uri "/api/alpha/missions/")
               (str/ends-with? uri "/wiring"))
          (let [raw (subs uri (count "/api/alpha/missions/")
                         (- (count uri) (count "/wiring")))]
            (handle-mission-wiring config (enc/decode-uri-component raw)))

          (and (= :get method) (string? uri)
               (str/starts-with? uri "/api/alpha/missions/"))
          (let [raw (subs uri (count "/api/alpha/missions/"))]
            (handle-mission-detail config (enc/decode-uri-component raw)))

          (and (= :post method) (= "/api/alpha/agents/auto" uri))
          (handle-agents-auto-register request config)

          (and (= :post method) (= "/api/alpha/agents" uri))
          (handle-agents-register request config)

          (and (= :post method) (string? uri)
               (str/starts-with? uri "/api/alpha/agents/")
               (str/ends-with? uri "/rebind"))
          (let [raw (subs uri (count "/api/alpha/agents/")
                         (- (count uri) (count "/rebind")))]
            (handle-agent-rebind config (enc/decode-uri-component raw) request))

          (and (= :post method) (string? uri)
               (str/starts-with? uri "/api/alpha/agents/")
               (str/ends-with? uri "/reset-session"))
          (let [raw (subs uri (count "/api/alpha/agents/")
                         (- (count uri) (count "/reset-session")))]
            (handle-agent-reset-session config (enc/decode-uri-component raw)))

          (and (= :delete method) (re-matches #"/api/alpha/agents/(.+)" uri))
          (let [[_ agent-id] (re-find #"/api/alpha/agents/(.+)" uri)]
            (handle-agent-delete config agent-id))

          (and (= :get method) (re-matches #"/api/alpha/agents/(.+)" uri))
          (let [[_ agent-id] (re-find #"/api/alpha/agents/(.+)" uri)]
            (handle-agent-get config agent-id))

          (and (= :get method) (= "/api/alpha/agents" uri))
          (handle-agents-list config)

          ;; CYDER process endpoints
          (and (= :post method) (string? uri)
               (str/starts-with? uri "/api/alpha/processes/")
               (str/ends-with? uri "/step"))
          (let [raw (subs uri (count "/api/alpha/processes/")
                         (- (count uri) (count "/step")))]
            (handle-process-step config (enc/decode-uri-component raw)))

          (and (= :delete method) (string? uri)
               (re-matches #"/api/alpha/processes/(.+)" uri))
          (let [[_ process-id] (re-find #"/api/alpha/processes/(.+)" uri)]
            (handle-process-delete config (enc/decode-uri-component process-id)))

          (and (= :get method) (string? uri)
               (re-matches #"/api/alpha/processes/(.+)" uri))
          (let [[_ process-id] (re-find #"/api/alpha/processes/(.+)" uri)]
            (handle-process-get config (enc/decode-uri-component process-id)))

          (and (= :get method) (= "/api/alpha/processes" uri))
          (handle-processes-list config)

          ;; Reflection endpoints
          (and (= :get method) (= "/api/alpha/reflect/namespaces" uri))
          (handle-reflect-namespaces request)

          (and (= :get method) (string? uri)
               (str/starts-with? uri "/api/alpha/reflect/ns/")
               (str/ends-with? uri "/full"))
          (let [raw (subs uri (count "/api/alpha/reflect/ns/")
                         (- (count uri) (count "/full")))]
            (handle-reflect-ns-full (enc/decode-uri-component raw)))

          (and (= :get method) (string? uri)
               (str/starts-with? uri "/api/alpha/reflect/ns/"))
          (let [raw (subs uri (count "/api/alpha/reflect/ns/"))]
            (handle-reflect-ns (enc/decode-uri-component raw)))

          (and (= :get method) (string? uri)
               (str/starts-with? uri "/api/alpha/reflect/var/"))
          (let [raw (subs uri (count "/api/alpha/reflect/var/"))
                idx (.indexOf raw "/")]
            (if (pos? idx)
              (handle-reflect-var
                (enc/decode-uri-component (subs raw 0 idx))
                (enc/decode-uri-component (subs raw (inc idx))))
              (json-response 400 {:ok false :error "Expected /api/alpha/reflect/var/:ns/:var"})))

          (and (= :get method) (string? uri)
               (str/starts-with? uri "/api/alpha/reflect/deps/"))
          (let [raw (subs uri (count "/api/alpha/reflect/deps/"))]
            (handle-reflect-deps (enc/decode-uri-component raw)))

          (and (= :get method) (string? uri)
               (str/starts-with? uri "/api/alpha/reflect/java/"))
          (let [raw (subs uri (count "/api/alpha/reflect/java/"))]
            (handle-reflect-java-class (enc/decode-uri-component raw)))

          (and (= :get method) (= "/api/alpha/concepts" uri))
          (handle-concepts request)

          (and (= :get method) (= "/api/alpha/concepts/health" uri))
          (handle-concepts-health)

          (and (= :get method) (= "/api/alpha/enrich/file" uri))
          (handle-enrich-file request config)

          (and (= :get method) (= "/health" uri))
          (handle-health config started-at)

          (and (= :get method) (= "/fulab/encyclopedia/corpuses" uri))
          (handle-encyclopedia-corpuses config)

          (and (= :get method) (re-matches #"/fulab/encyclopedia/([^/]+)/entries" uri))
          (let [[_ corpus] (re-find #"/fulab/encyclopedia/([^/]+)/entries" uri)
                corpus-name (enc/decode-uri-component corpus)]
            (handle-encyclopedia-entries request config corpus-name))

          (and (= :get method) (re-matches #"/fulab/encyclopedia/([^/]+)/entry/(.+)" uri))
          (let [[_ corpus raw-entry-id] (re-find #"/fulab/encyclopedia/([^/]+)/entry/(.+)" uri)
                corpus-name (enc/decode-uri-component corpus)
                entry-id (enc/decode-uri-component raw-entry-id)]
            (handle-encyclopedia-entry config corpus-name entry-id))

          :else
          (json-response 404 {"error" true
                              "code" "not-found"
                              "message" (str "Unknown endpoint: "
                                            (some-> method name str/upper-case)
                                            " " uri)}))))))

(defn start-server!
  "Start HTTP server on port. Returns {:server stop-fn :port p :started-at t}.

   Per realtime/verify-after-start (L4, L7): after start(), sleeps briefly
   then probes the port by attempting a TCP connection. Converts silent binding
   failures into immediate, actionable errors.

   Call (:server result) to stop the server (it's the stop function)."
  [handler port]
  (let [!server (atom nil)
        !stopped? (atom false)
        server (hk/run-server handler {:port port
                                       :legacy-return-value? false
                                       :error-logger (make-http-kit-error-logger !server)})
        _ (reset! !server server)
        stop-fn (fn [& {:keys [timeout] :or {timeout 100}}]
                  ;; Keep `:server` return value as a callable stop fn.
                  ;; Stop is idempotent and suppresses the known http-kit/JDK
                  ;; selector close race (NPE in AbstractSelectableChannel/removeKey).
                  (when (compare-and-set! !stopped? false true)
                    (try
                      (hk/server-stop! server {:timeout timeout})
                      (catch NullPointerException ex
                        (when-not (expected-http-kit-stop-race? ex)
                          (throw ex)))))
                  nil)
        _ (Thread/sleep 100)
        listening? (try
                     (with-open [sock (Socket.)]
                       (.connect sock (InetSocketAddress. "localhost" (int port)) 1000)
                       true)
                     (catch Exception _ false))]
    (if listening?
      {:server stop-fn :port port :started-at (str (Instant/now))}
      (do (stop-fn)
          (throw (ex-info "Server started but port is not listening (L7: verify-after-start)"
                          {:port port}))))))
