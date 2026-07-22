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
     GET  /api/alpha/coordination/edges — list social-layer mesh edges
     GET  /api/alpha/coordination/qa — run mesh misrouting QA
     GET  /api/alpha/invoke/jobs/:id — retrieve invoke job details
     POST /api/alpha/invoke/announce — record a queued invoke before external acceptance
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
     POST /api/alpha/evidence/psr — Pattern Selection Record (walkie-talkie)
     POST /api/alpha/evidence/pur — Pattern Use Record (walkie-talkie)
     POST /api/alpha/evidence/par — Post-Action Review (walkie-talkie)
     POST /api/alpha/arse/ask — post ArSE question (walkie-talkie)
     POST /api/alpha/arse/answer — answer ArSE question (walkie-talkie)
     GET  /api/alpha/arse/unanswered — list unanswered ArSE questions
     POST /api/alpha/todo — lightweight todo management (add/list/done)
     POST /api/alpha/portfolio/step — run one AIF portfolio step
     POST /api/alpha/portfolio/heartbeat — weekly heartbeat with bid/clear
     GET  /api/alpha/portfolio/state — current portfolio belief state
     GET  /api/alpha/invariants — run invariant checks, return domain reports
     GET  /health    — liveness check with agent/session counts

   Pattern references:
   - realtime/verify-after-start (L4, L7): start-server! probes port after
     start to confirm it is actually listening. Async startup can hide binding
     failures (port in use, permission denied).
   - realtime/request-param-resilience (L1, L3): delegates param extraction
     to protocol/extract-params for consistency across HTTP and WS."
  (:require [futon3c.transport.protocol :as proto]
            [futon3c.transport.encyclopedia :as enc]
            [futon3c.evidence.boundary :as boundary]
            [futon3c.evidence.store :as estore]
            [futon3c.agency.registry :as reg]
            [futon3c.agency.federation :as federation]
            [futon3c.agency.mesh-qa :as mesh-qa]
            [futon3c.agency.invariants :as agency-invariants]
            [futon3c.agency.turn-queue :as turn-queue]
            [futon3c.agency.bell-router :as bell-router]
            [futon3c.agency.clock-lineage :as clock-lineage]
            [futon3c.agency.clock-store :as clock-store]
            [futon3c.agency.parked-on :as parked-on]
            [futon3c.social.mode :as mode]
            [futon3c.social.dispatch :as dispatch]
            [futon3c.social.presence :as presence]
            [futon3c.social.persist :as persist]
            [futon3c.social.whistles :as whistles]
            [futon3c.social.coordination-ledger :as coordination-ledger]
            [futon3c.mission-control.service :as mcs]
            [futon3c.peripheral.mission-control-backend :as mcb]
            [futon3c.portfolio.core :as portfolio]
            [futon3c.agents.zai-api :as zai-api]
            [futon3c.reflection.core :as reflection]
            [futon3c.enrichment.query :as enrich]
            [futon3c.transport.peripheral-events :as peripheral-events]
            [futon3c.transport.ws.invoke :as ws-invoke]
            [futon3c.blackboard :as bb]
            [futon3c.mfuton-mode :as mfuton-mode]
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
            [clojure.java.shell :as shell]
            [clojure.set :as cset]
            [clojure.string :as str]
            [org.httpkit.server :as hk])
  (:import [java.time Instant]
           [java.net Socket InetSocketAddress]
           [java.nio.channels AsynchronousCloseException ClosedChannelException ClosedSelectorException]
           [java.util UUID]
           [java.util.concurrent Executors ExecutorService RejectedExecutionException]
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

(def ^:private invoke-lane-count
  ;; Shared invoke-executor lanes. Bumped 4 -> 8 (2026-06-12) now that drainer-v2
  ;; keeps bells off these lanes; 8 lets a Codex swarm run parallel to Claude WIP.
  ;; Override with FUTON3C_INVOKE_LANES. Takes effect on JVM restart (defonce pool).
  (or (try (some-> (or (System/getProperty "FUTON3C_INVOKE_LANES")
                       (System/getenv "FUTON3C_INVOKE_LANES"))
                   str/trim not-empty Long/parseLong)
           (catch Exception _ nil))
      8))

(defonce ^ExecutorService invoke-executor
  (Executors/newFixedThreadPool
   invoke-lane-count
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
   :headers {"Content-Type" "application/json"
             "Access-Control-Allow-Origin" "*"}
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

(defn- trim-stream-event
  "Compact a live invoke event for the durable job ledger. Text is
   truncated; tool inputs reduce to short previews; tool_result is
   dropped (the tool_use preview is the observability unit)."
  [event]
  (let [etype (str (:type event))]
    (case etype
      "text"
      (let [t (str (or (:text event) ""))]
        (when-not (str/blank? t)
          {:event-type "text"
           :payload {:text (if (> (count t) 2000)
                             (str (subs t 0 2000) " …[trimmed]")
                             t)}}))
      ;; The inbound user/caller prompt, recorded at job start so sessions
      ;; can rehydrate turns from the ledger (M-custom-harness D-7) — before
      ;; 2026-07-04 prompts were not persisted anywhere.
      "prompt"
      (let [t (str (or (:text event) ""))]
        (when-not (str/blank? t)
          {:event-type "prompt"
           :payload {:text (if (> (count t) 1500)
                             (str (subs t 0 1500) " …[trimmed]")
                             t)}}))
      "tool_use"
      {:event-type "tool_use"
       :payload {:tools (mapv str (or (:tools event) []))
                 :previews (mapv (fn [d]
                                   (let [input (:input d)
                                         hint (when (map? input)
                                                (some input [:path :command :pattern
                                                             :query :repo :namespace]))
                                         ;; single display line: collapse
                                         ;; newlines/whitespace runs (heredoc
                                         ;; commands break line-based
                                         ;; fontification downstream)
                                         hint (some-> hint str
                                                      (str/replace #"\s+" " "))
                                         hint (when hint
                                                (if (> (count hint) 120)
                                                  (subs hint 0 120)
                                                  hint))]
                                     (str (:name d) (when hint (str " " hint)))))
                                 (or (:tool_details event) []))}}
      nil)))

(defn- record-job-stream-event!
  "Append a live text/tool_use event to a job's ledger record so bell-seeded
   turns are observable via GET /api/alpha/invoke/jobs (agent-follow-mode).
   Deliberately does NOT persist per-event — finalize persists; this is a
   live observability feed, acceptable to lose on restart."
  [job-id event]
  (when-let [{:keys [event-type payload]} (trim-stream-event event)]
    (ensure-invoke-jobs-ledger!)
    (swap! !invoke-jobs-ledger
           (fn [ledger]
             (if-let [job (get-in ledger [:jobs job-id])]
               (assoc-in ledger [:jobs job-id]
                         (append-job-event job event-type payload))
               ledger)))
    ;; WS doorbell (2026-07-05): tell connected observers an event landed so
    ;; follow-mode can poll NOW instead of on its fallback interval. Tiny
    ;; frame — ids only, no payload; the poll fetches content (and repairs
    ;; any dropped frames, so this is latency-only, never correctness).
    (try
      (when-let [bc (requiring-resolve 'futon3c.transport.ws.invoke/broadcast-frame!)]
        (bc {"type" "invoke_event"
             "agent-id" (str (get-in @!invoke-jobs-ledger [:jobs job-id :agent-id]))
             "job-id" job-id
             "event-type" event-type}))
      (catch Throwable _))))

(defn- next-invoke-job-id
  [ledger]
  (let [next-seq (inc (long (or (:next-seq ledger) 0)))
        rand-sfx (subs (str (UUID/randomUUID)) 0 8)]
    [(str "invoke-" (System/currentTimeMillis) "-" next-seq "-" rand-sfx)
     next-seq]))

(def ^:private invoke-job-modes #{"work" "brief"})

(defn- normalize-invoke-job-mode
  [mode]
  (let [mode' (some-> (cond
                        (keyword? mode) (name mode)
                        (some? mode) (str mode))
                      str/lower-case str/trim)]
    (when (contains? invoke-job-modes mode') mode')))

(defn- invoke-job-mode
  ([prompt] (invoke-job-mode prompt nil))
  ([prompt explicit-mode]
   (or (normalize-invoke-job-mode explicit-mode)
       (let [p (str (or prompt ""))]
         (if (or (re-find #"(?i)\bmode:\s*task\b" p)
                 (re-find #"(?i)\b(task assignment|fm-\d{3}|falsify|prove|counterexample|state of play)\b" p))
           "work"
           "brief")))))

(defn- extract-trace-id
  [invoke-meta]
  (some (fn [k]
          (let [v (or (get invoke-meta k) (get invoke-meta (name k)))]
            (when (and (string? v) (not (str/blank? v)))
              v)))
        [:invoke-trace-id :invoke_trace_id :invokeTraceId]))

;; Keep this out of extended/comments mode: `#` is data in the PR alternative,
;; not a regex comment. Ordinary words beginning with "pr" must not match.
(def ^:private artifact-ref-re
  #"(?i)(https?://github\.com/\S+/(?:pull|issues)/\d+)|(\bPR\s*#\d+\b)|(\b[0-9a-f]{7,40}\b)|((?:/|\.{1,2}/|~?/)[^\s]+?\.(?:clj|cljs|cljc|el|md|txt|sh|py|js|ts|tsx|java|go|rs|tex|json|edn)\b)")

#_{:clj-kondo/ignore [:unused-private-var]}
(defn- first-matching-ref
  [pattern text]
  (when (string? text)
    (some-> (re-find pattern text)
            (str/replace #"[.,:;]+$" "")
            str/trim
            not-empty)))

(defn- first-artifact-ref
  [text]
  (or (mfuton-mode/first-frontiermath-local-artifact-ref text)
      (when (string? text)
        (some->> (re-find artifact-ref-re text)
                 rest
                 (remove nil?)
                 first
                 (#(str/replace % #"[.,:;]+$" ""))
                 str/trim))))

(defn- summarize-result-text
  [text]
  (let [t (str/trim (str/replace (str (or text "")) #"\s+" " "))]
    (if (<= (count t) 220)
      t
      (str (subs t 0 217) "..."))))

(def ^:private auto-bellback-caller "auto-bellback")

(defn- auto-bellback-enabled?
  []
  (let [raw (some-> (System/getenv "FUTON3C_AUTO_BELLBACK") str/trim str/lower-case)]
    (not (#{"0" "false" "no" "off"} raw))))

(defn- bell-router-enabled?
  "Bell-router slice (E-crossed-bells): make a bellback an EXPLICIT, self-describing
   reply — carry `:bellback-of <original-job>` for the conversation graph, and frame
   the prompt as 'RE: your bell …' so the receiving agent can thread it instead of
   mis-reading a context-free 'auto-bellback'. **Default ON 2026-06-27 (Joe)** — proven
   + composes with the reply-auto-routes dedup (the NEW-request thread line then says
   'just respond' rather than instructing a manual reply-bell). Explicit
   `FUTON3C_BELL_ROUTER=0/false/no/off` (system property or env) still disables it."
  []
  (let [prop (System/getProperty "FUTON3C_BELL_ROUTER")]
    (if (some? prop)
      (not (#{"0" "false" "no" "off"} (str/lower-case (str/trim prop))))
      (let [raw (some-> (System/getenv "FUTON3C_BELL_ROUTER") str/trim str/lower-case)]
        (not (#{"0" "false" "no" "off"} raw))))))

(def ^:private allowed-bell-types
  #{:query :answer :assert :challenge :agree :define :retract :suggest :request})

(defn- typed-bells-enabled?
  "M-typed-bells rollout flag. Default OFF: type/ref payload fields are ignored,
   and the legacy bell path remains inert."
  []
  (let [prop (System/getProperty "FUTON3C_TYPED_BELLS")]
    (if (some? prop)
      (not (#{"0" "false" "no" "off"} (str/lower-case (str/trim prop))))
      (let [raw (some-> (System/getenv "FUTON3C_TYPED_BELLS") str/trim str/lower-case)]
        (boolean (and raw (not (#{"0" "false" "no" "off" ""} raw))))))))

(defn- normalize-bell-type
  [raw]
  (let [t (if (or (nil? raw) (str/blank? (str raw)))
            :request
            (keyword (str/lower-case (str/trim (name raw)))))]
    (when (contains? allowed-bell-types t)
      t)))

(defn- nonblank-str
  [x]
  (some-> x str str/trim not-empty))

(defn- terminal-invoke-state?
  [state]
  (#{"done" "succeeded" "failed" "error" "timeout" "cancelled"} (str state)))

(defn- auto-bellback-job?
  [job]
  (or (= auto-bellback-caller (some-> (:caller job) str str/trim))
      (= auto-bellback-caller (some-> (:surface job) str str/trim))
      (= auto-bellback-caller (some-> (:kind job) str str/trim))))

(defn- valid-auto-bellback-caller?
  [caller agent-id caller-registered?]
  (let [caller-id (some-> caller str str/trim)]
    (and (boolean caller-registered?)
         (not (str/blank? (str caller-id)))
         (not (#{"http-caller" "joe"} caller-id))
         (not= caller-id (some-> agent-id str str/trim)))))

(def auto-bellback-recipient-types
  "Agent types whose job completions auto-bellback to the registered caller.
   v1 was codex-only; widened to :claude 2026-06-11 (Joe): claude agents'
   turns complete silently on the warm-pouch path unless THEY remember to
   bell — the completion contract should be structural, not behavioral.
   Loop-safety is unchanged: bellback jobs carry caller \"auto-bellback\"
   (unregistered + excluded), so a bellback never bellbacks.
   Widened to :zai 2026-07-04 (M-custom-harness slice-2 live test: a zai
   bell completed but no completion bell routed back to the caller)."
  #{:codex :claude :zai})

(defn- same-agent-id?
  [a b]
  (= (some-> a str str/trim) (some-> b str str/trim)))

(defn- auto-bellback-suppressing-park
  [job released-park-records]
  (let [caller (:caller job)]
    (some #(when (same-agent-id? caller (:agent %)) %) released-park-records)))

(defn- log-auto-bellback-suppressed!
  [job park]
  (println (str "[parked-on] auto-bellback suppressed for " (:job-id job)
                ": park " (:id park) " will wake " (:agent park))))

(defn should-auto-bellback?
  "Pure auto-bellback decision predicate. Recipient type and caller registration
   are passed in so tests and future recipient widening stay local."
  ([job recipient-type caller-registered? enabled?]
   (should-auto-bellback? job recipient-type caller-registered? enabled? nil))
  ([job recipient-type caller-registered? enabled? released-park-records]
   (boolean
    (and enabled?
         (terminal-invoke-state? (:state job))
         (contains? auto-bellback-recipient-types recipient-type)
         (valid-auto-bellback-caller? (:caller job) (:agent-id job) caller-registered?)
         (not (auto-bellback-job? job))
         (nil? (:auto-bellback job))
         (not (auto-bellback-suppressing-park job released-park-records))))))

(defn- auto-bellback-recipient-type
  [agent-id]
  (let [t (:agent/type (reg/get-agent (str agent-id)))]
    (when (contains? auto-bellback-recipient-types t) t)))

(defn- auto-bellback-caller-registered?
  [caller]
  (boolean (reg/get-agent (str caller))))

(defn- reply-auto-routes?
  "True when RECIPIENT-AGENT-ID's response this turn will be auto-delivered back to
   CALLER as a completion bell — so the agent must NOT also manually bell/whistle CALLER
   to answer (that double-delivers; the 2026-06-26 claude-11 bifurcation). Mirrors
   should-auto-bellback?'s eligibility minus the terminal-state check (which is always
   true once the turn finishes)."
  [recipient-agent-id caller]
  (boolean
   (and (auto-bellback-enabled?)
        (auto-bellback-recipient-type recipient-agent-id)
        (valid-auto-bellback-caller? caller recipient-agent-id
                                     (auto-bellback-caller-registered? caller)))))

(defn- auto-bellback-prompt
  [{:keys [job-id agent-id state result-summary result-text terminal-message]}]
  ;; Prefer the (bounded) FULL result text over the 220-char display summary:
  ;; bellbacks are OPERATIVE replies — a truncated dispatch inside one cost a
  ;; driver a whole held turn (zai-2, 2026-07-05). The summary stays for list
  ;; views; the bellback carries the words.
  (let [summary (or (some-> result-text str str/trim not-empty)
                    (some-> result-summary str str/trim not-empty)
                    (some-> terminal-message str str/trim not-empty)
                    "No result summary recorded.")]
    (if (bell-router-enabled?)
      ;; Self-describing reply: the receiving agent reads this as a reply to ITS bell,
      ;; not a context-free 'auto-bellback' it has to guess at (E-crossed-bells).
      (str "🔔 RE: your bell — job `" job-id "` to " agent-id
           " finished (state `" state "`).\n"
           summary
           "\n(Automated bellback. To continue this thread, bell or whistle "
           agent-id " directly.)"
           "\nDetails: /api/alpha/invoke/jobs/" job-id)
      (str "🔔 " agent-id " finished job `" job-id "` (state: `" state "`). "
           summary
           "\nDetails: /api/alpha/invoke/jobs/" job-id))))

(defn- auto-bellback-request
  ([job] (auto-bellback-request job nil))
  ([job released-park-records]
  (let [recipient-type (auto-bellback-recipient-type (:agent-id job))
        caller-registered? (auto-bellback-caller-registered? (:caller job))]
    (when (should-auto-bellback? job recipient-type caller-registered?
                                 (auto-bellback-enabled?) released-park-records)
      {:caller (str/trim (str (:caller job)))
       :bell-job-id (str "auto-bellback-" (:job-id job))
       :reply-to (str (:job-id job))   ;; the original bell this answers (bell-router)
       :prompt (auto-bellback-prompt job)}))))

(declare create-invoke-job!)
(declare run-invoke-job!)
(declare record-invoke-job-delivery-by-job-id!)

(defn- enqueue-auto-bellback!
  [{:keys [caller bell-job-id prompt reply-to]}]
  (let [job-id (create-invoke-job! {:requested-job-id bell-job-id
                                    :agent-id caller
                                    :prompt prompt
                                    ;; caller stays "auto-bellback" — loop-safety + mesh
                                    ;; heuristics key on it. The correlation is explicit
                                    ;; via :bellback-of, and the agent-facing thread is in
                                    ;; the reframed prompt (bell-router).
                                    :caller auto-bellback-caller
                                    :surface auto-bellback-caller
                                    :bellback-of (when (bell-router-enabled?) reply-to)})
        run-job (fn []
                  (run-invoke-job! {:job-id job-id
                                    :agent-id caller
                                    :prompt prompt
                                    :caller auto-bellback-caller
                                    :surface auto-bellback-caller}))
        deliver-result (fn [result]
                         (record-invoke-job-delivery-by-job-id!
                          job-id
                          {:surface auto-bellback-caller
                           :destination (str "caller " caller " via /api/alpha/invoke/jobs/" job-id)
                           :delivered? true
                           :note (if (:ok result) "auto-bellback-ready" "auto-bellback-error")}))]
    ;; I-1 — "single identity is sequential execution." Route the bellback through the
    ;; RECIPIENT agent's dedicated per-agent drainer so it serializes with that agent's
    ;; other turns (bells / repl) on ONE single-flight lane, instead of racing them on
    ;; the shared invoke-executor pool. Mirrors the proven bell-dispatch path above.
    ;;
    ;; Incident 2026-06-26: an auto-bellback to claude-11 ran on invoke-worker-2 while a
    ;; turn-drainer-claude-11 bell ran concurrently — two invoke pipelines for one
    ;; identity. The warm-pouch lock (agent-pouch/feed-turn!) kept the session intact
    ;; downstream (the "transaction"), but two concurrent dispatches for one agent is the
    ;; upstream defect this closes. Gated on drainer-v2 (default on) like the bell path;
    ;; the legacy lane stays as the flag-off fallback.
    (if (turn-queue/drainer-v2-enabled?)
      (let [r (turn-queue/accept-async!
               {:to caller :from auto-bellback-caller :surface auto-bellback-caller
                :prompt prompt
                :process-fn (fn [_entry]
                              (binding [turn-queue/*drained-by-outer* true]
                                (run-job)))
                :finalize-fn deliver-result})]
        (when (= :deduped (:status r))
          (deliver-result {:ok true :deduped true})))
      (.submit invoke-executor
               ^Runnable
               (fn [] (deliver-result (run-job)))))
    job-id))

(def ^:dynamic *enqueue-auto-bellback!* enqueue-auto-bellback!)

;; --- E-repl-continuations Car 2: parked-on join release wiring -----------------
;; A job reaching terminal state folds into any parked-on continuation awaiting it;
;; when a join completes, ONE resume turn is enqueued for the parked agent. The hook
;; is flag-gated DEFAULT-OFF (load-dark) so reloading this ns is behaviorally inert
;; until FUTON3C_PARKED_ON is explicitly enabled.
(def ^:private parked-resume-caller "parked-resume")

(defn- parked-on-enabled?
  []
  (let [prop (or (System/getProperty "FUTON3C_PARKED_ON")
                 (System/getenv "FUTON3C_PARKED_ON"))]
    (boolean (#{"1" "true" "on" "yes"} (some-> prop str/trim str/lower-case)))))

;; Ready-inbox: a buffer-surfaced park whose join completed parks its assembled
;; resume prompt for the repl buffer to POLL (GET /api/alpha/parked/ready),
;; alongside the WS push. Now DURABLE — lives in parked_on.clj's disk-backed
;; atom (survives JVM restart). Delivery is LEASE-BASED: a pop marks the item
;; leased (not consumed); the repl buffer ACKs after successful injection, and
;; expired unacked leases are returned to the queue front for redelivery
;; (E-park-delivery-losses bugs 2-3, 2026-07-13).

(defn- parked-ready-push! [agent session park-id prompt mode]
  (parked-on/ready-push! agent session park-id prompt mode))

(defn parked-ready-pop!
  "Lease ONE ready resume for AGENT/SESSION (FIFO, pop-one per call). Returns the
   leased item {:park-id :prompt :lease-deadline-ms} or nil. The item stays leased
   until the buffer ACKs via POST /api/alpha/parked/ready/ack; the 30s sweep
   returns expired unacked leases for redelivery."
  [agent session]
  (parked-on/ready-lease-one! agent session))

(defn- assemble-resume-prompt [rec]
  (str (:payload rec)
       (if (:deadline-expired? rec)
         (str "\n\n--- resumed: DEADLINE EXPIRED with " (count (:arrived rec))
              " of " (+ (count (:arrived rec)) (count (:awaiting rec)))
              " dependencies complete — the awaited work did NOT finish ---\n")
         (str "\n\n--- resumed: parked dependencies complete ("
              (count (:arrived rec)) ") ---\n"))
       (str/join "\n" (for [[dep summ] (:arrived rec)]
                        (str "• " dep ": " (or summ "(no summary)"))))))

(defn- buffer-surface?
  "A surface whose resume should be RUN BY the agent's repl buffer (streamed in place),
   not server-side — i.e. an Emacs repl surface (emacs-repl / emacs-codex-repl)."
  [surface]
  (and surface (str/starts-with? (str surface) "emacs")))

(defn- parked-resume!
  "Resume a parked agent. For a BUFFER-surfaced park, the durable ready-inbox is
   the ONLY delivery path (every resume flows through lease → deliver → ACK, so
   the busy gate, redelivery, and dedup all apply); a targeted `park-ready` WS
   frame is then sent as a wake-up POKE that makes the buffer poll immediately
   instead of waiting out the poll interval. The frame carries no authoritative
   payload — losing it costs latency, never the resume. For a headless/
   bell-surfaced park, enqueue a fresh turn on the agent's OWN drainer lane
   (mirrors enqueue-auto-bellback!'s I-1 routing)."
  [rec]
  (let [agent (:agent rec)
        prompt (assemble-resume-prompt rec)]
    ;; the *agents* pane shows parked lines (blackboard/parked-suffix) —
    ;; re-project on resume so the line clears promptly.
    (try (bb/project-agents! (reg/registry-status)) (catch Throwable _ nil))
    (if (buffer-surface? (:surface rec))
      (do
        (parked-ready-push! agent (:session rec) (:id rec) prompt
                            (or (:mode rec) :within-turn))
        (let [poked? (ws-invoke/send-frame! (str agent)
                                            {:type "park-ready"
                                             :agent (str agent)
                                             :session (str (:session rec))
                                             :park-id (:id rec)
                                             :surface (str (:surface rec))
                                             :mode (name (or (:mode rec) :within-turn))})]
          (str "park-ready-inbox" (when poked? "+poke") ":" (:id rec))))
      (let [job-id (create-invoke-job! {:agent-id agent :prompt prompt
                                        :caller parked-resume-caller :surface parked-resume-caller})
            run-job (fn [] (run-invoke-job! {:job-id job-id :agent-id agent :prompt prompt
                                             :caller parked-resume-caller :surface parked-resume-caller}))]
        (if (turn-queue/drainer-v2-enabled?)
          (turn-queue/accept-async! {:to agent :from parked-resume-caller :surface parked-resume-caller
                                     :prompt prompt
                                     :process-fn (fn [_entry]
                                                   (binding [turn-queue/*drained-by-outer* true]
                                                     (run-job)))})
          (.submit invoke-executor ^Runnable (fn [] (run-job))))
        job-id))))

(defn parked-job-lookup
  "Ledger view for parked-on reconcile/rehydrate. :result is the complete reply;
   legacy ledgers stored it as :result-text. :result-summary remains only as a
   fallback for records with neither full field."
  [job-id]
  (when-let [job (get-in (ensure-invoke-jobs-ledger!) [:jobs job-id])]
    {:state (:state job)
     :result (let [result (:result job)]
               (if (or (nil? result)
                       (and (string? result) (str/blank? result)))
                 (:result-text job)
                 result))
     :result-summary (:result-summary job)}))

(defn parked-on-notify!
  "Hot-path hook (flag-gated): job JOB-ID reached terminal state -> fold it into
   any parked-on join awaiting it. Returns the records that actually released so
   finalize can suppress the duplicate auto-bellback only after a resume was
   enqueued. Never throws into finalize."
  [job-id result]
  (when (parked-on-enabled?)
    (try
      (parked-on/note-completion! job-id result
                                  {:resume! parked-resume!
                                   :now-ms (System/currentTimeMillis)})
      (catch Throwable t
        (println (str "[parked-on] notify failed for " job-id ": "
                      (.getMessage t)))
        {:released [] :released-records []}))))
(defn- parked-on-sweep-tick []
  (try
    (parked-on/sweep-deadlines!
     {:now-ms (System/currentTimeMillis)
      :resume! parked-resume!
      :on-expire (fn [rec] (println (str "[parked-on] deadline expired, retracted: " (:id rec))))})
    ;; Bug 3: return expired unacked ready-leases to their queue front for redelivery.
    (parked-on/sweep-leased!
     {:now-ms (System/currentTimeMillis)
      :on-expire (fn [park-id] (println (str "[parked-on] lease expired, requeued: " park-id)))})
    (catch Throwable t (println (str "[parked-on] sweep failed: " (.getMessage t))))))

;; defonce so a Drawbridge reload of this ns never orphans the daemon thread.
(defonce ^:private parked-on-sweeper (atom nil))

(defn start-parked-on!
  "Boot the parked-on subsystem: rehydrate persisted records against the recovered
   ledger (R3), then start ONE daemon ticking sweep-deadlines! (R4 — its own thread,
   NOT the Arxana Clock). Idempotent + flag-gated: a no-op unless FUTON3C_PARKED_ON."
  []
  (when (parked-on-enabled?)
    (parked-on/rehydrate! {:ledger-lookup parked-job-lookup :resume! parked-resume!
                           :now-ms (System/currentTimeMillis)})
    (when (nil? @parked-on-sweeper)
      (let [exec (java.util.concurrent.Executors/newSingleThreadScheduledExecutor
                  (reify java.util.concurrent.ThreadFactory
                    (newThread [_ r] (doto (Thread. ^Runnable r "parked-on-sweeper")
                                       (.setDaemon true)))))]
        (.scheduleAtFixedRate exec ^Runnable parked-on-sweep-tick
                              30 30 java.util.concurrent.TimeUnit/SECONDS)
        (reset! parked-on-sweeper exec)))
    {:rehydrated true :sweeper (some? @parked-on-sweeper)}))
;; --- end parked-on Car 2 wiring -----------------------------------------------

(defn- auto-record-direct-delivery-surface?
  "True when /api/alpha/invoke should auto-record delivery to the caller.
   Direct Emacs callers receive the terminal response in-band over HTTP, while
   IRC/bell/whistle surfaces have an additional projection step that records
   delivery explicitly."
  [surface]
  (let [s (str/lower-case (str/trim (str (or surface ""))))]
    (or (str/blank? s)
        (= s "http")
        (str/starts-with? s "http ")
        (str/starts-with? s "emacs"))))

(defn- direct-delivery-surface-label
  [surface]
  (let [s (some-> surface str str/trim not-empty)
        lower (some-> s str/lower-case)]
    (if (or (nil? s)
            (str/blank? s)
            (= lower "http")
            (str/starts-with? (or lower "") "http "))
      "http"
      s)))

(defn- record-http-delivery!
  [{:keys [agent-id caller surface result]}]
  (when (auto-record-direct-delivery-surface? surface)
    (when-let [trace-id (extract-trace-id (:invoke-meta result))]
      (let [receipt {:surface (direct-delivery-surface-label surface)
                     :destination (str "caller " (or caller "http-caller"))
                     :delivered? true
                     :note "http-direct-response"}]
        (record-invoke-job-delivery! trace-id receipt)
        (when-let [record-fn (*resolve-delivery-recorder*)]
          (try
            (record-fn (str agent-id) (str trace-id)
                       {:surface (direct-delivery-surface-label surface)
                        :destination (str "caller " (or caller "http-caller"))
                        :delivered? true
                        :note "http-direct-response"})
            (catch Throwable _)))))))

(defn- classify-terminal
  [result no-evidence?]
  (cond
    no-evidence?
    ["failed" "no-execution-evidence" "codex task-mode reply had no execution evidence"]

    (and (not (:ok result))
         (map? (:error result))
         (let [msg (some-> (:error result) :error/message str str/lower-case)]
           (and (string? msg)
                (str/includes? msg "invoke interrupted"))))
    ["cancelled" "invoke-interrupted" (some-> result :error :error/message)]

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
  [{:keys [requested-job-id agent-id prompt caller surface bellback-of bell-type ref mode]}]
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
                 mode (invoke-job-mode prompt mode)
                 job (cond-> {:job-id job-id
                               :agent-id (str agent-id)
                               :caller (str (or caller "http-caller"))
                               :surface (str (or surface "http"))
                               :bellback-of (some-> bellback-of str)   ;; bell-router: this job is a reply to <job-id>
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
                               :events []}
                        bell-type (assoc :bell-type bell-type)
                        (some? ref) (assoc :ref (str ref)))]
             (reset! created-id job-id)
             (-> ledger
                 (assoc :next-seq next-seq)
                 (update :job-order (fnil conj []) job-id)
                 (assoc-in [:jobs job-id] (append-job-event job "accepted" {}))))))))
    ;; First-class durable coordination edge (E-patch-agent-evidence-leaks): record the
    ;; (from→to) edge keyed by job-id so the in-band `Edge:` join-key resolves to a stored
    ;; edge for EVERY job (not just wrapped social-dispatch invokes). Never break the hot path.
    (try
      (coordination-ledger/record-invoke-edge!
       {:from (or caller "http-caller") :to (str agent-id)
        :surface (or surface "http") :kind :invoke :edge-id @created-id})
      (catch Throwable _))
    (bb/project-agents! (reg/registry-status))
    @created-id))

(defn active-invoke-job-counts
  "Return canonical non-terminal invoke-job counts keyed by agent-id.
   Example:
   {\"codex-1\" {:queued-jobs 1 :running-jobs 0 :nonterminal-jobs 1}}"
  []
  (ensure-invoke-jobs-ledger!)
  (reduce
   (fn [acc job]
     (let [aid (some-> (:agent-id job) str str/trim not-empty)
           state (some-> (:state job) str)]
       (if-not (and aid (#{"queued" "running" "overrun"} state))
         acc
         (-> acc
             (update-in [aid :queued-jobs] (fnil + 0) (if (= "queued" state) 1 0))
             (update-in [aid :running-jobs] (fnil + 0) (if (#{"running" "overrun"} state) 1 0))
             (update-in [aid :nonterminal-jobs] (fnil inc 0))))))
   {}
   (vals (get @!invoke-jobs-ledger :jobs {}))))

(defn- running-invoke-job-for-agent
  [agent-id]
  (ensure-invoke-jobs-ledger!)
  (let [aid (some-> agent-id str str/trim)]
    (->> (get @!invoke-jobs-ledger :job-order [])
         reverse
         (keep (fn [job-id]
                 (let [job (get-in @!invoke-jobs-ledger [:jobs job-id])]
                   (when (and (= aid (some-> job :agent-id str str/trim))
                              (= "running" (some-> job :state str)))
                     job))))
         first)))

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
        execution (invoke-execution-evidence result job-id)
        trace-id (extract-trace-id invoke-meta)
        result-text (when (string? (:result result)) (:result result))
        ;; Bounded full text for the auto-bellback (operative reply channel);
        ;; whitespace preserved. 8000 chars covers any sane dispatch.
        bellback-text (when result-text
                        (if (<= (count result-text) 8000)
                          result-text
                          (str (subs result-text 0 7997) "...")))
        summary (when result-text (summarize-result-text result-text))
        artifact-ref (or (first-artifact-ref result-text)
                         (first-artifact-ref summary))
        updated-terminal-job (atom nil)]
    (update-invoke-jobs-ledger!
     (fn [ledger]
       (if-let [job (get-in ledger [:jobs job-id])]
         ;; First terminal transition wins: if the ceiling reaper already
         ;; force-terminated this job ("timeout"), the interrupted worker's
         ;; own finalize must not overwrite it (no timeout->failed flip,
         ;; no double delivery). Non-terminal states (queued/running/overrun)
         ;; finalize normally.
         (if (not (#{"queued" "running" "overrun"} (str (:state job))))
           ledger
           (let [finished-at (str (Instant/now))
               updated-job (-> job
                               (assoc :state terminal-state
                                      :finished-at finished-at
                                      :terminal-code terminal-code
                                      :terminal-message terminal-message
                                      :session-id sid
                                      :trace-id trace-id
                                      :result result-text
                                      :result-summary summary
                                      :result-text bellback-text
                                      :artifact-ref artifact-ref
                                      :execution execution)
                               (append-job-event terminal-state
                                                 {:code terminal-code
                                                  :message terminal-message}))]
           (reset! updated-terminal-job updated-job)
           (cond-> (assoc-in ledger [:jobs job-id] updated-job)
             (and (string? trace-id) (not (str/blank? trace-id)))
             (assoc-in [:trace->job trace-id] job-id))))
         ledger)))
    (let [released-park-records (:released-records (parked-on-notify! job-id result-text))]
      (when @updated-terminal-job
        (let [bellback-request (atom nil)]
          (update-invoke-jobs-ledger!
           (fn [ledger]
             (if-let [job (get-in ledger [:jobs job-id])]
               (let [finished-at (or (:finished-at job) (str (Instant/now)))
                     recipient-type (auto-bellback-recipient-type (:agent-id job))
                     caller-registered? (auto-bellback-caller-registered? (:caller job))
                     suppressing-park (auto-bellback-suppressing-park job released-park-records)]
                 (cond
                   (and suppressing-park
                        (should-auto-bellback? job recipient-type caller-registered?
                                               (auto-bellback-enabled?)))
                   (do
                     (log-auto-bellback-suppressed! job suppressing-park)
                     (assoc-in ledger [:jobs job-id :auto-bellback]
                               {:suppressed? true
                                :reason :parked-on
                                :park-id (:id suppressing-park)
                                :at finished-at}))

                   :else
                   (if-let [request (auto-bellback-request job)]
                     (do
                       (reset! bellback-request request)
                       (assoc-in ledger [:jobs job-id :auto-bellback]
                                 {:sent? true
                                  :bell-job-id (:bell-job-id request)
                                  :at finished-at}))
                     ledger)))
               ledger)))
          (when-let [request @bellback-request]
            (try
              (*enqueue-auto-bellback!* request)
              (catch Throwable t
                (println (str "[invoke-jobs] auto-bellback enqueue failed for " job-id ": "
                              (.getMessage t)))
                (flush)))))))
    nil))

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
                    :execution :auto-bellback :delivery :events]))

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

(defn- parse-env-ms
  "Parse a millisecond duration from env var NAME, falling back to DEFAULT."
  [name default]
  (try
    (let [raw (System/getenv name)]
      (if (and raw (not (str/blank? (str/trim raw))))
        (long (Integer/parseInt (str/trim raw)))
        default))
    (catch Exception _ default)))

(def ^:private default-job-cap-ms
  "Soft cap: jobs running longer than this enter the 'overrun' state.
   The lane keeps executing (supervised); this is NOT a terminal state."
  (* 35 60 1000))  ;; 35 minutes

(defn job-cap-ms
  "Current job cap in ms. Overridable via FUTON3C_JOB_CAP_MS."
  []
  (parse-env-ms "FUTON3C_JOB_CAP_MS" default-job-cap-ms))

(defn job-ceiling-ms
  "Hard ceiling: jobs running longer than this are force-terminated.
   Default 2x cap. Overridable via FUTON3C_JOB_CEILING_MS."
  []
  (parse-env-ms "FUTON3C_JOB_CEILING_MS" (* 2 (job-cap-ms))))

;; Keep the old const for backward compatibility.
(def ^:const stale-job-threshold-ms default-job-cap-ms)

;; Worker thread tracking for ceiling enforcement. Maps job-id -> {:thread Thread :future Future}.
(defonce ^:private !job-workers (atom {}))

(defn- register-job-worker!
  "Track the worker thread/future for JOB-ID so the ceiling reaper can interrupt it."
  [job-id thread future]
  (swap! !job-workers assoc job-id {:thread thread :future future}))

(defn- unregister-job-worker!
  [job-id]
  (swap! !job-workers dissoc job-id))

(defn- interrupt-job-worker!
  "Interrupt the worker thread for JOB-ID if tracked. Returns true if interrupted."
  [job-id]
  (when-let [{:keys [thread future]} (get @!job-workers job-id)]
    (swap! !job-workers dissoc job-id)
    (try
      (when future (future-cancel future))
      (catch Throwable _))
    (try
      (when thread (.interrupt thread))
      (catch Throwable _))
    true))

(defn- finalize-overrun-job!
  "Force-terminate an overrun job past the ceiling.
   Marks 'timeout', runs execution-evidence fallback, marks agent idle."
  [jid job ceiling-ms]
  (interrupt-job-worker! jid)
  (let [finished-at (str (Instant/now))
        agent-id (:agent-id job)
        execution (try
                    (invoke-execution-evidence {:ok false} jid)
                    (catch Throwable _ {:executed false :tool-events 0 :command-events 0}))
        updated-job (-> job
                        (assoc :state "timeout"
                               :finished-at finished-at
                               :terminal-code "job-ceiling-exceeded"
                               :terminal-message (str "Job force-terminated after exceeding "
                                                      (/ ceiling-ms 60000) " minute ceiling")
                               :execution execution)
                        (append-job-event "timeout" {:code "job-ceiling-exceeded"
                                                     :ceiling-ms ceiling-ms}))]
    (when agent-id
      (try (reg/mark-agent-idle! (str agent-id)) (catch Throwable _)))
    updated-job))

(defn reap-stale-invoke-jobs!
  "Transition running jobs past the cap to 'overrun' (non-terminal, supervised).
   Force-terminate overrun jobs past the ceiling to 'timeout' (terminal).
   Returns the count of jobs transitioned."
  ([] (reap-stale-invoke-jobs! (job-cap-ms)))
  ([threshold-ms]
   (ensure-invoke-jobs-ledger!)
   (let [ceiling-ms (job-ceiling-ms)
         now-ms (System/currentTimeMillis)
         transitioned (atom 0)]
     (update-invoke-jobs-ledger!
      (fn [ledger]
        (let [jobs (:jobs ledger)
              updated-jobs
              (reduce-kv
               (fn [acc jid job]
                 (let [started (:started-at job)
                       age-ms (when (string? started)
                                (try
                                  (- now-ms (.toEpochMilli (Instant/parse started)))
                                  (catch Exception _ nil)))]
                   (cond
                     ;; Running past cap -> overrun (non-terminal, supervised)
                     (and (= "running" (str (:state job)))
                          (some? age-ms) (> age-ms threshold-ms))
                     (do (swap! transitioned inc)
                         (assoc acc jid
                                (-> job
                                    (assoc :state "overrun"
                                           :overrun-at (str (Instant/now)))
                                    (append-job-event "overrun" {:code "job-cap-exceeded"
                                                                 :cap-ms threshold-ms}))))

                     ;; Overrun past ceiling -> timeout (terminal, force-terminated)
                     (and (= "overrun" (str (:state job)))
                          (some? age-ms) (> age-ms ceiling-ms))
                     (do (swap! transitioned inc)
                         (assoc acc jid (finalize-overrun-job! jid job ceiling-ms)))

                     :else
                     (assoc acc jid job))))
               {}
               jobs)]
          (assoc ledger :jobs updated-jobs))))
     (let [n @transitioned]
       (when (pos? n)
         (println (str "[invoke-jobs] Transitioned " n " stale/overrun job(s)"))
         (flush))
       n))))

#_{:clj-kondo/ignore [:unused-private-var]}
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

(defn- default-evidence-since
  "Default lower bound for broad evidence pages.
   Session/pattern-specific views keep exact history; unfiltered latest pages
   should never force the backing store into a full chronological scan."
  []
  (str (.minusSeconds (Instant/now) (* 48 60 60))))

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

(defn- expected-http-kit-submit-after-stop?
  "True when `http-kit` tried to hand work to its request executor after stop began."
  [msg ex]
  (and (instance? RejectedExecutionException ex)
       (boolean
        (#{"failed to submit task to executor service"
           "increase :queue-size if this happens often"}
         msg))))

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

(defn- suppress-http-kit-error?
  "True when `http-kit` emitted a known shutdown-only race after stop began."
  [status msg ex]
  (and (not= :running status)
       (or (expected-http-kit-shutdown-close? msg ex)
           (expected-http-kit-submit-after-stop? msg ex))))

(defn- make-http-kit-error-logger
  "Create an error logger that suppresses expected shutdown close races only."
  [!server]
  (fn [msg ex]
    (let [status (some-> @!server hk/server-status)
          suppress? (suppress-http-kit-error? status msg ex)]
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
  "GET /health — return agent + session counts, uptime, bridge health.
   Reads both live registry and config snapshot, reports the larger agent count.
   Live registry reflects HTTP-registered and federated agents;
   config snapshot reflects agents wired at startup.
   (Evidence total-count removed 2026-07-04 — unbounded XTDB scan, futon1a#5.)"
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
        ;; NOTE (futon1a#5, 2026-07-04): /health no longer reports a total evidence
        ;; count. `count* {}` is an UNBOUNDED full-store XTDB count (O(store)); on the
        ;; grown store it blew the 15s query-timeout, and /health is polled often
        ;; (liveness), so it flooded WARNs and blocked agent attach. The stat had NO
        ;; consumer and always degraded to -1 anyway. If store-size observability is
        ;; wanted later, add a cheap maintained/estimated counter as its own signal —
        ;; do NOT full-scan on a hot health poll. (Prior comment here noted the same
        ;; class of bug for the earlier future-based path; count* didn't go far enough.)
        irc-send-base (some-> (:irc-send-base config) str str/trim not-empty)
        irc-relay-configured? (fn? (:irc-send-fn config))
        queue-hardening (agency-invariants/queue-hardening-status)
        bridge (read-bridge-health)]
    (json-response 200 {"status" "ok"
                         "agents" (max live-count config-count)
                         "sessions" (count (persist/list-sessions {}))
                         "agent-summary" agent-summary
                         "irc-relay-configured" irc-relay-configured?
                         "irc-send-base" irc-send-base
                         "queue-hardening" queue-hardening
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
        limit (or (parse-int (get params "limit")) 100)
        subject (parse-subject params)
        evidence-type (parse-keyword (get params "type"))
        claim-type (parse-keyword (get params "claim-type"))
        include-ephemeral? (parse-bool (get params "include-ephemeral?"))
        pattern-id (parse-keyword (get params "pattern-id"))
        tags (parse-tags (get params "tag"))
        author (non-blank-string (get params "author"))
        session-id (non-blank-string (get params "session-id"))
        explicit-since (get params "since")
        explicit-before (get params "before")
        broad-page? (and (nil? explicit-since)
                         (nil? explicit-before)
                         (nil? session-id)
                         (nil? pattern-id))
        query (cond-> {}
                subject (assoc :query/subject subject)
                evidence-type (assoc :query/type evidence-type)
                claim-type (assoc :query/claim-type claim-type)
                author (assoc :query/author author)
                session-id (assoc :query/session-id session-id)
                pattern-id (assoc :query/pattern-id pattern-id)
                (or explicit-since broad-page?)
                (assoc :query/since (or explicit-since (default-evidence-since)))
                explicit-before
                (assoc :query/before explicit-before)
                (some? include-ephemeral?)
                (assoc :query/include-ephemeral? include-ephemeral?)
                (seq tags) (assoc :query/tags tags))
        backend-query (cond-> query
                        (and (int? limit)
                             (pos? limit))
                        (assoc :query/limit limit))
        evidence-store (evidence-store-for-config config)
        entries (cond->> (estore/query* evidence-store backend-query)
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
                author (assoc :query/author author)
                session-id (assoc :query/session-id session-id)
                pattern-id (assoc :query/pattern-id pattern-id)
                (get params "since") (assoc :query/since (get params "since"))
                (some? include-ephemeral?)
                (assoc :query/include-ephemeral? include-ephemeral?)
                (seq tags) (assoc :query/tags tags))
        evidence-store (evidence-store-for-config config)
        count* (estore/count* evidence-store query)]
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
    :store-timeout 503
    :store-unreachable 503
    :store-rejected 503
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
            result (boundary/append! evidence-store normalized)]
        (if (:ok result)
          (json-response 201 {:ok true
                              :evidence/id (get-in result [:entry :evidence/id])
                              :entry (:entry result)})
          (json-response (append-error-status (:error/code result))
                         {:ok false
                          :err (name (:error/code result))
                          :error result}))))))

;; =============================================================================
;; ArSE (Artificial Stack Exchange) endpoints — walkie-talkie surface
;; =============================================================================

(def ^:private arse-store-dir
  "Filesystem store directory for ArSE entities."
  (str (System/getProperty "user.home") "/code/storage/arse"))

(defn- arse-load-entities
  "Load ArSE entities from filesystem store."
  []
  (let [path (str arse-store-dir "/entities.json")]
    (if (.exists (io/file path))
      (json/parse-string (slurp path) true)
      [])))

(defn- arse-save-entities
  "Save ArSE entities to filesystem store."
  [entities]
  (let [dir (io/file arse-store-dir)]
    (.mkdirs dir)
    (spit (str arse-store-dir "/entities.json")
          (json/generate-string entities {:pretty true}))))

(defn- arse-update-manifest
  "Update ArSE manifest with current count and timestamp."
  [entity-count]
  (let [path (str arse-store-dir "/manifest.json")
        manifest (if (.exists (io/file path))
                   (json/parse-string (slurp path) true)
                   {})
        updated (assoc manifest
                       :entity_count entity-count
                       :last_updated (.toString (Instant/now)))]
    (spit path (json/generate-string updated {:pretty true}))))

(defn- arse-ask!
  [config {:keys [title question tags author]}]
  (let [title (or title "")
        question (or question "")
        tags (vec (or tags []))
        author (or author "agent")
        entities (arse-load-entities)]
    (if (str/blank? title)
      {:ok false :status 400 :err "title-required"
       :message "Question title is required"}
      (let [thread-id (str "ask-" (quot (System/currentTimeMillis) 1000)
                           "-" (count entities))
            entity {:entity/id thread-id
                    :entity/type "QAPair"
                    :entity/source "artificial-stack-exchange"
                    :title title
                    :question-body question
                    :answer-body ""
                    :tags tags
                    :score 0
                    :answer-score 0
                    ;; arse-ask! is the REAL-ask path: both callers (the /arse/ask
                    ;; HTTP endpoint and the typed-bell bridge) are genuine
                    ;; agent/human questions, NOT generated seed data. Synthetic
                    ;; seed QA is written by other means; provenance for bell-asks
                    ;; is carried by the "typed-bell" tag. (M-typed-bells, 2026-06-11)
                    :synthetic false
                    :source_node ""
                    :source_problem ""
                    :unanswered true
                    :author author}]
        (arse-save-entities (conj entities entity))
        (arse-update-manifest (inc (count entities)))
        (let [evidence-store (evidence-store-for-config config)
              q-id (str "arse-q-" thread-id)
              q-entry {:evidence-id q-id
                       :subject {:ref/type :arse-thread :ref/id thread-id}
                       :type :arse-qa
                       :claim-type :question
                       :author author
                       :body {:title title :text question}
                       :tags (mapv keyword tags)}
              result (boundary/append! evidence-store q-entry)]
          {:ok true
           :status 201
           :thread-id thread-id
           :evidence-id q-id
           :evidence-ok (:ok result)})))))

(defn- arse-answer!
  [config {:keys [thread-id answer author]}]
  (let [thread-id (or (nonblank-str thread-id) "")
        answer-text (or answer "")
        author (or author "agent")]
    (if (str/blank? thread-id)
      {:ok false :status 400 :err "thread-id-required"
       :message "thread-id is required"}
      (let [entities (arse-load-entities)
            target-idx (first (keep-indexed
                               (fn [i e]
                                 (when (= (or (:entity/id e) (:thread_id e))
                                          thread-id)
                                   i))
                               entities))]
        (if (nil? target-idx)
          {:ok false :status 404 :err "not-found"
           :message (str "Question not found: " thread-id)}
          (let [target (nth entities target-idx)
                updated (-> target
                            (assoc :answer-body answer-text)
                            (dissoc :unanswered))
                entities' (assoc (vec entities) target-idx updated)]
            (arse-save-entities entities')
            (let [evidence-store (evidence-store-for-config config)
                  q-id (str "arse-q-" thread-id)
                  a-id (str "arse-a-" thread-id)
                  a-entry {:evidence-id a-id
                           :subject {:ref/type :arse-thread :ref/id thread-id}
                           :type :arse-qa
                           :claim-type :conclusion
                           :author author
                           :body {:text answer-text}
                           :tags (mapv keyword (or (:tags target) []))
                           :in-reply-to q-id}
                  result (boundary/append! evidence-store a-entry)]
              {:ok true
               :status 200
               :thread-id thread-id
               :evidence-id a-id
               :evidence-ok (:ok result)})))))))

(defn- handle-arse-ask
  "POST /api/alpha/arse/ask — post a new ArSE question.
   Body: {\"title\": \"...\", \"question\": \"...\", \"tags\": [...], \"author\": \"...\"}
   Dual-writes to filesystem store + evidence landscape."
  [request config]
  (let [payload (parse-json-map (read-body request))]
    (if (nil? payload)
      (json-response 400 {:ok false :err "invalid-json"})
      (let [result (arse-ask! config {:title (:title payload)
                                      :question (:question payload)
                                      :tags (:tags payload)
                                      :author (:author payload)})]
        (json-response (:status result) result)))))

(defn- handle-arse-answer
  "POST /api/alpha/arse/answer — answer an existing ArSE question.
   Body: {\"thread-id\": \"...\", \"answer\": \"...\", \"author\": \"...\"}
   Dual-writes to filesystem store + evidence landscape."
  [request config]
  (let [payload (parse-json-map (read-body request))]
    (if (nil? payload)
      (json-response 400 {:ok false :err "invalid-json"})
      (let [result (arse-answer! config {:thread-id (or (:thread-id payload)
                                                        (get payload "thread-id"))
                                         :answer (:answer payload)
                                         :author (:author payload)})]
        (json-response (:status result) result)))))

(defn- handle-arse-unanswered
  "GET /api/alpha/arse/unanswered — list unanswered ArSE questions."
  [_request _config]
  (let [entities (arse-load-entities)
        unanswered (filterv :unanswered entities)]
    (json-response 200 {:ok true
                        :count (count unanswered)
                        :questions (mapv (fn [e]
                                          {:thread-id (or (:entity/id e) (:thread_id e))
                                           :title (:title e)
                                           :tags (:tags e)
                                           :author (:author e)})
                                        unanswered)})))

;; =============================================================================
;; Pattern search — futon3x tool surface
;; =============================================================================

(def ^:private patterns-tsv-path
  (str (System/getProperty "user.home") "/code/futon3/resources/sigils/patterns-index.tsv"))

(defonce ^:private !patterns-cache
  (atom nil))

(defn- load-patterns-tsv
  "Load and cache patterns from TSV. Returns vector of maps."
  []
  (or @!patterns-cache
      (let [f (io/file patterns-tsv-path)]
        (when (.exists f)
          (let [lines (->> (str/split-lines (slurp f))
                           (remove #(str/starts-with? % "#"))
                           (remove str/blank?))
                parsed (mapv (fn [line]
                               (let [cols (str/split line #"\t" -1)]
                                 {:pattern (nth cols 0 "")
                                  :tokipona (nth cols 1 "")
                                  :sigil (nth cols 2 "")
                                  :rationale (nth cols 3 "")
                                  :hotwords (nth cols 4 "")}))
                             lines)]
            (reset! !patterns-cache parsed)
            parsed)))))

(defn- search-patterns
  "Search patterns by keyword matching against pattern ID, rationale, and hotwords."
  [query limit]
  (let [patterns (or (load-patterns-tsv) [])
        terms (str/split (str/lower-case (str query)) #"\s+")
        scored (keep (fn [p]
                       (let [text (str/lower-case
                                   (str (:pattern p) " " (:rationale p) " " (:hotwords p)))
                             hits (count (filter #(str/includes? text %) terms))]
                         (when (pos? hits)
                           (assoc p :score hits))))
                     patterns)]
    (->> scored
         (sort-by :score >)
         (take (or limit 5))
         vec)))

(defn- handle-patterns-search
  "GET /api/alpha/patterns/search?q=...&limit=N — search pattern catalog."
  [request]
  (let [params (:query-string request)
        q (some-> params
                  (str/split #"&")
                  (->> (some (fn [p]
                               (when (str/starts-with? p "q=")
                                 (enc/decode-uri-component (subs p 2)))))))
        limit (some-> params
                      (str/split #"&")
                      (->> (some (fn [p]
                                   (when (str/starts-with? p "limit=")
                                     (try (Integer/parseInt (subs p 6))
                                          (catch Exception _ nil)))))))]
    (if-not q
      (json-response 400 {:ok false :err "missing-query"
                          :message "Provide ?q=<search terms>"})
      (let [results (search-patterns q limit)]
        (json-response 200 {:ok true
                            :query q
                            :count (count results)
                            :patterns results})))))

;; =============================================================================
;; Backpack persistence — survives server restarts
;; =============================================================================

(def ^:private backpack-file
  (str (System/getProperty "user.home") "/code/storage/futon3c/backpacks.json"))

(defonce ^:private !backpacks
  (atom (try
          (when (.exists (io/file backpack-file))
            (json/parse-string (slurp backpack-file) true))
          (catch Exception _ {}))))

(defn- save-backpacks!
  "Persist backpacks to disk."
  []
  (let [dir (io/file (str (System/getProperty "user.home") "/code/storage/futon3c"))]
    (.mkdirs dir)
    (spit backpack-file (json/generate-string @!backpacks {:pretty true}))))

(defn- backpack-put!
  "Store a pattern in an agent's backpack (memory + disk + registry)."
  [agent-id pattern-id psr-evidence-id]
  (let [entry {:active-pattern pattern-id
               :psr-evidence-id psr-evidence-id
               :psr-at (.toString (Instant/now))}]
    (swap! !backpacks assoc agent-id entry)
    (save-backpacks!)
    ;; Also update registry if agent is online
    (when (reg/agent-registered? agent-id)
      (reg/update-agent! agent-id
        :agent/metadata
        (merge (or (:agent/metadata (reg/get-agent agent-id)) {})
               {:backpack/active-pattern pattern-id
                :backpack/psr-evidence-id psr-evidence-id
                :backpack/psr-at (:psr-at entry)})))))

(defn- backpack-clear!
  "Clear the pattern from an agent's backpack (memory + disk + registry)."
  [agent-id]
  (swap! !backpacks dissoc agent-id)
  (save-backpacks!)
  ;; Also update registry if agent is online
  (when (reg/agent-registered? agent-id)
    (reg/update-agent! agent-id
      :agent/metadata
      (-> (or (:agent/metadata (reg/get-agent agent-id)) {})
          (dissoc :backpack/active-pattern
                  :backpack/psr-evidence-id
                  :backpack/psr-at)))))

(defn- backpack-get
  "Get an agent's backpack from persistent store (primary) or registry (fallback)."
  [agent-id]
  (or (get @!backpacks agent-id)
      (when-let [agent (reg/get-agent agent-id)]
        (let [m (or (:agent/metadata agent) {})]
          (when (:backpack/active-pattern m)
            {:active-pattern (:backpack/active-pattern m)
             :psr-evidence-id (:backpack/psr-evidence-id m)
             :psr-at (:backpack/psr-at m)})))))

;; =============================================================================
;; PSR/PUR/PAR endpoints — walkie-talkie evidence surface
;; =============================================================================

(defn- handle-psr
  "POST /api/alpha/evidence/psr — record a Pattern Selection Record.
   Body: {\"pattern-id\": \"agent/pause-is-not-failure\",
          \"query\": \"stuck on testing\",
          \"candidates\": [\"pattern-a\", \"pattern-b\"],
          \"rationale\": \"...\", \"confidence\": \"medium\",
          \"author\": \"claude-1\", \"session-id\": \"...\"}"
  [request config]
  (let [payload (parse-json-map (read-body request))]
    (if (nil? payload)
      (json-response 400 {:ok false :err "invalid-json"})
      (let [pattern-id (or (:pattern-id payload) (get payload "pattern-id") "")
            query (or (:query payload) (get payload "query") "")
            candidates (or (:candidates payload) (get payload "candidates") [])
            rationale (or (:rationale payload) (get payload "rationale") "")
            confidence (or (:confidence payload) (get payload "confidence") "medium")
            author (or (:author payload) (get payload "author") "agent")
            session-id (or (:session-id payload) (get payload "session-id"))
            sigil (or (:sigil payload) (get payload "sigil"))]
        (if (str/blank? pattern-id)
          (json-response 400 {:ok false :err "pattern-id-required"
                              :message "pattern-id is required"})
          (let [evidence-store (evidence-store-for-config config)
                e-id (str "psr-" (UUID/randomUUID))
                entry (cond-> {:evidence-id e-id
                               :subject {:ref/type :pattern
                                         :ref/id pattern-id}
                               :type :pattern-selection
                               :claim-type :observation
                               :author author
                               :pattern-id (keyword pattern-id)
                               :body {:query query
                                      :selected pattern-id
                                      :candidates candidates
                                      :rationale rationale
                                      :confidence confidence}
                               :tags [:psr]}
                        session-id (assoc :session-id session-id)
                        sigil (assoc-in [:body :sigil] sigil))
                result (boundary/append! evidence-store entry)]
            ;; Put pattern in agent's backpack (persisted to disk)
            (backpack-put! author pattern-id e-id)
            (json-response 201 {:ok true
                                :evidence-id e-id
                                :pattern-id pattern-id
                                :evidence-ok (:ok result)})))))))

(defn- handle-pur
  "POST /api/alpha/evidence/pur — record a Pattern Use Record.
   Body: {\"pattern-id\": \"agent/pause-is-not-failure\",
          \"outcome\": \"success\", \"actions\": \"...\",
          \"prediction-error\": \"low\",
          \"in-reply-to\": \"psr-...\",
          \"author\": \"claude-1\", \"session-id\": \"...\"}"
  [request config]
  (let [payload (parse-json-map (read-body request))]
    (if (nil? payload)
      (json-response 400 {:ok false :err "invalid-json"})
      (let [pattern-id (or (:pattern-id payload) (get payload "pattern-id") "")
            outcome (or (:outcome payload) (get payload "outcome") "")
            actions (or (:actions payload) (get payload "actions") "")
            expected (or (:expected payload) (get payload "expected") "")
            actual (or (:actual payload) (get payload "actual") "")
            prediction-error (or (:prediction-error payload) (get payload "prediction-error") "")
            notes (or (:notes payload) (get payload "notes") "")
            in-reply-to (or (:in-reply-to payload) (get payload "in-reply-to"))
            author (or (:author payload) (get payload "author") "agent")
            session-id (or (:session-id payload) (get payload "session-id"))]
        (if (str/blank? pattern-id)
          (json-response 400 {:ok false :err "pattern-id-required"
                              :message "pattern-id is required"})
          (let [evidence-store (evidence-store-for-config config)
                e-id (str "pur-" (UUID/randomUUID))
                entry (cond-> {:evidence-id e-id
                               :subject {:ref/type :pattern
                                         :ref/id pattern-id}
                               :type :pattern-outcome
                               :claim-type :conclusion
                               :author author
                               :pattern-id (keyword pattern-id)
                               :body {:outcome outcome
                                      :actions actions
                                      :expected expected
                                      :actual actual
                                      :prediction-error prediction-error
                                      :notes notes}
                               :tags [:pur]}
                        session-id (assoc :session-id session-id)
                        in-reply-to (assoc :in-reply-to in-reply-to))
                result (boundary/append! evidence-store entry)]
            ;; Clear pattern from agent's backpack (persisted to disk)
            (backpack-clear! author)
            (json-response 201 {:ok true
                                :evidence-id e-id
                                :pattern-id pattern-id
                                :evidence-ok (:ok result)})))))))

(defn- handle-par
  "POST /api/alpha/evidence/par — record a Post-Action Review.
   Body: {\"summary\": \"Fixed vitality endpoint\",
          \"patterns-used\": [{\"pattern\": \"...\", \"count\": 1}],
          \"what-went-well\": [\"...\"], \"what-could-improve\": [\"...\"],
          \"prediction-errors\": [{\"expected\": \"...\", \"actual\": \"...\", \"magnitude\": 0.5}],
          \"suggestions\": [\"...\"],
          \"commits\": [\"abc123\"], \"files-touched\": [\"src/foo.clj\"],
          \"author\": \"claude-1\", \"session-id\": \"...\"}"
  [request config]
  (let [payload (parse-json-map (read-body request))]
    (if (nil? payload)
      (json-response 400 {:ok false :err "invalid-json"})
      (let [summary (or (:summary payload) (get payload "summary") "")
            patterns-used (or (:patterns-used payload) (get payload "patterns-used") [])
            what-went-well (or (:what-went-well payload) (get payload "what-went-well") [])
            what-could-improve (or (:what-could-improve payload) (get payload "what-could-improve") [])
            prediction-errors (or (:prediction-errors payload) (get payload "prediction-errors") [])
            suggestions (or (:suggestions payload) (get payload "suggestions") [])
            commits (or (:commits payload) (get payload "commits") [])
            files-touched (or (:files-touched payload) (get payload "files-touched") [])
            author (or (:author payload) (get payload "author") "agent")
            session-id (or (:session-id payload) (get payload "session-id") "")]
        (if (str/blank? summary)
          (json-response 400 {:ok false :err "summary-required"
                              :message "summary is required"})
          (let [evidence-store (evidence-store-for-config config)
                e-id (str "par-" (UUID/randomUUID))
                entry (cond-> {:evidence-id e-id
                               :subject {:ref/type :session
                                         :ref/id (or session-id (str "par-" (UUID/randomUUID)))}
                               :type :reflection
                               :claim-type :observation
                               :author author
                               :body {:summary summary
                                      :patterns-used patterns-used
                                      :what-went-well what-went-well
                                      :what-could-improve what-could-improve
                                      :prediction-errors prediction-errors
                                      :suggestions suggestions
                                      :commits commits
                                      :files-touched files-touched}
                               :tags [:par]}
                        (not (str/blank? session-id))
                        (assoc :session-id session-id))
                result (boundary/append! evidence-store entry)]
            (json-response 201 {:ok true
                                :evidence-id e-id
                                :evidence-ok (:ok result)})))))))

(defn- handle-backpack
  "GET /api/alpha/backpack/:agent-id — view an agent's backpack contents.
   Reads from persistent store (survives restarts), not just registry."
  [agent-id]
  (let [bp (backpack-get agent-id)]
    (json-response 200 {:ok true
                        :agent-id agent-id
                        :backpack (or bp {})})))

;; =============================================================================
;; Agent registration endpoints
;; =============================================================================

(def ^:private default-capabilities
  {:claude [:explore :edit :test :coordination/execute]
   :codex  [:edit :test :coordination/execute]
   :zai    [:explore :edit :test :coordination/execute]
   :tickle [:mission-control :discipline :coordination/execute]})

(defn- project-dir-slug
  "Claude Code project-directory slug for an absolute path (/ and . become -)."
  [path]
  (str/replace (str path) #"[/.]" "-"))

(defn- claude-session-cwd
  "Return the cwd Claude resume needs for SESSION-ID, if the local transcript
   store has it. Claude resume lookup is project-directory scoped, so the
   returned cwd must be the one whose project slug matches the directory the
   transcript file LIVES IN — not necessarily the :cwd recorded inside the
   transcript: sessions that cd mid-conversation record child dirs there, and
   resuming from those looks in the wrong project directory and finds nothing
   (found live 2026-07-04: claude-10 post-crash-restore returned empty turns).
   Algorithm: walk the internal cwd's ancestors (self first) and return the
   first whose slug equals the transcript's parent directory name; fall back
   to the internal cwd when nothing matches (pre-fix behavior)."
  ([session-id]
   (claude-session-cwd
    (io/file (System/getProperty "user.home") ".claude" "projects")
    session-id))
  ([projects-root session-id]
   (when (and (string? session-id)
              (not (str/blank? session-id))
              (.exists ^java.io.File projects-root))
     (let [filename (str session-id ".jsonl")]
       (some
        (fn [^java.io.File f]
          (when (and (.isFile f) (= filename (.getName f)))
            (let [slug (.getName (.getParentFile f))
                  internal-cwd
                  (with-open [r (io/reader f)]
                    (some (fn [line]
                            (try
                              (let [parsed (json/parse-string line true)
                                    cwd (:cwd parsed)]
                                (when (and (string? cwd)
                                           (not (str/blank? cwd))
                                           (.isDirectory (io/file cwd)))
                                  cwd))
                              (catch Exception _ nil)))
                          (line-seq r)))]
              (when internal-cwd
                (or (loop [d (io/file internal-cwd)]
                      (when d
                        (if (= slug (project-dir-slug (.getPath d)))
                          (.getPath d)
                          (recur (.getParentFile d)))))
                    internal-cwd)))))
        (file-seq ^java.io.File projects-root))))))

(defn- default-session-file-for-agent
  "Return the default session-file path for AGENT-TYPE/AGENT-ID."
  [agent-type agent-id]
  (case agent-type
    :claude (format "/tmp/futon-session-id-%s" agent-id)
    :codex (format "/tmp/futon-codex-session-id-%s" agent-id)
    :zai (format "/tmp/futon-zai-session-id-%s" agent-id)
    nil))

(defn- make-session-id-atom
  "Create a session-id atom seeded from INITIAL-SESSION-ID or SESSION-FILE."
  [initial-session-id session-file]
  (atom (or initial-session-id
            (when (and session-file (.exists (java.io.File. session-file)))
              (some-> session-file slurp str/trim not-empty)))))

(defn- make-session-reset-fn
  "Create a reset hook that clears both persisted and in-memory session state."
  [session-file session-id-atom]
  (fn []
    (try
      (when session-id-atom
        (reset! session-id-atom nil))
      (when-let [file (some-> session-file java.io.File.)]
        (when (and (.exists ^java.io.File file)
                   (not (.delete ^java.io.File file)))
          (throw (ex-info "could not delete session file"
                          {:session-file session-file}))))
      {:ok true}
      (catch Exception e
        {:ok false
         :error (.getMessage e)}))))

(defn- make-local-agent-invoke-fn
  "Best-effort builder for a local invoke-fn for AGENT-TYPE."
  [agent-type {:keys [agent-id session-file initial-session-id requested-cwd emacs-socket session-id-atom model
                      evidence-store irc-send-fn]}]
  (let [sid-atom (or session-id-atom
                     (make-session-id-atom initial-session-id session-file))]
    (case agent-type
      :claude
      (try
        (require 'futon3c.dev)
        (when-let [make-fn (resolve 'futon3c.dev/make-claude-invoke-fn)]
          (@make-fn (cond-> {:agent-id agent-id
                             :session-file session-file
                             :session-id-atom sid-atom}
                      emacs-socket (assoc :emacs-socket emacs-socket)
                      model (assoc :model model)
                      requested-cwd (assoc :cwd requested-cwd))))
        (catch Throwable _ nil))

      :codex
      (try
        (require 'futon3c.dev)
        (when-let [make-fn (resolve 'futon3c.dev/make-codex-invoke-fn)]
          (@make-fn (cond-> {:agent-id agent-id
                             :session-file session-file
                             :session-id-atom sid-atom}
                      model (assoc :model model)
                      requested-cwd (assoc :cwd requested-cwd))))
        (catch Throwable _ nil))

      :zai
      (try
        (zai-api/make-invoke-fn
         (cond-> {:agent-id agent-id
                  :session-file session-file
                  :session-id-atom sid-atom
                  :initial-session-id initial-session-id
                  :evidence-store evidence-store
                  :irc-send-fn irc-send-fn}
           model (assoc :model model)
           requested-cwd (assoc :cwd requested-cwd)))
        (catch Throwable t
          (println (str "[zai] failed to build invoke-fn for " agent-id ": " (.getMessage t)))
          (flush)
          nil))

      nil)))

(defn- remote-home-refusal-response
  [agent-id]
  (json-response 409 {:ok false
                      :err "remote-home-local-registration-refused"
                      :message (str "Refusing to register remote-homed agent locally: "
                                    agent-id)
                      :agent-id agent-id}))

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
                           (get default-capabilities agent-type []))
            ;; the registrant's declared home federation point (e.g. the
            ;; laptop's codex-3 ws-bridge presence on the hub declares "oxf")
            ;; — drives site-grouping in the *agents* roster (AG-8 display)
            home-site (some-> (or (:home-site payload) (get payload "home-site"))
                              str str/trim str/lower-case not-empty)]
        (cond
          (or (nil? agent-id) (str/blank? (str agent-id)))
          (json-response 400 {:ok false :err "missing-agent-id"
                              :message "agent-id is required"})

          (nil? agent-type)
          (json-response 400 {:ok false :err "missing-type"
                              :message "type is required (claude, codex, zai, tickle, mock)"})

          (and proxy? (str/blank? (str origin-url)))
          (json-response 400 {:ok false :err "missing-origin-url"
                              :message "proxy registration requires origin-url"})

          (and (not origin-url)
               (federation/remote-homed-agent-id? agent-id))
          (remote-home-refusal-response agent-id)

          :else
          (if (and proxy? origin-url (not (str/blank? origin-url)))
            (let [result (federation/register-proxy-agent!
                          origin-url
                          agent-id
                          (merge
                           {:type agent-type
                            :capabilities capabilities
                            ;; origin's declared site (announce-to-peer! sends it) —
                            ;; the home-site source for bare, unqualified ids
                            :home-site home-site}
                           (select-keys payload
                                        [:status
                                         :session-id
                                         :campaign-id
                                         :mission-id
                                         :excursion-id
                                         :invoke-started-at
                                         :invoke-prompt-preview
                                         :invoke-activity])))]
              (if (:ok result)
                (json-response (if (= :registered (:action result)) 201 200)
                               {:ok true
                                :agent-id (:agent-id result)
                                :type (name agent-type)
                                :proxy true
                                :origin-url origin-url
                                :action (name (:action result))})
                (json-response 409 {:ok false
                                    :err (name (or (:action result) :registration-failed))
                                    :message (str "Could not register proxy: " agent-id)
                                    :detail result})))
            (let [invoke-fn (if ws-bridge?
                              nil
                              (fn [_prompt _session-id]
                                {:result "registered-via-http" :session-id nil}))
                  result (reg/register-agent!
                          {:agent-id {:id/value (str agent-id) :id/type :continuity}
                           :type agent-type
                           :invoke-fn invoke-fn
                           :capabilities capabilities
                           :metadata (cond-> {}
                                       ws-bridge? (assoc :ws-bridge? true)
                                       home-site (assoc :home-site (keyword home-site)))})]
              (if (and (map? result) (= false (:ok result)))
                (json-response 409 {:ok false
                                    :err "duplicate-registration"
                                    :message (str "Agent already registered: " agent-id)
                                    :detail result})
                (if (and (map? result) (:agent/id result))
                  (json-response 201 {:ok true
                                      :agent-id (get-in result [:agent/id :id/value])
                                      :type (name (:agent/type result))
                                      :proxy false
                                      :ws-bridge ws-bridge?})
                  (json-response 409 {:ok false
                                      :err "registration-failed"
                                      :message (str "Could not register: " agent-id)
                                      :detail result}))))))))))

(defn- handle-agents-auto-register
  "POST /api/alpha/agents/auto — allocate and register next available agent.
   Body: {\"type\": \"claude\", \"session-id\": \"...\", \"cwd\": \"...\"} — type is required.
   Finds the next unused ID (e.g. claude-2 if claude-1 exists) and registers it
   with a real invoke-fn (resolved from dev.clj's local factories).
   Each call creates a new, independent agent (I-1: one agent = one identity)."
  [request config]
  (let [payload (parse-json-map (read-body request))]
    (cond
      (nil? payload)
      (json-response 400 {:ok false :err "invalid-json"})

      :else
      (let [agent-type-str (or (:type payload) (get payload "type"))
            agent-type (parse-keyword agent-type-str)]
        (if (nil? agent-type)
          (json-response 400 {:ok false :err "missing-type"
                              :message "type is required"})
          (let [prefix (name agent-type)
                ghost (reg/find-reclaimable-agent agent-type)
                agent-id (or ghost
                             (let [matching-ids (->> (reg/registered-agents)
                                                     (map :id/value)
                                                     (filter #(str/starts-with? (str %) (str prefix "-")))
                                                     set)
                                   next-n (loop [n 1]
                                            (if (contains? matching-ids (str prefix "-" n))
                                              (recur (inc n))
                                              n))]
                               (str prefix "-" next-n)))]
            (if (federation/remote-homed-agent-id? agent-id)
              (remote-home-refusal-response agent-id)
              (let [initial-session-id (some-> (or (:session-id payload)
                                                   (get payload "session-id"))
                                               str
                                               str/trim
                                               not-empty)
                    requested-cwd (some-> (or (:cwd payload)
                                              (get payload "cwd"))
                                          str
                                          str/trim
                                          not-empty)
                    campaign-id (some-> (or (:campaign-id payload)
                                            (get payload "campaign-id"))
                                        str
                                        str/trim
                                        not-empty)
                    mission-id (some-> (or (:mission-id payload)
                                           (get payload "mission-id"))
                                       str
                                       str/trim
                                       not-empty)
                    excursion-id (some-> (or (:excursion-id payload)
                                             (get payload "excursion-id"))
                                         str
                                         str/trim
                                         not-empty)
                    emacs-socket (or (:emacs-socket payload) (get payload "emacs-socket"))
                    session-file (default-session-file-for-agent agent-type agent-id)]
                (when ghost
                  (when-let [stale-sf (case agent-type
                                        :claude (format "/tmp/futon-session-id-%s" ghost)
                                        :codex (format "/tmp/futon-codex-session-id-%s" ghost)
                                        :zai (format "/tmp/futon-zai-session-id-%s" ghost)
                                        nil)]
                    (let [f (java.io.File. stale-sf)]
                      (when (.exists f) (.delete f))))
                  (reg/unregister-agent! ghost))
                ;; I-1: a fresh auto-register (no initial-session-id) must not
                ;; silently inherit orphan session state from a prior incarnation.
                (when (and session-file (nil? initial-session-id))
                  (let [f (java.io.File. session-file)]
                    (when (.exists f) (.delete f))))
                (let [sid-atom (make-session-id-atom initial-session-id session-file)
                      session-reset-fn (make-session-reset-fn session-file sid-atom)
                      invoke-fn (make-local-agent-invoke-fn
                                 agent-type
                                 {:agent-id agent-id
                                  :session-file session-file
                                  :initial-session-id initial-session-id
                                  :session-id-atom sid-atom
                                  :requested-cwd requested-cwd
                                  :emacs-socket emacs-socket
                                  :evidence-store (evidence-store-for-config config)
                                  :irc-send-fn (:irc-send-fn config)})
                      result (when invoke-fn
                               (reg/register-agent!
                                {:agent-id {:id/value agent-id :id/type :continuity}
                                 :type agent-type
                                 :invoke-fn invoke-fn
                                 :session-reset-fn session-reset-fn
                                 :capabilities (get default-capabilities agent-type [])
                                 :metadata (cond-> {:auto-registered? true}
                                             (= agent-type :codex) (assoc :require-execution? true)
                                             requested-cwd (assoc :cwd requested-cwd)
                                             campaign-id (assoc :campaign-id campaign-id)
                                             mission-id (assoc :mission-id mission-id)
                                             excursion-id (assoc :excursion-id excursion-id)
                                             emacs-socket (assoc :emacs-socket emacs-socket))}))]
                  (when (and (map? result) (:agent/id result))
                    (when invoke-fn
                      (reg/update-agent! agent-id :agent/invoke-fn invoke-fn))
                    (when initial-session-id
                      (when session-file
                        (spit session-file initial-session-id))
                      (reg/update-agent! agent-id :agent/session-id initial-session-id)))
                  (if (and (map? result) (:agent/id result))
                    (json-response 201 {:ok true
                                        :agent-id agent-id
                                        :type (name agent-type)
                                        :session-id initial-session-id
                                        :session-file session-file
                                        :cwd requested-cwd})
                    (json-response 409 {:ok false
                                        :err "registration-failed"
                                        :message (str "Could not register: " agent-id)})))))))))))

(defn- handle-agent-restore
  "POST /api/alpha/agents/restore — rehydrate or refresh an exact agent identity.
   Body: {\"agent-id\": \"claude-17\", \"type\": \"claude\", \"session-id\": \"...\",
          \"session-file\": \"/tmp/futon-session-id-claude-17\", \"cwd\": \"...\",
          \"emacs-socket\": \"server\"}
   If the agent is already live, refresh its invoke-fn/session metadata.
   If it is missing, recreate the exact identity instead of allocating a new one."
  [request config]
  (let [payload (parse-json-map (read-body request))]
    (if (nil? payload)
      (json-response 400 {:ok false :err "invalid-json"})
      (let [agent-id (some-> (or (:agent-id payload) (get payload "agent-id"))
                             str str/trim not-empty)
            agent-type (some-> (or (:type payload) (get payload "type"))
                               parse-keyword)
            initial-session-id (some-> (or (:session-id payload)
                                           (get payload "session-id"))
                                       str str/trim not-empty)
            requested-cwd (some-> (or (:cwd payload) (get payload "cwd"))
                                  str str/trim not-empty)
            campaign-id (some-> (or (:campaign-id payload)
                                    (get payload "campaign-id"))
                                str str/trim not-empty)
            mission-id (some-> (or (:mission-id payload)
                                   (get payload "mission-id"))
                               str str/trim not-empty)
            excursion-id (some-> (or (:excursion-id payload)
                                     (get payload "excursion-id"))
                                 str str/trim not-empty)
            emacs-socket (some-> (or (:emacs-socket payload)
                                     (get payload "emacs-socket"))
                                 str str/trim not-empty)
            model (some-> (or (:model payload) (get payload "model"))
                          str str/trim not-empty)
            raw-metadata (or (:metadata payload) (get payload "metadata") {})
            raw-contracts (or (:agency/contracts payload)
                              (get payload "agency/contracts")
                              (:agency/contracts raw-metadata)
                              (get raw-metadata "agency/contracts"))
            restored-detached? (true? (or (:restored-detached? payload)
                                          (get payload "restored-detached?")))
            session-file (some-> (or (:session-file payload)
                                     (get payload "session-file")
                                     (default-session-file-for-agent agent-type agent-id))
                                 str str/trim not-empty)
            existing (when agent-id (reg/get-agent agent-id))
            restore-session-id (or initial-session-id
                                   (:agent/session-id existing))
            effective-cwd (or (when (= :claude agent-type)
                                (claude-session-cwd restore-session-id))
                              requested-cwd)]
        (cond
          (nil? agent-id)
          (json-response 400 {:ok false :err "missing-agent-id"
                              :message "agent-id is required"})

          (nil? agent-type)
          (json-response 400 {:ok false :err "missing-type"
                              :message "type is required"})

          (federation/remote-homed-agent-id? agent-id)
          (remote-home-refusal-response agent-id)

          :else
          (let [sid-atom (make-session-id-atom initial-session-id session-file)
                session-reset-fn (make-session-reset-fn session-file sid-atom)
                invoke-fn (make-local-agent-invoke-fn
                           agent-type
                           {:agent-id agent-id
                            :session-file session-file
                            :initial-session-id initial-session-id
                            :session-id-atom sid-atom
                            :requested-cwd effective-cwd
                            :emacs-socket emacs-socket
                            :model model
                            :evidence-store (evidence-store-for-config config)
                            :irc-send-fn (:irc-send-fn config)})
                metadata (cond-> (merge {:auto-registered? true}
                                        (when (map? raw-metadata) raw-metadata))
                           (= agent-type :codex) (assoc :require-execution? true)
                           session-file (assoc :session-file session-file)
                           effective-cwd (assoc :cwd effective-cwd)
                           campaign-id (assoc :campaign-id campaign-id)
                           mission-id (assoc :mission-id mission-id)
                           excursion-id (assoc :excursion-id excursion-id)
                           emacs-socket (assoc :emacs-socket emacs-socket)
                           model (assoc :model model)
                           (seq raw-contracts) (assoc :agency/contracts raw-contracts)
                           restored-detached? (assoc :restore/state :restored/detached
                                                     :restore/restored-at (str (java.time.Instant/now))))]
            (if (nil? invoke-fn)
              (json-response 500 {:ok false
                                  :err "restore-failed"
                                  :message (str "Could not build invoke-fn for " agent-id)})
              (let [existing existing]
                (when (and session-file initial-session-id)
                  (spit session-file initial-session-id))
                (if existing
                  (let [effective-session-id (or initial-session-id
                                                 (:agent/session-id existing))
                        metadata* (cond-> (merge (or (:agent/metadata existing) {})
                                                 metadata)
                                    true (dissoc :remote? "remote?"
                                                 :proxy? "proxy?"
                                                 :remote-proxy? "remote-proxy?"
                                                 :origin-url "origin-url"
                                                 :ws-bridge? "ws-bridge?"
                                                 :note "note")
                                    (nil? campaign-id) (dissoc :campaign-id "campaign-id")
                                    (nil? mission-id) (dissoc :mission-id "mission-id")
                                    (nil? excursion-id) (dissoc :excursion-id "excursion-id"))
                        result (reg/update-agent!
                                agent-id
                                :agent/type agent-type
                                :agent/invoke-fn invoke-fn
                                :agent/session-reset-fn session-reset-fn
                                :agent/capabilities (get default-capabilities agent-type [])
                                :agent/metadata metadata*
                                :agent/session-id effective-session-id
                                :agent/status (if restored-detached?
                                                :restored
                                                (:agent/status existing)))]
                    (when (and session-file effective-session-id)
                      (spit session-file effective-session-id))
                    (if (and (map? result) (= false (:ok result)))
                      (json-response 409 {:ok false
                                          :err "restore-failed"
                                          :message (str "Could not refresh: " agent-id)
                                          :detail result})
                      (json-response 200 {:ok true
                                          :action "updated"
                                          :agent-id agent-id
                                          :type (name agent-type)
                                          :session-id effective-session-id
                                          :session-file session-file
                                          :cwd effective-cwd})))
                  (let [result (reg/register-agent!
                                {:agent-id {:id/value agent-id :id/type :continuity}
                                 :type agent-type
                                 :invoke-fn invoke-fn
                                 :session-reset-fn session-reset-fn
                                 :capabilities (get default-capabilities agent-type [])
                                 :metadata metadata})]
                    (when (and (map? result) (:agent/id result))
                      (when initial-session-id
                        (reg/update-agent! agent-id :agent/session-id initial-session-id))
                      (when restored-detached?
                        (reg/update-agent! agent-id :agent/status :restored)))
                    (if (and (map? result) (:agent/id result))
                      (json-response 201 {:ok true
                                          :action "registered"
                                          :agent-id agent-id
                                          :type (name agent-type)
                                          :session-id initial-session-id
                                          :session-file session-file
                                          :cwd effective-cwd})
                      (json-response 409 {:ok false
                                          :err "restore-failed"
                                          :message (str "Could not restore: " agent-id)
                                          :detail result}))))))))))))

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
            (if-let [invoke-fn (make-local-agent-invoke-fn
                                :claude
                                {:agent-id agent-id
                                 :session-file (default-session-file-for-agent :claude agent-id)
                                 :initial-session-id (:agent/session-id agent)
                                 :emacs-socket emacs-socket})]
              (do
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
  "Historical emacs-chat transport envelope emitter.

   Disabled by policy: the Evidence Landscape is reserved for primary,
   semantic research records rather than transport-level prompt/response
   envelopes. Semantic turn evidence is recorded elsewhere (`chat-turn`,
   `context-retrieval`, invoke lifecycle, etc.)."
  [_evidence-store _author _text _session-id & {:keys [_mission-id]}]
  nil)

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
          (boundary/append! evidence-store
                          {:evidence/id (str "e-review-" (UUID/randomUUID))
                           :evidence/subject {:ref/type :portfolio :ref/id "global"}
                           :evidence/type :coordination
                           :evidence/claim-type :observation
                           :evidence/author (or author "mission-control")
                           :evidence/at (str (Instant/now))
                           :evidence/body {:event :portfolio-review-snapshot
                                           :portfolio/missions compact-missions
                                           :portfolio/summary summary
                                           :portfolio/coverage coverage}
                           :evidence/tags [:review :portfolio-snapshot]}))
        (catch Exception e
          (println (str "[review] snapshot emit warning: " (.getMessage e))))))))

;; --- Provenance edge (E-patch-agent-evidence-leaks) ---------------------------
;; Every mesh-injected turn is born carrying its coordination edge so no downstream
;; consumer can mistake a non-operator turn for an operator one. From/To/Origin are
;; TOTAL (always stamped); the keep/drop rule is the single predicate
;; "operator ∈ {From,To}". Origin resolves the role of From and never returns nil.
(def ^:private harness-callers
  #{"auto-bellback" "auto" "system" "cron" "heartbeat" "apm-harvest" "claude-loop"})

(defn- resolve-origin
  "Resolve the provenance role of a turn's author (the From endpoint):
   \"operator\" (Joe) | \"harness\" (automated agency/harness senders) | \"agent\"
   (a named agent, or any other programmatic caller — the safe non-operator default).
   Total: always returns one of the three."
  [caller surface]
  (let [c (some-> caller str str/trim str/lower-case)
        s (some-> surface str str/trim str/lower-case)]
    (cond
      (or (= c "joe") (= c "joe-repl"))                 "operator"
      (or (contains? harness-callers c)
          (= s "auto-bellback"))                        "harness"
      :else                                             "agent")))

(defn- wrap-surface-header
  "Prepend an authoritative surface header to PROMPT when SURFACE is non-nil.
   This ensures the agent sees a consistent, unambiguous surface declaration
   on every turn — even when session history has messages from other surfaces.
   Also includes the agent's pattern backpack if any patterns are active."
  ([prompt surface caller]
   (wrap-surface-header prompt surface caller nil nil))
  ([prompt surface caller agent-id]
   (wrap-surface-header prompt surface caller agent-id nil))
  ([prompt surface caller agent-id thread]
   (if (and surface (not (str/blank? (str surface))))
     (let [backpack (when agent-id
                      (some-> (get @reg/!registry (str agent-id))
                              :agent/metadata :backpack seq))
           ;; Will this turn's response be auto-delivered back to the caller? If so,
           ;; the agent must NOT also manually bell the caller (double-delivery — the
           ;; 2026-06-26 claude-11 dup). Drives the reply contract below.
           auto-routes? (and (:bell-id thread)
                             (reply-auto-routes? agent-id caller))]
       (str "--- CURRENT TURN ---\n"
            "Surface: " surface "\n"
            ;; Provenance edge (E-patch-agent-evidence-leaks): From/To/Origin are TOTAL —
            ;; always stamped, so an unmarked turn can never default to operator. Edge is the
            ;; join-key into the durable coordination ledger (= job-id; see record-invoke-edge!).
            "From: " (let [c (some-> caller str str/trim)]
                       (if (str/blank? c) "unknown-agent" c)) "\n"
            "To: " (let [t (some-> agent-id str str/trim)]
                     (if (str/blank? t) "unknown-agent" t)) "\n"
            "Origin: " (resolve-origin caller surface) "\n"
            (when-let [edge (:edge thread)]
              (str "Edge: " edge "\n"))
            ;; Caller retained as a legacy alias of From for back-compat with existing parsers.
            (when (and caller (not (str/blank? (str caller))))
              (str "Caller: " caller "\n"))
            ;; Reply-delivery contract: when the response auto-routes, say so EXPLICITLY
            ;; and forbid a manual re-send — independent of bell-router, since the dup
            ;; happened with bell-router off (the agent re-sent defensively).
            (when auto-routes?
              (str "Reply delivery: your response this turn is delivered back to " caller
                   " automatically as a completion bell. Just respond — do NOT also "
                   "bell/whistle " caller " to deliver the same answer (that double-delivers; "
                   "bell " caller " yourself only to open a genuinely NEW thread).\n"))
            ;; bell-router: thread context so the recipient can thread the bell and
            ;; reply IN-THREAD (no crossing). NEW request shows the id to reply-to —
            ;; but only instructs a MANUAL reply-bell when the response won't auto-route.
            (when (and (bell-router-enabled?) (:bell-id thread))
              (cond
                (:in-reply-to thread)
                (str "Thread: bell `" (:bell-id thread) "` — REPLY to bell `"
                     (:in-reply-to thread) "`\n")
                auto-routes?
                (str "Thread: bell `" (:bell-id thread) "` — NEW request from " caller
                     ". Just respond to answer in-thread (auto-routes back).\n")
                :else
                (str "Thread: bell `" (:bell-id thread) "` — NEW request. To answer "
                     "in-thread, bell/whistle " caller " with in-reply-to=`"
                     (:bell-id thread) "`.\n")))
            (when (and (typed-bells-enabled?) (:type thread))
              (str "Type: " (name (:type thread))
                   (when (:ref thread)
                     (str " — help resolve ArSE `" (:ref thread) "`"))
                   "\n"))
            (when (seq backpack)
              (str "Backpack: "
                   (str/join ", " (map (fn [{:keys [sigil pattern]}]
                                         (str "[" sigil "] " pattern))
                                       backpack))
                   "\n"))
            "---\n\n"
            prompt))
     prompt)))

(defn- emacs-buffer-summary
  [projection]
  (let [summary (:buffer-summary projection)
        buffer-surface (:buffer-surface projection)
        buffer (:buffer buffer-surface)
        user-cursor (:user-cursor buffer-surface)
        agent-cursor (:agent-cursor buffer-surface)]
    (or (some-> summary str str/trim not-empty)
        (when (map? buffer-surface)
          (format "buffer=%s user=(line %s col %s point %s) remote=%s"
                  (or (:name buffer) "?")
                  (or (:line user-cursor) "?")
                  (or (:column user-cursor) "?")
                  (or (:point user-cursor) "?")
                  (if (map? agent-cursor)
                    (format "(line %s col %s point %s)"
                            (or (:line agent-cursor) "?")
                            (or (:column agent-cursor) "?")
                            (or (:point agent-cursor) "?"))
                    "nil"))))))

(defn- surface-projection-block
  "Render an agent-facing live surface projection block, if any."
  [agent-id]
  (when-let [projection (and agent-id
                             (reg/current-surface-projection (str agent-id)))]
    (when (= "emacs-cursor" (:surface projection))
      (str "Live surface projection:\n"
           "- Surface: Emacs smart cursor\n"
           (when-let [editor-id (some-> (:editor-id projection) str str/trim not-empty)]
             (str "- Editor: " editor-id "\n"))
           (when-let [mode (some-> (:mode projection) str str/trim not-empty)]
             (str "- Cursor mode: " mode "\n"))
           (when-let [buffer-summary (emacs-buffer-summary projection)]
             (str "- Read surface `buffer`: " buffer-summary "\n"))
           "- Write surface `minibuffer`: emit lines `MINIBUFFER: <text-or-json>` in your final response to send commands or messages back to Emacs.\n"
           "- Structured command: `MINIBUFFER: {\"command\":\"eval-sexp\",\"sexp\":\"(with-current-buffer \\\"*scratch*\\\" (buffer-name))\"}`.\n"
           "- E2E script command: `MINIBUFFER: {\"command\":\"run-script\",\"steps\":[{\"op\":\"switch-buffer\",\"buffer\":\"*codex-repl:codex-8*\"},{\"op\":\"forward-line\",\"count\":2},{\"op\":\"snapshot\"}]}`.\n"
           "- Example: `MINIBUFFER: {\"command\":\"refresh-context\"}` or `MINIBUFFER: Inspect current defun`.\n\n"))))

(defn- wrap-agent-facing-surface
  "Apply authoritative surface header plus any live agent-facing projection."
  ([prompt surface caller agent-id]
   (wrap-agent-facing-surface prompt surface caller agent-id nil))
  ([prompt surface caller agent-id thread]
   (str (wrap-surface-header "" surface caller agent-id thread)
        (or (surface-projection-block agent-id) "")
        prompt)))

(defn- parse-minibuffer-directive
  [raw]
  (let [trimmed (some-> raw str str/trim)]
    (cond
      (str/blank? trimmed) nil
      (and (str/starts-with? trimmed "{")
           (str/ends-with? trimmed "}"))
      (try
        (let [parsed (json/parse-string trimmed true)]
          (when (map? parsed)
            parsed))
        (catch Exception _
          {:command "message"
           :message trimmed
           :prompt trimmed}))
      :else
      {:command "message"
       :message trimmed
       :prompt trimmed})))

(defn- extract-minibuffer-directives
  "Split RESULT-TEXT into visible text and routed MINIBUFFER directives."
  [result-text]
  (let [lines (str/split-lines (or result-text ""))]
    (reduce (fn [{:keys [visible directives]} line]
              (if-let [[_ payload] (re-matches #"^\s*MINIBUFFER:\s*(.+?)\s*$" line)]
                (if-let [directive (parse-minibuffer-directive payload)]
                  {:visible visible
                   :directives (conj directives directive)}
                  {:visible visible
                   :directives directives})
                {:visible (conj visible line)
                 :directives directives}))
            {:visible []
             :directives []}
            lines)))

(defn- maybe-route-surface-writes
  "Route agent-authored surface write directives and strip them from RESULT.
   Today this supports the canonical Emacs smart-cursor minibuffer seam."
  [agent-id result]
  (if-not (and (:ok result) (string? (:result result)))
    result
    (if-let [projection (and agent-id
                             (reg/current-surface-projection (str agent-id)))]
      (if (= "emacs-cursor" (:surface projection))
        (let [{:keys [visible directives]} (extract-minibuffer-directives (:result result))]
          (if (seq directives)
            (let [routed (doall
                          (map-indexed
                           (fn [idx directive]
                             (peripheral-events/send-peripheral-event!
                              (str agent-id)
                              :emacs-cursor
                              :minibuffer
                              (-> (cond-> directive
                                    (nil? (:request-id directive))
                                    (assoc :request-id (str "minibuffer-" idx)))
                                  (assoc :server-sent-at-ms
                                         (System/currentTimeMillis)))))
                           directives))
                  visible-result (str/join "\n" visible)]
              (cond-> (assoc result :result visible-result)
                true
                (update :invoke-meta #(assoc (or % {})
                                             :surface-write {:minibuffer-events (count directives)
                                                             :routed-events (count (filter true? routed))}))))
            result))
        result)
      result)))

(def ^:private task-mode-re
  #"(?i)\bmode:\s*task\b")

(def ^:private mission-work-re
  #"(?i)\b(task assignment|fm-\d{3}|falsify|prove|counterexample|state of play)\b")

(def ^:private planning-only-re
  #"(?i)\b(planning-only|not started|need clarification|need more context|cannot execute yet|blocked)\b")

(def ^:private bash-tool-name-re
  "Matches tool names that represent shell/command execution (Bash, bash, etc.)."
  #"(?i)\bbash\b")

(defn- tool-use-event?
  "True when a recorded job event is a tool_use event."
  [event]
  (= "tool_use" (str (:type event))))

(defn- event-names-bash?
  "True when a tool_use event's payload names a Bash-ish tool."
  [event]
  (let [tools (:tools event)
        previews (:previews event)]
    (or (some #(re-find bash-tool-name-re (str %)) tools)
        (some #(re-find bash-tool-name-re (str %)) previews))))

(defn- count-tool-use-events
  "Count tool_use events in EVENTS (a job's :events vector).
   Returns {:tool-events n :command-events m} where command-events is the
   subset of tool_use events whose payload names a Bash-ish tool."
  [events]
  (let [tool-events (filter tool-use-event? events)
        tool-count (count tool-events)
        command-count (count (filter event-names-bash? tool-events))]
    {:tool-events (long tool-count)
     :command-events (long command-count)}))

(defn- execution-evidence-from-ledger
  "Best-effort: derive execution evidence from a job's recorded stream events.
   Returns nil when the job is unknown or has no tool_use events. The ledger
   is in-memory and lost on restart — this is corroboration only."
  [job-id]
  (when (some? job-id)
    (let [events (:events (get-in @!invoke-jobs-ledger [:jobs job-id]))
          {:keys [tool-events command-events]} (count-tool-use-events events)]
      (when (pos? tool-events)
        {:executed true
         :tool-events tool-events
         :command-events command-events}))))

(defn- invoke-execution-evidence
  "Extract execution evidence map from invoke result metadata.

   When JOB-ID is supplied and the self-reported execution evidence (from
   :invoke-meta) is absent or reports zero tool-events, consult the job's
   recorded stream events as a best-effort fallback. The fallback may only
   UPGRADE evidence (absent/zero -> observed-positive), never downgrade."
  ([result]
   (invoke-execution-evidence result nil))
  ([result job-id]
   (let [invoke-meta (:invoke-meta result)
         execution (or (:execution invoke-meta) (get invoke-meta "execution"))
         raw-executed (or (:executed? execution) (get execution "executed?")
                         (:executed execution) (get execution "executed"))
         executed (cond
                    (boolean? raw-executed) raw-executed
                    (string? raw-executed) (boolean (parse-bool raw-executed))
                    :else (boolean raw-executed))
         tool-events (long (or (:tool-events execution) (get execution "tool-events") 0))
         command-events (long (or (:command-events execution) (get execution "command-events") 0))
         self-reported-positive? (or executed
                                     (pos? tool-events)
                                     (pos? command-events))
         fallback (when (not self-reported-positive?)
                    (execution-evidence-from-ledger job-id))]
     (cond
       (some? fallback) fallback
       :else {:executed executed
              :tool-events tool-events
              :command-events command-events}))))

(defn- enrich-result-with-stream-execution
  "Layer-1 source fix: populate :invoke-meta :execution on RESULT from the
   job's recorded tool_use stream events when the lane did not self-report
   execution telemetry (the claude lane currently does not; codex does).

   This runs BEFORE the execution gate (codex-task-no-execution?) so that an
   honest claude review that DID run tools is not refused. The ledger events
   are in-memory; on a fresh JVM the events are absent and this is a no-op,
   leaving the result unchanged. Mirrors the shape codex populates:
   {:executed true :tool-events n :command-events m}.

   Only UPGRADES — never overwrites existing self-reported execution evidence."
  [job-id result]
  (let [existing (or (get-in result [:invoke-meta :execution])
                     (get-in result [:invoke-meta "execution"]))
        existing-positive? (or (:executed? existing) (:executed existing)
                               (pos? (long (or (:tool-events existing) 0)))
                               (pos? (long (or (:command-events existing) 0))))]
    (if existing-positive?
      result
      (if-let [evidence (execution-evidence-from-ledger job-id)]
        (update result :invoke-meta
                #(assoc (or % {}) :execution evidence))
        result))))

(defn- agent-requires-execution?
  "Return true when the agent metadata requests execution evidence enforcement."
  [agent-id]
  (let [record (reg/get-agent agent-id)
        metadata (:agent/metadata record)
        aid (some-> agent-id str str/lower-case)]
    (boolean (or (get metadata :require-execution?)
                 (get metadata "require-execution?")
                 (and (string? aid)
                      (str/starts-with? aid "codex"))))))

(defn- codex-task-no-execution?
  "True when an execution-enforced agent returns a task-mode reply with no evidence.
   Optional REQUIRE-EXECUTION? bypasses metadata lookup for pure tests. JOB-MODE,
   when supplied, is authoritative; prompt inference remains the legacy fallback."
  ([agent-id prompt result]
   (codex-task-no-execution? agent-id prompt result nil))
  ([agent-id prompt result require-execution?]
   (codex-task-no-execution? agent-id prompt result require-execution? nil))
  ([agent-id prompt result require-execution? job-mode]
   (let [text (some-> (:result result) str str/trim)
         {:keys [executed tool-events command-events]} (invoke-execution-evidence result)
         enforced? (if (some? require-execution?)
                     (boolean require-execution?)
                     (agent-requires-execution? agent-id))
         work-mode? (if (some? job-mode)
                      (= "work" (normalize-invoke-job-mode job-mode))
                      (let [p (str (or prompt ""))]
                        (or (boolean (re-find task-mode-re p))
                            (boolean (re-find mission-work-re p)))))]
    (and enforced?
         (string? prompt)
         work-mode?
         (string? text)
         (not (str/blank? text))
         (not (boolean (re-find planning-only-re text)))
         (not executed)
         (zero? tool-events)
         (zero? command-events)))))

(defn- result-error-message
  [result]
  (let [err (:error result)]
    (cond
      (map? err) (str (:error/message err))
      (some? err) (str err)
      :else "")))

(defn- claude-missing-conversation-result?
  [agent-id result]
  (and (not (:ok result))
       (string? agent-id)
       (str/starts-with? (str/lower-case agent-id) "claude")
       (boolean
        (re-find #"No conversation found with session ID:"
                 (result-error-message result)))))

(defn- invoke-agent-with-session-recovery!
  "Invoke AGENT-ID, retrying once for Claude's stale resume-session failure.

   The retry is intentionally narrow. File/incoming session precedence remains
   unchanged for normal invokes; only the Claude CLI's authoritative
   \"No conversation found\" response clears backing continuity."
  [agent-id prompt timeout-ms]
  (let [aid (str agent-id)
        first-result (reg/invoke-agent! aid prompt timeout-ms)]
    (if (claude-missing-conversation-result? aid first-result)
      (let [reset-result (reg/reset-session! aid)]
        (if (:ok reset-result)
          (let [retry-result (reg/invoke-agent! aid prompt timeout-ms)]
            (cond-> retry-result
              (map? retry-result)
              (assoc :session-recovery
                     {:reason "claude-missing-conversation"
                      :old-session-id (:old-session-id reset-result)})))
          first-result))
      first-result)))

(defn- preclock-dispatch!
  "Make a payload dispatch clock visible during the turn under the fallback
   [agent-id nil] key. Durable lineage still happens post-turn with the real
   session id."
  [agent-id mission-id]
  (when (some-> mission-id str str/trim not-empty)
    (try
      (clock-store/set-dispatch-mission! (str agent-id) nil mission-id)
      (catch Throwable t
        (println (str "[invoke] pre-clock failed: " (.getMessage t)))
        (flush)
        nil))))

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
      (register-job-worker! job-id (Thread/currentThread) nil)
      (preclock-dispatch! agent-id mission-id)
      (let [effective-prompt (wrap-agent-facing-surface prompt surface caller agent-id)
            raw-result (invoke-agent-with-session-recovery! (str agent-id) effective-prompt timeout-ms)
            result (maybe-route-surface-writes agent-id raw-result)
            result (enrich-result-with-stream-execution job-id result)
            sid (:session-id result)
            no-evidence? (and (:ok result)
                              (codex-task-no-execution?
                               agent-id effective-prompt result nil
                               (:mode (get-in (ensure-invoke-jobs-ledger!)
                                              [:jobs job-id]))))
            [terminal-state terminal-code terminal-message]
            (classify-terminal result no-evidence?)]
        ;; D1/O3 durable lineage: a successful dispatch with a mission-id clocks
        ;; the agent-session to that mission (in-RAM + async substrate write).
        (when (and mission-id sid (:ok result))
          (clock-lineage/clock-dispatch! (str agent-id) sid mission-id))
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
                                   (assoc :invoke-meta (:invoke-meta result))
                                   (:session-recovery result)
                                   (assoc :session-recovery (:session-recovery result)))))
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
                            :message (.getMessage t)}))
      (finally
       (unregister-job-worker! job-id)
       ;; Guarantee: every terminal outcome resets agent status.
       (try (reg/mark-agent-idle! (str agent-id)) (catch Throwable _))))))

(defn- run-invoke-job!
  "Execute a queued invoke job to terminal state.
   Used by async bell worker and can be reused by other async surfaces."
  [{:keys [job-id agent-id prompt caller surface timeout-ms mission-id evidence-store]}]
  (let [ev-opts (when mission-id [:mission-id mission-id])]
    (try
      (mark-invoke-job-running! job-id)
      ;; Register the worker thread for ceiling enforcement (interrupt at hard ceiling).
      (register-job-worker! job-id (Thread/currentThread) nil)
      (preclock-dispatch! agent-id mission-id)
      (let [thread (let [bell? (= "bell" (some-> surface str str/trim))
                         job   (when bell? (get-in (ensure-invoke-jobs-ledger!) [:jobs job-id]))]
                     ;; :edge (= job-id) is the join-key, carried on ALL surfaces; bell-router /
                     ;; typed-bell fields stay bell-only and flag-gated as before.
                     (cond-> {:edge job-id}
                       bell? (assoc :bell-id job-id
                                    :in-reply-to (:bellback-of job)
                                    :type (:bell-type job)
                                    :ref (:ref job))))
            effective-prompt (wrap-agent-facing-surface prompt surface caller agent-id thread)
            ;; Install a ledger-appending event sink for the duration of the
            ;; invoke so bell-seeded turns record text/tool_use events (the
            ;; invoke-stream path installs its own sink; bells had none, so
            ;; they ran unobservably). Composes with and restores any
            ;; existing sink rather than clobbering it.
            aid (str agent-id)
            prev-sink (reg/get-invoke-event-sink aid)
            _ (reg/set-invoke-event-sink!
               aid
               (fn [event]
                 (try (record-job-stream-event! job-id event) (catch Throwable _))
                 (when prev-sink (try (prev-sink event) (catch Throwable _)))))
            ;; Persist the raw caller prompt (not the surface-wrapped one)
            ;; into the job record for D-7 session rehydration.
            _ (try (record-job-stream-event! job-id {:type "prompt" :text prompt})
                   (catch Throwable _))
            raw-result (try
                         (invoke-agent-with-session-recovery! aid effective-prompt timeout-ms)
                         (finally
                           (if prev-sink
                             (reg/set-invoke-event-sink! aid prev-sink)
                             (reg/clear-invoke-event-sink! aid))))
            result (maybe-route-surface-writes agent-id raw-result)
            result (enrich-result-with-stream-execution job-id result)
            sid (:session-id result)
            no-evidence? (and (:ok result)
                              (codex-task-no-execution?
                               agent-id effective-prompt result nil
                               (:mode (get-in (ensure-invoke-jobs-ledger!)
                                              [:jobs job-id]))))
            [terminal-state terminal-code terminal-message]
            (classify-terminal result no-evidence?)]
        ;; D1/O3 durable lineage: a successful dispatch with a mission-id clocks
        ;; the agent-session to that mission (in-RAM + async substrate write).
        (when (and mission-id sid (:ok result))
          (clock-lineage/clock-dispatch! (str agent-id) sid mission-id))
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
         :message (.getMessage t)})
      (finally
       ;; Unregister worker (ceiling reaper no longer needs to track this job).
       (unregister-job-worker! job-id)
       ;; Guarantee: every terminal outcome resets agent status. The registry's
       ;; invoke-agent! wrapper normally handles this, but if the worker thread
       ;; is killed or the invoke-fn blocks past process boundaries, mark-idle!
       ;; may never run. This finally ensures no terminal path leaks :invoking.
       (try (reg/mark-agent-idle! (str agent-id)) (catch Throwable _))))))

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
                                  :message (.getMessage t)}))))))))


;; -- E-pilot-hop-trigger-wiring §(5): agency hop HTTP endpoints --------------
;;
;; Thin wrappers around futon3c.agency.registry/hop! and hop-back!.  Body:
;;   POST /api/alpha/agency/hop      {"agent-id": "claude-1", "to-peripheral": "street-sweeper"}
;;   POST /api/alpha/agency/hop-back {"agent-id": "claude-1"}
;;
;; Returns the registry result + (best-effort) the emitted hop-event id.

(defn- handle-agency-hop
  [request]
  (try
    (let [body  (parse-json-map (read-body request))
          aid   (some-> (get body :agent-id) str)
          peri  (some-> (get body :to-peripheral) str)]
      (cond
        (str/blank? aid)
        (json-response 400 {:ok false :error "missing-agent-id"})

        (str/blank? peri)
        (json-response 400 {:ok false :error "missing-to-peripheral"})

        :else
        (let [hop! (requiring-resolve 'futon3c.agency.registry/hop!)
              r    (hop! aid peri)]
          (json-response (if (:ok r) 200 409) r))))
    (catch Exception e
      (json-response 500 {:ok false :error (.getMessage e)}))))

(defn- handle-agency-hop-back
  [request]
  (try
    (let [body (parse-json-map (read-body request))
          aid  (some-> (get body :agent-id) str)]
      (if (str/blank? aid)
        (json-response 400 {:ok false :error "missing-agent-id"})
        (let [hop-back! (requiring-resolve 'futon3c.agency.registry/hop-back!)
              r         (hop-back! aid)]
          (json-response (if (:ok r) 200 409) r))))
    (catch Exception e
      (json-response 500 {:ok false :error (.getMessage e)}))))

(defn- bell-type-payload
  [payload]
  (or (:type payload) (get payload "type")
      (:bell-type payload) (get payload "bell-type")
      (:bell_type payload) (get payload "bell_type")))

(defn- bell-ref-payload
  [payload]
  (or (:ref payload) (get payload "ref")
      (:bell-ref payload) (get payload "bell-ref")
      (:bell_ref payload) (get payload "bell_ref")))

(defn- maybe-typed-bell-arse-bridge!
  [config {:keys [bell-type ref prompt caller]}]
  (case bell-type
    :query
    (if ref
      {:ok true :ref ref}
      (let [prompt-str (str prompt)
            result (arse-ask! config {:title (subs prompt-str 0 (min 96 (count prompt-str)))
                                      :question prompt-str
                                      :tags ["typed-bell"]
                                      :author caller})]
        (if (:ok result)
          {:ok true :ref (:thread-id result) :arse result}
          (assoc result :ok false))))

    :answer
    (let [result (arse-answer! config {:thread-id ref
                                       :answer (str prompt)
                                       :author caller})]
      (if (:ok result)
        {:ok true :ref ref :arse result}
        (assoc result :ok false)))

    {:ok true :ref ref}))

(defn- req-query-param [request k]
  (when-let [qs (:query-string request)]
    (some-> (re-find (re-pattern (str "(?:^|&)" (java.util.regex.Pattern/quote k) "=([^&]*)")) qs)
            second
            (java.net.URLDecoder/decode "UTF-8"))))

(defn- agent-in-flight-turn?
  "True when AGENT currently has an in-flight turn (the authoritative busy signal).
   Source of truth: registry-status computes :invoking from ALL turn paths —
   server-side warm-pouch invoke, cold fallback, bell-drained turns, REPL-streamed
   turns, and external codex surfaces. This is the reliable idle signal the elisp
   poller's agent-chat--streaming-started flag was not (E-park-delivery-losses bug 2)."
  [agent]
  (try
    (= :invoking (get-in (reg/registry-status) [:agents (str agent) :status]))
    (catch Throwable _ false)))

(defn- handle-parked-ready
  "GET /api/alpha/parked/ready?agent=&session= — poll-and-lease the ready resume
   prompts for a repl buffer (E-park-continuations Car 2b, polling path).

   Bug 2 fix (server-side busy gate): if the agent has an in-flight turn, WITHHOLD
   all ready items (return empty :ready with :withheld true) WITHOUT popping — the
   items are retained and delivered on a later poll when idle. This is the robust
   idle signal: the registry knows whether the session is mid-turn across ALL paths.
   Bug 3 fix (lease): pop is non-destructive — the item is leased until ACKed."
  [request _config]
  (let [agent (req-query-param request "agent")
        session (req-query-param request "session")]
    (if (and (parked-on-enabled?) agent (agent-in-flight-turn? agent))
      ;; Bug 2: agent is mid-turn — withhold items, don't pop. They'll be
      ;; delivered on a later poll when the agent is idle.
      (json-response 200 {:ok true :ready [] :withheld true})
      (let [leased (if (and (parked-on-enabled?) agent)
                     (parked-ready-pop! agent (or session ""))
                     nil)]
        (json-response 200 {:ok true
                            :ready (if leased [leased] [])
                            :leased (some? leased)})))))

(defn- handle-parked-ready-ack
  "POST /api/alpha/parked/ready/ack — confirm delivery of a leased ready resume
   (clear its lease so it is not redelivered). Body: {\"park-id\":\"...\"} or query
   param ?park-id=. Returns {:ok true :acked bool}."
  [request _config]
  (let [body (parse-json-map (read-body request))
        park-id (or (:park-id body) (get body "park-id")
                    (req-query-param request "park-id"))]
    (cond
      (not (parked-on-enabled?))
      (json-response 503 {:ok false :error "parked-on-disabled"})
      (str/blank? (str park-id))
      (json-response 400 {:ok false :error "park-id-required"})
      :else
      (let [acked (parked-on/ready-ack! park-id)]
        (json-response 200 {:ok true :acked acked})))))

(defn- handle-parked
  "GET /api/alpha/parked?agent=&session=&mode= — outstanding (unreleased) parks.

   The default view remains :within-turn because this endpoint also supplies the
   turn-finalization :more-pending signal.  mode=all is the operator visibility
   view and includes background parks, without letting them defer finalization."
  [request _config]
  (let [agent (req-query-param request "agent")
        session (req-query-param request "session")
        mode (req-query-param request "mode")
        all-modes? (= "all" (some-> mode str/trim str/lower-case))
        recs (when (and (parked-on-enabled?) agent)
               (->> (vals (:records (parked-on/snapshot)))
                    (filter (fn [r] (and (= (str (:agent r)) (str agent))
                                         (or (str/blank? (str session))
                                             (= (str (:session r)) (str session)))
                                         (not (:released? r))
                                         (or all-modes?
                                             (= (or (:mode r) :within-turn)
                                                :within-turn)))))
                    (mapv (fn [r] {:id (:id r) :awaiting (vec (:awaiting r))
                                   :deadline-ms (:deadline-ms r)
                                   :mode (or (:mode r) :within-turn)}))))
        ;; A ready resume already in the inbox (dep completed, poller not yet fired)
        ;; also means "more is coming" — so the check is race-free even for a fast dep.
        inbox-pending (and (parked-on-enabled?) agent
                           (parked-on/ready-inbox-pending? agent (or session "")
                                                           :within-turn))
        within-turn-pending (some #(= (:mode %) :within-turn) recs)]
    (json-response 200 {:ok true :parked (vec (or recs []))
                        :more-pending (boolean (or within-turn-pending inbox-pending))})))

(defn- handle-park
  "POST /api/alpha/park — register a continuation (E-repl-continuations Car 2b):
   park AGENT's turn on a JOIN of AWAITING dep-ids; resume once all are terminal.
   Body: {\"agent\":\"claude-1\",\"awaiting\":[\"<bell/job-id>\"...],\"payload\":\"...\",
          \"deadline-ms\":N,\"timer-due-ms\":N,\"budget\":{...}}. Flag-gated."
  [request _config]
  (let [payload (parse-json-map (read-body request))]
    (cond
      (not (parked-on-enabled?))
      (json-response 503 {:ok false :error "parked-on-disabled"
                          :message "set FUTON3C_PARKED_ON to enable"})
      (nil? payload)
      (json-response 400 {:ok false :error "invalid-json"
                          :message "Request body must be a JSON object"})
      (str/blank? (str (or (:agent payload) (get payload "agent"))))
      (json-response 400 {:ok false :error "agent-required"})
      :else
      (let [result (parked-on/park!
                    {:agent (str (or (:agent payload) (get payload "agent")))
                     :session (or (:session payload) (get payload "session"))
                     :surface (or (:surface payload) (get payload "surface"))
                     :awaiting (or (:awaiting payload) (get payload "awaiting") [])
                     :payload (or (:payload payload) (get payload "payload"))
                     :mode (or (parse-keyword (or (:mode payload) (get payload "mode")))
                               :within-turn)
                     :timer-due-ms (or (:timer-due-ms payload) (get payload "timer-due-ms"))
                     :deadline-ms (or (:deadline-ms payload) (get payload "deadline-ms"))
                     :budget (or (:budget payload) (get payload "budget"))}
                    {:ledger-lookup parked-job-lookup :resume! parked-resume!
                     :now-ms (System/currentTimeMillis)})]
        ;; show the new park in the *agents* pane immediately
        (try (bb/project-agents! (reg/registry-status)) (catch Throwable _ nil))
        (json-response 200 (assoc result :ok true))))))

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
            raw-mode (or (:mode payload) (get payload "mode"))
            mode (normalize-invoke-job-mode raw-mode)
            mission-id (or (:mission-id payload) (get payload "mission-id"))
            ;; bell-router: a bell that answers another bell carries its id, so the
            ;; conversation graph correlates the reply (not just auto-bellbacks).
            in-reply-to (or (:in-reply-to payload) (get payload "in-reply-to")
                            (:in_reply_to payload) (get payload "in_reply_to")
                            (:reply-to payload) (get payload "reply-to"))
            raw-bell-type (bell-type-payload payload)
            typed? (typed-bells-enabled?)
            bell-type (when typed? (normalize-bell-type raw-bell-type))
            ref (when typed? (nonblank-str (bell-ref-payload payload)))
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

          (and (some? raw-mode) (nil? mode))
          (json-response 400 {:ok false :err "invalid-invoke-mode"
                              :message "mode must be work or brief"})

          (and typed? (nil? bell-type))
          (json-response 400 {:ok false :err "invalid-bell-type"
                              :message (str "type must be one of "
                                            (str/join ", " (sort (map name allowed-bell-types))))})

          (and typed? (= :answer bell-type) (nil? ref))
          (json-response 400 {:ok false :err "answer-ref-required"
                              :message "type=answer requires ref"})

          :else
          (if-let [existing-job (when (and typed? (nonblank-str requested-job-id))
                                  (get-in (ensure-invoke-jobs-ledger!)
                                          [:jobs (nonblank-str requested-job-id)]))]
            (json-response 202 {:ok true
                                :accepted true
                                :reused? true
                                :job-id (:job-id existing-job)
                                :state (:state existing-job)
                                :bell-type (:bell-type existing-job)
                                :ref (:ref existing-job)
                                :status-url (str "/api/alpha/invoke/jobs/" (:job-id existing-job))})
            (let [bridge (if typed?
                           (maybe-typed-bell-arse-bridge!
                            config {:bell-type bell-type
                                    :ref ref
                                    :prompt prompt
                                    :caller caller})
                           {:ok true})
                  ref' (:ref bridge)]
              (if-not (:ok bridge)
                (json-response (or (:status bridge) 400)
                               (select-keys bridge [:ok :err :message :thread-id :evidence-id]))
                (let [job-id (create-invoke-job! {:requested-job-id requested-job-id
                                                  :agent-id agent-id
                                                  :prompt prompt
                                                  :caller caller
                                                  :surface surface
                                                  :mode mode
                                                  :bellback-of (when (bell-router-enabled?) in-reply-to)
                                                  :bell-type (when typed? bell-type)
                                                  :ref (when typed? ref')})
                      run-job (fn []
                                (run-invoke-job! {:job-id job-id
                                                  :agent-id agent-id
                                                  :prompt prompt
                                                  :caller caller
                                                  :surface surface
                                                  :timeout-ms timeout-ms
                                                  :mission-id mission-id
                                                  :evidence-store evidence-store}))
                      deliver-result (fn [result]
                                       ;; Bell delivery means caller can obtain terminal result via canonical job query.
                                       (record-invoke-job-delivery-by-job-id!
                                        job-id
                                        {:surface "bell"
                                         :destination (str "caller " caller " via /api/alpha/invoke/jobs/" job-id)
                                         :delivered? true
                                         :note (if (:ok result) "bell-job-ready" "bell-job-error")}))]
                  (try
                    (if (turn-queue/drainer-v2-enabled?)
                      ;; Drainer v2: enqueue to the agent's dedicated drainer thread and
                      ;; return — never hold a shared invoke-executor lane for the turn.
                      (let [r (turn-queue/accept-async!
                               {:to (str agent-id) :from caller :surface surface :prompt prompt
                                :process-fn (fn [_entry]
                                              (binding [turn-queue/*drained-by-outer* true]
                                                (run-job)))
                                :finalize-fn deliver-result})]
                        (when (= :deduped (:status r))
                          (finalize-invoke-job! job-id "deduped" "duplicate-msg-id" nil
                                                {:ok true :deduped true} nil)))
                      ;; Legacy path: run on the shared invoke-executor pool.
                      (.submit invoke-executor
                               ^Runnable
                               (fn [] (deliver-result (run-job)))))
                    (json-response 202 (cond-> {:ok true
                                                :accepted true
                                                :job-id job-id
                                                :state "queued"
                                                :mode (invoke-job-mode prompt mode)
                                                :status-url (str "/api/alpha/invoke/jobs/" job-id)}
                                         typed? (assoc :bell-type bell-type
                                                       :ref ref'
                                                       :arse (:arse bridge))))
                    (catch Throwable t
                      (finalize-invoke-job! job-id "failed" "invoke-submit-failed" (.getMessage t) {:ok false} nil)
                      (json-response 503 {:ok false
                                          :job-id job-id
                                          :error "invoke-submit-failed"
                                          :message (.getMessage t)}))))))))))))

(defn- handle-invoke-announce
  "POST /api/alpha/invoke/announce — record a canonical queued invoke before any
   external surface announces acceptance.
   Body: {\"agent-id\":\"codex-1\",\"prompt\":\"...\",\"job-id\":\"optional\"}
   Returns immediately with the canonical queued job id and status URL."
  [request _config]
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
                        "announce")
            requested-job-id (or (:job-id payload) (get payload "job-id")
                                 (:job_id payload) (get payload "job_id"))]
        (cond
          (or (nil? agent-id) (str/blank? (str agent-id)))
          (json-response 400 {:ok false :err "missing-agent-id"
                              :message "agent-id is required"})

          (nil? prompt)
          (json-response 400 {:ok false :err "missing-prompt"
                              :message "prompt is required"})

          (nil? (reg/get-agent (str agent-id)))
          (json-response 404 {:ok false :err "agent-not-found"
                              :message (str "Agent not registered: " agent-id)})

          :else
          (let [job-id (create-invoke-job! {:requested-job-id requested-job-id
                                            :agent-id agent-id
                                            :prompt prompt
                                            :caller caller
                                            :surface surface})
                queued-jobs (get-in (active-invoke-job-counts)
                                    [(str agent-id) :queued-jobs]
                                    0)
                job (some-> job-id get-invoke-job invoke-job-public-view)]
            (json-response 202 {:ok true
                                :accepted true
                                :job-id job-id
                                :state "queued"
                                :queued-jobs queued-jobs
                                :status-url (str "/api/alpha/invoke/jobs/" job-id)
                                :job job})))))))

(defn repl-through-queue?
  "E2 (turn-delivery-invariants.md): route /invoke-stream (REPL/operator turns) through the
   durable turn-queue + per-agent drainer — same guarantees as a bell (single-writer, durable,
   reply-routed) — instead of a direct invoke on a shared lane. Default TRUE (2026-06-14):
   proven, so it survives a stale-env OOM-resume that doesn't carry the flag; requires
   drainer-v2. Set FUTON3C_REPL_THROUGH_QUEUE=false to force the legacy direct-invoke path."
  []
  (agency-invariants/repl-through-queue-enabled?))

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
          #_{:clj-kondo/ignore [:deprecated-var]}
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
                              (catch Throwable _)))
                  caller (or (some-> payload :caller str)
                             (some-> payload (get "caller") str)
                             "http-caller")
                  surface (or (some-> payload :surface str)
                              (some-> payload (get "surface") str))
                  mission-id (or (:mission-id payload) (get payload "mission-id"))
                  timeout-ms (some-> (or (:timeout-ms payload) (get payload "timeout-ms"))
                                     long)
                  evidence-store (evidence-store-for-config config)
                  ev-opts (when mission-id [:mission-id mission-id])
                  effective-prompt (wrap-agent-facing-surface prompt surface caller agent-id)
                  ;; Terminal done/error emission from an invoke result — shared by the
                  ;; queued (E2) path and the legacy direct path.
                  emit-terminal!
                  (fn [result]
                    (let [sid (:session-id result)]
                      (apply emit-invoke-evidence! evidence-store caller (str prompt) sid
                             (or ev-opts []))
                      (if (:ok result)
                        (do
                          (apply emit-invoke-evidence! evidence-store (str agent-id) (str (:result result)) sid
                                 (or ev-opts []))
                          (sink-fn (cond-> {:type "done"
                                            :ok true
                                            :result (:result result)
                                            :session-id sid}
                                     (:invoke-meta result)
                                     (assoc :invoke-meta (:invoke-meta result)))))
                        (let [err (:error result)
                              code (if (map? err) (:error/code err) :invoke-failed)
                              msg (if (map? err) (:error/message err) (str err))]
                          (sink-fn {:type "done" :ok false :error (name code) :message msg})))))]
              ;; Clean up on client disconnect
              (hk/on-close channel
                (fn [_status]
                  (reg/clear-invoke-event-sink! aid)))
              (if (and (repl-through-queue?) (turn-queue/drainer-v2-enabled?))
                ;; E2: route the REPL/operator turn through the durable turn-queue so it gets
                ;; the SAME guarantees as a bell — single-writer (the agent's drainer, no
                ;; shared-lane race with concurrent bells), durable queue entry, reply streamed
                ;; back to THIS channel. The event sink is installed INSIDE process-fn (drainer
                ;; thread, exclusive to this turn) so a bell drained just before it cannot
                ;; cross-talk onto this channel.
                (turn-queue/accept-async!
                 {:to aid :from caller :surface (or surface "repl")
                  :prompt effective-prompt
                  :process-fn
                  (fn [_entry]
                    (reg/set-invoke-event-sink! aid sink-fn)
                    (try
                      (binding [turn-queue/*drained-by-outer* true]
                        (maybe-route-surface-writes
                         agent-id
                         (reg/invoke-agent! aid effective-prompt timeout-ms)))
                      (finally
                        (reg/clear-invoke-event-sink! aid))))
                  :finalize-fn
                  (fn [result]
                    (try
                      (emit-terminal! result)
                      (catch Throwable t
                        (sink-fn {:type "done" :ok false :error "invoke-error"
                                  :message (.getMessage t)}))
                      (finally
                        (hk/close channel))))})
                ;; Legacy (flag OFF / no drainer-v2): direct invoke on a shared lane.
                (.submit invoke-executor
                  ^Runnable
                  (fn []
                    (reg/set-invoke-event-sink! aid sink-fn)
                    (try
                      (emit-terminal!
                       (maybe-route-surface-writes
                        agent-id
                        (reg/invoke-agent! (str agent-id) effective-prompt timeout-ms)))
                      (catch Throwable t
                        (sink-fn {:type "done" :ok false :error "invoke-error"
                                  :message (.getMessage t)}))
                      (finally
                        (reg/clear-invoke-event-sink! aid)
                        (hk/close channel)))))))))))))

(defn- invoke-job-terminal-state?
  [state]
  (not (#{"queued" "running" "overrun"} (str state))))

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
        #_{:clj-kondo/ignore [:deprecated-var]}
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
            #_{:clj-kondo/ignore [:deprecated-var]}
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



(defn- handle-coordination-qa
  "GET /api/alpha/coordination/qa?limit=N — unified mesh misrouting QA."
  [request]
  (let [params (parse-query-params request)
        limit (or (parse-int (get params "limit")) 100)]
    (json-response 200 (mesh-qa/current-report limit))))

(defn- handle-coordination-edges
  "GET /api/alpha/coordination/edges?limit=N — social-layer mesh edges.
   These are projected as outgoing coordination edges for mesh_trace.py; they
   complement the invoke-jobs ledger without replacing it."
  [request]
  (let [params (parse-query-params request)
        limit (or (parse-int (get params "limit")) 50)
        edges (coordination-ledger/recent-mesh-edges limit)]
    (json-response 200 {:ok true
                        :count (count edges)
                        :edges edges})))

(defn- handle-coordination-threads
  "GET /api/alpha/coordination/threads?limit=N — the bell conversation graph
   (E-crossed-bells): open bells, who-owes-whom (:by-agent), and A<->B crossings,
   correlated by the explicit :bellback-of. The :crossings list is the actionable
   signal — one side should whistle to reconcile (README-bells-and-whistles.md)."
  [request]
  (let [params (parse-query-params request)
        limit (or (parse-int (get params "limit")) 200)
        graph (bell-router/graph (recent-invoke-jobs limit))]
    (json-response 200 (assoc graph :ok true))))

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

(defn- relay-invoke-delivery-over-ws!
  "Best-effort relay of a delivery receipt to a WS-connected agent node."
  [agent-id invoke-trace-id {:keys [surface destination delivered? note]}]
  (try
    (ws-invoke/send-frame!
     (str agent-id)
     {"type" "invoke_delivery"
      "agent_id" (str agent-id)
      "invoke_trace_id" (str invoke-trace-id)
      "surface" (str (or surface "unknown"))
      "destination" (str (or destination "unknown"))
      "delivered" (boolean delivered?)
      "note" (str (or note ""))})
    (catch Throwable _
      false)))

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
          (let [trace-id (str invoke-trace-id)
                aid (str agent-id)
                _ (record-invoke-job-delivery! trace-id receipt)
                record-fn (*resolve-delivery-recorder*)
                local-record
                (if record-fn
                  (try
                    {:attempted? true
                     :recorded? (boolean (record-fn aid trace-id receipt))}
                    (catch Throwable t
                      {:attempted? true
                       :recorded? false
                       :error (.getMessage t)}))
                  {:attempted? false
                   :recorded? false})
                ws-relayed? (when-not (:recorded? local-record)
                              (relay-invoke-delivery-over-ws! aid trace-id receipt))]
            (cond
              (:recorded? local-record)
              (json-response 200 {:ok true
                                  :agent-id aid
                                  :invoke-trace-id trace-id
                                  :recorded true
                                  :relayed false})

              ws-relayed?
              (json-response 200 {:ok true
                                  :agent-id aid
                                  :invoke-trace-id trace-id
                                  :recorded false
                                  :relayed true})

              (:attempted? local-record)
              (json-response 502 {:ok false
                                  :err "invoke-delivery-record-failed"
                                  :message (or (:error local-record)
                                               "delivery receipt could not be written")})

              :else
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

(defn- handle-agent-clock
  "GET /api/alpha/agent-clock?agent-id=X&session-id=Y — the live auto-clock for
   that agent session (campaign/mission/excursion + witness). The repl buffer polls
   this on turn-end so its display reflects the durable clock the agent's tool-edits
   feed, instead of a disconnected Emacs-side clock (C-cascade-real D1/O3 sync)."
  [request]
  (let [params (parse-query-params request)
        agent-id (get params "agent-id")
        session-id (get params "session-id")]
    (if (str/blank? (str agent-id))
      (json-response 400 {:ok false :error "agent-id required"})
      (let [state (clock-store/current-state agent-id session-id)
            clock (:clock state)]
        (json-response 200 {:ok true
                            :agent-id agent-id
                            :session-id session-id
                            :campaign-id (:campaign-id clock)
                            :mission-id (:mission-id clock)
                            :excursion-id (:excursion-id clock)
                            :witness (:last-auto-clock-witness state)})))))

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

(defn- handle-agent-status
  "POST /api/alpha/agents/:id/status — report external invoke state.
   Body: {\"source\": \"emacs-agent-chat\", \"status\": \"invoking\"|\"idle\",
          \"activity\": \"optional description\", \"session_id\": \"optional\"}
   Emacs (or any surface) calls this to tell the server an agent is busy."
  [_config agent-id request]
  (let [payload (parse-json-map (read-body request))
        source (or (:source payload) (get payload "source") "http-api")
        status-val (or (:status payload) (get payload "status"))
        activity (or (:activity payload) (get payload "activity"))
        session-id (or (:session_id payload) (get payload "session_id")
                       (:session-id payload) (get payload "session-id"))]
    (if (str/blank? (str status-val))
      (json-response 400 {:ok false :err "missing-status"
                          :message "Body must include \"status\": \"invoking\" or \"idle\""})
      (let [result (reg/report-external-invoke!
                    agent-id source
                    (cond-> {:status status-val}
                      activity (assoc :activity activity)
                      session-id (assoc :session-id session-id)))]
        (json-response 200 result)))))

(defn- handle-agent-interrupt-invoke
  "POST /api/alpha/agents/:id/interrupt-invoke — request a best-effort
   interruption of the agent's current local invoke subprocess tree."
  [_config agent-id request]
  (let [payload (or (parse-json-map (read-body request)) {})
        requested-job-id (or (:job-id payload) (get payload "job-id")
                             (:job_id payload) (get payload "job_id"))
        running-job (running-invoke-job-for-agent agent-id)
        running-job-id (some-> running-job :job-id str)
        requested-job-id (some-> requested-job-id str str/trim not-empty)]
    (cond
      (and requested-job-id running-job-id (not= requested-job-id running-job-id))
      (json-response 409 {:ok false
                          :agent-id (str agent-id)
                          :error "running-job-mismatch"
                          :requested-job-id requested-job-id
                          :running-job-id running-job-id})

      :else
      (try
        (require 'futon3c.dev)
        (if-let [interrupt-fn (resolve 'futon3c.dev/interrupt-agent-invoke!)]
          (let [result (interrupt-fn (str agent-id))]
            (json-response (if (:ok result) 200 409)
                           (cond-> (assoc result :running-job-id running-job-id)
                             requested-job-id
                             (assoc :requested-job-id requested-job-id))))
          (json-response 503 {:ok false
                              :agent-id (str agent-id)
                              :error "interrupt-unavailable"
                              :message "local invoke interrupt surface is unavailable"}))
        (catch Throwable t
          (json-response 500 {:ok false
                              :agent-id (str agent-id)
                              :error "interrupt-error"
                              :message (.getMessage t)}))))))

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
  "GET /api/alpha/missions — cross-repo mission inventory with per-mission
  turn-count telemetry. Pass include-turn-counts=false when the caller needs
  only the strategic inventory; telemetry is ancillary and can be expensive."
  [request config]
  (let [include-turn-counts? (not= "false"
                                   (get (parse-query-params request)
                                        "include-turn-counts"))
        inventory (mcb/build-inventory)
        turn-counts (when include-turn-counts?
                      (mcb/mission-turn-count-telemetry
                       (evidence-store-for-config config)))
        missions (if turn-counts
                   (mcb/attach-turn-counts inventory turn-counts)
                   inventory)]
    (json-response 200 {:ok true
                        :missions missions
                        :count (count missions)
                        :turn-counts-included? include-turn-counts?
                        :turn-counts turn-counts})))

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
                result (boundary/append! evidence-store entry)]
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
                    result (boundary/append! evidence-store entry)]
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
  [request _config]
  (let [params (parse-query-params request)
        path (or (get params "path") (get params :path))]
    (if (or (nil? path) (str/blank? (str path)))
      (json-response 400 {:ok false :error "missing-path"
                           :message "path query parameter is required"})
      (let [futon1a-url (or (System/getenv "FUTON_SUBSTRATE_URL")
                            (System/getenv "FUTON1A_URL") "http://localhost:7071")
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
                              (let [result (boundary/append! evidence-store entry)]
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

(defn- handle-mc-sync-mission
  "POST /api/alpha/mc/sync-mission — push-sync a single mission markdown file
   into the evidence store as a versioned snapshot. Request body:
   {\"path\":\"/abs/path/to/M-foo.md\", \"repo\":\"futon6\"} where repo is optional."
  [request config]
  (let [body (read-body request)
        payload (try
                  (some-> body (json/parse-string true))
                  (catch Exception _
                    ::bad-json))
        path (or (:path payload) (get payload "path"))
        repo (or (:repo payload) (get payload "repo"))
        repo-kw (cond
                  (keyword? repo) repo
                  (string? repo) (keyword repo)
                  :else nil)
        evidence-store (evidence-store-for-config config)]
    (cond
      (= ::bad-json payload)
      (json-response 400 {:ok false
                          :error "bad-json"
                          :message "Expected JSON body with a mission path"})

      (or (nil? path) (str/blank? (str path)))
      (json-response 400 {:ok false
                          :error "missing-path"
                          :message "path is required"})

      :else
      (try
        (if-let [mission (mcb/parse-mission-path mcb/default-repo-roots path repo-kw)]
          (let [entry (mcb/mission->sync-evidence mission)
                append-result (when evidence-store
                                (boundary/append! evidence-store entry))
                duplicate? (= :duplicate-id (:error/code append-result))]
            (json-response 200
                           {:ok true
                            :mission mission
                            :evidence/id (:evidence/id entry)
                            :created (boolean (and append-result (:ok append-result)))
                            :skipped duplicate?
                            :stored? (boolean evidence-store)}))
          (json-response 404 {:ok false
                              :error "mission-not-found"
                              :message (str "No mission markdown found at path: " path)}))
        (catch Exception e
          (json-response 500 {:ok false
                              :error "mission-sync-failed"
                              :message (.getMessage e)}))))))

(defn- handle-mc-tensions
  "GET /api/alpha/mc/tensions — export structured tension data.
   Returns typed tension entries pre-shaped for hyperedge creation."
  [_config]
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
               (contains? payload :emit-evidence)
               (assoc :emit-evidence? (boolean (:emit-evidence payload)))
               (:agenda-id payload)
               (assoc :agenda-id (:agenda-id payload))
               (:claim payload)
               (assoc :claim (:claim payload))
               (:observation-source payload)
               (assoc :observation-source (:observation-source payload)))]
    (try
      (let [result (portfolio/portfolio-step! evidence-store opts)]
        (json-response 200
          {:ok true
           :recommendation (portfolio/format-recommendation result)
           :action (some-> (:action result) name)
           :run-id (get-in result [:run :run-id])
           :agenda-id (get-in result [:run :agenda-id])
           :observation-source (get-in result [:run :observation-source])
           :step-count {:before (get-in result [:run :step-before])
                        :after (get-in result [:run :step-after])}
           :diagnostics (:diagnostics result)
           :abstain (get-in result [:policy :abstain?])
           :evidence (:evidence result)
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
;; Invariant Runner
;; =============================================================================

(defn- try-resolve-domain
  "Attempt to resolve a domain's build-db and query-violations fns.
   Returns a domain spec or nil if the namespace can't be loaded."
  [domain-id ns-sym]
  (try
    (require ns-sym)
    (let [build-db (resolve (symbol (str ns-sym) "build-db"))
          qv (resolve (symbol (str ns-sym) "query-violations"))]
      (when (and build-db qv)
        {:domain-id domain-id
         :build-db @build-db
         :query-violations @qv}))
    (catch Exception _ nil)))

(def ^:private invariant-domain-registry
  "All known invariant domains with their logic namespaces."
  [[:agency   'futon3c.agency.logic]
   [:tickle   'futon3c.agents.tickle-logic]
   [:proof    'futon3c.peripheral.proof-logic]
   [:mission  'futon3c.peripheral.mission-logic]
   [:codex    'futon3c.agents.codex-code-logic]
   [:portfolio 'futon3c.portfolio.logic]])

(defn- handle-invariants
  "GET /api/alpha/invariants — run invariant checks across all domains.
   Returns domain reports, violation summary, and obligation counts.

   Each domain's build-db is called with nil input. Domains whose
   build-db requires live state will produce empty results (no false
   positives). This endpoint reports what is structurally checkable
   from the HTTP context."
  [_request _config]
  (try
    (require 'futon3c.logic.invariant-runner)
    (let [run-aggregate (resolve 'futon3c.logic.invariant-runner/run-aggregate)
          domains (vec (keep (fn [[did ns-sym]]
                               (try-resolve-domain did ns-sym))
                             invariant-domain-registry))
          load-profile {}
          aggregate (@run-aggregate load-profile domains)
          {:keys [reports summary]} aggregate]
      (json-response 200
        {:ok true
         :domains (mapv (fn [{:keys [domain-id state has-violations? violations]}]
                          {:domain domain-id
                           :state state
                           :has-violations has-violations?
                           :violation-categories (when has-violations?
                                                   (into {}
                                                         (for [[k v] violations
                                                               :when (seq v)]
                                                           [k (count v)])))})
                        reports)
         :summary summary
         :registry-size (count invariant-domain-registry)
         :resolved-size (count domains)}))
    (catch Exception e
      (json-response 500 {:ok false :error "invariant-check-failed"
                          :message (.getMessage e)}))))

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
;; War Machine — strategic synthesis visualiser backing API
;; =============================================================================
;;
;; The canonical war-machine scan lives in futon2 (futon2.report.war-machine).
;; Exposing it here lets the CLJS web viewer at futon2/web/war-machine share the
;; existing futon3c JVM instead of spinning up a second HTTP server.
;;
;; futon2.report.war-machine returns values with namespaced keys (e.g.
;; :mission/id). Cheshire's default keyword encoder drops the namespace
;; ((name :mission/id) → "id"), which would collide with :id on siblings.
;; stringify-wm-keys preserves namespaces on the wire by emitting "ns/name"
;; for namespaced keywords (the CLJS client re-keywordises via cljs-http's
;; :keywordize-keys default, and (keyword "mission/id") → :mission/id).

(defn stringify-wm-response
  [x]
  (cond
    (map? x)    (into {}
                      (map (fn [[k v]]
                             [(cond
                                (keyword? k) (if (namespace k)
                                               (str (namespace k) "/" (name k))
                                               (name k))
                                :else (str k))
                              (stringify-wm-response v)])
                           x))
    (vector? x) (mapv stringify-wm-response x)
    (seq? x)    (mapv stringify-wm-response x)
    (set? x)    (mapv stringify-wm-response x)
    (keyword? x) (if (namespace x)
                   (str (namespace x) "/" (name x))
                   (name x))
    :else x))

(def stringify-wm-keys stringify-wm-response)

(def ^:private pilot-inhabitations-edn-path
  "/home/joe/code/futon5a/data/pilot-inhabitations.edn")

#_{:clj-kondo/ignore [:unused-private-var]}
(defn- pattern-id->collection-name
  "Pattern-ids in the evidence store are keywords like :iiching/exotype-000.
   The collection name is the keyword's namespace (\"iiching\").  Falls back
   gracefully if pattern-id is a string of the form \"col/name\" or anything
   else (returns nil and the entry is skipped)."
  [pattern-id]
  (cond
    (keyword? pattern-id) (namespace pattern-id)
    (string? pattern-id)
    (let [idx (.indexOf ^String pattern-id "/")]
      (when (pos? idx) (subs pattern-id 0 idx)))
    :else nil))

(defn derive-pattern-activations
  "Walk the live evidence store for `context-retrieval`-tagged entries —
   each represents one A→B turn whose futon3a retrieval surfaced N patterns.
   Each result `.id` in the body is a `collection-name/pattern-name` string;
   count one activation per result per turn, group by collection.

   Returns `{collection-name <activation-count>}`.  Limited to the recent
   window if :days is provided (uses :evidence/at timestamps).  Quiet on
   errors so a malformed evidence store never breaks the war-machine
   endpoint.  Resolves the live store via (mcs/!config :evidence-store) with
   fallback to estore/!store, mirroring handle-aif-stack-live's pattern.

   NOTE: this is the pattern-SURFACED-by-retrieval signal, not a curated
   PSR-application signal.  Per Joe's emacs-repl 2026-05-25 clarification:
   'activation through the tagging that goes on per A→B turn in each turn
   submitted to the evidence landscape.'  Future cycles may layer a more
   discriminating signal (PSR-confirmed-activation vs raw-retrieval) on
   top; v0 surfaces the retrieval count."
  [{:keys [days]}]
  (try
    (let [since-ms (when (and (int? days) (pos? days))
                     (- (System/currentTimeMillis) (* days 24 60 60 1000)))
          live-store (or (:evidence-store @mcs/!config) estore/!store)
          entries (estore/query* live-store {:query/tags [:context-retrieval]})
          recent? (fn [e]
                    (or (nil? since-ms)
                        (when-let [at (:evidence/at e)]
                          (try
                            (let [t (cond
                                      (number? at) (long at)
                                      (string? at) (.toEpochMilli (java.time.Instant/parse at))
                                      :else 0)]
                              (>= t since-ms))
                            (catch Throwable _ true)))))
          ;; :evidence/body has STRING keys (it was deserialised from
          ;; JSON-shaped input that retained literal keys); guard for both
          ;; shapes so the function is robust across body-source conventions.
          results-of (fn [e]
                       (or (get-in e [:evidence/body :results])
                           (get-in e [:evidence/body "results"])))
          collection-of (fn [r]
                          (let [id (or (:id r) (get r "id"))]
                            (cond
                              (keyword? id) (namespace id)
                              (string? id)
                              (let [i (.indexOf ^String id "/")]
                                (when (pos? i) (subs id 0 i)))
                              :else nil)))]
      (->> entries
           (filter recent?)
           (mapcat results-of)
           (keep collection-of)
           frequencies))
    (catch Throwable _ {})))

(defn enrich-patterns-with-activations
  "Given the WM `data` map and a days window, attach an :activations-Nd count
   to each collection in :patterns.collections, plus :activations-window-days
   for the UI to know which window the count covers."
  [data days]
  (if (and (map? data) (map? (:patterns data)))
    (let [activations (derive-pattern-activations {:days days})
          window-key (keyword (str "activations-" days "d"))]
      (update-in data [:patterns :collections]
                 (fn [cols]
                   (mapv (fn [c]
                           (let [collection-key (or (some-> (:id c) name)
                                                    (when (string? (:id c)) (:id c))
                                                    (:name c))
                                 n (long (or (get activations collection-key) 0))]
                             (assoc c
                                    window-key n
                                    :activations-window-days days)))
                         cols))))
    data))

(def ^:private vsatarcs-aif-path
  "/home/joe/code/futon4/docs/vsatarcs-alignment-completeness.aif.edn")

(defn- vsatarcs-processed-event-ids
  "Read VSATARCS bilateral-evidence file and return the set of
   :writer-event-id strings present.  Used to mark Inhabitation Log
   events with their VSATARCS-processed status (Joe's emacs-repl
   request, 2026-05-25: '[E-pilot-vsatarcs-feed] could in principle
   go into the Inhabitation Log so we would know which items had been
   processed for VSATARCS').

   Errors absorbed (returns empty set) so a malformed VSATARCS file
   never breaks the war-machine endpoint."
  []
  (try
    (let [data (-> vsatarcs-aif-path slurp edn/read-string)
          entries (or (:bilateral-evidence data) [])]
      (into #{} (keep :writer-event-id) entries))
    (catch Throwable _ #{})))

(defn- annotate-vsatarcs-status
  "Decorate each pilot-inhabitation event with :vsatarcs-processed? bool."
  [events processed-ids]
  (mapv (fn [ev]
          (assoc ev :vsatarcs-processed?
                 (boolean (contains? processed-ids (:id ev)))))
        events))

(defn derive-pilot-inhabitations
  "Read futon5a/data/pilot-inhabitations.edn and derive the summary block
   the War Machine UI's Inhabitation Log card consumes.  Returns a map with
   :current-inhabitant, :previous-inhabitant, :all-events, :stale?,
   :source-file.  Errors are absorbed into :stale? true with :error message
   so a malformed file never breaks the war-machine endpoint.

   Each event in :all-events / :wip-events / :done-events carries a
   :vsatarcs-processed? flag indicating whether E-pilot-vsatarcs-feed has
   ingested it (cross-referenced via VSATARCS :writer-event-id)."
  []
  (try
    (let [data (-> pilot-inhabitations-edn-path slurp edn/read-string)
          processed-ids (vsatarcs-processed-event-ids)
          events (annotate-vsatarcs-status (vec (:events data)) processed-ids)
          starts (filter #(= :inhabitation-start (:event %)) events)
          ends   (filter #(= :inhabitation-end (:event %)) events)
          latest-by-time (fn [coll]
                           (->> coll
                                (sort-by #(or (:at %) (:at-approx %) ""))
                                last))
          latest-start (latest-by-time starts)
          latest-end   (latest-by-time ends)
          ;; current inhabitant: latest start whose :pilot-agent doesn't
          ;; appear as the agent of a later :inhabitation-end.
          current-active? (and latest-start
                               (or (nil? latest-end)
                                   (> (compare (or (:at latest-start)
                                                   (:at-approx latest-start) "")
                                               (or (:at latest-end)
                                                   (:at-approx latest-end) "")) 0)))
          current-agent (when current-active? (:pilot-agent latest-start))
          previous-agent (when latest-end (:pilot-agent latest-end))
          events-by-agent (fn [agent]
                            (filter #(= agent (:pilot-agent %)) events))
          wip-events (when current-agent
                       (filter #(contains? #{:cycle-in-flight :substrate-creation}
                                           (:event %))
                               (events-by-agent current-agent)))
          done-events (when previous-agent
                        (filter #(= :cycle-complete (:event %))
                                (events-by-agent previous-agent)))]
      {:current-inhabitant  (when current-agent
                              {:agent       current-agent
                               :since       (or (:at latest-start)
                                                (:at-approx latest-start))
                               :wip-cycles  (count wip-events)
                               :wip-events  (vec wip-events)})
       :previous-inhabitant (when previous-agent
                              {:agent       previous-agent
                               :ended-at    (or (:at latest-end)
                                                (:at-approx latest-end))
                               :done-cycles (count done-events)
                               :done-events (vec done-events)})
       :all-events          events
       :stale?              false
       :source-file         pilot-inhabitations-edn-path
       :schema-version      (:schema-version data)})
    (catch Throwable t
      {:stale?      true
       :error       (.getMessage t)
       :source-file pilot-inhabitations-edn-path})))

;; -- WM operator-clear sentinel ------------------------------------------------
;;
;; Operator-side mute for the :stop-the-line override.  When the apparatus
;; emitting :stop-the-line has been miscalibrated (stale snapshot, tier-
;; threshold divergence, etc.) the operator can drop a sentinel file to
;; suppress downstream :stop-the-line propagation until the calibration is
;; corrected.  Band-aid, NOT the proper fix — see
;; futon3c/holes/missions/E-wm-staleness-meta-stop.md.
;;
;; Sentinel shape (edn at ~/code/storage/futon0/wm-operator-clear.edn):
;;   {:cleared-at #inst "..."  ;; when set
;;    :until      #inst "..."  ;; auto-expire
;;    :reason     "..."        ;; one-liner the operator can read later
;;    :cleared-by "..."}
;;
;; Per CLAUDE.md §9 (never silently swallow), the clear is embedded in the
;; response under :operator-clear so the override is visible, not hidden.

(def ^:private wm-operator-clear-path
  (str (System/getProperty "user.home")
       "/code/storage/futon0/wm-operator-clear.edn"))

(defn read-wm-operator-clear
  "Read sentinel iff present and :until is in the future. nil otherwise."
  []
  (try
    (let [f (java.io.File. ^String wm-operator-clear-path)]
      (when (.exists f)
        (let [data (edn/read-string (slurp f))
              until (:until data)
              until-inst (cond
                           (instance? java.util.Date until)
                           (.toInstant ^java.util.Date until)
                           (string? until) (java.time.Instant/parse until)
                           :else nil)]
          (when (and until-inst (.isAfter until-inst (java.time.Instant/now)))
            data))))
    (catch Exception _ nil)))

(defn downgrade-stl-tier
  "Map :stop-the-line tier values down to :high.  No-op on other tiers."
  [tier]
  (cond
    (= :stop-the-line tier)  :high
    (= "stop-the-line" tier) "high"
    :else                    tier))

(defn downgrade-stl-mode
  "Map :stop-the-line judgement mode down to :recovery (informational)."
  [mode]
  (cond
    (= :stop-the-line mode)  :recovery
    (= "stop-the-line" mode) "recovery"
    :else                    mode))

(defn apply-wm-operator-clear
  "If an operator-clear sentinel is active, downgrade :stop-the-line tiers
   across :metabolic-balance / :commit-hygiene and the :judgement :mode so
   the override stops propagating to the UI.  Sentinel embedded under
   :operator-clear (visible, not silent)."
  [{:keys [data judgement] :as bundle}]
  (if-let [clear (read-wm-operator-clear)]
    (let [downgrade-coll (fn [coll]
                           (mapv #(update % :tier downgrade-stl-tier)
                                 (or coll [])))
          data' (-> data
                    (update-in [:metabolic-balance :max-tier] downgrade-stl-tier)
                    (update-in [:metabolic-balance :per-repo] downgrade-coll)
                    (update-in [:metabolic-balance :channels] downgrade-coll)
                    (update-in [:commit-hygiene :max-tier] downgrade-stl-tier)
                    (update-in [:commit-hygiene :queues] downgrade-coll)
                    (assoc :operator-clear clear))
          judgement' (let [m (:mode judgement)]
                       (if (or (= :stop-the-line m) (= "stop-the-line" m))
                         (-> judgement
                             (assoc :mode (downgrade-stl-mode m))
                             (assoc :operator-cleared? true)
                             (assoc :pre-clear-mode m))
                         judgement))]
      {:data data' :judgement judgement'})
    bundle))

(defn wm-response-payload
  "Attach dynamic top-level WM payload fields that are cheap to derive
   outside the heavyweight scan itself.

   In addition to :pilot-inhabitations, enriches :patterns.collections[]
   with per-collection :activations-Nd from PSR evidence-store entries in
   the same days window (M-pattern-application-diagnostic integration,
   2026-05-25 claude-1)."
  [{:keys [data judgement] :as bundle}]
  (let [days (or (get-in data [:window :days])
                 (get-in data [:window "days"])
                 14)
        data' (-> data
                  (assoc :pilot-inhabitations (derive-pilot-inhabitations))
                  (enrich-patterns-with-activations days))]
    (assoc bundle :data data' :judgement judgement)))

(defn wm-scan-age-seconds
  [as-of]
  (when as-of
    (max 0
         (.getSeconds (java.time.Duration/between ^Instant as-of
                                                  (Instant/now))))))

(def ^:private vsatarcs-status-script
  (str (System/getProperty "user.home")
       "/code/futon4/scripts/build-invariant-state-projection.bb"))

(defn- current-vsatarcs-status
  []
  (try
    (if-not (.exists (io/file vsatarcs-status-script))
      {:available? false
       :build {:status :error}
       :error (str "Missing projection script: " vsatarcs-status-script)}
      (let [{:keys [exit out err]} (shell/sh "bb" vsatarcs-status-script "--wm-status")]
        (if (zero? exit)
          (assoc (edn/read-string out) :available? true)
          {:available? false
           :build {:status :error}
           :error (or (not-empty err)
                      (str "VSATARCS status command exited " exit))})))
    (catch Exception e
      {:available? false
       :build {:status :error}
       :error (.getMessage e)})))

(defn- wm-prebuilt-response-body
  "Inject lightweight freshness metadata into a pre-rendered WM JSON body
   without re-encoding the entire 10MB+ payload on every request."
  [snapshot scheduler]
  (let [as-of (:as-of snapshot)
        age (wm-scan-age-seconds as-of)
        vsatarcs-status (current-vsatarcs-status)
        body (:body snapshot)]
    (if (and (string? body) (str/starts-with? body "{"))
      (str "{\"as-of\":" (json/generate-string (some-> as-of str))
           ",\"scan-age-seconds\":" (or age "null")
           ",\"scheduler\":" (json/generate-string scheduler)
           ",\"vsatarcs-status\":" (json/generate-string vsatarcs-status)
           "," (subs body 1))
      (json/generate-string
       (assoc (:payload snapshot)
              "as-of" (some-> as-of str)
              "scan-age-seconds" age
              "scheduler" scheduler
              "vsatarcs-status" vsatarcs-status)))))

(defn- wm-scheduler-ensure-started! []
  (try
    (when-let [f (requiring-resolve 'futon3c.wm.scheduler/ensure-started!)]
      (f))
    (catch Throwable _ nil)))

(defn- wm-scheduler-status-snapshot []
  (try
    (when-let [f (requiring-resolve 'futon3c.wm.scheduler/status)]
      (f))
    (catch Throwable _ nil)))

(defn- wm-scheduler-request-window! [days]
  (try
    (when-let [f (requiring-resolve 'futon3c.wm.scheduler/request-window!)]
      (f days))
    (catch Throwable _ nil)))

(defn- wm-scheduler-snapshot-for-days [days]
  (try
    (when-let [f (requiring-resolve 'futon3c.wm.scheduler/snapshot-for-days)]
      (f days))
    (catch Throwable _ nil)))

(defn- r14-gamma-summary
  "R14 γ (policy-precision) live state, folded into the war-machine payload for the
   AIF↔cascade loop render (C-cascade-real).

   γ modulates the selection temperature `τ_eff = τ/γ` and LEARNS from the chosen
   policy's realized-vs-expected outcome — the R16→R14 `:realized-outcome` trace
   contract (futon2.aif.policy-precision). That feed is STAGED behind
   `fold-realized/*live-wire?*` (off; operator-governed R16-ARM): enactment isn't
   live-wired, so there are **0 policy-outcome samples** and γ holds at its
   reduction-safe prior 1.0 (policy_precision.clj: 'absent today ⇒ no sample ⇒ γ
   holds at 1.0').

   HONESTY NOTE: the calibration report over repl-traces (predicted-vs-realised
   DISCHARGE) is a RELATED but DISTINCT signal — NOT γ's policy-outcome feed. It is
   surfaced under `:calibration-signal` for context, never as γ's sample count."
  []
  (let [base (try
               ;; LIVE-WIRED 2026-07-02 (Joe-ratified): read the REAL γ-state from
               ;; the latest WM trace record — the same state `judge` threads
               ;; tick-to-tick. The scheduled runner (futon2.aif.enact) now
               ;; computes act-gates per tick and enacts the first :pass
               ;; (artifact-only; WM-I4 firing stays operator-gated), so
               ;; :realized-outcome records flow and γ accrues through burn-in.
               ;; coerce-state guards the retired v0 :error-history schema (the
               ;; 8 junk samples that pinned γ to 0.5 for days; found 2026-07-02).
               (let [rec ((requiring-resolve 'futon2.aif.trace/latest-trace-record))
                     coerce (requiring-resolve 'futon2.aif.policy-precision/coerce-state)
                     st (coerce (:policy-precision rec))
                     gamma (double (:policy-precision st 1.0))
                     samples (long (:samples st 0))]
                 {:gamma gamma
                  :held-at-prior? (= gamma 1.0)
                  :bounds [0.5 2.0]
                  :policy-outcome-samples samples
                  :burn-in-threshold 5
                  :burn-in-met? (>= samples 5)
                  :mean-perf (:mean-perf st)
                  :live-wire? true
                  :last-realized-outcome (:realized-outcome rec)
                  :last-enactment (:enactment rec)
                  :act-gate-verdicts (:act-gate-verdicts rec)
                  :status (str "R16 enactment LIVE-WIRED (2026-07-02; scheduled-runner enactor, "
                               "artifact-only — WM-I4 firing stays operator-gated). γ=" gamma
                               " from " samples " realized policy-outcome sample(s); 5-sample "
                               "burn-in, then γ moves. Retired-schema guard active (the γ=0.5 "
                               "pin on 8 junk 2026-06-27 samples is fixed). (The calibration "
                               "discharge signal below is a related but DISTINCT readout, not "
                               "γ's learning feed.)")})
               (catch Throwable t
                 {:gamma 1.0 :held-at-prior? true :bounds [0.5 2.0]
                  :policy-outcome-samples 0 :burn-in-threshold 5 :burn-in-met? false
                  :live-wire? true
                  :status (str "R16 live-wired but trace read failed: " (.getMessage t))}))]
    (try
      (let [report ((requiring-resolve 'futon3c.aif.calibration/calibration-report)
                    ((requiring-resolve 'futon3c.aif.calibration/load-evidence)))]
        (assoc base :calibration-signal
               {:paired (:paired-count report)
                :independent (or (:independent-paired-count report) 0)
                :verdict (:verdict report)
                :note "predicted-vs-realised DISCHARGE over repl-traces — distinct from γ's policy feed"}))
      (catch Throwable t (assoc base :calibration-signal {:error (.getMessage t)})))))

(defn- handle-war-machine
  "GET /api/alpha/war-machine[?days=N] — return cached WM JSON snapshot.

   The heavyweight scan runs on a background schedule in
   `futon3c.wm.scheduler`. HTTP reads the cached atom and returns either
   the latest snapshot for the requested day-window or a 503 while the
   background warmup is still in progress.

   Payload now includes :pilot-inhabitations derived from
   futon5a/data/pilot-inhabitations.edn — the War Machine UI's
   Inhabitation Log card (which replaced the static Pilot Contract card per
   M-war-machine-pilot cycle-3, 2026-05-25) reads from this block."
  [request]
  (try
    (let [;; Two-source query-param parsing: the standard Ring form
          ;; (:query-params, populated by wrap-params middleware) AND a
          ;; manual :query-string scan, because the futon3c Ring stack
          ;; does not currently wrap-params here so :query-params is nil.
          qs (or (:query-string request) "")
          days-from-qs (when-let [m (re-find #"(?:^|&)days=([0-9]+)" qs)]
                         (try (Integer/parseInt (second m))
                              (catch NumberFormatException _ nil)))
          days-from-params (let [v (get-in request [:query-params "days"])]
                             (when (string? v)
                               (try (Integer/parseInt v)
                                    (catch NumberFormatException _ nil))))
          days (or days-from-params days-from-qs 14)
          _ (wm-scheduler-ensure-started!)
          snapshot (wm-scheduler-snapshot-for-days days)]
      (if snapshot
        (let [scheduler (wm-scheduler-status-snapshot)
              ;; wm-prebuilt-response-body returns a pre-rendered JSON STRING (to avoid
              ;; re-encoding the 10MB+ payload) — so splice :r14-gamma into it as the
              ;; first key rather than assoc'ing onto a map.
              s    (wm-prebuilt-response-body snapshot scheduler)
              body (if (and (string? s) (str/starts-with? s "{"))
                     (str "{\"r14-gamma\":" (json/generate-string (r14-gamma-summary)) "," (subs s 1))
                     s)]
          (-> (json-response 200 body)
              (assoc-in [:headers "Access-Control-Allow-Origin"] "*")))
        (do
          (wm-scheduler-request-window! days)
          (-> (json-response 503
                             {:ok false
                              :error "war-machine-snapshot-unavailable"
                              :days days
                              :retry-after-seconds 60
                              :scheduler (wm-scheduler-status-snapshot)
                              :r14-gamma (r14-gamma-summary)
                              :message "war-machine snapshot not ready yet; background warmup started"})
              (assoc-in [:headers "Access-Control-Allow-Origin"] "*")
              (assoc-in [:headers "Retry-After"] "60")))))
    (catch Exception e
      (json-response 500 {:ok false
                          :error "war-machine-snapshot-read-failed"
                          :message (.getMessage e)}))))

;; =============================================================================
;; AIF+ stack — live projection of the stack's self-model
;; =============================================================================
;;
;; Loads the cached structural prior at futon5a/holes/stories/THE-STACK.aif.edn
;; and overlays live mission status from `mcb/build-inventory'. The output
;; mirrors THE-STACK.aif.edn's shape (so the existing prose generators and
;; the planned War Machine view-mode read the same payload) plus a few
;; metadata fields marking liveness.
;;
;; Cached EDN supplies *structure* (spine selection, conflict definitions,
;; edge topology, frame definitions, cross-leaf relations). Registry supplies
;; *data* (current node statuses for nodes whose :origin resolves to a
;; mission@repo ref).  Spine nodes whose origin is a sorry id retain their
;; cached status — that's a v2 enhancement (query the sorry registry).

(defn- portfolio-scheduler-status-snapshot
  "Best-effort scheduler snapshot for inclusion in the AIF stack response.
   Resolves the scheduler ns lazily so the HTTP handler does not require
   the scheduler at compile time."
  []
  (try
    (when-let [s (requiring-resolve 'futon3c.portfolio-inference.scheduler/status)]
      (s))
    (catch Throwable _ nil)))

(defn- handle-aif-stack-live
  "GET /api/alpha/aif-stack/live — live-projected AIF+ stack self-model.

   Returns the structural prior with live :status overlays applied to
   spine nodes whose :origin resolves to a mission ref.  Each overlaid
   node carries :live-status? true.  Top-level :live? indicates whether
   the mission inventory was reachable; on inventory failure the cached
   stack is returned with :live? false.

   Also embeds a :scheduler block with the recurring AIF tick's period
   and last/next-tick-at so the War Machine UI can derive its own poll
   cadence and freshness thresholds from a single source of truth — see
   M-stack-stereolithography Checkpoint 5 (cadence walk-back)."
  ([_request]
   (handle-aif-stack-live
    _request
    {:evidence-store (or (:evidence-store @mcs/!config)
                         estore/!store)}))
  ([_request config]
   (try
     (let [generate (requiring-resolve 'futon3c.aif.stack-generator/generate-live)
           evidence-store (evidence-store-for-config config)]
       (if generate
         (if-let [stack (generate {:evidence-store evidence-store})]
           (let [scheduler (portfolio-scheduler-status-snapshot)
                 stack-with-sched (cond-> stack
                                    scheduler (assoc :scheduler scheduler))]
             (-> (json-response 200 (stringify-wm-keys stack-with-sched))
                 (assoc-in [:headers "Access-Control-Allow-Origin"] "*")))
           (json-response 503
                          {:ok false
                           :error "stack-generator returned nil"
                           :hint  "check that THE-STACK.aif.edn exists at futon5a/holes/stories/"}))
         (json-response 503
                        {:ok false
                         :error "futon3c.aif.stack-generator not loadable"})))
     (catch Exception e
       (json-response 500 {:ok false
                           :error "aif-stack-generation-failed"
                           :message (.getMessage e)})))))

(defn- handle-show-in-emacs
  "POST /api/alpha/war-machine/show-in-emacs — open a War Machine target in
   the user's running Emacs.

   Body shapes:
     {:kind 'vsatarcs-story' :leaf 'leaf-invariants' :scene-anchor 'optional'}
     {:kind 'workspace-file' :path 'futon3/holes/strategy/globe1-market-interface.devmap'}

   Delegates to `futon3c.aif.emacs-bridge/open-target` via
   requiring-resolve so this transport namespace stays free of subprocess
   dependencies (per I-2)."
  [request]
  (let [body   (parse-json-map (read-body request))
        kind   (some-> (get body :kind) str)
        leaf   (some-> (get body :leaf) str)
        anchor (some-> (get body :scene-anchor) str)
        path   (some-> (get body :path) str)]
    (if (and (str/blank? leaf) (str/blank? path))
      (json-response 400 {:ok false :error "missing-target"
                          :hint "POST {:kind 'vsatarcs-story' :leaf 'leaf-name'} or {:kind 'workspace-file' :path 'futon3/...'}"})
      (try
        (let [opener (requiring-resolve 'futon3c.aif.emacs-bridge/open-target)
              result (opener {:kind kind :leaf leaf :scene-anchor anchor :path path})]
          (cond
            (:ok? result)
            (-> (json-response 200 (select-keys result [:ok? :kind :leaf :path :resolved-path]))
                (assoc-in [:headers "Access-Control-Allow-Origin"] "*"))

            (:emacsclient-missing? result)
            (-> (json-response 503 (assoc result :error "emacsclient-missing"))
                (assoc-in [:headers "Access-Control-Allow-Origin"] "*"))

            (#{"leaf-not-found" "workspace-file-not-found"} (:error result))
            (-> (json-response 404 result)
                (assoc-in [:headers "Access-Control-Allow-Origin"] "*"))

            :else
            (-> (json-response 500 (assoc result :error "emacsclient-failed"))
                (assoc-in [:headers "Access-Control-Allow-Origin"] "*"))))
        (catch Exception e
          (json-response 500 {:ok false
                              :error "show-in-emacs-failed"
                              :message (.getMessage e)}))))))

;; =============================================================================
;; Public API
;; =============================================================================

;; =============================================================================
;; E-wm-operator-lane — operator portal data backbone (claude-7, 2026-06-05).
;;
;; `extra-routes` is the reload-safe route extension point: once the one-time
;; re-mount wires make-handler's :else to call it, NEW routes are added by
;; editing `extra-routes` and a plain Drawbridge reload — no further re-mount.
;; Handlers use requiring-resolve to keep this ns's :require surface unchanged.
;; =============================================================================

(defn- olane-cors [resp]
  (assoc-in resp [:headers "Access-Control-Allow-Origin"] "*"))

(defn- olane-kw->str [x]
  (cond (keyword? x) (name x) (nil? x) nil :else (str x)))

(defn- olane-bulletin-item->json [it]
  {:id       (olane-kw->str (:id it))
   :title    (:title it)
   :why      (:why it)
   :lane     (olane-kw->str (:lane it))
   :source   (olane-kw->str (:source it))
   :target   (olane-kw->str (:target it))
   :salience (:salience it)
   :path     (:path it)
   :repo     (:repo it)
   :unblock-action (:unblock-action it)            ; WM needs-you items: the one concrete clearing step
   :run-id   (:run-id it)})                         ; provenance for WM-emitted items (nil otherwise)

(defn handle-operator-bulletin
  "GET /api/alpha/war-machine/operator-bulletin — the Morning Bulletin projection
   over live forward-model data (E-wm-operator-lane)."
  [_request _config]
  (try
    (let [items ((requiring-resolve 'futon3c.wm.operator-lane-adapter/operator-items))
          build (requiring-resolve 'futon3c.wm.operator-bulletin/build-bulletin)
          b     (build items :date (subs (str (Instant/now)) 0 10))
          body  {:date         (:date b)
                 :generated-at (str (Instant/now))
                 :nag          (mapv olane-bulletin-item->json (:nag b))
                 :brief        (mapv olane-bulletin-item->json (:brief b))
                 :silent-count (:silent-count b)
                 :total        (:total b)}]
      (olane-cors (json-response 200 body)))
    (catch Exception e
      (olane-cors (json-response 500 {:ok false :error "operator-bulletin-failed"
                                      :message (.getMessage e)})))))

(defn handle-forward-model
  "GET /api/alpha/forward-model — raw business forward-model surface (mint.edn)."
  [_request _config]
  (try
    (let [path @(requiring-resolve 'futon3c.wm.operator-lane-adapter/default-mint-path)
          mint ((requiring-resolve 'clojure.edn/read-string) (slurp path))]
      (olane-cors (json-response 200 mint)))
    (catch Exception e
      (olane-cors (json-response 500 {:ok false :error "forward-model-failed"
                                      :message (.getMessage e)})))))

(defn handle-cascade-real
  "GET /api/alpha/cascade-real — a live snapshot of the composing cascade (the
   'real data' the pipeline-pattern-cascade view renders): per-dimension counts,
   cross-dimension shared-node overlaps, honest holes, the canonical spine, and
   :consistent?. CORS-enabled so the file:// HTML can fetch it."
  [_request _config]
  (try
    (let [summary ((requiring-resolve 'futon3c.logic.cascade-real-live/cascade-real-summary))]
      (olane-cors (json-response 200 summary)))
    (catch Exception e
      (olane-cors (json-response 500 {:ok false :error "cascade-real-failed"
                                      :message (.getMessage e)})))))

(defn handle-cascade-real-graph
  "GET /api/alpha/cascade-real/graph — the per-section STRUCTURE (nodes+edges) the
   pipeline-pattern-cascade BODY renders (lineage / clusters / holes / arrows / held +
   the honest patterns gap), so the cascade regenerates from live data instead of the
   hand-built sketch (C-cascade-real §7 DISSOLUTION, Checklist B). CORS-enabled for the
   file:// HTML fetch. Complements /cascade-real (which gives the header metadata)."
  [_request _config]
  (try
    (let [graph ((requiring-resolve 'futon3c.logic.cascade-real-live/cascade-real-graph))]
      (olane-cors (json-response 200 graph)))
    (catch Exception e
      (olane-cors (json-response 500 {:ok false :error "cascade-real-graph-failed"
                                      :message (.getMessage e)})))))

(def ^:private jvm-incident-symbols
  {:incidents 'futon3c.runtime.incidents/incidents
   :health 'futon3c.runtime.incidents/health})

(defn- resolve-jvm-incident-fns
  [ks]
  (let [resolved
        (into {}
              (for [k ks]
                [k (try
                     (requiring-resolve (get jvm-incident-symbols k))
                     (catch Throwable _ nil))]))]
    (when (every? ifn? (vals resolved)) resolved)))

(defn- jvm-incidents-unavailable-response []
  (json-response
   501
   {:ok false
    :err "jvm-incidents-unavailable"
    :message "The serving JVM cannot resolve futon3c.runtime.incidents"}))

(defn handle-jvm-incidents
  "GET /api/alpha/jvm/incidents — newest durable uncaught JVM failures."
  [request]
  (if-let [{read-incidents :incidents}
           (resolve-jvm-incident-fns [:incidents])]
    (let [requested (or (parse-int (get (parse-query-params request) "limit")) 50)
          limit (-> requested (max 0) (min 500))
          records (read-incidents limit)]
      (json-response 200 {:ok true
                          :incidents records
                          :count (count records)}))
    (jvm-incidents-unavailable-response)))

(defn handle-jvm-health
  "GET /api/alpha/jvm/health — current JVM health plus durable incident count."
  [_request]
  (if-let [{health :health} (resolve-jvm-incident-fns [:health])]
    (json-response 200 (assoc (health) :ok true))
    (jvm-incidents-unavailable-response)))

(def ^:private morning-brief-symbols
  {:review! 'futon2.aif.morning-brief/review!
   :addendum! 'futon2.aif.morning-brief/addendum!
   :items 'futon2.aif.morning-brief/items
   :reviews 'futon2.aif.morning-brief/reviews
   :addenda 'futon2.aif.morning-brief/addenda
   :item-objectives 'futon2.aif.morning-brief/item-objectives})

(defn- resolve-morning-brief-fns
  [ks]
  (let [resolved
        (into {}
              (for [k ks]
                [k (try
                     (requiring-resolve (get morning-brief-symbols k))
                     (catch Throwable _ nil))]))]
    ;; `requiring-resolve` returns Vars, which are invokable but not `fn?`.
    (when (every? ifn? (vals resolved)) resolved)))

(defn- morning-brief-unavailable-response []
  (json-response
   501
   {:ok false
    :err "morning-brief-unavailable"
    :message "The serving JVM cannot resolve futon2.aif.morning-brief; Morning Brief storage is unavailable"}))

(defn- nonblank-string? [x]
  (and (string? x) (not (str/blank? x))))

(defn- duplicate-morning-brief-review-error? [e]
  (or (instance? java.nio.file.FileAlreadyExistsException e)
      (some? (:review-id (ex-data e)))
      (str/includes? (or (.getMessage e) "") "already reviewed")))

(defn handle-morning-brief-review
  "POST /api/alpha/morning-brief/review — append one typed operator review
   through futon2.aif.morning-brief/review!."
  [request]
  (if-let [{review! :review!} (resolve-morning-brief-fns [:review!])]
    (let [payload (parse-json-map (read-body request))
          attempt-id (:attempt-id payload)
          objective (:objective payload)
          answer (:answer payload)
          note (:note payload)
          reviewer (:reviewer payload)]
      (if-not (and payload
                   (every? nonblank-string?
                           [attempt-id objective answer note reviewer]))
        (json-response
         400
         {:ok false
          :err "invalid-morning-brief-review"
          :message "attempt-id, objective, answer, note, and reviewer must be non-blank strings"})
        (try
          (let [review (review! attempt-id (keyword objective) (keyword answer)
                                note reviewer)]
            (json-response 200 {:ok true :review review}))
          (catch Exception e
            (json-response
             (if (duplicate-morning-brief-review-error? e) 409 400)
             {:ok false
              :err (if (duplicate-morning-brief-review-error? e)
                     "morning-brief-review-conflict"
                     "invalid-morning-brief-review")
              :message (or (.getMessage e) "Morning Brief review failed")})))))
    (morning-brief-unavailable-response)))

(defn handle-morning-brief-addendum
  "POST /api/alpha/morning-brief/addendum — append one notebook record
   through futon2.aif.morning-brief/addendum!."
  [request]
  (if-let [{addendum! :addendum!} (resolve-morning-brief-fns [:addendum!])]
    (let [payload (parse-json-map (read-body request))
          attempt-id (:attempt-id payload)
          kind (:kind payload)
          title (:title payload)
          body (:body payload)
          author (:author payload)]
      (if-not (and payload
                   (every? nonblank-string?
                           [attempt-id kind title body author]))
        (json-response
         400
         {:ok false
          :err "invalid-morning-brief-addendum"
          :message "attempt-id, kind, title, body, and author must be non-blank strings"})
        (try
          (json-response 200 {:ok true
                              :addendum (addendum! attempt-id (keyword kind)
                                                   title body author)})
          (catch Exception e
            (json-response 400 {:ok false
                                :err "invalid-morning-brief-addendum"
                                :message (or (.getMessage e)
                                             "Morning Brief addendum failed")})))))
    (morning-brief-unavailable-response)))

(defn handle-morning-brief-pending
  "GET /api/alpha/morning-brief/pending — return items with applicable and
   answered objective sets and sorted addenda, resolved from the canonical
   futon2 store API."
  [_request]
  (if-let [{items :items reviews :reviews addenda :addenda
            item-objectives :item-objectives}
           (resolve-morning-brief-fns [:items :reviews :addenda
                                      :item-objectives])]
    (try
      (let [review-records (reviews)
            addenda-by-attempt
            (->> (addenda)
                 (group-by :attempt-id)
                 (map (fn [[attempt-id records]]
                        [attempt-id (vec (sort-by :created-at records))]))
                 (into {}))
            answered-by-attempt
            (reduce (fn [acc review]
                      (update acc (:attempt-id review) (fnil conj [])
                              (:objective review)))
                    {} review-records)
            item-records
            (mapv (fn [item]
                    (assoc item
                           :applicable-objectives (vec (item-objectives item))
                           :answered-objectives
                           (vec (distinct
                                 (get answered-by-attempt (:attempt-id item) [])))
                           :addenda (get addenda-by-attempt
                                         (:attempt-id item) [])))
                  (items))]
        (json-response 200 {:ok true :items item-records
                            :count (count item-records)}))
      (catch Exception e
        (json-response 500 {:ok false :err "morning-brief-read-failed"
                            :message (.getMessage e)})))
    (morning-brief-unavailable-response)))

(defn extra-routes
  "Reload-safe route extension point for E-wm-operator-lane and future routes.
   Returns a response map, or nil to fall through to make-handler's 404."
  [request config]
  (let [method (:request-method request)
        uri    (:uri request)]
    (cond
      (and (= :get method) (= "/api/alpha/jvm/incidents" uri))
      (handle-jvm-incidents request)

      (and (= :get method) (= "/api/alpha/jvm/health" uri))
      (handle-jvm-health request)

      (and (= :post method) (= "/api/alpha/morning-brief/review" uri))
      (handle-morning-brief-review request)

      (and (= :post method) (= "/api/alpha/morning-brief/addendum" uri))
      (handle-morning-brief-addendum request)

      (and (= :get method) (= "/api/alpha/morning-brief/pending" uri))
      (handle-morning-brief-pending request)

      (and (= :get method) (= "/api/alpha/cascade-real/graph" uri))
      (handle-cascade-real-graph request config)

      (and (= :get method) (= "/api/alpha/cascade-real" uri))
      (handle-cascade-real request config)

      (and (= :get method) (= "/api/alpha/war-machine/operator-bulletin" uri))
      (handle-operator-bulletin request config)

      (and (= :get method) (= "/api/alpha/forward-model" uri))
      (handle-forward-model request config)

      (and (= :get method) (= "/api/alpha/coordination/edges" uri))
      (handle-coordination-edges request)

      (and (= :get method) (= "/api/alpha/coordination/qa" uri))
      (handle-coordination-qa request)

      (and (= :get method) (= "/api/alpha/coordination/threads" uri))
      (handle-coordination-threads request)

      ;; C-cascade-real D1/O3: durable auto-clock for the repl buffer to poll.
      (and (= :get method) (= "/api/alpha/agent-clock" uri))
      (handle-agent-clock request)

      ;; M-live-efe-map VERIFY: read-only live join over agents, WM ticks,
      ;; clocks, invoke jobs, and the frozen EFE coordinate set.
      (and (= :get method) (= "/api/alpha/live-efe-map" uri))
      (let [f (requiring-resolve 'futon3c.live-efe-map/build-response)]
        (-> (json-response 200
                           (f {:registry (reg/registry-status)
                               :invoke-jobs (recent-invoke-jobs 200)
                               :evidence-store (evidence-store-for-config config)
                               :wm-limit 25}))
            (assoc-in [:headers "Access-Control-Allow-Origin"] "*")))

      ;; E-repl-continuations Car 2b: register a parked-on continuation.
      (and (= :post method) (= "/api/alpha/park" uri))
      (handle-park request config)

      ;; E-repl-continuations Car 2b: repl buffer polls for ready resumes.
      (and (= :get method) (= "/api/alpha/parked/ready" uri))
      (handle-parked-ready request config)

      ;; E-park-delivery-losses bug 3: ACK a leased ready resume (confirm delivery).
      (and (= :post method) (= "/api/alpha/parked/ready/ack" uri))
      (handle-parked-ready-ack request config)

      ;; E-repl-continuations: outstanding parks (within-turn unification).
      (and (= :get method) (= "/api/alpha/parked" uri))
      (handle-parked request config)

      :else nil)))

(defn make-handler
  "Create an HTTP request handler wired to the social pipeline.

   config:
     :registry  — AgentRegistryShape (may include :peripheral-config)
     :patterns  — PatternLibrary

   Returns a Ring handler fn that routes to the social pipeline."
  [config]
  (let [started-at (Instant/now)]
    (fn [request]
      (try
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

          ;; Walkie-talkie: pattern search
          (and (= :get method) (= "/api/alpha/patterns/search" uri))
          (handle-patterns-search request)

          ;; Walkie-talkie: PSR/PUR/PAR evidence endpoints
          (and (= :post method) (= "/api/alpha/evidence/psr" uri))
          (handle-psr request config)

          (and (= :post method) (= "/api/alpha/evidence/pur" uri))
          (handle-pur request config)

          (and (= :post method) (= "/api/alpha/evidence/par" uri))
          (handle-par request config)

          ;; Walkie-talkie: backpack
          (and (= :get method) (string? uri)
               (str/starts-with? uri "/api/alpha/backpack/"))
          (let [raw (subs uri (count "/api/alpha/backpack/"))]
            (handle-backpack (enc/decode-uri-component raw)))

          ;; Walkie-talkie: ArSE endpoints
          (and (= :post method) (= "/api/alpha/arse/ask" uri))
          (handle-arse-ask request config)

          (and (= :post method) (= "/api/alpha/arse/answer" uri))
          (handle-arse-answer request config)

          (and (= :get method) (= "/api/alpha/arse/unanswered" uri))
          (handle-arse-unanswered request config)

          (and (= :post method) (= "/api/alpha/invoke" uri))
          (handle-invoke request config)

          (and (= :post method) (= "/api/alpha/invoke/announce" uri))
          (handle-invoke-announce request config)

          (and (= :post method) (= "/api/alpha/bell" uri))
          (handle-bell request config)

          ;; E-pilot-hop-trigger-wiring §(5): agency hop endpoints
          (and (= :post method) (= "/api/alpha/agency/hop" uri))
          (handle-agency-hop request)

          (and (= :post method) (= "/api/alpha/agency/hop-back" uri))
          (handle-agency-hop-back request)

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

          (and (= :post method) (= "/api/alpha/mc/sync-mission" uri))
          (handle-mc-sync-mission request config)

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

          (and (= :get method) (= "/api/alpha/invariants" uri))
          (handle-invariants request config)

          ;; War Machine — strategic synthesis snapshot for the CLJS viewer
          (and (= :get method) (= "/api/alpha/war-machine" uri))
          (handle-war-machine request)

          (and (= :get method) (= "/api/alpha/aif-stack/live" uri))
          (handle-aif-stack-live request config)

          (and (= :post method)
               (= "/api/alpha/war-machine/show-in-emacs" uri))
          (handle-show-in-emacs request)

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

          (and (= :post method) (= "/api/alpha/agents/restore" uri))
          (handle-agent-restore request config)

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
               (str/ends-with? uri "/status"))
          (let [raw (subs uri (count "/api/alpha/agents/")
                         (- (count uri) (count "/status")))]
            (handle-agent-status config (enc/decode-uri-component raw) request))

          (and (= :post method) (string? uri)
               (str/starts-with? uri "/api/alpha/agents/")
               (str/ends-with? uri "/interrupt-invoke"))
          (let [raw (subs uri (count "/api/alpha/agents/")
                         (- (count uri) (count "/interrupt-invoke")))]
            (handle-agent-interrupt-invoke config (enc/decode-uri-component raw) request))

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

          ;; Mission inventory endpoint
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
          (or (extra-routes request config)
              (json-response 404 {"error" true
                                  "code" "not-found"
                                  "message" (str "Unknown endpoint: "
                                                (some-> method name str/upper-case)
                                                " " uri)}))))
        (catch InterruptedException _
          ;; JVM is tearing down (or a downstream XTDB query thread was
          ;; interrupted). Convert to a clean 503 instead of letting an
          ;; ERROR log + stacktrace surface during graceful shutdown.
          (Thread/interrupted)  ; clear the interrupt flag
          (json-response 503 {:ok false :error "shutting-down"}))))))

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
      (do
        (try (start-parked-on!) (catch Throwable t
                                  (println (str "[parked-on] boot failed: " (.getMessage t)))))
        {:server stop-fn :port port :started-at (str (Instant/now))})
      (do (stop-fn)
          (throw (ex-info "Server started but port is not listening (L7: verify-after-start)"
                          {:port port}))))))
