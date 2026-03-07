(ns futon3c.social.whistles
  "Whistle dispatcher — synchronous request-response coordination primitive.

   Whistles are blocking requests that invoke an agent and wait for a response.
   The counterpart to bells (async fire-and-forget): bell is 'tell', whistle
   is 'ask'.

   Architecture:
   - Whistles delegate to registry/invoke-agent! for the actual invocation
   - Evidence store records the whistle exchange (request + response)
   - No subprocess spawning — agents are already running, whistles just invoke them
   - Timeout handling inherited from invoke-agent!

   Pattern references:
   - realtime/rendezvous-handshake: whistle send + response = exchange proof
   - realtime/liveness-heartbeats: whistle to idle agent proves it is responding"
  (:require [futon3c.agency.registry :as registry]
            [futon3c.evidence.store :as estore]
            [clojure.string :as str])
  (:import [java.time Instant]
           [java.util UUID]))

(defn- now-str [] (str (Instant/now)))

(defn- truncate [s max-len]
  (let [s (str s)]
    (if (<= (count s) max-len)
      s
      (subs s 0 max-len))))

(defn- make-whistle-evidence
  "Create a combined evidence entry for a whistle exchange."
  [agent-id prompt response status author session-id]
  {:evidence/id (str "e-" (UUID/randomUUID))
   :evidence/subject {:ref/type :agent :ref/id (str agent-id)}
   :evidence/type :coordination
   :evidence/claim-type :step
   :evidence/author (or author "whistle-dispatcher")
   :evidence/at (now-str)
   :evidence/body {:agent-id agent-id
                   :prompt (truncate prompt 200)
                   :response (truncate response 500)
                   :status status}
   :evidence/tags [:whistle :coordination]
   :evidence/session-id (or session-id (str "whistle-" (UUID/randomUUID)))})

(defn- resolve-delivery-recorder
  "Best-effort resolver for invoke delivery receipt recorder.
   Returns (fn [agent-id invoke-trace-id receipt]) or nil."
  []
  (try
    (when-let [dev-ns (find-ns 'futon3c.dev)]
      (when-let [record-fn (ns-resolve dev-ns 'record-invoke-delivery!)]
        (fn [agent-id invoke-trace-id receipt]
          (record-fn agent-id invoke-trace-id receipt))))
    (catch Throwable _
      nil)))

(def ^:dynamic *resolve-delivery-recorder*
  "Indirection for delivery recorder lookup (test seam)."
  resolve-delivery-recorder)

(defn- record-whistle-delivery!
  "Mark where a whistle response was delivered.
   Delivery happens via return value to the whistle caller surface."
  [agent-id author result]
  (let [invoke-meta (:invoke-meta result)
        invoke-trace-id (or (some-> invoke-meta :invoke-trace-id str str/trim not-empty)
                            (some-> invoke-meta :invoke_trace_id str str/trim not-empty)
                            (some-> invoke-meta (get "invoke-trace-id") str str/trim not-empty)
                            (some-> invoke-meta (get "invoke_trace_id") str str/trim not-empty))]
    (when invoke-trace-id
      (when-let [record! (*resolve-delivery-recorder*)]
        (try
          (record! (str agent-id) invoke-trace-id
                   {:surface "whistle"
                    :destination (str "caller " (or (some-> author str str/trim not-empty) "unknown"))
                    :delivered? true
                    :note (if (:ok result) "whistle-response" "whistle-error")})
          (catch Throwable _ nil))))))

(defn whistle!
  "Send a synchronous request to an agent and wait for response.
  Bell is 'tell', whistle is 'ask'.

   config:
     :agent-id       — target agent (string or TypedAgentId)
     :prompt         — the question/request to send
     :author         — who initiated the whistle (e.g. \"joe\", \"claude-1\")
     :timeout-ms     — optional, default 1800000 (30 minutes)
     :evidence-store — optional (emits coordination evidence if provided)

   Returns:
     {:whistle/ok true  :whistle/response \"...\" :whistle/agent-id \"...\"
      :whistle/session-id \"...\" :whistle/at \"...\"}

   Or on failure:
     {:whistle/ok false :whistle/error \"...\" :whistle/agent-id \"...\"
      :whistle/at \"...\"}"
  [{:keys [agent-id prompt author timeout-ms evidence-store]}]
  (cond
    (nil? agent-id)
    {:whistle/ok false :whistle/error "whistle requires :agent-id"
     :whistle/at (now-str)}

    (nil? prompt)
    {:whistle/ok false :whistle/error "whistle requires :prompt"
     :whistle/agent-id (str agent-id) :whistle/at (now-str)}

    :else
    (let [aid-val (if (map? agent-id) (:id/value agent-id) (str agent-id))
          timeout (or timeout-ms 1800000)
          result (registry/invoke-agent! aid-val prompt timeout)
          status (cond
                   (:ok result) :completed
                   (let [e (:error result)
                         msg (if (map? e) (:error/message e) (str e))]
                     (and (string? msg) (str/includes? msg "timeout")))
                   :timeout
                   :else :error)
          response-str (if (:ok result)
                         (str (:result result))
                         (let [e (:error result)]
                           (if (map? e) (:error/message e) (str e))))]
      ;; Emit evidence
      (when evidence-store
        (estore/append* evidence-store
                        (make-whistle-evidence
                         aid-val prompt response-str status
                         (or author "whistle-dispatcher")
                         (:session-id result))))
      ;; Record delivery on whistle surface (best-effort, no impact on return value).
      (record-whistle-delivery! aid-val author result)
      (let [invoke-meta (:invoke-meta result)
            invoke-trace-id (or (some-> invoke-meta :invoke-trace-id str str/trim not-empty)
                                (some-> invoke-meta :invoke_trace_id str str/trim not-empty)
                                (some-> invoke-meta (get "invoke-trace-id") str str/trim not-empty)
                                (some-> invoke-meta (get "invoke_trace_id") str str/trim not-empty))]
      ;; Return whistle-shaped result
      (if (:ok result)
        (cond-> {:whistle/ok true
                 :whistle/response (:result result)
                 :whistle/agent-id aid-val
                 :whistle/session-id (:session-id result)
                 :whistle/at (now-str)}
          invoke-trace-id (assoc :whistle/invoke-trace-id invoke-trace-id))
        (cond-> {:whistle/ok false
                 :whistle/error response-str
                 :whistle/agent-id aid-val
                 :whistle/at (now-str)}
          invoke-trace-id (assoc :whistle/invoke-trace-id invoke-trace-id)))))))
