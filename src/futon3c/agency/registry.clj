(ns futon3c.agency.registry
  "Unified agent registry — single source of truth for agent state.

   Rewritten from futon3/agency/registry.clj with R1-R11 compliance:

   R2: Single routing authority — one atom, not three stores.
       Register same ID twice → error, not overwrite.
   R3: Atomic state transitions — swap! with validation.
       Concurrent register/unregister cannot corrupt state.
   R4: Loud failure — all operations return typed maps or SocialError.
       No (catch Exception _ nil), no boolean returns.
   R5: Bounded lifecycle — agents have :registered-at + optional :ttl-ms.
       reap-expired! removes agents past their TTL.
   R6: Typed identifiers — agent IDs are TypedAgentId maps.
       {:id/value \"claude-1\" :id/type :continuity}

   Design: single registry atom with one entry per agent-id value.
   The triple-store problem from futon3 (registry + local-handlers +
   connected-agents) is eliminated by having one authoritative store."
  (:require [clojure.string :as str]
            [futon3c.blackboard :as bb]
            [futon3c.social.shapes :as shapes]
            [futon3c.transport.ws.invoke :as ws-invoke])
  (:import [java.time Instant]))

;; =============================================================================
;; Agent Registry — single atom, single routing authority (R2)
;; =============================================================================

(defonce ^{:doc "Optional callback invoked after successful registration.
   Set via set-on-register! to enable federation announcements.
   Signature: (fn [agent-record] ...) — called asynchronously."}
  !on-register (atom nil))

(def ^:private ws-invoke-timeout-ms 120000)

(defonce ^{:doc "Registry of agents.

   Structure: {agent-id-value -> agent-record}

   Agent record:
   {:agent/id         TypedAgentId
    :agent/type       :claude | :codex | :tickle | :mock | :peripheral
    :agent/invoke-fn  (fn [prompt session-id] -> result-map)
    :agent/capabilities [:keyword ...]
    :agent/session-id string (may be updated by invoke)
    :agent/registered-at Instant
    :agent/last-active Instant
    :agent/ttl-ms     long (optional — bounded lifecycle R5)
    :agent/metadata   map}"}
  !registry
  (atom {}))

;; =============================================================================
;; Helpers
;; =============================================================================

(defn- agent-id-value
  "Extract the string value from a TypedAgentId."
  [typed-id]
  (cond
    (map? typed-id)    (let [v (:id/value typed-id)]
                          (if (string? v) v (str v)))
    (string? typed-id) typed-id
    (keyword? typed-id) (name typed-id)
    :else              (str typed-id)))

(defn- make-social-error
  "Create a SocialError map (R4)."
  [code message & {:as context}]
  {:error/component :registry
   :error/code code
   :error/message message
   :error/at (str (Instant/now))
   :error/context (or context {})})

(defn- now [] (Instant/now))

(defn- invoke-routing-info
  "Compute invoke routing readiness for an agent record.
   Exposes whether invoke will route via local fn, WS bridge, or fail."
  [aid-val agent]
  (let [local? (fn? (:agent/invoke-fn agent))
        ws-available? (ws-invoke/available? aid-val)
        route (cond
                local? :local
                ws-available? :ws
                :else :none)
        note (or (get-in agent [:agent/metadata :note])
                 (get-in agent [:agent/metadata "note"]))
        agent-type (:agent/type agent)
        diagnostic (case route
                     :local "local invoke-fn registered"
                     :ws "ws bridge connected"
                     (let [base (if (= :codex agent-type)
                                  "ws bridge not connected — start codex bridge on laptop"
                                  "no local invoke-fn and no ws bridge")]
                       (if (and (string? note) (not (str/blank? note)))
                         (str base " (" note ")")
                         base)))]
    {:invoke-route route
     :invoke-ready? (not= :none route)
     :invoke-local? local?
     :invoke-ws-available? ws-available?
     :invoke-diagnostic diagnostic}))

;; =============================================================================
;; Registry Operations
;; =============================================================================

(defn reset-registry!
  "Reset the registry to empty state. For testing only."
  []
  (reset! !registry {}))

(defn set-on-register!
  "Set callback invoked asynchronously after successful agent registration.
   Pass nil to clear. Signature: (fn [agent-record] ...)."
  [f]
  (reset! !on-register f))

(defn get-agent
  "Get agent record by typed ID, or nil if not registered."
  [typed-id]
  (get @!registry (agent-id-value typed-id)))

(defn agent-registered?
  "Check if an agent is registered."
  [typed-id]
  (contains? @!registry (agent-id-value typed-id)))

(defn register-agent!
  "Register an agent with the registry.

   Options:
     :agent-id      - Required. TypedAgentId map.
     :type          - Required. :claude, :codex, :tickle, :mock, or :peripheral.
     :invoke-fn     - Required. Function (fn [prompt session-id] -> result-map).
     :capabilities  - Required. Vector of keyword capabilities.
     :session-id    - Optional. Initial session ID.
     :ttl-ms        - Optional. Bounded lifecycle in milliseconds (R5).
     :metadata      - Optional. Arbitrary metadata map.

   Returns:
     Agent record on success (R1: typed result).
     {:ok false :error SocialError} on failure (R2: duplicate → error, not overwrite)."
  [{:keys [agent-id type invoke-fn capabilities session-id ttl-ms metadata]}]
  (let [aid-val (agent-id-value agent-id)
        typed-id (if (map? agent-id)
                   agent-id
                   {:id/value (str agent-id) :id/type :continuity})
        ts (now)
        agent-record {:agent/id typed-id
                      :agent/type type
                      :agent/invoke-fn invoke-fn
                      :agent/capabilities (vec (or capabilities []))
                      :agent/session-id session-id
                      :agent/registered-at ts
                      :agent/last-active ts
                      :agent/ttl-ms ttl-ms
                      :agent/metadata (or metadata {})}
        ;; R2: Atomic check-and-set — reject duplicate, don't overwrite
        result (atom nil)]
    (swap! !registry
           (fn [m]
             (if (contains? m aid-val)
               (do (reset! result
                           {:ok false
                            :error (make-social-error
                                    :duplicate-registration
                                    (str "Agent already registered: " aid-val)
                                    :existing-id aid-val)})
                   m)
               (do (reset! result agent-record)
                   (assoc m aid-val agent-record)))))
    (let [r @result]
      ;; Fire on-register hook asynchronously for federation announcement
      (when (and (map? r) (:agent/id r))
        (when-let [hook @!on-register]
          (future
            (try (hook r)
                 (catch Exception _)))))
      r)))

(defn unregister-agent!
  "Unregister an agent.

   Returns:
     {:ok true :agent/id TypedAgentId} on success (R1: typed result).
     {:ok false :error SocialError} if agent was not registered (R4: loud failure)."
  [typed-id]
  (let [aid-val (agent-id-value typed-id)
        result (atom nil)]
    (swap! !registry
           (fn [m]
             (if-let [agent (get m aid-val)]
               (do (reset! result {:ok true :agent/id (:agent/id agent)})
                   (dissoc m aid-val))
               (do (reset! result
                           {:ok false
                            :error (make-social-error
                                    :agent-not-found
                                    (str "Agent not registered: " aid-val)
                                    :agent-id aid-val)})
                   m))))
    @result))

(defn deregister-agent!
  "Compatibility wrapper around `unregister-agent!`.

   Returns legacy shape:
   {:ok true :agent-id id} or {:ok false :error \"not-found\"}."
  [agent-id]
  (let [aid-val (agent-id-value agent-id)
        result (unregister-agent! agent-id)]
    (if (:ok result)
      {:ok true :agent-id aid-val}
      {:ok false :error "not-found"})))

(defn update-agent!
  "Update fields in an agent record atomically.

   Returns updated agent record, or {:ok false :error SocialError}."
  [typed-id & {:as updates}]
  (let [aid-val (agent-id-value typed-id)
        result (atom nil)]
    (swap! !registry
           (fn [m]
             (if-let [agent (get m aid-val)]
               (let [updated (merge agent updates {:agent/last-active (now)})]
                 (reset! result updated)
                 (assoc m aid-val updated))
               (do (reset! result
                           {:ok false
                            :error (make-social-error
                                    :agent-not-found
                                    (str "Agent not registered: " aid-val))})
                   m))))
    @result))

(defn reset-session!
  "Clear an agent's session-id so the next invoke starts a fresh conversation.
   Useful when a session becomes poisoned (e.g. invalid tool-use in history).

   Returns:
     {:ok true :agent-id aid :old-session-id old-sid} on success.
     {:ok false :error SocialError} if agent not found."
  [agent-id]
  (let [aid-val (agent-id-value agent-id)
        result (atom nil)]
    (swap! !registry
           (fn [m]
             (if-let [agent (get m aid-val)]
               (let [old-sid (:agent/session-id agent)]
                 (reset! result {:ok true
                                 :agent-id aid-val
                                 :old-session-id old-sid})
                 (assoc m aid-val (assoc agent
                                        :agent/session-id nil
                                        :agent/last-active (now))))
               (do (reset! result
                           {:ok false
                            :error (make-social-error
                                    :agent-not-found
                                    (str "Agent not registered: " aid-val)
                                    :agent-id aid-val)})
                   m))))
    @result))

;; =============================================================================
;; Invocation (R1: delivery receipt, R4: loud failure)
;; =============================================================================

(defn invoke-agent!
  "Invoke an agent with a prompt.

   Looks up agent, calls invoke-fn, updates session-id and last-active.

   Returns:
     {:ok true :result ... :session-id ...} on success.
     {:ok false :error SocialError} on failure (R4: typed error with component)."
  ([typed-id prompt]
   (invoke-agent! typed-id prompt nil))
  ([typed-id prompt timeout-ms]
   (let [aid-val (agent-id-value typed-id)]
     (if-let [agent (get @!registry aid-val)]
       (let [invoke-fn (:agent/invoke-fn agent)
             routing-info (invoke-routing-info aid-val agent)
             current-session (:agent/session-id agent)
             timeout-ms (when (and timeout-ms (pos? (long timeout-ms))) (long timeout-ms))
             prompt-preview (let [s (str prompt)]
                              (subs s 0 (min 120 (count s))))
             project-agents! (fn []
                               (bb/project-agents!
                                {:agents (into {}
                                               (map (fn [[aid a]]
                                                      [aid (cond-> {:type (:agent/type a)
                                                                    :metadata (:agent/metadata a)
                                                                    :status (or (:agent/status a) :idle)}
                                                             (:agent/invoke-started-at a)
                                                             (assoc :invoke-started-at (str (:agent/invoke-started-at a))
                                                                    :invoke-prompt-preview (:agent/invoke-prompt-preview a))
                                                             (:agent/invoke-activity a)
                                                             (assoc :invoke-activity (:agent/invoke-activity a)))])
                                                    @!registry))
                                 :count (count @!registry)}))
             mark-invoking! (fn []
                              (swap! !registry
                                     (fn [m]
                                       (if-let [a (get m aid-val)]
                                         (assoc m aid-val
                                                (assoc a
                                                       :agent/status :invoking
                                                       :agent/invoke-started-at (now)
                                                       :agent/invoke-prompt-preview prompt-preview))
                                         m)))
                              (project-agents!))
             mark-idle! (fn [session-id]
                          ;; Update only if still registered (R5: no resurrect).
                          (swap! !registry
                                 (fn [m]
                                   (if-let [agent* (get m aid-val)]
                                     (assoc m aid-val
                                            (merge agent*
                                                   {:agent/session-id (or session-id current-session)
                                                    :agent/last-active (now)
                                                    :agent/status :idle
                                                    :agent/invoke-started-at nil
                                                    :agent/invoke-prompt-preview nil
                                                    :agent/invoke-activity nil
                                                    :agent/invoke-event-sink nil}))
                                     m)))
                          (project-agents!))]
         (mark-invoking!)
         (try
           (cond
             invoke-fn
                   (let [call-invoke (fn []
                                 (try
                                   (invoke-fn prompt current-session)
                                   (catch clojure.lang.ArityException _
                                     (invoke-fn prompt))))
                   result-map (if timeout-ms
                                (let [f (future (call-invoke))
                                      v (deref f timeout-ms ::timeout)]
                                  (if (= v ::timeout)
                                    (do (future-cancel f)
                                        {:error "timeout" :exit-code -1 :timeout-ms timeout-ms})
                                    v))
                                (call-invoke))
                   {:keys [result session-id error]} result-map]
               (mark-idle! session-id)
               (if error
                 {:ok false
                  :error (make-social-error
                          :invoke-error
                          (str error)
                          :agent-id aid-val
                          :timeout-ms (:timeout-ms result-map))}
                 (let [invoke-meta (not-empty (dissoc result-map :result :session-id :error))]
                   (cond-> {:ok true :result result :session-id session-id}
                     invoke-meta (assoc :invoke-meta invoke-meta)))))

             (:invoke-ws-available? routing-info)
             (let [prompt-str (if (string? prompt) prompt (pr-str prompt))
                   response (ws-invoke/invoke! aid-val prompt-str current-session timeout-ms)
                   session-id (when (map? response) (:session-id response))]
               (mark-idle! session-id)
               (cond
                 (= response ws-invoke/timeout-sentinel)
                 {:ok false
                  :error (make-social-error
                          :invoke-error
                          (str "WS invoke timeout after " (or timeout-ms ws-invoke-timeout-ms) "ms")
                          :agent-id aid-val
                          :timeout-ms (or timeout-ms ws-invoke-timeout-ms))}

                 (and (map? response) (:error response))
                 {:ok false
                  :error (make-social-error
                          :invoke-error
                          (str (:error response))
                          :agent-id aid-val)}

                 (map? response)
                 (let [invoke-meta (not-empty (dissoc response :result :session-id :error))]
                   (cond-> {:ok true :result (:result response) :session-id (:session-id response)}
                     invoke-meta (assoc :invoke-meta invoke-meta)))

                 :else
                 {:ok false
                  :error (make-social-error
                          :invoke-error
                          "Unknown WS invoke failure"
                          :agent-id aid-val)}))

             :else
             (do
               (mark-idle! nil)
               {:ok false
                :error (make-social-error
                        :invoke-error
                        (str "Agent has no invoke handler (" (:invoke-diagnostic routing-info) ")")
                        :agent-id aid-val
                        :invoke-route (:invoke-route routing-info)
                        :invoke-local? (:invoke-local? routing-info)
                        :invoke-ws-available? (:invoke-ws-available? routing-info))}))
           (catch Exception e
             (mark-idle! nil)
             {:ok false
              :error (make-social-error
                      :invoke-exception
                      (.getMessage e)
                      :agent-id aid-val
                      :exception-class (.getName (class e)))})))
       {:ok false
        :error (make-social-error
                :agent-not-found
                (str "Agent not registered: " aid-val)
                :agent-id aid-val)}))))

;; =============================================================================
;; Bounded Lifecycle (R5)
;; =============================================================================

(defn reap-expired!
  "Remove agents whose TTL has expired (R5: bounded lifecycle).
   Returns vector of reaped agent IDs."
  []
  (let [now-ms (System/currentTimeMillis)
        reaped (atom [])]
    (swap! !registry
           (fn [m]
             (reduce-kv
              (fn [acc aid-val agent]
                (if-let [ttl (:agent/ttl-ms agent)]
                  (let [registered-ms (.toEpochMilli ^Instant (:agent/registered-at agent))
                        expired? (> now-ms (+ registered-ms ttl))]
                    (if expired?
                      (do (swap! reaped conj (:agent/id agent))
                          (dissoc acc aid-val))
                      acc))
                  acc))
              m
              m)))
    @reaped))

;; =============================================================================
;; Activity updates — called from invoke fns during long-running operations
;; =============================================================================

(defn update-invoke-activity!
  "Update the current activity string for an invoking agent.
   Called from invoke-fn stream parsers to surface tool use, thinking, etc.
   Does NOT trigger a blackboard refresh — the ticker handles that every 5s."
  [agent-id-val activity-str]
  (swap! !registry
         (fn [m]
           (if-let [a (get m agent-id-val)]
             (assoc m agent-id-val
                    (assoc a :agent/invoke-activity activity-str))
             m))))

(defn set-invoke-event-sink!
  "Set a streaming event callback for an agent. sink-fn: (fn [event-map])."
  [agent-id-val sink-fn]
  (swap! !registry
         (fn [m]
           (if-let [a (get m agent-id-val)]
             (assoc m agent-id-val (assoc a :agent/invoke-event-sink sink-fn))
             m))))

(defn get-invoke-event-sink
  "Get the event sink callback for an agent, or nil."
  [agent-id-val]
  (:agent/invoke-event-sink (get @!registry agent-id-val)))

(defn clear-invoke-event-sink!
  "Remove the event sink callback for an agent."
  [agent-id-val]
  (swap! !registry
         (fn [m]
           (if-let [a (get m agent-id-val)]
             (assoc m agent-id-val (dissoc a :agent/invoke-event-sink))
             m))))

;; =============================================================================
;; Introspection
;; =============================================================================

(defn running-codex-session-ids
  "Best-effort detection of local `codex exec --json resume <sid>` processes.
   Returns a set of active session IDs.

   This is used to surface external Codex activity (e.g. emacs codex-repl)
   in the shared *agents* panel even when that invoke did not flow through
   registry/invoke-agent!."
  []
  (try
    (with-open [processes (java.lang.ProcessHandle/allProcesses)]
      (->> (iterator-seq (.iterator processes))
           (keep (fn [^java.lang.ProcessHandle process]
                   (let [cmd-opt (.. process info commandLine)]
                     (when (.isPresent cmd-opt)
                       (let [line (.get cmd-opt)]
                         (when (and (str/includes? line "codex exec --json")
                                    (str/includes? line " resume "))
                           (second (re-find #"resume\s+([0-9a-fA-F-]{36})\b" line))))))))
           set))
    (catch Throwable _
      #{})))

(defn registry-status
  "Return status of all registered agents."
  []
  (let [codex-session-ids (running-codex-session-ids)]
    {:agents
     (into {}
           (map (fn [[aid agent]]
                  (let [base-status (or (:agent/status agent) :idle)
                        routing-info (invoke-routing-info aid agent)
                        session-id (:agent/session-id agent)
                        external-codex-invoking?
                        (and (= :codex (:agent/type agent))
                             (not= base-status :invoking)
                             (string? session-id)
                             (contains? codex-session-ids session-id))
                        status (if external-codex-invoking? :invoking base-status)
                        invoke-started-at (or (:agent/invoke-started-at agent)
                                              (when external-codex-invoking?
                                                (:agent/last-active agent)))
                        invoke-prompt-preview (or (:agent/invoke-prompt-preview agent)
                                                  (when external-codex-invoking?
                                                    "[external invoke]"))
                        invoke-activity (or (:agent/invoke-activity agent)
                                            (when external-codex-invoking?
                                              "codex exec running (external surface)"))]
                    [aid (cond-> {:type (:agent/type agent)
                                  :id (:agent/id agent)
                                  :session-id session-id
                                  :registered-at (str (:agent/registered-at agent))
                                  :last-active (str (:agent/last-active agent))
                                  :capabilities (:agent/capabilities agent)
                                  :ttl-ms (:agent/ttl-ms agent)
                                  :metadata (:agent/metadata agent)
                                  :invoke-route (:invoke-route routing-info)
                                  :invoke-ready? (:invoke-ready? routing-info)
                                  :invoke-local? (:invoke-local? routing-info)
                                  :invoke-ws-available? (:invoke-ws-available? routing-info)
                                  :invoke-diagnostic (:invoke-diagnostic routing-info)
                                  :status status}
                           invoke-started-at
                           (assoc :invoke-started-at (str invoke-started-at)
                                  :invoke-prompt-preview invoke-prompt-preview)
                           invoke-activity
                           (assoc :invoke-activity invoke-activity))]))
                @!registry))
     :count (count @!registry)}))

(defn registered-agents
  "Return list of registered TypedAgentId maps."
  []
  (mapv :agent/id (vals @!registry)))

(defn shutdown-all!
  "Unregister all agents. Returns count of agents removed."
  []
  (let [n (count @!registry)]
    (reset! !registry {})
    n))
