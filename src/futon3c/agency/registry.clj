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
  (:require [futon3c.blackboard :as bb]
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
             current-session (:agent/session-id agent)
             timeout-ms (when (and timeout-ms (pos? (long timeout-ms))) (long timeout-ms))
             prompt-preview (let [s (str prompt)]
                              (subs s 0 (min 120 (count s))))
             project-agents! (fn []
                               (bb/project-agents!
                                {:agents (into {}
                                               (map (fn [[aid a]]
                                                      [aid (cond-> {:type (:agent/type a)
                                                                    :status (or (:agent/status a) :idle)}
                                                             (:agent/invoke-started-at a)
                                                             (assoc :invoke-started-at (str (:agent/invoke-started-at a))
                                                                    :invoke-prompt-preview (:agent/invoke-prompt-preview a)))])
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
                                                    :agent/invoke-prompt-preview nil}))
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
                 {:ok true :result result :session-id session-id}))

             (ws-invoke/available? aid-val)
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
                 {:ok true :result (:result response) :session-id (:session-id response)}

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
                        "Agent has no invoke handler"
                        :agent-id aid-val)}))
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
;; Introspection
;; =============================================================================

(defn registry-status
  "Return status of all registered agents."
  []
  {:agents
   (into {}
         (map (fn [[aid agent]]
                [aid (cond-> {:type (:agent/type agent)
                              :id (:agent/id agent)
                              :session-id (:agent/session-id agent)
                              :registered-at (str (:agent/registered-at agent))
                              :last-active (str (:agent/last-active agent))
                              :capabilities (:agent/capabilities agent)
                              :ttl-ms (:agent/ttl-ms agent)
                              :status (or (:agent/status agent) :idle)}
                       (:agent/invoke-started-at agent)
                       (assoc :invoke-started-at (str (:agent/invoke-started-at agent))
                              :invoke-prompt-preview (:agent/invoke-prompt-preview agent)))])
              @!registry))
   :count (count @!registry)})

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
