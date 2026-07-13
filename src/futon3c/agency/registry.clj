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
            [futon3c.transport.ws.invoke :as ws-invoke])
  (:import [java.time Instant]))

(declare registry-status)

;; =============================================================================
;; Agent Registry — single atom, single routing authority (R2)
;; =============================================================================

(defonce ^{:doc "Optional callback invoked after successful registration.
   Set via set-on-register! to enable federation announcements.
   Signature: (fn [agent-record] ...) — called asynchronously."}
  !on-register (atom nil))

(defonce ^{:doc "Optional callback invoked after a successful invoke completes
   for an agent that declares the completion-bell contract.
   Set via set-on-invoke-complete! to wire post-invoke coordination.
   Signature: (fn [agent-record result-map] ...) — called asynchronously."}
  !on-invoke-complete (atom nil))

(defonce ^{:doc "Optional callback invoked (async, in a future) whenever ANY agent
   transitions from :invoking to :idle. Unlike !on-invoke-complete, this fires
   for all agents regardless of the completion-bell contract.
   Set via set-on-idle! to wire bell-driven dispatch.
   Signature: (fn [agent-id outcome] ...) where outcome is
   {:ok bool :error str-or-nil :session-id str-or-nil}."}
  !on-idle (atom nil))

(def ^:private ws-invoke-timeout-ms 120000)
(def ^:private external-invoke-fresh-ms 15000)
(def ^:private surface-projection-fresh-ms 300000)

(def ^:dynamic *resolve-invoke-job-counts*
  "Best-effort resolver for futon3c.transport.http/active-invoke-job-counts.
   Returns a 0-arity fn or nil."
  (fn []
    (when-let [http-ns (find-ns 'futon3c.transport.http)]
      (ns-resolve http-ns 'active-invoke-job-counts))))

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
    :agent/metadata   map}"
           :durable true}
  !registry
  (atom {}))

;; ^:durable metadata (M-reachable-from-boot 2026-05-01): `!registry` is
;; the authoritative routing container. External code must go through the
;; helper surface in this namespace (`register-agent!`,
;; `unregister-agent!`, etc.), not direct `(reset! !registry ...)` or
;; `(swap! !registry ...)` from arbitrary call sites. The structural
;; guard lives in `scripts/check-reachable-from-boot-agent-registry.sh`.

;; Roster persistence (Desktop Save / W5) is installed by `start-futon3c!`
;; AFTER `restore-on-boot!` has consumed the saved roster — NOT here at ns-load.
;; Installing the watch at ns-load ran an eager initial `persist-registry!`
;; against the still-empty registry, clobbering the saved roster before restore
;; could read it (the round-trip restored 0 agents). See dev/bootstrap.clj.

(declare registry-status)

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
  (let [metadata (:agent/metadata agent)
        stale-proxy? (and (:proxy? metadata)
                          (:federation/stale? metadata))
        local? (and (fn? (:agent/invoke-fn agent))
                    (not stale-proxy?))
        ws-available? (ws-invoke/available? aid-val)
        route (cond
                stale-proxy? :none
                local? :local
                ws-available? :ws
                :else :none)
        note (or (:note metadata)
                 (get metadata "note"))
        agent-type (:agent/type agent)
        diagnostic (case route
                     :local "local invoke-fn registered"
                     :ws "ws bridge connected"
                     (let [base (cond
                                  stale-proxy?
                                  (str "federation peer unreachable"
                                       (when-let [err (:federation/last-error metadata)]
                                         (str " — " err)))

                                  (= :codex agent-type)
                                  "ws bridge not connected — start codex bridge on laptop"
                                  :else
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

(defn set-on-invoke-complete!
  "Set callback invoked asynchronously after successful invoke completion for
   agents whose metadata declares the completion-bell contract.
   Pass nil to clear. Signature: (fn [agent-record result-map] ...)."
  [f]
  (reset! !on-invoke-complete f))

(defn set-on-idle!
  "Set callback invoked (in a future) whenever any agent transitions to :idle.
   Pass nil to clear. Signature: (fn [agent-id outcome] ...) where outcome is
   {:ok bool :error str-or-nil :session-id str-or-nil}."
  [f]
  (reset! !on-idle f))

(defn- fire-on-idle!
  "Fire the !on-idle callback with agent-id and invoke outcome."
  [agent-id outcome]
  (when-let [on-idle @!on-idle]
    (future
      (try (on-idle agent-id outcome)
           (catch Exception e
             (println "[registry] on-idle callback error:" (.getMessage e)))))))

(defn- broadcast-agents-ws!
  "Broadcast agent status summary to all connected WS bridges."
  []
  (future
    (try
      (let [status (registry-status)
            summary (into {}
                          (map (fn [[aid info]]
                                 [aid {:status (:status info)
                                       :type (:type info)
                                       :invoke-activity (:invoke-activity info)}]))
                          (:agents status))]
        (ws-invoke/broadcast-frame!
         {"type" "agents_status"
          "agents" summary
          "count" (:count status)}))
      (catch Throwable _ nil))))

(def ^:private bell-file "/tmp/futon-bell.edn")

(defn ring-bell-file!
  "Write a turn-completed event to the bell file as a plist.
   Uses plist syntax so Emacs `read` can parse it directly.
   Emacs watches this file and fires joe/visible-bell on change."
  [agent-id]
  (try
    (let [nonce (rand-int 1000000)
          ts (str (java.time.Instant/now))]
      (spit bell-file
            (str "(:agent-id \"" agent-id "\" :timestamp \"" ts "\" :nonce " nonce ")")))
    (catch Throwable _ nil)))

(defn- completion-bell-contract?
  [agent]
  (let [metadata (:agent/metadata agent)
        contracts (or (:agency/contracts metadata)
                      (get metadata "agency/contracts")
                      {})]
    (true? (or (:bell-on-complete? contracts)
               (get contracts "bell-on-complete?")
               (:bell-on-complete? metadata)
               (get metadata "bell-on-complete?")))))

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
     :session-reset-fn - Optional. Zero-arity fn that clears any backing
                         session continuity (session file, atom, etc.).

   Returns:
     Agent record on success (R1: typed result).
     {:ok false :error SocialError} on failure (R2: duplicate → error, not overwrite)."
  [{:keys [agent-id type invoke-fn capabilities session-id ttl-ms metadata
           session-reset-fn]}]
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
                      :agent/session-reset-fn session-reset-fn
                      :agent/registered-at ts
                      :agent/last-active ts
                      :agent/ttl-ms ttl-ms
                      ;; E-pilot-hop-trigger-wiring: agent-side fields for
                      ;; the bidirectional hop pointer (claude-2 A1 in
                      ;; ~/code/storage/hop-wiring-scratch.md).  Default
                      ;; nil/[] so existing agents are unaffected.
                      :agent/current-peripheral nil
                      :agent/hop-stack []
                      ;; Peripheral-side field (only populated when this
                      ;; record represents a :type :peripheral entry).
                      ;; Bidirectional pointer back to the agent currently
                      ;; inhabiting this peripheral.
                      :agent/current-inhabitant nil
                      :agent/metadata (merge {:agency/contracts {:bell-on-complete? (boolean invoke-fn)}}
                                             (or metadata {}))}
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
               ;; Auto-touch unless the caller supplies :agent/last-active
               ;; explicitly (federation proxies mirror the REMOTE'S value —
               ;; stamping sync time made every proxy reset to idle-0 each cycle).
               (let [updated (merge agent {:agent/last-active (now)} updates)]
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
;; E-pilot-hop-trigger-wiring: bidirectional hop primitives
;; =============================================================================
;;
;; war-machine-pilot ⇄ {street-sweeper, night-shift, ...} transition mechanic.
;; Spec: futon3c/holes/missions/E-pilot-hop-trigger-wiring.md.
;; Co-design: claude-2 A1 in ~/code/storage/hop-wiring-scratch.md.
;;
;; Hop semantics: an AGENT (e.g. claude-1) inhabits a PERIPHERAL (e.g.
;; war-machine-pilot).  A hop transitions the agent's inhabitation to a
;; new peripheral (e.g. street-sweeper) while pushing the prior one onto
;; the agent's hop-stack.  Hop-back pops the stack.
;;
;; The pointer is BIDIRECTIONAL: agent records carry :current-peripheral
;; + :hop-stack; peripheral records carry :current-inhabitant.  Both sides
;; updated atomically in a single swap! so there is no consistency window.
;;
;; Foreign-hop-in rejection (operator-approved hard mode, design choice
;; #2 in the spec): if the destination peripheral's :current-inhabitant
;; is non-nil AND not the requesting agent, the hop is rejected.

(defn- hop-update-agent
  [agent prev-peri new-peri]
  (-> agent
      (assoc :agent/current-peripheral new-peri)
      ;; Only push prev onto stack if there WAS a prev; pushing nil
      ;; would corrupt subsequent hop-back operations.
      (update :agent/hop-stack
              (fn [stack]
                (let [s (or stack [])]
                  (if prev-peri (conj s prev-peri) s))))
      (assoc :agent/last-active (now))))

(defn- hop-back-update-agent
  [agent]
  (let [stack (or (:agent/hop-stack agent) [])
        prev  (peek stack)
        rest  (if (seq stack) (pop stack) [])]
    {:agent (-> agent
                (assoc :agent/current-peripheral prev)
                (assoc :agent/hop-stack rest)
                (assoc :agent/last-active (now)))
     :popped prev}))

(def ^:dynamic *enable-hop-event-emission?*
  "Whether hop! / hop-back! emit :hop-in / :hop-out entries to
   pilot-inhabitations.edn.  Default true (production).  Tests rebind
   to false to avoid polluting the live substrate."
  true)

(defn- emit-hop-event!
  "Lazily call futon3c.agency.hop-events/log-hop-event! to append a
   :hop-in / :hop-out entry to pilot-inhabitations.edn.  Lazy require
   avoids a compile-time cycle.  Errors are swallowed (the registry
   transition has already succeeded; substrate-write is best-effort)."
  [event-kind payload]
  (when *enable-hop-event-emission?*
    (try
      (when-let [f (requiring-resolve 'futon3c.agency.hop-events/log-hop-event!)]
        (f event-kind payload))
      (catch Throwable _ nil))))

(defn hop!
  "Transition AGENT-ID's inhabitation to NEW-PERIPHERAL-ID.

   Bidirectional atomic update of both registry records:
     agent : :current-peripheral <- new; :hop-stack <- conj prev
     new peripheral : :current-inhabitant <- agent
     prev peripheral (if any) : :current-inhabitant <- nil (only if it was the agent)

   Foreign-hop-in rejection: if new peripheral's :current-inhabitant is
   non-nil and != agent, hop is rejected with :error :peripheral-occupied.

   Returns:
     {:ok true :from <prev-peri-id-or-nil> :to <new-peri-id> :agent-id ...}
     {:ok false :error :peripheral-occupied :by <other-agent-id>}
     {:ok false :error :agent-not-registered}
     {:ok false :error :peripheral-not-registered}
     {:ok false :error :hop-to-same-peripheral} (no-op rejected loudly per R4)"
  [agent-id new-peripheral-id]
  (let [aid-val (agent-id-value agent-id)
        peri-val (agent-id-value new-peripheral-id)
        result (atom nil)]
    (swap!
     !registry
     (fn [m]
       (let [agent (get m aid-val)
             new-peri (get m peri-val)]
         (cond
           (nil? agent)
           (do (reset! result {:ok false
                               :error :agent-not-registered
                               :agent-id aid-val})
               m)

           (nil? new-peri)
           (do (reset! result {:ok false
                               :error :peripheral-not-registered
                               :peripheral-id peri-val})
               m)

           (= peri-val (:agent/current-peripheral agent))
           (do (reset! result {:ok false
                               :error :hop-to-same-peripheral
                               :peripheral-id peri-val})
               m)

           (and (some? (:agent/current-inhabitant new-peri))
                (not= aid-val (:agent/current-inhabitant new-peri)))
           (do (reset! result {:ok false
                               :error :peripheral-occupied
                               :peripheral-id peri-val
                               :by (:agent/current-inhabitant new-peri)})
               m)

           :else
           (let [prev-peri (:agent/current-peripheral agent)
                 agent'    (hop-update-agent agent prev-peri peri-val)
                 new-peri' (assoc new-peri :agent/current-inhabitant aid-val)
                 m'        (-> m
                               (assoc aid-val agent')
                               (assoc peri-val new-peri'))
                 ;; Clear prev peripheral's inhabitant only if it was the
                 ;; agent we are hopping (defensive — should always be).
                 m''       (if (and prev-peri (get m' prev-peri))
                             (update m' prev-peri
                                     (fn [p]
                                       (if (= aid-val (:agent/current-inhabitant p))
                                         (assoc p :agent/current-inhabitant nil)
                                         p)))
                             m')]
             (reset! result {:ok true
                             :from prev-peri
                             :to peri-val
                             :agent-id aid-val})
             m'')))))
    (let [r @result]
      (when (:ok r)
        (emit-hop-event! :hop-in
                         {:agent-id (:agent-id r)
                          :from-peri (:from r)
                          :to-peri (:to r)}))
      r)))

(defn hop-back!
  "Pop AGENT-ID's :hop-stack and return inhabitation to the previous
   peripheral.  Single atomic swap! restoring bidirectional pointers.

   Returns:
     {:ok true :from <current-peri> :to <prev-peri-or-nil> :agent-id ...}
     {:ok false :error :hop-stack-empty}
     {:ok false :error :agent-not-registered}"
  [agent-id]
  (let [aid-val (agent-id-value agent-id)
        result (atom nil)]
    (swap!
     !registry
     (fn [m]
       (let [agent (get m aid-val)]
         (cond
           (nil? agent)
           (do (reset! result {:ok false
                               :error :agent-not-registered
                               :agent-id aid-val})
               m)

           (empty? (:agent/hop-stack agent))
           (do (reset! result {:ok false
                               :error :hop-stack-empty
                               :agent-id aid-val})
               m)

           :else
           (let [current-peri (:agent/current-peripheral agent)
                 {:keys [agent popped]} (hop-back-update-agent agent)
                 m'           (assoc m aid-val agent)
                 ;; Clear current peripheral's :current-inhabitant if it
                 ;; was the agent (defensive).
                 m''          (if (and current-peri (get m' current-peri))
                                (update m' current-peri
                                        (fn [p]
                                          (if (= aid-val (:agent/current-inhabitant p))
                                            (assoc p :agent/current-inhabitant nil)
                                            p)))
                                m')
                 ;; Set popped (= new current) peripheral's
                 ;; :current-inhabitant to the agent.
                 m'''         (if (and popped (get m'' popped))
                                (assoc-in m'' [popped :agent/current-inhabitant] aid-val)
                                m'')]
             (reset! result {:ok true
                             :from current-peri
                             :to popped
                             :agent-id aid-val})
             m''')))))
    (let [r @result]
      (when (:ok r)
        (emit-hop-event! :hop-out
                         {:agent-id (:agent-id r)
                          :from-peri (:from r)
                          :to-peri (:to r)}))
      r)))

(defn current-peripheral
  "Return AGENT-ID's currently-inhabited peripheral id, or nil."
  [agent-id]
  (:agent/current-peripheral (get @!registry (agent-id-value agent-id))))

(defn current-inhabitant
  "Return PERIPHERAL-ID's current-inhabitant agent id, or nil."
  [peripheral-id]
  (:agent/current-inhabitant (get @!registry (agent-id-value peripheral-id))))

(defn hop-stack
  "Return AGENT-ID's hop-stack (vector of peripheral ids; top of stack
   is the last-departed peripheral)."
  [agent-id]
  (or (:agent/hop-stack (get @!registry (agent-id-value agent-id))) []))

(defn reset-session!
  "Clear an agent's session-id so the next invoke starts a fresh conversation.
   Useful when a session becomes poisoned (e.g. invalid tool-use in history).

   Returns:
     {:ok true :agent-id aid :old-session-id old-sid} on success.
     {:ok false :error SocialError} if agent not found."
  [agent-id]
  (let [aid-val (agent-id-value agent-id)
        agent (get @!registry aid-val)]
    (if-let [agent agent]
      (let [old-sid (:agent/session-id agent)
            reset-fn (:agent/session-reset-fn agent)
            reset-result
            (if reset-fn
              (try
                (let [result (reset-fn)]
                  (cond
                    (or (nil? result) (true? result)) {:ok true}
                    (and (map? result) (= false (:ok result))) result
                    :else {:ok true}))
                (catch Exception e
                  {:ok false
                   :error (make-social-error
                           :session-reset-failed
                           (.getMessage e)
                           :agent-id aid-val
                           :exception-class (.getName (class e)))}))
              {:ok true})]
        (if (= false (:ok reset-result))
          {:ok false
           :error (or (:error reset-result)
                      (make-social-error
                       :session-reset-failed
                       (str "Session reset failed for " aid-val)
                       :agent-id aid-val))}
          (let [result (atom nil)]
            (swap! !registry
                   (fn [m]
                     (if-let [agent* (get m aid-val)]
                       (do
                         (reset! result {:ok true
                                         :agent-id aid-val
                                         :old-session-id old-sid})
                         (assoc m aid-val
                                (-> agent*
                                    (assoc :agent/session-id nil
                                           :agent/last-active (now))
                                    (dissoc :agent/external-invokes
                                            :agent/external-heartbeat-at))))
                       (do
                         (reset! result
                                 {:ok false
                                  :error (make-social-error
                                          :agent-not-found
                                          (str "Agent not registered: " aid-val)
                                          :agent-id aid-val)})
                         m))))
            @result)))
      {:ok false
       :error (make-social-error
               :agent-not-found
               (str "Agent not registered: " aid-val)
               :agent-id aid-val)})))

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
             _trace (when (not= "false" (System/getProperty "FUTON3C_INVOKE_TRACE"))
                      ;; Step-0 duplicate-delivery instrument (turn-delivery-invariants.md, D1).
                      ;; A doubled bell shows TWO lines: same msg-id+preview, different thread
                      ;; (turn-drainer-* = accept-async queue; conductor/tickle + invoke-executor
                      ;; = the second dispatcher). A clean whistle shows ONE line.
                      ;; Writes to /tmp/invoke-trace.log (println-to-stdout goes to Joe's dev
                      ;; terminal, ungreppable). Silence via (System/setProperty "FUTON3C_INVOKE_TRACE" "false").
                      (let [line (str "[invoke-trace] at=" (now)
                                      " agent=" aid-val
                                      " msg-id=" (some-> (re-find #"(?i)Msg-?ID:\s*(\S+)" (str prompt)) second)
                                      " thread=" (.getName (Thread/currentThread))
                                      " preview=" (pr-str prompt-preview))]
                        (println line)
                        (try (spit "/tmp/invoke-trace.log" (str line "\n") :append true)
                             (catch Throwable _))
                        (flush)))
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
                                 :count (count @!registry)})
                               (broadcast-agents-ws!))
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
                          (project-agents!)
                          ;; Agency bell: write turn-completed to file.
                          ;; Emacs watches this file → joe/visible-bell.
                          (future (ring-bell-file! aid-val)))]
         (mark-invoking!)
         (let [invoke-result
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
                   (let [invoke-meta (not-empty (dissoc result-map :result :session-id :error))
                         final-agent (get @!registry aid-val)]
                     (when (and final-agent
                                (completion-bell-contract? final-agent))
                       (when-let [hook @!on-invoke-complete]
                         (future
                           (try
                             (hook final-agent result-map)
                             (catch Exception _)))))
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
                        :exception-class (.getName (class e)))}))]
           ;; Fire on-idle with outcome — after result is known.
           (fire-on-idle! aid-val invoke-result)
           invoke-result))
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

(defn report-external-invoke!
  "Record or clear externally-driven invoke state for AGENT-ID-VAL.

   SOURCE is a stable surface key such as \"emacs-codex-repl\".
   STATE may include:
   {:status \"invoking\"|\"idle\"|:invoking|:idle
    :session-id string
    :campaign-id string
    :excursion-id string
    :prompt-preview string
    :activity string
    :mission-id string}

   Invoking state is treated as live only while refreshed within
   `external-invoke-fresh-ms`; callers should heartbeat during long runs."
  [agent-id-val source state]
  (let [aid-val (agent-id-value agent-id-val)
        source-key (some-> source str str/trim not-empty)
        now* (now)
        status (let [raw (:status state)]
                 (cond
                   (keyword? raw) raw
                   (string? raw) (keyword (str/lower-case raw))
                   :else nil))
        clear? (or (nil? source-key)
                   (nil? status)
                   (= :idle status))]
    (when source-key
      (swap! !registry
             (fn [m]
               (if-let [agent (get m aid-val)]
                 (let [existing (get-in agent [:agent/external-invokes source-key])
                       next-external
                       (if clear?
                         (let [remaining (dissoc (:agent/external-invokes agent) source-key)]
                           (when (seq remaining) remaining))
                         (assoc (or (:agent/external-invokes agent) {})
                                source-key
                                (cond-> {:source source-key
                                         :status :invoking
                                         :started-at (or (:started-at existing) now*)
                                         :updated-at now*}
                                  (some-> (:session-id state) str str/trim not-empty)
                                  (assoc :session-id (some-> (:session-id state) str str/trim))
                                  (some-> (:prompt-preview state) str str/trim not-empty)
                                  (assoc :prompt-preview (some-> (:prompt-preview state) str str/trim))
                                  (some-> (:activity state) str str/trim not-empty)
                                  (assoc :activity (some-> (:activity state) str str/trim))
                                  (some-> (:campaign-id state) str str/trim not-empty)
                                  (assoc :campaign-id (some-> (:campaign-id state) str str/trim))
                                  (some-> (:mission-id state) str str/trim not-empty)
                                  (assoc :mission-id (some-> (:mission-id state) str str/trim))
                                  (some-> (:excursion-id state) str str/trim not-empty)
                                  (assoc :excursion-id (some-> (:excursion-id state) str str/trim)))))
                       agent* (cond-> (assoc agent :agent/external-heartbeat-at now*)
                                next-external
                                (assoc :agent/external-invokes next-external)
                                (nil? next-external)
                                (dissoc :agent/external-invokes)
                                (and (not clear?)
                                     (some-> (:session-id state) str str/trim not-empty))
                                (assoc :agent/session-id (some-> (:session-id state) str str/trim)))]
                   (assoc m aid-val agent*))
                 m)))
      (bb/project-agents! (registry-status))
      (broadcast-agents-ws!))
    {:ok true
     :agent-id aid-val
     :source source-key
     :status (or status :idle)}))

(defn clear-external-invoke!
  "Clear externally-driven invoke state for AGENT-ID-VAL and SOURCE."
  [agent-id-val source]
  (report-external-invoke! agent-id-val source {:status :idle}))

(defn report-surface-projection!
  "Record or refresh a live agent-facing surface projection.

   SOURCE is a stable surface key such as \"emacs-cursor:editor-main\".
   PROJECTION is a structured map describing the live read/write surface.
   Nil or empty projections are rejected; callers should use
   `clear-surface-projection!` when the surface is no longer active."
  [agent-id-val source projection]
  (let [aid-val (agent-id-value agent-id-val)
        source-key (some-> source str str/trim not-empty)
        now* (now)
        normalized (when (map? projection)
                     (not-empty
                      (cond-> {}
                        (some-> (:surface projection) str str/trim not-empty)
                        (assoc :surface (some-> (:surface projection) str str/trim))
                        (some-> (:peripheral-id projection) name str/trim not-empty)
                        (assoc :peripheral-id (keyword (name (:peripheral-id projection))))
                        (some-> (:editor-id projection) str str/trim not-empty)
                        (assoc :editor-id (some-> (:editor-id projection) str str/trim))
                        (some-> (:mode projection) str str/trim not-empty)
                        (assoc :mode (some-> (:mode projection) str str/trim))
                        (some-> (:buffer-surface projection) map? boolean)
                        (assoc :buffer-surface (:buffer-surface projection))
                        (some-> (:minibuffer-surface projection) map? boolean)
                        (assoc :minibuffer-surface (:minibuffer-surface projection))
                        (some-> (:buffer-summary projection) str str/trim not-empty)
                        (assoc :buffer-summary (some-> (:buffer-summary projection) str str/trim))
                        (some-> (:write-surface projection) str str/trim not-empty)
                        (assoc :write-surface (some-> (:write-surface projection) str str/trim))
                        (some-> (:write-contract projection) str str/trim not-empty)
                        (assoc :write-contract (some-> (:write-contract projection) str str/trim))
                        (some-> (:debug projection) map? boolean)
                        (assoc :debug (:debug projection)))))]
    (when (and source-key normalized)
      (swap! !registry
             (fn [m]
               (if-let [agent (get m aid-val)]
                 (let [existing (get-in agent [:agent/surface-projections source-key])
                       next-projections
                       (assoc (or (:agent/surface-projections agent) {})
                              source-key
                              (merge {:source source-key
                                      :started-at (or (:started-at existing) now*)
                                      :updated-at now*}
                                     normalized))]
                   (assoc m aid-val
                          (assoc agent :agent/surface-projections next-projections)))
                 m)))
      (bb/project-agents! (registry-status))
      (broadcast-agents-ws!))
    {:ok true
     :agent-id aid-val
     :source source-key
     :active? (boolean (and source-key normalized))}))

(defn clear-surface-projection!
  "Clear a live surface projection for AGENT-ID-VAL and SOURCE."
  [agent-id-val source]
  (let [aid-val (agent-id-value agent-id-val)
        source-key (some-> source str str/trim not-empty)]
    (when source-key
      (swap! !registry
             (fn [m]
               (if-let [agent (get m aid-val)]
                 (let [remaining (dissoc (:agent/surface-projections agent) source-key)
                       agent* (cond-> agent
                                true (dissoc :agent/surface-projections)
                                (seq remaining) (assoc :agent/surface-projections remaining))]
                   (assoc m aid-val agent*))
                 m)))
      (bb/project-agents! (registry-status))
      (broadcast-agents-ws!))
    {:ok true
     :agent-id aid-val
     :source source-key}))

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

(defn- surface-projection-live?
  [entry]
  (let [updated-at ^Instant (:updated-at entry)]
    (and (instance? Instant updated-at)
         (<= (- (.toEpochMilli (now))
                (.toEpochMilli updated-at))
             surface-projection-fresh-ms))))

(defn current-surface-projection
  "Return the freshest live surface projection for AGENT-ID-VAL, or nil."
  [agent-id-val]
  (let [aid-val (agent-id-value agent-id-val)
        agent (get @!registry aid-val)]
    (->> (:agent/surface-projections agent)
         vals
         (filter surface-projection-live?)
         (sort-by (fn [entry]
                    (.toEpochMilli ^Instant (:updated-at entry))))
         last)))

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

(defn- external-invoke-live?
  [entry]
  (let [updated-at ^Instant (:updated-at entry)]
    (and (= :invoking (:status entry))
         (instance? Instant updated-at)
         (<= (- (.toEpochMilli (now))
                (.toEpochMilli updated-at))
             external-invoke-fresh-ms))))

(defn- freshest-external-invoke
  [agent]
  (->> (:agent/external-invokes agent)
       vals
       (filter external-invoke-live?)
       (sort-by (fn [entry]
                  (.toEpochMilli ^Instant (:updated-at entry))))
       last))

(defn registry-status
  "Return status of all registered agents."
  []
  (let [registry @!registry
        now* (now)
        codex-session-ids (running-codex-session-ids)
        ws-connected (ws-invoke/connected-agent-ids)
        job-counts-fn (*resolve-invoke-job-counts*)
        invoke-job-counts (if job-counts-fn
                            (try
                              (or (job-counts-fn) {})
                              (catch Throwable _ {}))
                            {})]
    {:agents
     (into {}
           (map (fn [[aid agent]]
                  (let [base-status (or (:agent/status agent) :idle)
                        routing-info (invoke-routing-info aid agent)
                        external-invoke (freshest-external-invoke agent)
                        surface-projection (->> (:agent/surface-projections agent)
                                                vals
                                                (filter surface-projection-live?)
                                                (sort-by (fn [entry]
                                                           (.toEpochMilli ^Instant (:updated-at entry))))
                                                last)
                        last-heartbeat (:agent/external-heartbeat-at agent)
                        recent-heartbeat?
                        (and (instance? Instant last-heartbeat)
                             (<= (- (.toEpochMilli ^Instant now*)
                                    (.toEpochMilli ^Instant last-heartbeat))
                                 external-invoke-fresh-ms))
                        session-id (or (:session-id external-invoke)
                                       (:agent/session-id agent))
                        campaign-id (or (:campaign-id external-invoke)
                                        (get-in agent [:agent/metadata :campaign-id])
                                        (get-in agent [:agent/metadata "campaign-id"]))
                        mission-id (or (:mission-id external-invoke)
                                       (get-in agent [:agent/metadata :mission-id])
                                       (get-in agent [:agent/metadata "mission-id"]))
                        excursion-id (or (:excursion-id external-invoke)
                                         (get-in agent [:agent/metadata :excursion-id])
                                         (get-in agent [:agent/metadata "excursion-id"]))
                        {:keys [queued-jobs running-jobs nonterminal-jobs]}
                        (get invoke-job-counts aid {})
                        external-codex-invoking?
                        (and (= :codex (:agent/type agent))
                             (not= base-status :invoking)
                             (not recent-heartbeat?)
                             (string? session-id)
                             (contains? codex-session-ids session-id))
                        job-running? (pos? (long (or running-jobs 0)))
                        external-invoking? (or (some? external-invoke)
                                               external-codex-invoking?)
                        status (if (or external-invoking? job-running?)
                                 :invoking
                                 base-status)
                        invoke-started-at (or (:agent/invoke-started-at agent)
                                              (:started-at external-invoke)
                                              (when external-codex-invoking?
                                                (or last-heartbeat
                                                    (:agent/last-active agent))))
                        invoke-prompt-preview (or (:agent/invoke-prompt-preview agent)
                                                  (:prompt-preview external-invoke)
                                                  (when external-codex-invoking?
                                                    "[external invoke]"))
                        invoke-activity (or (:agent/invoke-activity agent)
                                            (:activity external-invoke)
                                            (when external-codex-invoking?
                                              "codex exec running (external surface)"))]
                    [aid (cond-> {:type (:agent/type agent)
                                  :id (:agent/id agent)
                                  :session-id session-id
                                  :campaign-id campaign-id
                                  :mission-id mission-id
                                  :excursion-id excursion-id
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
                                  :completion-bell-required? (completion-bell-contract? agent)
                                  :status status}
                           queued-jobs
                           (assoc :queued-jobs queued-jobs)
                           running-jobs
                           (assoc :running-jobs running-jobs)
                           nonterminal-jobs
                           (assoc :nonterminal-jobs nonterminal-jobs)
                           invoke-started-at
                           (assoc :invoke-started-at (str invoke-started-at)
                                  :invoke-prompt-preview invoke-prompt-preview)
                           invoke-activity
                           (assoc :invoke-activity invoke-activity)
                           surface-projection
                           (assoc :surface-projection
                                  (dissoc surface-projection :started-at :updated-at)))]))
                registry))
     :count (count registry)
     :ws-connected ws-connected
     :ws-unregistered (->> ws-connected
                           (remove #(contains? registry %))
                           vec)}))

(defn registered-agents
  "Return list of registered TypedAgentId maps."
  []
  (mapv :agent/id (vals @!registry)))

(defn find-reclaimable-agent
  "Find the lowest-numbered idle, session-less reclaimable agent of TYPE.

   Reclaimable agents are either local auto-registered ghosts, or unreachable
   remote placeholders with no invoke function. The latter covers restart
   recovery when a stale remote `codex-1' placeholder would otherwise force
   local auto-registration to allocate `codex-2'. Returns agent-id string, or
   nil."
  [agent-type]
  (let [prefix (name agent-type)]
    (->> (vals @!registry)
         (filter (fn [agent]
                   (let [aid-val (get-in agent [:agent/id :id/value])
                         meta (:agent/metadata agent)
                         local-auto-ghost? (and (not (:remote? meta))
                                                (not (:proxy? meta))
                                                (:auto-registered? meta))
                         unreachable-remote-placeholder?
                         (and (:remote? meta)
                              (not (:proxy? meta))
                              (nil? (:agent/invoke-fn agent)))]
                     (and (= (:agent/type agent) agent-type)
                          (str/starts-with? (str aid-val) (str prefix "-"))
                          (= (or (:agent/status agent) :idle) :idle)
                          (nil? (:agent/session-id agent))
                          (or local-auto-ghost?
                              unreachable-remote-placeholder?)))))
         (sort-by #(get-in % [:agent/id :id/value]))
         first
         (#(some-> % (get-in [:agent/id :id/value]))))))

(defn shutdown-all!
  "Unregister all agents. Returns count of agents removed."
  []
  (let [n (count @!registry)]
    (reset! !registry {})
    n))

(defn backpack-add!
  "Append a pattern entry to an agent's backpack under :agent/metadata."
  [agent-id pattern-entry]
  (swap! !registry
         update-in [agent-id :agent/metadata :backpack]
         (fn [bp] (vec (conj (or bp []) pattern-entry)))))

(defn backpack-clear!
  "Clear an agent's pattern backpack."
  [agent-id]
  (swap! !registry
         assoc-in [agent-id :agent/metadata :backpack] []))

(defn backpack
  "Read an agent's current pattern backpack."
  [agent-id]
  (get-in @!registry [agent-id :agent/metadata :backpack]))
