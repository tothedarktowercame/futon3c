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
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
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

(def ^:dynamic *resolve-uplink-announce*
  "Best-effort resolver for futon3c.agency.fed-uplink/announce!.
   Returns a 0-arity fn or nil. Kept dynamic to avoid a registry -> uplink
   namespace dependency cycle."
  (fn []
    (when-let [uplink-ns (find-ns 'futon3c.agency.fed-uplink)]
      (ns-resolve uplink-ns 'announce!))))

(def ^:dynamic *resolve-peer-announce*
  "Best-effort resolver for futon3c.agency.federation/announce!.
   Returns a one-argument fn accepting the local agent record, or nil. This is
   separate from the WS-uplink roster announcement: a node may have HTTP peers
   and no uplink, as the Amsterdam worker does."
  (fn []
    (when-let [fed-ns (find-ns 'futon3c.agency.federation)]
      (ns-resolve fed-ns 'announce!))))

(def ^:dynamic *resolve-site-prefix*
  "Best-effort resolver for futon3c.agency.federation/site-prefix.
   Returns a 0-arity fn or nil. Kept dynamic to avoid a registry -> federation
   dependency cycle: federation already requires registry, so registry must not
   require it back. Same idiom as *resolve-uplink-announce* above — and it keeps
   ONE source of truth for what this site's prefix is, rather than a second
   copy of the env read that could drift from federation's."
  (fn []
    (when-let [fed-ns (find-ns 'futon3c.agency.federation)]
      (ns-resolve fed-ns 'site-prefix))))

(defonce ^{:doc "Registry of agents.

   Structure: {agent-id-value -> agent-record}

   Agent record:
   {:agent/id         TypedAgentId
    :agent/type       :claude | :codex | :tickle | :mock | :peripheral
    :agent/invoke-fn  (fn [prompt session-id] -> result-map)
    :agent/delivery-mode :push | :inbox
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

(defn- declares-arity?
  "True when F is a Clojure fn that declares an invoke method of arity N.

   Structural, not exception-driven: probing by catching ArityException from
   the call itself cannot distinguish 'this fn takes fewer args' from 'the
   body threw ArityException', and retrying in the second case dispatches the
   turn twice. Non-AFunction callables (vars, proxies, mocks) return false and
   take the legacy fallback chain."
  [f n]
  (boolean
   (when (instance? clojure.lang.AFunction f)
     (some (fn [^java.lang.reflect.Method m]
             (and (= "invoke" (.getName m))
                  (= (int n) (.getParameterCount m))))
           (.getDeclaredMethods (class f))))))

(defn- make-social-error
  "Create a SocialError map (R4)."
  [code message & {:as context}]
  {:error/component :registry
   :error/code code
   :error/message message
   :error/at (str (Instant/now))
   :error/context (or context {})})

(defn- now [] (Instant/now))

(defn- session-owner
  "Return the id of another agent that already owns SESSION-ID, if any."
  [registry agent-id session-id]
  (when session-id
    (some (fn [[other-id agent]]
            (when (and (not= other-id agent-id)
                       (= session-id (:agent/session-id agent)))
              other-id))
          registry)))

(defn- activity-quiet-ms
  "Milliseconds since AT — how long an invoking lane has been silent.

   This is the number that separates a working lane from a stuck one. Job
   wall-clock age cannot: a bell past the soft cap is expected to keep running
   (README-agency-cap.md), so age alone reads the same for both."
  [at]
  (when at
    (try
      (max 0 (- (System/currentTimeMillis) (.toEpochMilli ^Instant at)))
      (catch Throwable _ nil))))

(defn- invoke-routing-info
  "Compute invoke routing readiness for an agent record.
   Exposes whether invoke will route via local fn, WS bridge, or fail."
  [aid-val agent]
  (let [metadata (:agent/metadata agent)
        stale-proxy? (and (:proxy? metadata)
                          (:federation/stale? metadata))
        inbox? (= :inbox (:agent/delivery-mode agent))
        ;; A pull-only seat is reachable (a bell lands in its inbox) but is
        ;; NEVER invocable — invoke-agent! refuses it with :pull-only-agent.
        ;; Reporting :local here would leave the row claiming a spawn path
        ;; that cannot fire, which is the failure E-bell-clink-adapter exists
        ;; to prevent: a seat that reads as reachable while its inbox is unread.
        local? (and (fn? (:agent/invoke-fn agent))
                    (not stale-proxy?)
                    (not inbox?))
        ws-available? (and (ws-invoke/available? aid-val) (not inbox?))
        route (cond
                stale-proxy? :none
                inbox? :inbox
                local? :local
                ws-available? :ws
                :else :none)
        note (or (:note metadata)
                 (get metadata "note"))
        agent-type (:agent/type agent)
        diagnostic (case route
                     :inbox "pull-only seat — bells are written to its inbox; invoke is refused"
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
    {:delivery-mode (:agent/delivery-mode agent :push)
     :invoke-route route
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
                                       :invoke-started-at (:invoke-started-at info)
                                       :invoke-prompt-preview (:invoke-prompt-preview info)
                                       :invoke-activity (:invoke-activity info)}]))
                          (:agents status))]
        (ws-invoke/broadcast-frame!
         {"type" "agents_status"
          "agents" summary
          "count" (:count status)}))
      (catch Throwable _ nil))))

(defn- announce-uplink-roster!
  []
  (when-let [announce-fn (try
                           (*resolve-uplink-announce*)
                           (catch Throwable _ nil))]
    (future
      (try
        (announce-fn)
        (catch Throwable _ nil)))))

(defn- announce-peer-agent!
  "Push one local agent's current runtime state to configured HTTP peers."
  [agent-id]
  (when-let [agent (get @!registry agent-id)]
    (when-not (get-in agent [:agent/metadata :proxy?])
      (when-let [announce-fn (try
                               (*resolve-peer-announce*)
                               (catch Throwable _ nil))]
        (future
          (try
            (announce-fn agent)
            (catch Throwable _ nil)))))))

(defonce ^:private !agents-status-publish-state
  (atom {:phase :idle}))

(defn publish-agents-status!
  "Publish the current agent status snapshot to local and WS agent HUDs.
   Use this after multi-agent registry updates that do not flow through
   register-agent! or invoke-agent!.

   :announce-uplink? defaults true. Set it false while importing an uplink
   roster: announcing in response to a roster creates an announce/roster echo
   loop on the same federation connection."
  ([] (publish-agents-status! {}))
  ([{:keys [announce-uplink?]
     :or {announce-uplink? true}}]
   (let [status (registry-status)]
     (bb/project-agents! status)
     (broadcast-agents-ws!)
     (when announce-uplink?
       (announce-uplink-roster!))
     {:ok true
      :count (:count status)})))

(defn publish-agents-status-async!
  "Leading-and-trailing, non-blocking variant of `publish-agents-status!`.

   Status projection may contact many Emacs sockets and must not run on a
   transport's ordered receive worker. Requests received during a publication
   coalesce into one trailing publication using the latest options. HUD
   projection is best-effort and does not define federation delivery semantics."
  ([] (publish-agents-status-async! {}))
  ([opts]
   (let [[previous _]
         (swap-vals! !agents-status-publish-state
                     (fn [{:keys [phase] :as state}]
                       (case phase
                         :idle {:phase :running :opts opts}
                         :running {:phase :running-dirty :opts opts}
                         :running-dirty (assoc state :opts opts))))
         scheduled? (= :idle (:phase previous))]
     (when scheduled?
       (future
         (loop [publish-opts opts]
           (try
             (publish-agents-status! publish-opts)
             (catch Throwable t
               (println (str "[registry] async status publication failed: "
                             (.getMessage t)))))
           (let [[completed _]
                 (swap-vals! !agents-status-publish-state
                             (fn [{:keys [phase opts]}]
                               (case phase
                                 :running {:phase :idle}
                                 :running-dirty {:phase :running :opts opts})))]
             (when (= :running-dirty (:phase completed))
               (recur (:opts completed)))))))
     {:ok true :scheduled? scheduled?})))

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

(defn- local-site-alias->bare
  "Area codes (TN-agency-area-codes). This box registers its OWN agents bare, so
   `claude-6` is an INDEXICAL: it names whichever claude-6 is local to the asker.
   Our peers already carry our global name — the oxf peer holds `lon-claude-6`
   bound to lucy's claude-6 session — but the box itself does not answer to it.
   Your own number has an area code even when you never dial it locally.

   Maps `<our-site>-<id>` back to the local `<id>`: lon-claude-6 -> claude-6.

   Returns nil for ANY other prefix, which is the load-bearing half: oxf-claude-2
   must never resolve to a local agent. That would be the AG-2 violation
   federation/remote-homed-agent-id? exists to prevent. We only ever strip OUR
   OWN area code."
  [id-value]
  (when-let [site-prefix-fn (*resolve-site-prefix*)]
    (when-let [site (site-prefix-fn)]
      (let [prefix (str site "-")
            s (str id-value)]
        (when (and (str/starts-with? s prefix)
                   (> (count s) (count prefix)))
          (subs s (count prefix)))))))

(def ^:private agent-personas-resource "agent-personas.edn")

(defonce ^:private !agent-personas (atom {}))

(defn- read-agent-personas
  []
  (let [resource (io/resource agent-personas-resource)]
    (when-not resource
      (throw (ex-info "Agent persona resource not found"
                      {:resource agent-personas-resource})))
    (let [personas (edn/read-string (slurp resource))]
      (when-not (and (map? personas)
                     (every? string? (keys personas))
                     (every? string? (vals personas)))
        (throw (ex-info "Agent persona resource must map names to agent ids"
                        {:resource agent-personas-resource})))
      personas)))

(defn reload-agent-personas!
  "Reload persona-to-agent bindings from resources/agent-personas.edn.

   Reloading changes lookup aliases only. It never adds registry keys or agent
   records. A persona may name an agent that is not currently registered; that
   binding remains configured and simply resolves to nil until the target is
   registered."
  []
  (reset! !agent-personas (read-agent-personas)))

(defn agent-personas
  "Return the currently loaded persona-to-agent-id bindings."
  []
  @!agent-personas)

(defn- init-agent-personas!
  "Load persona bindings at namespace load time, tolerantly.

   `reload-agent-personas!` throws on a missing or malformed resource. That is
   right when an operator asks for a reload and wrong here: this call sits at the
   top level of the registry namespace, and `(require 'futon3c.agency.registry
   :reload)` is the sanctioned hot-load path for the shared JVM. A typo in a
   hand-edited config file whose empty value is legitimate must not be able to
   abort that require and take the registry with it. So: fall back to no personas
   and say so on stdout."
  []
  (try
    (reload-agent-personas!)
    (catch Throwable t
      (reset! !agent-personas {})
      (println "[registry] agent personas unavailable, continuing with none:"
               (.getMessage t)))))

(init-agent-personas!)

(defn- persona-agent
  [reg id]
  (when-let [target-id (get (agent-personas) (str id))]
    (let [record (get reg target-id)]
      (when-not (get-in record [:agent/metadata :proxy?])
        record))))

(defn get-agent
  "Get agent record by typed ID, or nil if not registered.

   Resolves this site's area code as an ALIAS, never a second record: both
   `claude-6` and `lon-claude-6` return the SAME record. Aliasing at LOOKUP
   rather than indexing a second key is deliberate — a second key would double
   the registry count and make `*agents*` misreport the roster, and a second
   RECORD would violate I-1 (one agent = one session = one identity). lucy
   already carries a `lon-claude-1` ghost with a null session-id beside the real
   `claude-1`, which is what dual registration looks like when it goes wrong.

   Aliases are FALLBACKS: a registered bare id wins, then this site's area-code
   alias is consulted, then a configured orchestrator persona. Persona aliases
   resolve only to currently registered, non-proxy agents."
  [typed-id]
  (let [id (agent-id-value typed-id)
        reg @!registry]
    (or (get reg id)
        (when-let [bare (local-site-alias->bare id)]
          (get reg bare))
        (persona-agent reg id))))

(defn addressable-names
  "Every name get-agent will resolve: registered ids, this site's area code for
   each LOCAL agent, and configured personas whose targets are registered local
   agents.

   'Registered' and 'addressable' were the same set until aliases existed, and
   code that conflates them now reports a caller as unreachable while the router
   happily reaches it — mesh_qa's MQ-7 gates on a raw key set while
   transport/http's auto-bellback-caller-registered? gates on get-agent. A QA
   check that contradicts the router is worse than no check. So: one rule, asked
   once, here.

   Proxies are excluded, which is what keeps this honest — oxf-claude-2 is a
   proxy (:proxy? true) and must never acquire OUR area code as lon-oxf-claude-2.
   Already-qualified ids are excluded too, so a lon- name is never re-prefixed."
  []
  (let [reg @!registry
        ids (map str (keys reg))
        site (when-let [f (*resolve-site-prefix*)] (f))
        area-names (when site
                     (keep (fn [id]
                             (let [record (get reg id)]
                               (when (and
                                      (not (get-in record [:agent/metadata :proxy?]))
                                      (nil? (local-site-alias->bare id)))
                                 (str site "-" id))))
                           ids))
        persona-names (keep (fn [persona]
                              (when (persona-agent reg persona)
                                persona))
                            (keys (agent-personas)))]
    (into (set ids) (concat area-names persona-names))))

(defn agent-registered?
  "Check if an agent is registered, by any of its names (see get-agent).

   Goes through get-agent so the area code resolves here too. This is what makes
   a qualified signature routable: auto-bellback gates on
   `(boolean (reg/get-agent caller))` (transport/http.clj
   auto-bellback-caller-registered?), and mesh_qa's MQ-7 says an unregistered
   caller 'cannot auto-bell back' — so before this, signing --from lon-claude-6
   made the sender unaddressable and silently ate the reply."
  [typed-id]
  (some? (get-agent typed-id)))

(defn register-agent!
  "Register an agent with the registry.

   Options:
     :agent-id      - Required. TypedAgentId map.
     :type          - Required. :claude, :codex, :tickle, :mock, or :peripheral.
     :invoke-fn     - Required. Function (fn [prompt session-id] -> result-map).
     :capabilities  - Required. Vector of keyword capabilities.
     :session-id    - Optional. Initial session ID.
     :delivery-mode - Optional. :push (default) or :inbox.
     :ttl-ms        - Optional. Bounded lifecycle in milliseconds (R5).
     :metadata      - Optional. Arbitrary metadata map.
     :session-reset-fn - Optional. Zero-arity fn that clears any backing
                         session continuity (session file, atom, etc.).

   Returns:
     Agent record on success (R1: typed result).
     {:ok false :error SocialError} on failure (R2: duplicate → error, not overwrite)."
  [{:keys [agent-id type invoke-fn capabilities session-id ttl-ms metadata delivery-mode
           session-reset-fn]}]
  (let [aid-val (agent-id-value agent-id)
        delivery-mode (or delivery-mode :push)
        typed-id (if (map? agent-id)
                   agent-id
                   {:id/value (str agent-id) :id/type :continuity})
        ts (now)
        agent-record {:agent/id typed-id
                      :agent/type type
                      :agent/delivery-mode delivery-mode
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
    (if-not (#{:push :inbox} delivery-mode)
      (reset! result
              {:ok false
               :error (make-social-error
                       :invalid-delivery-mode
                       "delivery-mode must be :push or :inbox"
                       :agent-id aid-val
                       :delivery-mode delivery-mode)})
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
               (if-let [owner (session-owner m aid-val session-id)]
                 (do (reset! result
                             {:ok false
                              :error (make-social-error
                                      :session-already-owned
                                      (str "Session already owned by " owner)
                                      :agent-id aid-val
                                      :session-id session-id
                                      :owner-id owner)})
                     m)
                 (do (reset! result agent-record)
                     (assoc m aid-val agent-record)))))))
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
               (let [next-session-id (if (contains? updates :agent/session-id)
                                       (:agent/session-id updates)
                                       (:agent/session-id agent))]
                 (if-let [owner (session-owner m aid-val next-session-id)]
                   (do (reset! result
                               {:ok false
                                :error (make-social-error
                                        :session-already-owned
                                        (str "Session already owned by " owner)
                                        :agent-id aid-val
                                        :session-id next-session-id
                                        :owner-id owner)})
                       m)
                   (let [updated (merge agent {:agent/last-active (now)} updates)]
                     (reset! result updated)
                     (assoc m aid-val updated))))
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
  ([typed-id prompt invoke-options]
   (let [invoke-options (if (map? invoke-options)
                          invoke-options
                          {:timeout-ms invoke-options})
         requested-aid-val (agent-id-value typed-id)
         resolved-agent (get-agent typed-id)
         aid-val (or (some-> resolved-agent :agent/id :id/value str)
                     requested-aid-val)]
     (if-let [agent resolved-agent]
       (if (= :inbox (:agent/delivery-mode agent))
         {:ok false
          :error (make-social-error
                  :pull-only-agent
                  (str "Agent " aid-val
                       " is pull-only (delivery-mode inbox); bells are delivered to its inbox, not by invoke.")
                  :agent-id aid-val
                  :delivery-mode :inbox)}
       (let [invoke-fn (:agent/invoke-fn agent)
             routing-info (invoke-routing-info aid-val agent)
             current-session (:agent/session-id agent)
             timeout-ms (some-> (:timeout-ms invoke-options) long)
             timeout-ms (when (and timeout-ms (pos? timeout-ms)) timeout-ms)
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
                                                             (assoc :invoke-activity (:agent/invoke-activity a))
                                                             (:agent/invoke-activity-at a)
                                                             (assoc :invoke-activity-at
                                                                    (str (:agent/invoke-activity-at a))
                                                                    :invoke-quiet-ms
                                                                    (activity-quiet-ms
                                                                     (:agent/invoke-activity-at a))))])
                                                    @!registry))
                                 :count (count @!registry)})
                               (broadcast-agents-ws!)
                               ;; The operator HUD may be on a federation peer.
                               ;; Local projection alone left that proxy frozen
                               ;; at its boot-time state while this box was
                               ;; actively invoking the lane.
                               (announce-uplink-roster!)
                               (announce-peer-agent! aid-val))
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
                                                    :agent/invoke-activity-at nil
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
                                   ;; Prefer the 3-arity contract so the caller's
                                   ;; deadline reaches the process itself, not just
                                   ;; the layers that give up waiting for it.
                                   (if (declares-arity? invoke-fn 3)
                                     (invoke-fn prompt current-session
                                                (assoc invoke-options :timeout-ms timeout-ms))
                                     (try
                                       (invoke-fn prompt current-session)
                                       (catch clojure.lang.ArityException _
                                         (invoke-fn prompt)))))
                     result-map (if timeout-ms
                                  (let [f (future (call-invoke))
                                        v (deref f timeout-ms ::timeout)]
                                    (if (= v ::timeout)
                                      ;; Detach, do NOT cancel. future-cancel
                                      ;; interrupts this thread but does not kill
                                      ;; the codex child, so the old behaviour left
                                      ;; a live orphan writing files with nobody
                                      ;; listening, and discarded a result that
                                      ;; often landed seconds later. The caller's
                                      ;; deadline ends the caller's WAIT; ending
                                      ;; the WORK is the job supervisor's call
                                      ;; (README-agency-cap.md).
                                      (do
                                        ;; Keep the lane :invoking until the turn
                                        ;; really finishes, so a detached turn is
                                        ;; not double-dispatched, and release it
                                        ;; with the session-id it actually returns.
                                        (future
                                          (let [late (try @f (catch Throwable _ nil))]
                                            (mark-idle! (:session-id late))))
                                        {:error "timeout" :exit-code -1
                                         :timeout-ms timeout-ms :detached? true})
                                      v))
                                  (call-invoke))
                     {:keys [result session-id error]} result-map]
                 (when-not (:detached? result-map)
                   (mark-idle! session-id))
                 (if error
                   {:ok false
                    :error (make-social-error
                            :invoke-error
                            (str error)
                            :agent-id aid-val
                            :timeout-ms (:timeout-ms result-map)
                            ;; :detached? true means the turn is STILL RUNNING —
                            ;; the caller stopped waiting, the work did not stop.
                            ;; Callers must not read this as "no work happened".
                            :detached? (boolean (:detached? result-map)))}
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
           invoke-result)))
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
   Does NOT trigger a blackboard refresh — the ticker handles that every 5s.
   When an invoke stream owns the agent's event sink, the same authoritative
   update is pushed as an `invoke.activity` event. Stream progress therefore
   cannot depend on an adapter also remembering to mirror its private raw event.

   Stamps :agent/invoke-activity-at as well. An activity string with no time on
   it cannot be told apart from a stale one: on 2026-08-03 three codex lanes
   past the soft cap read `invoke-activity \"using bash\"` with `last-active`
   from BEFORE the job started, and were reported wedged. They were working —
   burning CPU on live keepalive'd sockets. The age is what distinguishes
   `using bash (3s ago)` from `using bash (51m ago)`; without it, wall-clock
   age of the job is the only signal left, and that is an SLA number, not
   evidence of stuckness (see README-agency-cap.md).

   An activity report is proof of a live local invoke stream, so if the agent
   has been flipped to :idle mid-turn (reconcile-stale-invoking! after a long
   quiet Bash, or a premature job completion — 2026-08-15, claude-2 rendered
   idle while dispatching packets), restore :agent/status :invoking. Only :idle
   is restored; other states (:restored etc.) are left alone."
  [agent-id-val activity-str]
  (let [activity-at (now)
        updated
        (swap! !registry
               (fn [m]
                 (if-let [a (get m agent-id-val)]
                   (assoc m agent-id-val
                          (cond-> (assoc a :agent/invoke-activity activity-str
                                           :agent/invoke-activity-at activity-at)
                            (= :idle (:agent/status a))
                            (assoc :agent/status :invoking)))
                   m)))]
    (when-let [sink (get-in updated [agent-id-val :agent/invoke-event-sink])]
      (try
        (sink {:type "invoke.activity"
               :agent-id agent-id-val
               :activity activity-str
               :at (str activity-at)})
        (catch Throwable _)))
    updated))

(defn- with-idle-invoke-state
  [agent now*]
  (merge agent
         {:agent/last-active now*
          :agent/status :idle
          :agent/invoke-started-at nil
          :agent/invoke-prompt-preview nil
          :agent/invoke-activity nil
          :agent/invoke-activity-at nil}))

(defn mark-agent-idle!
  "Public idle-reset for use by HTTP-layer try/finally guarantees.
   Resets :agent/status to :idle, clears invoke metadata. Idempotent —
   safe to call when already idle. Only updates if the agent is still
   registered (R5: no resurrect). Returns true if a transition occurred."
  [agent-id-val]
  (let [aid-val (agent-id-value agent-id-val)
        transitioned? (atom false)]
    (swap! !registry
           (fn [m]
             (if-let [agent* (get m aid-val)]
               (do
                 (when (= :invoking (:agent/status agent*))
                   (reset! transitioned? true))
                 (assoc m aid-val
                        (with-idle-invoke-state agent* (now))))
               m)))
    @transitioned?))

(defn- invoke-jobs-running-for-agent?
  "Best-effort check: does the invoke-jobs ledger have a running/queued job
   for AGENT-ID? Uses requiring-resolve to avoid a registry -> transport.http
   dependency cycle. Returns false when the ledger is unavailable."
  [agent-id]
  (try
    (when-let [http-ns (or (find-ns 'futon3c.transport.http)
                           (try (require 'futon3c.transport.http)
                                (find-ns 'futon3c.transport.http)
                                (catch Throwable _)))]
      (when-let [counts-fn (ns-resolve http-ns 'active-invoke-job-counts)]
        (let [counts (counts-fn)
              entry (get counts agent-id)]
          (pos? (long (or (:running-jobs entry)
                          (get entry "running-jobs")
                          0))))))
    (catch Throwable _ false)))

(defn- turn-draining-for-agent?
  "Best-effort check: is AGENT-ID currently draining a turn in the turn-queue?
   Covers the turns the invoke-jobs ledger cannot see — /invoke-stream operator
   turns and buffer-delivered park resumes create no ledger job, so the ledger
   guard alone let reconcile-stale-invoking! flip a live turn to :idle after a
   >120s quiet Bash (2026-08-15, claude-2 mid park-resume turn). The drainer's
   :draining set covers every local turn shape. Uses requiring-resolve to stay
   decoupled from the queue ns; returns false when unavailable."
  [agent-id]
  (try
    (when-let [snapshot-fn (requiring-resolve 'futon3c.agency.turn-queue/snapshot)]
      (contains? (or (:draining (snapshot-fn)) #{}) (str agent-id)))
    (catch Throwable _ false)))

(defn- stale-invoking-without-fresh-activity?
  [agent threshold-ms now-ms]
  (let [started-at (:agent/invoke-started-at agent)
        last-active (:agent/last-active agent)
        activity-at (:agent/invoke-activity-at agent)
        ref-inst (or started-at last-active)
        age-ms (when (instance? Instant ref-inst)
                 (- now-ms (.toEpochMilli ^Instant ref-inst)))
        activity-age-ms (when (instance? Instant activity-at)
                          (- now-ms (.toEpochMilli ^Instant activity-at)))]
    (and (= :invoking (:agent/status agent))
         (not (get-in agent [:agent/metadata :proxy?]))
         (some? age-ms)
         (> age-ms (long threshold-ms))
         (not (and (some? activity-age-ms)
                   (<= activity-age-ms (long threshold-ms)))))))

(defn reconcile-stale-invoking!
  "Periodic repair for local agents whose :agent/status is :invoking, whose
   invoking state and latest activity are older than THRESHOLD-MS (default
   120s), and who have no running job in the local invoke-jobs ledger.

   Federated proxies are never repaired locally: their home site owns their
   status and job ledger. Returns a vector of repaired agent-id strings."
  ([]
   (reconcile-stale-invoking! 120000))
  ([threshold-ms]
   (let [scan-now-ms (.toEpochMilli (now))
         candidate-ids
         (for [[aid agent] @!registry
               :when (stale-invoking-without-fresh-activity?
                      agent threshold-ms scan-now-ms)]
           aid)
         repaired (atom [])]
     (doseq [aid candidate-ids
             :when (not (or (invoke-jobs-running-for-agent? aid)
                            (turn-draining-for-agent? aid)))]
       (let [now* (now)
             now-ms (.toEpochMilli now*)
             [before after]
             (swap-vals! !registry
                         (fn [registry]
                           (if-let [agent (get registry aid)]
                             (if (stale-invoking-without-fresh-activity?
                                  agent threshold-ms now-ms)
                               (assoc registry aid
                                      (with-idle-invoke-state agent now*))
                               registry)
                             registry)))]
         (when (and (= :invoking (get-in before [aid :agent/status]))
                    (= :idle (get-in after [aid :agent/status])))
           (swap! repaired conj aid))))
     @repaired)))

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
                       ;; An external status report IS activity evidence:
                       ;; without this stamp an apparatus that only ever
                       ;; reports externally keeps its registration-time
                       ;; :agent/last-active forever, so the roster renders
                       ;; "idle (Nh ago)" minutes after a completed run
                       ;; (war-machine, 2026-07-25).
                       agent* (cond-> (assoc agent
                                             :agent/external-heartbeat-at now*
                                             :agent/last-active now*)
                                next-external
                                (assoc :agent/external-invokes next-external)
                                (nil? next-external)
                                (dissoc :agent/external-invokes)
                                (and (not clear?)
                                     (some-> (:session-id state) str str/trim not-empty))
                                (assoc :agent/session-id (some-> (:session-id state) str str/trim)))]
                   (assoc m aid-val agent*))
                 m)))
      (publish-agents-status!))
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

(def ^:private codex-session-scan-ttl-ms
  "Cache TTL for running-codex-session-ids. The scan walks the whole process
   table with a native ProcessHandle info call per process; callers (the
   agents ticker, every registry-status, every federation roster import)
   invoke it far more often than external codex sessions appear or vanish.
   Uncached, it made the federation ws on-receive slower than the uplink's
   announce cadence — see make-ws-handler in transport/ws.clj (2026-07-18)."
  5000)

(defonce ^:private !codex-session-scan
  (atom nil))  ;; {:at-ms long, :ids #{sid ...}}

(defn running-codex-session-ids
  "Best-effort detection of local `codex exec --json resume <sid>` processes.
   Returns a set of active session IDs, cached for codex-session-scan-ttl-ms.

   This is used to surface external Codex activity (e.g. emacs codex-repl)
   in the shared *agents* panel even when that invoke did not flow through
   registry/invoke-agent!."
  []
  (let [now-ms (System/currentTimeMillis)
        cached @!codex-session-scan]
    (if (and cached (< (- now-ms (:at-ms cached)) codex-session-scan-ttl-ms))
      (:ids cached)
      (let [ids (try
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
                    #{}))]
        (reset! !codex-session-scan {:at-ms now-ms :ids ids})
        ids))))

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
                        {:keys [queued-jobs running-jobs nonterminal-jobs
                                unconsumed-count oldest-unconsumed-age-ms]}
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
                                              "codex exec running (external surface)"))
                        invoke-activity-at (:agent/invoke-activity-at agent)]
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
                                  :delivery-mode (:delivery-mode routing-info)
                                  :completion-bell-required? (completion-bell-contract? agent)
                                  :status status}
                           queued-jobs
                           (assoc :queued-jobs queued-jobs)
                           running-jobs
                           (assoc :running-jobs running-jobs)
                           nonterminal-jobs
                           (assoc :nonterminal-jobs nonterminal-jobs)
                           (= :inbox (:agent/delivery-mode agent))
                           (assoc :unconsumed-count (long (or unconsumed-count 0))
                                  :oldest-unconsumed-age-ms oldest-unconsumed-age-ms)
                           invoke-started-at
                           (assoc :invoke-started-at (str invoke-started-at)
                                  :invoke-prompt-preview invoke-prompt-preview)
                           invoke-activity
                           (assoc :invoke-activity invoke-activity)
                           invoke-activity-at
                           (assoc :invoke-activity-at (str invoke-activity-at)
                                  :invoke-quiet-ms (activity-quiet-ms invoke-activity-at))
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
                          ;; A pull-only seat is session-less BY DESIGN -- it is
                          ;; not spawnable, which is the whole point of the lane
                          ;; -- so the session-less test misreads it as a ghost.
                          ;; claude-clink-1 was reclaimed out from under a live
                          ;; watcher on 2026-08-26; bells to it then failed
                          ;; agent-not-found silently, which is the exact defect
                          ;; E-bell-clink-adapter exists to remove.
                          (not= :inbox (:agent/delivery-mode agent))
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
