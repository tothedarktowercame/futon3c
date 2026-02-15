(ns futon3c.runtime.agents
  "Runtime helpers for bringing agents online (Codex/Claude) with futon3c protocols.

   This namespace is intentionally thin. It does not contain routing, mode, or
   peripheral logic; it only helps wire live agent registry state into the
   existing social/transport pipeline."
  (:require [futon3c.agency.registry :as reg]
            [futon3c.peripheral.real-backend :as rb]
            [futon3c.peripheral.registry :as preg]
            [futon3c.social.shapes :as shapes]
            [futon3c.transport.http :as http]
            [futon3c.transport.ws :as ws]))

(def ^:private default-capabilities
  {:claude [:explore :edit :test :coordination/execute]
   :codex  [:edit :test :coordination/execute]
   :mock   [:explore]})

(defonce ^:private !default-evidence-store
  (atom {:entries {} :order []}))

(defonce ^:private !default-discipline-state
  (atom {:psr/by-pattern {}
         :pur/history []
         :pivot/history []
         :par/history []}))

(defn make-default-peripheral-config
  "Build a live peripheral config with a real backend.

   opts:
   - :cwd working directory for tool execution (default: user.dir)
   - :timeout-ms command timeout for shell tools
   - :evidence-store atom used for evidence append/read
   - :discipline-state atom for PSR/PUR/PAR continuity
   - :notions-index-path explicit futon3a notions TSV index path"
  ([]
   (make-default-peripheral-config {}))
  ([{:keys [cwd timeout-ms evidence-store discipline-state notions-index-path]}]
   (let [e-store (or evidence-store !default-evidence-store)
         d-state (or discipline-state !default-discipline-state)
         backend-config (cond-> {:cwd (or cwd (System/getProperty "user.dir"))
                                 :evidence-store e-store
                                 :discipline-state d-state}
                          timeout-ms (assoc :timeout-ms timeout-ms)
                          notions-index-path (assoc :notions-index-path notions-index-path))]
     {:backend (rb/make-real-backend backend-config)
      :peripherals (preg/load-peripherals)
      :evidence-store e-store})))

(defn register-agent!
  "Register an agent in the live unified registry.

   opts:
   - :agent-id (string)
   - :type (:claude | :codex | :mock | :peripheral)
   - :invoke-fn ((fn [prompt session-id] ...))
   - :capabilities (optional vector, defaults by type)
   - :ttl-ms, :metadata (optional passthrough)"
  [{:keys [agent-id type invoke-fn capabilities ttl-ms metadata]}]
  (reg/register-agent!
   {:agent-id {:id/value agent-id :id/type :continuity}
    :type type
    :invoke-fn invoke-fn
    :capabilities (vec (or capabilities (get default-capabilities type [])))
    :ttl-ms ttl-ms
    :metadata metadata}))

(defn register-codex!
  "Convenience wrapper for registering a Codex agent."
  [{:keys [agent-id invoke-fn capabilities ttl-ms metadata]
    :or {agent-id "codex-1"}}]
  (register-agent! {:agent-id agent-id
                    :type :codex
                    :invoke-fn invoke-fn
                    :capabilities capabilities
                    :ttl-ms ttl-ms
                    :metadata metadata}))

(defn register-claude!
  "Convenience wrapper for registering a Claude agent."
  [{:keys [agent-id invoke-fn capabilities ttl-ms metadata]
    :or {agent-id "claude-1"}}]
  (register-agent! {:agent-id agent-id
                    :type :claude
                    :invoke-fn invoke-fn
                    :capabilities capabilities
                    :ttl-ms ttl-ms
                    :metadata metadata}))

(defn registry-snapshot
  "Build an AgentRegistryShape snapshot from the live unified registry.

   This snapshot is the typed constraint input consumed by S-presence/S-dispatch."
  ([] (registry-snapshot {}))
  ([{:keys [peripheral-config]}]
   (let [status (reg/registry-status)
         agents (into {}
                      (map (fn [[id {:keys [capabilities type]}]]
                             [id (cond-> {:capabilities (vec capabilities)}
                                   (some? type) (assoc :type type))]))
                      (:agents status))
         snapshot (cond-> {:agents agents}
                    peripheral-config (assoc :peripheral-config peripheral-config))]
     (if (shapes/valid? shapes/AgentRegistryShape snapshot)
       snapshot
       (throw (ex-info "registry-snapshot does not conform to AgentRegistryShape"
                       {:snapshot snapshot
                        :validation (shapes/validate shapes/AgentRegistryShape snapshot)}))))))

(defn runtime-config
  "Build a transport-ready config map from live registry + patterns.

   opts:
   - :patterns PatternLibrary
   - :peripheral-config optional (passed through into registry snapshot)
   - :enable-peripherals? default true; when false, skip auto peripheral config
   - :cwd/:timeout-ms/:evidence-store/:discipline-state/:notions-index-path
     optional inputs for default peripheral backend wiring"
  [{:keys [patterns peripheral-config enable-peripherals?
           cwd timeout-ms evidence-store discipline-state notions-index-path]
    :or {enable-peripherals? true}}]
  (let [resolved-peripheral-config
        (or peripheral-config
            (when enable-peripherals?
              (make-default-peripheral-config
               {:cwd cwd
                :timeout-ms timeout-ms
                :evidence-store evidence-store
                :discipline-state discipline-state
                :notions-index-path notions-index-path})))
        cfg {:registry (registry-snapshot {:peripheral-config resolved-peripheral-config})
             :patterns patterns}]
    (when-not (shapes/valid? shapes/PatternLibrary (:patterns cfg))
      (throw (ex-info "Invalid PatternLibrary for runtime-config"
                      {:patterns patterns
                       :validation (shapes/validate shapes/PatternLibrary patterns)})))
    cfg))

(defn make-http-handler
  "Create an HTTP handler from live runtime state."
  [opts]
  (http/make-handler (runtime-config opts)))

(defn make-ws-callbacks
  "Create WS callbacks from live runtime state."
  [opts]
  (ws/make-ws-callbacks
   (merge (runtime-config opts)
          (select-keys opts [:send-fn :close-fn :on-connect :on-disconnect :irc-interceptor]))))

(defn make-ws-handler
  "Create an http-kit WS handler from live runtime state."
  [opts]
  (ws/make-ws-handler
   (merge (runtime-config opts)
          (select-keys opts [:send-fn :close-fn :on-connect :on-disconnect :irc-interceptor]))))
