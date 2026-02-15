(ns futon3c.runtime.agents
  "Runtime helpers for bringing agents online (Codex/Claude) with futon3c protocols.

   This namespace is intentionally thin. It does not contain routing, mode, or
   peripheral logic; it only helps wire live agent registry state into the
   existing social/transport pipeline."
  (:require [futon3c.agency.registry :as reg]
            [futon3c.social.shapes :as shapes]
            [futon3c.transport.http :as http]
            [futon3c.transport.ws :as ws]))

(def ^:private default-capabilities
  {:claude [:explore :edit :test :coordination/execute]
   :codex  [:edit :test :coordination/execute]
   :mock   [:explore]})

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
   - :peripheral-config optional (passed through into registry snapshot)"
  [{:keys [patterns peripheral-config]}]
  (let [cfg {:registry (registry-snapshot {:peripheral-config peripheral-config})
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
