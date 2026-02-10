(ns futon3c.social.authenticate
  "S-authenticate: resolve a verified presence record into a stable identity.

   R6 (identifier-separation): transport IDs (:transport) are not continuity IDs
   (:continuity). S-authenticate performs the boundary conversion:
   transport-namespace agent-id -> continuity-namespace agent-id, then enriches
   the identity with type and capabilities from the registry snapshot.

   R4 (loud failure): always returns AgentIdentity or SocialError (never nil)."
  (:require [futon3c.social.shapes :as shapes])
  (:import [java.time Instant]))

(defn- now-str []
  (str (Instant/now)))

(defn- social-error
  [code message & {:as context}]
  (cond-> {:error/component :S-authenticate
           :error/code code
           :error/message message
           :error/at (now-str)}
    (seq context) (assoc :error/context context)))

(defn- resolve-agent-id
  "Resolve presence-level agent-id to a continuity TypedAgentId (R6)."
  [typed-agent-id]
  (case (:id/type typed-agent-id)
    :continuity typed-agent-id
    :transport  (assoc typed-agent-id :id/type :continuity)
    :protocol   ::unresolvable
    ::unresolvable))

(defn resolve-identity
  "Resolve agent identity from a verified presence record.
   Returns AgentIdentity on success, SocialError on failure."
  [presence-record registry]
  (cond
    (not (shapes/valid? shapes/PresenceRecord presence-record))
    (social-error :invalid-presence
                  "Invalid PresenceRecord input"
                  :input presence-record
                  :validation (or (:error (shapes/validate shapes/PresenceRecord presence-record)) {}))

    (not (shapes/valid? shapes/AgentRegistryShape registry))
    (social-error :invalid-registry
                  "Invalid registry input"
                  :registry registry
                  :validation (or (:error (shapes/validate shapes/AgentRegistryShape registry)) {}))

    :else
    (let [presence-id (:presence/agent-id presence-record)
          resolved-id (resolve-agent-id presence-id)]
      (cond
        (= ::unresolvable resolved-id)
        (social-error :invalid-agent-id
                      "Presence agent-id could not be resolved to continuity namespace"
                      :agent-id presence-id)

        :else
        (let [agent-key (:id/value resolved-id)
              agent (get-in registry [:agents agent-key])]
          (cond
            (nil? agent)
            (social-error :agent-not-found
                          "Agent not found in registry"
                          :agent-id resolved-id)

            (nil? (:type agent))
            (social-error :agent-type-missing
                          "Agent type missing in registry"
                          :agent-id resolved-id
                          :registry-entry agent)

            :else
            (let [identity {:identity/agent-id resolved-id
                            :identity/type (:type agent)
                            :identity/capabilities (vec (:capabilities agent))
                            :identity/at (:presence/at presence-record)}]
              (if (shapes/valid? shapes/AgentIdentity identity)
                identity
                (social-error :invalid-identity
                              "Internal error: AgentIdentity did not conform to shape"
                              :identity identity
                              :validation (or (:error (shapes/validate shapes/AgentIdentity identity)) {}))))))))))

