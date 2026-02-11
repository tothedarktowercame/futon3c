(ns futon3c.social.presence
  "S-presence: verify agent presence from a connection event.

   R7 (rendezvous-handshake): transport connection alone is not presence.
   Presence requires both:
   1) agent exists in the registry (constraint input)
   2) connection includes explicit readiness signal (:conn/metadata {:ready true})

   R4 (loud failure): always returns a PresenceRecord or SocialError, never nil."
  (:require [futon3c.social.shapes :as shapes])
  (:import [java.time Instant]))

(defn- now-str []
  (str (Instant/now)))

(defn- social-error
  [code message & {:as context}]
  (cond-> {:error/component :S-presence
           :error/code code
           :error/message message
           :error/at (now-str)}
    (seq context) (assoc :error/context context)))

(defn- registry-agent-exists?
  "Return true if the agent-id is present in the provided registry snapshot
   (AgentRegistryShape). The snapshot is the authoritative constraint input (I3).

   registry-or-nil must be nil or a valid AgentRegistryShape. Any other value
   returns false (verify rejects invalid registries before reaching this point)."
  [registry-or-nil typed-agent-id]
  (cond
    (nil? registry-or-nil)
    false

    (shapes/valid? shapes/AgentRegistryShape registry-or-nil)
    (contains? (:agents registry-or-nil) (:id/value typed-agent-id))

    :else
    false))

(defn verify
  "Verify agent presence from a connection event.
   Returns PresenceRecord on success, SocialError on failure."
  [connection registry-or-nil]
  (cond
    (not (shapes/valid? shapes/AgentConnection connection))
    (social-error :invalid-connection
                  "Invalid AgentConnection input"
                  :input connection
                  :validation (or (:error (shapes/validate shapes/AgentConnection connection)) {}))

    (and (some? registry-or-nil)
         (map? registry-or-nil)
         (not (shapes/valid? shapes/AgentRegistryShape registry-or-nil)))
    (social-error :invalid-registry
                  "Invalid registry input"
                  :registry registry-or-nil
                  :validation (or (:error (shapes/validate shapes/AgentRegistryShape registry-or-nil)) {}))

    :else
    (let [agent-id (:conn/agent-id connection)
          ready? (true? (get-in connection [:conn/metadata :ready]))]
      (cond
        (nil? registry-or-nil)
        (social-error :registry-missing
                      "Registry is required for S-presence verification")

        (not (registry-agent-exists? registry-or-nil agent-id))
        (social-error :agent-not-found
                      "Agent not found in registry"
                      :agent-id agent-id)

        (not ready?)
        (social-error :not-ready
                      "Connection does not include readiness handshake"
                      :agent-id agent-id
                      :conn-id (:conn/id connection)
                      :metadata (or (:conn/metadata connection) {}))

        :else
        (let [presence {:presence/agent-id agent-id
                        :presence/conn-id (:conn/id connection)
                        :presence/ready? true
                        :presence/transport (:conn/transport connection)
                        :presence/at (:conn/at connection)}]
          (if (shapes/valid? shapes/PresenceRecord presence)
            presence
            (social-error :invalid-presence-record
                          "Internal error: PresenceRecord did not conform to shape"
                          :presence presence
                          :validation (or (:error (shapes/validate shapes/PresenceRecord presence)) {}))))))))

