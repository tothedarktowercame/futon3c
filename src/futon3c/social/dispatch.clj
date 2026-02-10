(ns futon3c.social.dispatch
  "S-dispatch: route a classified message to an agent and emit a delivery receipt.

   R1 (delivery-receipt): every call returns DispatchReceipt or SocialError.
   R2 (single routing authority): agent lookup and invocation go through the
   unified agent registry (futon3c.agency.registry). No separate routing table.
   R4 (loud failure): never nil, errors are typed and component-scoped."
  (:require [futon3c.social.shapes :as shapes]
            [futon3c.agency.registry :as reg])
  (:import [java.time Instant]))

(defn- now-str []
  (str (Instant/now)))

(defn- social-error
  [code message & {:as context}]
  (cond-> {:error/component :S-dispatch
           :error/code code
           :error/message message
           :error/at (now-str)}
    (seq context) (assoc :error/context context)))

(defn- coerce-prompt
  "Best-effort: turn payload into something invoke-fn can accept."
  [payload]
  (cond
    (string? payload) payload
    (nil? payload) ""
    :else (pr-str payload)))

(defn dispatch
  "Route a classified message to its target agent.
   Returns DispatchReceipt on success, SocialError on failure.
   R1: every message produces a receipt or explicit failure.
   R2: single routing authority — one registry, one dispatch path."
  [classified-message registry]
  (cond
    (not (shapes/valid? shapes/ClassifiedMessage classified-message))
    (social-error :invalid-message
                  "Invalid ClassifiedMessage input"
                  :message classified-message
                  :validation (or (:error (shapes/validate shapes/ClassifiedMessage classified-message)) {}))

    (not (shapes/valid? shapes/AgentRegistryShape registry))
    (social-error :invalid-registry
                  "Invalid registry input"
                  :registry registry
                  :validation (or (:error (shapes/validate shapes/AgentRegistryShape registry)) {}))

    :else
    (let [target (:msg/to classified-message)]
      (cond
        (nil? target)
        (social-error :no-target
                      "Missing :msg/to target"
                      :msg/id (:msg/id classified-message))

        (not (shapes/valid? shapes/TypedAgentId target))
        (social-error :invalid-target
                      "Invalid :msg/to target (expected TypedAgentId)"
                      :target target
                      :msg/id (:msg/id classified-message))

        (not (contains? (:agents registry) (:id/value target)))
        (social-error :agent-not-found
                      "Target agent not present in registry snapshot"
                      :target target
                      :msg/id (:msg/id classified-message))

        :else
        (let [agent-record (reg/get-agent target)]
          (cond
            (nil? agent-record)
            (social-error :agent-not-registered
                          "Target agent not registered in live registry"
                          :target target
                          :msg/id (:msg/id classified-message))

            :else
            (let [resp (reg/invoke-agent! target (coerce-prompt (:msg/payload classified-message)))
                  receipt (when (= true (:ok resp))
                            {:receipt/msg-id (:msg/id classified-message)
                             :receipt/to target
                             :receipt/delivered? true
                             :receipt/at (now-str)
                             :receipt/route "registry/invoke"})]
              (cond
                (= true (:ok resp))
                (if (shapes/valid? shapes/DispatchReceipt receipt)
                  receipt
                  (social-error :invalid-receipt
                                "Internal error: DispatchReceipt did not conform to shape"
                                :receipt receipt
                                :validation (or (:error (shapes/validate shapes/DispatchReceipt receipt)) {})))

                :else
                (social-error :invoke-failed
                              "Agent invocation failed"
                              :target target
                              :msg/id (:msg/id classified-message)
                              :registry-error (:error resp))))))))))
