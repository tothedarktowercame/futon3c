(ns futon3c.social.dispatch
  "S-dispatch: route a classified message to an agent and emit a delivery receipt.

   R1 (delivery-receipt): every call returns DispatchReceipt or SocialError.
   R2 (single routing authority): agent lookup and invocation go through the
   unified agent registry (futon3c.agency.registry). No separate routing table.
   R4 (loud failure): never nil, errors are typed and component-scoped."
  (:require [futon3c.social.shapes :as shapes]
            [futon3c.agency.registry :as reg]
            [futon3c.peripheral.registry :as preg]
            [futon3c.evidence.store :as estore]
            [futon3c.social.persist :as persist])
  (:import [java.time Instant]
           [java.util UUID]))

(defn- now-str []
  (str (Instant/now)))

;; =============================================================================
;; Route selection — pure functions for peripheral bridge (Part I)
;; =============================================================================

(def ^:private default-peripheral
  "Default peripheral per agent type.
   :claude → :explore (understand before changing)
   :codex  → :edit (receives specific instructions)
   All others → :explore (safe default)"
  {:claude :explore
   :codex  :edit})

(def ^:private valid-peripheral-ids
  "Set of valid PeripheralId values."
  #{:explore :edit :test :deploy :reflect :proof :chat})

(defn select-peripheral
  "Given agent type and message payload, choose starting peripheral.
   Returns PeripheralId (:explore, :edit, etc.).

   Default mapping:
     :claude → :explore (understand before changing)
     :codex  → :edit (receives specific instructions)
     :mock   → :explore (safe default)

   Override via {:peripheral :edit} in payload."
  [agent-type payload]
  (let [override (when (map? payload) (:peripheral payload))]
    (if (and override (valid-peripheral-ids override))
      override
      (get default-peripheral agent-type :explore))))

(defn select-route
  "Given a ClassifiedMessage and agent record from the registry, determine routing.
   Returns {:route :direct} for coordination messages,
   or {:route :peripheral :peripheral-id <id>} for action messages.

   Uses :msg/mode to decide. Action messages enter peripheral sessions.
   Coordination messages are invoked directly (existing behavior)."
  [classified-message agent-record]
  (if (= :action (:msg/mode classified-message))
    {:route :peripheral
     :peripheral-id (select-peripheral (:agent/type agent-record)
                                       (:msg/payload classified-message))}
    {:route :direct}))

(defn- social-error
  [code message & {:as context}]
  (cond-> {:error/component :S-dispatch
           :error/code code
           :error/message message
           :error/at (now-str)}
    (seq context) (assoc :error/context context)))

(defn- coerce-prompt
  "Normalize payload for invoke-fn. Passes structured data through;
   only converts nil to empty string. No lossy pr-str conversion."
  [payload]
  (if (nil? payload) "" payload))

;; =============================================================================
;; Peripheral dispatch — action-mode messages through peripheral system
;; =============================================================================

(defn- emit-dispatch-evidence
  "Emit the root evidence entry for a peripheral dispatch.
   This entry becomes the in-reply-to target for the peripheral's start evidence."
  [evidence-store session-id msg-id peripheral-id agent-id]
  (when evidence-store
    (let [entry {:evidence/id (str "e-" (UUID/randomUUID))
                 :evidence/type :coordination
                 :evidence/claim-type :goal
                 :evidence/subject {:ref/type :session :ref/id session-id}
                 :evidence/body {:event :dispatch
                                 :msg/id msg-id
                                 :peripheral-id peripheral-id
                                 :session-id session-id}
                 :evidence/tags [:dispatch :session-start]
                 :evidence/session-id session-id
                 :evidence/author (if (map? agent-id) (:id/value agent-id) (str agent-id))
                 :evidence/at (now-str)}
          result (estore/append* evidence-store entry)]
      (when (:ok result)
        (:evidence/id entry)))))

(defn- peripheral-dispatch
  "Dispatch an action-mode message through the peripheral system.
   Returns DispatchReceipt (with :receipt/session-id, :receipt/peripheral-id,
   :receipt/fruit) or SocialError."
  [classified-message agent-entry peripheral-id config]
  (let [session-id (str "sess-" (UUID/randomUUID))
        msg-id (:msg/id classified-message)
        agent-id (:msg/from classified-message)
        {:keys [backend peripherals evidence-store]} config]
    ;; Emit root evidence entry
    (emit-dispatch-evidence evidence-store session-id msg-id peripheral-id agent-id)
    ;; Build context and run chain
    (let [context (cond-> {:session-id session-id}
                    evidence-store (assoc :evidence-store evidence-store))
          steps [{:peripheral-id peripheral-id
                  :actions []
                  :stop-reason "dispatch-initiated"
                  :exit-condition :session-close}]
          result (try
                   (preg/run-chain {:backend backend
                                    :peripherals peripherals
                                    :evidence-store evidence-store}
                                   context
                                   steps)
                   (catch Exception e
                     {:ok false :error (social-error :peripheral-failed
                                                     (str "Peripheral execution failed: " (.getMessage e)))}))]
      (if (:ok result)
        (let [fruit (first (:fruits result))
              receipt {:receipt/msg-id msg-id
                       :receipt/to (:msg/to classified-message)
                       :receipt/delivered? true
                       :receipt/at (now-str)
                       :receipt/route "peripheral/run-chain"
                       :receipt/session-id session-id
                       :receipt/peripheral-id peripheral-id
                       :receipt/fruit fruit}]
          ;; Persist session
          (persist/persist-session! receipt {:session/id session-id})
          ;; Update session with fruit if present
          (when fruit
            (persist/update-session! session-id {:peripheral-fruit fruit
                                                 :peripheral-id peripheral-id}))
          (if (shapes/valid? shapes/DispatchReceipt receipt)
            receipt
            (social-error :invalid-receipt
                          "Internal error: DispatchReceipt did not conform to shape"
                          :receipt receipt
                          :validation (or (:error (shapes/validate shapes/DispatchReceipt receipt)) {}))))
        (if-let [err (:error result)]
          (if (shapes/valid? shapes/SocialError err)
            err
            (social-error :peripheral-failed
                          "Peripheral chain failed"
                          :chain-error err))
          (social-error :peripheral-failed
                        "Peripheral chain returned failure"))))))

(defn- direct-invoke
  "Existing direct invocation path — coordination messages."
  [classified-message target]
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
                    :registry-error (:error resp)))))

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
        (let [agent-entry (get (:agents registry) (:id/value target))
              route (select-route classified-message
                                  {:agent/type (:type agent-entry)})]
          (if (= :peripheral (:route route))
            (if-let [config (:peripheral-config registry)]
              (peripheral-dispatch classified-message agent-entry
                                   (:peripheral-id route) config)
              ;; No peripheral config → fall back to direct invoke
              (direct-invoke classified-message target))
            (direct-invoke classified-message target)))))))
