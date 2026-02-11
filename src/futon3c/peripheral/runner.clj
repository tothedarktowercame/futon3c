(ns futon3c.peripheral.runner
  "PeripheralRunner protocol — lifecycle for peripheral execution.

   Each peripheral is a constrained situation of action (M-peripheral-behavior).
   The runner protocol defines the lifecycle: start → step* → stop.

   Design method (reverse-morphogenesis, A11):
     run the peripheral → observe the fruit → apply ← backwards
     → do the inferred constraints match the spec?

   Peripherals are pure state machines. Side effects (tool execution, evidence
   emission) flow through injected backends and helpers."
  (:require [futon3c.social.shapes :as shapes]))

;; =============================================================================
;; PeripheralRunner protocol
;; =============================================================================

(defprotocol PeripheralRunner
  (start [this context]
    "Initialize peripheral with context from hop.
     context: {:session-id string, ... keys from transfer-context}
     Returns {:ok true :state <initial-state> :evidence EvidenceEntry} | SocialError")
  (step [this state action]
    "Execute one action within the peripheral's constraints.
     action: {:tool :keyword :args [...]}
     Returns {:ok true :state <new-state> :result <tool-result>
              :evidence EvidenceEntry} | SocialError")
  (stop [this state reason]
    "Finalize peripheral, produce exit context.
     reason: string describing why the peripheral is stopping.
     Returns {:ok true :context <for-next-peripheral> :fruit <output>
              :evidence EvidenceEntry} | SocialError"))

;; =============================================================================
;; Helpers for implementations
;; =============================================================================

(defn runner-error
  "Create a SocialError scoped to a peripheral."
  [peripheral-id code message & {:as context}]
  (cond-> {:error/component :peripheral
           :error/code code
           :error/message message
           :error/at (str (java.time.Instant/now))}
    (seq context) (assoc :error/context (assoc context :peripheral-id peripheral-id))))

(defn validate-context
  "Validate that required context keys are present.
   required-keys: set of keywords that must be in context.
   Returns nil on success, SocialError on failure."
  [peripheral-id context required-keys]
  (let [missing (remove #(contains? context %) required-keys)]
    (when (seq missing)
      (runner-error peripheral-id :missing-context
                    (str "Missing required context keys: " (vec missing))
                    :missing (vec missing)
                    :context context))))
