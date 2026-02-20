(ns futon3c.peripheral.chat
  "Chat peripheral — IRC session context container.

   A minimal peripheral (no tools) that tracks an agent's presence in an
   IRC channel. The actual message relay happens at the transport layer
   (relay bridge in irc.clj). This peripheral provides:
   - Session lifecycle evidence (goal/conclusion)
   - Hop-out points to explore/edit/reflect
   - Context container: which channel, session duration

   Fruit: {:channel str :message-count int}
   Exit context: {:session-id str :channel str}"
  (:require [futon3c.peripheral.common :as common]
            [futon3c.peripheral.evidence :as evidence]
            [futon3c.peripheral.runner :as runner]
            [futon3c.peripheral.tools :as tools]))

;; =============================================================================
;; PeripheralRunner
;; =============================================================================

(defrecord ChatPeripheral [spec backend]
  runner/PeripheralRunner

  (start [_ context]
    (if-let [err (runner/validate-context :chat context #{:session-id})]
      err
      (let [sid (:session-id context)
            author (common/resolve-author context)
            channel (or (:channel context) "#futon")
            ev (evidence/make-start-evidence :chat sid author)
            state {:session-id sid
                   :author author
                   :channel channel
                   :message-count 0
                   :last-evidence-id (:evidence/id ev)
                   :evidence-store (:evidence-store context)}
            append-err (common/maybe-append-evidence! state ev)]
        (if append-err
          append-err
          {:ok true :state state :evidence ev}))))

  (step [_ state action]
    ;; Chat has tools=#{} — validate-action rejects all tool actions.
    ;; This is correct: IRC message relay is transport-level, not tool-level.
    (if-let [err (common/validate-action :chat action)]
      err
      (runner/runner-error :chat :no-tools
                           "Chat peripheral has no tools — IRC messages flow via transport relay")))

  (stop [_ state reason]
    (let [fruit {:channel (:channel state)
                 :message-count (:message-count state)}
          ev (evidence/make-stop-evidence
              :chat
              (:session-id state)
              (:author state)
              fruit
              reason
              (:last-evidence-id state))
          append-err (common/maybe-append-evidence! state ev)]
      (if append-err
        append-err
        {:ok true
         :context {:session-id (:session-id state)
                   :channel (:channel state)}
         :fruit fruit
         :evidence ev}))))

;; =============================================================================
;; Factory
;; =============================================================================

(defn make-chat
  "Create a chat peripheral.
   Backend is accepted for interface compatibility but unused (no tools)."
  ([] (make-chat (tools/make-mock-backend)))
  ([backend]
   (->ChatPeripheral (common/load-spec :chat) backend))
  ([spec backend]
   (->ChatPeripheral spec backend)))
