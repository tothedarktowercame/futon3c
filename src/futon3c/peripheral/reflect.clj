(ns futon3c.peripheral.reflect
  "Reflect peripheral implementation.

   Constraints: read + musn-log tools over session-log scope.
   Fruit: {:par EvidenceEntry}
   Exit context: {:session-id ...}."
  (:require [futon3c.peripheral.common :as common]
            [futon3c.peripheral.evidence :as evidence]
            [futon3c.peripheral.runner :as runner]
            [futon3c.peripheral.tools :as tools])
  (:import [java.time Instant]
           [java.util UUID]))

(defn- now-str []
  (str (Instant/now)))

(defn- make-par-entry
  [state reason]
  (let [par-body {:problem (str "Session closed: " reason)
                  :approach (->> (:steps state) (map (comp name :tool)) vec)
                  :result (str "Recorded " (count (:steps state)) " reflective step(s)")}]
    {:evidence/id (str "par-" (UUID/randomUUID))
     :evidence/subject {:ref/type :session :ref/id (:session-id state)}
     :evidence/type :reflection
     :evidence/claim-type :observation
     :evidence/author (:author state)
     :evidence/at (now-str)
     :evidence/body {:peripheral :reflect :par par-body}
     :evidence/tags [:peripheral :reflect :par]
     :evidence/session-id (:session-id state)
     :evidence/in-reply-to (:last-evidence-id state)}))

(defn- dispatch-step
  [spec backend state action]
  (if-let [err (common/validate-action :reflect action)]
    err
    (let [{:keys [tool args]} (common/normalize-action action)
          dispatch-result (tools/dispatch-tool tool args spec backend)]
      (cond
        (common/social-error? dispatch-result)
        dispatch-result

        (not (:ok dispatch-result))
        (runner/runner-error :reflect :tool-execution-failed
                             "Reflect tool execution failed"
                             :tool tool
                             :args args
                             :result dispatch-result)

        :else
        (let [result (:result dispatch-result)
              ev (evidence/make-step-evidence
                  :reflect (:session-id state) (:author state)
                  tool args result (:last-evidence-id state))
              new-state (-> state
                            (assoc :last-evidence-id (:evidence/id ev))
                            (update :steps conj {:tool tool :args args :result result}))
              append-err (common/maybe-append-evidence! new-state ev)]
          (if append-err
            append-err
            {:ok true
             :state new-state
             :result result
             :evidence ev}))))))

(defrecord ReflectPeripheral [spec backend]
  runner/PeripheralRunner
  (start [_ context]
    (if-let [err (runner/validate-context :reflect context #{:session-id})]
      err
      (let [sid (:session-id context)
            author (common/resolve-author context)
            ev (evidence/make-start-evidence :reflect sid author)
            state {:session-id sid
                   :author author
                   :last-evidence-id (:evidence/id ev)
                   :steps []
                   :evidence-store (:evidence-store context)}
            append-err (common/maybe-append-evidence! state ev)]
        (if append-err
          append-err
          {:ok true :state state :evidence ev}))))

  (step [_ state action]
    (dispatch-step spec backend state action))

  (stop [_ state reason]
    (let [par-entry (make-par-entry state reason)
          state-with-par (assoc state :last-evidence-id (:evidence/id par-entry))
          append-par-err (common/maybe-append-evidence! state-with-par par-entry)]
      (if append-par-err
        append-par-err
        (let [fruit {:par par-entry}
              ev (evidence/make-stop-evidence
                  :reflect
                  (:session-id state)
                  (:author state)
                  fruit
                  reason
                  (:evidence/id par-entry))
              append-stop-err (common/maybe-append-evidence! state-with-par ev)]
          (if append-stop-err
            append-stop-err
            {:ok true
             :context {:session-id (:session-id state)}
             :fruit fruit
             :evidence ev}))))))

(defn make-reflect
  "Create a reflect peripheral with injected backend."
  ([] (make-reflect (tools/make-mock-backend)))
  ([backend]
   (->ReflectPeripheral (common/load-spec :reflect) backend))
  ([spec backend]
   (->ReflectPeripheral spec backend)))
