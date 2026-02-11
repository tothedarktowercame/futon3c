(ns futon3c.peripheral.deploy
  "Deploy peripheral implementation.

   Constraints: git/deploy commands only.
   Fruit: {:committed? bool, :pushed? bool, :sha string|nil}
   Exit context: {:session-id ...}."
  (:require [futon3c.peripheral.common :as common]
            [futon3c.peripheral.evidence :as evidence]
            [futon3c.peripheral.runner :as runner]
            [futon3c.peripheral.tools :as tools]))

(defn- merge-deploy-state
  [state tool result]
  (let [committed? (or (:committed? state)
                       (= :committed (:status result))
                       (= :commit (:status result))
                       (true? (:committed? result))
                       (and (= :bash-git tool)
                            (string? result)
                            (boolean (re-find #"(?i)commit" result))))
        pushed? (or (:pushed? state)
                    (= :pushed (:status result))
                    (true? (:pushed? result))
                    (and (= :bash-deploy tool)
                         (string? result)
                         (boolean (re-find #"(?i)push|deploy" result))))
        sha (or (:sha result)
                (:commit-sha result)
                (:sha state))]
    (assoc state :committed? committed? :pushed? pushed? :sha sha)))

(defn- dispatch-step
  [spec backend state action]
  (if-let [err (common/validate-action :deploy action)]
    err
    (let [{:keys [tool args]} (common/normalize-action action)
          dispatch-result (tools/dispatch-tool tool args spec backend)]
      (cond
        (common/social-error? dispatch-result)
        dispatch-result

        (not (:ok dispatch-result))
        (runner/runner-error :deploy :tool-execution-failed
                             "Deploy tool execution failed"
                             :tool tool
                             :args args
                             :result dispatch-result)

        :else
        (let [result (:result dispatch-result)
              ev (evidence/make-step-evidence
                  :deploy (:session-id state) (:author state)
                  tool args result (:last-evidence-id state))
              new-state (-> state
                            (assoc :last-evidence-id (:evidence/id ev))
                            (update :steps conj {:tool tool :args args :result result})
                            (merge-deploy-state tool result))
              append-err (common/maybe-append-evidence! new-state ev)]
          (if append-err
            append-err
            {:ok true
             :state new-state
             :result result
             :evidence ev}))))))

(defrecord DeployPeripheral [spec backend]
  runner/PeripheralRunner
  (start [_ context]
    (if-let [err (runner/validate-context :deploy context #{:session-id})]
      err
      (let [sid (:session-id context)
            author (common/resolve-author context)
            ev (evidence/make-start-evidence :deploy sid author)
            state {:session-id sid
                   :author author
                   :last-evidence-id (:evidence/id ev)
                   :steps []
                   :committed? false
                   :pushed? false
                   :sha nil
                   :evidence-store (:evidence-store context)}
            append-err (common/maybe-append-evidence! state ev)]
        (if append-err
          append-err
          {:ok true :state state :evidence ev}))))

  (step [_ state action]
    (dispatch-step spec backend state action))

  (stop [_ state reason]
    (let [fruit {:committed? (:committed? state)
                 :pushed? (:pushed? state)
                 :sha (:sha state)}
          ev (evidence/make-stop-evidence
              :deploy
              (:session-id state)
              (:author state)
              fruit
              reason
              (:last-evidence-id state))
          append-err (common/maybe-append-evidence! state ev)]
      (if append-err
        append-err
        {:ok true
         :context {:session-id (:session-id state)}
         :fruit fruit
         :evidence ev}))))

(defn make-deploy
  "Create a deploy peripheral with injected backend."
  ([] (make-deploy (tools/make-mock-backend)))
  ([backend]
   (->DeployPeripheral (common/load-spec :deploy) backend))
  ([spec backend]
   (->DeployPeripheral spec backend)))
