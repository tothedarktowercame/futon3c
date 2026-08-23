(ns futon3c.apm.campaign-executor
  "Claim, execute, and certify one campaign obligation."
  (:require [futon3c.apm.campaign-ledger :as ledger]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.campaign-regulator :as regulator]))

(defn- event-id [event-without-id]
  (machine/ledger-digest [event-without-id]))

(defn- make-event [certificate seq version actor at type body]
  (let [event {:event/seq seq :event/type type
               :event/campaign-id (:campaign/id certificate)
               :event/actor actor :event/at at
               :event/expected-version version :event/body body}]
    (assoc event :event/id (event-id event))))

(defn execute!
  "Execute OBLIGATION against LEDGER-PATH through HANDLERS.

  The obligation is durably claimed before HANDLER runs. HANDLER is selected by
  action :kind and receives the action plus :idempotency-key. It must return
  {:ok true :certificate map}. Handler failure leaves the visible claim in the
  ledger for a separate recovery policy; this function never retries it."
  [{:keys [ledger-path obligation current-certificate handlers actor at
           claim-context]}]
  (let [authorization (regulator/authorize obligation current-certificate)
        action (:obligation/action obligation)
        handler (get handlers (:kind action))]
    (cond
      (not (:ok authorization)) authorization

      (not (true? (:campaign/claims-required? current-certificate)))
      {:ok false :error/code :campaign-executor-claims-required}

      (:active/claim current-certificate)
      {:ok false :error/code :campaign-executor-claim-already-active}

      (not (fn? handler))
      {:ok false :error/code :campaign-obligation-handler-missing
       :kind (:kind action)}

      (not (and (string? actor) (not-empty actor)
                (string? at) (not-empty at)))
      {:ok false :error/code :campaign-executor-identity-time-required}

      (not (or (nil? claim-context)
               (and (map? claim-context)
                    (every? #{:batch/permit-id :batch/action-index :trigger/id}
                            (keys claim-context)))))
      {:ok false :error/code :campaign-executor-claim-context-invalid}

      :else
      (let [version (:campaign/version current-certificate)
            seq (:ledger/event-count current-certificate)
            claim-event (make-event current-certificate seq version actor at
                                    :obligation/claimed
                                    (merge claim-context {:obligation obligation}))
            claim (ledger/compare-and-append!
                   ledger-path version (:ledger/digest current-certificate)
                   claim-event)]
        (if-not (:ok claim)
          {:ok false :error/code :campaign-obligation-claim-refused
           :claim claim}
          (let [handler-result
                (try
                  (handler (assoc action
                                  :idempotency-key (:obligation/id obligation)))
                  (catch Throwable t
                    {:ok false :error/code :campaign-obligation-handler-threw
                     :finding {:message (.getMessage t)}}))]
            (if-not (and (:ok handler-result)
                         (map? (:certificate handler-result)))
              {:ok false
               :error/code (or (:error/code handler-result)
                               :campaign-obligation-certificate-invalid)
               :claim-persisted? true
               :claim-receipt claim
               :handler-result handler-result}
              (let [completion (:completion action)
                    completion-body
                    (assoc (:event/body completion)
                           :obligation/id (:obligation/id obligation)
                           :certificate (:certificate handler-result))
                    completion-event
                    (make-event current-certificate
                                (get-in claim [:after :event-count])
                                (get-in claim [:after :version])
                                actor at (:event/type completion) completion-body)
                    completed
                    (ledger/compare-and-append!
                     ledger-path
                     (get-in claim [:after :version])
                     (get-in claim [:after :digest])
                     completion-event)]
                (if (:ok completed)
                  {:ok true :completed? true
                   :obligation/id (:obligation/id obligation)
                   :claim-receipt claim :completion-receipt completed
                   :effect-certificate (:certificate handler-result)}
                  {:ok false :error/code :campaign-obligation-completion-refused
                   :claim-persisted? true :effect-completed? true
                   :claim-receipt claim :completion completed
                  :effect-certificate (:certificate handler-result)})))))))))

(defn complete-claimed!
  "Complete the exact durable active claim from an independently certified,
   idempotent effect. This never creates or releases a claim."
  [{:keys [ledger-path current-certificate handlers actor at]}]
  (let [claim (:active/claim current-certificate)
        obligation (:obligation claim)
        action (:obligation/action obligation)
        handler (get handlers (:kind action))]
    (cond
      (not (map? claim))
      {:ok false :error/code :campaign-recovery-active-claim-required}
      (not= (:obligation/id claim) (:obligation/id obligation))
      {:ok false :error/code :campaign-recovery-claim-identity-mismatch}
      (= actor (:actor claim))
      {:ok false :error/code :campaign-recovery-independent-actor-required}
      (not (fn? handler))
      {:ok false :error/code :campaign-obligation-handler-missing}
      :else
      (let [handled (handler (assoc action :idempotency-key (:obligation/id claim)))]
        (if-not (and (:ok handled) (map? (:certificate handled)))
          {:ok false :error/code :campaign-recovery-effect-not-certified
           :handler-result handled}
          (let [completion (:completion action)
                body (assoc (:event/body completion)
                            :obligation/id (:obligation/id claim)
                            :certificate (:certificate handled)
                            :recovery? true)
                event (make-event current-certificate
                                  (:ledger/event-count current-certificate)
                                  (:campaign/version current-certificate)
                                  actor at (:event/type completion) body)
                completed (ledger/compare-and-append!
                           ledger-path (:campaign/version current-certificate)
                           (:ledger/digest current-certificate) event)]
            (if (:ok completed)
              {:ok true :recovered? true :completion-receipt completed
               :effect-certificate (:certificate handled)}
              {:ok false :error/code :campaign-recovery-completion-refused
               :completion completed})))))))
