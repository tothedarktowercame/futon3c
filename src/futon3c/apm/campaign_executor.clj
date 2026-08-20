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
  [{:keys [ledger-path obligation current-certificate handlers actor at]}]
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

      :else
      (let [version (:campaign/version current-certificate)
            seq (:ledger/event-count current-certificate)
            claim-event (make-event current-certificate seq version actor at
                                    :obligation/claimed
                                    {:obligation obligation})
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
