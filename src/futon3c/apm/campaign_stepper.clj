(ns futon3c.apm.campaign-stepper
  "Operator-steppable execution over the same obligations used by campaign batches.

  Inspection is read/checkpoint-only.  A step permit binds one immutable campaign
  certificate, obligation, and complete gate report; it can authorize exactly one
  executor claim."
  (:require [futon3c.apm.campaign-executor :as executor]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.campaign-regulator :as regulator]
            [futon3c.apm.campaign-runner :as runner]))

(def gate-statuses #{:pass :fail})

(defn- gate-report-error [gates]
  (cond
    (not (vector? gates)) :campaign-stepper-gates-not-vector
    (empty? gates) :campaign-stepper-gates-required
    (not-every? map? gates) :campaign-stepper-gate-not-map
    (not-every? keyword? (map :gate/id gates)) :campaign-stepper-gate-id-required
    (not= (count gates) (count (distinct (map :gate/id gates))))
    :campaign-stepper-gate-id-duplicate
    (not-every? gate-statuses (map :gate/status gates))
    :campaign-stepper-gate-status-invalid
    (not-every? map? (map :gate/evidence gates))
    :campaign-stepper-gate-evidence-required))

(defn inspect!
  "Checkpoint and expose the next obligation with its named qualification gates.

  GATE-PROVIDER receives {:certificate ... :obligation ...} and must return a
  complete vector of gate results.  No obligation is claimed or executed."
  [{:keys [gate-provider] :as options}]
  (if-not (fn? gate-provider)
    {:ok false :error/code :campaign-stepper-gate-provider-required}
    (let [checkpoint (runner/checkpoint! options {:checkpoint/stage :inspect})]
      (if-not (:ok checkpoint)
        checkpoint
        (let [certificate (:certificate checkpoint)
              decision (regulator/decide certificate)]
          (if-not (= :dispatch (:decision decision))
            {:ok (= :complete (:decision decision))
             :stepper/status (:decision decision)
             :decision decision :checkpoint checkpoint}
            (let [obligation (:obligation decision)
                  supplied (try
                             {:ok true
                              :gates (gate-provider
                                      {:certificate certificate
                                       :obligation obligation})}
                             (catch Throwable t
                               {:ok false
                                :error/code :campaign-stepper-gate-provider-failed
                                :finding {:message (.getMessage t)}}))]
              (if-not (:ok supplied)
                supplied
                (let [gates (:gates supplied)]
                  (if-let [code (gate-report-error gates)]
                    {:ok false :error/code code :gates gates
                     :checkpoint checkpoint :decision decision}
                    (let [failed (filterv #(= :fail (:gate/status %)) gates)
                          report-body
                          {:report/type :campaign-step-inspection
                           :certificate/id (:certificate/id certificate)
                           :campaign/version (:campaign/version certificate)
                           :ledger/digest (:ledger/digest certificate)
                           :obligation/id (:obligation/id obligation)
                           :gates gates}
                          report (assoc report-body :report/id
                                        (machine/ledger-digest [report-body]))]
                      {:ok (empty? failed)
                       :stepper/status (if (empty? failed) :ready :blocked)
                       :report report :failed-gates failed
                       :obligation obligation :checkpoint checkpoint})))))))))))

(defn issue-permit
  "Bind explicit operator approval to one ready inspection report."
  [{:keys [report issuer issued-at]}]
  (cond
    (not= (:report/id report)
          (machine/ledger-digest [(dissoc report :report/id)]))
    {:ok false :error/code :campaign-stepper-report-invalid}
    (some #(not= :pass (:gate/status %)) (:gates report))
    {:ok false :error/code :campaign-stepper-report-not-ready}
    (not (and (string? issuer) (not-empty issuer)
              (string? issued-at) (not-empty issued-at)))
    {:ok false :error/code :campaign-stepper-permit-identity-time-required}
    :else
    (let [body {:permit/type :campaign-step :permit/version 1
                :report/id (:report/id report)
                :certificate/id (:certificate/id report)
                :obligation/id (:obligation/id report)
                :permit/issuer issuer :permit/issued-at issued-at}]
      {:ok true :permit (assoc body :permit/id
                               (machine/ledger-digest [body]))})))

(defn step!
  "Inspect again and execute exactly one gate-passing, explicitly permitted step."
  [{:keys [permit trusted-permit-id trusted-issuer handlers actor] :as options}]
  (let [inspection (inspect! options)
        report (:report inspection)]
    (cond
      (not (:ok inspection)) inspection
      (not= :ready (:stepper/status inspection)) inspection
      (not= (:permit/id permit)
            (machine/ledger-digest [(dissoc permit :permit/id)]))
      {:ok false :stepper/status :refused
       :error/code :campaign-stepper-permit-invalid :inspection inspection}
      (not= trusted-permit-id (:permit/id permit))
      {:ok false :stepper/status :refused
       :error/code :campaign-stepper-permit-untrusted :inspection inspection}
      (not= trusted-issuer (:permit/issuer permit))
      {:ok false :stepper/status :refused
       :error/code :campaign-stepper-permit-issuer-untrusted :inspection inspection}
      (not (and (= :campaign-step (:permit/type permit))
                (= 1 (:permit/version permit))
                (= (:report/id report) (:report/id permit))
                (= (:certificate/id report) (:certificate/id permit))
                (= (:obligation/id report) (:obligation/id permit))))
      {:ok false :stepper/status :refused
       :error/code :campaign-stepper-permit-stale :inspection inspection}
      :else
      (let [certificate (get-in inspection [:checkpoint :certificate])
            obligation (:obligation inspection)
            executed (executor/execute!
                      {:ledger-path (:ledger-path options)
                       :obligation obligation
                       :current-certificate certificate
                       :handlers handlers :actor actor
                       :at (:generated-at certificate)
                       :claim-context {}})
            after (runner/checkpoint!
                   options {:checkpoint/stage :after-step
                            :obligation/id (:obligation/id obligation)
                            :execution/status (if (:ok executed)
                                                :completed :failed)})]
        (cond
          (not (:ok after))
          {:ok false :error/code :campaign-stepper-post-checkpoint-failed
           :inspection inspection :execution executed :post-checkpoint after}
          (not (:ok executed))
          {:ok false :stepper/status :stop
           :error/code :campaign-stepper-execution-failed
           :inspection inspection :execution executed :post-checkpoint after}
          :else
          {:ok true :stepper/status :advanced :permit/id (:permit/id permit)
           :inspection inspection :execution executed :post-checkpoint after})))))
