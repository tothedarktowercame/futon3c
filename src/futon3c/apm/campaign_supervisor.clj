(ns futon3c.apm.campaign-supervisor
  "Scheduler-safe re-entry for campaign progression and claim recovery."
  (:require [futon3c.apm.campaign-recovery :as recovery]
            [futon3c.apm.campaign-runner :as runner]))

(defn- assess [assessment-fn context]
  (try
    {:ok true :assessment (assessment-fn context)}
    (catch Throwable t
      {:ok false :error/code :campaign-supervisor-assessment-failed
       :finding {:message (.getMessage t)}})))

(defn tick!
  "Perform one scheduler invocation.

  With no active claim, delegate one normal runner step. With a claim, obtain
  one independent assessment, invoke evidence-gated recovery, then checkpoint
  and project the result. This function never sleeps, polls, or retries."
  [{:keys [assessment-fn recovery-assessor trigger-id] :as options}]
  (let [before (runner/checkpoint! options {:checkpoint/stage :supervisor})]
    (if-not (:ok before)
      before
      (let [certificate (:certificate before)
            claim (:active/claim certificate)]
        (if-not claim
          (runner/step! (assoc options :require-batch-permit? true
                               :require-trigger? true :trigger-id trigger-id))
          (cond
            (not (fn? assessment-fn))
            {:ok false :runner/status :stop
             :error/code :campaign-supervisor-assessment-provider-required
             :checkpoint before :claim-retained? true}

            (not (and (string? recovery-assessor) (not-empty recovery-assessor)))
            {:ok false :runner/status :stop
             :error/code :campaign-supervisor-assessor-required
             :checkpoint before :claim-retained? true}

            :else
            (let [assessment-result
                  (assess assessment-fn
                          {:certificate certificate :claim claim})]
              (if-not (:ok assessment-result)
                (assoc assessment-result :runner/status :stop
                       :checkpoint before :claim-retained? true)
                (let [recovered
                      (recovery/recover!
                       {:ledger-path (:ledger-path options)
                        :current-certificate certificate
                        :assessment (:assessment assessment-result)
                        :assessor recovery-assessor
                        :at (:generated-at certificate)})
                      after (runner/checkpoint!
                             options {:checkpoint/stage :after-recovery
                                      :obligation/id (:obligation/id claim)
                                      :recovery/status
                                      (if (:ok recovered) :applied :refused)})]
                  (cond
                    (not (:ok after))
                    {:ok false :runner/status :stop
                     :error/code :campaign-supervisor-post-checkpoint-failed
                     :recovery recovered :checkpoint before
                     :post-checkpoint after}

                    (not (:ok recovered))
                    {:ok false :runner/status :stop
                     :error/code :campaign-supervisor-recovery-refused
                     :recovery recovered :checkpoint before
                     :post-checkpoint after
                     :claim-retained? true}

                    :else
                    {:ok true :runner/status :recovered
                     :recovery recovered :checkpoint before
                     :post-checkpoint after}))))))))))
