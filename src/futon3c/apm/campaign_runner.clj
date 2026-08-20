(ns futon3c.apm.campaign-runner
  "Bounded, checkpointed execution of validated campaign obligations."
  (:require [futon3c.apm.campaign-batch :as batch]
            [futon3c.apm.campaign-executor :as executor]
            [futon3c.apm.campaign-projection :as projection]
            [futon3c.apm.campaign-regulator :as regulator]
            [futon3c.apm.campaign-snapshot :as snapshot])
  (:import [java.time Instant]))

(defn- invoke [f argument error-code]
  (try
    {:ok true :value (f argument)}
    (catch Throwable t
      {:ok false :error/code error-code
       :finding {:message (.getMessage t)}})))

(defn- invoke-clock [clock]
  (try
    {:ok true :value (clock)}
    (catch Throwable t
      {:ok false :error/code :campaign-runner-clock-failed
       :finding {:message (.getMessage t)}})))

(defn checkpoint!
  "Observe, certify, persist, and project the current ledger state.

  OBSERVATION-FN receives CONTEXT. NOW-FN is called exactly once. Publication
  succeeds before the returned certificate may authorize an action."
  [{:keys [ledger-path observation-fn now-fn max-age-ms
           certificate-directory projection-directory project-fn]}
   context]
  (cond
    (not (fn? observation-fn))
    {:ok false :error/code :campaign-runner-observation-provider-required}

    (not (fn? now-fn))
    {:ok false :error/code :campaign-runner-clock-required}

    :else
    (let [clock (invoke-clock now-fn)]
      (if-not (:ok clock)
        clock
        (let [now (:value clock)]
          (if-not (instance? Instant now)
            {:ok false :error/code :campaign-runner-clock-invalid}
            (let [observed (invoke observation-fn context
                                   :campaign-runner-observation-provider-failed)]
              (if-not (:ok observed)
                observed
                (let [snap (snapshot/snapshot
                            {:ledger-path ledger-path
                             :observation (:value observed)
                             :now now :max-age-ms (or max-age-ms 60000)})]
                  (if-not (:ok snap)
                    {:ok false :error/code :campaign-runner-snapshot-failed
                     :snapshot snap}
                    (let [certificate (:certificate snap)
                          persisted (snapshot/persist! certificate-directory certificate)]
                      (if-not (:ok persisted)
                        {:ok false :error/code :campaign-runner-certificate-persist-failed
                         :certificate certificate :persistence persisted}
                        (let [published (projection/project!
                                         projection-directory (:path persisted) project-fn)]
                          (if-not (:ok published)
                            {:ok false :error/code :campaign-runner-projection-failed
                             :certificate certificate :persistence persisted
                             :projection published}
                            {:ok true :certificate certificate
                             :persistence persisted :projection published}))))))))))))))

(defn step!
  "Run at most one obligation, with authoritative checkpoints before and after.

  A failed effect is still followed by a checkpoint, making its durable claim
  visible to the projection and recovery policy."
  [{:keys [handlers actor require-batch-permit? batch-permit
           trusted-permit-id trusted-permit-issuer batch-action-index] :as options}]
  (let [before (checkpoint! options {:checkpoint/stage :before})]
    (if-not (:ok before)
      before
      (let [certificate (:certificate before)
            decision (regulator/decide certificate)]
        (if-not (= :dispatch (:decision decision))
          {:ok (= :complete (:decision decision))
           :runner/status (:decision decision)
           :decision decision :checkpoint before}
          (let [obligation (:obligation decision)
                permit-authorization
                (when require-batch-permit?
                  (batch/authorize
                   {:permit batch-permit :trusted-permit-id trusted-permit-id
                    :trusted-issuer trusted-permit-issuer
                    :actor actor :certificate certificate
                    :obligation obligation :action-index batch-action-index}))]
            (if (and require-batch-permit? (not (:ok permit-authorization)))
              {:ok false :runner/status :stop
               :error/code :campaign-runner-batch-permit-refused
               :decision decision :authorization permit-authorization
               :checkpoint before}
              (let [executed (executor/execute!
                          {:ledger-path (:ledger-path options)
                           :obligation obligation
                           :current-certificate certificate
                           :handlers handlers :actor actor
                           :at (:generated-at certificate)})
                after (checkpoint!
                       options {:checkpoint/stage :after
                                :obligation/id (:obligation/id obligation)
                                :execution/status (if (:ok executed) :completed :failed)})]
            (cond
              (not (:ok after))
              {:ok false :error/code :campaign-runner-post-checkpoint-failed
               :decision decision :execution executed :checkpoint before
               :post-checkpoint after}

              (not (:ok executed))
              {:ok false :error/code :campaign-runner-execution-failed
               :runner/status :stop :decision decision :execution executed
               :checkpoint before :post-checkpoint after}

              :else
              {:ok true :runner/status :advanced :decision decision
               :execution executed :checkpoint before :post-checkpoint after})))))))))

(defn run-batch!
  "Run no more than MAX-ACTIONS obligations. Every return includes a checkpoint.

  The function stops on completion, refusal, recovery-required state, or the
  explicit action bound; it never sleeps, polls, or retries."
  [{:keys [max-actions] :as options}]
  (if-not (and (integer? max-actions) (pos? max-actions))
    {:ok false :error/code :campaign-runner-action-bound-required}
    (loop [completed 0]
      (let [result (step! (assoc options
                                 :require-batch-permit? true
                                 :batch-action-index completed))]
        (cond
          (= :stop (:runner/status result))
          (assoc result :ok false :batch/status :stopped
                 :batch/completed-actions completed)

          (not (:ok result))
          (assoc result :batch/completed-actions completed)

          (= :complete (:runner/status result))
          (assoc result :batch/status :complete
                 :batch/completed-actions completed)

          (= max-actions (inc completed))
          {:ok true :runner/status :paused :batch/status :action-bound
           :batch/completed-actions (inc completed)
           :checkpoint (:post-checkpoint result) :last-step result}

          :else
          (recur (inc completed)))))))
