(ns futon3c.apm.live-job-driver
  "Exactly-once durable boundary shared by every live APM role job.

   The canonical Agency job is announced, ticketed, and persisted before it is
   activated. A restart therefore polls the recorded job instead of dispatching
   a duplicate. Terminal evidence is delegated to a phase-specific validator."
  (:require [futon3c.apm.campaign-machine :as machine]))

(def terminal-states #{:done :failed :error :cancelled})

(defn ticket [request response]
  (if-not (and (:ok response) (string? (:job-id response))
               (not-empty (:job-id response)))
    {:ok false :error/code :live-job-announce-failed}
    (let [body {:dispatch/id (:dispatch/id request)
                :job-id (:job-id response) :agent-id (:agent-id request)
                :frame-id (:frame-id request) :problem-id (:problem-id request)
                :phase (:phase request)}]
      {:ok true :ticket (assoc body :ticket/id (machine/ledger-digest [body]))})))

(defn drive!
  "Advance one job by at most one externally visible state transition."
  [{:keys [request state announce-fn activate-fn job-fn persist-fn
           terminal-validator receipt-provider terminal-repair-request-fn]}]
  (cond
    (not (and (map? request) (string? (:dispatch/id request))
              (every? fn? [announce-fn activate-fn job-fn persist-fn
                            terminal-validator receipt-provider])))
    {:ok false :error/code :live-job-driver-input-invalid}

    (nil? state)
    (let [announced (ticket request (announce-fn request))]
      (if-not (:ok announced)
        announced
        (let [next-state {:state/type :live-job-dispatched
                          :request request :ticket (:ticket announced)}
              persisted (persist-fn next-state)]
          (cond
            (not (:ok persisted))
            {:ok false :error/code :live-job-ticket-persistence-failed}

            :else
            (let [activated (activate-fn request (:ticket announced))]
              (if (:ok activated)
                (let [accepted-state (assoc next-state :activation/accepted? true)
                      accepted-persisted (persist-fn accepted-state)]
                  (if (:ok accepted-persisted)
                    {:ok true :status :awaiting-terminal :state accepted-state}
                    {:ok false
                     :error/code :live-job-activation-acceptance-persistence-failed
                     :state next-state}))
                {:ok false :error/code :live-job-activation-failed
                 :state next-state :finding activated}))))))

    (not= :live-job-dispatched (:state/type state))
    {:ok false :error/code :live-job-state-invalid}

    (not= (:dispatch/id request) (get-in state [:request :dispatch/id]))
    {:ok false :error/code :live-job-request-state-mismatch}

    (not (:activation/accepted? state))
    (let [job (job-fn (get-in state [:ticket :job-id]))
          observed-accepted? (or (contains? terminal-states (:state job))
                                 (contains? #{:activating :running :overrun}
                                            (:state job)))]
      (if observed-accepted?
        ;; A running or terminal canonical job is stronger durable evidence
        ;; than the lost local 202 observation.  Persist the reconciliation
        ;; before terminal validation; never reinterpret a client timeout or
        ;; an unchanged queued job as acceptance.
        (let [accepted-state (assoc state
                                    :activation/accepted? true
                                    :activation/reconciled-from (:state job))
              persisted (persist-fn accepted-state)]
          (if (:ok persisted)
            {:ok true :status :awaiting-terminal :state accepted-state}
            {:ok false
             :error/code :live-job-activation-acceptance-persistence-failed
             :state state}))
        (let [activated (activate-fn request (:ticket state))]
          (if-not (:ok activated)
            {:ok false :error/code :live-job-activation-failed
             :state state :finding activated}
            (let [accepted-state (assoc state :activation/accepted? true)
                  persisted (persist-fn accepted-state)]
              (if (:ok persisted)
                {:ok true :status :awaiting-terminal :state accepted-state}
                {:ok false
                 :error/code :live-job-activation-acceptance-persistence-failed
                 :state state}))))))

    :else
    (let [active-request (or (:active-request state) request)
          job (job-fn (get-in state [:ticket :job-id]))]
      (cond
        (not (contains? terminal-states (:state job)))
        {:ok true :status :awaiting-terminal :state state}

        (not= :done (:state job))
        {:ok false :error/code :live-job-terminal-failure
         :finding (select-keys job [:job-id :agent-id :state :terminal-code])}

        :else
        (let [validated (terminal-validator active-request (:ticket state) job)]
          (if (:ok validated)
            (let [provided (receipt-provider active-request (:ticket state)
                                             job validated)]
              (if-not (:ok provided)
                provided
                (let [next-state (assoc state :state/type :live-job-certified
                                        :receipt (:certificate provided))]
                  (if (:ok (persist-fn next-state))
                    {:ok true :status :certified :state next-state
                     :certificate (:certificate provided)}
                    {:ok false
                     :error/code :live-job-receipt-persistence-failed}))))
            (cond
              (pos? (or (:terminal-repair-attempts state) 0))
              (assoc validated :error/code :live-job-terminal-repair-exhausted
                     :repair/attempts (:terminal-repair-attempts state))

              (not (fn? terminal-repair-request-fn)) validated

              :else
              (let [repair (terminal-repair-request-fn
                            active-request (:ticket state) job validated)
                    repair-request (:request repair)]
                (if-not (and (:ok repair) (map? repair-request)
                             (string? (:dispatch/id repair-request)))
                  {:ok false
                   :error/code :live-job-terminal-repair-request-invalid
                   :finding repair}
                  (let [announced (ticket repair-request
                                          (announce-fn repair-request))]
                    (if-not (:ok announced)
                      announced
                      (let [next-state
                            (assoc state
                                   :active-request repair-request
                                   :ticket (:ticket announced)
                                   :activation/accepted? false
                                   :terminal-repair-attempts 1
                                   :terminal-repair/original-job-id (:job-id job)
                                   :terminal-repair/findings (:findings validated))]
                        (if-not (:ok (persist-fn next-state))
                          {:ok false
                           :error/code :live-job-terminal-repair-persistence-failed}
                          (let [activated (activate-fn repair-request
                                                       (:ticket announced))]
                            (if-not (:ok activated)
                              {:ok false :error/code :live-job-activation-failed
                               :state next-state :finding activated}
                              (let [accepted (assoc next-state
                                                    :activation/accepted? true)]
                                (if (:ok (persist-fn accepted))
                                  {:ok true :status :awaiting-terminal
                                   :repair? true :state accepted}
                                  {:ok false
                                   :error/code :live-job-activation-acceptance-persistence-failed
                                   :state next-state})))))))))))))))))
