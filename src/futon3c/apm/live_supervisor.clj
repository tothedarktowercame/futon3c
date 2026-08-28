(ns futon3c.apm.live-supervisor
  "Fail-closed one-command supervisor for one complete APM frame."
  (:require [futon3c.apm.phase-status :as phase-status]))

(defn tick!
  "Perform one deterministic supervisor tick.

   Dependencies are explicit so tests can prove ordering. A certified phase is
   advanced and projected before the next tick is parked. An awaiting job parks
   on that exact canonical job id."
  [{:keys [launch-audit-fn inspect-fn drive-phase-fn advance-fn project-fn
           recover-claim-fn park-fn continuation-payload]}]
  (if-not (every? fn? [launch-audit-fn inspect-fn drive-phase-fn advance-fn
                        project-fn park-fn])
    {:ok false :error/code :live-supervisor-provider-missing}
    (let [audit (launch-audit-fn)]
      (if-not (:ok audit)
        {:ok false :error/code :live-supervisor-launch-audit-failed
         :audit audit}
        (let [inspection (inspect-fn)]
          (cond
            (and (= :campaign-obligation-claim-recovery-required
                    (get-in inspection [:decision :reason]))
                 (fn? recover-claim-fn))
            (let [recovered (recover-claim-fn inspection)]
              (if-not (:ok recovered)
                recovered
                (let [projected (project-fn)
                      parked (when (:ok projected)
                               (park-fn {:awaiting []
                                         :payload continuation-payload}))]
                  (if (and (:ok projected) (:ok parked))
                    {:ok true :status :claim-recovered :recovery recovered
                     :projection projected :park parked}
                    {:ok false :error/code :live-supervisor-recovery-publication-failed
                     :projection projected :park parked}))))

            (not (:ok inspection)) inspection
            (= :complete (:stepper/status inspection))
            {:ok true :status :frame-complete :inspection inspection}
            (not= :ready (:stepper/status inspection))
            {:ok false :error/code :live-supervisor-inspection-not-ready
             :inspection inspection}
            :else
            (let [action (get-in inspection [:obligation :obligation/action])
                  driven (drive-phase-fn action)
                  status-class (phase-status/classify (:status driven))]
              (cond
                (not (:ok driven)) driven

                (= :waiting-terminal status-class)
                (let [job-id (or (:job-id driven)
                                 (get-in driven [:state :ticket :job-id])
                                 (get-in driven [:state :active :ticket :job-id]))
                      projected (when (and (string? job-id) (not-empty job-id))
                                  (project-fn))
                      parked (when (and (:ok projected)
                                        (string? job-id) (not-empty job-id))
                               (park-fn {:awaiting [job-id]
                                         :payload continuation-payload}))]
                  (cond
                    (nil? projected)
                    {:ok false :error/code :live-supervisor-job-id-missing
                     :finding driven}
                    (not (:ok projected))
                    {:ok false :error/code :live-supervisor-projection-failed
                     :finding projected}
                    (:ok parked)
                    {:ok true :status :parked :phase (:phase action)
                     :job-id job-id :projection projected :park parked}
                    :else
                    {:ok false :error/code :live-supervisor-park-failed
                     :finding parked}))

                (= :certified status-class)
                (let [advanced (advance-fn (:kind action)
                                           (:certificate driven))]
                  (if-not (:ok advanced)
                    advanced
                    (let [projected (project-fn)]
                      (if-not (:ok projected)
                        {:ok false :error/code :live-supervisor-projection-failed
                         :finding projected}
                        (let [parked (park-fn {:awaiting []
                                              :payload continuation-payload})]
                          (if (:ok parked)
                            {:ok true :status :phase-advanced
                             :phase (:phase action) :advance advanced
                             :projection projected :park parked}
                            {:ok false :error/code :live-supervisor-park-failed
                             :finding parked}))))))

                (= :terminal-evidence-collected status-class)
                (let [projected (project-fn)
                      parked (when (:ok projected)
                               (park-fn {:awaiting []
                                         :payload continuation-payload}))]
                  (cond
                    (not (:ok projected))
                    {:ok false :error/code :live-supervisor-projection-failed
                     :finding projected}
                    (:ok parked)
                    {:ok true :status :terminal-collected
                     :phase (:phase action) :collection (:collection driven)
                     :projection projected :park parked}
                    :else
                    {:ok false :error/code :live-supervisor-park-failed
                     :finding parked}))

                (= :waiting-substrate status-class)
                (let [resume-at-ms (get-in driven
                                           [:substrate/condition :resume-at-ms])
                      projected (project-fn)
                      parked (when (and (:ok projected) (nat-int? resume-at-ms))
                               (park-fn {:awaiting []
                                         :retry/not-before-ms resume-at-ms
                                         :payload continuation-payload}))]
                  (cond
                    (not (nat-int? resume-at-ms))
                    {:ok false
                     :error/code :live-supervisor-substrate-resumption-invalid
                     :finding driven}
                    (not (:ok projected))
                    {:ok false :error/code :live-supervisor-projection-failed
                     :finding projected}
                    (:ok parked)
                    {:ok true :status :awaiting-substrate
                     :phase (:phase action) :projection projected :park parked
                     :substrate/resume-at-ms resume-at-ms}
                    :else
                    {:ok false :error/code :live-supervisor-park-failed
                     :finding parked}))

                (= :waiting-transport-retry status-class)
                (let [projected (project-fn)
                      parked (when (:ok projected)
                               (park-fn {:awaiting []
                                         :retry/not-before-ms
                                         (get-in driven
                                                 [:state
                                                  :transport-retry/not-before-ms])
                                         :payload continuation-payload}))]
                  (cond
                    (not (:ok projected))
                    {:ok false :error/code :live-supervisor-projection-failed
                     :finding projected}
                    (:ok parked)
                    {:ok true :status :transport-retry-scheduled
                     :phase (:phase action) :projection projected :park parked
                     :retry/not-before-ms
                     (get-in driven [:state :transport-retry/not-before-ms])}
                    :else
                    {:ok false :error/code :live-supervisor-park-failed
                     :finding parked}))

                :else
                {:ok false
                 :error/code :live-supervisor-phase-status-vocabulary-incomplete
                 :finding {:status (:status driven)
                           :classification status-class
                           :known-statuses
                           (vec (sort phase-status/known-statuses))
                           :phase-result driven}}))))))))
