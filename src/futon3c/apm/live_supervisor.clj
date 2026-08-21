(ns futon3c.apm.live-supervisor
  "Fail-closed one-command supervisor for one complete APM frame.")

(defn tick!
  "Perform one deterministic supervisor tick.

   Dependencies are explicit so tests can prove ordering. A certified phase is
   advanced and projected before the next tick is parked. An awaiting job parks
   on that exact canonical job id."
  [{:keys [launch-audit-fn inspect-fn drive-phase-fn advance-fn project-fn
           park-fn continuation-payload]}]
  (if-not (every? fn? [launch-audit-fn inspect-fn drive-phase-fn advance-fn
                        project-fn park-fn])
    {:ok false :error/code :live-supervisor-provider-missing}
    (let [audit (launch-audit-fn)]
      (if-not (:ok audit)
        {:ok false :error/code :live-supervisor-launch-audit-failed
         :audit audit}
        (let [inspection (inspect-fn)]
          (cond
            (not (:ok inspection)) inspection
            (= :complete (:stepper/status inspection))
            {:ok true :status :frame-complete :inspection inspection}
            (not= :ready (:stepper/status inspection))
            {:ok false :error/code :live-supervisor-inspection-not-ready
             :inspection inspection}
            :else
            (let [action (get-in inspection [:obligation :obligation/action])
                  driven (drive-phase-fn action)]
              (cond
                (not (:ok driven)) driven

                (= :awaiting-terminal (:status driven))
                (let [job-id (or (:job-id driven)
                                 (get-in driven [:state :ticket :job-id])
                                 (get-in driven [:state :active :ticket :job-id]))
                      parked (when (and (string? job-id) (not-empty job-id))
                               (park-fn {:awaiting [job-id]
                                         :payload continuation-payload}))]
                  (cond
                    (nil? parked)
                    {:ok false :error/code :live-supervisor-job-id-missing
                     :finding driven}
                    (:ok parked)
                    {:ok true :status :parked :phase (:phase action)
                     :job-id job-id :park parked}
                    :else
                    {:ok false :error/code :live-supervisor-park-failed
                     :finding parked}))

                (= :certified (:status driven))
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

                :else
                {:ok false :error/code :live-supervisor-phase-status-invalid
                 :finding driven}))))))))
