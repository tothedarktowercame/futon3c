(ns futon3c.apm.live-job-driver
  "Exactly-once durable boundary shared by every live APM role job.

   The canonical Agency job is announced, ticketed, and persisted before it is
   activated. A restart therefore polls the recorded job instead of dispatching
   a duplicate. Terminal evidence is delegated to a phase-specific validator."
  (:require [futon3c.apm.campaign-machine :as machine]))

(def terminal-states #{:done :failed :error :cancelled})
(def default-terminal-budget {:collection-attempts 1 :repair-attempts 1})

(def durable-reference-keys
  #{:job-id :solver/prior-job-id :repair/of-job-id :submission/id :receipt/id
    :prior-receipt-id :terminal-job-id})

(defn durable-references
  "Return durable job/submission/receipt references in stable path order."
  [state]
  (letfn [(walk [path value]
            (cond
              (map? value)
              (mapcat (fn [[k v]]
                        (let [p (conj path k)]
                          (if (and (contains? durable-reference-keys k)
                                   (string? v) (not-empty v))
                            [{:path p :key k :id v}]
                            (walk p v))))
                      (sort-by (comp pr-str key) value))
              (vector? value)
              (mapcat (fn [[i v]] (walk (conj path i) v))
                      (map-indexed vector value))
              (sequential? value)
              (mapcat (fn [[i v]] (walk (conj path i) v))
                      (map-indexed vector value))
              :else []))]
    (vec (walk [] state))))

(defn scan-durable-references
  "Synchronously resolve every durable attempt/job/submission/receipt reference.

   RESOLVE-FN receives one reference map. A readable target is `{:ok true
   :value ...}`. Returns the first missing/corrupt reference, or `{:ok true}`."
  [state resolve-fn]
  (if-not (and (map? state) (fn? resolve-fn))
    {:ok false :error/code :durable-reference-scan-input-invalid}
    (loop [[reference & more] (durable-references state)]
      (if-not reference
        {:ok true}
        (let [resolved (try
                         (resolve-fn reference)
                         (catch Exception e
                           {:ok false :error/code :durable-reference-corrupt
                            :exception/class (.getName (class e))
                            :exception/message (.getMessage e)}))]
          (if (and (:ok resolved) (some? (:value resolved)))
            (recur more)
            {:ok false
             :error/code (or (:error/code resolved)
                             :durable-reference-missing)
             :reference reference
             :finding (dissoc resolved :value)}))))))

(declare ticket)

(defn- supersede-unaccepted!
  [{:keys [active-request state announce-fn activate-fn persist-fn cancel-fn
           ticket-register-fn cancellation-observation]}]
  (let [old-ticket (:ticket state)
        cancelled (or cancellation-observation
                      (cancel-fn (:job-id old-ticket)))]
    (if-not (:ok cancelled)
      {:ok false :error/code :live-job-unaccepted-cancellation-failed
       :state state :finding cancelled}
      (let [announced (ticket active-request (announce-fn active-request))]
        (cond
          (not (:ok announced)) announced
          (= (:job-id old-ticket) (get-in announced [:ticket :job-id]))
          {:ok false :error/code :live-job-supersession-identity-reused}
          :else
          (let [next-state (-> state
                               (assoc :ticket (:ticket announced)
                                      :activation/accepted? false
                                      :activation/failure nil
                                      :activation-supersession-attempts 1)
                               (update :superseded-tickets (fnil conj [])
                                       (assoc old-ticket
                                              :cancellation cancelled)))]
            (if-not (:ok (persist-fn next-state))
              {:ok false :error/code :live-job-supersession-persistence-failed}
              (let [registered (if (fn? ticket-register-fn)
                                 (ticket-register-fn active-request
                                                     (:ticket announced))
                                 {:ok true})
                    activated (when (:ok registered)
                                (activate-fn active-request (:ticket announced)))]
                (cond
                  (not (:ok registered))
                  {:ok false
                   :error/code :live-job-submission-authority-registration-failed
                   :state next-state :finding registered}
                  (not (:ok activated))
                  {:ok false :error/code :live-job-activation-failed
                   :state next-state :finding activated}
                  :else
                  (let [accepted (assoc next-state :activation/accepted? true)]
                    (if (:ok (persist-fn accepted))
                      {:ok true :status :awaiting-terminal
                       :supersession? true :state accepted}
                      {:ok false
                       :error/code :live-job-activation-acceptance-persistence-failed
                       :state next-state})))))))))))

(defn ticket [request response]
  (if-not (and (:ok response) (string? (:job-id response))
               (not-empty (:job-id response)))
    {:ok false :error/code :live-job-announce-failed}
    (let [body {:dispatch/id (:dispatch/id request)
                :job-id (:job-id response) :agent-id (:agent-id request)
                :frame-id (:frame-id request) :problem-id (:problem-id request)
                :phase (:phase request)}]
      {:ok true :ticket (assoc body :ticket/id (machine/ledger-digest [body]))})))

(defn- terminal-budget [configured]
  (merge default-terminal-budget configured))

(defn- valid-terminal-budget? [configured]
  (let [{:keys [collection-attempts repair-attempts]} (terminal-budget configured)]
    (and (pos-int? collection-attempts) (pos-int? repair-attempts))))

(defn terminal-collection-record [request ticket job submission attempt]
  (let [body {:collection/type :typed-role-terminal
              :dispatch/id (:dispatch/id request)
              :job-id (:job-id ticket)
              :role (:role request)
              :terminal-state (:state job)
              :terminal-code (:terminal-code job)
              :attempt attempt
              :submission/available? (some? submission)
              :submission/id (:submission/id submission)}]
    (assoc body :collection/id (machine/ledger-digest [body]))))

(defn drive!
  "Advance one job by at most one externally visible state transition."
  [{:keys [request state announce-fn activate-fn job-fn persist-fn
           terminal-validator receipt-provider terminal-repair-request-fn
           ticket-register-fn terminal-submission-provider cancel-fn
           missing-observation-provider terminal-budget-config]}]
  (cond
    (not (and (map? request) (string? (:dispatch/id request))
              (every? fn? [announce-fn activate-fn job-fn persist-fn
                            terminal-validator receipt-provider])
              (valid-terminal-budget? terminal-budget-config)))
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
            (let [registered (if (fn? ticket-register-fn)
                               (ticket-register-fn request (:ticket announced))
                               {:ok true})
                  activated (when (:ok registered)
                              (activate-fn request (:ticket announced)))]
              (if-not (:ok registered)
                {:ok false :error/code :live-job-submission-authority-registration-failed
                 :finding registered :state next-state}
              (if (:ok activated)
                (let [accepted-state (assoc next-state :activation/accepted? true)
                      accepted-persisted (persist-fn accepted-state)]
                  (if (:ok accepted-persisted)
                    {:ok true :status :awaiting-terminal :state accepted-state}
                    {:ok false
                     :error/code :live-job-activation-acceptance-persistence-failed
                     :state next-state}))
                {:ok false :error/code :live-job-activation-failed
                 :state next-state :finding activated})))))))

    (not= :live-job-dispatched (:state/type state))
    {:ok false :error/code :live-job-state-invalid}

    (not= (:dispatch/id request) (get-in state [:request :dispatch/id]))
    {:ok false :error/code :live-job-request-state-mismatch}

    (not (:activation/accepted? state))
    (let [job (job-fn (get-in state [:ticket :job-id]))
          observed-accepted? (or (contains? terminal-states (:state job))
                                 (contains? #{:activating :running :overrun}
                                            (:state job)))
          unaccepted-state? (and (not (:activation/accepted? state))
                                 (contains? #{:queued :cancelled} (:state job)))
          supersession-eligible?
          (and unaccepted-state? (fn? cancel-fn)
               (fn? terminal-submission-provider)
               (zero? (or (:activation-supersession-attempts state) 0))
               (or (:activation/failure state)
                   (pos? (or (:typed-submission-migration-attempts state) 0))))]
      (cond
        supersession-eligible?
        (supersede-unaccepted!
         {:active-request (or (:active-request state) request)
          :state state :announce-fn announce-fn :activate-fn activate-fn
          :persist-fn persist-fn :cancel-fn cancel-fn
          :ticket-register-fn ticket-register-fn
          :cancellation-observation
          (when (= :cancelled (:state job))
            {:ok true :state :cancelled
             :job-id (get-in state [:ticket :job-id])
             :reconciled? true})})

        observed-accepted?
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
        :else
        (let [activated (activate-fn request (:ticket state))]
          (if-not (:ok activated)
            (let [failed-state (assoc state :activation/failure activated)]
              (if (:ok (persist-fn failed-state))
                {:ok false :error/code :live-job-activation-failed
                 :state failed-state :finding activated}
                {:ok false :error/code :live-job-activation-failure-persistence-failed
                 :state state}))
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
        (if (and (fn? terminal-submission-provider)
                 (nil? (:terminal-collection state)))
          (let [submission (terminal-submission-provider
                            active-request (:ticket state) job)
                configured (terminal-budget terminal-budget-config)
                collection (terminal-collection-record
                            active-request (:ticket state) job submission 1)
                next-state (assoc state :terminal-collection
                                  {:evidence collection :submission submission
                                   :budget configured})]
            (if (:ok (persist-fn next-state))
              {:ok true :status :terminal-collected :state next-state
               :collection collection}
              {:ok false :error/code :live-job-terminal-collection-persistence-failed
               :state state}))
        (let [submission (if (fn? terminal-submission-provider)
                           (get-in state [:terminal-collection :submission])
                           nil)
              configured (terminal-budget terminal-budget-config)
              max-repairs (:repair-attempts configured)
              job (if submission
                    (let [payload (:payload submission)]
                      (assoc job
                             :report (merge (:authority submission)
                                            (:evidence payload)
                                            (select-keys payload
                                                         [:command-own-exit
                                                          :outcome
                                                          :failure-account
                                                          ;; Some JSON clients
                                                          ;; emit the Student's
                                                          ;; query ledger beside
                                                          ;; :evidence. Preserve
                                                          ;; it for canonical
                                                          ;; memory-use validation.
                                                          :queries]))
                             :typed-submission submission))
                    job)
              validated (if (and (fn? terminal-submission-provider)
                                 (nil? submission))
                          {:ok false :error/code :live-job-submission-missing
                           :findings [:typed-submission-missing]}
                          (terminal-validator active-request (:ticket state) job))
              typed-contract-migration?
              (and (fn? terminal-submission-provider)
                   (= [:typed-submission-missing] (:findings validated))
                   (pos? (or (:terminal-repair-attempts state) 0))
                   (zero? (or (:typed-submission-migration-attempts state) 0)))]
          (if (:ok validated)
            (let [provided (receipt-provider active-request (:ticket state)
                                             job validated)]
              (cond
                (not (:ok provided))
                provided

                ;; A provider may defer certification behind a further job
                ;; (a Guide deposit's independent review); the validated
                ;; terminal is re-observed on the next tick.
                (= :awaiting-terminal (:status provided))
                (assoc provided :state state)

                ;; Receipt providers may hold at another durable boundary
                ;; (for example, a Guide promotion awaiting apparatus repair).
                ;; Only an explicit :certified result carrying a certificate
                ;; may turn the live job into :live-job-certified.
                (and (some? (:status provided))
                     (not= :certified (:status provided)))
                (assoc provided :state state)

                (not (map? (:certificate provided)))
                {:ok false
                 :error/code :live-job-certificate-missing
                 :provider-result (dissoc provided :state)}

                :else
                (let [next-state (assoc state :state/type :live-job-certified
                                        :receipt (:certificate provided))]
                  (if (:ok (persist-fn next-state))
                    {:ok true :status :certified :state next-state
                     :certificate (:certificate provided)}
                    {:ok false
                     :error/code :live-job-receipt-persistence-failed}))))
            (cond
              (and (>= (or (:terminal-repair-attempts state) 0) max-repairs)
                   (not typed-contract-migration?))
              (if (and (= [:typed-submission-missing] (:findings validated))
                       (fn? missing-observation-provider))
                (let [provided (missing-observation-provider
                                active-request (:ticket state) job
                                (:terminal-repair-attempts state)
                                (get-in state [:terminal-collection :evidence]))]
                  (if-not (:ok provided)
                    provided
                    (let [receipt (:certificate provided)
                          recovered? (= :student-observation-recovered
                                        (:receipt/type receipt))
                          next-state (assoc state :state/type :live-job-certified
                                            :receipt receipt
                                            :learning/outcome
                                            (if recovered? :observed :unobserved))]
                      (if (:ok (persist-fn next-state))
                        {:ok true :status :certified :state next-state
                         :certificate (:certificate provided)}
                        {:ok false :error/code :live-job-receipt-persistence-failed}))))
                (assoc validated :error/code :live-job-terminal-repair-exhausted
                       :repair/attempts (:terminal-repair-attempts state)))

              (not (fn? terminal-repair-request-fn)) validated

              :else
              (let [repair (terminal-repair-request-fn
                            active-request (:ticket state) job
                            (cond-> validated
                              typed-contract-migration?
                              (assoc :repair/kind
                                     :typed-submission-contract-migration)
                              (not typed-contract-migration?)
                              (assoc :repair/next-attempt
                                     (inc (or (:terminal-repair-attempts state) 0)))))
                    repair-request (:request repair)]
                (if-not (and (:ok repair) (map? repair-request)
                             (string? (:dispatch/id repair-request)))
                  {:ok false
                   :error/code :live-job-terminal-repair-request-invalid
                   :finding repair}
                  (let [predecessor
                        {:job job
                         :ticket (:ticket state)
                         :terminal-collection (:terminal-collection state)
                         :findings (:findings validated)}
                        already-archived?
                        (= (:job-id job)
                           (get-in (peek (:superseded-terminals state))
                                   [:job :job-id]))
                        archived-state
                        (cond-> (dissoc state :terminal-collection)
                          (not already-archived?)
                          (update :superseded-terminals (fnil conj []) predecessor))
                        archived-persisted (persist-fn archived-state)]
                    (if-not (:ok archived-persisted)
                      {:ok false
                       :error/code :live-job-terminal-repair-archive-persistence-failed
                       :state state}
                      (let [announced (ticket repair-request
                                              (announce-fn repair-request))]
                        (if-not (:ok announced)
                          announced
                          (let [next-state
                            (cond->
                             (assoc archived-state
                                    :active-request repair-request
                                    :ticket (:ticket announced)
                                    :activation/accepted? false
                                    :terminal-repair-attempts
                                    (if typed-contract-migration?
                                      (:terminal-repair-attempts state)
                                      (inc (or (:terminal-repair-attempts state) 0)))
                                    :terminal-repair/original-job-id (:job-id job)
                                    :terminal-repair/findings (:findings validated))
                              typed-contract-migration?
                              (assoc :typed-submission-migration-attempts 1
                                     :typed-submission-migration/of-job-id
                                     (:job-id job)))]
                        (if-not (:ok (persist-fn next-state))
                          {:ok false
                           :error/code :live-job-terminal-repair-persistence-failed}
                          (let [registered (if (fn? ticket-register-fn)
                                             (ticket-register-fn
                                              repair-request (:ticket announced))
                                             {:ok true})
                                activated (when (:ok registered)
                                            (activate-fn repair-request
                                                         (:ticket announced)))]
                            (if-not (:ok registered)
                              {:ok false
                               :error/code :live-job-submission-authority-registration-failed
                               :state next-state :finding registered}
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
                                   :state next-state})))))))))))))))))))))
