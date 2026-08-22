(ns futon3c.apm.live-solver-rounds
  "Durable multi-round controller for a solver siege.

   A terminal Agency turn is not a terminal solve.  Unsolved turns are recorded
   and the same seat/session/branch is continued up to an explicit round cap."
  (:require [clojure.string :as str]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.live-job-driver :as job-driver]))

(def default-max-rounds 50)
(def strategy-checkpoint-every 10)

(defn strategy-checkpoint-round? [ordinal]
  (zero? (mod ordinal strategy-checkpoint-every)))

(defn round-request [base-request ordinal prior]
  (let [body (cond-> (-> base-request
                         (dissoc :dispatch/id)
                         (assoc :solver/round ordinal
                                :solver/max-rounds default-max-rounds
                                :solver/remaining-rounds
                                (- default-max-rounds ordinal)
                                :solver/strategy-checkpoint?
                                (strategy-checkpoint-round? ordinal)))
               prior
               (assoc :solver/prior-job-id (:job-id prior)
                      :solver/prior-session-id (:session-id prior)
                      :solver/prior-report (:report prior)
                      :solver/prior-validation (:validation prior)
                      :solver/remediation
                      {:required? (false? (get-in prior [:validation :ok]))
                       :findings (get-in prior [:validation :findings])
                       :instruction
                       "Correct the prior validator findings in committed state, then rerun every terminal check. For an out-of-scope committed mutation, add a corrective commit restoring that path to the registered base; do not rewrite history."}))]
    (assoc body :dispatch/id (machine/ledger-digest [body]))))

(defn- container [state base-request]
  (cond
    (nil? state)
    {:state/type :solver-rounds :budget/max-rounds default-max-rounds
     :base-request base-request :rounds [] :active nil}

    (= :live-job-dispatched (:state/type state))
    {:state/type :solver-rounds :budget/max-rounds default-max-rounds
     :base-request base-request :rounds [] :active state}

    (= :solver-rounds (:state/type state)) state
    :else state))

(defn- persist-container [persist-fn state]
  (let [result (persist-fn state)]
    (if (:ok result) {:ok true :state state}
        {:ok false :error/code :solver-round-state-persistence-failed})))

(defn- dispatch-round!
  [{:keys [announce-fn activate-fn persist-fn]} state]
  (let [ordinal (inc (count (:rounds state)))
        prior (last (:rounds state))
        request (round-request (:base-request state) ordinal prior)
        announced (job-driver/ticket request (announce-fn request))]
    (if-not (:ok announced)
      announced
      (let [active {:state/type :live-job-dispatched :request request
                    :ticket (:ticket announced)}
            staged (assoc state :active active)
            persisted (persist-container persist-fn staged)]
        (if-not (:ok persisted)
          persisted
          (let [activated (activate-fn request (:ticket announced))]
            (if-not (:ok activated)
              {:ok false :error/code :solver-round-activation-failed
               :state staged :finding activated}
              (let [accepted (assoc-in staged [:active :activation/accepted?] true)
                    saved (persist-container persist-fn accepted)]
                (if (:ok saved)
                  {:ok true :status :awaiting-terminal :state accepted}
                  saved)))))))))

(defn- normalize-round-report [report]
  (let [lean (:lean report)]
    (cond-> report
      (and (nil? (:solver/outcome report)) (:solver/outcome lean))
      (assoc :solver/outcome (:solver/outcome lean))
      (and (nil? (:residual report)) (:residual lean))
      (assoc :residual (:residual lean))
      (and (nil? (:artifact-commits report)) (:artifact-commits lean))
      (assoc :artifact-commits (:artifact-commits lean)))))

(defn- round-outcome [report]
  (cond
    (and (= :claimed-defect (:solver/outcome report))
         (string? (:residual report)) (not (str/blank? (:residual report))))
    :claimed-defect

    (and (= :progress (:solver/outcome report))
         (string? (:residual report)) (not (str/blank? (:residual report))))
    :progress

    :else :inadequate))

(defn- strategy-checkpoint-valid? [report]
  (let [{:keys [summary obligations decomposition next-plan]}
        (:solver/strategy report)]
    (and (string? summary) (not (str/blank? summary))
         (vector? obligations)
         (every? #(and (string? %) (not (str/blank? %))) obligations)
         (vector? decomposition)
         (every? (fn [{:keys [obligation decision reason] :as item}]
                   (and (map? item)
                        (string? obligation) (not (str/blank? obligation))
                        (contains? #{:delegate :sequential} decision)
                        (string? reason) (not (str/blank? reason))))
                 decomposition)
         (string? next-plan) (not (str/blank? next-plan)))))

(defn- terminal-round [active job validation ordinal]
  (let [report (normalize-round-report (:report job))]
   {:ordinal ordinal
   :dispatch/id (get-in active [:request :dispatch/id])
   :job-id (get-in active [:ticket :job-id])
   :session-id (:session-id job)
   :terminal-state (:state job)
   :outcome (round-outcome report)
   :report report
   :validation (select-keys validation [:ok :error/code :findings :missing])}))

(defn- terminal-failure-signature [round]
  {:report (:report round)
   :error/code (get-in round [:validation :error/code])
   :findings (get-in round [:validation :findings])
   :missing (or (get-in round [:validation :missing]) #{})})

(defn drive!
  "Advance a solver siege by one durable boundary.

   VALIDATE-SOLVED and PROVIDE-RECEIPT retain the strict proof gate. Any
   terminal return that misses it is recorded as a spent round and continued;
   at MAX-ROUNDS the state requires human intervention and never pretends the
   problem is invalid."
  [{:keys [request state activate-fn job-fn persist-fn
           validate-solved provide-receipt max-rounds]
    :or {max-rounds default-max-rounds} :as effects}]
  (let [state (container state request)]
    (cond
      (= :live-job-certified (:state/type state))
      {:ok true :status :certified :state state :certificate (:receipt state)}

      (= :solver-human-intervention-required (:state/type state))
      {:ok false :error/code :solver-human-intervention-required :state state}

      (= :solver-defect-review-required (:state/type state))
      {:ok false :error/code :solver-defect-review-required :state state}

      (= :solver-remediation-required (:state/type state))
      {:ok false :error/code :solver-remediation-required :state state}

      (not= :solver-rounds (:state/type state))
      {:ok false :error/code :solver-round-state-invalid}

      (nil? (:active state))
      (dispatch-round! effects state)

      (not (get-in state [:active :activation/accepted?]))
      (let [active (:active state)
            activated (activate-fn (:request active) (:ticket active))]
        (if-not (:ok activated)
          {:ok false :error/code :solver-round-activation-failed
           :state state :finding activated}
          (let [accepted (assoc-in state [:active :activation/accepted?] true)
                saved (persist-container persist-fn accepted)]
            (if (:ok saved)
              {:ok true :status :awaiting-terminal :state accepted}
              saved))))

      :else
      (let [active (:active state)
            job (job-fn (get-in active [:ticket :job-id]))
            expected-session (:session-id (first (:rounds state)))]
        (if-not (contains? job-driver/terminal-states (:state job))
          {:ok true :status :awaiting-terminal :state state}
          (if (and expected-session (not= expected-session (:session-id job)))
            {:ok false :error/code :solver-session-mismatch
             :finding {:expected expected-session :actual (:session-id job)}}
          (let [validation (if (= :done (:state job))
                             (validate-solved (:request active) (:ticket active) job)
                             {:ok false :error/code :solver-job-terminal-failure})]
            (if (:ok validation)
              (let [receipt (provide-receipt (:request active) (:ticket active)
                                             job validation)]
                (if-not (:ok receipt)
                  receipt
                  (let [certified {:state/type :live-job-certified
                                   :base-request (:base-request state)
                                   :rounds (:rounds state) :active active
                                   :receipt (:certificate receipt)}
                        saved (persist-container persist-fn certified)]
                    (if (:ok saved)
                      {:ok true :status :certified :state certified
                       :certificate (:certificate receipt)}
                      saved))))
              (let [ordinal (or (get-in active [:request :solver/round])
                                (inc (count (:rounds state))))
                    completed (terminal-round active job validation ordinal)
                    rounds (conj (:rounds state) completed)
                    next-state (assoc state :rounds rounds :active nil)]
                (cond
                  (and (seq (:rounds state))
                       (= (terminal-failure-signature completed)
                          (terminal-failure-signature
                           (last (:rounds state)))))
                  (let [stopped (assoc next-state
                                       :state/type :solver-remediation-required
                                       :remediation
                                       {:finding (:validation completed)
                                        :instruction
                                        "Correct the repeated validator finding in committed state before resuming; identical terminal artifacts must not consume the proof-search budget."})
                        saved (persist-container persist-fn stopped)]
                    (if (:ok saved)
                      {:ok false :error/code :solver-remediation-required
                       :state stopped}
                      saved))

                  (and (< (count rounds) max-rounds)
                       (strategy-checkpoint-round? ordinal)
                       (not (strategy-checkpoint-valid? (:report completed))))
                  (let [stopped (assoc next-state
                                       :state/type :solver-strategy-checkpoint-required)
                        saved (persist-container persist-fn stopped)]
                    (if (:ok saved)
                      {:ok false :error/code :solver-strategy-checkpoint-required
                       :state stopped}
                      saved))

                  (= :claimed-defect (:outcome completed))
                  (let [stopped (assoc next-state
                                       :state/type :solver-defect-review-required)
                        saved (persist-container persist-fn stopped)]
                    (if (:ok saved)
                      {:ok false :error/code :solver-defect-review-required
                       :state stopped}
                      saved))

                  (>= (count rounds) max-rounds)
                  (let [stopped (assoc next-state
                                       :state/type :solver-human-intervention-required
                                       :budget/max-rounds max-rounds)
                        saved (persist-container persist-fn stopped)]
                    (if (:ok saved)
                      {:ok false :error/code :solver-human-intervention-required
                       :state stopped}
                      saved))
                  :else
                  (let [saved (persist-container persist-fn next-state)]
                    (if (:ok saved)
                      (dispatch-round! effects next-state)
                      saved))))))))))))

(defn repair-checkpoint!
  "Revalidate the already-terminal checkpoint artifact after a protocol repair.

   No job is announced or activated. The spent rounds remain part of the
   certified state and only a genuinely valid terminal artifact can cross it."
  [{:keys [state persist-fn validate-solved provide-receipt]}]
  (if-not (and (= :solver-strategy-checkpoint-required (:state/type state))
               (seq (:rounds state))
               (every? fn? [persist-fn validate-solved provide-receipt]))
    {:ok false :error/code :solver-checkpoint-repair-input-invalid}
    (let [completed (last (:rounds state))
          ordinal (:ordinal completed)
          prior (when (> ordinal 1) (nth (:rounds state) (- ordinal 2)))
          request (round-request (:base-request state) ordinal prior)
          ticket {:job-id (:job-id completed)}
          job {:job-id (:job-id completed)
               :agent-id (:agent-id request)
               :session-id (:session-id completed)
               :state (:terminal-state completed)
               :report (:report completed)}
          validation (validate-solved request ticket job)]
      (if-not (:ok validation)
        {:ok false :error/code :solver-checkpoint-repair-validation-failed
         :validation validation}
        (let [receipt (provide-receipt request ticket job validation)]
          (if-not (:ok receipt)
            receipt
            (let [certified {:state/type :live-job-certified
                             :base-request (:base-request state)
                             :rounds (:rounds state) :active nil
                             :repair/source-state :solver-strategy-checkpoint-required
                             :receipt (:certificate receipt)}
                  saved (persist-container persist-fn certified)]
              (if (:ok saved)
                {:ok true :status :certified :state certified
                 :certificate (:certificate receipt)}
                saved))))))))

(defn resume-remediation!
  "Resume a halted siege with one corrective round carrying typed findings.

   This is an explicit state transition: it never edits the solver workspace or
   certifies the prior artifact. The next seat turn must produce a new terminal
   artifact that passes the unchanged validator."
  [{:keys [state persist-fn] :as effects}]
  (if-not (and (or (= :solver-remediation-required (:state/type state))
                   (and (= :solver-strategy-checkpoint-required
                           (:state/type state))
                        (< 1 (count (:rounds state)))
                        (= (terminal-failure-signature (last (:rounds state)))
                           (terminal-failure-signature
                            (nth (:rounds state) (- (count (:rounds state)) 2))))))
               (seq (:rounds state))
               (fn? persist-fn))
    {:ok false :error/code :solver-remediation-resume-input-invalid}
    (let [resumed (assoc state :state/type :solver-rounds :active nil)
          saved (persist-container persist-fn resumed)]
      (if (:ok saved)
        (dispatch-round! effects resumed)
        saved))))
