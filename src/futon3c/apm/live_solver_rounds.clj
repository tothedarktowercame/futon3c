(ns futon3c.apm.live-solver-rounds
  "Durable multi-round controller for a solver siege.

   A terminal Agency turn is not a terminal solve.  Unsolved turns are recorded
   and the same seat/session/branch is continued up to an explicit round cap."
  (:require [clojure.string :as str]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.live-job-driver :as job-driver]
            [futon3c.apm.typed-role-submission :as submission]))

(def default-max-rounds 50)
(def strategy-checkpoint-every 10)

(defn strategy-checkpoint-round? [ordinal]
  (zero? (mod ordinal strategy-checkpoint-every)))

(defn- checkpoint-round? [request ordinal]
  (boolean
   (or (strategy-checkpoint-round? ordinal)
       (and (= 1 ordinal) (:solver/strategy-before-solve? request)))))

(defn- select-round-role-card [request ordinal]
  (let [checkpoint? (checkpoint-round? request ordinal)
        [path blob] (if checkpoint?
                      [(or (:solver/restrategize-role-card-path request)
                           (:role-card-path request))
                       (or (:solver/restrategize-role-card-blob request)
                           (:role-card-blob request))]
                      [(or (:solver/regular-role-card-path request)
                           (:role-card-path request))
                       (or (:solver/regular-role-card-blob request)
                           (:role-card-blob request))])]
    (-> request
        (assoc :role-card-path path :role-card-blob blob
               :solver/role-card-mode (if checkpoint? :restrategize :regular))
        (dissoc :solver/regular-role-card-path :solver/regular-role-card-blob
                :solver/restrategize-role-card-path :solver/restrategize-role-card-blob))))

(defn round-request [base-request ordinal prior]
  (let [body (cond-> (-> (select-round-role-card base-request ordinal)
                         (dissoc :dispatch/id)
                         (assoc :solver/round ordinal
                                :solver/max-rounds default-max-rounds
                                :solver/remaining-rounds
                                (- default-max-rounds ordinal)
                                :solver/strategy-checkpoint?
                                (checkpoint-round? base-request ordinal)))
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
    (submission/prepare-request
     (assoc body :dispatch/id (machine/ledger-digest [body])))))

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
  [{:keys [announce-fn activate-fn persist-fn ticket-register-fn]} state]
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
          (let [registered (if (fn? ticket-register-fn)
                             (ticket-register-fn request (:ticket announced))
                             {:ok true})
                activated (when (:ok registered)
                            (activate-fn request (:ticket announced)))]
            (if-not (:ok registered)
              {:ok false
               :error/code :solver-round-submission-authority-registration-failed
               :state staged :finding registered}
            (if-not (:ok activated)
              {:ok false :error/code :solver-round-activation-failed
               :state staged :finding activated}
              (let [accepted (assoc-in staged [:active :activation/accepted?] true)
                    saved (persist-container persist-fn accepted)]
                (if (:ok saved)
                  {:ok true :status :awaiting-terminal
                   :job-id (get-in accepted [:active :ticket :job-id])
                   :state accepted}
                  saved))))))))))

(defn- dispatch-terminal-repair!
  [{:keys [announce-fn activate-fn persist-fn ticket-register-fn]}
   state findings]
  (let [active (:active state)
        attempt (inc (or (:terminal-repair-attempts active) 0))
        body (-> (:request active)
                 (dissoc :dispatch/id)
                 (assoc :repair/attempt attempt
                        :repair/of-job-id (get-in active [:ticket :job-id])
                        :repair/findings (or findings
                                             [:typed-submission-missing])))
        request (submission/prepare-request
                 (assoc body :dispatch/id (machine/ledger-digest [body])))
        announced (job-driver/ticket request (announce-fn request))]
    (if-not (:ok announced)
      announced
      (let [next-active {:state/type :live-job-dispatched :request request
                         :ticket (:ticket announced)
                         :terminal-repair-attempts attempt}
            staged (assoc state :active next-active)]
        (if-not (:ok (persist-container persist-fn staged))
          {:ok false :error/code :solver-terminal-repair-persistence-failed}
          (let [registered (if (fn? ticket-register-fn)
                             (ticket-register-fn request (:ticket announced))
                             {:ok true})
                activated (when (:ok registered)
                            (activate-fn request (:ticket announced)))]
            (cond
              (not (:ok registered))
              {:ok false :error/code :solver-terminal-repair-registration-failed}
              (not (:ok activated))
              {:ok false :error/code :solver-terminal-repair-activation-failed}
              :else
              (let [accepted (assoc-in staged [:active :activation/accepted?] true)]
                (if (:ok (persist-container persist-fn accepted))
                  {:ok true :status :awaiting-terminal :repair? true
                   :state accepted}
                  {:ok false
                   :error/code :solver-terminal-repair-acceptance-persistence-failed})))))))))

(defn- normalize-round-report [report]
  (let [lean (:lean report)
        account (:failure-account report)
        prefixed (fn [prefix]
                   (some (fn [entry]
                           (when (and (string? entry)
                                      (str/starts-with? entry prefix))
                             (subs entry (count prefix))))
                         account))]
    (cond-> report
      (and (nil? (:solver/outcome report)) (:solver/outcome lean))
      (assoc :solver/outcome (:solver/outcome lean))
      (and (nil? (:solver/outcome report)) (string? (:outcome report)))
      (assoc :solver/outcome (keyword (:outcome report)))
      (and (nil? (:residual report)) (:residual lean))
      (assoc :residual (:residual lean))
      (and (nil? (:residual report)) (prefixed "residual: "))
      (assoc :residual (prefixed "residual: "))
      (and (nil? (:artifact-commits report)) (:artifact-commits lean))
      (assoc :artifact-commits (:artifact-commits lean))
      (and (nil? (:artifact-commits report)) (prefixed "artifact-commits: "))
      (assoc :artifact-commits (prefixed "artifact-commits: ")))))

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
              {:ok true :status :awaiting-terminal
               :job-id (get-in accepted [:active :ticket :job-id])
               :state accepted}
              saved))))

      :else
      (let [active (:active state)
            raw-job (job-fn (get-in active [:ticket :job-id]))
            provider (:terminal-submission-provider effects)
            terminal? (contains? job-driver/terminal-states (:state raw-job))
            collection (:terminal-collection active)
            freshly-collected (when (and terminal? provider (nil? collection))
                                (provider (:request active) (:ticket active)
                                          raw-job))
            typed (if provider (:submission collection) nil)
            job (if typed
                  (let [payload (:payload typed)]
                    (assoc raw-job
                           :report (merge (:authority typed) (:evidence payload)
                                          (select-keys payload
                                                       [:command-own-exit :outcome
                                                        :failure-account]))
                           :typed-submission typed))
                  raw-job)
            expected-session (:session-id (first (:rounds state)))]
        (cond
          (and terminal? provider (nil? collection))
          (let [evidence (job-driver/terminal-collection-record
                          (:request active) (:ticket active) raw-job
                          freshly-collected 1)
                next-state (assoc-in state [:active :terminal-collection]
                                     {:evidence evidence
                                      :submission freshly-collected})]
            (if (:ok (persist-container persist-fn next-state))
              {:ok true :status :terminal-collected :state next-state
               :collection evidence}
              {:ok false :error/code :solver-terminal-collection-persistence-failed}))

          (not (contains? job-driver/terminal-states (:state job)))
          {:ok true :status :awaiting-terminal
           :job-id (get-in state [:active :ticket :job-id])
           :state state}

          (and (= :done (:state job))
               provider
               (nil? typed))
          (let [max-repairs (get-in effects [:terminal-budget-config
                                             :repair-attempts] 1)]
            (if (< (or (:terminal-repair-attempts active) 0) max-repairs)
              (dispatch-terminal-repair! effects state
                                         [:typed-submission-missing])
              {:ok false :error/code :solver-typed-submission-repair-exhausted
               :repair/attempts (:terminal-repair-attempts active)
               :collection (:evidence collection) :state state}))

          (and expected-session (not= expected-session (:session-id job)))
            {:ok false :error/code :solver-session-mismatch
             :finding {:expected expected-session :actual (:session-id job)}}

          :else
          (let [validation (if (and (= :done (:state job))
                                    (or (nil? (:terminal-submission-provider effects))
                                        typed))
                             (validate-solved (:request active) (:ticket active) job)
                             {:ok false
                              :error/code (if (= :done (:state job))
                                            :solver-typed-submission-missing
                                            :solver-job-terminal-failure)})]
            (if (and (:solver/strategy-before-solve? (:base-request state))
                     (= 1 (get-in active [:request :solver/round])))
              (let [completed (terminal-round active job validation 1)
                    next-state (assoc state :rounds (conj (:rounds state) completed)
                                      :active nil)]
                (if (strategy-checkpoint-valid? (:report job))
                  (let [saved (persist-container persist-fn next-state)]
                    (if (:ok saved)
                      (dispatch-round! effects next-state)
                      saved))
                  (let [stopped (assoc next-state
                                       :state/type :solver-strategy-checkpoint-required)
                        saved (persist-container persist-fn stopped)]
                    (if (:ok saved)
                      {:ok false :error/code :solver-strategy-checkpoint-required
                       :state stopped}
                      saved))))
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
                  (let [max-repairs (get-in effects [:terminal-budget-config
                                                     :repair-attempts] 1)]
                    (if (< (or (:terminal-repair-attempts active) 0)
                           max-repairs)
                      (dispatch-terminal-repair!
                       effects
                       (update state :checkpoint/invalid-observations
                               (fnil conj []) completed)
                       [:solver-strategy-missing-or-invalid])
                      (let [stopped (assoc next-state
                                           :state/type
                                           :solver-strategy-checkpoint-required)
                            saved (persist-container persist-fn stopped)]
                        (if (:ok saved)
                          {:ok false
                           :error/code :solver-strategy-checkpoint-required
                           :state stopped}
                          saved))))

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

(defn resume-strategy-collection!
  "Dispatch one bounded, typed collection repair for a persisted checkpoint.

   The invalid terminal observation remains durable under
   :checkpoint/invalid-observations.  It is not counted as a proof-search
   round, and the repair request retains the same checkpoint ordinal."
  [{:keys [state] :as effects}]
  (if-not (and (= :solver-strategy-checkpoint-required (:state/type state))
               (seq (:rounds state))
               (nil? (:active state)))
    {:ok false :error/code :solver-strategy-collection-resume-input-invalid}
    (let [completed (last (:rounds state))
          ordinal (:ordinal completed)
          prior-rounds (pop (:rounds state))
          request (round-request (:base-request state) ordinal
                                 (last prior-rounds))
          active {:state/type :live-job-dispatched
                  :request request
                  :ticket {:job-id (:job-id completed)}
                  :activation/accepted? true
                  :terminal-repair-attempts 0}
          resumed (-> state
                      (assoc :state/type :solver-rounds
                             :rounds prior-rounds
                             :active active)
                      (update :checkpoint/invalid-observations
                              (fnil conj []) completed))]
      (dispatch-terminal-repair!
       effects resumed [:solver-strategy-missing-or-invalid]))))

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
