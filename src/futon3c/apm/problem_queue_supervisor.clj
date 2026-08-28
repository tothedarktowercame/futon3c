(ns futon3c.apm.problem-queue-supervisor
  "Just-in-time frame minting over a pinned problem queue.

  At most one frame is provisioned. A successor may be minted only from a
  durably terminal predecessor; queued problems carry no seats or workspaces."
  (:require [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.phase-status :as phase-status]))

(def terminal-results #{:closed :partial :void})

(declare prepare-next)

(defn valid-frame-park?
  [park]
  (and
   (every? #(and (string? %) (not-empty %))
           ((juxt :frame/id :problem/id :residual
                  :last-valid-receipt/id) park))
   (= :claude-supervisor (:decision/owner park))
   (or (and (= :awaiting-decision (:decision/status park))
            (true? (:decision/bell-required park)))
       (and (= :decided (:decision/status park))
            (false? (:decision/bell-required park))
            (= (:last-valid-receipt/id park)
               (get-in park [:decision/record :last-valid-receipt/id]))))
   (case (:state/type park)
     :solver-human-intervention-frame-park
     (and (every? #(and (string? %) (not-empty %))
                  ((juxt :solver/final-head :solver/state-path) park))
          (= :claude-required (:student/decision park))
          (pos-int? (:solver/rounds-completed park)))

     :scribe-reduce-apparatus-frame-park
     (and (= :scribe-reduce (:phase park))
          (= :promotion-deposit-retries-exhausted (:error/code park))
          (string? (:promotion/state-path park))
          (pos-int? (:deposit/attempts park))
          (seq (:deposit/findings park)))

     :promotion-apparatus-frame-park
     (and (= :promotion (:phase park))
          (= :promotion-apparatus-repair-exhausted (:error/code park))
          (string? (:promotion/state-path park))
          (keyword? (:repair/kind park))
          (pos-int? (:repair/attempts park))
          (seq (:promotion/findings park)))

     :role-terminal-repair-frame-park
     (and (keyword? (:phase park))
          (= :live-job-terminal-repair-exhausted (:error/code park))
          (string? (:role/state-path park))
          (keyword? (:repair/kind park))
          (pos-int? (:repair/attempts park))
          (seq (:role/findings park)))

     false)))

(defn queue-plan [problems]
  (let [body {:queue/type :apm-problem-queue :queue/version 1
              :problems (mapv #(select-keys % [:problem/id :repository
                                                :base-branch :revision :path
                                                :blob :classification])
                              problems)}]
    (assoc body :queue/id (machine/ledger-digest [body]))))

(defn validate-plan [plan]
  (let [problems (:problems plan)
        ids (mapv :problem/id problems)
        findings (cond-> []
                   (not= :apm-problem-queue (:queue/type plan))
                   (conj :queue-type-invalid)
                   (not= 1 (:queue/version plan)) (conj :queue-version-invalid)
                   (not= (:queue/id plan)
                         (machine/ledger-digest [(dissoc plan :queue/id)]))
                   (conj :queue-content-address-invalid)
                   (empty? problems) (conj :queue-empty)
                   (not= (count ids) (count (distinct ids)))
                   (conj :queue-problem-duplicate)
                   (some #(or (not (every? string?
                                           ((juxt :problem/id :repository :revision
                                                  :path :blob) %)))
                              (not= :non-excluded (:classification %)))
                         problems)
                   (conj :queue-problem-ineligible))]
    (if (seq findings)
      {:ok false :error/code :problem-queue-invalid :findings findings}
      {:ok true :plan plan})))

(defn initial-state [plan]
  (let [body {:state/type :apm-problem-queue :state/version 1
              :queue/id (:queue/id plan) :next-index 0
              :active nil :completed []}]
    (assoc body :state/id (machine/ledger-digest [body]))))

(defn valid-state? [state]
  (and (= :apm-problem-queue (:state/type state))
       (= 1 (:state/version state))
       (or (not= :voided-slot-awaiting-revision (:status state))
           (let [handoff (:statement-repair/handoff state)]
             (and (= :statement-repair (:obligation/type handoff))
                  (= :guide (:repair/role handoff))
                  (= 1 (:repair/attempt handoff))
                  (= 1 (:repair/max-attempts handoff))
                  (true? (:dispatch/intent-persisted handoff))
                  (every? #(and (string? %) (not-empty %))
                          ((juxt :obligation/id :frame/id :problem/id
                                 :source/revision :source/path :source/blob)
                           handoff)))))
       (= (:state/id state)
          (machine/ledger-digest [(dissoc state :state/id)]))))

(defn- addressed [state]
  (assoc (dissoc state :state/id) :state/id
         (machine/ledger-digest [(dissoc state :state/id)])))

(defn- statement-repair-handoff [frame terminal-receipt]
  (let [problem (:problem frame)
        body {:obligation/type :statement-repair
              :frame/id (:frame/id frame) :problem/id (:problem/id frame)
              :repair/role :guide :repair/attempt 1 :repair/max-attempts 1
              :source/repository (:repository problem)
              :source/revision (:revision problem) :source/path (:path problem)
              :source/blob (:blob problem)
              :diagnostic (merge
                           {:problem/outcome :refuted
                            :terminal-receipt/id (:receipt/id terminal-receipt)}
                           (select-keys terminal-receipt
                                        [:problem/registered-target
                                         :void/failed-invariants :error/code]))
              :instruction :repair-registered-statement-once
              :required-output [:replacement-pinned-problem :guide-receipt]
              :exhaustion/action :discard-and-advance
              :dispatch/intent-persisted true
              :dispatch/status :pending}]
    (assoc body :obligation/id (machine/ledger-digest [body]))))

(defn- dispatch-repair [state dispatch-fn persist-state-fn]
  (let [handoff (:statement-repair/handoff state)]
    (if-not (fn? dispatch-fn)
      {:ok false :error/code :problem-queue-guide-dispatch-provider-missing
       :state state}
      (let [result (dispatch-fn handoff)]
        (if-not (and (:ok result)
                     (string? (:dispatch/id result))
                     (not-empty (:dispatch/id result)))
          (merge {:ok false :error/code :problem-queue-guide-dispatch-failed
                  :state state} (select-keys result [:dispatch/error]))
          (let [dispatched (addressed
                            (assoc state :statement-repair/handoff
                                   (assoc handoff :dispatch/status :dispatched
                                          :dispatch/id (:dispatch/id result))))]
            (if (:ok (persist-state-fn dispatched))
              {:ok true :status :guide-statement-repair-dispatched
               :state dispatched :handoff (:statement-repair/handoff dispatched)}
              {:ok false :error/code
               :problem-queue-state-persistence-failed})))))))

(defn revise-voided-slot
  "Replace the pins of the slot restored by a void, preserving its logical id.

  Returns a new content-addressed plan and matching state. No other queue slot
  or cursor position may change."
  [plan state replacement repair-receipt]
  (let [slot (:next-index state)
        current (get (:problems plan) slot)
        problem-id (:problem/id current)
        attempts (get-in state [:statement-repair-attempts problem-id] 0)]
    (cond
      (not (valid-state? state))
      {:ok false :error/code :problem-queue-state-invalid}
      (not= :voided-slot-awaiting-revision (:status state))
      {:ok false :error/code :problem-queue-slot-revision-not-authorized}
      (not= (:queue/id plan) (:queue/id state))
      {:ok false :error/code :problem-queue-state-plan-mismatch}
      (nil? current)
      {:ok false :error/code :problem-queue-voided-slot-missing}
      (not= (:problem/id current) (:problem/id replacement))
      {:ok false :error/code :problem-queue-slot-problem-id-changed}
      (not (and (= :guide (:repair/role repair-receipt))
                (= (get-in state [:statement-repair/handoff :obligation/id])
                   (:obligation/id repair-receipt))
                (string? (:receipt/id repair-receipt))
                (re-matches #"[0-9a-f]{64}" (:receipt/id repair-receipt))))
      {:ok false :error/code :problem-queue-guide-repair-receipt-invalid}
      (not (zero? attempts))
      {:ok false :error/code :problem-queue-statement-repair-exhausted}
      :else
      (let [problems (assoc (:problems plan) slot replacement)
            revised (queue-plan problems)
            checked (validate-plan revised)]
        (if-not (:ok checked)
          checked
          {:ok true :plan revised
           :state (addressed
                   (-> state
                       (assoc :queue/id (:queue/id revised))
                       (assoc-in [:statement-repair-attempts problem-id] 1)
                       (assoc-in [:statement-repair-receipts problem-id]
                                 (:receipt/id repair-receipt))
                       (dissoc :status :statement-repair/handoff)))})))))

(defn- collect-repair [plan state providers]
  (let [{:keys [observe-statement-repair-fn persist-state-fn]} providers
        handoff (:statement-repair/handoff state)]
    (if-not (fn? observe-statement-repair-fn)
      {:ok false :error/code :problem-queue-guide-observation-provider-missing}
      (let [observed (observe-statement-repair-fn handoff)]
        (cond
          (not (:ok observed)) observed
          (= :pending (:status observed))
          {:ok true :status :guide-statement-repair-dispatched
           :state state :handoff handoff}
          (= :failed (:status observed))
          (let [problem-id (:problem/id handoff)
                advanced (addressed
                          (-> state
                              (assoc-in [:statement-repair-attempts problem-id] 1)
                              (update :next-index inc)
                              (dissoc :status :statement-repair/handoff)))]
            (if-not (:ok (persist-state-fn advanced))
              {:ok false :error/code :problem-queue-state-persistence-failed}
              (prepare-next plan advanced providers)))
          (= :complete (:status observed))
          (let [revised (revise-voided-slot
                         plan state (:replacement-pinned-problem observed)
                         (:guide-receipt observed))]
            (if-not (:ok revised)
              revised
              (if-not (:ok (persist-state-fn (:state revised)))
                {:ok false :error/code :problem-queue-state-persistence-failed}
                (prepare-next (:plan revised) (:state revised) providers))))
          :else
          {:ok false :error/code :problem-queue-guide-observation-invalid})))))

(defn reconcile-park-decisions
  "Attach authoritative decision records to their receipt-matched parks.

  Matching is deliberately by the last valid receipt rather than frame id: a
  frame may park more than once. Unmatched parks and records are inert. The
  decision's disposition is recorded but never executed here."
  [state decision-records]
  (if-not (valid-state? state)
    {:ok false :error/code :problem-queue-state-invalid}
    (let [by-receipt (into {}
                           (keep (fn [record]
                                   (when-let [receipt
                                              (:last-valid-receipt/id record)]
                                     [receipt record])))
                           decision-records)
          matched (volatile! [])
          parks (mapv
                 (fn [park]
                   (if-let [record (get by-receipt
                                        (:last-valid-receipt/id park))]
                     (do
                       (vswap! matched conj (:last-valid-receipt/id park))
                       (assoc park
                              :decision/status :decided
                              :decision/bell-required false
                              :decision/record record))
                     park))
                 (:parked state))
          changed? (not= parks (:parked state))]
      {:ok true
       :changed? changed?
       :matched-receipt-ids @matched
       :unmatched-records (->> decision-records
                               (remove #(contains? (set @matched)
                                                   (:last-valid-receipt/id %)))
                               vec)
       :state (if changed?
                (addressed (assoc state :parked parks))
                state)})))

(defn pause-after-active
  "Durably request that the active frame finish and retire without minting a
  successor. The active frame and queue cursor are otherwise unchanged."
  [state]
  (cond
    (not (valid-state? state))
    {:ok false :error/code :problem-queue-state-invalid}
    (nil? (:active state))
    {:ok false :error/code :problem-queue-no-active-frame}
    :else
    {:ok true :state (addressed (assoc state :status :pause-after-active))}))

(defn resume-paused
  "Return an intentionally paused queue to its ordinary runnable state.

  The cursor, completed frames, and absence of an active frame are preserved;
  the next tick remains solely responsible for minting the successor."
  [state]
  (cond
    (not (valid-state? state))
    {:ok false :error/code :problem-queue-state-invalid}
    (not= :paused (:status state))
    {:ok false :error/code :problem-queue-not-paused}
    (some? (:active state))
    {:ok false :error/code :problem-queue-paused-active-frame-invalid}
    :else
    {:ok true :state (addressed (dissoc state :status))}))

(defn complete-active-without-successor
  "Record a retryable terminal frame without preparing the queue's next item.

  NEXT-INDEX is deliberately unchanged. Callers must disable this queue before
  starting the distinct same-problem retry queue."
  [state terminal-receipt]
  (let [active (:active state)]
    (cond
      (not (valid-state? state))
      {:ok false :error/code :problem-queue-state-invalid}
      (nil? active)
      {:ok false :error/code :problem-queue-no-active-frame}
      (not (and (= :partial (:frame/result terminal-receipt))
                (= :unsolved (:problem/outcome terminal-receipt))
                (true? (:retry/same-problem? terminal-receipt))))
      {:ok false :error/code :problem-queue-retry-terminal-invalid}
      :else
      {:ok true
       :state (addressed
               (-> state
                   (update :completed conj
                           {:frame/id (get-in active [:frame :frame/id])
                            :problem/id (get-in active [:frame :problem/id])
                            :frame/result :partial
                            :retry/same-problem? true
                            :terminal-receipt/id (:receipt/id terminal-receipt)})
                   (assoc :active nil :status :retry-superseded)))})))

(defn- prepare-next
  [plan state {:keys [mint-frame-fn qualify-frame-fn prepare-frame-fn
                      persist-state-fn]}]
  (if (= (:next-index state) (count (:problems plan)))
    (let [complete (addressed (assoc state :status :complete))
          persisted (persist-state-fn complete)]
      (if (:ok persisted)
        {:ok true :status :batch-complete :state complete}
        {:ok false :error/code :problem-queue-state-persistence-failed}))
    (let [problem (nth (:problems plan) (:next-index state))
          minted (mint-frame-fn {:problem problem :ordinal (:next-index state)
                                 :queue/id (:queue/id plan)})
          ;; Retain the qualifier's own result. Discarding it left
          ;; :problem-queue-frame-qualification-failed as the only record, so a
          ;; stalled regulator said THAT qualification failed and nothing about
          ;; WHY -- unreadable artifact, bad digest, invalid mint all look
          ;; identical. Cost an evening of guessing on 2026-08-26.
          qualification (when (:ok minted) (qualify-frame-fn (:frame minted)))]
      (cond
        (not (:ok minted)) minted
        (= :queued-frame-eligibility-invalid (:error/code qualification))
        (let [frame (:frame minted)
              park {:state/type :eligibility-frame-park
                    :frame/id (:frame/id frame)
                    :problem/id (:problem/id frame)
                    :decision/status :parked
                    :park/reason :problem-ineligible
                    :qualification qualification}
              advanced (addressed
                        (-> state
                            (update :parked (fnil conj []) park)
                            (update :next-index inc)))
              persisted (persist-state-fn advanced)]
          (if-not (:ok persisted)
            {:ok false :error/code :problem-queue-state-persistence-failed}
            (prepare-next plan advanced
                          {:mint-frame-fn mint-frame-fn
                           :qualify-frame-fn qualify-frame-fn
                           :prepare-frame-fn prepare-frame-fn
                           :persist-state-fn persist-state-fn})))
        (not (:ok qualification))
        {:ok false :error/code :problem-queue-frame-qualification-failed
         :frame (:frame minted) :qualification qualification}
        :else
        (let [prepared (prepare-frame-fn (:frame minted))]
          (if-not (:ok prepared)
            prepared
            (let [active {:frame (:frame minted)
                          :preparation/id (:preparation/id prepared)}
                  advanced (addressed (-> state
                                         (assoc :active active)
                                         (update :next-index inc)))
                  persisted (persist-state-fn advanced)]
              (if (:ok persisted)
                {:ok true :status :frame-prepared :state advanced
                 :frame (:frame minted)}
                {:ok false :error/code
                 :problem-queue-state-persistence-failed}))))))))

(defn tick!
  "Perform one queue transition.

  A nil active frame prepares the first/next problem. An active frame receives
  one supervised tick. Only a terminal result can retire it and authorize
  just-in-time creation of its successor."
  [{:keys [plan state-provider persist-state-fn mint-frame-fn
           qualify-frame-fn prepare-frame-fn frame-tick-fn retire-frame-fn
           dispatch-statement-repair-fn]
    :as providers}]
  (let [plan-check (validate-plan plan)
        state (or (state-provider) (initial-state plan))]
    (cond
      (not (:ok plan-check)) plan-check
      (not (every? fn? [state-provider persist-state-fn mint-frame-fn
                        qualify-frame-fn prepare-frame-fn frame-tick-fn
                        retire-frame-fn]))
      {:ok false :error/code :problem-queue-provider-missing}
      (not (valid-state? state))
      {:ok false :error/code :problem-queue-state-invalid}
      (not= (:queue/id plan) (:queue/id state))
      {:ok false :error/code :problem-queue-state-plan-mismatch}
      (= :complete (:status state))
      {:ok true :status :batch-complete :state state}
      (= :paused (:status state))
      {:ok true :status :batch-paused :state state}
      (= :voided-slot-awaiting-revision (:status state))
      (if (= :dispatched
             (get-in state [:statement-repair/handoff :dispatch/status]))
        (collect-repair plan state providers)
        (dispatch-repair state dispatch-statement-repair-fn persist-state-fn))
      (nil? (:active state))
      (prepare-next plan state providers)
      :else
      (let [active (:active state)
            result (frame-tick-fn (:frame active))]
        (cond
          (not (:ok result)) result
          (= :unknown (phase-status/classify :problem-queue-frame
                                             (:status result)))
          {:ok false
           :error/code :problem-queue-frame-status-vocabulary-incomplete
           :finding {:status (:status result)
                     :known-statuses
                     (vec (sort (phase-status/known-statuses
                                 :problem-queue-frame)))}}
          (= :frame-parked (:status result))
          (let [park (:frame/park result)]
            (if-not (and (valid-frame-park? park)
                         (= (:frame/id park)
                            (get-in active [:frame :frame/id]))
                         (= (:problem/id park)
                            (get-in active [:frame :problem/id])))
              {:ok false :error/code :problem-queue-frame-park-invalid}
              (let [pause? (= :pause-after-active (:status state))
                    cleared (addressed
                             (-> state
                                 (update :parked (fnil conj []) park)
                                 (assoc :active nil)
                                 (cond-> pause? (assoc :status :paused))))
                    persisted (persist-state-fn cleared)]
                (if-not (:ok persisted)
                  {:ok false :error/code
                   :problem-queue-state-persistence-failed}
                  (if pause?
                    {:ok true :status :batch-paused :state cleared}
                    (prepare-next plan cleared providers))))))
          (not= :frame-complete (:status result))
          (assoc result :queue/id (:queue/id plan)
                 :active/frame-id (get-in active [:frame :frame/id]))
          (not (contains? terminal-results (:frame/result result)))
          {:ok false :error/code :problem-queue-terminal-result-invalid}
          :else
          (let [retired (retire-frame-fn
                         {:frame (:frame active) :terminal-receipt
                          (:terminal-receipt result)})]
            (if-not (:ok retired)
              retired
              (let [void? (= :void (:frame/result result))
                    refuted? (and void?
                                  (= :refuted
                                     (get-in result
                                             [:terminal-receipt
                                              :problem/outcome])))
                    problem-id (get-in active [:frame :problem/id])
                    repair-exhausted?
                    (and refuted?
                         (pos? (get-in state
                                       [:statement-repair-attempts problem-id]
                                       0)))
                    pause? (= :pause-after-active (:status state))
                    cleared (addressed
                             (cond-> (assoc state :active nil)
                               (not void?)
                               (update :completed conj
                                       {:frame/id (get-in active [:frame :frame/id])
                                        :problem/id (get-in active
                                                            [:frame :problem/id])
                                        :frame/result (:frame/result result)
                                        :terminal-receipt/id
                                        (get-in result
                                                [:terminal-receipt :receipt/id])})
                               (and refuted? (not repair-exhausted?))
                               (update :next-index dec)
                               (and refuted? (not repair-exhausted?))
                               (assoc :status :voided-slot-awaiting-revision
                                      :statement-repair/handoff
                                      (statement-repair-handoff
                                       (:frame active)
                                       (:terminal-receipt result)))
                               pause? (assoc :status :paused)))
                    persisted (persist-state-fn cleared)]
                (if-not (:ok persisted)
                  {:ok false :error/code
                   :problem-queue-state-persistence-failed}
                  (if (and refuted? (not repair-exhausted?))
                    (dispatch-repair cleared dispatch-statement-repair-fn
                                     persist-state-fn)
                    (if pause?
                    {:ok true :status :batch-paused :state cleared}
                    (prepare-next plan cleared providers))))))))))))
