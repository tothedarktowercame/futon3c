(ns futon3c.apm.problem-queue-supervisor
  "Just-in-time frame minting over a pinned problem queue.

  At most one frame is provisioned. A successor may be minted only from a
  durably terminal predecessor; queued problems carry no seats or workspaces."
  (:require [futon3c.apm.campaign-machine :as machine]))

(def terminal-results #{:closed :partial :void})

(defn valid-frame-park?
  [park]
  (and
   (every? #(and (string? %) (not-empty %))
           ((juxt :frame/id :problem/id :residual
                  :last-valid-receipt/id) park))
   (case (:state/type park)
     :solver-human-intervention-frame-park
     (and (every? #(and (string? %) (not-empty %))
                  ((juxt :solver/final-head :solver/state-path) park))
          (pos-int? (:solver/rounds-completed park)))

     :scribe-reduce-apparatus-frame-park
     (and (= :scribe-reduce (:phase park))
          (= :promotion-deposit-retries-exhausted (:error/code park))
          (string? (:promotion/state-path park))
          (pos-int? (:deposit/attempts park))
          (seq (:deposit/findings park)))

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
       (= (:state/id state)
          (machine/ledger-digest [(dissoc state :state/id)]))))

(defn- addressed [state]
  (assoc (dissoc state :state/id) :state/id
         (machine/ledger-digest [(dissoc state :state/id)])))

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
                (= :partial (:problem/outcome terminal-receipt))
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
                                 :queue/id (:queue/id plan)})]
      (cond
        (not (:ok minted)) minted
        (not (:ok (qualify-frame-fn (:frame minted))))
        {:ok false :error/code :problem-queue-frame-qualification-failed
         :frame (:frame minted)}
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
           qualify-frame-fn prepare-frame-fn frame-tick-fn retire-frame-fn]
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
      (nil? (:active state))
      (prepare-next plan state providers)
      :else
      (let [active (:active state)
            result (frame-tick-fn (:frame active))]
        (cond
          (not (:ok result)) result
          (not (contains? #{:parked :phase-advanced :terminal-collected
                            :claim-recovered :frame-parked :frame-complete}
                          (:status result)))
          {:ok false :error/code :problem-queue-frame-status-invalid}
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
              (let [pause? (= :pause-after-active (:status state))
                    cleared (addressed
                             (-> state
                                 (update :completed conj
                                         {:frame/id (get-in active [:frame :frame/id])
                                          :problem/id (get-in active
                                                              [:frame :problem/id])
                                          :frame/result (:frame/result result)
                                          :terminal-receipt/id
                                          (get-in result
                                                  [:terminal-receipt :receipt/id])})
                                 (assoc :active nil)
                                 (cond-> pause? (assoc :status :paused))))
                    persisted (persist-state-fn cleared)]
                (if-not (:ok persisted)
                  {:ok false :error/code
                   :problem-queue-state-persistence-failed}
                  (if pause?
                    {:ok true :status :batch-paused :state cleared}
                    (prepare-next plan cleared providers)))))))))))
