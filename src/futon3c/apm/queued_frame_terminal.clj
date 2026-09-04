(ns futon3c.apm.queued-frame-terminal
  "Atomic terminal boundary for a just-in-time queued frame.

  The problem-bank metadata receipt records verified solve pinning. Workspace
  removal is separately authorized by independent retirement audits; neither
  operation is allowed from a conversational or merely terminal-looking status."
  (:require [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.phase-status :as phase-status]))

(def frame-results #{:closed :partial :void})
(def learning-outcomes #{:observed :partially-observed :unobserved :skipped})

(def retirement-audit-retry-delays-ms
  "Bounded retry schedule for transient retirement preconditions.  The seven
  waits total 585 seconds; an eighth pending observation exhausts the retry."
  [15000 30000 60000 120000 120000 120000 120000])

(defn addressed? [receipt id-key]
  (= (get receipt id-key)
     (machine/ledger-digest [(dissoc receipt id-key)])))

(defn validate-terminal [frame receipt]
  (let [progress-retry? (and (= :unsolved (:problem/outcome receipt))
                             (= :partial (:frame/result receipt)))
        valid-outcome? (contains?
                        #{[:solved :closed] [:solved :partial]
                          [:unsolved :partial] [:unsolved :void]
                          [:refuted :void]}
                        [(:problem/outcome receipt) (:frame/result receipt)])
        findings
        (cond-> []
          (not= :frame-terminal (:receipt/type receipt))
          (conj :terminal-receipt-type-invalid)
          (not (addressed? receipt :receipt/id))
          (conj :terminal-receipt-content-invalid)
          (not= (:frame/id frame) (:frame/id receipt))
          (conj :terminal-frame-mismatch)
          (not= (:problem/id frame) (:problem/id receipt))
          (conj :terminal-problem-mismatch)
          (not (contains? frame-results (:frame/result receipt)))
          (conj :terminal-frame-result-invalid)
          (not (contains? #{:solved :unsolved :refuted}
                          (:problem/outcome receipt)))
          (conj :terminal-problem-outcome-invalid)
          (not valid-outcome?)
          (conj :terminal-outcome-result-invalid)
          (not (contains? learning-outcomes (:learning/outcome receipt)))
          (conj :terminal-learning-outcome-invalid)
          (and (not progress-retry?)
               (not= :void (:frame/result receipt))
               (not (and (string? (:verify-receipt/id receipt))
                         (re-matches #"[0-9a-f]{64}"
                                     (:verify-receipt/id receipt)))))
          (conj :terminal-verify-receipt-invalid)
          (and progress-retry?
               (not (and (string? (:solver-progress-receipt/id receipt))
                         (re-matches #"[0-9a-f]{64}"
                                     (:solver-progress-receipt/id receipt)))))
          (conj :terminal-solver-progress-receipt-invalid)
          (not (and (string? (get-in receipt [:solver :branch]))
                    (string? (get-in receipt [:solver :head]))
                    (re-matches #"[0-9a-f]{40}"
                                (get-in receipt [:solver :head]))))
          (conj :terminal-solver-identity-invalid))
        heads (:workspace/terminal-heads receipt)]
    (if (and (= #{:solver :student} (set (keys heads)))
             (every? #(and (string? %) (re-matches #"[0-9a-f]{40}" %))
                     (vals heads))
             (= (get heads :solver) (get-in receipt [:solver :head])))
      (if (seq findings)
        {:ok false :error/code :queued-frame-terminal-invalid :findings findings}
        {:ok true :receipt receipt})
      {:ok false :error/code :queued-frame-terminal-invalid
       :findings (conj findings :terminal-workspace-heads-invalid)})))

(defn build-problem-bank
  ([frame terminal]
   (build-problem-bank frame terminal {:status :skipped}))
  ([frame terminal pin-result]
   (let [progress-retry? (and (= :unsolved (:problem/outcome terminal))
                              (= :partial (:frame/result terminal)))
        body {:receipt/type (if progress-retry?
                              :queued-solver-progress-bank
                              :queued-problem-bank)
              :frame/id (:frame/id frame) :problem/id (:problem/id frame)
              :problem/outcome (:problem/outcome terminal)
              :frame/result (:frame/result terminal)
              :learning/outcome (:learning/outcome terminal)
              :verify-receipt/id (:verify-receipt/id terminal)
              :solver-progress-receipt/id
              (:solver-progress-receipt/id terminal)
              :retry/same-problem? progress-retry?
              :source/terminal-receipt-id (:receipt/id terminal)
              :solver/branch (get-in terminal [:solver :branch])
              :solver/head (get-in terminal [:solver :head])
              :workspace/terminal-heads (:workspace/terminal-heads terminal)
              ;; NOT conditional on the pin. The machine retains the solver
              ;; branch for every frame, solved or not, so tying this to a
              ;; successful pin made it report false for partial and void
              ;; frames whose branches are retained -- a receipt asserting
              ;; something untrue, which is the defect the pin exists to fix.
              ;; The verified claim is :solve/pin-status; this field keeps the
              ;; meaning it has always had.
              :branch-retained? true
              :solve/pin-status (:status pin-result)}
        body (cond-> body
               (:ref pin-result) (assoc :solve/pin-ref (:ref pin-result))
               (:reason pin-result) (assoc :solve/pin-reason (:reason pin-result)))]
     (assoc body :receipt/id (machine/ledger-digest [body])))))

(defn- pin-outcome
  [frame terminal-receipt pin-solve-fn]
  (if-not (and (fn? pin-solve-fn)
               (= :solved (:problem/outcome terminal-receipt)))
    {:status :skipped}
    (try
      (let [result (pin-solve-fn frame terminal-receipt)]
        (if (contains? #{:pinned :refused :skipped} (:status result))
          result
          {:status :refused :reason :pin-result-invalid}))
      (catch Throwable _
        {:status :refused :reason :pin-effect-threw}))))

(defn retire!
  "Pin a verified solve, persist terminal metadata, audit, and retire one frame.

  The bank receipt is persisted before workspace retirement. Every audit must
  be supplied by AUDIT-FN and satisfy workspace-lifecycle's full certificate."
  [{:keys [frame terminal-receipt leases audit-fn retire-workspace-fn
           retirement-status-fn persist-bank-fn retire-seats-fn pin-solve-fn]}]
  (let [terminal-check (validate-terminal frame terminal-receipt)]
    (cond
      (not (:ok terminal-check)) terminal-check
      ;; pin-solve-fn is deliberately NOT required. Adding it to this list
      ;; invalidated every existing caller that predates pinning -- the queue
      ;; integration test alone cascaded 21 failures -- and a frame must still
      ;; close where no pinner is wired. Absent, it degrades to :skipped.
      (not (every? fn? [audit-fn retire-workspace-fn retirement-status-fn
                        persist-bank-fn retire-seats-fn]))
      {:ok false :error/code :queued-frame-terminal-provider-missing}
      (not= #{:solver :student} (set (keys leases)))
      {:ok false :error/code :queued-frame-terminal-leases-incomplete}
      :else
      (let [pin-result (pin-outcome frame terminal-receipt pin-solve-fn)
            bank (build-problem-bank frame terminal-receipt pin-result)
            persisted (persist-bank-fn frame bank)]
        (if-not (:ok persisted)
          {:ok false :error/code :queued-frame-bank-persistence-failed}
          (let [retirements
                (reduce
                 (fn [result [role lease]]
                   (if-not (:ok result)
                     (reduced result)
                     (let [status (retirement-status-fn
                                   lease
                                   (get-in terminal-receipt
                                           [:workspace/terminal-heads role]))]
                       (cond
                         (not (:ok status)) (reduced status)
                         (= :complete
                            (phase-status/classify :queued-frame-retirement
                                                   (:status status)))
                         (assoc-in result [:workspace-receipts role]
                                   (:receipt status))
                         (= :unknown
                            (phase-status/classify :queued-frame-retirement
                                                   (:status status)))
                         (reduced {:ok false
                                   :error/code
                                   :queued-frame-retirement-status-vocabulary-incomplete
                                   :role role :status status})
                         :else
                         (let [audit-result
                               (audit-fn frame terminal-receipt role lease)
                               audit (:audit audit-result)]
                           (if-not (:ok audit-result)
                             (reduced
                              (if (= :workspace-retirement-audit-pending
                                     (:error/code audit-result))
                                (assoc audit-result
                                       :status :workspace-retirement-audit-pending
                                       :role role)
                                audit-result))
                             (let [retired (retire-workspace-fn lease audit)]
                               (if (:ok retired)
                                 (assoc-in result [:workspace-receipts role]
                                           (:receipt retired))
                                 (reduced retired)))))))))
                 {:ok true :workspace-receipts {}}
                 (sort-by (comp name key) leases))]
            (if (or (not (:ok retirements))
                    (= :workspace-retirement-audit-pending
                       (:status retirements)))
              retirements
              (let [seats (retire-seats-fn frame terminal-receipt)]
                (if-not (:ok seats)
                  seats
                  {:ok true :bank-receipt bank
                   :workspace-receipts (:workspace-receipts retirements)
                   :seat-retirement seats})))))))))

(defn retire-with-retry!
  "Run one due retirement attempt and durably record its classification.

  RETRY-STATE is the previously persisted value (or nil). PERSIST-RETRY-FN is
  called before a pending, exhausted, resolved, or structural result is
  returned. NOW-MS-FN is injectable so tests do not sleep. A structural audit
  failure is recorded and replayed without another audit attempt."
  [{:keys [retry-state persist-retry-fn now-ms-fn] :as opts}]
  (let [now-ms ((or now-ms-fn #(System/currentTimeMillis)))
        terminal-state? #{:resolved :exhausted :structural-invalid}
        stored-status (:retry/status retry-state)]
    (cond
      (and (terminal-state? stored-status) (:retry/result retry-state))
      (:retry/result retry-state)

      (and (= :pending stored-status)
           (< now-ms (or (:retry/not-before-ms retry-state) 0)))
      {:ok true :status :awaiting-substrate
       :retry/kind :workspace-retirement-audit
       :retry/not-before-ms (:retry/not-before-ms retry-state)
       :retry/attempts (:retry/attempts retry-state)}

      :else
      (let [result (retire! opts)
            pending? (= :workspace-retirement-audit-pending
                        (:error/code result))
            structural? (and (not (:ok result))
                             (= :workspace-retirement-audit-invalid
                                (:error/code result)))
            attempts (vec (or (:retry/attempts retry-state) []))
            attempt (inc (count attempts))
            observation (cond-> {:attempt attempt :observed-at-ms now-ms
                                 :classification (cond pending? :pending
                                                       structural? :structural-invalid
                                                       (:ok result) :resolved
                                                       :else :failure)}
                          (:pending result) (assoc :pending (:pending result))
                          (:role result) (assoc :role (:role result)))
            attempts' (conj attempts observation)
            delay-ms (get retirement-audit-retry-delays-ms (dec attempt))
            [state returned]
            (cond
              (and pending? delay-ms)
              (let [not-before (+ now-ms delay-ms)
                    state {:retry/type :workspace-retirement-audit
                           :retry/status :pending
                           :retry/not-before-ms not-before
                           :retry/attempts attempts'}]
                [state {:ok true :status :awaiting-substrate
                        :retry/kind :workspace-retirement-audit
                        :retry/not-before-ms not-before
                        :pending (:pending result)
                        :retry/attempts attempts'}])

              pending?
              (let [failure {:ok false
                             :error/code :workspace-retirement-audit-retry-exhausted
                             :pending (:pending result)
                             :retry/attempts attempts'}]
                [{:retry/type :workspace-retirement-audit
                  :retry/status :exhausted :retry/attempts attempts'
                  :retry/result failure}
                 failure])

              structural?
              [{:retry/type :workspace-retirement-audit
                :retry/status :structural-invalid :retry/attempts attempts'
                :retry/result result}
               result]

              (:ok result)
              [{:retry/type :workspace-retirement-audit
                :retry/status :resolved :retry/attempts attempts'
                :retry/result result}
               result]

              :else [nil result])]
        (if-not state
          returned
          (let [persisted (persist-retry-fn state)]
            (if (:ok persisted)
              returned
              {:ok false
               :error/code :workspace-retirement-audit-retry-persistence-failed
               :persistence/result persisted})))))))
