(ns futon3c.apm.problem-queue-supervisor-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string]
            [futon3c.apm.problem-queue-supervisor :as sut]))

(def problems
  (mapv (fn [n] {:problem/id (str "p" n) :repository "/repo"
                  :revision (str "revision-" n) :path (str "p" n ".lean")
                  :blob (str "blob-" n) :classification :non-excluded})
        (range 1 6)))

(defn harness []
  (let [plan (sut/queue-plan problems) state (atom nil) calls (atom [])]
    {:plan plan :state state :calls calls
     :providers
     {:plan plan :state-provider #(deref state)
      :persist-state-fn #(do (reset! state %) (swap! calls conj [:persist])
                             {:ok true})
      :mint-frame-fn
      (fn [{:keys [problem ordinal]}]
        (swap! calls conj [:mint (:problem/id problem)])
        {:ok true :frame {:frame/id (str "q" (inc ordinal))
                          :problem/id (:problem/id problem) :problem problem}})
      :qualify-frame-fn #(do (swap! calls conj [:qualify (:frame/id %)])
                             {:ok true})
      :prepare-frame-fn #(do (swap! calls conj [:prepare (:frame/id %)])
                             {:ok true :preparation/id
                              (str "prep-" (:frame/id %))})
      :dispatch-statement-repair-fn
      (fn [handoff]
        (swap! calls conj [:dispatch-repair (:obligation/id handoff)])
        {:ok true :dispatch/id (str "guide-job-" (:frame/id handoff))})
      :observe-statement-repair-fn (constantly {:ok true :status :pending})
      :frame-tick-fn (fn [frame] (swap! calls conj [:tick (:frame/id frame)])
                       {:ok true :status :frame-complete :frame/result :closed
                        :terminal-receipt {:receipt/id
                                           (str "close-" (:frame/id frame))}})
      :retire-frame-fn #(do (swap! calls conj [:retire
                                               (get-in % [:frame :frame/id])])
                            {:ok true})}}))

(deftest five-problem-chain-provisions-only-current-frame
  (let [{:keys [providers state calls]} (harness)]
    (is (= :frame-prepared (:status (sut/tick! providers))))
    (is (= "p1" (get-in @state [:active :frame :problem/id])))
    (is (= 1 (count (filter #(= :prepare (first %)) @calls))))
    (dotimes [_ 4]
      (is (= :frame-prepared (:status (sut/tick! providers))))
      (is (= 1 (- (count (filter #(= :prepare (first %)) @calls))
                    (count (filter #(= :retire (first %)) @calls))))))
    (is (= :batch-complete (:status (sut/tick! providers))))
    (is (= 5 (count (:completed @state))))
    (is (= (:next-index @state) (:frame-ordinal @state)))
    (is (nil? (:active @state)))
    (is (= ["p1" "p2" "p3" "p4" "p5"]
           (mapv :problem/id (:completed @state))))))

(deftest successor-is-not-minted-before-terminal-result
  (let [{:keys [providers calls]} (harness)]
    (is (= :frame-prepared (:status (sut/tick! providers))))
    (let [result (sut/tick! (assoc providers :frame-tick-fn
                                   (constantly {:ok true :status :parked})))]
      (is (= :parked (:status result)))
      (is (= 1 (count (filter #(= :mint (first %)) @calls)))))))

(deftest awaiting-retirement-leaves-active-frame-and-mints-no-successor
  (let [{:keys [providers state calls]} (harness)
        awaiting {:ok false :status :awaiting-substrate
                  :error/code :workspace-retirement-audit-retry-waiting
                  :pending #{:no-running-or-parked-job-references-workspace}}]
    (is (= :frame-prepared (:status (sut/tick! providers))))
    (let [before @state
          result (sut/tick! (assoc providers :retire-frame-fn
                                   (constantly awaiting)))]
      (is (= awaiting result))
      (is (= before @state) "active frame remains authoritative")
      (is (= ["p1"] (mapv second (filter #(= :mint (first %)) @calls))))
      (is (empty? (filter #(= :bank-write (first %)) @calls))))))

(deftest frame-fault-disposition-parks-mundane-and-unknown-errors
  (doseq [failure
          [{:ok false :error/code :live-job-terminal-failure
            :finding {:state :cancelled
                      :terminal-message
                      "Cancelled by http-caller: typed-submission wrapper reconciliation"}}
           {:ok false :error/code :future-unrecognised-frame-failure
            :finding {:detail "preserve me"}}]]
    (let [{:keys [providers state]} (harness)]
      (is (= :frame-prepared (:status (sut/tick! providers))))
      (let [result (sut/tick! (assoc providers :frame-tick-fn
                                     (constantly failure)))
            park (first (:parked @state))]
        (is (= :frame-prepared (:status result)))
        (is (= "p2" (get-in @state [:active :frame :problem/id])))
        (is (= :fault-frame-park (:state/type park)))
        (is (= (:error/code failure) (:error/code park)))
        (is (= failure (:fault/result park)))))))

(deftest fault-classification-reads-only-the-current-result-envelope
  (doseq [[failure expected]
          [[{:ok false :error/code :invalid-state} :campaign-stop]
           [{:ok false :error/code :x
             :findings [:campaign-ledger-digest-mismatch]} :campaign-stop]
           [{:ok false :error/code :live-job-terminal-failure} :frame-park]
           [{:ok false :error/code :live-job-terminal-failure
             :repair-attempt-history
             [{:attempt 1 :error/code :invalid-state}]
             :superseded-terminals
             [{:error/code :impossible-transition}]} :frame-park]]]
    (is (= expected
           (if (sut/fault-frame-park {:frame/id "f" :problem/id "p"}
                                     failure)
             :frame-park
             :campaign-stop))
        (pr-str failure))))

(deftest queue-tick-reconciles-fault-park-decisions
  (let [{:keys [providers state]} (harness)
        failure {:ok false :error/code :mundane-frame-failure}]
    (is (= :frame-prepared (:status (sut/tick! providers))))
    (is (= :frame-prepared
           (:status (sut/tick! (assoc providers :frame-tick-fn
                                      (constantly failure))))))
    (is (= :awaiting-decision
           (get-in @state [:parked 0 :decision/status])))
    (sut/tick!
     (assoc providers :park-decision-records-provider
            (constantly [{:frame/id "q1"
                          :decision/status :decided
                          :decision/disposition :partial}])))
    (is (= :decided (get-in @state [:parked 0 :decision/status])))
    (is (false? (get-in @state [:parked 0 :decision/bell-required])))
    (is (= :partial
           (get-in @state [:parked 0 :decision/record
                           :decision/disposition])))))

(deftest corruption-fault-does-not-park-or-advance
  (let [{:keys [providers state]} (harness)
        failure {:ok false :error/code :campaign-ledger-digest-mismatch
                 :finding {:expected "a" :observed "b"}}]
    (is (= :frame-prepared (:status (sut/tick! providers))))
    (let [before @state
          result (sut/tick! (assoc providers :frame-tick-fn
                                   (constantly failure)))]
      (is (= failure result))
      (is (= before @state))
      (is (empty? (:parked @state))))))

(deftest recorded-campaign-failures-replay-as-frame-parks
  (let [failures
        [{:ok false :error/code :workspace-retirement-audit-invalid
          :findings [:worktree-clean]
          :finding {:workspace/status :dirty}}
         {:ok false :error/code :workspace-retirement-audit-invalid
          :passed #{:frame-terminal :worktree-clean :branch-ref-exists
                    :required-artifacts-content-addressed
                    :no-active-ledger-claim-references-workspace
                    :head-commit-recorded-in-terminal-receipt
                    :independent-retirement-audit-passed}
          :missing #{:no-running-or-parked-job-references-workspace}}
         {:ok false :error/code :live-job-terminal-failure
          :finding {:state :cancelled :terminal-code :operator-cancelled
                    :terminal-message
                    "Cancelled by http-caller: typed-submission wrapper reconciliation"}}]]
    (doseq [[index failure] (map-indexed vector failures)]
      (let [{:keys [providers state]} (harness)]
        (is (= :frame-prepared (:status (sut/tick! providers))))
        (if (< index 2)
          (sut/tick! (assoc providers :retire-frame-fn
                            (constantly failure)))
          (sut/tick! (assoc providers :frame-tick-fn
                            (constantly failure))))
        (let [park (first (:parked @state))]
          (is (= :fault-frame-park (:state/type park)))
          (is (= failure (:fault/result park)))
          (is (= (:error/code failure) (:error/code park)))
          (is (= "p2" (get-in @state [:active :frame :problem/id]))))))))

(deftest ineligible-unit-is-parked-and-next-problem-is-prepared
  (let [{:keys [providers state calls]} (harness)
        result
        (sut/tick!
         (assoc providers :qualify-frame-fn
                (fn [frame]
                  (swap! calls conj [:qualify (:frame/id frame)])
                  (if (= "p1" (:problem/id frame))
                    {:ok false :error/code :queued-frame-eligibility-invalid
                     :findings [:countdown-manifest-eligibility-shape-invalid]
                     :manifest/id "bad-manifest"}
                    {:ok true}))))]
    (is (= :frame-prepared (:status result)))
    (is (= "p2" (get-in @state [:active :frame :problem/id])))
    (is (= 2 (:next-index @state)))
    (is (= [{:state/type :eligibility-frame-park
             :frame/id "q1" :problem/id "p1"
             :decision/status :parked
             :park/reason :problem-ineligible
             :qualification
             {:ok false :error/code :queued-frame-eligibility-invalid
              :findings [:countdown-manifest-eligibility-shape-invalid]
              :manifest/id "bad-manifest"}}]
           (:parked @state)))
    (is (= [[:mint "p1"] [:qualify "q1"] [:persist]
            [:mint "p2"] [:qualify "q2"] [:prepare "q2"] [:persist]]
           @calls))))

(deftest solver-round-exhaustion-parks-frame-and-prepares-successor
  (let [{:keys [providers state calls]} (harness)
        park {:state/type :solver-human-intervention-frame-park
              :decision/owner :claude-supervisor
              :decision/status :awaiting-decision
              :decision/bell-required true
              :frame/id "q1" :problem/id "p1"
              :solver/rounds-completed 50
              :solver/final-head "head-50"
              :solver/branch "exp/q1-solver"
              :solver/state-path "/campaign/q1/live/solve.edn"
              :last-valid-receipt/id "receipt-49"
              :residual "prove the missing Jordan ordering theorem"
              :student/decision :claude-required}]
    (is (= :frame-prepared (:status (sut/tick! providers))))
    (let [result
          (sut/tick!
           (assoc providers :frame-tick-fn
                  (constantly {:ok true :status :frame-parked
                               :frame/park park})))]
      (is (= :frame-prepared (:status result)))
      (is (= "p2" (get-in @state [:active :frame :problem/id])))
      (is (= [park] (:parked @state)))
      (is (empty? (filter #(= :retire (first %)) @calls)))
      (is (= ["p1" "p2"]
             (mapv second (filter #(= :mint (first %)) @calls)))))))

(deftest invalid-frame-park-does-not-advance-queue
  (let [{:keys [providers state calls]} (harness)]
    (sut/tick! providers)
    (let [before @state
          result (sut/tick!
                  (assoc providers :frame-tick-fn
                         (constantly {:ok true :status :frame-parked
                                      :frame/park
                                      {:state/type
                                       :solver-human-intervention-frame-park
                                       :frame/id "q1" :problem/id "p1"}})))]
      (is (= :problem-queue-frame-park-invalid (:error/code result)))
      (is (= before @state))
      (is (= 1 (count (filter #(= :mint (first %)) @calls)))))))

(deftest scribe-apparatus-park-preserves-record-and-prepares-successor
  (let [{:keys [providers state calls]} (harness)
        park {:state/type :scribe-reduce-apparatus-frame-park
              :decision/owner :claude-supervisor
              :decision/status :awaiting-decision
              :decision/bell-required true
              :frame/id "q1" :problem/id "p1" :phase :scribe-reduce
              :promotion/state-path "/campaign/q1/live/scribe-reduce.edn"
              :last-valid-receipt/id "student-attempt-3"
              :error/code :promotion-deposit-retries-exhausted
              :deposit/attempts 3
              :deposit/findings [{:ordinal 1
                                  :findings [:candidate-body-missing]}]
              :residual "candidate body missing"}]
    (sut/tick! providers)
    (let [result (sut/tick!
                  (assoc providers :frame-tick-fn
                         (constantly {:ok true :status :frame-parked
                                      :frame/park park})))]
      (is (= :frame-prepared (:status result)))
      (is (= "p2" (get-in @state [:active :frame :problem/id])))
      (is (= [park] (:parked @state)))
      (is (empty? (filter #(= :retire (first %)) @calls))))))

(deftest promotion-apparatus-park-preserves-record-and-prepares-successor
  (let [{:keys [providers state calls]} (harness)
        park {:state/type :promotion-apparatus-frame-park
              :decision/owner :claude-supervisor
              :decision/status :awaiting-decision
              :decision/bell-required true
              :frame/id "q1" :problem/id "p1" :phase :promotion
              :promotion/state-path "/campaign/q1/live/promote-solver.edn"
              :last-valid-receipt/id "solver-receipt"
              :error/code :promotion-apparatus-repair-exhausted
              :repair/kind :review-projection :repair/attempts 1
              :promotion/findings [{:failure :edge-write-failed}]
              :persisted-review-result
              {:review-job "terminal-review"
               :reviews [{:memory-id "m" :verdict :approve}]}
              :residual "edge write failed"}]
    (sut/tick! providers)
    (let [result (sut/tick!
                  (assoc providers :frame-tick-fn
                         (constantly {:ok true :status :frame-parked
                                      :frame/park park})))]
      (is (= :frame-prepared (:status result)))
      (is (= "p2" (get-in @state [:active :frame :problem/id])))
      (is (= [park] (:parked @state)))
      (is (empty? (filter #(= :retire (first %)) @calls))))))

(deftest role-terminal-repair-park-preserves-record-and-prepares-successor
  (let [{:keys [providers state calls]} (harness)
        park {:state/type :role-terminal-repair-frame-park
              :decision/owner :claude-supervisor
              :decision/status :awaiting-decision
              :decision/bell-required true
              :frame/id "q1" :problem/id "p1"
              :phase :guide-intervention-1
              :role/state-path "/campaign/q1/live/guide-intervention-1.edn"
              :last-valid-receipt/id "student-attempt-1-receipt"
              :error/code :live-job-terminal-repair-exhausted
              :repair/kind :terminal-submission :repair/attempts 1
              :role/findings [:typed-submission-missing]
              :residual "[:typed-submission-missing]"}]
    (sut/tick! providers)
    (let [result (sut/tick!
                  (assoc providers :frame-tick-fn
                         (constantly {:ok true :status :frame-parked
                                      :frame/park park})))]
      (is (= :frame-prepared (:status result)))
      (is (= "p2" (get-in @state [:active :frame :problem/id])))
      (is (= [park] (:parked @state)))
      (is (empty? (filter #(= :retire (first %)) @calls))))))

(defn role-terminal-void [frame]
  {:ok true :status :frame-complete :frame/result :void
   :terminal-receipt
   {:receipt/id (str "void-" (:frame/id frame))
    :frame/result :void :problem/outcome :unsolved
    :void/classification :role-terminal-unrecoverable
    :void/failed-invariants
    [:live-job-terminal-repair-exhausted :typed-submission-missing]}})

(defn role-terminal-park [frame]
  {:ok true :status :frame-parked
   :frame/park {:state/type :role-terminal-repair-frame-park
                :decision/owner :claude-supervisor
                :decision/status :awaiting-decision
                :decision/bell-required true
                :frame/id (:frame/id frame) :problem/id (:problem/id frame)
                :phase :student-attempt-1
                :role/state-path (str "/campaign/" (:frame/id frame)
                                      "/live/student-attempt-1.edn")
                :last-valid-receipt/id "promote-solver-receipt"
                :error/code :live-job-terminal-repair-exhausted
                :repair/kind :terminal-submission :repair/attempts 1
                :role/findings [:live-job-terminal-repair-exhausted
                                :typed-submission-missing]
                :residual "[:typed-submission-missing]"}})

(deftest role-terminal-park-records-streak-and-advances
  (let [{:keys [providers state]} (harness)]
    (is (= :frame-prepared (:status (sut/tick! providers))))
    (let [result (sut/tick! (assoc providers
                                   :frame-tick-fn role-terminal-park))]
      (is (= :frame-prepared (:status result)))
      (is (= "p2" (get-in @state [:active :frame :problem/id])))
      (is (= 1 (get-in @state [:consecutive-frame-failures :count])))
      (is (= {:classification :role-terminal-unrecoverable
              :failed-invariants [:live-job-terminal-repair-exhausted
                                  :typed-submission-missing]}
             (get-in @state [:consecutive-frame-failures :signature])))
      (is (= 1 (count (:parked @state)))))))

(deftest third-consecutive-identical-role-terminal-park-is-campaign-fatal
  ;; The systematic brake survives the 2026-09-06 park-not-void ruling:
  ;; a broken role seat must stop the queue, not drain it frame by frame.
  (let [{:keys [providers state calls]} (harness)
        failing (assoc providers :frame-tick-fn role-terminal-park)]
    (is (= :frame-prepared (:status (sut/tick! providers))))
    (is (= :frame-prepared (:status (sut/tick! failing))))
    (is (= :frame-prepared (:status (sut/tick! failing))))
    (let [result (sut/tick! failing)]
      (is (= :problem-queue-systematic-frame-failure (:error/code result)))
      (is (= sut/systematic-frame-failure-limit
             (get-in result [:failure :count])))
      (is (= :failed-systematic-frame-failure (:status @state)))
      (is (nil? (:active @state)))
      (is (= 3 (count (:parked @state)))
          "the third park is still durably recorded")
      (is (= 3 (count (filter #(= :mint (first %)) @calls)))
          "the fourth problem is not minted"))))

(deftest park-streak-continues-a-void-streak-with-the-same-signature
  (let [{:keys [providers state]} (harness)]
    (sut/tick! providers)
    (sut/tick! (assoc providers :frame-tick-fn role-terminal-void))
    (is (= 1 (get-in @state [:consecutive-frame-failures :count])))
    (sut/tick! (assoc providers :frame-tick-fn role-terminal-park))
    (is (= 2 (get-in @state [:consecutive-frame-failures :count]))
        "identical invariants count across the void->park transition")))

(deftest unrelated-park-leaves-role-terminal-streak-untouched
  (let [{:keys [providers state]} (harness)
        fault-park (fn [frame]
                     {:ok true :status :frame-parked
                      :frame/park {:state/type :fault-frame-park
                                   :frame/id (:frame/id frame)
                                   :problem/id (:problem/id frame)
                                   :error/code :frame-tick-threw
                                   :fault/disposition :frame-park
                                   :fault/result {:ok false}
                                   :residual "boom"
                                   :decision/owner :claude-supervisor
                                   :decision/status :awaiting-decision
                                   :decision/bell-required true}})]
    (sut/tick! providers)
    (sut/tick! (assoc providers :frame-tick-fn role-terminal-park))
    (is (= 1 (get-in @state [:consecutive-frame-failures :count])))
    (sut/tick! (assoc providers :frame-tick-fn fault-park))
    (is (= 1 (get-in @state [:consecutive-frame-failures :count]))
        "an unrelated fault park neither extends nor resets the streak")))

(deftest isolated-role-terminal-void-advances-and-records-durable-streak
  (let [{:keys [providers state]} (harness)]
    (is (= :frame-prepared (:status (sut/tick! providers))))
    (let [result (sut/tick! (assoc providers
                                   :frame-tick-fn role-terminal-void))]
      (is (= :frame-prepared (:status result)))
      (is (= "p2" (get-in @state [:active :frame :problem/id])))
      (is (= 1 (get-in @state [:consecutive-frame-failures :count])))
      (is (= "q1" (get-in @state
                           [:consecutive-frame-failures :last-frame-id])))
      (is (= {:frame/id "q1" :problem/id "p1" :frame/result :void
              :void/classification :role-terminal-unrecoverable
              :void/failed-invariants
              [:live-job-terminal-repair-exhausted :typed-submission-missing]
              :terminal-receipt/id "void-q1"}
             (first (:dispositions @state)))))))

(deftest third-consecutive-identical-frame-failure-is-campaign-fatal
  (let [{:keys [providers state calls]} (harness)
        failing (assoc providers :frame-tick-fn role-terminal-void)]
    (is (= :frame-prepared (:status (sut/tick! providers))))
    (is (= :frame-prepared (:status (sut/tick! failing))))
    (is (= :frame-prepared (:status (sut/tick! failing))))
    (let [result (sut/tick! failing)]
      (is (= :problem-queue-systematic-frame-failure (:error/code result)))
      (is (= sut/systematic-frame-failure-limit
             (get-in result [:failure :count])))
      (is (= :failed-systematic-frame-failure (:status @state)))
      (is (nil? (:active @state)))
      (is (= 3 (count (filter #(= :mint (first %)) @calls)))
          "the fourth problem is not minted"))))

(deftest successful-frame-resets-consecutive-failure-streak
  (let [{:keys [providers state]} (harness)
        failing (assoc providers :frame-tick-fn role-terminal-void)]
    (sut/tick! providers)
    (sut/tick! failing)
    (is (= 1 (get-in @state [:consecutive-frame-failures :count])))
    (sut/tick! providers)
    (is (nil? (:consecutive-frame-failures @state)))
    (is (= "p3" (get-in @state [:active :frame :problem/id])))))

(deftest pause-after-active-retires-current-frame-without-minting-successor
  (let [{:keys [providers state calls]} (harness)]
    (is (= :frame-prepared (:status (sut/tick! providers))))
    (let [requested (sut/pause-after-active @state)]
      (is (:ok requested))
      ((:persist-state-fn providers) (:state requested)))
    (let [result (sut/tick! providers)]
      (is (= :batch-paused (:status result)))
      (is (= :paused (:status @state)))
      (is (nil? (:active @state)))
      (is (= 1 (:next-index @state)))
      (is (= 1 (count (filter #(= :mint (first %)) @calls))))
      (is (= 1 (count (:completed @state)))))))

(deftest resume-paused-preserves-cursor-and-mints-exact-successor
  (let [{:keys [providers state calls]} (harness)]
    (is (= :frame-prepared (:status (sut/tick! providers))))
    ((:persist-state-fn providers)
     (:state (sut/pause-after-active @state)))
    (is (= :batch-paused (:status (sut/tick! providers))))
    (let [paused @state
          resumed (sut/resume-paused paused)]
      (is (:ok resumed))
      (is (= (:next-index paused) (get-in resumed [:state :next-index])))
      (is (= (:completed paused) (get-in resumed [:state :completed])))
      (is (nil? (get-in resumed [:state :active])))
      ((:persist-state-fn providers) (:state resumed)))
    (is (= :frame-prepared (:status (sut/tick! providers))))
    (is (= "p2" (get-in @state [:active :frame :problem/id])))
    (is (= ["p1" "p2"]
           (mapv second (filter #(= :mint (first %)) @calls))))))

(deftest durable-intermediate-collection-statuses-remain-nonterminal
  (doseq [status [:awaiting-substrate :transport-retry-scheduled
                  :terminal-collected :claim-recovered]]
    (let [{:keys [providers calls]} (harness)]
      (sut/tick! providers)
      (let [result (sut/tick! (assoc providers :frame-tick-fn
                                     (constantly {:ok true :status status})))]
        (is (= status (:status result)))
        (is (= 1 (count (filter #(= :mint (first %)) @calls))))))))

(deftest same-problem-retry-clears-active-without-advancing-or-minting
  (let [{:keys [providers state calls]} (harness)
        _ (sut/tick! providers)
        before-index (:next-index @state)
        receipt {:receipt/id (apply str (repeat 64 "a"))
                 :frame/result :partial :problem/outcome :unsolved
                 :retry/same-problem? true}
        result (sut/complete-active-without-successor @state receipt)]
    (is (:ok result))
    (is (= before-index (get-in result [:state :next-index])))
    (is (nil? (get-in result [:state :active])))
    (is (= :retry-superseded (get-in result [:state :status])))
    (is (= 1 (count (filter #(= :mint (first %)) @calls))))))

(deftest void-erases-frame-and-recasts-same-queue-slot
  (let [{:keys [providers state]} (harness)
        _ (sut/tick! providers)
        void-result
        (sut/tick!
         (assoc providers :frame-tick-fn
                (constantly
                 {:ok true :status :frame-complete :frame/result :void
                  :terminal-receipt {:receipt/id "void-q1"
                                     :problem/outcome :refuted}})))
        replacement (assoc (first problems)
                           :revision "corrected-revision"
                           :blob "corrected-blob")
        guide-receipt {:repair/role :guide
                       :obligation/id (get-in @state
                                              [:statement-repair/handoff
                                               :obligation/id])
                       :receipt/id (apply str (repeat 64 "d"))}
        revised (sut/revise-voided-slot (:plan providers) @state replacement
                                        guide-receipt)]
    (is (= :guide-statement-repair-dispatched (:status void-result)))
    (is (= :guide (get-in @state [:statement-repair/handoff :repair/role])))
    (is (= :dispatched
           (get-in @state [:statement-repair/handoff :dispatch/status])))
    (is (nil? (:active @state)))
    (is (empty? (:completed @state)))
    (is (= 0 (:next-index @state)))
    (is (:ok revised) (pr-str revised))
    (is (= "p1" (get-in revised [:plan :problems 0 :problem/id])))
    (is (= "corrected-blob" (get-in revised [:plan :problems 0 :blob])))
    (is (not= (:queue/id (:plan providers))
              (get-in revised [:plan :queue/id])))
    (is (= 1 (get-in revised [:state :statement-repair-attempts "p1"])))
    (reset! state (:state revised))
    (is (= :frame-prepared
           (:status (sut/tick! (assoc providers :plan (:plan revised))))))
    (is (= "p1" (get-in @state [:active :frame :problem/id])))
    (is (= :frame-prepared
           (:status
            (sut/tick!
             (assoc providers :plan (:plan revised)
                    :frame-tick-fn
                    (constantly
                     {:ok true :status :frame-complete :frame/result :void
                      :terminal-receipt {:receipt/id "void-q1-repair"
                                         :problem/outcome :refuted}}))))))
    (is (= "p2" (get-in @state [:active :frame :problem/id])))
    (is (= 2 (:next-index @state)))
    (is (empty? (:completed @state)))))

(deftest revised-voided-slot-retries-in-a-new-frame
  (let [{:keys [providers state]} (harness)
        _ (sut/tick! providers)
        _ (sut/tick!
           (assoc providers :frame-tick-fn
                  (constantly
                   {:ok true :status :frame-complete :frame/result :void
                    :terminal-receipt {:receipt/id "void-q1"
                                       :problem/outcome :refuted}})))
        voided-frame-id (get-in @state [:statement-repair/handoff :frame/id])
        obligation-id
        "d6fd972b4c8b2dd896da1fde27e5ddb5536d78914a38729d0de77935beacb6e6"
        live-state (#'sut/addressed
                    (-> @state
                        ;; f203 predates :frame-ordinal; exercise the persisted
                        ;; campaign backfill, not merely a fresh queue.
                        (dissoc :frame-ordinal)
                        (assoc-in [:statement-repair/handoff :obligation/id]
                                  obligation-id)))
        replacement (assoc (first problems)
                           :revision
                           "9d22c1aba6d65bc9147a67ee5b6c19697fc52e17"
                           :blob
                           "640eb44b9bcf052931f787825019e6a7e3e3b799")
        receipt {:repair/role :guide
                 :obligation/id obligation-id
                 :receipt/id
                 "b3c363a9674c1aa4c096d7c68903f09dbfc8f9d3178b72792b5c6d37b0a21926"}
        revised (sut/revise-voided-slot (:plan providers) live-state
                                        replacement receipt)]
    (is (:ok revised) (pr-str revised))
    (is (= 0 (get-in revised [:state :next-index])))
    (is (= "p1" (get-in revised [:plan :problems 0 :problem/id])))
    (let [prepared (#'sut/prepare-next (:plan revised) (:state revised)
                                       (:providers (assoc (harness)
                                                          :plan (:plan revised))))]
      (is (:ok prepared) (pr-str prepared))
      (is (not= voided-frame-id (get-in prepared [:frame :frame/id])))
      (is (= "p1" (get-in prepared [:frame :problem/id])))
      (is (= (:revision replacement)
             (get-in prepared [:frame :problem :revision])))
      (is (= (:blob replacement)
             (get-in prepared [:frame :problem :blob]))))))

(deftest only-guide-may-author-the-single-statement-repair
  (let [{:keys [providers state]} (harness)
        _ (sut/tick! providers)
        _ (sut/tick! (assoc providers :frame-tick-fn
                            (constantly
                             {:ok true :status :frame-complete
                              :frame/result :void
                              :terminal-receipt {:receipt/id "void-q1"
                                                 :problem/outcome :refuted}})))
        replacement (assoc (first problems) :blob "corrected-blob")]
    (is (= :problem-queue-guide-repair-receipt-invalid
           (:error/code
            (sut/revise-voided-slot
             (:plan providers) @state replacement
             {:repair/role :solver
              :obligation/id (get-in @state
                                     [:statement-repair/handoff :obligation/id])
              :receipt/id (apply str (repeat 64 "e"))}))))))

(deftest awaiting-repair-never-remints-and-retries-a-persisted-dispatch
  (let [{:keys [providers state calls]} (harness)
        _ (sut/tick! providers)
        failed (sut/tick!
                (assoc providers
                       :dispatch-statement-repair-fn (constantly {:ok false})
                       :frame-tick-fn
                       (constantly
                        {:ok true :status :frame-complete :frame/result :void
                         :terminal-receipt {:receipt/id "void-q1"
                                            :problem/outcome :refuted}})))
        pending-state @state
        mint-count (count (filter #(= :mint (first %)) @calls))
        retried (sut/tick! providers)]
    (is (= :problem-queue-guide-dispatch-failed (:error/code failed)))
    (is (= :voided-slot-awaiting-revision (:status pending-state)))
    (is (= :pending
           (get-in pending-state [:statement-repair/handoff :dispatch/status])))
    (is (= :guide-statement-repair-dispatched (:status retried)))
    (is (= mint-count (count (filter #(= :mint (first %)) @calls))))))

(deftest completed-guide-handoff-installs-repair-and-remints-without-human-step
  (let [{:keys [providers state]} (harness)
        _ (sut/tick! providers)
        _ (sut/tick!
           (assoc providers :frame-tick-fn
                  (constantly
                   {:ok true :status :frame-complete :frame/result :void
                    :terminal-receipt {:receipt/id "void-q1"
                                       :problem/outcome :refuted}})))
        obligation-id (get-in @state
                              [:statement-repair/handoff :obligation/id])
        replacement (assoc (first problems)
                           :revision "guide-revision" :blob "guide-blob")
        result (sut/tick!
                (assoc providers :observe-statement-repair-fn
                       (constantly
                        {:ok true :status :complete
                         :replacement-pinned-problem replacement
                         :guide-receipt
                         {:repair/role :guide :obligation/id obligation-id
                          :receipt/id (apply str (repeat 64 "f"))}})))]
    (is (= :frame-prepared (:status result)))
    (is (= "guide-blob"
           (get-in result [:frame :problem :blob])))
    (is (= 1 (get-in @state [:statement-repair-attempts "p1"])))
    (is (nil? (:statement-repair/handoff @state)))))

(deftest queue-plan-preserves-explicit-retained-branch
  (let [problem (assoc (first problems) :base-branch "exp/retained")]
    (is (= "exp/retained"
           (get-in (sut/queue-plan [problem]) [:problems 0 :base-branch])))))

(deftest queue-and-terminal-invariants-fail-closed
  (testing "duplicate problem"
    (let [plan (sut/queue-plan [(first problems) (first problems)])]
      (is (= :problem-queue-invalid
             (:error/code (sut/tick! (assoc (:providers (harness))
                                           :plan plan)))))))
  (testing "nonterminal completion result"
    (let [{:keys [providers]} (harness)]
      (sut/tick! providers)
      (is (= :problem-queue-terminal-result-invalid
             (:error/code
              (sut/tick! (assoc providers :frame-tick-fn
                                (constantly {:ok true :status :frame-complete
                                             :frame/result :running})))))))))

(deftest guide-receipt-validity-is-one-question-with-one-answer
  ;; Live pin: the receipt f199's guide actually returned, 2026-09-08. It
  ;; carries :terminal-receipt/id and no :receipt/id -- which is exactly what
  ;; its prompt asked for, since that prompt required only that the receipt
  ;; repeat :obligation/id. The observer accepted it, the supervisor rejected
  ;; it, and the rejection faulted the coordinator on every tick.
  (let [obligation "d731694d1ffb909a8010a4035de81e9382452e2b9bb3df7b573c6bbbf8f506fa"
        f199-receipt {:obligation/id obligation
                      :repair/role :guide
                      :repair/attempt 1
                      :repair/max-attempts 1
                      :repair/status :completed
                      :frame/id "f199"
                      :problem/id "m01J05"
                      :terminal-receipt/id
                      "4bfde3eda1076303649fb9e1d12e58fe6879ac84e4c8ad092b790281a245a75f"
                      :invariant-repaired :statement-refuted-by-solver}]
    (is (not (sut/valid-guide-receipt? obligation f199-receipt))
        "a 64-hex id under :terminal-receipt/id is not a repair receipt")
    (is (sut/valid-guide-receipt?
         obligation
         (assoc f199-receipt :receipt/id
                "4bfde3eda1076303649fb9e1d12e58fe6879ac84e4c8ad092b790281a245a75f"))
        "the same receipt discharges the obligation once it carries :receipt/id")
    (testing "each requirement is load-bearing"
      (let [good (assoc f199-receipt :receipt/id
                        "4bfde3eda1076303649fb9e1d12e58fe6879ac84e4c8ad092b790281a245a75f")]
        (is (not (sut/valid-guide-receipt? obligation (dissoc good :repair/role))))
        (is (not (sut/valid-guide-receipt? obligation (assoc good :repair/role :solver))))
        (is (not (sut/valid-guide-receipt? "other-obligation" good)))
        (is (not (sut/valid-guide-receipt? obligation (assoc good :receipt/id "SHORT"))))
        (is (not (sut/valid-guide-receipt? obligation (assoc good :receipt/id
                                                            (clojure.string/upper-case
                                                             (:receipt/id good)))))
            "uppercase hex is not the pinned form")
        (is (not (sut/valid-guide-receipt? obligation nil)))))))
