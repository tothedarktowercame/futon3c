(ns futon3c.agency.parked-on-test
  "E-repl-continuations INSTANTIATE — the five VERIFY cases as executable tests
   (§5), plus the core join semantics. No live JVM / no :7071: the ns is
   dependency-injected (resume!/ledger-lookup/now-ms)."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [futon3c.agency.parked-on :as p]))

(use-fixtures :each (fn [t] (p/clear!) (t)))

(defn- collector [] (atom []))
(defn- resume-into [a] (fn [rec] (swap! a conj (:id rec))))

(deftest join-waits-for-all-deps
  (testing "a 2-dep join resumes ONCE, only after BOTH deps are terminal"
    (let [fired (collector)
          r (p/park! {:agent "claude-1" :awaiting ["b1" "b2"] :payload "P"}
                     {:ledger-lookup (constantly nil) :resume! (resume-into fired) :now-ms 1000})]
      (is (= :parked (:status r)))
      (p/note-completion! "b1" "sum1" {:resume! (resume-into fired) :now-ms 1001})
      (is (= [] @fired) "no resume until the second dep arrives")
      (p/note-completion! "b2" "sum2" {:resume! (resume-into fired) :now-ms 1002})
      (is (= [(:id r)] @fired) "resume fires once, when the join completes")
      (is (nil? (get-in (p/snapshot) [:records (:id r)])) "record retracted after release"))))

(deftest case1-lost-wakeup-reconciled-at-park
  (testing "a dep already terminal in the ledger at park time -> immediate resume"
    (let [fired (collector)
          ledger {"b1" {:state "processed" :result-summary "done"}}
          r (p/park! {:agent "claude-1" :awaiting ["b1"] :payload "P"}
                     {:ledger-lookup ledger :resume! (resume-into fired) :now-ms 1000})]
      (is (= :released (:status r)) "park reconciled the already-finished dep")
      (is (= [(:id r)] @fired)))))

(deftest all-durable-inflight-states-remain-parked
  (doseq [state ["announced" "accepted" "queued" "activating" "running"
                 "overrun" "invoking" "parked" :activating :overrun]]
    (p/clear!)
    (let [fired (collector)
          result (p/park! {:agent "codex-10" :awaiting ["job-1"] :payload "P"}
                          {:ledger-lookup (constantly {:state state})
                           :resume! (resume-into fired) :now-ms 1000})]
      (is (= :parked (:status result)) (str state))
      (is (empty? @fired) (str state)))))

(deftest reconcile-prefers-full-result-and-falls-back-for-legacy-jobs
  (testing "already-terminal jobs deliver :result; absent/blank results use :result-summary"
    (doseq [[job expected] [[{:state "done" :result "the complete reply"
                              :result-summary "short preview"}
                             "the complete reply"]
                            [{:state "done" :result nil
                              :result-summary "legacy summary"}
                             "legacy summary"]
                            [{:state "done" :result "   "
                              :result-summary "blank fallback"}
                             "blank fallback"]]]
      (p/clear!)
      (let [resumed (atom nil)]
        (p/park! {:agent "claude-1" :awaiting ["b1"] :payload "P"}
                 {:ledger-lookup (constantly job)
                  :resume! #(reset! resumed %)
                  :now-ms 1000})
        (is (= expected (get-in @resumed [:arrived "b1"])))))))

(deftest case2-double-delivery-fires-once
  (testing "the same dep completing twice resumes exactly once (index-consumption + single-fire)"
    (let [fired (collector)
          r (p/park! {:agent "claude-1" :awaiting ["b1"] :payload "P"}
                     {:ledger-lookup (constantly nil) :resume! (resume-into fired) :now-ms 1000})]
      (p/note-completion! "b1" "s" {:resume! (resume-into fired) :now-ms 1001})
      (p/note-completion! "b1" "s-again" {:resume! (resume-into fired) :now-ms 1002})
      (is (= [(:id r)] @fired) "second delivery of the same dep is a no-op"))))

(deftest completion-consumes-only-matching-park
  (testing "one dependency wake leaves the same agent's unrelated park live"
    (let [resumed (atom [])
          resume! #(swap! resumed conj %)
          first-park (p/park! {:agent "claude-1" :awaiting ["job-1"]
                               :payload "first"}
                              {:ledger-lookup (constantly nil)
                               :resume! resume! :now-ms 1000})
          second-park (p/park! {:agent "claude-1" :awaiting ["job-2"]
                                :payload "second"}
                               {:ledger-lookup (constantly nil)
                                :resume! resume! :now-ms 1000})]
      (p/note-completion! "job-1" "result-1"
                          {:resume! resume! :now-ms 1001})
      (is (= [(:id first-park)] (mapv :id @resumed)))
      (is (= "result-1" (get-in (first @resumed) [:arrived "job-1"])))
      (is (nil? (get-in (p/snapshot) [:records (:id first-park)])))
      (is (= #{"job-2"}
             (get-in (p/snapshot) [:records (:id second-park) :awaiting]))
          "the unrelated park remains listed and indexed")
      (is (= #{(:id second-park)}
             (get-in (p/snapshot) [:index "job-2"]))))))

(deftest reparking-running-dependency-replaces-instead-of-stacking
  (testing "duplicate [agent job] parks share one record and one delivery"
    (let [resumed (atom [])
          resume! #(swap! resumed conj %)
          first-park (p/park! {:agent "claude-1" :awaiting ["job-1"]
                               :payload "old payload"}
                              {:ledger-lookup (constantly {:state "running"})
                               :resume! resume! :now-ms 1000})
          replacement (p/park! {:agent "claude-1" :awaiting ["job-1"]
                                :payload "new payload"}
                               {:ledger-lookup (constantly {:state "running"})
                                :resume! resume! :now-ms 1001})]
      (is (= (:id first-park) (:id replacement)))
      (is (= 1 (count (:records (p/snapshot)))))
      (p/note-completion! "job-1" "done" {:resume! resume! :now-ms 1002})
      (p/note-completion! "job-1" "done-again" {:resume! resume! :now-ms 1003})
      (is (= 1 (count @resumed)))
      (is (= "new payload" (:payload (first @resumed)))))))

(deftest parking-completed-dependency-delivers-once
  (testing "immediate reconcile fires once and a duplicate park does not stack"
    (let [resumed (atom [])
          resume! #(swap! resumed conj %)
          ledger (constantly {:state "done" :result "complete payload"})
          first-park (p/park! {:agent "claude-1" :awaiting ["job-1"]
                               :payload "continue"}
                              {:ledger-lookup ledger :resume! resume! :now-ms 1000})
          duplicate (p/park! {:agent "claude-1" :awaiting ["job-1"]
                              :payload "continue again"}
                             {:ledger-lookup ledger :resume! resume! :now-ms 1001})]
      (is (= :released (:status first-park)))
      (is (= :released (:status duplicate)))
      (is (= (:id first-park) (:id duplicate)))
      (is (= 1 (count @resumed)))
      (is (= "complete payload"
             (get-in (first @resumed) [:arrived "job-1"]))))))

(deftest case3-budget-exhaustion-retracts-no-fire
  (testing "a join that completes with no budget left is retracted, never resumed, no zombie"
    (let [fired (collector)
          r (p/park! {:agent "claude-1" :awaiting ["b1"] :payload "P" :budget {:resumes-left 0}}
                     {:ledger-lookup (constantly nil) :resume! (resume-into fired) :now-ms 1000})]
      (p/note-completion! "b1" "s" {:resume! (resume-into fired) :now-ms 1001})
      (is (= [] @fired) "exhausted budget -> no resume")
      (is (nil? (get-in (p/snapshot) [:records (:id r)])) "record retracted, not left awaiting forever"))))

(deftest case4-teardown-rehydrate-reconciles
  (testing "parked across a teardown: a dep that finished during downtime releases on boot"
    (let [fired (collector)]
      ;; park while the dep is in-flight -> stays parked, persisted to disk
      (p/park! {:agent "claude-1" :awaiting ["b1"] :payload "P"}
               {:ledger-lookup (constantly {:state "running"})
                :resume! (resume-into fired) :now-ms 1000})
      (is (= [] @fired))
      ;; boot: recover-inflight-jobs marked the lost worker :failed -> counts as arrived
      (let [ledger {"b1" {:state "failed" :result-summary "worker-lost-on-restart"}}
            res (p/rehydrate! {:ledger-lookup ledger :resume! (resume-into fired) :now-ms 2000})]
        (is (= 1 (:loaded res)))
        (is (= 1 (count @fired)) "join completed during downtime -> one resume on boot")))))

(deftest case5-deadline-expiry-wakes-with-flag-and-timer-fires
  (testing "a past-deadline record RESUMES with :deadline-expired? true (backstop
wake, E-park-delivery-losses finding 6 — previously it expired silently); a due
no-dep timer park resumes plainly"
    (let [resumed (atom [])
          expired (collector)
          resume! (fn [r] (swap! resumed conj [(:id r) (boolean (:deadline-expired? r))]))
          dl (p/park! {:agent "claude-1" :awaiting ["b1"] :payload "P" :deadline-ms 500}
                      {:ledger-lookup (constantly nil) :resume! resume! :now-ms 1000})
          tm (p/park! {:agent "claude-1" :awaiting [] :payload "T" :timer-due-ms 400}
                      {:ledger-lookup (constantly nil) :resume! resume! :now-ms 1000})
          r (p/sweep-deadlines! {:now-ms 1000 :resume! resume!
                                 :on-expire (resume-into expired)})]
      (is (= [(:id dl)] (:expired r)) "past-deadline record expired")
      (is (= [(:id tm)] (:timer-fired r)) "due timer park fired")
      (is (= [(:id dl)] @expired) "on-expire called for the deadline record")
      (is (= [[(:id dl) true] [(:id tm) false]] @resumed)
          "expired park resumed WITH the deadline flag; timer park resumed without"))))

(deftest no-deps-no-timer-resumes-immediately
  (testing "parking on an empty join with no timer is a degenerate immediate resume"
    (let [fired (collector)
          r (p/park! {:agent "claude-1" :awaiting [] :payload "P"}
                     {:ledger-lookup (constantly nil) :resume! (resume-into fired) :now-ms 1000})]
      (is (= :released-immediately (:status r)))
      (is (= [(:id r)] @fired)))))

(deftest park-requires-canonical-internal-timestamps
  (doseq [[field value] [[:timer-due-ms "400"]
                         [:deadline-ms 1.5]
                         [:timer-due-ms -1]]]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"must be a canonical non-negative integer"
         (p/park! (assoc {:agent "codex-10" :awaiting [] :payload "P"}
                         field value)
                  {:ledger-lookup (constantly nil) :now-ms 1000})))))

;; ---------------------------------------------------------------------------;;
;; E-park-delivery-losses bugs 2-3: lease / ack / redelivery + busy-withhold
;; ---------------------------------------------------------------------------;;

(deftest ready-push-then-lease-one
  (testing "ready-push! enqueues FIFO; ready-lease-one! pops one and marks it leased"
    (p/ready-push! "a1" "s1" "pk-1" "prompt-1")
    (p/ready-push! "a1" "s1" "pk-2" "prompt-2")
    (is (p/ready-inbox-pending? "a1" "s1"))
    (let [item (p/ready-lease-one! "a1" "s1" 1000 90000)]
      (is (= "pk-1" (:park-id item)) "FIFO: first pushed is first leased")
      (is (= "prompt-1" (:prompt item)))
      (is (= 91000 (:lease-deadline-ms item)) "deadline = now + lease-ms")
      (is (p/ready-inbox-pending? "a1" "s1") "still pending: pk-2 in queue + pk-1 leased"))))

(deftest ready-push-coalesces-a-park-entry
  (testing "the same released park cannot stack duplicate inbox deliveries"
    (p/ready-push! "a1" "s1" "pk-1" "prompt-1")
    (p/ready-push! "a1" "s1" "pk-1" "prompt-duplicate")
    (let [first-item (p/ready-lease-one! "a1" "s1" 1000 90000)]
      (is (= "pk-1" (:park-id first-item)))
      (is (= "prompt-1" (:prompt first-item)))
      (is (nil? (p/ready-lease-one! "a1" "s1" 1000 90000))
          "only one delivery was queued"))))

(deftest background-ready-items-do-not-count-as-within-turn-pending
  (testing "background parks resume from the inbox but do not defer turn finalization"
    (p/ready-push! "a1" "s1" "pk-bg" "prompt-bg" :background)
    (is (p/ready-inbox-pending? "a1" "s1") "generic pending still sees background work")
    (is (not (p/ready-inbox-pending? "a1" "s1" :within-turn))
        "within-turn more-pending ignores background ready items")
    (let [item (p/ready-lease-one! "a1" "s1" 1000 90000)]
      (is (= :background (:mode item)))
      (is (not (p/ready-inbox-pending? "a1" "s1" :within-turn))
          "within-turn more-pending also ignores leased background items"))))

(deftest lease-then-ack-clears-lease
  (testing "ready-ack! confirms delivery and removes the lease"
    (p/ready-push! "a1" "s1" "pk-1" "prompt-1")
    (let [item (p/ready-lease-one! "a1" "s1" 1000 90000)]
      (is (some? item))
      (is (p/ready-inbox-pending? "a1" "s1") "leased item counts as pending")
      (is (true? (p/ready-ack! "pk-1")) "ack returns true for a known lease")
      (is (false? (p/ready-ack! "pk-1")) "double-ack returns false")
      (is (not (p/ready-inbox-pending? "a1" "s1")) "after ack, nothing pending"))))

(deftest lease-then-no-ack-then-sweep-redelivers
  (testing "an unacked expired lease is returned to the FRONT of the queue for redelivery"
    (p/ready-push! "a1" "s1" "pk-1" "prompt-1")
    (p/ready-push! "a1" "s1" "pk-2" "prompt-2")
    ;; Lease pk-1 (FIFO front), do NOT ack
    (let [first-pop (p/ready-lease-one! "a1" "s1" 1000 90000)]
      (is (= "pk-1" (:park-id first-pop))))
    ;; Lease pk-2
    (let [second-pop (p/ready-lease-one! "a1" "s1" 1000 90000)]
      (is (= "pk-2" (:park-id second-pop))))
    ;; Queue is now empty, both leased, neither acked. Sweep at 200000 (past deadline 91000).
    (let [r (p/sweep-leased! {:now-ms 200000})]
      (is (= 2 (count (:requeued r))) "both expired leases requeued"))
    ;; Redelivery: pk-1 should be at the FRONT (it was leased first, redelivered to front)
    (let [redelivered (p/ready-lease-one! "a1" "s1" 200000 90000)]
      (is (= "pk-1" (:park-id redelivered)) "pk-1 redelivered first (front of queue)"))))

(deftest old-file-rehydrate-tolerates-missing-new-keys
  (testing "rehydrate! tolerates an old persisted shape without :ready-inbox / :leased"
    ;; Simulate an old file by writing EDN without the new keys, then rehydrating.
    ;; The clear! + park! writes a current-shape file; we overwrite with old shape.
    (p/park! {:agent "claude-1" :awaiting ["b1"] :payload "P"}
             {:ledger-lookup (constantly nil) :resume! (constantly nil) :now-ms 1000})
    ;; Overwrite the store with an OLD-shape file (no :ready-inbox, no :leased).
    (spit "/tmp/futon3c-parked-on.edn"
          (pr-str {:records {"old-park" {:id "old-park" :agent "claude-1" :awaiting #{"b1"}
                                          :arrived {} :payload "old" :budget {:resumes-left 1}}}
                   :index {"b1" #{"old-park"}}}))
    (let [res (p/rehydrate! {:ledger-lookup (constantly {:state "failed" :result-summary "gone"})
                             :resume! (constantly nil) :now-ms 2000})]
      (is (= 1 (:loaded res)) "loaded the old-shape record")
      (is (= 0 (:requeued-leases res)) "no stale leases in old file")
      ;; The new keys should be present in the snapshot (merge tolerance).
      (is (map? (:ready-inbox (p/snapshot))))
      (is (map? (:leased (p/snapshot)))))))
