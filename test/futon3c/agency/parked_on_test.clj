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

(deftest case2-double-delivery-fires-once
  (testing "the same dep completing twice resumes exactly once (index-consumption + single-fire)"
    (let [fired (collector)
          r (p/park! {:agent "claude-1" :awaiting ["b1"] :payload "P"}
                     {:ledger-lookup (constantly nil) :resume! (resume-into fired) :now-ms 1000})]
      (p/note-completion! "b1" "s" {:resume! (resume-into fired) :now-ms 1001})
      (p/note-completion! "b1" "s-again" {:resume! (resume-into fired) :now-ms 1002})
      (is (= [(:id r)] @fired) "second delivery of the same dep is a no-op"))))

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

(deftest case5-deadline-expires-and-timer-fires
  (testing "a past-deadline record expires WITHOUT resuming; a due no-dep timer park resumes"
    (let [fired (collector)
          expired (collector)
          dl (p/park! {:agent "claude-1" :awaiting ["b1"] :payload "P" :deadline-ms 500}
                      {:ledger-lookup (constantly nil) :resume! (resume-into fired) :now-ms 1000})
          tm (p/park! {:agent "claude-1" :awaiting [] :payload "T" :timer-due-ms 400}
                      {:ledger-lookup (constantly nil) :resume! (resume-into fired) :now-ms 1000})
          r (p/sweep-deadlines! {:now-ms 1000 :resume! (resume-into fired)
                                 :on-expire (resume-into expired)})]
      (is (= [(:id dl)] (:expired r)) "past-deadline record expired")
      (is (= [(:id tm)] (:timer-fired r)) "due timer park fired")
      (is (= [(:id dl)] @expired) "on-expire called for the deadline record")
      (is (= [(:id tm)] @fired) "only the timer park resumed; the expired one did NOT"))))

(deftest no-deps-no-timer-resumes-immediately
  (testing "parking on an empty join with no timer is a degenerate immediate resume"
    (let [fired (collector)
          r (p/park! {:agent "claude-1" :awaiting [] :payload "P"}
                     {:ledger-lookup (constantly nil) :resume! (resume-into fired) :now-ms 1000})]
      (is (= :released-immediately (:status r)))
      (is (= [(:id r)] @fired)))))
