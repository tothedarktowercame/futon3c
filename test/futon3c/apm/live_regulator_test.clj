(ns futon3c.apm.live-regulator-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [futon3c.apm.live-regulator :as sut])
  (:import [java.util.concurrent Executors]))

(defn- clear-runners! []
  (let [registry (var-get #'sut/runners)]
    (doseq [[_ {:keys [executor]}] @registry]
      (.shutdownNow executor))
    (reset! registry {})))

(use-fixtures :each
  (fn [test-fn]
    (clear-runners!)
    (try (test-fn) (finally (clear-runners!)))))

(defn- with-lock [f] (f))

(deftest tick-persists-running-and-terminal-results
  (let [saved (atom nil)
        persist #(do (reset! saved %) {:ok true})
        first-result (sut/tick!
                      {:state (sut/initial-state "r1")
                       :tick-fn (constantly {:ok true :status :parked
                                             :job-id "job-1"})
                       :persist-fn persist})
        final-result (sut/tick!
                      {:state (:state first-result)
                       :tick-fn (constantly {:ok true :status :frame-complete})
                       :persist-fn persist})]
    (is (= :running (:status first-result)))
    (is (= "job-1" (get-in first-result [:state :regulator/last-result :job-id])))
    (is (= :complete (:status final-result)))
    (is (= 2 (:regulator/ticks @saved)))))

(deftest failed-supervisor-tick-is-durable-and-terminal
  (let [saved (atom nil)
        result (sut/tick!
                {:state (sut/initial-state "r1")
                 :tick-fn (constantly {:ok false :error/code :failed-invariant})
                 :persist-fn #(do (reset! saved %) {:ok true})})]
    (is (false? (:ok result)))
    (is (= :failed (:status result)))
    (is (= :failed (:regulator/status @saved)))
    (is (= :failed-invariant
           (get-in @saved [:regulator/last-result :error/code])))))

(deftest claim-is-durable-before-effects-and-cleared-after-completion
  (let [writes (atom [])
        result (sut/tick!
                {:state (sut/initial-state "claimed")
                 :now-fn (constantly "2026-08-27T12:00:00Z")
                 :persist-fn #(do (swap! writes conj %) {:ok true})
                 :tick-state-fn
                 (fn [claimed]
                   (is (= :live-regulator-tick-claim
                          (get-in claimed [:regulator/tick-claim :state/type])))
                   {:ok true :status :parked})})]
    (is (:ok result))
    (is (= 2 (count @writes)))
    (is (some? (:regulator/tick-claim (first @writes))))
    (is (nil? (:regulator/tick-claim (second @writes))))
    (is (= :settled (:regulator/reconciliation (second @writes))))))

(deftest failed-claim-write-blocks-the-effect
  (let [effects (atom 0)
        writes (atom 0)
        result (sut/tick!
                {:state (sut/initial-state "claim-fails")
                 :persist-fn (fn [_]
                               (swap! writes inc)
                               {:ok false :error/code :disk-full})
                 :tick-fn #(do (swap! effects inc) {:ok true})})]
    (is (= :live-regulator-claim-persistence-failed (:error/code result)))
    (is (= 1 @writes))
    (is (zero? @effects))))

(deftest failed-completion-write-leaves-the-durable-claim-for-recovery
  (let [durable (atom nil)
        writes (atom 0)
        result (sut/tick!
                {:state (sut/initial-state "completion-fails")
                 :tick-fn (constantly {:ok true :status :parked})
                 :persist-fn
                 (fn [state]
                   (if (= 1 (swap! writes inc))
                     (do (reset! durable state) {:ok true})
                     {:ok false :error/code :disk-full}))})]
    (is (= :live-regulator-persistence-failed (:error/code result)))
    (is (= 2 @writes))
    (is (= :live-regulator-tick-claim
           (get-in @durable [:regulator/tick-claim :state/type])))
    (is (= :claimed (:regulator/reconciliation @durable)))))

(deftest stale-claim-is-reconciled-without-minting-a-successor
  (let [claim {:state/type :live-regulator-tick-claim
               :regulator/id "recover" :tick/epoch 7 :tick/ordinal 4
               :tick/id "recover:7:4" :tick/claimed-at "then"}
        state (assoc (sut/initial-state "recover")
                     :regulator/epoch 7 :regulator/ticks 3
                     :regulator/tick-claim claim)
        writes (atom [])
        result (sut/tick! {:state state
                           :persist-fn #(do (swap! writes conj %) {:ok true})
                           :tick-state-fn
                           (fn [claimed]
                             (is (= :reconciling
                                    (:regulator/reconciliation claimed)))
                             {:ok true :status :awaiting-job})})]
    (is (:ok result))
    (is (= 1 (count @writes)))
    (is (= claim (get-in result [:state :regulator/last-completed-tick])))
    (is (= 4 (get-in result [:state :regulator/ticks])))))

(deftest superseded-runner-epoch-cannot-produce-effects
  (let [effects (atom 0)
        writes (atom 0)
        state (assoc (sut/initial-state "epoch") :regulator/epoch 2)
        result (sut/tick! {:state state :expected-epoch 1
                           :persist-fn (fn [_] (swap! writes inc) {:ok true})
                           :tick-fn (fn [] (swap! effects inc) {:ok true})})]
    (is (= :live-regulator-epoch-superseded (:error/code result)))
    (is (zero? @writes))
    (is (zero? @effects))))

(deftest terminal-recovery-does-not-run-or-repersist
  (let [calls (atom [])
        state (assoc (sut/initial-state "r1") :regulator/status :complete)
        result (sut/tick! {:state state
                           :tick-fn #(swap! calls conj :tick)
                           :persist-fn #(swap! calls conj [:persist %])})]
    (is (:ok result))
    (is (= :complete (:status result)))
    (is (empty? @calls))))

(deftest malformed-state-and-provider-fail-closed
  (is (= :live-regulator-state-invalid
         (:error/code (sut/tick! {:state {} :tick-fn identity
                                  :persist-fn identity}))))
  (is (= :live-regulator-provider-missing
         (:error/code (sut/tick! {:state (sut/initial-state "r1")}))))
  (is (= :live-regulator-durable-stop-required
         (:error/code (sut/stop! "r1")))))

(deftest failed-regulator-resume-retains-repair-evidence
  (let [failed (assoc (sut/initial-state "r")
                      :regulator/status :failed :regulator/ticks 1
                      :regulator/last-result {:ok false :error/code :tick-threw})
        durable (atom failed)
        result (sut/repair-resume!
                {:state failed :reason "reloaded the complete proof spine"
                 :persist-fn #(do (reset! durable %) {:ok true})})]
    (is (:ok result))
    (is (= :running (:regulator/status @durable)))
    (is (= :tick-threw
           (get-in @durable [:regulator/failures 0 :result :error/code])))
    (is (= :live-regulator-not-repairable
           (:error/code (sut/repair-resume!
                         {:state @durable :reason "again"
                          :persist-fn (constantly {:ok true})}))))))

(deftest completed-regulator-continuation-retains-completion-evidence
  (let [completed (assoc (sut/initial-state "r")
                         :regulator/status :complete :regulator/ticks 9
                         :regulator/last-result {:ok true :status :frame-complete})
        durable (atom completed)
        result (sut/continue-complete!
                {:state completed :reason "operator resumed paused queue"
                 :persist-fn #(do (reset! durable %) {:ok true})})]
    (is (:ok result))
    (is (= :running (:regulator/status @durable)))
    (is (= 9 (get-in @durable [:regulator/completions 0 :ticks])))
    (is (= "operator resumed paused queue"
           (get-in @durable
                   [:regulator/completions 0 :continuation/reason])))
    (is (= :live-regulator-not-complete
           (:error/code (sut/continue-complete!
                         {:state @durable :reason "again"
                          :persist-fn (constantly {:ok true})}))))))

(deftest stopped-regulator-reopens-only-from-a-quiescence-witness
  (let [witness {:state/type :durable-quiescence-witness :tick-claim nil}
        stopped (assoc (sut/initial-state "r")
                       :regulator/status :stopped
                       :regulator/quiescence-witness witness)
        durable (atom stopped)
        result (sut/resume-stopped!
                {:state stopped :now-fn (constantly "resumed")
                 :persist-fn #(do (reset! durable %) {:ok true})})]
    (is (= :running (:status result)))
    (is (= [witness] (:regulator/quiescence-history @durable)))
    (is (nil? (:regulator/quiescence-witness @durable)))
    (is (= :live-regulator-quiescence-witness-invalid
           (:error/code
            (sut/resume-stopped!
             {:state (dissoc stopped :regulator/quiescence-witness)
              :persist-fn (constantly {:ok true})}))))))

(deftest scheduled-runner-executes-without-an-agent-continuation
  (let [saved (atom nil)
        result (sut/start!
                {:regulator-id "scheduled-test"
                 :period-ms 1000
                 :read-fn (constantly nil)
                 :persist-fn #(do (reset! saved %)
                                  {:ok true})
                 :with-tick-lock-fn with-lock
                 :tick-fn (constantly {:ok true :status :frame-complete})})]
    (try
      (is (= :started (:status result)))
      ;; Same contention budget as above.
      (is (not= :timeout (deref (:first-tick result) 15000 :timeout)))
      (is (= :complete (:regulator/status @saved)))
      (finally (sut/cancel-scheduler! "scheduled-test")))))

(deftest start-replaces-a-stale-shutdown-runner
  (let [id "stale-runner-test"
        executor (Executors/newSingleThreadScheduledExecutor)
        saved (atom (sut/initial-state id))]
    (.shutdown executor)
    (swap! (var-get #'sut/runners)
           assoc id {:executor executor :state (atom (sut/initial-state id))})
    (try
      (let [started (sut/start!
                     {:regulator-id id :period-ms 1000
                      :read-fn #(deref saved)
                      :persist-fn #(do (reset! saved %) {:ok true})
                      :with-tick-lock-fn with-lock
                      :tick-fn (constantly
                                {:ok true :status :frame-complete})})]
        (is (= :started (:status started)))
        (is (not= :timeout
                  (deref (:first-tick started) 2000 :timeout))))
      (is (= :complete (:regulator/status @saved)))
      (finally (sut/cancel-scheduler! id)))))

(deftest campaign-scoped-runners-coexist-and-stop-independently
  (let [ticks-a (atom 0)
        ticks-b (atom 0)
        persisted-a (atom nil)
        persisted-b (atom nil)
        ready-a (promise)
        ready-b (promise)
        start (fn [id ticks persisted ready]
                (sut/start!
                 {:regulator-id id
                  :period-ms 1000
                  :read-fn (constantly nil)
                  :persist-fn #(do (reset! persisted %)
                                   (deliver ready true)
                                   {:ok true})
                  :with-tick-lock-fn with-lock
                  :tick-fn #(do (swap! ticks inc)
                                {:ok true :status :parked})}))]
    (try
      (is (= :started (:status (start "countdown-regulator:campaign-a"
                                      ticks-a persisted-a ready-a))))
      (is (= :started (:status (start "countdown-regulator:campaign-b"
                                      ticks-b persisted-b ready-b))))
      ;; Real-time budget, not a synchronisation point. The first tick is
      ;; scheduled with zero initial delay and this test's persist-fn is an
      ;; in-memory reset! with a no-op tick lock, so the tick itself is
      ;; effectively free — this is NOT an A5 cost. What exceeds 2s is JVM
      ;; contention: inside the qualification gate this namespace shares a
      ;; process with ninety-odd others, and the scheduled executor competes
      ;; with Clojure compiling them. Generous so it fails only if scheduling
      ;; is actually broken.
      (is (= true (deref ready-a 15000 :timeout)))
      (is (= true (deref ready-b 15000 :timeout)))
      (is (pos? @ticks-a))
      (is (pos? @ticks-b))
      (is (= "countdown-regulator:campaign-a"
             (:regulator/id (sut/status "countdown-regulator:campaign-a"))))
      (is (= "countdown-regulator:campaign-b"
             (:regulator/id (sut/status "countdown-regulator:campaign-b"))))
      (is (= :stopped
             (:status (sut/cancel-scheduler! "countdown-regulator:campaign-a"))))
      (is (nil? (sut/status "countdown-regulator:campaign-a")))
      (is (= :running
             (:regulator/status
              (sut/status "countdown-regulator:campaign-b"))))
      (is (= "countdown-regulator:campaign-a"
             (:regulator/id @persisted-a)))
      (is (= "countdown-regulator:campaign-b"
             (:regulator/id @persisted-b)))
      (finally
        (sut/cancel-scheduler! "countdown-regulator:campaign-a")
        (sut/cancel-scheduler! "countdown-regulator:campaign-b")))))
