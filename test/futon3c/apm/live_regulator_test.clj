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
         (:error/code (sut/tick! {:state (sut/initial-state "r1")})))))

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

(deftest scheduled-runner-executes-without-an-agent-continuation
  (let [persisted (promise) saved (atom nil)
        result (sut/start!
                {:regulator-id "scheduled-test"
                 :period-ms 1000
                 :read-fn (constantly nil)
                 :persist-fn #(do (reset! saved %)
                                  (deliver persisted %)
                                  {:ok true})
                 :tick-fn (constantly {:ok true :status :frame-complete})})]
    (try
      (is (= :started (:status result)))
      (is (not= :timeout (deref persisted 2000 :timeout)))
      (is (= :complete (:regulator/status @saved)))
      (finally (sut/stop! "scheduled-test")))))

(deftest start-replaces-a-stale-shutdown-runner
  (let [id "stale-runner-test"
        executor (Executors/newSingleThreadScheduledExecutor)
        persisted (promise)
        saved (atom nil)]
    (.shutdown executor)
    (swap! (var-get #'sut/runners)
           assoc id {:executor executor :state (atom (sut/initial-state id))})
    (try
      (is (= :started
             (:status
              (sut/start!
               {:regulator-id id :period-ms 1000
                :read-fn (constantly (sut/initial-state id))
                :persist-fn #(do (reset! saved %)
                                 (deliver persisted %)
                                 {:ok true})
                :tick-fn (constantly {:ok true :status :frame-complete})}))))
      (is (not= :timeout (deref persisted 2000 :timeout)))
      (is (= :complete (:regulator/status @saved)))
      (finally (sut/stop! id)))))

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
                  :tick-fn #(do (swap! ticks inc)
                                {:ok true :status :parked})}))]
    (try
      (is (= :started (:status (start "countdown-regulator:campaign-a"
                                      ticks-a persisted-a ready-a))))
      (is (= :started (:status (start "countdown-regulator:campaign-b"
                                      ticks-b persisted-b ready-b))))
      (is (= true (deref ready-a 2000 :timeout)))
      (is (= true (deref ready-b 2000 :timeout)))
      (is (pos? @ticks-a))
      (is (pos? @ticks-b))
      (is (= "countdown-regulator:campaign-a"
             (:regulator/id (sut/status "countdown-regulator:campaign-a"))))
      (is (= "countdown-regulator:campaign-b"
             (:regulator/id (sut/status "countdown-regulator:campaign-b"))))
      (is (= :stopped
             (:status (sut/stop! "countdown-regulator:campaign-a"))))
      (is (nil? (sut/status "countdown-regulator:campaign-a")))
      (is (= :running
             (:regulator/status
              (sut/status "countdown-regulator:campaign-b"))))
      (is (= "countdown-regulator:campaign-a"
             (:regulator/id @persisted-a)))
      (is (= "countdown-regulator:campaign-b"
             (:regulator/id @persisted-b)))
      (finally
        (sut/stop! "countdown-regulator:campaign-a")
        (sut/stop! "countdown-regulator:campaign-b")))))
