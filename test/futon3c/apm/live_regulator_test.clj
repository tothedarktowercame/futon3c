(ns futon3c.apm.live-regulator-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.apm.live-regulator :as sut]))

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

(deftest scheduled-runner-executes-without-an-agent-continuation
  (let [ran (promise) saved (atom nil)
        result (sut/start!
                {:regulator-id "scheduled-test"
                 :period-ms 1000
                 :read-fn (constantly nil)
                 :persist-fn #(do (reset! saved %) {:ok true})
                 :tick-fn #(do (deliver ran true)
                               {:ok true :status :frame-complete})})]
    (try
      (is (= :started (:status result)))
      (is (= true (deref ran 2000 :timeout)))
      (loop [attempt 0]
        (when (and (< attempt 20)
                   (not= :complete (:regulator/status @saved)))
          (Thread/sleep 25)
          (recur (inc attempt))))
      (is (= :complete (:regulator/status @saved)))
      (finally (sut/stop! "scheduled-test")))))
