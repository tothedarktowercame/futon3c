(ns futon3c.apm.semantic-progress-watchdog-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [futon3c.apm.semantic-progress-watchdog :as sut])
  (:import [java.util.concurrent Executors ScheduledExecutorService]))

(def cursor
  {:frame-id "f49" :phase :solve :attempt-ordinal 1
   :obligation/status :ready :active-job-id nil
   :last-committed-event-id "event-7"})

(defn- clear-runners! []
  (let [registry (var-get #'sut/runners)]
    (doseq [executor (vals @registry)]
      (.shutdownNow executor))
    (reset! registry {})))

(use-fixtures :each
  (fn [test-fn]
    (clear-runners!)
    (try (test-fn) (finally (clear-runners!)))))

(defn observation [& {:as overrides}]
  (merge {:cursor cursor
          :regulator {:regulator/status :running}
          :supervisor/status :ready}
         overrides))

(defn run-check [watch-state observation now-ms]
  (let [stops (atom [])
        persisted (atom [])
        result (sut/check!
                {:watch-state watch-state
                 :observation observation
                 :now-ms now-ms
                 :registry-path "/registry.edn"
                 :coordinator-id "campaign"
                 :stop-fn (fn [registry-path coordinator-id]
                            (swap! stops conj [registry-path coordinator-id])
                            {:ok true :status :stopped
                             :durably-disabled? true})
                 :persist-fn (fn [state]
                               (swap! persisted conj state)
                               {:ok true})})]
    [result @stops @persisted]))

(deftest unchanged-ready-cursor-halts-after-five-minutes
  (let [prior (:state (sut/evaluate nil (observation) 1000))
        [result stops persisted]
        (run-check prior (observation) (+ 1000 sut/internal-progress-max-ms))]
    (is (= :halted (:status result)))
    (is (= :internal-semantic-progress-stalled
           (get-in result [:reason :code])))
    (is (= [["/registry.edn" "campaign"]] stops))
    (is (= true (get-in result [:stop :durably-disabled?])))
    (is (= :halted (:watchdog/status (last persisted))))))

(deftest changed-cursor-resets-progress-clock
  (let [prior (:state (sut/evaluate nil (observation) 1000))
        changed (observation :cursor (assoc cursor :phase :verify))
        [result stops _] (run-check prior changed 900000)]
    (is (= :watching (:status result)))
    (is (empty? stops))
    (is (= 900000 (get-in result [:state :watchdog/last-progress-ms])))))

(deftest external-job-inside-deadline-does-not-halt
  (let [[result stops _]
        (run-check nil
                   (observation :awaiting-job
                                {:job-id "solver-1" :deadline 600000})
                   650000)]
    (is (= :watching (:status result)))
    (is (empty? stops))))

(deftest external-job-past-deadline-and-grace-halts
  (let [[result stops _]
        (run-check nil
                   (observation :awaiting-job
                                {:job-id "solver-1" :deadline 600000})
                   (+ 600000 sut/external-deadline-grace-ms 1))]
    (is (= :external-job-deadline-exceeded
           (get-in result [:reason :code])))
    (is (= 1 (count stops)))))

(deftest external-job-without-deadline-fails-closed
  (let [[result stops _]
        (run-check nil
                   (observation :awaiting-job {:job-id "solver-1"})
                   2000)]
    (is (= :external-job-deadline-missing
           (get-in result [:reason :code])))
    (is (= 1 (count stops)))))

(deftest failed-regulator-halts-immediately
  (let [[result stops _]
        (run-check nil
                   (observation :regulator {:regulator/status :failed})
                   0)]
    (is (= :regulator-failed (get-in result [:reason :code])))
    (is (= 1 (count stops)))))

(deftest stale-tick-claim-halts
  (let [[result _ _]
        (run-check nil
                   (observation :tick-claim {:claimed-at 1000})
                   (+ 1000 sut/scheduler-claim-max-ms 1))]
    (is (= :scheduler-claim-stale (get-in result [:reason :code])))))

(deftest stale-tick-claim-validly-awaiting-external-job-does-not-halt
  (let [claimed-at 1000
        now (+ claimed-at sut/scheduler-claim-max-ms 1)
        [result stops _]
        (run-check nil
                   (observation
                    :tick-claim {:claimed-at claimed-at}
                    :awaiting-job {:job-id "solver-1"
                                   :deadline (+ now 1000)})
                   now)]
    (is (= :watching (:status result)))
    (is (empty? stops))
    (is (true? (get-in result [:state :watchdog/trace-observation
                               :valid-external-wait?])))))

(deftest stale-tick-claim-does-not-mask-expired-external-deadline
  (let [claimed-at 1000
        deadline 2000
        now (+ deadline sut/external-deadline-grace-ms 1)
        [result stops _]
        (run-check nil
                   (observation
                    :tick-claim {:claimed-at claimed-at}
                    :awaiting-job {:job-id "solver-1" :deadline deadline})
                   now)]
    (is (= :external-job-deadline-exceeded
           (get-in result [:reason :code])))
    (is (= 1 (count stops)))))

(deftest immediate-integrity-failures-halt
  (doseq [[observation-key reason]
          [[:invalid-state? :invalid-state]
           [:failed-launch-audit? :failed-launch-audit]
           [:impossible-transition? :impossible-transition]]]
    (let [[result _ _]
          (run-check nil (observation observation-key true) 0)]
      (is (= reason (get-in result [:reason :code]))))))

(deftest watchdog-executor-is-independent-of-dead-watched-executor
  (let [^ScheduledExecutorService watched
        (Executors/newSingleThreadScheduledExecutor)
        ^ScheduledExecutorService watchdog
        (Executors/newSingleThreadScheduledExecutor)
        id (str "watchdog-test-" (random-uuid))]
    (.shutdownNow watched)
    (try
      (let [started (sut/start! {:watchdog-id id
                                 :watch-fn (fn [] nil)
                                 :period-ms 60000
                                 :executor-fn (constantly watchdog)})]
        (is (:ok started))
        (is (.isShutdown watched))
        (is (not (.isShutdown watchdog)))
        (is (identical? watchdog (:executor started))))
      (finally
        (sut/stop! id)
        (.shutdownNow watchdog)))))

(deftest start-replaces-stale-watchdog-executor
  (let [first-executor (Executors/newSingleThreadScheduledExecutor)
        second-executor (Executors/newSingleThreadScheduledExecutor)
        executors (atom [first-executor second-executor])
        id (str "watchdog-rearm-" (random-uuid))
        start #(sut/start! {:watchdog-id id
                            :watch-fn (fn [] nil)
                            :period-ms 60000
                            :executor-fn (fn []
                                           (let [executor (first @executors)]
                                             (swap! executors subvec 1)
                                             executor))})]
    (try
      (is (= :started (:status (start))))
      (.shutdownNow first-executor)
      (let [rearmed (start)]
        (is (= :started (:status rearmed)))
        (is (identical? second-executor (:executor rearmed)))
        (is (sut/running? id)))
      (finally
        (sut/stop! id)
        (.shutdownNow first-executor)
        (.shutdownNow second-executor)))))
