(ns futon3c.apm.semantic-progress-watchdog-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is use-fixtures]]
            [futon3c.apm.durable-coordinator :as coordinator]
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
                 :stop-fn (fn [registry-path coordinator-id cause]
                            (swap! stops conj [registry-path coordinator-id cause])
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
    (is (= [["/registry.edn" "campaign"
             {:stop-cause/type :fault
              :stop-cause/fault-class :substrate
              :stop-cause/reason-code :internal-semantic-progress-stalled
              :stop-cause/reason
              {:code :internal-semantic-progress-stalled
               :last-progress-ms 1000}}]]
           stops))
    (is (= true (get-in result [:stop :durably-disabled?])))
    (is (= :halted (:watchdog/status (last persisted))))))

(deftest changed-cursor-resets-progress-clock
  (let [prior (:state (sut/evaluate nil (observation) 1000))
        changed (observation :cursor (assoc cursor :phase :verify))
        [result stops _] (run-check prior changed 900000)]
    (is (= :watching (:status result)))
    (is (empty? stops))
    (is (= 900000 (get-in result [:state :watchdog/last-progress-ms])))))

(def f193-transition
  ;; The frame's last durable transition, verbatim shape from
  ;; jit-all-open-v3-f193/problem-transitions.edn. This is the cursor's source
  ;; now: it is appended only when the frame transitions, so it cannot flicker
  ;; with the coordinator's tick cycle.
  {:frame-id "f193"
   :phase :guide-intervention-1
   :event/id "3f3168efa6afd"
   :event/sequence 14
   :ledger/event-count 15
   :operation {:job-id "apm-role-93e78eacd865"
               :status :waiting-for-terminal-result
               :role :guide}})

(def f193-queue-state
  (edn/read-string
   (slurp (io/resource "resources/apm-regressions/f193-semantic-stall/queue-state.edn"))))

(def f193-durable-coordinator-state
  (edn/read-string
   (slurp (io/resource "resources/apm-regressions/f193-semantic-stall/coordinator.edn"))))

(defn f193-coordinator-state [tick-job-id]
  {:state/type :live-regulator
   :regulator/status :running
   :coordinator/pending-intent {:job-id tick-job-id}
   :regulator/last-result
   {:status :intent-persisted
    :queue/result
    {:status :parked
     :projection
     {:projection
      {:frame {:phase :guide-intervention-1}
       :operation {:status :waiting-for-terminal-result
                   :role :guide
                   :job-id "apm-role-93e78eacd865"}}
      :transition {:event/id "3f3168efa6afd"}}}}})

(deftest coordinator-ticks-do-not-count-as-frame-progress
  (let [first-observation
        (coordinator/watchdog-observation
         {:coordinator/enabled? true}
         (f193-coordinator-state "jit-tick-5e07a189-a")
         f193-queue-state f193-transition)
        next-observation
        (coordinator/watchdog-observation
         {:coordinator/enabled? true}
         (f193-coordinator-state "jit-tick-5e07a189-b")
         f193-queue-state f193-transition)
        prior (:state (sut/evaluate nil first-observation 1000))
        next (:state (sut/evaluate prior next-observation 2000))]
    (is (= {:frame-id "f193"
            :phase :guide-intervention-1
            :attempt-ordinal 24
            :obligation/status :waiting-for-terminal-result
            :active-job-id "apm-role-93e78eacd865"
            :last-committed-event-id "3f3168efa6afd"
            :event-sequence 14}
           (sut/progress-cursor first-observation)))
    (is (= (sut/progress-cursor first-observation)
           (sut/progress-cursor next-observation)))
    (is (= 1000 (:watchdog/last-progress-ms next)))))

(deftest genuinely-progressing-frame-resets-progress-clock
  (let [before (coordinator/watchdog-observation
                {:coordinator/enabled? true}
                (f193-coordinator-state "jit-tick-a")
                f193-queue-state f193-transition)
        progressed (coordinator/watchdog-observation
                    {:coordinator/enabled? true}
                    (f193-coordinator-state "jit-tick-b")
                    (assoc-in f193-queue-state [:active :frame :frame/id] "f194")
                    f193-transition)
        prior (:state (sut/evaluate nil before 1000))
        next (:state (sut/evaluate prior progressed 2000))]
    (is (= "f194" (get-in next [:watchdog/cursor :frame-id])))
    (is (= 2000 (:watchdog/last-progress-ms next)))))

(deftest f193-stalled-frame-halts-at-the-existing-semantic-progress-bound
  (let [running-state (assoc f193-durable-coordinator-state
                             :regulator/status :running)
        observed (coordinator/watchdog-observation
                  {:coordinator/enabled? true}
                  running-state
                  f193-queue-state f193-transition)
        prior (:state (sut/evaluate nil observed 1000))
        [result stops _]
        (run-check prior observed (+ 1000 sut/internal-progress-max-ms))]
    (is (= "f193" (get-in observed [:cursor :frame-id])))
    (is (= "m00A02" (get-in f193-queue-state
                             [:active :frame :problem/id])))
    ;; Was :parked, which was the QUEUE TICK's status read from
    ;; :regulator/last-result. The cursor now reports the frame's own role
    ;; operation, verbatim from problem-transitions.edn -- what f193 is
    ;; actually doing, not what the last tick returned.
    (is (= :waiting-for-terminal-result
           (get-in observed [:cursor :obligation/status])))
    (is (= :internal-semantic-progress-stalled
           (get-in result [:reason :code])))
    (is (= :halted (:status result)))
    (is (= 1 (count stops)))))

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

(deftest failed-regulator-halts-only-for-underlying-corruption
  (let [[result stops _]
        (run-check nil
                   (observation
                    :regulator {:regulator/status :failed
                                :regulator/last-result
                                {:ok false :error/code
                                 :campaign-ledger-digest-mismatch}})
                   0)]
    (is (= :campaign-ledger-digest-mismatch
           (get-in result [:reason :code])))
    (is (= 1 (count stops))))
  (let [[result stops _]
        (run-check nil
                   (observation
                    :regulator {:regulator/status :failed
                                :regulator/last-result
                                {:ok false :error/code :mundane-failure}})
                   0)]
    (is (= :watching (:status result)))
    (is (empty? stops))))

(deftest stale-tick-claim-halts
  (let [[result _ _]
        (run-check nil
                   (observation :tick-claim {:claimed-at 1000})
                   (+ 1000 sut/scheduler-claim-max-ms 1))]
    (is (= :scheduler-claim-stale (get-in result [:reason :code])))))

(deftest watchdog-fault-classification-is-conservative-and-actionable
  (doseq [code sut/integrity-fault-codes]
    (is (= :integrity
           (:stop-cause/fault-class (sut/fault-stop-cause {:code code})))
        (str code " must never be automatically restarted")))
  (doseq [code sut/substrate-fault-codes]
    (is (= :substrate
           (:stop-cause/fault-class (sut/fault-stop-cause {:code code})))
        (str code " is eligible for supervised restart")))
  (is (= :frame
         (:stop-cause/fault-class
          (sut/fault-stop-cause {:code :unclassified-future-fault})))
      "unknown codes are frame-confined, not evidence of corruption"))

(deftest every-current-watchdog-halt-has-a-keyword-reason-code
  (let [prior (:state (sut/evaluate nil (observation) 1000))
        halt-observations
        [[nil (observation :regulator
                           {:regulator/status :failed
                            :regulator/last-result
                            {:ok false :error/code
                             :campaign-ledger-digest-mismatch}}) 1000]
         [nil (observation :invalid-state? true :invalid-state {}) 1000]
         [nil (observation :failed-launch-audit? true
                           :launch-audit {}) 1000]
         [nil (observation :impossible-transition? true
                           :transition {}) 1000]
         [nil (observation :awaiting-job {:job-id "job"}) 1000]
         [nil (observation :awaiting-job {:job-id "job" :deadline 1})
          (+ 1 sut/external-deadline-grace-ms 1)]
         [nil (observation :tick-claim {:claimed-at 1})
          (+ 1 sut/scheduler-claim-max-ms 1)]
         [prior (observation) (+ 1000 sut/internal-progress-max-ms)]]]
    (doseq [[watch-state observed now-ms] halt-observations]
      (let [decision (sut/evaluate watch-state observed now-ms)]
        (is (= :halt (:status decision)))
        (is (keyword? (get-in decision [:reason :code])))))))

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

(deftest reconciliation-claim-supersedes-historical-launch-audit-failure
  (let [[result stops _]
        (run-check nil
                   (observation
                    :failed-launch-audit? true
                    :tick-claim {:claimed-at 1000}
                    :awaiting-job {:job-id "jit-repair-tick"
                                   :deadline 600000})
                   2000)]
    (is (= :watching (:status result)))
    (is (empty? stops))))

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
