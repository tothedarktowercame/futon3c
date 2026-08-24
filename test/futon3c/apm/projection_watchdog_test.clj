(ns futon3c.apm.projection-watchdog-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.apm.projection-watchdog :as watchdog])
  (:import [java.time Instant]))

(def healthy
  {:now (Instant/parse "2026-08-23T22:00:00Z")
   :coordinator-age-seconds 2 :max-heartbeat-age-seconds 120
   :coordinator {:regulator/status :running :regulator/ticks 10
                 :regulator/last-result {:ok true}
                 :coordinator/pending-intent
                 {:intent/digest "i" :dispatch/id "d" :dispatch/action :tick
                  :job-id "j" :pre-state/digest "s" :pre-state/version 9
                  :expected/postcondition {:status/one-of [:parked]}}}
   :transition {:event/id "event" :event/observed-at "2026-08-23T21:59:59Z"
                :publication/receipt-id "p"
                :frame-id "f" :problem-id "x"
                :phase :student-attempt-1
                :operation {:status :waiting-for-terminal-result
                            :agent-id "f-student" :job-id "role-job"}}
   :publication {:receipt/id "p" :transition/event-id "event"}
   :phase-state {:state/type :live-job-dispatched
                 :request {:turn-timeout-ms 3600000
                           :terminal-budget {:collection-attempts 1
                                             :repair-attempts 1}}
                 :ticket {:job-id "role-job"}}
   :agent {:ok true :agent {:status "invoking" :running-jobs 1
                            :invoke-started-at "2026-08-23T21:30:00Z"}}
   :job {:ok true :job {:state "running"}}})

(defn codes [x] (set (map :error/code (:watch/findings x))))

(deftest all-modeled-failure-classes-alert
  (is (= :healthy (:watch/status (watchdog/evaluate healthy))))
  (is (= watchdog/obligation-ids (:watch/checked (watchdog/evaluate healthy))))
  (doseq [[label mutation code]
          [[:stopped #(assoc-in % [:coordinator :regulator/status] :failed)
            :coordinator-not-running]
           [:heartbeat #(assoc % :coordinator-age-seconds 121)
            :coordinator-heartbeat-stale]
           [:intent #(update-in % [:coordinator :coordinator/pending-intent]
                                dissoc :dispatch/id) :pending-intent-incomplete]
           [:publication #(assoc-in % [:publication :transition/event-id] "other")
            :projection-publication-diverged]
           [:unattended #(-> %
                             (assoc-in [:transition :operation :status] :preparing)
                             (assoc-in [:transition :event/observed-at]
                                       "2026-08-23T21:57:00Z"))
            :unattended-transition-stale]
           [:state #(assoc-in % [:phase-state :state/type] :live-job-announced)
            :active-phase-state-invalid]
           [:job #(-> %
                     (assoc-in [:phase-state :ticket :job-id] "other")
                     (assoc-in [:transition :event/observed-at]
                               "2026-08-23T21:59:40Z"))
            :projected-job-mismatch]
           [:agent #(assoc % :agent {:ok false}) :agency-agent-unreachable]
           [:idle #(assoc-in % [:agent :agent :status] "idle")
            :agency-job-not-running]
           [:terminal-stale #(-> %
                                (assoc-in [:agent :agent :status] "idle")
                                (assoc-in [:agent :agent :running-jobs] nil)
                                (assoc :job {:ok true :job
                                             {:state "done"
                                              :finished-at "2026-08-23T21:57:00Z"}}))
            :terminal-job-collection-stale]
           [:timeout #(assoc-in % [:phase-state :request :turn-timeout-ms] 1)
            :active-job-timeout]
           [:budget #(assoc-in % [:phase-state :request :terminal-budget
                                  :repair-attempts] 0)
            :terminal-budget-invalid]
           [:result #(assoc-in % [:coordinator :regulator/last-result :ok] false)
            :coordinator-last-result-failed]]]
    (testing (name label)
      (let [result (watchdog/evaluate (mutation healthy))]
        (is (= :alert (:watch/status result)))
        (is (contains? (codes result) code))))))

(deftest certified-completion-does-not-require-a-retired-role-job
  (let [completed (-> healthy
                      (assoc-in [:coordinator :regulator/status] :complete)
                      (assoc-in [:coordinator :regulator/last-result]
                                {:ok true :status :frame-complete})
                      (assoc :phase-state {:state/type :live-job-certified}
                             :agent {:ok false}))
        result (watchdog/evaluate completed)]
    (is (= :healthy (:watch/status result)) (pr-str (:watch/findings result)))
    (is (empty? (:watch/findings result)))))

(deftest bounded-solver-round-envelope-exposes-its-active-job
  (let [solver (assoc healthy :phase-state
                      {:state/type :solver-rounds
                       :budget/max-rounds 50
                       :rounds []
                       :active (:phase-state healthy)})
        result (watchdog/evaluate solver)]
    (is (= :healthy (:watch/status result)) (pr-str (:watch/findings result)))
    (is (empty? (:watch/findings result)))))

(deftest promotion-envelope-exposes-its-bounded-live-job
  (let [promotion (assoc healthy :phase-state
                         {:state/type :promotion
                          :stage :deposit
                          :job "role-job"
                          :request (get-in healthy [:phase-state :request])
                          :ticket {:job-id "role-job"}})
        result (watchdog/evaluate promotion)]
    (is (= :healthy (:watch/status result)) (pr-str (:watch/findings result)))
    (is (empty? (:watch/findings result)))))

(deftest recently-terminal-job-is-a-bounded-collection-state
  (let [collecting (-> healthy
                       (assoc-in [:agent :agent :status] "idle")
                       (assoc-in [:agent :agent :running-jobs] nil)
                       (assoc :job {:ok true :job
                                    {:state "done"
                                     :finished-at "2026-08-23T21:59:58Z"}}))
        result (watchdog/evaluate collecting)]
    (is (= :healthy (:watch/status result)) (pr-str (:watch/findings result)))
    (is (empty? (:watch/findings result)))))

(deftest fresh-solver-round-handoff-allows-projection-catchup
  (let [handoff (assoc-in healthy [:phase-state :ticket :job-id] "next-job")
        result (watchdog/evaluate handoff)]
    (is (= :healthy (:watch/status result)) (pr-str (:watch/findings result)))
    (is (empty? (:watch/findings result)))))
