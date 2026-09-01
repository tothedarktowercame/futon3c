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

(def running-cascade
  {:state/type :memory-cascade-operation
   :operation/id "cascade-1"
   :operation :memory-cascade-expansion
   :frame-id "f" :problem-id "x" :phase :student-attempt-2 :attempt 2
   :status :running :started-at "2026-08-23T21:58:00Z"
   :started-at-ms 1787522280000 :budget-ms 300000
   :deadline-at-ms 1787522580000
   :progress {:stage :expanding :seed-count 10}})

(deftest conformant-cascade-suppresses-only-generic-staleness-within-bound
  (let [stale (-> healthy
                  (assoc :coordinator-age-seconds 180
                         :cascade-operation running-cascade
                         :job {:ok true :job
                               {:state "done"
                                :finished-at "2026-08-23T21:57:00Z"}}))
        result (watchdog/evaluate stale)]
    (is (= :waiting (:watch/status result)) (pr-str (:watch/findings result)))
    (is (not (contains? (codes result) :coordinator-heartbeat-stale)))
    (is (not (contains? (codes result) :terminal-job-collection-stale)))))

(deftest expired-and-malformed-cascade-operations-fail-closed
  (let [expired (watchdog/evaluate
                 (assoc healthy :coordinator-age-seconds 180
                        :cascade-operation
                        (assoc running-cascade :deadline-at-ms 1787522399000
                               :budget-ms 119000)))
        malformed (watchdog/evaluate
                   (assoc healthy :cascade-operation
                          (dissoc running-cascade :deadline-at-ms)))]
    (is (contains? (codes expired) :cascade-operation-deadline-exceeded))
    (is (contains? (codes expired) :coordinator-heartbeat-stale))
    (is (contains? (codes malformed) :cascade-operation-malformed))))

(deftest terminal-cascade-success-and-failure-are-conformant-observations
  (doseq [terminal [(assoc running-cascade :status :succeeded
                           :finished-at-ms 1787522300000
                           :result {:outcome :ok :elapsed-ms 20000})
                    (assoc running-cascade :status :failed
                           :finished-at-ms 1787522300000
                           :result {:outcome :failed-503 :elapsed-ms 20000
                                    :http/status 503})]]
    (let [result (watchdog/evaluate
                  (assoc healthy :cascade-operation terminal))]
      (is (= :healthy (:watch/status result)) (pr-str (:watch/findings result)))
      (is (empty? (:watch/findings result))))))

(deftest declared-substrate-backoff-is-waiting-not-unattended
  (let [result
        (watchdog/evaluate
         (-> healthy
             (assoc :coordinator-age-seconds 180)
             (assoc-in [:transition :operation] nil)
             (assoc-in [:transition :event/observed-at]
                       "2026-08-23T21:57:00Z")
             (assoc-in [:coordinator :regulator/last-result]
                       {:ok true :status :awaiting-substrate
                        :retry/not-before-ms 1787523000000})))]
    (is (= :waiting (:watch/status result)))
    (is (= {:wake-at-ms 1787523000000} (:substrate-wait result)))
    (is (not (contains? (codes result) :coordinator-heartbeat-stale)))
    (is (not (contains? (codes result) :unattended-transition-stale)))))

(deftest operationless-guide-promotion-retry-is-waiting
  (let [result
        (watchdog/evaluate
         (-> healthy
             (assoc :coordinator-age-seconds 180)
             (assoc-in [:transition :operation] nil)
             (assoc-in [:transition :event/observed-at]
                       "2026-08-23T21:57:00Z")
             (assoc :phase-state
                    {:state/type :promotion
                     :stage :awaiting-transport-retry
                     :transport-retry/not-before-ms 1787523000000
                     :transport-retry/attempt 1
                     :transport-retry/max-attempts 3})))]
    (is (= :waiting (:watch/status result)))
    (is (= 1 (get-in result [:transport-retry :attempt])))
    (is (not (contains? (codes result) :coordinator-heartbeat-stale)))
    (is (not (contains? (codes result) :unattended-transition-stale)))))

(deftest bounded-coordinator-intent-owns-terminal-collection-wait
  (let [intent {:state/type :durable-coordinator-intent
                :intent/digest "intent"
                :dispatch/id "dispatch"
                :dispatch/action :tick
                :job-id "tick"
                :pre-state/digest "pre"
                :pre-state/version 1
                :expected/postcondition {:status :done}
                :dispatch/parameters {:deadline-ms 1787523000000}}
        terminal {:ok true :job {:state "done"
                                 :finished-at "2026-08-23T21:57:00Z"}}
        result (watchdog/evaluate
                (-> healthy
                    (assoc :coordinator-age-seconds 180 :job terminal)
                    (assoc-in [:coordinator :coordinator/pending-intent] intent)))]
    (is (= :waiting (:watch/status result)) (pr-str (:watch/findings result)))
    (is (empty? (:watch/findings result)))
    (is (= {:job-id "tick" :deadline-ms 1787523000000}
           (:coordinator-intent-wait result)))))

(deftest expired-coordinator-intent-does-not-hide-staleness
  (let [intent {:state/type :durable-coordinator-intent
                :intent/digest "intent"
                :dispatch/id "dispatch"
                :dispatch/action :tick
                :job-id "tick"
                :pre-state/digest "pre"
                :pre-state/version 1
                :expected/postcondition {:status :done}
                :dispatch/parameters {:deadline-ms 1787522399000}}
        result (watchdog/evaluate
                (-> healthy
                    (assoc :coordinator-age-seconds 180)
                    (assoc-in [:coordinator :coordinator/pending-intent] intent)))]
    (is (= :alert (:watch/status result)))
    (is (contains? (codes result) :coordinator-heartbeat-stale))))

(deftest expired-transport-retry-no-longer-suppresses-staleness
  (let [result
        (watchdog/evaluate
         (-> healthy
             (assoc :coordinator-age-seconds 180)
             (assoc-in [:transition :operation] nil)
             (assoc-in [:transition :event/observed-at]
                       "2026-08-23T21:57:00Z")
             (assoc :phase-state
                    {:state/type :promotion
                     :stage :awaiting-transport-retry
                     :transport-retry/not-before-ms 1787522399000
                     :transport-retry/attempt 1
                     :transport-retry/max-attempts 3})))]
    (is (= :alert (:watch/status result)))
    (is (contains? (codes result) :coordinator-heartbeat-stale))
    (is (contains? (codes result) :unattended-transition-stale))))

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

(deftest scheduled-promotion-transport-retry-retires-projected-terminal
  (let [retrying (-> healthy
                     (assoc :phase-state
                             {:state/type :promotion
                             :stage :awaiting-transport-retry
                             :transport-retry/not-before-ms 9999999999999
                             :transport-retry/attempt 1
                             :transport-retry/max-attempts 3
                             :transport-retry/last-error-code
                             :memory-snapshot-visibility-not-obtained})
                     (assoc-in [:publication :transition/event-id] "prior")
                     (assoc-in [:transition :event/observed-at]
                               "2026-08-23T21:00:00Z")
                     (assoc :agent {:ok false}
                            :job {:ok true :job
                                  {:state "done"
                                   :finished-at "2026-08-23T21:00:00Z"}}))
        result (watchdog/evaluate retrying)]
    (is (= :waiting (:watch/status result)) (pr-str (:watch/findings result)))
    (is (empty? (:watch/findings result)))
    (is (= {:wake-at-ms 9999999999999
            :attempt 1 :max-attempts 3
            :last-failure :memory-snapshot-visibility-not-obtained}
           (:transport-retry result)))))

(deftest terminal-promotion-transport-park-is-coherent-with-retired-job
  (let [parked (-> healthy
                   (assoc :phase-state
                          {:state/type :promotion
                           :stage :awaiting-apparatus-repair
                           :transport-retry/terminal? true
                           :error/code :promotion-substrate-retry-exhausted})
                   (assoc-in [:transition :operation] nil)
                   (assoc :agent {:ok false} :job {:ok true}))
        result (watchdog/evaluate parked)]
    (is (= :healthy (:watch/status result)) (pr-str (:watch/findings result)))
    (is (empty? (:watch/findings result)))))

(deftest durable-frame-closure-retires-the-last-projected-role-job
  (let [closed (-> healthy
                   (assoc :frame-closed? true
                          :phase-state {:state/type :live-job-certified}
                          :agent {:ok false}
                          :job {:ok true :job
                                {:state "done"
                                 :finished-at "2026-08-23T21:00:00Z"}})
                   (assoc-in [:transition :event/observed-at]
                             "2026-08-23T21:00:00Z"))
        result (watchdog/evaluate closed)]
    (is (= :healthy (:watch/status result)) (pr-str (:watch/findings result)))
    (is (empty? (:watch/findings result)))))

(deftest every-durable-terminal-result-retires-the-last-projected-role-job
  (doseq [frame-result [:closed :partial :void]]
    (let [dir (java.nio.file.Files/createTempDirectory
               "projection-watchdog-terminal-"
               (make-array java.nio.file.attribute.FileAttribute 0))
          frame-dir (.resolve dir "frame")
          terminal-dir (.resolve frame-dir "terminal")
          publication-dir (.resolve frame-dir "publications")
          live-dir (.resolve frame-dir "live")
          transition-log (.resolve frame-dir "problem-transitions.edn")
          coordinator-path (.resolve dir "coordinator.edn")]
      (doseq [path [frame-dir terminal-dir publication-dir live-dir]]
        (java.nio.file.Files/createDirectories
         path (make-array java.nio.file.attribute.FileAttribute 0)))
      (spit (str transition-log) (str (pr-str (:transition healthy)) "\n"))
      (spit (str coordinator-path) (pr-str (:coordinator healthy)))
      (spit (str (.resolve terminal-dir "frame-terminal.edn"))
            (pr-str {:frame/id "f" :frame/result frame-result}))
      (spit (str (.resolve publication-dir "latest.edn"))
            (pr-str (:publication healthy)))
      (spit (str (.resolve live-dir "student-attempt-1.edn"))
            (pr-str {:state/type :live-job-certified}))
      (let [observation (watchdog/observe
                         {:transition-log (str transition-log)
                          :coordinator-state (str coordinator-path)
                          :agency-base "http://127.0.0.1:1"
                          :max-heartbeat-age-seconds 120})]
        (is (true? (:frame-closed? observation)) (name frame-result))
        (is (= {:ok true} (:job observation)) (name frame-result))
        (is (= {:ok true} (:agent observation)) (name frame-result))))))

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

(deftest guide-promotion-review-selects-its-nested-durable-state
  (let [frame-dir (java.nio.file.Path/of "/tmp/frame" (make-array String 0))
        transition {:phase :guide-intervention-1
                    :operation {:role :promotion-proctor}}
        selected (#'watchdog/phase-state-path frame-dir transition)]
    (is (= "/tmp/frame/live/guide-intervention-1-review.edn"
           (str selected)))))

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

(deftest healthy-and-evidence-backed-waiting-are-operational
  (is (watchdog/operational? (watchdog/evaluate healthy)))
  (let [waiting (watchdog/evaluate
                 (assoc healthy :phase-state
                        {:state/type :promotion
                         :stage :awaiting-transport-retry
                         :transport-retry/not-before-ms 1787523000000
                         :transport-retry/attempt 1
                         :transport-retry/max-attempts 3}))]
    (is (= :waiting (:watch/status waiting)))
    (is (watchdog/operational? waiting)))
  (is (not (watchdog/operational?
            (watchdog/evaluate
             (assoc healthy :coordinator-age-seconds 121))))))
