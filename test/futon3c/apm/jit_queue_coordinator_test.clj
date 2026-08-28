(ns futon3c.apm.jit-queue-coordinator-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]
            [futon3c.apm.countdown-control :as countdown]
            [futon3c.apm.durable-coordinator :as durable]
            [futon3c.apm.jit-queue-coordinator :as sut]
            [futon3c.apm.live-preflight-runtime :as runtime]
            [futon3c.apm.semantic-progress-watchdog :as watchdog])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- await-until [pred]
  (loop [attempt 0]
    (cond (pred) true
          (= attempt 150) false
          :else (do (Thread/sleep 20) (recur (inc attempt))))))

(deftest pending-jit-tick-carries-work-budget-deadline
  (let [now 1000000
        period-ms 500
        work-timeout-minutes 30
        work-timeout-ms (* work-timeout-minutes 60 1000)
        config {:coordinator-id "jit-queue:q"
                :queue-name "q"
                :queue-id "queue-id"
                :coordinator/period-ms period-ms
                :tick-work-timeout-minutes work-timeout-minutes}
        decide (:decide-fn (sut/adapter-constructor config))
        state {:state/type :live-regulator
               :regulator/status :running
               :regulator/ticks 7}
        requested (:coordinator/intent (binding [sut/*intent-now-fn* (constantly now)]
                                          (decide state)))
        intent (durable/make-intent "jit-queue:q" state requested)
        observation (durable/watchdog-observation
                     {:coordinator/id "jit-queue:q"
                      :coordinator/enabled? true}
                     (assoc state :coordinator/pending-intent intent))
        watching (watchdog/evaluate nil observation now)
        expired (watchdog/evaluate
                 nil observation
                 (+ now work-timeout-ms
                    watchdog/external-deadline-grace-ms 1))]
    (is (= work-timeout-ms
           (get-in intent [:dispatch/parameters :permitted-duration-ms])))
    (is (= :coordinator/tick-work-timeout-minutes
           (get-in intent [:dispatch/parameters
                           :permitted-duration-source])))
    (is (= (+ now work-timeout-ms)
           (get-in observation [:awaiting-job :deadline])))
    (is (= :watching (:status watching)))
    (is (= :watching
           (:status (watchdog/evaluate
                     nil observation
                     (+ now period-ms
                        watchdog/external-deadline-grace-ms 1)))))
    (is (not= :external-job-deadline-missing
              (get-in watching [:reason :code])))
    (is (= :external-job-deadline-exceeded
           (get-in expired [:reason :code])))))

(deftest pending-jit-tick-defaults-to-student-role-work-budget
  (let [now 2000000
        decide (:decide-fn
                (sut/adapter-constructor
                 {:coordinator-id "jit-queue:q"
                  :queue-name "q" :queue-id "queue-id"
                  :coordinator/period-ms 500}))
        intent (binding [sut/*intent-now-fn* (constantly now)]
                 (:coordinator/intent
                  (decide {:regulator/ticks 1})))]
    (is (= (* 30 60 1000)
           (get-in intent [:dispatch/parameters :permitted-duration-ms])))
    (is (= (+ now (* 30 60 1000))
           (get-in intent [:dispatch/parameters :deadline-ms])))))

(deftest delayed-transport-retry-survives-restart-and-wakes-at-deadline
  (let [clock (atom 1000)
        config {:coordinator-id "jit-queue:q" :queue-name "q"
                :queue-id "queue-id" :coordinator/period-ms 500}
        initial {:state/type :live-regulator :regulator/id "jit-queue:q"
                 :regulator/status :running :regulator/ticks 7}
        adapter (sut/adapter-constructor config)
        requested (binding [sut/*intent-now-fn* #(long @clock)]
                    ((:decide-fn adapter) initial))
        pending (merge initial (:regulator/state-updates requested))
        reconciled
        (with-redefs [countdown/autonomous-problem-list-step!
                      (constantly
                       {:ok true :status :transport-retry-scheduled
                        :retry/not-before-ms 601000
                        :transport-retry {:attempt 1 :max-attempts 3}
                        :transport-retry/history
                        [{:attempt 1 :failed-at-ms 1000
                          :error/component :transport}]})]
          (binding [sut/*intent-now-fn* #(long @clock)]
            ((:reconcile-fn adapter)
             (:coordinator/pending-intent pending) pending)))
        restarted-state (-> pending
                            (merge (:regulator/state-updates reconciled))
                            (assoc :coordinator/pending-intent nil
                                   :coordinator/pending-pre-state-digest nil))
        restarted-adapter (sut/adapter-constructor config)
        waiting (binding [sut/*intent-now-fn* #(long @clock)]
                  (#'durable/coordinator-tick
                   "jit-queue:q" restarted-adapter restarted-state))
        watchdog-observation
        (durable/watchdog-observation
         {:coordinator/id "jit-queue:q" :coordinator/enabled? true}
         restarted-state)]
    (is (= :queue-tick-complete (:status reconciled)))
    (is (= 601000
           (get-in restarted-state
                   [:coordinator/delayed-retry :not-before-ms])))
    (is (= :awaiting-substrate (:status waiting)))
    (is (= 601000 (get-in watchdog-observation [:awaiting-job :deadline])))
    (reset! clock 601000)
    (let [woken (binding [sut/*intent-now-fn* #(long @clock)]
                  (#'durable/coordinator-tick
                   "jit-queue:q" restarted-adapter restarted-state))]
      (is (= :intent-persisted (:status woken)))
      (is (nil? (get-in woken [:regulator/state-updates
                               :coordinator/delayed-retry])))
      (is (= 601000
             (get-in woken [:regulator/state-updates
                            :coordinator/last-woken-retry :woken-at-ms]))))))

(deftest adapter-runs-without-initiator-and-survives-runner-restart
  (let [root (Files/createTempDirectory "jit-coordinator-"
                                        (make-array FileAttribute 0))
        registry (str (.resolve root "registry.edn"))
        state-path (str (.resolve root "state.edn"))
        calls (atom [])
        launch {:problems [{:problem/id "p1"}] :authority {:control-root "/c"}
                :queue-name "q" :queue-id "queue-id"}
        options {:registry-path registry :state-path state-path
                 :coordinator-id "jit-queue:q" :launch launch :period-ms 100}]
    (with-redefs [countdown/autonomous-problem-list-step!
                  (fn [observed]
                    (swap! calls conj observed)
                    (if (= 1 (count @calls))
                      {:ok true :status :parked :job-id "role-job"}
                      {:ok true :status :batch-complete}))]
      ;; The initiating future terminates immediately after registration/start.
      (try
        (is (:ok @(future (sut/start! options))))
        (is (await-until
             #(and (= 1 (count @calls))
                   (string?
                    (get-in (edn/read-string (slurp state-path))
                            [:coordinator/last-settled-intent :job-id])))))
        (is (= :stopped
               (:status (durable/cancel-scheduler! "jit-queue:q"))))
        (let [pending (edn/read-string (slurp state-path))]
          (is (string? (get-in pending [:coordinator/last-settled-intent
                                        :job-id]))))
        (is (:ok (durable/start-registered! registry "jit-queue:q")))
        (is (await-until #(= :complete
                             (get-in (sut/status registry "jit-queue:q")
                                     [:durable-state :regulator/status]))))
        (is (= 2 (count @calls)))
        (is (every? #(nil? (get-in % [:authority :session])) @calls))
        (finally (durable/cancel-scheduler! "jit-queue:q"))))))

(deftest autonomous-list-step-does-not-publish-a-controller-park
  (let [park-calls (atom 0)
        request (atom nil)]
    (with-redefs [countdown/set-alight-problem-queue!
                  (fn [observed _]
                    (reset! request observed)
                    {:ok true :status :frame-prepared})
                  runtime/http-json
                  (fn [& _] (swap! park-calls inc)
                    {:ok true :http/status 200})]
      (is (= :frame-prepared
             (:status
              (countdown/autonomous-problem-list-step!
               {:problems [] :authority {:control-root "/home/joe/code/futon3c"
                                         :apparatus-root "/home/joe/code/futon3c"}
                :queue-name "test"}))))
      (is (zero? @park-calls))
      (is (= "countdown-regulator:durable-jit"
             (get-in @request [:authority :regulator-id])))
      (is (some? (get-in @request [:authority :regulator-capability]))))))
