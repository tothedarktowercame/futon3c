(ns futon3c.apm.durable-coordinator-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is use-fixtures]]
            [futon3c.apm.durable-coordinator :as sut]
            [futon3c.apm.live-preflight-runtime :as persistence]
            [futon3c.apm.live-regulator :as regulator]
            [futon3c.apm.semantic-progress-watchdog :as watchdog])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- temp-paths []
  (let [root (Files/createTempDirectory "durable-coordinator-"
                                        (make-array FileAttribute 0))]
    {:registry (str (.resolve root "registry.edn"))
     :state-a (str (.resolve root "a.edn"))
     :state-b (str (.resolve root "b.edn"))}))

(defn- clear-runner-registry! [registry entries]
  (doseq [entry entries
          :let [executor (if (map? entry) (:executor entry) entry)]
          :when executor]
    (.shutdownNow executor))
  (reset! registry {}))

(defn- clear-runners! []
  (let [regulator-runners (var-get #'regulator/runners)
        watchdog-runners (var-get #'watchdog/runners)]
    (clear-runner-registry! regulator-runners (vals @regulator-runners))
    (clear-runner-registry! watchdog-runners (vals @watchdog-runners))))

(use-fixtures :each
  (fn [test-fn]
    (let [clock (atom 0)]
      (clear-runners!)
      (try
        (binding [sut/*watchdog-now-fn* #(swap! clock + 20)
                  sut/*enabled-transition-now-fn* #(swap! clock + 20)]
          (test-fn))
        (finally (clear-runners!))))))

(defn- await-until [pred]
  (let [deadline (+ (sut/*watchdog-now-fn*) 200000)]
    (loop []
      (cond
        (pred) true
        (>= (sut/*watchdog-now-fn*) deadline) false
        :else (do (Thread/yield) (recur))))))

(defn- state-status [path]
  (let [file (java.io.File. path)]
    (when (.isFile file)
      (some-> path slurp edn/read-string :regulator/status))))

(defn- registered-entry [registry coordinator-id]
  (get-in (sut/read-registry registry) [:entries coordinator-id]))

(deftest enabled-transitions-are-append-only-and-preserve-current-read-path
  (let [{:keys [registry state-a]} (temp-paths)]
    (is (:ok (sut/register! {:registry-path registry :coordinator-id "c:history"
                             :adapter :test/none :config {}
                             :state-path state-a :period-ms 10})))
    (with-redefs [regulator/cancel-scheduler! (fn [_] {:ok true :status :stopped})
                  sut/start-registered! (fn [_ _] {:ok true :status :started})]
      (is (:durably-disabled? (sut/stop! registry "c:history")))
      (is (:ok (sut/resume! registry "c:history")))
      (is (:durably-disabled? (sut/stop! registry "c:history")))
      (let [entry (registered-entry registry "c:history")
            history (:coordinator/enabled-history entry)]
        (is (false? (:coordinator/enabled? entry)))
        (is (= [[nil true] [true false] [false true] [true false]]
               (mapv (juxt :enabled/previous :enabled/new) history)))
        (is (= [:durable-coordinator/register!
                :durable-coordinator/stop!
                :durable-coordinator/resume!
                :durable-coordinator/stop!]
               (mapv :transition/actor history)))
        (is (every? string? (map :durable-state/digest history)))
        (is (apply < (map :transition/timestamp-ms history)))))))

(deftest failed-history-write-blocks-enabled-transition-and-successor
  (let [{:keys [registry state-a]} (temp-paths)
        stopped (atom 0)
        started (atom 0)]
    (is (:ok (sut/register! {:registry-path registry :coordinator-id "c:blocked"
                             :adapter :test/none :config {}
                             :state-path state-a :period-ms 10})))
    (let [before (sut/read-registry registry)]
      (with-redefs [persistence/atomic-persist!
                    (fn [& _] {:ok false :error/code :test/archive-failed})
                    regulator/cancel-scheduler! (fn [_] (swap! stopped inc)
                                      {:ok true :status :stopped})
                    sut/start-registered! (fn [& _] (swap! started inc)
                                            {:ok true :status :started})]
        (is (= :test/archive-failed
               (:error/code (sut/stop! registry "c:blocked"))))
        (is (= before (sut/read-registry registry)))
        (is (zero? @stopped))
        (is (= :test/archive-failed
               (:error/code (sut/resume! registry "c:blocked"))))
        (is (= before (sut/read-registry registry)))
        (is (zero? @started))))))

(deftest status-reports-the-durable-tick-claim-and-reconciliation
  (let [{:keys [registry state-a]} (temp-paths)
        claim {:state/type :live-regulator-tick-claim
               :regulator/id "c:status" :tick/epoch 3 :tick/ordinal 9
               :tick/id "c:status:3:9" :tick/claimed-at "claimed"}]
    (is (:ok (sut/register! {:registry-path registry
                             :coordinator-id "c:status"
                             :adapter :test/none :config {}
                             :state-path state-a :period-ms 10})))
    (spit state-a (str (pr-str (assoc (regulator/initial-state "c:status")
                                     :regulator/tick-claim claim
                                     :regulator/reconciliation :required))
                       "\n"))
    (let [observed (sut/status registry "c:status")]
      (is (= claim (:tick-claim observed)))
      (is (= :required (:reconciliation/status observed)))
      (is (= claim (get-in observed
                           [:durable-state :regulator/tick-claim]))))))

(deftest coordinator-start-arms-independent-watchdog
  (let [{:keys [registry state-a]} (temp-paths)
        armed (atom [])]
    (sut/register-adapter!
     :test/watchdog
     (fn [_] {:decide-fn (fn [_] {:ok true :status :idle})
              :reconcile-fn (fn [_ _] {:ok true :status :idle})}))
    (is (:ok (sut/register! {:registry-path registry :coordinator-id "c:watch"
                             :adapter :test/watchdog :config {}
                             :state-path state-a :period-ms 10})))
    (binding [sut/*watchdog-start-fn*
              (fn [request]
                (swap! armed conj request)
                {:ok true :status :started})
              sut/*watchdog-running-fn* (constantly true)]
      (with-redefs [regulator/start! (fn [_] {:ok true :status :started})]
        (is (:ok (sut/start-entry! registry
                                   (registered-entry registry "c:watch"))))
        (is (= ["semantic-progress:c:watch"]
               (mapv :watchdog-id @armed)))
        (is (fn? (:watch-fn (first @armed))))))))

(deftest durable-stop-disarms-watchdog
  (let [{:keys [registry state-a]} (temp-paths)
        stopped (atom [])]
    (is (:ok (sut/register! {:registry-path registry :coordinator-id "c:watch"
                             :adapter :test/none :config {}
                             :state-path state-a :period-ms 10})))
    (binding [sut/*watchdog-stop-fn*
              (fn [id]
                (swap! stopped conj id)
                {:ok true :status :stopped})]
      (with-redefs [regulator/cancel-scheduler! (fn [_] {:ok true :status :stopped})]
        (let [result (sut/stop! registry "c:watch")]
          (is (:durably-disabled? result))
          (is (= ["semantic-progress:c:watch"] @stopped)))))))

(deftest running-without-live-watchdog-is-durably-halted
  (let [{:keys [registry state-a]} (temp-paths)
        stopped (atom [])]
    (sut/register-adapter!
     :test/unwatched
     (fn [_] {:decide-fn (fn [_] {:ok true :status :idle})
              :reconcile-fn (fn [_ _] {:ok true :status :idle})}))
    (is (:ok (sut/register! {:registry-path registry :coordinator-id "c:bare"
                             :adapter :test/unwatched :config {}
                             :state-path state-a :period-ms 10})))
    (binding [sut/*watchdog-start-fn* (fn [_] {:ok true :status :started})
              sut/*watchdog-running-fn* (constantly false)
              sut/*watchdog-stop-fn*
              (fn [id] (swap! stopped conj id) {:ok true :status :stopped})]
      (with-redefs [regulator/start! (fn [_] {:ok true :status :started})
                    regulator/cancel-scheduler! (fn [_] {:ok true :status :stopped})]
        (let [result (sut/start-registered! registry "c:bare")]
          (is (= :durable-coordinator-running-unwatched
                 (:error/code result)))
          (is (false? (get-in (sut/read-registry registry)
                              [:entries "c:bare" :coordinator/enabled?])))
          (is (= ["semantic-progress:c:bare"] @stopped)))))))

(deftest recovery-rearms-watchdog-and-disabled-entry-does-not-arm
  (let [{:keys [registry state-a state-b]} (temp-paths)
        armed (atom [])]
    (sut/register-adapter!
     :test/rearm
     (fn [_] {:decide-fn (fn [_] {:ok true :status :idle})
              :reconcile-fn (fn [_ _] {:ok true :status :idle})}))
    (doseq [[id state] [["c:enabled" state-a] ["c:disabled" state-b]]]
      (is (:ok (sut/register! {:registry-path registry :coordinator-id id
                               :adapter :test/rearm :config {}
                               :state-path state :period-ms 10}))))
    (is (:durably-disabled? (sut/stop! registry "c:disabled")))
    (binding [sut/*watchdog-start-fn*
              (fn [request]
                (swap! armed conj (:watchdog-id request))
                {:ok true :status :started})
              sut/*watchdog-running-fn* (constantly true)
              sut/*watchdog-stop-fn* (fn [_] {:ok true :status :not-running})]
      (with-redefs [regulator/start! (fn [_] {:ok true :status :started})]
        (is (:ok (sut/recover-all! registry)))
        (is (:ok (sut/recover-all! registry)))
        (is (= ["semantic-progress:c:enabled"
                "semantic-progress:c:enabled"] @armed))))))

(deftest activation-intent-is-persisted-before-reconcile
  (let [{:keys [registry state-a]} (temp-paths)
        observations (atom [])]
    (sut/register-adapter!
     :test/two-phase
     (fn [_]
       {:decide-fn (fn [_]
                     (swap! observations conj [:decide (Files/exists
                                                        (java.nio.file.Path/of state-a (make-array String 0))
                                                        (make-array java.nio.file.LinkOption 0))])
                     {:ok true :coordinator/action :activate
                      :coordinator/intent
                      {:job-id "job-fixed"
                       :dispatch/id "dispatch-fixed"
                       :dispatch/action :invoke
                       :expected/postcondition {:job/state :terminal}}})
        :reconcile-fn (fn [intent state]
                        (swap! observations conj
                               [:reconcile intent (:coordinator/pending-intent state)])
                        {:ok true :status :frame-complete
                         :coordinator/clear-intent? true})}))
    (is (:ok (sut/register! {:registry-path registry :coordinator-id "c:a"
                             :adapter :test/two-phase :config {}
                             :state-path state-a :period-ms 10})))
    (try
      (is (:ok (sut/start-registered! registry "c:a")))
      (is (await-until #(= :complete (state-status state-a))))
      (let [[_ reconcile] @observations]
        (is (= "job-fixed" (get-in reconcile [1 :job-id])))
        (is (= (reconcile 1) (reconcile 2))))
      (finally (sut/cancel-scheduler! "c:a")))))

(deftest restart-reconciles-persisted-intent-with-same-job-id
  (let [{:keys [registry state-a]} (temp-paths)
        reconciled (atom [])]
    (sut/register-adapter!
     :test/restart
     (fn [_]
       {:decide-fn (fn [_] {:ok true :status :awaiting-job})
        :reconcile-fn (fn [intent _]
                        (swap! reconciled conj (:job-id intent))
                        {:ok true :status :frame-complete
                         :coordinator/clear-intent? true})}))
    (is (:ok (sut/register! {:registry-path registry :coordinator-id "c:restart"
                             :adapter :test/restart :config {}
                             :state-path state-a :period-ms 10})))
    (let [pre-state {:state/type :live-regulator
                     :regulator/id "c:restart"
                     :regulator/status :running
                     :regulator/ticks 0}
          intent (sut/make-intent
                  "c:restart" pre-state
                  {:job-id "job-stable" :dispatch/id "dispatch-stable"
                   :dispatch/action :invoke
                   :expected/postcondition {:job/state :terminal}})]
      (spit state-a (str (pr-str (assoc pre-state
                                        :regulator/ticks 1
                                        :coordinator/pending-intent intent
                                        :coordinator/pending-pre-state-digest
                                        (:pre-state/digest intent)))
                         "\n")))
    (try
      (is (:ok (sut/start-registered! registry "c:restart")))
      (is (await-until #(contains? #{:complete :failed}
                                    (state-status state-a))))
      (is (= :complete (state-status state-a)) (slurp state-a))
      (is (= ["job-stable"] @reconciled))
      (finally (sut/cancel-scheduler! "c:restart")))))

(deftest typed-intent-integrity-kills-field-mutations
  (let [pre-state {:state/type :live-regulator
                   :regulator/id "c:bound"
                   :regulator/status :running
                   :regulator/ticks 4}
        intent (sut/make-intent
                "c:bound" pre-state
                {:job-id "job-4" :dispatch/id "dispatch-4"
                 :dispatch/action :invoke
                 :expected/postcondition {:job/state :terminal}})
        persisted (assoc pre-state
                         :regulator/ticks 5
                         :coordinator/pending-intent intent
                         :coordinator/pending-pre-state-digest
                         (:pre-state/digest intent))
        redigest #(assoc % :intent/digest (sut/intent-digest %))]
    (is (sut/valid-intent? "c:bound" persisted intent))
    (is (false? (sut/valid-intent? "c:bound" persisted
                                  (assoc intent :job-id "job-injected"))))
    (is (false? (sut/valid-intent? "c:bound" persisted
                                  (redigest (assoc intent :coordinator/id
                                                  "c:other")))))
    (is (false? (sut/valid-intent? "c:bound" persisted
                                  (redigest (assoc intent :pre-state/digest
                                                  "wrong-state")))))
    (is (false? (sut/valid-intent? "c:bound" persisted
                                  (redigest (dissoc intent :dispatch/id)))))
    (is (false? (sut/valid-intent?
                 "c:bound" persisted
                 (assoc intent :expected/postcondition {:job/state :queued}))))))

(deftest invalid-persisted-intent-never-reaches-reconcile
  (let [{:keys [registry state-a]} (temp-paths)
        reconcile-calls (atom 0)
        pre-state {:state/type :live-regulator :regulator/id "c:reject"
                   :regulator/status :running :regulator/ticks 0}
        intent (sut/make-intent
                "c:reject" pre-state
                {:job-id "job-original" :dispatch/id "dispatch-original"
                 :dispatch/action :invoke
                 :expected/postcondition {:job/state :terminal}})]
    (sut/register-adapter!
     :test/reject-tamper
     (fn [_] {:decide-fn (fn [_] {:ok true :status :awaiting-job})
              :reconcile-fn (fn [_ _]
                              (swap! reconcile-calls inc)
                              {:ok true :status :frame-complete})}))
    (is (:ok (sut/register! {:registry-path registry
                             :coordinator-id "c:reject"
                             :adapter :test/reject-tamper :config {}
                             :state-path state-a :period-ms 10})))
    (spit state-a
          (str (pr-str (assoc pre-state
                              :regulator/ticks 1
                              :coordinator/pending-intent
                              (assoc intent :job-id "job-injected")
                              :coordinator/pending-pre-state-digest
                              (:pre-state/digest intent)))
               "\n"))
    (try
      (is (:ok (sut/start-registered! registry "c:reject")))
      (is (await-until #(= :failed (state-status state-a))))
      (is (zero? @reconcile-calls))
      (is (= [:intent-digest]
             (get-in (edn/read-string (slurp state-a))
                     [:regulator/last-result :findings])))
      (finally (sut/cancel-scheduler! "c:reject")))))

(deftest pending-intent-survives-multiple-polls-and-runner-restart
  (let [{:keys [registry state-a]} (temp-paths)
        reconciled (atom [])]
    (sut/register-adapter!
     :test/multi-poll
     (fn [_]
       {:decide-fn
        (fn [_]
          {:ok true :coordinator/action :activate
           :coordinator/intent
           {:job-id "job-multi" :dispatch/id "dispatch-multi"
            :dispatch/action :invoke
            :expected/postcondition {:job/state :terminal}}})
        :reconcile-fn
        (fn [intent _]
          (let [poll (count (swap! reconciled conj
                                   (select-keys intent [:job-id :dispatch/id])))]
            (if (< poll 3)
              {:ok true :status :awaiting-job}
              {:ok true :status :frame-complete
               :coordinator/clear-intent? true})))}))
    (is (:ok (sut/register! {:registry-path registry
                             :coordinator-id "c:multi"
                             :adapter :test/multi-poll :config {}
                             :state-path state-a :period-ms 1})))
    (try
      (is (:ok (sut/start-registered! registry "c:multi")))
      (is (await-until
           #(let [file (java.io.File. state-a)
                  state (when (.isFile file)
                          (edn/read-string (slurp file)))]
              (and (= 1 (count @reconciled))
                   (>= (:regulator/ticks state 0) 2)))))
      (is (= :stopped (:status (sut/cancel-scheduler! "c:multi"))))
      (let [stopped (edn/read-string (slurp state-a))]
        (is (= "job-multi"
               (get-in stopped [:coordinator/pending-intent :job-id])))
        (is (>= (:regulator/ticks stopped) 2)))
      (is (:ok (sut/start-registered! registry "c:multi")))
      (is (await-until #(= :complete (state-status state-a))))
      (is (= 3 (count @reconciled)))
      (is (apply = @reconciled))
      (is (= {:job-id "job-multi" :dispatch/id "dispatch-multi"}
             (first @reconciled)))
      (finally (sut/cancel-scheduler! "c:multi")))))

(deftest pending-intent-rejects-rewound-tick
  (let [pre-state {:state/type :live-regulator :regulator/id "c:rewind"
                   :regulator/status :running :regulator/ticks 7}
        intent (sut/make-intent
                "c:rewind" pre-state
                {:job-id "job-7" :dispatch/id "dispatch-7"
                 :dispatch/action :invoke
                 :expected/postcondition {:job/state :terminal}})
        rewound (assoc pre-state
                       :coordinator/pending-intent intent
                       :coordinator/pending-pre-state-digest
                       (:pre-state/digest intent))]
    (is (false? (sut/valid-intent? "c:rewind" rewound intent)))
    (is (= [:pre-state-version-relationship]
           (sut/intent-findings "c:rewind" rewound intent)))))

(deftest typed-registry-recovers-two-coordinators-without-directory-discovery
  (let [{:keys [registry state-a state-b]} (temp-paths)
        ticks (atom {})]
    (sut/register-adapter!
     :test/concurrent
     (fn [{:keys [name]}]
       {:decide-fn (fn [_]
                     (swap! ticks update name (fnil inc 0))
                     {:ok true :status :awaiting-job})
        :reconcile-fn (fn [_ _] {:ok true :status :awaiting-job})}))
    (doseq [[id name state] [["c:a" :a state-a] ["c:b" :b state-b]]]
      (is (:ok (sut/register! {:registry-path registry :coordinator-id id
                               :adapter :test/concurrent :config {:name name}
                               :state-path state :period-ms 10}))))
    (try
      (let [result (sut/recover-all! registry)]
        (is (:ok result))
        (is (= #{"c:a" "c:b"} (set (keys (:results result))))))
      (is (await-until #(and (pos? (get @ticks :a 0))
                             (pos? (get @ticks :b 0)))))
      (finally (sut/cancel-scheduler! "c:a")
               (sut/cancel-scheduler! "c:b")))))

(deftest conflicting-or-tampered-registration-fails-closed
  (let [{:keys [registry state-a state-b]} (temp-paths)
        registration {:registry-path registry :coordinator-id "c:a"
                      :adapter :test/none :config {} :state-path state-a
                      :period-ms 10}]
    (is (:ok (sut/register! registration)))
    (is (= :durable-coordinator-registration-conflict
           (:error/code (sut/register! (assoc registration :state-path state-b)))))
    (let [contents (slurp registry)]
      (spit registry (.replace contents state-a state-b)))
    (is (= :durable-coordinator-registry-invalid
           (:error/code (sut/recover-all! registry))))))

(deftest one-problem-has-one-bounded-retrying-coordinator
  (let [{:keys [registry state-a state-b]} (temp-paths)
        base {:registry-path registry :coordinator-id "c:problem"
              :problem-id "m94A03" :retry-max 2
              :adapter :test/none :config {} :state-path state-a
              :period-ms 10}]
    (is (:ok (sut/register! base)))
    (is (= :durable-coordinator-problem-already-registered
           (:error/code
            (sut/register! (assoc base :coordinator-id "c:problem-retry-v2"
                                  :state-path state-b)))))
    (is (= 1 (get-in (sut/retry! registry "c:problem")
                     [:entry :retry/count])))
    (is (= 2 (get-in (sut/retry! registry "c:problem")
                     [:entry :retry/count])))
    (is (= :durable-coordinator-retry-exhausted
           (:error/code (sut/retry! registry "c:problem"))))
    (is (= 1 (count (:entries (sut/read-registry registry)))))))

(deftest durable-stop-prevents-startup-recovery
  (let [{:keys [registry state-a]} (temp-paths)]
    (sut/register-adapter!
     :test/stoppable
     (fn [_] {:decide-fn (fn [_] {:ok true :status :awaiting-job})
              :reconcile-fn (fn [_ _] {:ok true :status :awaiting-job})}))
    (is (:ok (sut/register! {:registry-path registry :coordinator-id "c:stop"
                             :adapter :test/stoppable :config {}
                             :state-path state-a :period-ms 10})))
    (try
      (let [started (sut/start-registered! registry "c:stop")]
        (is (:ok started))
      (is (await-until #(some? (sut/status "c:stop"))))
        (let [first-stop (sut/stop! registry "c:stop")]
          (is (:durably-disabled? first-stop))
          (when (= :draining (:status first-stop))
            (is (string? (get-in first-stop [:in-flight-tick :tick/id])))
            (is (not= :timeout
                      (deref (:first-tick started) 2000 :timeout)))
            (is (= :stopped
                   (:status (sut/stop! registry "c:stop"))))))
      (is (= :disabled
             (get-in (sut/recover-all! registry) [:results "c:stop" :status])))
      (is (false? (get-in (sut/status registry "c:stop")
                            [:registration :coordinator/enabled?]))))
      (finally (sut/cancel-scheduler! "c:stop")))))

(deftest stop-exposes-draining-claim-before-durable-quiescence
  (let [{:keys [registry state-a]} (temp-paths)
        coordinator-id "c:drain-observer"
        tick-entered (promise)
        release-tick (promise)]
    (sut/register-adapter!
     :test/drain-observer
     (fn [_]
       {:decide-fn (fn [_]
                     (deliver tick-entered true)
                     @release-tick
                     {:ok true :status :awaiting-job})
        :reconcile-fn (fn [_ _] {:ok true :status :awaiting-job})}))
    (is (:ok (sut/register! {:registry-path registry
                             :coordinator-id coordinator-id
                             :adapter :test/drain-observer :config {}
                             :state-path state-a :period-ms 1000})))
    (try
      (let [started (sut/start-registered! registry coordinator-id)]
        (is (= true (deref tick-entered 2000 :timeout)))
        (let [stop-result (future (sut/stop! registry coordinator-id))
              observer (future
                         (loop []
                           (let [registration
                                 (get-in (sut/read-registry registry)
                                         [:entries coordinator-id])]
                             (if (= :draining
                                    (:coordinator/lifecycle registration))
                               (edn/read-string (slurp state-a))
                               (do (Thread/yield) (recur))))))
              observed (deref observer 2000 :timeout)
              draining (deref stop-result 2000 :timeout)]
          (is (map? observed))
          (is (= :live-regulator-tick-claim
                 (get-in observed [:regulator/tick-claim :state/type])))
          (is (not= :stopped (:regulator/status observed)))
          (is (nil? (:regulator/quiescence-witness observed)))
          (is (= :draining (:status draining)))
          (is (= (get-in observed [:regulator/tick-claim :tick/id])
                 (get-in draining [:in-flight-tick :tick/id])))
          (is (= (get-in draining [:in-flight-tick :tick/id])
                 (get-in (sut/recover-all! registry)
                         [:results coordinator-id :in-flight-tick :tick/id])))
          (deliver release-tick true)
          (is (not= :timeout (deref (:first-tick started) 2000 :timeout)))
          (let [stopped (sut/stop! registry coordinator-id)
                durable (edn/read-string (slurp state-a))]
            (is (= :stopped (:status stopped)))
            (is (= :stopped (:regulator/status durable)))
            (is (nil? (:regulator/tick-claim durable)))
            (is (= :durable-quiescence-witness
                   (get-in durable
                           [:regulator/quiescence-witness :state/type])))
            (is (nil? (get-in durable
                              [:regulator/quiescence-witness :tick-claim])))
            (is (= :disabled
                   (get-in (sut/recover-all! registry)
                           [:results coordinator-id :status]))))))
      (finally
        (deliver release-tick true)
        (sut/cancel-scheduler! coordinator-id)))))

(deftest unexpected-postcondition-fails-before-state-advance
  (let [{:keys [registry state-a]} (temp-paths)]
    (sut/register-adapter!
     :test/postcondition
     (fn [_]
       {:decide-fn
        (fn [_] {:ok true :coordinator/action :activate
                 :coordinator/intent
                 {:job-id "job-post" :dispatch/id "dispatch-post"
                  :dispatch/action :invoke :dispatch/parameters {}
                  :expected/postcondition {:ruling/one-of [:closed]}}})
        :reconcile-fn
        (fn [_ _] {:ok true :status :done
                   :coordinator/clear-intent? true
                   :lane/result {:ruling :partial-banked}
                   :regulator/state-updates {:forbidden/advance true}})}))
    (is (:ok (sut/register! {:registry-path registry
                             :coordinator-id "c:postcondition"
                             :adapter :test/postcondition :config {}
                             :state-path state-a :period-ms 10})))
    (try
      (is (:ok (sut/start-registered! registry "c:postcondition")))
      (is (await-until #(= :failed (state-status state-a))))
      (let [state (edn/read-string (slurp state-a))]
        (is (= :durable-coordinator-postcondition-violated
               (get-in state [:regulator/last-result :error/code])))
        (is (nil? (:forbidden/advance state)))
        (is (some? (:coordinator/pending-intent state))))
      (finally (sut/cancel-scheduler! "c:postcondition")))))
