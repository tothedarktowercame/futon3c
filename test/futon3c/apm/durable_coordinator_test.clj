(ns futon3c.apm.durable-coordinator-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]
            [futon3c.apm.durable-coordinator :as sut]
            [futon3c.apm.live-regulator :as regulator])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- temp-paths []
  (let [root (Files/createTempDirectory "durable-coordinator-"
                                        (make-array FileAttribute 0))]
    {:registry (str (.resolve root "registry.edn"))
     :state-a (str (.resolve root "a.edn"))
     :state-b (str (.resolve root "b.edn"))}))

(defn- await-until [pred]
  (loop [attempt 0]
    (cond (pred) true
          (= attempt 100) false
          :else (do (Thread/sleep 20) (recur (inc attempt))))))

(defn- state-status [path]
  (let [file (java.io.File. path)]
    (when (.isFile file)
      (some-> path slurp edn/read-string :regulator/status))))

(defn- registered-entry [registry coordinator-id]
  (get-in (sut/read-registry registry) [:entries coordinator-id]))

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
      (with-redefs [regulator/stop! (fn [_] {:ok true :status :stopped})]
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
                    regulator/stop! (fn [_] {:ok true :status :stopped})]
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
      (finally (sut/stop! "c:a")))))

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
      (finally (sut/stop! "c:restart")))))

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
      (finally (sut/stop! "c:reject")))))

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
                             :state-path state-a :period-ms 100})))
    (try
      (is (:ok (sut/start-registered! registry "c:multi")))
      (is (await-until #(= 1 (count @reconciled))))
      (is (= :stopped (:status (sut/stop! "c:multi"))))
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
      (finally (sut/stop! "c:multi")))))

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
      (finally (sut/stop! "c:a") (sut/stop! "c:b")))))

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
      (is (:ok (sut/start-registered! registry "c:stop")))
      (is (await-until #(some? (sut/status "c:stop"))))
      (is (:durably-disabled? (sut/stop! registry "c:stop")))
      (is (= :disabled
             (get-in (sut/recover-all! registry) [:results "c:stop" :status])))
      (is (false? (get-in (sut/status registry "c:stop")
                          [:registration :coordinator/enabled?])))
      (finally (sut/stop! "c:stop")))))

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
      (finally (sut/stop! "c:postcondition")))))
