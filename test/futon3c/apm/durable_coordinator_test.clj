(ns futon3c.apm.durable-coordinator-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]
            [futon3c.apm.durable-coordinator :as sut])
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
