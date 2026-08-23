(ns futon3c.apm.library-lane-coordinator-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.apm.durable-coordinator :as durable]
            [futon3c.apm.library-lane-adapters :as adapters]
            [futon3c.apm.library-lane-coordinator :as sut]
            [futon3c.apm.library-lane-effects :as effects]
            [futon3c.apm.library-lane-launch :as launch]
            [futon3c.apm.library-lane-runner :as runner])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- await-until [pred]
  (loop [n 0]
    (cond (pred) true (= n 200) false
          :else (do (Thread/sleep 20) (recur (inc n))))))

(defn- fixture []
  (let [root (Files/createTempDirectory "library-coordinator-"
                                        (make-array FileAttribute 0))]
    {:registry (str (.resolve root "registry.edn"))
     :state (str (.resolve root "state.edn"))}))

(deftest awaiting-step-survives-runner-restart-with-identical-intent
  (let [{:keys [registry state]} (fixture)
        calls (atom [])
        options {:registry-path registry :state-path state
                 :coordinator-id "library:t00J02" :problem-id "t00J02"
                 :period-ms 100}]
    (with-redefs [sut/run-step!
                  (fn [_]
                    (swap! calls conj
                           (get-in (sut/status registry "library:t00J02")
                                   [:durable-state :coordinator/pending-intent]))
                    (if (< (count @calls) 3)
                      {:ok true :ruling :awaiting}
                      {:ok true :ruling :closed}))]
      (try
        (is (:ok @(future (sut/start! options))))
        (is (await-until #(= 1 (count @calls))))
        (is (= :stopped (:status (durable/stop! "library:t00J02"))))
        (is (:ok (durable/start-registered! registry "library:t00J02")))
        (is (await-until #(= :complete
                             (get-in (sut/status registry "library:t00J02")
                                     [:durable-state :regulator/status]))))
        (is (= 3 (count @calls)))
        (is (apply = (map #(select-keys % [:job-id :dispatch/id]) @calls)))
        (finally (durable/stop! "library:t00J02"))))))

(deftest partial-bank-clears-intent-and-mints-successor
  (let [{:keys [registry state]} (fixture)
        calls (atom [])
        options {:registry-path registry :state-path state
                 :coordinator-id "library:partial" :problem-id "t00J02"
                 :period-ms 10}]
    (with-redefs [sut/run-step!
                  (fn [_]
                    (swap! calls conj
                           (get-in (sut/status registry "library:partial")
                                   [:durable-state :coordinator/pending-intent]))
                    (if (= 1 (count @calls))
                      {:ok true :ruling :partial-banked}
                      {:ok true :ruling :closed}))]
      (try
        (is (:ok (sut/start! options)))
        (is (await-until #(= 2 (count @calls))))
        (is (not= (get-in @calls [0 :job-id])
                  (get-in @calls [1 :job-id])))
        (is (not= (get-in @calls [0 :dispatch/id])
                  (get-in @calls [1 :dispatch/id])))
        (is (true? (get-in (sut/status registry "library:partial")
                           [:durable-state :library/strategy-required?])))
        (finally (durable/stop! "library:partial"))))))

(deftest each-phase-has-a-distinct-durable-intent
  (let [{:keys [registry state]} (fixture)
        observations (atom [])
        options {:registry-path registry :state-path state
                 :coordinator-id "library:phases" :problem-id "t00J02"
                 :period-ms 10}]
    (with-redefs [sut/run-step!
                  (fn [{:keys [phase]}]
                    (swap! observations conj
                           [phase (get-in (sut/status registry "library:phases")
                                          [:durable-state
                                           :coordinator/pending-intent
                                           :job-id])])
                    (if (= :bank phase)
                      {:ok true :ruling :closed}
                      {:ok true :ruling :phase-certified :phase phase}))]
      (try
        (is (:ok (sut/start! options)))
        (is (await-until #(= 4 (count @observations))))
        (is (= [:preflight :solve :verify :bank]
               (mapv first @observations)))
        (is (= 4 (count (distinct (map second @observations)))))
        (finally (durable/stop! "library:phases"))))))

(deftest phase-state-drift-prevents-external-step
  (let [calls (atom 0)
        reconcile (:reconcile-fn (sut/adapter-constructor {}))]
    (with-redefs [sut/run-step! (fn [_] (swap! calls inc)
                                 {:ok true :ruling :phase-certified})]
      (let [result (reconcile {:dispatch/parameters {:phase :solve}}
                              {:library/phase :preflight})]
        (is (false? (:ok result)))
        (is (= :library-lane-phase-intent-drift (:error/code result)))
        (is (zero? @calls))))))

(deftest run-step-resumes-persisted-config-before-workspace-preparation
  (let [launches (atom 0)
        resumed-config {:seats {:solver {:agent-id "solver"}}
                        :outcome-fn (fn [_] :outcome)}]
    (with-redefs [effects/live-effects
                  (fn [_] {:observe-problem-fn
                           (fn [_] {:ok true :problem {:revision "rev"}})
                           :outcome-fn (:outcome-fn resumed-config)})
                  launch/resume-config
                  (fn [request]
                    (is (= "rev" (:revision request)))
                    resumed-config)
                  launch/launch!
                  (fn [_] (swap! launches inc) {:ok false})
                  adapters/make-phase-inputs-fn
                  (constantly :phase-inputs)
                  adapters/make-bank-request-fn
                  (constantly :bank-request)
                  runner/step-one!
                  (fn [request]
                    (is (= :solve (:phase-limit request)))
                    (is (= :phase-inputs (:phase-inputs-fn request)))
                    {:ok true :ruling :awaiting})]
      (is (= {:ok true :ruling :awaiting}
             (sut/run-step! {:agency-base "agency" :corpus-root "corpus"
                             :frames-root "frames" :state-root "state"
                             :problem-id "t00J02" :trunk-branch "trunk"
                             :keying-target "target"
                             :contract-path
                             "holes/labs/M-apm-demonstration/frame-cycle-contract-codex-only-v1.edn"
                             :phase :solve})))
      (is (zero? @launches)))))

(deftest legacy-pending-intent-migration-preserves-dispatch-identity
  (let [{:keys [registry state]} (fixture)
        options {:registry-path registry :state-path state
                 :coordinator-id "library:migrate" :problem-id "t00J02"
                 :period-ms 1000}]
    (with-redefs [sut/run-step! (fn [_] {:ok true :ruling :awaiting})]
      (try
        (is (:ok (sut/start! options)))
        (is (await-until #(some? (get-in (sut/status registry "library:migrate")
                                         [:durable-state
                                          :coordinator/pending-intent]))))
        (durable/stop! "library:migrate")
        (let [before (get-in (sut/status registry "library:migrate")
                             [:durable-state :coordinator/pending-intent])
              legacy (-> before
                         (dissoc :dispatch/parameters :intent/digest)
                         (#(assoc % :intent/digest
                                  (durable/intent-digest %))))]
          (spit state (pr-str (assoc (:durable-state
                                     (sut/status registry "library:migrate"))
                                    :coordinator/pending-intent legacy)))
          (is (:ok (sut/migrate-pending-phase-intent!
                    registry "library:migrate")))
          (let [after (get-in (sut/status registry "library:migrate")
                              [:durable-state :coordinator/pending-intent])]
            (is (= (select-keys before [:job-id :dispatch/id
                                       :pre-state/version :pre-state/digest])
                   (select-keys after [:job-id :dispatch/id
                                      :pre-state/version :pre-state/digest])))
            (is (= {:phase :preflight} (:dispatch/parameters after)))
            (is (= {:ruling/one-of sut/phase-rulings}
                   (:expected/postcondition after)))
            (is (durable/valid-intent?
                 "library:migrate"
                 (:durable-state (sut/status registry "library:migrate"))
                 after))))
        (finally (durable/stop! "library:migrate"))))))
