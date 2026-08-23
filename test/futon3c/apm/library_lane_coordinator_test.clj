(ns futon3c.apm.library-lane-coordinator-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.apm.durable-coordinator :as durable]
            [futon3c.apm.library-lane-coordinator :as sut])
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
