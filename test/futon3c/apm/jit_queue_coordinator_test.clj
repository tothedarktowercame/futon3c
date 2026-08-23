(ns futon3c.apm.jit-queue-coordinator-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]
            [futon3c.apm.countdown-control :as countdown]
            [futon3c.apm.durable-coordinator :as durable]
            [futon3c.apm.jit-queue-coordinator :as sut]
            [futon3c.apm.live-preflight-runtime :as runtime])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- await-until [pred]
  (loop [attempt 0]
    (cond (pred) true
          (= attempt 150) false
          :else (do (Thread/sleep 20) (recur (inc attempt))))))

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
        (is (await-until #(= 1 (count @calls))))
        (is (= :stopped
               (:status (durable/stop! "jit-queue:q"))))
        (let [pending (edn/read-string (slurp state-path))]
          (is (string? (get-in pending [:coordinator/last-settled-intent
                                        :job-id]))))
        (is (:ok (durable/start-registered! registry "jit-queue:q")))
        (is (await-until #(= :complete
                             (get-in (sut/status registry "jit-queue:q")
                                     [:durable-state :regulator/status]))))
        (is (= 2 (count @calls)))
        (is (every? #(nil? (get-in % [:authority :session])) @calls))
        (finally (durable/stop! "jit-queue:q"))))))

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
