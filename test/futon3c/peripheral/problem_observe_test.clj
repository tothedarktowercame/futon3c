(ns futon3c.peripheral.problem-observe-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.apm.preregistration :as prereg]
            [futon3c.peripheral.problem :as problem]
            [futon3c.peripheral.runner :as runner]
            [futon3c.peripheral.tools :as tools]
            [futon3c.substrate.client :as substrate])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- test-problem [dispatch-fn]
  (problem/make-problem
   (tools/make-mock-backend) dispatch-fn "/tmp/problem-observe-state"
   (fn [_] {:checkout "/tmp/checkout" :base-revision (apply str (repeat 40 "a"))})
   (fn [_] {:harness-revision (apply str (repeat 40 "b"))
            :harness-tree-dirty? false})
   (constantly []) (constantly 0)))

(deftest guide-events-are-machine-recorded-and-caller-cannot-retarget
  (let [calls (atom [])
        p (test-problem (fn [opts packet]
                          (swap! calls conj [opts packet])
                          {:job-id "guide-job"}))
        initial (:state (runner/start p {:session-id "guide" :problem-id "p"
                                         :cycle/mode :store-mode
                                         :harness-repo "/harness"}))
        state (assoc initial :current-phase :guided-solve :current-cycle-id "c"
                     :cycle/outputs
                     {:registration {:reg/solver-seat "codex-4"}})
        guided (runner/step p state
                            {:tool :guide-solver
                             :args [{:to "attacker"} "please inspect"]})
        stamped (#'problem/stamp-environment-outputs
                 (:state guided)
                 {:solver-attempt {} :memory-offers []
                  :ground-control-events [:forged]})
        events (:ground-control-events stamped)]
    (is (:ok guided))
    (is (= "codex-4" (get-in @calls [0 0 :to])))
    (is (= "guide-job" (:job-id (first events))))
    (is (= "codex-4" (:ground-control/recipient (first events))))
    (is (= 1 (get (#'problem/measurement-values
                   {:ground-control-events events})
                  "attempts or closer hops")))))

(deftest guidance-measurement-gate-fires-and-stays-silent
  (let [registration {:reg/solver-seat "codex-4" :required-capabilities []}
        job {:agent-id "codex-4" :caller "anyone"
             :created-at "2026-08-15T00:30:00Z"}
        base-trace {:cycle/window {:opened-at "2026-08-15T00:00:00Z"
                                   :closed-at "2026-08-15T01:00:00Z"}
                    :memory-offers []
                    :measurement {:meas/values {"attempts or closer hops" 1}}
                    :required-measurement-fields []
                    :available-artifact-ids [] :need-probe-retrieved-ids []
                    :promoted-artifact-ids []
                    :importable-promoted-artifact-ids []
                    :need-tagged-promoted-artifact-ids []
                    :capability-probes []}
        evidence {:status :ok :jobs [job]}
        good (prereg/trace-content-failures registration base-trace evidence
                                            "codex-4")
        bad (prereg/trace-content-failures
             registration
             (assoc-in base-trace [:measurement :meas/values
                                   "attempts or closer hops"] 0)
             evidence "codex-4")]
    (is (not (some #{:guidance-measurement-mismatch} good)))
    (is (some #{:guidance-measurement-mismatch} bad))))

(deftest observe-tools-return-content-without-becoming-outputs
  (let [path (Files/createTempFile "problem-registration-" ".edn"
                                   (make-array FileAttribute 0))
        backend (problem/make-problem-cycle-backend
                 (tools/make-mock-backend) (constantly {}) (atom {})
                 (constantly []) (constantly 0))]
    (try
      (spit (.toFile path) "{:problem {:problem-id \"p\"}}")
      (let [read-result (tools/execute-tool backend :read-registration
                                            [(str path)])
            validation (tools/execute-tool backend :validate-registration
                                           [(:result read-result)])
            attempt (tools/execute-tool
                     backend :read-attempt-result
                     ["attempt/2"
                      {:cycle/outputs
                       {:solver-attempt {:attempt/id "attempt/1"}
                        :student-attempts [{:attempt/id "attempt/2"
                                            :result :proved}]}}])]
        (is (= "p" (get-in read-result [:result :problem :problem-id])))
        (is (false? (get-in validation [:result :valid?])))
        (is (= :proved (get-in attempt [:result :result]))))
      (with-redefs [substrate/hyperedges-by-type
                    (fn [type options]
                      [{:hx/id "h" :type type :limit (:limit options)}])]
        (let [read-result (tools/execute-tool backend :read-substrate
                                               [:memory/assert {:limit 3}])]
          (is (:ok read-result))
          (is (= "h" (get-in read-result [:result :rows 0 :hx/id])))))
      (doseq [tool [:read-registration :validate-registration
                    :read-substrate :read-attempt-result]]
        (is (= :observe (get problem/tool-ops tool)))
        (is (not-any? #(contains? % tool) (vals problem/required-outputs))))
      (finally
        (Files/deleteIfExists path)))))

(deftest read-substrate-refuses-a-full-page
  (let [backend (problem/make-problem-cycle-backend
                 (tools/make-mock-backend) (constantly {}) (atom {})
                 (constantly []) (constantly 0))]
    (with-redefs [substrate/hyperedges-by-type
                  (fn [_ _] (repeat 2 {:hx/id "h"}))]
      (let [result (tools/execute-tool backend :read-substrate
                                       [:memory/assert {:limit 2}])]
        (is (false? (:ok result)))
        (is (re-find #"truncated" (:error result)))))))
