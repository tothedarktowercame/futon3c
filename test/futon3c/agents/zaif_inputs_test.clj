(ns futon3c.agents.zaif-inputs-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.agents.zaif-inputs :as zinputs]))

(deftest gamma-cell-exact-value
  (testing "the exact γ cell M-futon-forward-model → 0.7071067811865476 (2^-1/2)"
    (let [data (zinputs/load-gamma-table)
          cells (zinputs/gamma-cells data)]
      (is (= 0.7071067811865476
             (get cells "M-futon-forward-model"))))))

(deftest gamma-for-missing-uses-uniform-prior
  (testing "missing mission → 1.0 (uniform prior)"
    (let [cells (zinputs/gamma-cells (zinputs/load-gamma-table))]
      (is (= 1.0 (zinputs/gamma-for cells "M-nonexistent-mission")))
      (is (= 1.0 (zinputs/gamma-for cells nil))))))

(deftest gamma-for-known-mission
  (testing "known missions return their cell value"
    (let [cells (zinputs/gamma-cells (zinputs/load-gamma-table))]
      (is (= 1.0 (zinputs/gamma-for cells "M-points-de-fuite")))
      (is (= 0.7071067811865476 (zinputs/gamma-for cells "M-futon-forward-model"))))))

(deftest correction-rate-derivation
  (testing "M-futon-forward-model (10 corrections, 0 approvals) → rate 1.0"
    (let [rates (zinputs/correction-rate-table (zinputs/load-gamma-table))]
      (is (= 1.0 (get rates "M-futon-forward-model")))))
  (testing "M-points-de-fuite (1 approval, 0 corrections) → rate 0.0"
    (let [rates (zinputs/correction-rate-table (zinputs/load-gamma-table))]
      (is (= 0.0 (get rates "M-points-de-fuite"))))))

(deftest c-uncertainty-for-known-and-unknown
  (testing "known high-correction mission → high c-uncertainty"
    (let [rates (zinputs/correction-rate-table (zinputs/load-gamma-table))]
      (is (= 1.0 (zinputs/c-uncertainty-for "M-futon-forward-model" rates)))))
  (testing "unknown mission → mild prior 0.3"
    (let [rates (zinputs/correction-rate-table (zinputs/load-gamma-table))]
      (is (= 0.3 (zinputs/c-uncertainty-for "M-brand-new-mission" rates)))
      (is (= 0.3 (zinputs/c-uncertainty-for nil rates))))))

(deftest hydrate-inputs-full-shape
  (testing "hydrated inputs have all channels populated for a known mission"
    (let [inputs (zinputs/hydrate-inputs {:mission "M-futon-forward-model"
                                           :context "fix the forward model bug"})]
      (is (= "M-futon-forward-model" (:mission inputs)))
      (is (= 0.7071067811865476
             (get-in inputs [:gamma "M-futon-forward-model" :policy-precision])))
      (is (= 1.0 (get-in inputs [:c-belief :operator-c-uncertainty])))
      (is (contains? (get-in inputs [:observations :posting-stats]) :total-docs)))))

(deftest hydrate-inputs-extracts-mission-from-context
  (testing "mission extracted from context text when :mission not given"
    (let [inputs (zinputs/hydrate-inputs {:context "working on M-points-de-fuite today"})]
      (is (= "M-points-de-fuite" (:mission inputs))))))

(deftest hydrator-fn-failure-path
  (testing "make-hydrator returns empty maps when gamma file is missing"
    ;; Force a bad path via env; the load should fail silently
    (zinputs/reset-gamma-cache!)
    (with-redefs [zinputs/gamma-edn-path (fn [] "/nonexistent/path/to/gamma.edn")]
      (let [hydrator (zinputs/make-hydrator)
            inputs (hydrator {:mission "M-anything" :context "test"})]
        ;; Degraded: no gamma cell, no c-uncertainty from table, but doesn't throw
        (is (map? inputs))
        (is (contains? inputs :gamma))
        (is (contains? inputs :c-belief))))))

(deftest hydrator-fn-never-throws
  (testing "the hydrator fn never throws, even with garbage input"
    (let [hydrator (zinputs/make-hydrator)]
      (is (map? (hydrator nil)))
      (is (map? (hydrator {})))
      (is (map? (hydrator {:mission 42 :context nil}))))))

(deftest posting-stats-derivation
  (testing "empty context → empty posting stats"
    (is (= {} (zinputs/estimate-posting-stats "")))
    (is (= {} (zinputs/estimate-posting-stats nil))))
  (testing "non-empty context → stats with total-docs and dfs"
    (let [stats (zinputs/estimate-posting-stats "fix the model bug in the model")]
      (is (pos? (:total-docs stats)))
      (is (vector? (:dfs stats)))
      (is (pos? (:estimated-tokens stats))))))
