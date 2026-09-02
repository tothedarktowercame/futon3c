(ns futon3c.agents.zaif-inputs-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.agents.zaif-controller :as zaif]
            [futon3c.agents.zaif-inputs :as zinputs]))

(def task-belief-absence
  {:absence :d8/task-belief-actand-source-absent})

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
      (is (= :table-cell (:gamma-source inputs)))
      (is (= 1.0 (get-in inputs [:c-belief :operator-c-uncertainty])))
      (is (= task-belief-absence (:task-belief inputs)))
      (is (contains? (get-in inputs [:observations :posting-stats]) :total-docs)))))

(deftest gamma-source-distinguishes-table-cell-and-defaults
  (testing "a table cell that equals the uniform prior is still a table read"
    (is (= :table-cell
           (:gamma-source
            (zinputs/hydrate-inputs {:mission "M-points-de-fuite"})))))
  (testing "a named mission absent from the table uses the table-miss default"
    (is (= :default-table-miss
           (:gamma-source
            (zinputs/hydrate-inputs {:mission "M-not-in-gamma-table"})))))
  (testing "an unclocked input uses the no-mission default"
    (is (= :default-no-mission
           (:gamma-source (zinputs/hydrate-inputs {}))))))

(deftest absent-task-belief-is-zero-and-auditable-end-to-end
  (let [inputs (zinputs/hydrate-inputs {:mission "M-no-actand"
                                        :context "continue the task"})
        decision (zaif/decide inputs)
        entry (zaif/decision-evidence-entry
               {:agent-id "zai-test" :sid "sid-d8" :turn-id "turn-d8"
                :round 1 :decision decision :inputs inputs})]
    (is (= 0.0 (get-in decision [:g-terms :act])))
    (is (= task-belief-absence
           (get-in entry [:evidence/body :inputs-snapshot :task-belief])))))

(deftest provenanced-actand-value-reaches-controller
  (let [source {:act-value 0.4
                :provenance {:source :z1/actand-world-model
                             :query :actand-for-task}}
        inputs (zinputs/hydrate-inputs {:mission "M-new-source"
                                        :context "act now"
                                        :actand-query-result source})
        decision (zaif/decide inputs)]
    (is (= source (:task-belief inputs)))
    (is (= 0.4 (get-in decision [:g-terms :act])))))

(deftest unprovenanced-actand-value-is-refused
  (let [inputs (zinputs/hydrate-inputs {:mission "M-refuse"
                                        :context "act now"
                                        :actand-query-result {:act-value 0.9}})
        decision (zaif/decide inputs)
        entry (zaif/decision-evidence-entry
               {:agent-id "zai-test" :sid "sid-refuse" :turn-id "turn-refuse"
                :round 1 :decision decision :inputs inputs})]
    (is (= {:absence :d8/task-belief-actand-source-absent
            :refused :d8/unprovenanced-task-belief}
           (:task-belief inputs)))
    (is (= 0.0 (get-in decision [:g-terms :act])))
    (is (= (:task-belief inputs)
           (get-in entry [:evidence/body :inputs-snapshot :task-belief])))))

(deftest gamma-source-error-fallback-is-typed
  ;; Reviewer addition (claude-2): the one branch d6f1d898's tests leave
  ;; unpinned -- the make-hydrator catch path must speak the same vocabulary.
  (let [hydrator (zinputs/make-hydrator)]
    (with-redefs [zinputs/hydrate-inputs (fn [_] (throw (ex-info "boom" {})))]
      (is (= :default-no-mission (:gamma-source (hydrator {:context "x"})))
          "missionless error fallback")
      (is (= :default-table-miss
             (:gamma-source (hydrator {:mission "M-x" :context "x"})))
          "mission-bearing error fallback"))))

(deftest live-recorded-vocabulary-transition-pin
  ;; LIVE-PIN (board rule, 2026-09-02): values captured verbatim from live
  ;; record e-0f2f9aec-6240-40e9-a25a-e45d9452076f (zai-3, 2026-08-09,
  ;; :zaif-arm-choice) -- the recorded pre-D8b vocabulary, so this test fails
  ;; if either the record's meaning or the new absence vocabulary drifts.
  (let [live-snapshot {:task-belief {}
                       :c-belief {:operator-c-uncertainty 0.3}
                       :gamma "{nil {:policy-precision 1.0}}"
                       :observations {:posting-stats {:total-docs 106
                                                      :dfs [1 1 1 1 1 1 1 1 1 1]
                                                      :estimated-tokens 212}}}]
    (testing "the live corpus's recorded task-belief vocabulary is bare {}"
      (is (= {} (:task-belief live-snapshot))))
    (testing "the same source-less condition now hydrates to the typed absence"
      (let [inputs (zinputs/hydrate-inputs {:context "continue the task"})]
        (is (= task-belief-absence (:task-belief inputs)))
        (is (not= (:task-belief live-snapshot) (:task-belief inputs))
            "an audit can now distinguish post-D8b records from the 114-session corpus")))))

(deftest hydrate-inputs-does-not-infer-mission-from-context
  (testing "prompt text is not a typed mission source"
    (let [inputs (zinputs/hydrate-inputs {:context "working on M-points-de-fuite today"})]
      (is (nil? (:mission inputs)))
      (is (= :d10/unclocked (:mission-source inputs))))))

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

(deftest hydrator-error-fallback-carries-typed-task-belief-absence
  (with-redefs [zinputs/hydrate-inputs (fn [_]
                                        (throw (ex-info "planted failure" {})))]
    (is (= task-belief-absence
           (:task-belief ((zinputs/make-hydrator) {:context "test"}))))))

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
