(ns futon3c.agents.zaif-inputs-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon2.aif.selection-gain :as selection-gain]
            [futon3c.agents.zaif-controller :as zaif]
            [futon3c.agents.zaif-inputs :as zinputs]))

(defn- workspace-root
  []
  (let [source-file (-> (io/resource "futon3c/agents/zaif_inputs_test.clj")
                        .toURI
                        io/file)]
    (loop [dir (.getParentFile source-file)]
      (cond
        (nil? dir)
        (throw (ex-info "Could not locate futon3c checkout" {}))

        (.isFile (io/file dir "deps.edn"))
        (.getParentFile dir)

        :else
        (recur (.getParentFile dir))))))

(def task-belief-absence
  {:absence :d8/task-belief-actand-source-absent})

(defn- harness-artifact
  [filename]
  (-> (io/file (workspace-root)
               "futon2/holes/labs"
               filename)
      slurp
      edn/read-string))

(deftest r7-declared-mark-outranks-lexical-guess
  ;; LIVE PIN: the coupling-density precision is captured verbatim from tracked
  ;; fixture 801976e7-R7.edn, harvested from live record/run id
  ;; 801976e7-01c6-4e39-aada-27f620f7c2f1.
  (let [table (harness-artifact "zaif-harness/runs/U9-r7-precision-table.edn")
        fixture (harness-artifact
                 "wm-contract/runs/U12-c-mis-falsifier/node-fixtures/801976e7-R7.edn")
        rows (into {} (map (juxt :channel-class identity)) (:rows table))
        declared (get-in rows [:declared-operator-mark :precision])
        lexical (get-in rows [:lexical-probe :precision])]
    (is (= "801976e7-01c6-4e39-aada-27f620f7c2f1" (:run/id fixture)))
    (is (= 7.285663818719639
           (get-in fixture [:value :coupling-density :precision])))
    (is (= 1.0 declared))
    (is (= 0.42 lexical))
    (is (> declared lexical)
        "the declared operator channel, not lexical mission guessing, has higher within-kind precision")))

(deftest r7-wm-fixture-preserves-present-and-absent-precision-channels
  ;; LIVE PIN: values are read verbatim from tracked fixture 801976e7-R7.edn,
  ;; harvested from live record/run id 801976e7-01c6-4e39-aada-27f620f7c2f1.
  (let [table (harness-artifact "zaif-harness/runs/U9-r7-precision-table.edn")
        fixture (harness-artifact
                 "wm-contract/runs/U12-c-mis-falsifier/node-fixtures/801976e7-R7.edn")
        wm-row (some #(when (= :war-machine-observation (:channel-class %)) %)
                     (:rows table))]
    (is (= :R7 (:node fixture)))
    (is (= :present (:status fixture)))
    (is (= "futon2.aif.precision/update-precision-state" (:via fixture)))
    (is (= (:run/id fixture) (:run/id wm-row)))
    (is (= 8 (:covered-count wm-row) (count (:value fixture))))
    (is (= 6 (count (:absent-not-zero wm-row))))
    (is (= 7.285663818719639
           (get-in wm-row [:precision :coupling-density])
           (get-in fixture [:value :coupling-density :precision])))))

(deftest r8-zaif-replay-types-the-missing-realised-observation
  ;; LIVE PIN: values are read verbatim from D9's tracked replay of live
  ;; evidence record e-0f2f9aec-6240-40e9-a25a-e45d9452076f.
  (let [report (harness-artifact "zaif-harness/runs/D9-tie-order-count.edn")
        pin (get-in report [:live :live-pin])
        replayed (zaif/decide (:inputs pin))
        realised-observation
        (if (contains? replayed :realised-observation)
          {:status :present :value (:realised-observation replayed)}
          {:status :absent :reason :zaif-v0-no-realised-observation})]
    (is (= "e-0f2f9aec-6240-40e9-a25a-e45d9452076f" (:id pin)))
    (is (= (:decision pin) replayed))
    (is (= {:status :absent :reason :zaif-v0-no-realised-observation}
           realised-observation))))

(deftest r8-wm-fixture-replays-at-exact-delta-zero
  ;; LIVE PIN: values are read verbatim from tracked fixture 801976e7-R8.edn,
  ;; harvested from live record/run id 801976e7-01c6-4e39-aada-27f620f7c2f1.
  (let [fixture (harness-artifact
                 "wm-contract/runs/U12-c-mis-falsifier/node-fixtures/801976e7-R8.edn")
        prediction-error (get-in fixture [:value :ticks-firing-ratio])
        replayed-error (- (:observed prediction-error)
                          (:predicted-mean prediction-error))
        replayed-weighted-error (* (:precision prediction-error)
                                   replayed-error)]
    (is (= "801976e7-01c6-4e39-aada-27f620f7c2f1" (:run/id fixture)))
    (is (= :R8 (:node fixture)))
    (is (= :prediction-error/v1 (:producer-contract prediction-error)))
    (is (= 0.0 (:observed prediction-error) (:predicted-mean prediction-error)))
    (is (= 21.0 (:precision prediction-error)))
    (is (= 0.0 replayed-error (:error prediction-error)))
    (is (= 0.0 replayed-weighted-error (:weighted-error prediction-error)))))

(deftest r14-verdict-stream-moves-temperature-and-types-empty
  ;; LIVE PIN: the gamma value and event id are captured verbatim from tracked
  ;; B1 fold record b1-gamma-mission.edn, whose live corpus includes evidence
  ;; record e-63c25e11-ac8b-4287-966e-bdc7f007bc78.
  ;; H6b (2026-09-17): the two flat τ_eff assertions (effective-temperature
  ;; raised/lowered by selection gain, and the spread-τ value) were deleted
  ;; with the flat law they tested — futon2.aif.policy/effective-temperature
  ;; no longer exists. The selection-gain facts themselves still compute and
  ;; stay pinned below.
  (let [record (harness-artifact "M-zaif-harness/b1-gamma-mission.edn")
        pinned-cell (get-in record [:cells "M-futon-forward-model"])
        empty-stream {:status :absent
                      :reason :no-verdict-events
                      :state (selection-gain/coerce-state nil)}
        corrections (repeat 5 -0.5)
        corrected-state (reduce selection-gain/update-selection-gain
                                (selection-gain/initial-selection-gain-state)
                                corrections)]
    (is (= 0.7071067811865476 (:policy-precision pinned-cell)))
    (is (= 10 (:samples pinned-cell)))
    (is (some #(= "e-63c25e11-ac8b-4287-966e-bdc7f007bc78" (:id %))
              (:events record)))
    (is (= :absent (:status empty-stream)))
    (is (= :no-verdict-events (:reason empty-stream)))
    (is (= 1.0 (selection-gain/selection-gain-for (:state empty-stream))))
    (is (= 0.7071067811865476
           (selection-gain/selection-gain-for corrected-state)))))

(deftest r14-wm-fixture-pins-live-temperature
  ;; LIVE PIN: values are read verbatim from tracked fixture 801976e7-R14.edn,
  ;; harvested from live record/run id 801976e7-01c6-4e39-aada-27f620f7c2f1.
  (let [fixture (harness-artifact
                 "wm-contract/runs/U12-c-mis-falsifier/node-fixtures/801976e7-R14.edn")]
    (is (= :R14 (:node fixture)))
    (is (= "801976e7-01c6-4e39-aada-27f620f7c2f1" (:run/id fixture)))
    (is (= :present (:status fixture)))
    (is (= "futon2.report.war-machine/invoke-strategic-selection" (:via fixture)))
    (is (= [:decision :tau] (:field fixture)))
    (is (= 1.0 (:value fixture)))))

(deftest default-gamma-path-is-classpath-anchored
  (let [source (io/resource "futon3c/agents/zaif_inputs.clj")
        expected (.getCanonicalPath
                  (io/file
                   "/home/joe/code/futon2/holes/labs/M-zaif-harness/b1-gamma-mission.edn"))
        original-cwd (System/getProperty "user.dir")]
    (try
      (System/setProperty "user.dir" "/tmp/a-foreign-review-checkout")
      (let [resolved (.getCanonicalPath
                      (io/file
                       (zinputs/default-gamma-path-from-source source)))]
        (is (= expected resolved)
            "checkout basename and process cwd do not participate in resolution")
        (is (.isFile (io/file resolved))
            "live pin: the resolved default is the real B1 artifact"))
      (finally
        (System/setProperty "user.dir" original-cwd)))))

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

(deftest attributed-mission-controls-zaif-act-risk
  (let [actand {:act-value 0.8
                :provenance {:source :z1/actand-world-model
                             :query :actand-for-task}}
        attributed (zinputs/hydrate-inputs
                    {:mission "M-futon-forward-model"
                     :mission-source :dispatch/mission-id
                     :actand-query-result actand})
        unclocked (zinputs/hydrate-inputs {:actand-query-result actand})
        attributed-decision (zaif/decide attributed)
        unclocked-decision (zaif/decide unclocked)
        evidence (zaif/decision-evidence-entry
                  {:agent-id "zai-test" :sid "sid-r5" :turn-id "turn-r5"
                   :round 1 :decision attributed-decision :inputs attributed})]
    (testing "the attributed mission selects its real gamma cell before scoring act"
      (is (= :table-cell (:gamma-source attributed)))
      (is (= 0.7071067811865476 (:gamma-used attributed-decision)))
      (is (= (* 0.8 0.7071067811865476)
             (get-in attributed-decision [:g-terms :act])))
      (is (< (get-in attributed-decision [:g-terms :act])
             (get-in unclocked-decision [:g-terms :act]))))
    (testing "the decision record retains the attribution used by the risk term"
      (is (= "M-futon-forward-model" (get-in evidence [:evidence/body :mission])))
      (is (= :dispatch/mission-id
             (get-in evidence [:evidence/body :inputs-snapshot :mission-source])))
      (is (= :table-cell
             (get-in evidence [:evidence/body :inputs-snapshot :gamma-source]))))))

(deftest wm-r5-fixture-pins-mission-risk-identity
  ;; LIVE PIN: values below are read verbatim from tracked fixture
  ;; 0a18c4f7-R5.edn, harvested from live record/run id
  ;; 0a18c4f7-758e-400a-8223-9c52edf07450.
  (let [fixture-file (io/file
                      (workspace-root)
                      "futon2/holes/labs/wm-contract/runs"
                      "U12-c-mis-falsifier/node-fixtures/0a18c4f7-R5.edn")
        fixture (edn/read-string (slurp fixture-file))
        ranked (:value fixture)
        top (first ranked)]
    (is (= :R5 (:node fixture)))
    (is (= :present (:status fixture)))
    (is (= "futon2.aif.efe/rank-actions" (:via fixture)))
    (is (= "M-zaif-harness-v1" (get-in top [:action :target])))
    (is (= 114.21190142192009 (:G-risk top)))
    (is (= -108.87689318874708 (:G-ambiguity top)))
    (is (= 5.3350082331730135 (:G-core top)))
    (is (= (:G-core top) (+ (:G-risk top) (:G-ambiguity top)))
        "the recorded mission ranking reaches R5 as risk plus ambiguity")))

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
