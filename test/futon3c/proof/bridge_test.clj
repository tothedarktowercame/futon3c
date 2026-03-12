(ns futon3c.proof.bridge-test
  "Tests for the proof bridge — verifies the agent-facing API
   correctly delegates to ProofBackend tools."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [futon3c.peripheral.proof-backend :as proof-backend]
            [futon3c.proof.bridge :as pb]))

;; =============================================================================
;; Fixture: reset bridge backends between tests to avoid cross-contamination
;; =============================================================================

(defn reset-backends [f]
  (pb/reload! "FM-001")
  (pb/reload! "BRIDGE-TEST-TMP")
  (f)
  (pb/reload! "FM-001")
  (pb/reload! "BRIDGE-TEST-TMP"))

(use-fixtures :each reset-backends)

;; =============================================================================
;; FM-001 exists on disk — test against real state
;; =============================================================================

(deftest summary-loads-fm-001
  (testing "summary of FM-001 returns problem info"
    (let [result (pb/summary "FM-001")]
      (is (:ok result) (str "Expected ok, got: " result))
      (let [r (:result result)]
        (is (= "FM-001" (:problem-id r)))
        (is (keyword? (:mode r)))
        (is (number? (:version r)))
        (is (number? (:cycles-count r)))
        (is (number? (:ledger-count r)))
        (is (boolean? (:falsify-done? r)))
        (is (string? (:statement r)))))))

(deftest mode-reads-current-mode
  (testing "mode returns the current proof mode"
    (let [result (pb/mode "FM-001")]
      (is (:ok result) (str "Expected ok, got: " result))
      (is (keyword? (get-in result [:result :mode]))))))

(deftest canonical-reads-statement
  (testing "canonical returns problem statement"
    (let [result (pb/canonical "FM-001")]
      (is (:ok result) (str "Expected ok, got: " result))
      (is (string? (get-in result [:result :statement]))))))

(deftest ledger-query-works
  (testing "ledger query returns items (may be empty)"
    (let [result (pb/ledger "FM-001")]
      (is (:ok result) (str "Expected ok, got: " result)))))

(deftest cycles-list-works
  (testing "cycles list returns cycles (may be empty)"
    (let [result (pb/cycles "FM-001")]
      (is (:ok result) (str "Expected ok, got: " result)))))

(deftest nonexistent-problem-returns-error
  (testing "loading a nonexistent problem returns an error"
    (let [result (pb/summary "NONEXISTENT-999")]
      (is (false? (:ok result))))))

(deftest dag-check-on-fm-001
  (testing "DAG check runs without error"
    (let [result (pb/dag-check "FM-001")]
      (is (:ok result) (str "Expected ok, got: " result)))))

(deftest load-problem-returns-full-state
  (testing "load-problem returns the full proof state"
    (let [result (pb/load-problem "FM-001")]
      (is (:ok result) (str "Expected ok, got: " result))
      (let [state (:result result)]
        (is (= "FM-001" (:proof/problem-id state)))
        (is (map? (:proof/canonical state)))
        (is (map? (:proof/ledger state)))
        (is (vector? (:proof/cycles state)))))))

;; =============================================================================
;; Mutation persistence: shared backend means mutations survive across calls
;; =============================================================================

(deftest mutation-persists-across-calls
  (testing "canonical-update! followed by canonical reads the updated value"
    ;; Use a temp problem to avoid mutating FM-001
    (let [tmp-id "BRIDGE-TEST-TMP"
          tmp-file (io/file "data/proof-state" (str tmp-id ".edn"))
          initial-state {:proof/problem-id tmp-id
                         :proof/version 0
                         :proof/canonical {:statement "Initial"
                                           :closure-criterion "Test"
                                           :statement-hash "sha256:test"
                                           :version-history [{:version 0
                                                              :statement "Initial"
                                                              :hash "sha256:test"
                                                              :changed-at "2026-01-01T00:00:00Z"
                                                              :reason "test"}]}
                         :proof/ledger {}
                         :proof/cycles []
                         :proof/failed-routes []
                         :proof/updated-at "2026-01-01T00:00:00Z"
                         :proof/current-mode :SPEC
                         :proof/falsify-completed? false
                         :proof/tryharder-log []}]
      (try
        ;; Write initial state
        (.mkdirs (.getParentFile tmp-file))
        (spit tmp-file (pr-str initial-state))

        ;; Update canonical via bridge
        (let [update-result (pb/canonical-update! tmp-id "Updated statement" "test update")]
          (is (:ok update-result) (str "Update failed: " update-result)))

        ;; Read it back — should see the update (same backend, auto-saved)
        (let [read-result (pb/canonical tmp-id)]
          (is (:ok read-result) (str "Read failed: " read-result))
          (is (= "Updated statement" (get-in read-result [:result :statement]))))

        ;; Verify it's persisted to disk
        (let [disk-state (edn/read-string (slurp tmp-file))]
          (is (= "Updated statement"
                 (get-in disk-state [:proof/canonical :statement]))))

        (finally
          (pb/reload! tmp-id)
          (.delete tmp-file))))))

(deftest bridge-respects-proof-state-root-property
  (testing "bridge uses the configured proof-state-root override when present"
    (let [tmp-id "BRIDGE-TEST-ROOT"
          temp-root (doto (java.io.File/createTempFile "bridge-proof-root" "")
                      (.delete)
                      (.mkdirs))
          state-root (io/file temp-root "active")
          default-file (io/file "data/proof-state" (str tmp-id ".edn"))
          override-file (io/file state-root (str tmp-id ".edn"))
          property-name "futon3c.proof-state-root"]
      (try
        (proof-backend/init-problem!
          {:cwd (System/getProperty "user.dir")
           :proof-state-root (.getPath state-root)}
          tmp-id
          "Bridge override statement"
          "Test criterion")
        (System/setProperty property-name (.getPath state-root))
        (pb/reload! tmp-id)
        (let [read-result (pb/canonical tmp-id)]
          (is (:ok read-result) (str "Read failed: " read-result))
          (is (= "Bridge override statement" (get-in read-result [:result :statement]))))
        (is (.exists override-file))
        (is (not (.exists default-file)))
        (finally
          (System/clearProperty property-name)
          (pb/reload! tmp-id)
          (.delete override-file)
          (.delete state-root)
          (.delete temp-root))))))

(deftest bridge-fails-closed-on-proof-state-root-mismatch
  (testing "bridge rejects a proof-state-root change until the cached backend is reloaded"
    (let [tmp-id "BRIDGE-TEST-MISMATCH"
          temp-root-a (doto (java.io.File/createTempFile "bridge-proof-root-a" "")
                        (.delete)
                        (.mkdirs))
          temp-root-b (doto (java.io.File/createTempFile "bridge-proof-root-b" "")
                        (.delete)
                        (.mkdirs))
          state-root-a (io/file temp-root-a "active")
          state-root-b (io/file temp-root-b "active")
          property-name "futon3c.proof-state-root"]
      (try
        (proof-backend/init-problem!
          {:cwd (System/getProperty "user.dir")
           :proof-state-root (.getPath state-root-a)}
          tmp-id
          "Bridge mismatch statement"
          "Test criterion")
        (System/setProperty property-name (.getPath state-root-a))
        (pb/reload! tmp-id)
        (let [read-result (pb/canonical tmp-id)]
          (is (:ok read-result) (str "Initial read failed: " read-result)))
        (System/setProperty property-name (.getPath state-root-b))
        (is (thrown-with-msg?
              clojure.lang.ExceptionInfo
              #"Proof-state root changed"
              (pb/canonical tmp-id)))
        (finally
          (System/clearProperty property-name)
          (pb/reload! tmp-id)
          (.delete (io/file state-root-a (str tmp-id ".edn")))
          (.delete state-root-a)
          (.delete temp-root-a)
          (.delete state-root-b)
          (.delete temp-root-b))))))

(deftest reset-forces-reload
  (testing "reset! discards cached backend, next call reloads from disk"
    (let [result-before (pb/mode "FM-001")]
      (is (:ok result-before))
      (pb/reload! "FM-001")
      (let [result-after (pb/mode "FM-001")]
        (is (:ok result-after))
        ;; Mode should be the same (reloaded from disk)
        (is (= (get-in result-before [:result :mode])
               (get-in result-after [:result :mode])))))))

;; =============================================================================
;; Lakatosian layer: conjecture lifecycle
;; =============================================================================

(defn- make-tmp-problem!
  "Create a temporary problem for mutation tests. Returns the problem id."
  [problem-id]
  (let [tmp-file (io/file "data/proof-state" (str problem-id ".edn"))
        state {:proof/problem-id problem-id
               :proof/version 0
               :proof/canonical {:statement "Test problem"
                                 :closure-criterion "Test"
                                 :statement-hash "sha256:test"
                                 :version-history [{:version 0
                                                    :statement "Test problem"
                                                    :hash "sha256:test"
                                                    :changed-at "2026-01-01T00:00:00Z"
                                                    :reason "test"}]}
               :proof/ledger {"L-1" {:item/id "L-1"
                                     :item/label "Test obligation"
                                     :item/status :open
                                     :item/depends-on #{}
                                     :item/unlocks #{}
                                     :item/artifact-paths []}}
               :proof/cycles []
               :proof/failed-routes []
               :proof/updated-at "2026-01-01T00:00:00Z"
               :proof/current-mode :SPEC
               :proof/falsify-completed? false
               :proof/tryharder-log []}]
    (.mkdirs (.getParentFile tmp-file))
    (spit tmp-file (pr-str state))
    problem-id))

(defn- cleanup-tmp-problem! [problem-id]
  (pb/reload! problem-id)
  (.delete (io/file "data/proof-state" (str problem-id ".edn"))))

(deftest conjecture-add-and-query
  (let [pid (make-tmp-problem! "CONJ-TEST-1")]
    (try
      (testing "conjecture-add! creates a conjecture on a ledger item"
        (let [result (pb/conjecture-add! pid "L-1"
                       {:id "H-test"
                        :hypothesis "X > 0 for all X"})]
          (is (:ok result))
          (is (= 1 (count (:item/conjectures (:result result)))))
          (is (= "H-test" (-> result :result :item/conjectures first :id)))))

      (testing "conjectures query returns them"
        (let [result (pb/conjectures pid "L-1")]
          (is (:ok result))
          (is (= 1 (count (:result result))))
          (is (= :untested (-> result :result first :status)))))

      (testing "conjectures query across all items"
        (let [result (pb/conjectures pid)]
          (is (:ok result))
          (is (contains? (:result result) "L-1"))))

      (finally (cleanup-tmp-problem! pid)))))

(deftest conjecture-add-rejects-missing-item
  (let [pid (make-tmp-problem! "CONJ-TEST-2")]
    (try
      (let [result (pb/conjecture-add! pid "L-nonexistent"
                     {:id "H-x" :hypothesis "test"})]
        (is (not (:ok result))))
      (finally (cleanup-tmp-problem! pid)))))

(deftest conjecture-test-records-outcomes
  (let [pid (make-tmp-problem! "CONJ-TEST-3")]
    (try
      (pb/conjecture-add! pid "L-1"
        {:id "H-t" :hypothesis "Test hypothesis"})

      (testing "recording a refuted test — single refuted means all-refuted"
        (let [result (pb/conjecture-test! pid "L-1" "H-t"
                       {:method "computation"
                        :params {:n 3}
                        :outcome :refuted
                        :evidence-ref "test-evidence"
                        :agent "test-agent"})]
          (is (:ok result))
          (let [conj (-> result :result :item/conjectures first)]
            (is (= :refuted (:status conj)))
            (is (= 1 (count (:tests conj))))
            (is (= :refuted (-> conj :tests first :outcome))))))

      (testing "adding an inconclusive test changes to partially-tested"
        (let [result (pb/conjecture-test! pid "L-1" "H-t"
                       {:method "computation"
                        :params {:n 5}
                        :outcome :inconclusive
                        :agent "test-agent"})]
          (is (:ok result))
          (let [conj (-> result :result :item/conjectures first)]
            ;; Mix of refuted + inconclusive → partially-tested
            (is (= :partially-tested (:status conj)))
            (is (= 2 (count (:tests conj)))))))

      (testing "adding a confirmed test overrides to confirmed"
        (let [result (pb/conjecture-test! pid "L-1" "H-t"
                       {:method "analytical"
                        :outcome :confirmed
                        :agent "test-agent"})]
          (is (:ok result))
          (is (= :confirmed (-> result :result :item/conjectures first :status)))))

      (finally (cleanup-tmp-problem! pid)))))

(deftest conjecture-test-rejects-missing-conjecture
  (let [pid (make-tmp-problem! "CONJ-TEST-4")]
    (try
      (pb/conjecture-add! pid "L-1" {:id "H-exists" :hypothesis "test"})
      (let [result (pb/conjecture-test! pid "L-1" "H-nonexistent"
                     {:method "test" :outcome :refuted})]
        (is (not (:ok result))))
      (finally (cleanup-tmp-problem! pid)))))

(deftest conjecture-refine-creates-linked-conjecture
  (let [pid (make-tmp-problem! "CONJ-TEST-5")]
    (try
      (pb/conjecture-add! pid "L-1"
        {:id "H-old" :hypothesis "Original hypothesis"})

      (let [result (pb/conjecture-refine! pid "L-1" "H-old"
                     {:id "H-new"
                      :hypothesis "Refined hypothesis"
                      :reason "Original too broad"})]
        (is (:ok result))
        (let [conjs (:item/conjectures (:result result))]
          (is (= 2 (count conjs)))
          ;; Old conjecture has refinement link
          (is (= "H-new" (-> conjs first :refinements first :to)))
          ;; New conjecture replaces old
          (is (= "H-new" (-> conjs second :id)))
          (is (= "H-old" (-> conjs second :replaces)))
          (is (= :untested (-> conjs second :status)))))

      (finally (cleanup-tmp-problem! pid)))))

;; =============================================================================
;; Pólya layer: heuristic lifecycle
;; =============================================================================

(deftest heuristic-add-and-query
  (let [pid (make-tmp-problem! "HEUR-TEST-1")]
    (try
      (testing "heuristic-add! records a heuristic move"
        (let [result (pb/heuristic-add! pid "L-1"
                       {:type :analogy
                        :content "Paley graphs for prime-square q"
                        :by "claude-2"})]
          (is (:ok result))
          (is (= 1 (count (:item/heuristics (:result result)))))
          (is (= :active (-> result :result :item/heuristics first :status)))))

      (testing "adding a second heuristic accumulates"
        (pb/heuristic-add! pid "L-1"
          {:type :simplification
           :content "Try n=3 before n=25"
           :by "claude-2"})
        (let [result (pb/heuristics pid "L-1")]
          (is (:ok result))
          (is (= 2 (count (get (:result result) "L-1"))))))

      (testing "query all heuristics across items"
        (let [result (pb/heuristics pid)]
          (is (:ok result))
          (is (contains? (:result result) "L-1"))))

      (testing "filter by status"
        (let [result (pb/heuristics pid "L-1" :active)]
          (is (:ok result))
          (is (= 2 (count (get (:result result) "L-1"))))))

      (finally (cleanup-tmp-problem! pid)))))

(deftest heuristic-add-rejects-invalid-type
  (let [pid (make-tmp-problem! "HEUR-TEST-2")]
    (try
      (let [result (pb/heuristic-add! pid "L-1"
                     {:type :invalid-type
                      :content "bad"
                      :by "test"})]
        (is (not (:ok result))))
      (finally (cleanup-tmp-problem! pid)))))

(deftest heuristic-retire-marks-status
  (let [pid (make-tmp-problem! "HEUR-TEST-3")]
    (try
      (pb/heuristic-add! pid "L-1"
        {:type :analogy
         :content "Paley works"
         :by "test"})

      (testing "retire changes status"
        (let [result (pb/heuristic-retire! pid "L-1"
                       :analogy "Paley works" :retired)]
          (is (:ok result))
          (is (= :retired (-> result :result :item/heuristics first :status)))))

      (testing "retired heuristic filtered out by active query"
        (let [result (pb/heuristics pid "L-1" :active)]
          (is (:ok result))
          (is (empty? (get (:result result) "L-1")))))

      (finally (cleanup-tmp-problem! pid)))))

(deftest heuristic-retire-rejects-missing
  (let [pid (make-tmp-problem! "HEUR-TEST-4")]
    (try
      (let [result (pb/heuristic-retire! pid "L-1"
                     :analogy "nonexistent" :retired)]
        (is (not (:ok result))))
      (finally (cleanup-tmp-problem! pid)))))

;; =============================================================================
;; Mentor guidance synthesis
;; =============================================================================

(deftest mentor-guidance-synthesizes-recommendations
  (let [pid (make-tmp-problem! "GUIDE-TEST-1")]
    (try
      ;; Add an untested conjecture
      (pb/conjecture-add! pid "L-1"
        {:id "H-g" :hypothesis "Untested claim"})

      ;; Add an active dead-end heuristic
      (pb/heuristic-add! pid "L-1"
        {:type :dead-end
         :content "Random search intractable at n=50"
         :by "test"})

      ;; Add an active analogy
      (pb/heuristic-add! pid "L-1"
        {:type :analogy
         :content "Like Paley construction"
         :by "test"})

      (let [result (pb/mentor-guidance pid "L-1")]
        (is (:ok result))
        (let [g (:result result)]
          (is (= "L-1" (:item-id g)))
          (is (= 1 (count (:conjectures g))))
          (is (= 2 (count (:active-heuristics g))))

          ;; Should recommend testing the untested conjecture
          (is (some #(= :test-conjecture (:action %)) (:recommendations g)))

          ;; Should flag dead-end for strategy shift
          (is (some #(= :consider-strategy-shift (:action %)) (:recommendations g)))

          ;; Should recommend verifying the analogy
          (is (some #(= :verify-analogy (:action %)) (:recommendations g)))))

      (finally (cleanup-tmp-problem! pid)))))

(deftest mentor-guidance-rejects-missing-item
  (let [pid (make-tmp-problem! "GUIDE-TEST-2")]
    (try
      (let [result (pb/mentor-guidance pid "L-nonexistent")]
        (is (not (:ok result))))
      (finally (cleanup-tmp-problem! pid)))))

;; =============================================================================
;; Dispatch envelope
;; =============================================================================

(deftest make-dispatch-envelope-creates-test-envelope
  (testing "TEST envelope has required fields"
    (let [env (pb/make-dispatch-envelope "FM-001" :TEST
                {:item-id "F1-opposite"
                 :conjecture-id "H-F1"
                 :method "SAT"
                 :params {:n [5 6]}})]
      (is (= :TEST (:move env)))
      (is (= "FM-001" (:problem-id env)))
      (is (= "F1-opposite" (:item-id env)))
      (is (= "H-F1" (:conjecture-id env)))
      (is (map? (:report-format env)))
      (is (string? (:created-at env))))))

(deftest make-dispatch-envelope-rejects-invalid-move
  (testing "invalid move type throws"
    (is (thrown? clojure.lang.ExceptionInfo
          (pb/make-dispatch-envelope "FM-001" :INVALID-MOVE
            {:item-id "test"})))))

(deftest summary-includes-conjecture-and-heuristic-counts
  (testing "summary now reports conjecture and heuristic counts"
    (let [result (pb/summary "FM-001")]
      (is (:ok result))
      (let [r (:result result)]
        (is (number? (:conjectures-count r)))
        (is (number? (:conjectures-tested r)))
        (is (number? (:heuristics-count r)))
        (is (number? (:heuristics-active r)))))))
