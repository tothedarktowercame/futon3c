(ns futon3c.proof.bridge-test
  "Tests for the proof bridge — verifies the agent-facing API
   correctly delegates to ProofBackend tools."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing use-fixtures]]
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
