(ns futon3c.apm.generated-contract
  "Validation boundary for the canonical contract emitted by Lean."
  (:require [cheshire.core :as json]))

(def required-bounds
  {:solver-max-rounds 50
   :solver-checkpoint-every 10
   :student-attempts 3
   :guide-interventions 2
   :analyst-tenure-frames 2
   :seat-turn-timeout-ms 3600000
   :zai-request-timeout-ms 300000})

(def required-dispatch-policy
  {:preannounce-required true
   :activation-status 202
   :idempotent-reactivation true
   :terminal-command-own-exit 0
   :persist-claim true
   :persist-receipt true
   :restart-same-job true
   :typed-terminal-output-required true
   :typed-role-submission-tool-required true
   :submission-authority-controller-owned true
   :submission-persisted-before-advance true
   :conversation-is-receipt-authority false
   :submission-conflict-policy "reject"
   :submission-covered-role-count 7
   :terminal-collection-required true
   :terminal-collection-persisted true
   :terminal-collection-before-missing-observation true
   :terminal-collection-attempts-per-role 1
   :terminal-repair-attempts-per-role 1
   :terminal-collection-covered-role-count 7
   :promotion-review-enums-normalized true
   :promotion-approved-candidates-accounted true
   :promotion-approved-unattached-refused true
   :promotion-rejections-explicit true
   :role-terminal-budgets
   {:solver {:collection-attempts 1 :repair-attempts 1}
    :student {:collection-attempts 1 :repair-attempts 1}
    :guide {:collection-attempts 1 :repair-attempts 1}
    :scribe {:collection-attempts 1 :repair-attempts 1}
    :proctor {:collection-attempts 1 :repair-attempts 1}
    :promotion-proctor {:collection-attempts 1 :repair-attempts 1}
    :analyst {:collection-attempts 1 :repair-attempts 1}}
   :missing-observation-student-only true
   :unbounded-conversational-retries false
   :typed-submission-migration-max 1
   :typed-submission-migration-fresh-session true
   :typed-submission-migration-preserves-snapshot true
   :activation-supersession-max 1
   :activation-supersession-requires-cancellation true
   :activation-supersession-distinct-job true
   :deterministic-job-id-before-announce true
   :announce-activate-request-identical true
   :queued-job-survives-restart true
   :conflicting-job-replay-policy "reject"
   :terminal-output-repair-attempts 1
   :repair-feedback-findings-required true
   :client-timeout-is-success false
   :terminal-lifecycle-actions-covered ["close-block" "close-campaign"]
   :retirement-binds-recorded-terminal-head true
   :terminal-collection-is-supervisor-progress true
   :artifact-identity-from-authority-not-observation true
   :preflight-requires-positive-sorry-baseline true
   :preflight-blocking-warning-count 0
   :preflight-nonblocking-warning-kinds
   ["linter" "deprecation" "compiler-warning"]
   :coordinator-intent-persisted-before-activation true
   :coordinator-restart-reconciles-deterministic-job-id true
   :coordinator-startup-uses-typed-registry true
   :coordinator-one-registration-per-problem true
   :coordinator-retries-increment-same-entry true
   :coordinator-retry-beyond-maximum-refused true
   :coordinator-startup-directory-heuristics false})

(def required-memory-policy
  {:content-addressed-snapshot true :admit-after-solve-verify true
   :independent-review true :student-attempts 3
   :fresh-student-sessions true :exact-snapshot-binding true
   :fresh-session-rotation-mints-new-id true
   :student-session-distinctness-required-for-closed-frame true
   :fresh-attempt-worktree-reset-to-base true
   :attempt-state-preserved-before-reset true
   :guide-deposits-independent-review true
   :guide-union-snapshot-content-addressed true
   :next-student-binds-latest-reviewed-snapshot true
   :candidate-pattern-binding-required true
   :open-reviewed-corpus-search true
   :search-capable-roles ["student" "scribe" "promotion-proctor"]
   :search-query-trace-persisted true
   :search-results-content-addressed true
   :student-open-search-distinct-from-proactive-snapshot true
   :self-reported-query-is-search-evidence false
   :student-dispatch-witness-required true
   :student-dispatch-required-fields
   ["attempt-ordinal" "promotion-receipt-id" "snapshot-id"
    "snapshot-digest" "accessible-memory-ids"]})

(def required-isolation-policy
  {:campaign-scoped-regulator true :campaign-scoped-problem-buffer true
   :distinct-continuation-session true :distinct-analyst-session true
   :projection-ledger-binding true})

(def required-promotion-policy
  {:distinct-promotion-proctor true
   :base-problem-blob-required true
   :problem-path-required true
   :solver-final-head-required true
   :typed-lanes-required 4
   :persisted-review-reason-required true
   :persisted-review-residual-required true
   :student-query-log-required true})

(def required-terminal-policy
  {:certified-phase-receipts 11
   :separate-problem-frame-outcomes true
   :learning-outcome-required true
   :solved-partial-bankable true
   :bankable-solved-successor-eligible true
   :unsolved-partial-retry-same-problem true
   :retry-requires-retained-solver-head true
   :retry-does-not-advance-problem-queue true
   :close-result-wire-canonicalization true
   :missing-observation-receipt-type "student-observation-missing"
   :missing-observation-author "controller"
   :missing-observation-may-satisfy-observation-dependency true
   :missing-observation-may-impersonate-student false})

(def required-analyst-policy
  {:outside-frame-order true :wake-after-terminal-only true
   :partial-terminal-wakes-analyst true
   :exactly-once-per-frame true :append-only-series-input true
   :tenure-frames 2 :successor-handoff-required true
   :in-flight-mutation false})

(defn read-contract [path]
  (try
    {:ok true :contract (json/parse-string (slurp path) true)}
    (catch Exception e
      {:ok false :error/code :generated-contract-unreadable
       :exception/message (.getMessage e)})))

(defn expected-transitions [phase-order]
  (mapv (fn [from to] {:from from :to to})
        phase-order (concat (rest phase-order) [nil])))

(defn validate
  "Refuse an emitted artifact whose transition table is not total over its
   phase order, whose bounds drift, or whose verify edge bypasses promotion."
  [contract]
  (let [phase-order (:phase-order contract)
        transitions (:transitions contract)
        findings
        (cond-> []
          (not= 1 (:schema-version contract))
          (conj :generated-contract-schema-version-invalid)
          (not= "apm-complete-frame-cycle-v2" (:contract-id contract))
          (conj :generated-contract-id-invalid)
          (not= (count phase-order) (count (distinct phase-order)))
          (conj :generated-contract-phase-duplicate)
          (not= (expected-transitions phase-order) transitions)
          (conj :generated-contract-transition-table-invalid)
          (not= "promote-solver"
                (:to (some #(when (= "verify" (:from %)) %) transitions)))
          (conj :generated-contract-verify-bypasses-promotion)
          (not= required-bounds (:bounds contract))
          (conj :generated-contract-bounds-invalid)
          (not= required-dispatch-policy (:dispatch-policy contract))
          (conj :generated-contract-dispatch-policy-invalid)
          (not= required-memory-policy (:memory-policy contract))
          (conj :generated-contract-memory-policy-invalid)
          (not= required-promotion-policy (:promotion-policy contract))
          (conj :generated-contract-promotion-policy-invalid)
          (not= required-isolation-policy (:isolation-policy contract))
          (conj :generated-contract-isolation-policy-invalid)
          (not= required-terminal-policy (:terminal-policy contract))
          (conj :generated-contract-terminal-policy-invalid)
          (not= required-analyst-policy (:analyst-policy contract))
          (conj :generated-contract-analyst-policy-invalid))]
    (if (seq findings)
      {:ok false :error/code :generated-contract-invalid :findings findings}
      {:ok true :contract contract})))

(defn validate-round-trip [path clojure-contract]
  (let [loaded (read-contract path)]
    (if-not (:ok loaded)
      loaded
      (let [validated (validate (:contract loaded))
            emitted-phases (mapv keyword (get-in loaded [:contract :phase-order]))]
        (if-not (:ok validated)
          validated
          (if (= emitted-phases (:phase-order clojure-contract))
            {:ok true :contract (:contract loaded)}
            {:ok false :error/code :generated-contract-round-trip-mismatch
             :finding {:emitted emitted-phases
                       :clojure (:phase-order clojure-contract)}}))))))
