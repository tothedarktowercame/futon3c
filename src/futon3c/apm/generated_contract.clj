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
   :student-turn-timeout-ms 1800000
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
   :submission-covered-role-count 8
   :terminal-collection-required true
   :terminal-collection-persisted true
   :terminal-collection-before-missing-observation true
   :terminal-collection-attempts-per-role 1
   :terminal-repair-attempts-per-role 1
   :terminal-collection-covered-role-count 8
   :promotion-review-enums-normalized true
   :promotion-approved-candidates-accounted true
   :promotion-approved-unattached-refused true
   :promotion-rejections-explicit true
   :role-terminal-budgets
   {:solver {:collection-attempts 1 :repair-attempts 1}
    :student {:collection-attempts 1 :repair-attempts 1}
    :guide {:collection-attempts 1 :repair-attempts 1}
    :scribe {:collection-attempts 1 :repair-attempts 1}
    :zai-scribe {:collection-attempts 1 :repair-attempts 1}
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
   :frame-terminal-persisted-before-retirement true
   :retirement-replay-uses-persisted-terminal true
   :retired-workspace-absence-is-postcondition true
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
   :coordinator-startup-directory-heuristics false
   :gate-manifest-authority-binding "campaign-and-frame-exact"
   :qualification-contract-digest-binding "registered-and-observed-exact"
   :proof-target-name-authority "source-declaration"})

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
   :review-verdicts
   {:judgements ["approve" "reassign" "reject"]
    :apparatus-failures ["cannot-judge"]}
   :promotion-pass-resolution-required true
   :exact-pattern-set-required-for ["approve"]
   :nonapproval-pattern-actions
   {:reassign "replace"
    :reject "retain-proposed"}
   :completed-pass-required true
   :completed-pass-candidate-accounting "exactly-once"
   :materialized-artifact-required-fields
   ["artifact-id" "content-digest" "persisted-content-digest"
    "read-back-content-digest" "persistence-receipt-id"]
   :materialized-artifact-digests-must-match true
   :review-evidence-materialized-before-disposition true
   :returned-review-controller-evidence-required false
   :persisted-review-controller-evidence-required true
   :nonpublishing-dispositions ["reject"]
   :projection-failure-action
   "hold-at-review-awaiting-apparatus-repair"
   :projection-repair-reuses-persisted-judgement true
   :projection-repair-redispatches-reviewer false
   :projection-repair-max-attempts 1
   :projection-repair-exhaustion-action
   "park-frame-and-continue-queue"
   :projection-repair-exhaustion-decision-owner "claude-supervisor"
   :projection-repair-exhaustion-bell-required true
   :promotion-successor-validation
   "before-snapshot-publication-and-certification"
   :certified-pass-snapshot-materialized true
   :certified-pass-published-candidates-exact true
   :review-dispatch-resolution-required true
   :review-dispatch-candidate-required
   ["persisted" "fetchable" "parent-pattern-fetchable"]
   :review-dispatch-reviewer-inputs-required
   ["base-problem-blob-fetchable" "solver-final-head-fetchable"
    "evidence-job-traces-fetchable"]
   :unresolved-review-dispatch-action
   "hold-at-deposit-awaiting-apparatus-repair"
   :unresolved-review-resume-action
   "append-only-successor-after-contract-change"
   :resolved-judgements-immutable-on-resume true
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
   :problem-outcomes ["solved" "unsolved" "refuted"]
   :refuted-requires-frame-void true
   :refuted-retry-same-problem false
   :statement-refuted-void-outcome "refuted"
   :non-refutation-void-outcome "unsolved"
   :early-void-canonical-prefix true
   :voided-slot-reuses-logical-problem-id true
   :corrected-pins-require-plan-revision true
   :statement-repair-role "guide"
   :statement-repair-max-attempts 1
   :statement-repair-handoff-required true
   :statement-repair-intent-before-dispatch true
   :statement-repair-dispatch-retryable true
   :statement-repair-terminal-result-collected true
   :statement-repair-success-action "install-and-remint"
   :statement-repair-required-output
   ["replacement-pinned-problem" "guide-receipt"]
   :statement-repair-exhaustion-action "discard-and-advance"
   :retry-requires-retained-solver-head true
   :retry-does-not-advance-problem-queue true
   :close-result-wire-canonicalization true
   :missing-observation-receipt-type "student-observation-missing"
   :missing-observation-author "controller"
   :missing-observation-may-satisfy-observation-dependency true
   :missing-observation-may-impersonate-student false
   :recovered-observation-receipt-type "student-observation-recovered"
   :recovered-observation-author "controller"
   :recovered-observation-satisfies-observation true
   :recovered-observation-forces-partial-learning false
   :recovered-observation-certifies-rejected-candidate false})

(def required-student-candidate-policy
  {:student-terminal-candidate-required true
   :student-candidate-content-addressed true
   :student-candidate-lean-validated true
   :student-candidate-persisted-before-receipt true
   :student-candidate-replay-idempotent true
   :rejected-student-candidate-evidence-only true
   :missing-observation-records-certified-candidate false
   :missing-observation-controller-memory-use-required true
   :missing-observation-scribe-compatible true})

(def required-analyst-policy
  {:outside-frame-order true :wake-after-terminal-only true
   :partial-terminal-wakes-analyst true
   :exactly-once-per-frame true :append-only-series-input true
   :tenure-frames 2 :successor-handoff-required true
   :in-flight-mutation false})

(def required-submission-schemas
  {:schema-version 1
   :controller-derived-fields
   ["job-id" "dispatch-id" "agent-id" "frame-id" "problem-id" "phase"
    "role" "attempt-ordinal" "submission-attempt" "fresh-session-nonce"
    "memory-snapshot" "memory-cascade"
    "evidence.memory-cascade.used-via-cascade"
    "evidence.memory-use.receipt-id"
    "evidence.memory-use.snapshot-id" "evidence.memory-use.snapshot-digest"
    "evidence.memory-use.accessible-memory-ids"
    "evidence.memory-use.surfaced-ids" "evidence.memory-use.queries"
    "evidence.memory-search-receipt-ids"]
   :role-authored-fields
   {:solver ["command-own-exit" "outcome" "failure-account" "evidence"]
    :student ["command-own-exit" "outcome" "failure-account"
              "evidence.memory-use.used-ids"]
    :guide ["command-own-exit" "outcome" "failure-account" "evidence"]
    :scribe ["command-own-exit" "outcome" "failure-account" "evidence"]
    :zai-scribe ["command-own-exit" "outcome" "failure-account" "evidence"]
    :proctor ["command-own-exit" "outcome" "failure-account" "evidence"]
    :promotion-proctor
    ["command-own-exit" "outcome" "failure-account" "evidence"]
    :analyst ["command-own-exit" "outcome" "failure-account" "evidence"]}
   :student-memory-use
   {:role-authored-fields ["used-ids"]
    :controller-derived-fields
    ["receipt-id" "snapshot-id" "snapshot-digest" "accessible-memory-ids"
     "surfaced-ids" "queries"]}
   :self-reported-controller-identifiers-are-evidence false})

(def required-phase-io
  {:preflight {:requires [] :produces ["preflight-receipt"]}
   :solve {:requires ["preflight-receipt"]
           :produces ["solve-receipt" "committed-proof"]}
   :verify {:requires ["solve-receipt" "committed-proof"]
            :produces ["verify-receipt"]}
   :promote-solver {:requires ["solve-receipt" "verify-receipt"]
                    :produces ["solver-promotion-receipt"
                               "solver-memory-snapshot"]}
   :student-attempt-1
   {:requires ["preflight-receipt" "solver-memory-snapshot"]
    :produces ["student-attempt-1-receipt" "memory-use-1-receipt"]}
   :guide-intervention-1
   {:requires ["student-attempt-1-receipt" "memory-use-1-receipt"]
    :produces ["guide-intervention-1-receipt"]}
   :student-attempt-2
   {:requires ["guide-intervention-1-receipt" "solver-memory-snapshot"]
    :produces ["student-attempt-2-receipt" "memory-use-2-receipt"]}
   :guide-intervention-2
   {:requires ["student-attempt-2-receipt" "memory-use-2-receipt"]
    :produces ["guide-intervention-2-receipt"]}
   :student-attempt-3
   {:requires ["guide-intervention-2-receipt" "solver-memory-snapshot"]
    :produces ["student-attempt-3-receipt" "memory-use-3-receipt"]}
   :scribe-reduce
   {:requires ["solve-receipt" "verify-receipt" "solver-promotion-receipt"
               "student-attempt-1-receipt" "memory-use-1-receipt"
               "student-attempt-2-receipt" "memory-use-2-receipt"
               "student-attempt-3-receipt" "memory-use-3-receipt"
               "guide-intervention-1-receipt" "guide-intervention-2-receipt"]
    :produces ["scribe-lane-receipt" "memory-disposition-receipt"
               "promotion-review-receipt"]}
   :close-frame
   {:requires ["solve-receipt" "verify-receipt" "solver-promotion-receipt"
               "student-attempt-1-receipt" "memory-use-1-receipt"
               "student-attempt-2-receipt" "memory-use-2-receipt"
               "student-attempt-3-receipt" "memory-use-3-receipt"
               "guide-intervention-1-receipt" "guide-intervention-2-receipt"
               "scribe-lane-receipt" "memory-disposition-receipt"
               "promotion-review-receipt"]
    :produces ["frame-close-receipt" "frame-trace"]}})

(def required-receipt-schemas
  {:frame-preflight ["receipt/id" "receipt/type" "receipt/frame-id"
                     "receipt/problem-id" "receipt/result"]
   :frame-solve ["receipt/id" "receipt/type" "receipt/frame-id"
                 "receipt/problem-id" "receipt/final-head" "receipt/lean"]
   :frame-verify ["receipt/id" "receipt/type" "receipt/frame-id"
                  "receipt/problem-id" "receipt/solve-receipt-id"
                  "receipt/mathematical-sound?"]
   :solver-promotion
   ["receipt/id" "receipt/type" "receipt/frame-id" "receipt/problem-id"
    "receipt/input-receipt-ids" "receipt/lanes" "receipt/dispositions"
    "receipt/promotion-reviews" "receipt/snapshot-id" "receipt/snapshot-digest"
    "receipt/snapshot-path" "receipt/reviewed-memory-ids"
    "receipt/independent-review?" "receipt/promotion-pass-witness"]
   :student-attempt
   ["receipt/id" "receipt/type" "receipt/frame-id" "receipt/problem-id"
    "receipt/attempt-ordinal" "receipt/fresh-session-id" "receipt/job-id"
    "receipt/outcome" "receipt/failure-account" "receipt/memory-use"
    "receipt/memory-snapshot"]
   :student-observation-missing
   ["receipt/id" "receipt/type" "receipt/frame-id" "receipt/problem-id"
    "receipt/attempt-ordinal" "receipt/job-id" "receipt/author"
    "receipt/reason" "receipt/repair-attempts" "receipt/memory-snapshot"
    "receipt/harness-observed" "receipt/memory-use"]
   :student-observation-recovered
   ["receipt/id" "receipt/type" "receipt/frame-id" "receipt/problem-id"
    "receipt/attempt-ordinal" "receipt/job-id" "receipt/author"
    "receipt/reason" "receipt/repair-attempts" "receipt/memory-snapshot"
    "receipt/harness-observed" "receipt/memory-use"
    "receipt/candidate-disposition"]
   :guide-intervention
   ["receipt/id" "receipt/type" "receipt/frame-id" "receipt/problem-id"
    "receipt/intervention-ordinal" "receipt/mode" "receipt/input-attempt-id"
    "receipt/effect" "receipt/channel-audit"]
   :scribe-reduce
   ["receipt/id" "receipt/type" "receipt/frame-id" "receipt/problem-id"
    "receipt/input-receipt-ids" "receipt/lanes" "receipt/dispositions"
    "receipt/promotion-reviews" "receipt/promotion-pass-witness"]
   :frame-close
   ["receipt/id" "receipt/type" "receipt/frame-id" "receipt/problem-id"
    "receipt/input-receipt-ids" "receipt/trace-id" "receipt/result"
    "receipt/learning-outcome"]})

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
        terminal-policy (:terminal-policy contract)
        candidate-policy
        (select-keys terminal-policy (keys required-student-candidate-policy))
        allowed-terminal-policy-keys
        (into (set (keys required-terminal-policy))
              (keys required-student-candidate-policy))
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
          (not= required-terminal-policy
                (select-keys terminal-policy (keys required-terminal-policy)))
          (conj :generated-contract-terminal-policy-invalid)
          (not (every? allowed-terminal-policy-keys (keys terminal-policy)))
          (conj :generated-contract-terminal-policy-invalid)
          (and (seq candidate-policy)
               (not= required-student-candidate-policy candidate-policy))
          (conj :generated-contract-student-candidate-policy-invalid)
          (not= required-analyst-policy (:analyst-policy contract))
          (conj :generated-contract-analyst-policy-invalid)
          (not= required-submission-schemas (:submission-schemas contract))
          (conj :generated-contract-submission-schemas-invalid)
          (not= required-phase-io (:phases contract))
          (conj :generated-contract-phase-io-invalid)
          (not= required-receipt-schemas
                (update-vals (:receipt-schemas contract) :required))
          (conj :generated-contract-receipt-schemas-invalid))]
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
