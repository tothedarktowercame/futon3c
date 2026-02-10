(ns futon3c.evidence.integration-test
  "M-forum-refactor Part IV: evidence landscape integration tests.

   End-to-end scenarios exercising store → threads → validate together,
   plus multi-timescale coexistence, forks, conjectures, ephemeral
   compaction, pattern mining, and pipeline wiring."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [futon3c.evidence.store :as store]
            [futon3c.evidence.threads :as threads]
            [futon3c.social.shapes :as shapes]
            [futon3c.social.test-fixtures :as fix]
            [futon3c.social.validate :as validate])
  (:import [java.time Instant]))

(use-fixtures
  :each
  (fn [f]
    (store/reset-store!)
    (f)))

;; =============================================================================
;; Helpers
;; =============================================================================

(defn- ts [epoch-ms]
  (str (Instant/ofEpochMilli epoch-ms)))

(defn- append! [m]
  (let [r (store/append! m)]
    (assert (:ok r) (str "append! failed: " (pr-str r)))
    (:entry r)))

;; =============================================================================
;; 1. End-to-end: append → project → proof steps → validate
;; =============================================================================

(deftest end-to-end-append-project-validate
  (testing "append evidence → project thread → post proof steps → validate outcome"
    (let [msg-id "msg-e2e-1"
          subj (fix/make-artifact-ref :evidence msg-id)
          patterns (fix/mock-patterns)]

      ;; Post goal
      (append! {:evidence-id "g1" :subject subj :type :coordination
                :claim-type :goal :author "claude-1" :body {:goal "implement feature X"}
                :tags [:integration]})

      ;; Post proof steps
      (append! {:evidence-id "s1" :subject subj :type :coordination
                :claim-type :step :author "claude-1" :body {:action "write shapes"}
                :tags [:integration] :in-reply-to "g1"})
      (append! {:evidence-id "s2" :subject subj :type :coordination
                :claim-type :evidence :author "codex-1" :body {:action "tests pass"}
                :tags [:integration] :in-reply-to "s1"})

      ;; Thread projection before conclusion
      (let [tp (threads/project-thread store/!store subj)]
        (is (= 3 (:thread/entry-count tp)))
        (is (= :open (:thread/status tp)))
        (is (= #{"claude-1" "codex-1"} (:thread/participants tp))))

      ;; Validate without conclusion — should be coordination-failed
      (let [receipt (fix/make-dispatch-receipt {:receipt/msg-id msg-id})
            result (validate/validate-outcome receipt patterns store/!store)]
        (fix/assert-valid! shapes/CoordinationOutcome result)
        (is (false? (:outcome/valid? result)))
        (is (= :coordination-failed (:outcome/type result))))

      ;; Post conclusion
      (append! {:evidence-id "c1" :subject subj :type :coordination
                :claim-type :conclusion :author "claude-1"
                :body {:summary "feature X complete, tests pass"}
                :tags [:integration] :in-reply-to "s2"})

      ;; Thread now closed
      (let [tp (threads/project-thread store/!store subj)]
        (is (= 4 (:thread/entry-count tp)))
        (is (= :closed (:thread/status tp))))

      ;; Validate with conclusion — should be coordination-complete
      (let [receipt (fix/make-dispatch-receipt {:receipt/msg-id msg-id})
            result (validate/validate-outcome receipt patterns store/!store)]
        (fix/assert-valid! shapes/CoordinationOutcome result)
        (is (true? (:outcome/valid? result)))
        (is (= :coordination-complete (:outcome/type result)))))))

;; =============================================================================
;; 2. Multi-timescale coexistence
;; =============================================================================

(deftest multi-timescale-evidence-coexists
  (testing "entries from different timescales coexist in the same store"
    (let [subj (fix/make-artifact-ref :mission "M-multi")]
      ;; Social timescale
      (append! {:evidence-id "social-1" :subject subj :type :coordination
                :claim-type :goal :author "claude-1" :body {} :tags [:social]})
      (append! {:evidence-id "social-2" :subject subj :type :presence-event
                :claim-type :observation :author "claude-1" :body {:event "joined"}
                :tags [:social] :in-reply-to "social-1"})

      ;; Task timescale
      (append! {:evidence-id "task-1" :subject subj :type :gate-traversal
                :claim-type :step :author "codex-1" :body {:gate "G5"}
                :tags [:task] :in-reply-to "social-1"})

      ;; Glacial timescale
      (append! {:evidence-id "glacial-1" :subject subj :type :reflection
                :claim-type :observation :author "claude-1"
                :body {:par "recurring tension in auth flow"}
                :tags [:glacial] :in-reply-to "social-1"})

      ;; All coexist in one thread
      (let [tp (threads/project-thread store/!store subj)]
        (is (= 4 (:thread/entry-count tp)))
        (is (= #{"claude-1" "codex-1"} (:thread/participants tp))))

      ;; Query by type filters correctly
      (is (= 1 (count (store/query {:query/type :gate-traversal}))))
      (is (= 1 (count (store/query {:query/type :reflection}))))
      (is (= 1 (count (store/query {:query/type :coordination}))))
      (is (= 1 (count (store/query {:query/type :presence-event})))))))

;; =============================================================================
;; 3. Front page query
;; =============================================================================

(deftest front-page-excludes-ephemeral
  (testing "recent-activity returns across subjects, excludes ephemeral"
    (let [s1 (fix/make-artifact-ref :mission "M1")
          s2 (fix/make-artifact-ref :mission "M2")]
      ;; Durable entries on two subjects
      (store/append* store/!store
                     (fix/make-evidence-entry {:evidence/id "d1" :evidence/subject s1
                                               :evidence/at (ts 3000)}))
      (store/append* store/!store
                     (fix/make-evidence-entry {:evidence/id "d2" :evidence/subject s2
                                               :evidence/at (ts 4000)}))
      ;; Ephemeral entry
      (store/append* store/!store
                     (fix/make-evidence-entry {:evidence/id "eph1" :evidence/subject s1
                                               :evidence/at (ts 5000)
                                               :evidence/ephemeral? true}))

      (let [activity (store/recent-activity {:limit 10})]
        (is (= 2 (count activity)) "front page excludes ephemeral")
        (is (= #{"d2" "d1"} (set (map :evidence/id activity))))
        ;; Newest first
        (is (= "d2" (:evidence/id (first activity))))))))

;; =============================================================================
;; 4. Fork scenario
;; =============================================================================

(deftest fork-branches-accumulate-evidence
  (testing "proof attempt forks, both branches accumulate evidence independently"
    (let [subj (fix/make-artifact-ref :mission "M-fork")]
      ;; Common prefix
      (append! {:evidence-id "g" :subject subj :type :coordination
                :claim-type :goal :author "claude-1"
                :body {:goal "fix auth bug"} :tags [:fork-test]})
      (append! {:evidence-id "s1" :subject subj :type :coordination
                :claim-type :step :author "claude-1"
                :body {:approach "check token validation"} :tags [:fork-test]
                :in-reply-to "g"})

      ;; Branch A: fork from s1
      (append! {:evidence-id "fA" :subject subj :type :coordination
                :claim-type :step :author "claude-1"
                :body {:approach "rewrite token parser"} :tags [:fork-test]
                :fork-of "s1" :in-reply-to "s1"})
      (append! {:evidence-id "fA-ev" :subject subj :type :coordination
                :claim-type :evidence :author "claude-1"
                :body {:result "parser works"} :tags [:fork-test]
                :in-reply-to "fA"})

      ;; Branch B: also fork from s1
      (append! {:evidence-id "fB" :subject subj :type :coordination
                :claim-type :step :author "codex-1"
                :body {:approach "patch middleware"} :tags [:fork-test]
                :fork-of "s1" :in-reply-to "s1"})
      (append! {:evidence-id "fB-ev" :subject subj :type :coordination
                :claim-type :evidence :author "codex-1"
                :body {:result "middleware patched"} :tags [:fork-test]
                :in-reply-to "fB"})

      ;; Main thread shows forked status
      (let [tp (threads/project-thread store/!store subj)]
        (is (= 6 (:thread/entry-count tp)))
        (is (= :forked (:thread/status tp)))

        ;; Fork projections — grouped by branch point
        ;; Both fA and fB fork from s1, so one fork projection from that point
        (let [forks (threads/thread-forks tp)]
          (is (= 1 (count forks)) "one fork projection from branch point s1")
          (let [fork (first forks)]
            ;; Fork projection includes prefix (g, s1) + both branches + descendants
            (is (>= (:thread/entry-count fork) 4)
                "fork has prefix + both branch entries")
            (is (= :forked (:thread/status fork)))))

        ;; Forks queryable from store
        (let [forks-of-s1 (store/get-forks "s1")]
          (is (= #{"fA" "fB"} (set (map :evidence/id forks-of-s1)))))))))

;; =============================================================================
;; 5. Conjecture lifecycle
;; =============================================================================

(deftest conjecture-lifecycle-open-to-confirmed
  (testing "conjecture posted → evidence accumulated → conclusion confirms"
    (let [subj (fix/make-artifact-ref :mission "M-conj")]
      ;; Goal
      (append! {:evidence-id "g" :subject subj :type :coordination
                :claim-type :goal :author "claude-1"
                :body {:goal "verify auth performance"} :tags [:conj-test]})

      ;; Conjecture: "token cache will reduce latency by 50%"
      (append! {:evidence-id "cj1" :subject subj :type :conjecture
                :claim-type :conjecture :author "claude-1"
                :body {:prediction "token cache reduces latency 50%"}
                :tags [:conj-test] :in-reply-to "g" :conjecture? true})

      ;; Conjecture is open
      (let [tp (threads/project-thread store/!store subj)
            cs (threads/thread-conjectures tp)]
        (is (= 1 (count cs)))
        (is (= :open (:status (first cs)))))

      ;; Accumulate evidence
      (append! {:evidence-id "ev1" :subject subj :type :coordination
                :claim-type :evidence :author "codex-1"
                :body {:benchmark "latency dropped 55%"}
                :tags [:conj-test] :in-reply-to "cj1"})

      ;; Conclusion confirms the conjecture
      (append! {:evidence-id "concl" :subject subj :type :coordination
                :claim-type :conclusion :author "claude-1"
                :body {:summary "conjecture confirmed: 55% reduction"}
                :tags [:conj-test] :in-reply-to "cj1"})

      ;; Conjecture now confirmed, thread closed
      (let [tp (threads/project-thread store/!store subj)
            cs (threads/thread-conjectures tp)]
        (is (= :confirmed (:status (first cs))))
        (is (= :closed (:thread/status tp)))))))

(deftest conjecture-lifecycle-refuted
  (testing "conjecture posted → correction refutes it"
    (let [subj (fix/make-artifact-ref :mission "M-conj-refute")]
      (append! {:evidence-id "g" :subject subj :type :coordination
                :claim-type :goal :author "claude-1"
                :body {:goal "test hypothesis"} :tags [:conj-test]})
      (append! {:evidence-id "cj" :subject subj :type :conjecture
                :claim-type :conjecture :author "claude-1"
                :body {:prediction "X will improve Y"}
                :tags [:conj-test] :in-reply-to "g" :conjecture? true})

      ;; Correction refutes the conjecture
      (append! {:evidence-id "corr" :subject subj :type :correction
                :claim-type :correction :author "codex-1"
                :body {:finding "X actually degraded Y"}
                :tags [:conj-test] :in-reply-to "cj"})

      (let [tp (threads/project-thread store/!store subj)
            cs (threads/thread-conjectures tp)]
        (is (= :refuted (:status (first cs))))))))

;; =============================================================================
;; 6. Ephemeral compaction
;; =============================================================================

(deftest ephemeral-compaction-preserves-durable
  (testing "ephemeral compaction removes old ephemeral, preserves durable entries"
    (let [subj (fix/make-artifact-ref :mission "M-eph")]
      ;; Durable goal
      (store/append* store/!store
                     (fix/make-evidence-entry
                      {:evidence/id "g" :evidence/subject subj
                       :evidence/claim-type :goal :evidence/at (ts 1000)}))

      ;; Ephemeral intermediate computations (old)
      (store/append* store/!store
                     (fix/make-evidence-entry
                      {:evidence/id "eph-old-1" :evidence/subject subj
                       :evidence/at (ts 1500) :evidence/ephemeral? true}))
      (store/append* store/!store
                     (fix/make-evidence-entry
                      {:evidence/id "eph-old-2" :evidence/subject subj
                       :evidence/at (ts 1800) :evidence/ephemeral? true}))

      ;; Ephemeral recent computation
      (store/append* store/!store
                     (fix/make-evidence-entry
                      {:evidence/id "eph-new" :evidence/subject subj
                       :evidence/at (ts 5000) :evidence/ephemeral? true}))

      ;; Durable conclusion
      (store/append* store/!store
                     (fix/make-evidence-entry
                      {:evidence/id "concl" :evidence/subject subj
                       :evidence/claim-type :conclusion :evidence/at (ts 6000)}))

      ;; Before compaction: 5 entries total
      (is (= 5 (count (store/query {:query/include-ephemeral? true}))))

      ;; Compact ephemeral older than ts 3000
      (let [result (store/compact-ephemeral! {:older-than (ts 3000)})]
        (is (= 2 (:compacted result)) "two old ephemeral entries compacted"))

      ;; After: 3 entries remain (g, eph-new, concl)
      (let [remaining (store/query {:query/include-ephemeral? true})]
        (is (= 3 (count remaining)))
        (is (= #{"g" "eph-new" "concl"} (set (map :evidence/id remaining)))))

      ;; Default query (no ephemeral) returns only durable
      (let [durable (store/query {})]
        (is (= #{"g" "concl"} (set (map :evidence/id durable))))))))

;; =============================================================================
;; 7. Pattern mining from completed threads
;; =============================================================================

(deftest pattern-mining-from-completed-thread
  (testing "thread-patterns extracts pattern usage from a completed thread"
    (let [subj (fix/make-artifact-ref :mission "M-patterns")]
      (append! {:evidence-id "g" :subject subj :type :coordination
                :claim-type :goal :author "claude-1"
                :body {:goal "refactor dispatch"} :tags [:patterns]})

      ;; Pattern selection
      (append! {:evidence-id "psr1" :subject subj :type :pattern-selection
                :claim-type :step :author "claude-1"
                :body {:pattern :delivery-receipt :reason "need delivery guarantees"}
                :tags [:patterns] :in-reply-to "g"
                :pattern-id :delivery-receipt})

      ;; Pattern applied
      (append! {:evidence-id "pur1" :subject subj :type :pattern-outcome
                :claim-type :evidence :author "claude-1"
                :body {:success? true :detail "receipts working"}
                :tags [:patterns] :in-reply-to "psr1"
                :pattern-id :delivery-receipt})

      ;; Second pattern
      (append! {:evidence-id "psr2" :subject subj :type :pattern-selection
                :claim-type :step :author "codex-1"
                :body {:pattern :rendezvous-handshake}
                :tags [:patterns] :in-reply-to "g"
                :pattern-id :rendezvous-handshake})

      ;; Conclusion
      (append! {:evidence-id "c" :subject subj :type :coordination
                :claim-type :conclusion :author "claude-1"
                :body {:summary "dispatch refactored with delivery receipts"}
                :tags [:patterns] :in-reply-to "pur1"})

      (let [tp (threads/project-thread store/!store subj)
            pats (threads/thread-patterns tp)]
        (is (= :closed (:thread/status tp)))
        (is (= 2 (count pats)) "two patterns used")
        (let [by-id (into {} (map (juxt :pattern-id identity) pats))]
          ;; delivery-receipt: selected + outcome = 2 applications, success
          (is (= 2 (:applied-count (get by-id :delivery-receipt))))
          (is (true? (:success? (get by-id :delivery-receipt))))
          ;; rendezvous-handshake: selected only = 1 application, no outcome
          (is (= 1 (:applied-count (get by-id :rendezvous-handshake))))
          (is (false? (:success? (get by-id :rendezvous-handshake)))))))))

;; =============================================================================
;; 8. Pipeline wiring: S-dispatch receipt → evidence → S-validate
;; =============================================================================

(deftest pipeline-dispatch-to-evidence-to-validate
  (testing "pipeline receipt generates evidence that S-validate can assess"
    (let [msg-id "msg-pipeline-ev"
          subj (fix/make-artifact-ref :evidence msg-id)
          patterns (fix/mock-patterns)
          receipt (fix/make-dispatch-receipt {:receipt/msg-id msg-id})]

      ;; Simulate: after dispatch, components emit evidence about the coordination
      (append! {:evidence-id "ev-presence" :subject subj :type :presence-event
                :claim-type :observation :author "claude-1"
                :body {:event "agent joined"} :tags [:pipeline]})
      (append! {:evidence-id "ev-goal" :subject subj :type :coordination
                :claim-type :goal :author "claude-1"
                :body {:goal "coordinate task"} :tags [:pipeline]
                :in-reply-to "ev-presence"})
      (append! {:evidence-id "ev-mode" :subject subj :type :mode-transition
                :claim-type :step :author "claude-1"
                :body {:from :discuss :to :diagnose} :tags [:pipeline]
                :in-reply-to "ev-goal"})
      (append! {:evidence-id "ev-dispatch" :subject subj :type :coordination
                :claim-type :step :author "claude-1"
                :body {:receipt-id msg-id :delivered? true} :tags [:pipeline]
                :in-reply-to "ev-mode"})

      ;; Without conclusion: validate says incomplete
      (let [result (validate/validate-outcome receipt patterns store/!store)]
        (fix/assert-valid! shapes/CoordinationOutcome result)
        (is (false? (:outcome/valid? result))))

      ;; Conclude the coordination
      (append! {:evidence-id "ev-conclusion" :subject subj :type :coordination
                :claim-type :conclusion :author "claude-1"
                :body {:summary "task dispatched and confirmed"} :tags [:pipeline]
                :in-reply-to "ev-dispatch"})

      ;; Now validate says complete
      (let [result (validate/validate-outcome receipt patterns store/!store)]
        (fix/assert-valid! shapes/CoordinationOutcome result)
        (is (true? (:outcome/valid? result)))
        (is (= :coordination-complete (:outcome/type result)))

        ;; Evidence map includes entry count
        (is (= 5 (get-in result [:outcome/evidence :entry-count])))))))

;; =============================================================================
;; 9. Proof-tree invariants on integration scenario
;; =============================================================================

(deftest proof-tree-invariants-hold
  (testing "proof-tree invariants hold on a realistic integration thread"
    (let [subj (fix/make-artifact-ref :mission "M-invariants")
          t-base 1000]
      ;; Build a thread with monotonic timestamps
      (store/append* store/!store
                     (fix/make-evidence-entry
                      {:evidence/id "g" :evidence/subject subj
                       :evidence/type :coordination :evidence/claim-type :goal
                       :evidence/author "claude-1" :evidence/at (ts t-base)}))
      (store/append* store/!store
                     (fix/make-evidence-entry
                      {:evidence/id "s1" :evidence/subject subj
                       :evidence/type :coordination :evidence/claim-type :step
                       :evidence/author "claude-1" :evidence/at (ts (+ t-base 1000))
                       :evidence/in-reply-to "g"}))
      (store/append* store/!store
                     (fix/make-evidence-entry
                      {:evidence/id "s2" :evidence/subject subj
                       :evidence/type :coordination :evidence/claim-type :evidence
                       :evidence/author "codex-1" :evidence/at (ts (+ t-base 2000))
                       :evidence/in-reply-to "s1"}))
      (store/append* store/!store
                     (fix/make-evidence-entry
                      {:evidence/id "c" :evidence/subject subj
                       :evidence/type :coordination :evidence/claim-type :conclusion
                       :evidence/author "claude-1" :evidence/at (ts (+ t-base 3000))
                       :evidence/in-reply-to "s2"}))

      (let [tp (threads/project-thread store/!store subj)
            entries (:thread/entries tp)
            by-id (into {} (map (juxt :evidence/id identity) entries))]

        ;; Invariant 1: Tree validity — every in-reply-to references entry in projection
        (doseq [e entries]
          (when-let [parent (:evidence/in-reply-to e)]
            (is (contains? by-id parent)
                (str (:evidence/id e) " references " parent " which must exist"))))

        ;; Invariant 2: Root invariant — goal entry exists with :claim-type :goal
        (is (= :goal (:evidence/claim-type (:thread/goal tp))))

        ;; Invariant 3: No orphans — entry count matches
        (is (= 4 (count entries)))

        ;; Invariant 4: Claim ordering — conclusion replies to step or evidence, not conclusion
        (let [conclusion (get by-id "c")
              parent (get by-id (:evidence/in-reply-to conclusion))]
          (is (contains? #{:step :evidence} (:evidence/claim-type parent))))

        ;; Invariant 5: Author tracking
        (is (= #{"claude-1" "codex-1"} (:thread/participants tp)))

        ;; Invariant 6: Monotonic timestamps
        (let [instants (map #(Instant/parse (:evidence/at %)) entries)]
          (is (= instants (sort instants)) "timestamps are non-decreasing"))

        ;; Invariant 7: Entry count consistency
        (is (= (:thread/entry-count tp) (count entries)))))))
