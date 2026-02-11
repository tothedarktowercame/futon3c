(ns futon3c.peripheral.round-trip-test
  "M-peripheral-behavior Part IIa tests.

   Tests the ← verification framework: verify-constraints checks that
   peripheral evidence matches spec constraints; run-and-verify exercises
   the full round-trip with a test peripheral."
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.peripheral.evidence :as evidence]
            [futon3c.peripheral.round-trip :as rt]
            [futon3c.peripheral.runner :as runner]
            [futon3c.peripheral.tools :as tools]
            [futon3c.social.shapes :as shapes]
            [futon3c.social.test-fixtures :as fix]))

;; =============================================================================
;; Test peripheral specs
;; =============================================================================

(def explore-spec
  {:peripheral/id :explore
   :peripheral/tools #{:read :glob :grep :bash-readonly :web-fetch}
   :peripheral/scope :full-codebase
   :peripheral/entry #{:default :from-reflect}
   :peripheral/exit #{:found-target :ready-to-edit :user-request :hop-reflect}
   :peripheral/context {:session-id :inherit}})

(def edit-spec
  {:peripheral/id :edit
   :peripheral/tools #{:read :edit :write :bash}
   :peripheral/scope {:paths ["src/" "docs/" "scripts/"]}
   :peripheral/entry #{:from-explore :user-request}
   :peripheral/exit #{:tests-pass :ready-to-commit :blocked :hop-test :hop-reflect}
   :peripheral/context {:session-id :inherit :target-files :from-explore}})

(def reflect-spec
  {:peripheral/id :reflect
   :peripheral/tools #{:read :musn-log}
   :peripheral/scope :session-log-only
   :peripheral/entry #{:session-close :user-request :agent-request :from-any}
   :peripheral/exit #{:par-generated}
   :peripheral/context {:session-id :inherit :session-log :fetch-from-musn}})

;; =============================================================================
;; A minimal test peripheral that implements PeripheralRunner
;; =============================================================================

(defrecord TestPeripheral [spec backend]
  runner/PeripheralRunner
  (start [_ context]
    (let [sid (:session-id context)
          ev (evidence/make-start-evidence (:peripheral/id spec) sid "test-agent")]
      {:ok true
       :state {:session-id sid :author "test-agent" :last-evidence-id (:evidence/id ev) :steps []}
       :evidence ev}))

  (step [_ state action]
    (let [pid (:peripheral/id spec)
          dispatch-result (tools/dispatch-tool (:tool action) (:args action) spec backend)]
      (if (shapes/valid? shapes/SocialError dispatch-result)
        dispatch-result
        (let [ev (evidence/make-step-evidence
                  pid (:session-id state) (:author state)
                  (:tool action) (:args action) (:result dispatch-result)
                  (:last-evidence-id state))]
          {:ok true
           :state (-> state
                      (assoc :last-evidence-id (:evidence/id ev))
                      (update :steps conj action))
           :result (:result dispatch-result)
           :evidence ev}))))

  (stop [_ state reason]
    (let [pid (:peripheral/id spec)
          fruit {:actions (count (:steps state)) :reason reason}
          ev (evidence/make-stop-evidence
              pid (:session-id state) (:author state)
              fruit reason (:last-evidence-id state))]
      {:ok true
       :context {:session-id (:session-id state)}
       :fruit fruit
       :evidence ev})))

(defn make-test-peripheral
  "Create a test peripheral with a mock backend."
  ([spec]
   (make-test-peripheral spec {}))
  ([spec mock-results]
   (->TestPeripheral spec (tools/make-mock-backend mock-results))))

;; =============================================================================
;; verify-constraints — clean run
;; =============================================================================

(deftest clean-run-passes-verification
  (testing "evidence from a well-behaved explore session passes all constraints"
    (let [sid "sess-clean"
          start (evidence/make-start-evidence :explore sid "claude-1")
          step1 (evidence/make-step-evidence :explore sid "claude-1"
                                             :glob ["**/*.clj"] {:matches 5}
                                             (:evidence/id start))
          step2 (evidence/make-step-evidence :explore sid "claude-1"
                                             :read ["src/a.clj"] {:content "..."}
                                             (:evidence/id step1))
          stop (evidence/make-stop-evidence :explore sid "claude-1"
                                            {:found ["src/a.clj"]}
                                            "found target"
                                            (:evidence/id step2))
          result (rt/verify-constraints explore-spec [start step1 step2 stop])]
      (is (true? (:ok result))))))

;; =============================================================================
;; verify-constraints — tool violation
;; =============================================================================

(deftest tool-violation-detected
  (testing "evidence containing a disallowed tool produces a :tool-not-allowed violation"
    (let [sid "sess-bad-tool"
          start (evidence/make-start-evidence :explore sid "claude-1")
          ;; :edit is NOT in explore's tool set
          bad-step (evidence/make-step-evidence :explore sid "claude-1"
                                                :edit ["src/a.clj"] {:ok true}
                                                (:evidence/id start))
          stop (evidence/make-stop-evidence :explore sid "claude-1"
                                            {} "done" (:evidence/id bad-step))
          result (rt/verify-constraints explore-spec [start bad-step stop])]
      (is (false? (:ok result)))
      (is (= 1 (count (filter #(= :tool-not-allowed (:type %)) (:violations result)))))
      (let [v (first (:violations result))]
        (is (= :edit (:tool v)))
        (is (= (:evidence/id bad-step) (:evidence-id v)))))))

;; =============================================================================
;; verify-constraints — scope violation
;; =============================================================================

(deftest scope-violation-detected
  (testing "evidence containing out-of-scope args produces an :out-of-scope violation"
    (let [sid "sess-bad-scope"
          start (evidence/make-start-evidence :edit sid "claude-1")
          ;; test/ is NOT in edit's scope {:paths ["src/" "docs/" "scripts/"]}
          bad-step (evidence/make-step-evidence :edit sid "claude-1"
                                                :edit ["test/core_test.clj"] {:ok true}
                                                (:evidence/id start))
          stop (evidence/make-stop-evidence :edit sid "claude-1"
                                            {} "done" (:evidence/id bad-step))
          result (rt/verify-constraints edit-spec [start bad-step stop])]
      (is (false? (:ok result)))
      (is (= 1 (count (filter #(= :out-of-scope (:type %)) (:violations result)))))
      (let [v (first (filter #(= :out-of-scope (:type %)) (:violations result)))]
        (is (= :edit (:tool v)))
        (is (= ["test/core_test.clj"] (:args v)))))))

;; =============================================================================
;; verify-constraints — evidence type violation
;; =============================================================================

(deftest evidence-type-violation-detected
  (testing "evidence with wrong :evidence/type produces a :wrong-evidence-type violation"
    (let [sid "sess-bad-type"
          ;; Explore should have :coordination, but we'll give it :reflection
          start (assoc (evidence/make-start-evidence :explore sid "claude-1")
                       :evidence/type :reflection)
          stop (evidence/make-stop-evidence :explore sid "claude-1"
                                            {} "done" (:evidence/id start))
          result (rt/verify-constraints explore-spec [start stop])]
      (is (false? (:ok result)))
      (is (some #(= :wrong-evidence-type (:type %)) (:violations result))))))

;; =============================================================================
;; verify-constraints — structural violations
;; =============================================================================

(deftest missing-goal-detected
  (testing "evidence without :goal start produces a :missing-goal violation"
    (let [sid "sess-no-goal"
          ;; Start with a :step instead of :goal
          step (evidence/make-step-evidence :explore sid "claude-1"
                                            :read ["a.clj"] {} nil)
          stop (evidence/make-stop-evidence :explore sid "claude-1"
                                            {} "done" (:evidence/id step))
          result (rt/verify-constraints explore-spec [step stop])]
      (is (false? (:ok result)))
      (is (some #(= :missing-goal (:type %)) (:violations result))))))

(deftest missing-conclusion-detected
  (testing "evidence without :conclusion stop produces a :missing-conclusion violation"
    (let [sid "sess-no-stop"
          start (evidence/make-start-evidence :explore sid "claude-1")
          step (evidence/make-step-evidence :explore sid "claude-1"
                                            :read ["a.clj"] {} (:evidence/id start))
          result (rt/verify-constraints explore-spec [start step])]
      (is (false? (:ok result)))
      (is (some #(= :missing-conclusion (:type %)) (:violations result))))))

;; =============================================================================
;; verify-constraints — works with reflect (different evidence type)
;; =============================================================================

(deftest reflect-uses-reflection-type
  (testing "reflect peripheral requires :reflection type evidence"
    (let [sid "sess-reflect"
          start (evidence/make-start-evidence :reflect sid "claude-1")
          stop (evidence/make-stop-evidence :reflect sid "claude-1"
                                            {:par {:went-well "stuff"}}
                                            "session close"
                                            (:evidence/id start))
          result (rt/verify-constraints reflect-spec [start stop])]
      (is (true? (:ok result))))))

;; =============================================================================
;; run-and-verify — full ← round-trip
;; =============================================================================

(deftest run-and-verify-clean-explore
  (testing "run-and-verify: clean explore session passes ← verification"
    (let [peripheral (make-test-peripheral explore-spec {:glob {:matches 5}
                                                         :read {:content "..."}})
          context {:session-id "sess-rv-1"}
          actions [{:tool :glob :args ["**/*.clj"]}
                   {:tool :read :args ["src/a.clj"]}]
          result (rt/run-and-verify peripheral context actions "found target")]
      ;; run-and-verify returns ok + fruit + evidence
      (is (map? (:fruit result)))
      (is (= 4 (count (:evidence result))))  ;; start + 2 steps + stop
      ;; Evidence chain has correct claim types
      (is (= [:goal :step :step :conclusion]
             (mapv :evidence/claim-type (:evidence result)))))))

(deftest run-and-verify-step-failure-returns-error
  (testing "run-and-verify: disallowed tool produces SocialError at step"
    (let [peripheral (make-test-peripheral explore-spec)
          context {:session-id "sess-rv-fail"}
          ;; :edit is not in explore's tool set
          actions [{:tool :edit :args ["src/a.clj"]}]
          result (rt/run-and-verify peripheral context actions "done")]
      (is (false? (:ok result)))
      (is (shapes/valid? shapes/SocialError (:error result))))))

;; =============================================================================
;; Multiple violations accumulate
;; =============================================================================

(deftest multiple-violations-accumulated
  (testing "verify-constraints accumulates all violations, not just the first"
    (let [sid "sess-multi"
          start (evidence/make-start-evidence :explore sid "claude-1")
          ;; Violation 1: :edit not allowed in explore
          bad1 (evidence/make-step-evidence :explore sid "claude-1"
                                            :edit ["src/a.clj"] {} (:evidence/id start))
          ;; Violation 2: :write not allowed in explore
          bad2 (evidence/make-step-evidence :explore sid "claude-1"
                                            :write ["src/b.clj"] {} (:evidence/id bad1))
          stop (evidence/make-stop-evidence :explore sid "claude-1"
                                            {} "done" (:evidence/id bad2))
          result (rt/verify-constraints explore-spec [start bad1 bad2 stop])]
      (is (false? (:ok result)))
      (is (= 2 (count (filter #(= :tool-not-allowed (:type %)) (:violations result))))))))
