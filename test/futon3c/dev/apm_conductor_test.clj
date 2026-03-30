(ns futon3c.dev.apm-conductor-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.dev.apm-conductor :as conductor]))

(deftest parse-dependency-graph-extracts-concrete-entries
  (let [text "Stage 2 — LEMMA DEPENDENCY GRAPH\n--------------------------------\n\n1. **Completion lemma**\n   - **Formal dependency**: completion of Lebesgue measure by Borel sets up to null symmetric difference.\n   - **Informal dependency**: if the statement only asks for equality almost everywhere, reach for completion rather than explicit construction.\n   - **Why this becomes thinkable here**: the goal is to replace a measurable set/function by a Borel representative, which is exactly a completion move.\n   - **Lean target/type**: `∀ A, MeasurableSet A -> ∃ B, MeasurableSet[Borel] B ∧ μ (A ∆ B) = 0`.\n   - **Mathlib status/search terms**: likely in Mathlib; search `toMeasurable`, `ae_eq_set`, `completion`.\n   - **Critical path**: Yes, it unlocks the entire construction.\n\n2. **Simple approximation**\n   - **Formal dependency**: measurable functions admit simple-function approximations.\n   - **Informal dependency**: once the set case is handled, the function case usually reduces by step-function approximation.\n   - **Why this becomes thinkable here**: the target asks for a Borel measurable representative of a measurable function, so approximation is the standard bridge.\n   - **Lean target/type**: `Measurable f -> ∃ s_n, Tendsto s_n ...`.\n   - **Mathlib status/search terms**: custom wiring around simple functions; search `SimpleFunc`, `approx`, `ae_measurable`.\n   - **Critical path**: No, downstream of completion.\n"
        parsed (#'conductor/parse-dependency-graph text)]
    (is (= 2 (count parsed)))
    (is (= "Completion lemma" (:lemma (first parsed))))
    (is (= "completion of Lebesgue measure by Borel sets up to null symmetric difference."
           (:formal-dependency (first parsed))))
    (is (= "if the statement only asks for equality almost everywhere, reach for completion rather than explicit construction."
           (:informal-dependency (first parsed))))
    (is (= "the goal is to replace a measurable set/function by a Borel representative, which is exactly a completion move."
           (:why-this-now (first parsed))))
    (is (= "`∀ A, MeasurableSet A -> ∃ B, MeasurableSet[Borel] B ∧ μ (A ∆ B) = 0`."
           (:lean-type (first parsed))))
    (is (true? (:on-critical-path (first parsed))))
    (is (= "likely in Mathlib; search `toMeasurable`, `ae_eq_set`, `completion`."
           (:source (first parsed))))
    (is (= ["toMeasurable" "ae_eq_set" "completion"]
           (:search-terms (first parsed)))))) 

(deftest parse-arse-questions-extracts-question-answer-pairs
  (let [text "**ArSE Questions**\n1. *Why is this hard?* Because the completion machinery is easy to forget.\n2. *What is the key insight?* Use `toMeasurable`.\n3. *Why does step N work?* `ae_eq_set` converts a.e. equality to null symmetric difference.\n4. *What connects to this?* Completion arguments in probability and functional analysis.\n5. *Where is intuition wrong?* You do not need to build explicit approximations by hand.\n"
        parsed (#'conductor/parse-arse-questions text)]
    (is (= 5 (count parsed)))
    (is (= :why-hard (:type (first parsed))))
    (is (= "Why is this hard?" (:question (first parsed))))
    (is (= "Because the completion machinery is easy to forget." (:answer (first parsed))))))

(deftest parse-arse-questions-handles-lowercase-heading-and-qa-format
  (let [text "**Connections**\n- Context.\n\n**EXAM-DAY FIELD KIT**\n1. **Do not parse me**\n\n---\n\n**ArSE questions**\n\n1. *Why is this hard?* (OBSERVE)\n   **Q:** Why can’t we estimate the derivatives directly?\n   **A:** Because the pole term must be separated first.\n\n2. *What is the crux?* (PROPOSE)\n   **Q:** What idea unlocks the proof?\n   **A:** Subtract the principal part and use Cauchy estimates.\n"
        parsed (#'conductor/parse-arse-questions text)]
    (is (= 2 (count parsed)))
    (is (= :why-hard (:type (first parsed))))
    (is (= "Why can’t we estimate the derivatives directly?"
           (:question (first parsed))))
    (is (= "Because the pole term must be separated first."
           (:answer (first parsed))))
    (is (= :what-crux (:type (second parsed))))
    (is (= "What idea unlocks the proof?"
           (:question (second parsed))))))

(deftest fully-closed-execute-rejects-placeholder-lean-artifacts
  (let [tmp (doto (java.io.File/createTempFile "apm-placeholder" ".lean")
              (.deleteOnExit))
        _ (spit tmp "import Mathlib\n\n/-- Placeholder for a01J04. -/\nlemma residue_from_derivatives (f : ℂ → ℂ) (λ : ℂ) : True :=\n  trivial\n")]
    (with-redefs [conductor/discover-lean-artifacts (fn [_problem _execute-start-ms _output]
                                                       [(.getAbsolutePath tmp)])]
      (is (false? (#'conductor/fully-closed-execute? {:id "a01J04"} 0 "Lean artifact written and checked."))))))

(deftest fully-closed-execute-accepts-nontrivial-lean-artifacts
  (let [tmp (doto (java.io.File/createTempFile "apm-real" ".lean")
              (.deleteOnExit))
        _ (spit tmp "import Mathlib\n\ntheorem geometric_decay (T R : ℝ) (h : T < R) : T / R < 1 := by\n  nlinarith\n")]
    (with-redefs [conductor/discover-lean-artifacts (fn [_problem _execute-start-ms _output]
                                                       [(.getAbsolutePath tmp)])]
      (is (true? (#'conductor/fully-closed-execute? {:id "a01J04"} 0 "Lean artifact written and checked."))))))

(deftest execute-record-merges-lean-deltas-without-overwriting-stages
  (let [full "Stage 1 — THE CLEAN PROOF\n--------------------------------\n\nFull proof.\n\nStage 2 — LEMMA DEPENDENCY GRAPH\n--------------------------------\n\n1. **Main lemma**\n   - **Formal dependency**: theorem X.\n   - **Informal dependency**: recognize pattern Y.\n   - **Why this becomes thinkable here**: cue Z.\n   - **Lean target/type**: `Goal`.\n   - **Mathlib status/search terms**: search `foo`.\n   - **Critical path**: yes.\n\nStage 3 — LEAN FORMALIZATION\n--------------------------------\n\nInitial Lean skeleton.\n\nStage 4 — FORMAL-TO-INFORMAL REVISION\n--------------------------------\n\nReader-facing revision."
        delta "Closed helper lemma `foo`; one `sorry` remains in the Cauchy estimate."
        record (#'conductor/merge-execute-record nil full)
        merged (#'conductor/merge-execute-record record delta)
        notes (#'conductor/execute-record->notes merged)]
    (is (= "Full proof." (:stage1 merged)))
    (is (= "Reader-facing revision." (:stage4 merged)))
    (is (.contains notes "Initial Lean skeleton."))
    (is (.contains notes "Closed helper lemma `foo`; one `sorry` remains in the Cauchy estimate."))
    (is (.contains notes "recognize pattern Y."))))

(deftest execute-record-ignores-indirect-lean-delta
  (let [full "Stage 1 — THE CLEAN PROOF\n--------------------------------\n\nFull proof.\n\nStage 2 — LEMMA DEPENDENCY GRAPH\n--------------------------------\n\n1. **Main lemma**\n   - **Formal dependency**: theorem X.\n   - **Informal dependency**: recognize pattern Y.\n   - **Why this becomes thinkable here**: cue Z.\n   - **Lean target/type**: `Goal`.\n   - **Mathlib status/search terms**: search `foo`.\n   - **Critical path**: yes.\n\nStage 3 — LEAN FORMALIZATION\n--------------------------------\n\nInitial Lean skeleton.\n\nStage 4 — FORMAL-TO-INFORMAL REVISION\n--------------------------------\n\nReader-facing revision."
        indirect "Updated the Lean proof in /home/joe/code/apm-lean/lean-proofs/a01J04/Main.lean; see notes there."
        record (#'conductor/merge-execute-record nil full)
        merged (#'conductor/merge-execute-record record indirect)
        notes (#'conductor/execute-record->notes merged)]
    (is (= [] (:lean-deltas merged)))
    (is (.contains notes "Initial Lean skeleton."))
    (is (not (.contains notes "see notes there")))))

(deftest execute-record-does-not-enter-delta-mode-before-full-submission
  (let [delta "Closed helper lemma `foo`; one `sorry` remains in the Cauchy estimate."
        merged (#'conductor/merge-execute-record nil delta)]
    (is (false? (#'conductor/execute-record-complete? merged)))
    (is (= [] (:lean-deltas merged)))
    (is (nil? (#'conductor/execute-record->notes merged)))))

(deftest execute-record-becomes-complete-only-after-full-stage-submission
  (let [full "Stage 1 — THE CLEAN PROOF\n--------------------------------\n\nFull proof.\n\nStage 2 — LEMMA DEPENDENCY GRAPH\n--------------------------------\n\n1. **Main lemma**\n   - **Formal dependency**: theorem X.\n   - **Informal dependency**: recognize pattern Y.\n   - **Why this becomes thinkable here**: cue Z.\n   - **Lean target/type**: `Goal`.\n   - **Mathlib status/search terms**: search `foo`.\n   - **Critical path**: yes.\n\nStage 3 — LEAN FORMALIZATION\n--------------------------------\n\nInitial Lean skeleton.\n\nStage 4 — FORMAL-TO-INFORMAL REVISION\n--------------------------------\n\nReader-facing revision."
        record (#'conductor/merge-execute-record nil full)]
    (is (true? (#'conductor/execute-record-complete? record)))
    (is (.contains (#'conductor/execute-record->notes record) "Reader-facing revision."))))

(deftest execute-record-accepts-bold-stage-headings-without-underlines
  (let [full "**Stage 1 — THE CLEAN PROOF**\nProof text.\n\n**Stage 2 — LEMMA DEPENDENCY GRAPH**\n1. **Main lemma**\n   - **Formal dependency**: theorem X.\n   - **Informal dependency**: recognize pattern Y.\n   - **Why this becomes thinkable here**: cue Z.\n   - **Lean target/type**: `Goal`.\n   - **Mathlib status/search terms**: search `foo`.\n   - **Critical path**: yes.\n\n**Stage 3 — LEAN FORMALIZATION**\nLean text.\n\n**Stage 4 — FORMAL-TO-INFORMAL REVISION**\nRevision text."
        record (#'conductor/merge-execute-record nil full)]
    (is (true? (#'conductor/execute-record-complete? record)))
    (is (= "Proof text." (:stage1 record)))
    (is (= "Lean text." (:stage3 record)))
    (is (= "Revision text." (:stage4 record)))))
