(ns futon3c.dev.apm-conductor-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.agency.registry :as reg]
            [futon3c.agents.apm-work-queue :as apm-queue]
            [futon3c.dev.apm-conductor :as conductor]
            [futon3c.peripheral.proof-backend :as pb]
            [futon3c.peripheral.tools :as tools]))

(deftype ScriptBackend [cache phase-idx phase-history]
  tools/ToolBackend
  (execute-tool [_ tool-id args]
    (case tool-id
      :proof-load
      (let [pid (first args)]
        (swap! cache assoc pid {:proof/problem-id pid :proof/cycles []})
        {:ok true :result (get @cache pid)})

      :cycle-begin
      {:ok true :result {:cycle/id "C1"}}

      :cycle-advance
      (let [[pid _cycle-id phase-data] args
            phases [:observe :propose :execute :validate :classify :integrate]]
        (if (< @phase-idx (count phases))
          (let [phase (nth phases @phase-idx)
                validation-error (apm-queue/apm-phase-validator phase phase-data {})]
            (if validation-error
              validation-error
              (do
                (swap! phase-history conj {:phase phase :phase-data phase-data})
                (swap! cache assoc pid {:proof/problem-id pid
                                        :proof/cycles @phase-history})
                (swap! phase-idx inc)
                {:ok true :result {:accepted phase}})))
          {:ok true :result {:accepted :mechanical}}))

      {:ok true :result nil})))

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

(deftest scripted-conductor-replay-completes-one-problem
  (let [tmp-dir (.toFile (java.nio.file.Files/createTempDirectory "apm-replay" (make-array java.nio.file.attribute.FileAttribute 0)))
        proof-root (.getAbsolutePath (io/file tmp-dir "proof-state"))
        artifact-dir (io/file tmp-dir "lean-proofs" "replay01")
        _ (.mkdirs artifact-dir)
        artifact (io/file artifact-dir "Main.lean")
        _ (spit artifact "import Mathlib\n\ntheorem replay_nontrivial (x : ℝ) : x = x := by rfl\n")
        idle-callback (atom nil)
        prompts (atom [])
        logs (atom [])
        phase-idx (atom 0)
        phase-history (atom [])
        backend (ScriptBackend. (atom {}) phase-idx phase-history)
        observe "WHAT IS REALLY BEING ASKED\nShow the zero-counting argument converges.\n\nWHY IT IS HARD\nOne must turn growth into summability."
        propose "THE KEY INSIGHT\nUse Jensen-style counting bounds.\n\nTHE NAIVE APPROACH THAT FAILS\nA termwise estimate on zeros gives no global control."
        execute "**Stage 1 — THE CLEAN PROOF**\nUse the growth bound to control the zero counting function, then compare the weighted series with a convergent integral.\n\n**Stage 2 — LEMMA DEPENDENCY GRAPH**\n1. **Counting bound**\n   - **Formal dependency**: Jensen-type control turns exponential growth into linear control on the zero counting function.\n   - **Informal dependency**: when an entire function has exponential type, the right hidden move is to estimate how many zeros lie in each disk.\n   - **Why this becomes thinkable here**: the target is a summability statement over zeros, so the natural bridge is a counting function rather than a direct termwise estimate.\n   - **Lean target/type**: `∃ C ≥ 0, ∀ r ≥ 0, (N r : ℝ) ≤ C * (1 + r)`.\n   - **Mathlib status/search terms**: custom; search `Summable`, `Real.rpow`, `Nat.cast`.\n   - **Critical path**: yes.\n\n2. **Integral comparison**\n   - **Formal dependency**: linear counting growth with `α > 1` implies convergence of the weighted Dirichlet series.\n   - **Informal dependency**: once the counting function is tame, dyadic or integral comparison is the standard way to prove summability.\n   - **Why this becomes thinkable here**: the exponent condition `α > 1` is exactly the threshold where a one-dimensional tail integral converges.\n   - **Lean target/type**: `Summable (fun n => Real.rpow (1 + ‖zeros n‖) (-α))`.\n   - **Mathlib status/search terms**: partially in Mathlib; search `summable_nat_add_iff`, `Real.summable_nat_rpow`.\n   - **Critical path**: yes.\n\n**Stage 3 — LEAN FORMALIZATION**\nBuilt the typed scaffold and closed the final theorem in the current artifact.\n\n**Stage 4 — FORMAL-TO-INFORMAL REVISION**\nThe hard step is not the last comparison but knowing to pass through the zero-counting function first."
        validate "Non-circularity analysis: the zero-counting estimate is independent of the desired summability conclusion.\n\nLean status: built successfully, zero sorry."
        classify "**Classification**: proved.\n\n**CONFIDENCE INVERSION**: The surprise is that the proof is really about counting zeros in disks, not estimating each zero separately."
        integrate "**Connections**\nThis is a prototype for Jensen, Cartwright, and density arguments in complex analysis.\n\n**EXAM-DAY FIELD KIT**\n1. Summability over zeros is usually a counting-function problem.\n2. Exponential type suggests Jensen-style disk estimates.\n3. `α > 1` is the one-dimensional convergence threshold.\n\n**ArSE Questions**\n1. *Why is this hard?*\n   **Q:** Why can’t we just bound each zero directly?\n   **A:** Because the hypothesis controls global growth, not individual zeros.\n2. *What is the key insight?*\n   **Q:** What single move unlocks the proof?\n   **A:** Replace the series by a counting-function estimate.\n3. *Why does step N work?*\n   **Q:** Why does linear zero growth imply convergence here?\n   **A:** Because the tail compares to an integral with exponent greater than one.\n4. *What connects to this?*\n   **Q:** Where else does this method appear?\n   **A:** In Jensen formula, entire-function theory, and zero-density arguments.\n5. *Where is intuition wrong?*\n   **Q:** What false instinct should we avoid?\n   **A:** Thinking the zeros must be controlled one-by-one rather than statistically."
        manifest [{:id "replay01" :subject "Analysis"}]]
    (with-redefs [conductor/log! (fn [entry] (swap! logs conj entry))
                  conductor/proof-state-root proof-root
                  conductor/discover-lean-artifacts (fn [_ _ _] [(.getAbsolutePath artifact)])
                  reg/set-on-idle! (fn [f] (reset! idle-callback f))
                  reg/invoke-agent! (fn [_agent-id prompt _timeout-ms]
                                      (swap! prompts conj prompt)
                                      {:ok true :result ""})
                  apm-queue/load-apm-manifest (fn [] manifest)
                  apm-queue/load-problem-tex (fn [_] "Problem body")
                  apm-queue/emit-apm-evidence! (fn [& _] nil)
                  pb/make-proof-backend (fn [_config] backend)]
      (conductor/start-apm-conductor! nil :agent-id "codex-1" :problem-ids ["replay01"])
      (doseq [payload [observe propose execute validate classify integrate]]
        (@idle-callback "codex-1" {:ok true :result payload}))
      (testing "the scripted run reaches batch completion"
        (is (nil? @conductor/!apm-conductor))
        (is (= 1 (:problems-done @conductor/!apm-state)))
        (is (= :integrate (-> @phase-history last :phase)))
        (is (= 5 (count (get-in (-> @phase-history last :phase-data) [:arse-questions]))))
        (is (= 2 (count (get-in (nth @phase-history 2) [:phase-data :dependency-graph]))))
        (is (some #(= :problem-complete (:event %)) @logs))
        (is (some #(= :batch-complete (:event %)) @logs))))))
