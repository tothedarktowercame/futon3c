(ns futon3c.dev.apm-conductor-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [futon3c.agency.registry :as reg]
            [futon3c.agents.apm-work-queue :as apm-queue]
            [futon3c.dev.apm-conductor :as conductor]
            [futon3c.dev.apm-frames :as frames]
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
            phases [:observe :propose :target-check :execute :validate :classify :integrate]]
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

(def ^:private valid-target-check-output
  "**TARGET SANITY CHECK**\n- mentions-problem-objects?: yes\n- avoids-assuming-conclusion?: yes\n- meaningful-without-prose?: yes\n- notes: The target theorem still states the actual claim.\n\n**PROOF-PLAN.EDN**\n```edn\n{:goal \"Prove the claim.\"\n :terms [{:name \"x\" :meaning \"the main object\" :needed-because \"it appears in the statement\"}]\n :strategy [{:id :main-step\n             :formal-dependency \"main reduction lemma\"\n             :informal-dependency \"reduce to the canonical estimate\"\n             :why-this-now \"the target is a direct convergence statement\"\n             :lean-target \"theorem main_target : True\"\n             :mathlib-status \"custom\"\n             :critical-path true}]\n :stage-status {:stage1 :done :stage2 :done :stage3 :pending :stage4 :pending}}\n```\n\n**FORMAL-ALIGNMENT.EDN**\n```edn\n{:main-claim {:informal-claim \"Prove the claim.\"\n              :formal-name \"main_target\"\n              :formal-target \"theorem main_target : True\"\n              :sanity-check {:mentions-problem-objects? true\n                             :avoids-assuming-conclusion? true\n                             :meaningful-without-prose? true\n                             :notes \"The target theorem still states the claim being pursued.\"}}\n :alignments [{:formal-name \"main_target\"\n               :formal-statement \"theorem main_target : True\"\n               :informal-clause \"Main claim\"\n               :role :main-theorem}]}\n```")

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

(deftest parse-dependency-graph-tolerates-label-variants
  (testing "parses 'Why thinkable' and 'Lean target' (a02J02 format)"
    (let [text "1. **Kadec-Klee property**\n   - **Formal dependency**: uniform convexity of L^p implies Kadec-Klee.\n   - **Informal dependency**: norm convergence + weak convergence forces strong.\n   - **Why thinkable**: the hypothesis pairs a.e. convergence with norm convergence.\n   - **Lean target**: `UniformConvex (Lp E p μ)`.\n   - **Mathlib status**: likely; search `UniformConvex`, `Lp`.\n   - **Critical path**: yes.\n"
          parsed (#'conductor/parse-dependency-graph text)]
      (is (= 1 (count parsed)))
      (is (= "Kadec-Klee property" (:lemma (first parsed))))
      (is (= "uniform convexity of L^p implies Kadec-Klee." (:formal-dependency (first parsed))))
      (is (= "norm convergence + weak convergence forces strong." (:informal-dependency (first parsed))))
      (is (= "the hypothesis pairs a.e. convergence with norm convergence." (:why-this-now (first parsed))))
      (is (= "`UniformConvex (Lp E p μ)`." (:lean-type (first parsed))))
      (is (true? (:on-critical-path (first parsed))))))

  (testing "parses when 'Why now' field is omitted entirely (a02J05 format)"
    (let [text "1. **Contour setup**\n   - **Formal dependency**: standard contour for sin(x)/x.\n   - **Informal dependency**: recognize the half-plane trick.\n   - **Lean target/type**: `ContourIntegral`.\n   - **Critical path**: yes.\n"
          parsed (#'conductor/parse-dependency-graph text)
          entry (first parsed)]
      (is (= 1 (count parsed)))
      (is (= "Contour setup" (:lemma entry)))
      (is (some? (:formal-dependency entry)))
      (is (some? (:informal-dependency entry)))
      (is (nil? (:why-this-now entry)) "why-this-now nil when field omitted is acceptable")))

  (testing "parses 'Why relevant here' variant"
    (let [text "1. **Tonelli swap**\n   - **Formal dependency**: Tonelli theorem for nonneg integrands.\n   - **Informal dependency**: switching integration order is the standard move.\n   - **Why relevant here**: integrand is nonneg by construction.\n   - **Lean target**: `integral_integral_swap`.\n   - **Critical path**: no.\n"
          parsed (#'conductor/parse-dependency-graph text)
          entry (first parsed)]
      (is (= "Tonelli swap" (:lemma entry)))
      (is (= "integrand is nonneg by construction." (:why-this-now entry)))
      (is (= "`integral_integral_swap`." (:lean-type entry))))))

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

(deftest indirect-notes-does-not-false-positive-on-math-prose
  (testing "mathematical prose with 'documented' far from file paths is NOT indirect"
    (is (false? (#'conductor/indirect-notes?
                 "We documented that the bound holds via the dominated convergence theorem, then integrated into the overall proof structure. The Lean formalization follows from the dependency graph above.")))
    (is (false? (#'conductor/indirect-notes?
                 "The result was captured by the following inequality: ||Tf|| <= ||K||_1 ||f||_2. This is well-documented in the literature on Lp spaces and gives the contraction bound. See the clean proof in Stage 1 for details."))))

  (testing "actual indirection IS detected"
    (is (true? (#'conductor/indirect-notes? "See notes in the sidecar file.")))
    (is (true? (#'conductor/indirect-notes? "Documented in proof_peripheral/apm-a02J03-execute.md")))
    (is (true? (#'conductor/indirect-notes? "Recorded in lean-proofs/a02J03/Main.lean")))
    (is (true? (#'conductor/indirect-notes? "Written into apm-a02J03-execute.md for later use."))))

  (testing "short pointer sentence with nearby file path IS detected"
    (is (true? (#'conductor/indirect-notes? "Saved into the file proof_peripheral/notes.md.")))
    (is (true? (#'conductor/indirect-notes? "See file for full details.")))))

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
        frame-workspace {:frame/id "frame-replay01"
                         :frame/workspace-root (str tmp-dir "/frame")
                         :frame/module-root "ApmCanaries.Frames.Replay01.Run1"
                         :frame/lean-root (str tmp-dir "/apm-lean/ApmCanaries/Frames/Replay01/Run1")
                         :frame/shared-extension-root (str tmp-dir "/apm-lean/ApmCanaries/Local")
                         :artifacts {:proof-plan (str tmp-dir "/frame/proof-plan.edn")
                                     :formal-alignment (str tmp-dir "/frame/formal-alignment.edn")
                                     :changelog (str tmp-dir "/frame/changelog.edn")
                                     :execute-notes (str tmp-dir "/frame/execute.md")
                                     :workspace-metadata (str tmp-dir "/frame/workspace.json")
                                     :lean-main (.getAbsolutePath artifact)
                                     :lean-scratch (str tmp-dir "/apm-lean/ApmCanaries/Frames/Replay01/Run1/Scratch.lean")}}
        observe "WHAT IS REALLY BEING ASKED\nShow the zero-counting argument converges.\n\nWHY IT IS HARD\nOne must turn growth into summability."
        propose "THE KEY INSIGHT\nUse Jensen-style counting bounds.\n\nTHE NAIVE APPROACH THAT FAILS\nA termwise estimate on zeros gives no global control."
        target-check "**TARGET SANITY CHECK**\n- mentions-problem-objects?: yes\n- avoids-assuming-conclusion?: yes\n- meaningful-without-prose?: yes\n- notes: The target theorem still states the weighted zero-series convergence claim.\n\n**PROOF-PLAN.EDN**\n```edn\n{:goal \"Show the weighted zero series converges.\"\n :terms [{:name \"zero counting function\" :meaning \"counts zeros in disks\" :needed-because \"turns global growth into a summability estimate\"}]\n :strategy [{:id :counting-bound :formal-dependency \"Jensen-type counting bound\" :informal-dependency \"replace termwise zero estimates by a counting function\" :why-this-now \"the target is a series over zeros, so counting is the natural bridge\" :lean-target \"∃ C ≥ 0, ∀ r ≥ 0, (N r : ℝ) ≤ C * (1 + r)\" :mathlib-status \"custom; search `Summable`, `Real.rpow`, `Nat.cast`\" :critical-path true}\n            {:id :integral-comparison :formal-dependency \"integral comparison for p-series tails\" :informal-dependency \"once counting growth is linear, compare to a convergent integral\" :why-this-now \"the hypothesis gives α > 1, exactly the one-dimensional convergence threshold\" :lean-target \"Summable (fun n => Real.rpow (1 + ‖zeros n‖) (-α))\" :mathlib-status \"partially in Mathlib; search `summable_nat_add_iff`, `Real.summable_nat_rpow`\" :critical-path true}]\n :stage-status {:stage1 :done :stage2 :done :stage3 :pending :stage4 :pending}}\n```\n\n**FORMAL-ALIGNMENT.EDN**\n```edn\n{:main-claim {:informal-claim \"Show the weighted zero series converges.\"\n              :formal-name \"weighted_zero_series_summable\"\n              :formal-target \"theorem weighted_zero_series_summable : Summable (fun n => Real.rpow (1 + ‖zeros n‖) (-α))\"\n              :sanity-check {:mentions-problem-objects? true\n                             :avoids-assuming-conclusion? true\n                             :meaningful-without-prose? true\n                             :notes \"The target theorem states the actual convergence claim.\"}}\n :alignments [{:formal-name \"weighted_zero_series_summable\"\n               :formal-statement \"theorem weighted_zero_series_summable : Summable (fun n => Real.rpow (1 + ‖zeros n‖) (-α))\"\n               :informal-clause \"Stage 1: the weighted zero series converges.\"\n               :role :main-theorem}]}\n```"
        execute "**Stage 1 — THE CLEAN PROOF**\nUse the growth bound to control the zero counting function, then compare the weighted series with a convergent integral.\n\n**Stage 2 — LEMMA DEPENDENCY GRAPH**\n1. **Counting bound**\n   - **Formal dependency**: Jensen-type control turns exponential growth into linear control on the zero counting function.\n   - **Informal dependency**: when an entire function has exponential type, the right hidden move is to estimate how many zeros lie in each disk.\n   - **Why this becomes thinkable here**: the target is a summability statement over zeros, so the natural bridge is a counting function rather than a direct termwise estimate.\n   - **Lean target/type**: `∃ C ≥ 0, ∀ r ≥ 0, (N r : ℝ) ≤ C * (1 + r)`.\n   - **Mathlib status/search terms**: custom; search `Summable`, `Real.rpow`, `Nat.cast`.\n   - **Critical path**: yes.\n\n2. **Integral comparison**\n   - **Formal dependency**: linear counting growth with `α > 1` implies convergence of the weighted Dirichlet series.\n   - **Informal dependency**: once the counting function is tame, dyadic or integral comparison is the standard way to prove summability.\n   - **Why this becomes thinkable here**: the exponent condition `α > 1` is exactly the threshold where a one-dimensional tail integral converges.\n   - **Lean target/type**: `Summable (fun n => Real.rpow (1 + ‖zeros n‖) (-α))`.\n   - **Mathlib status/search terms**: partially in Mathlib; search `summable_nat_add_iff`, `Real.summable_nat_rpow`.\n   - **Critical path**: yes.\n\n**Stage 3 — LEAN FORMALIZATION**\nBuilt the typed scaffold and closed the final theorem in the current artifact.\n\n**Stage 4 — FORMAL-TO-INFORMAL REVISION**\nThe hard step is not the last comparison but knowing to pass through the zero-counting function first."
        validate "Non-circularity analysis: the zero-counting estimate is independent of the desired summability conclusion.\n\nLean status: built successfully, zero sorry."
        classify "**Classification**: proved.\n\n**CONFIDENCE INVERSION**: The surprise is that the proof is really about counting zeros in disks, not estimating each zero separately."
        integrate "**Connections**\nThis is a prototype for Jensen, Cartwright, and density arguments in complex analysis.\n\n**EXAM-DAY FIELD KIT**\n1. Summability over zeros is usually a counting-function problem.\n2. Exponential type suggests Jensen-style disk estimates.\n3. `α > 1` is the one-dimensional convergence threshold.\n\n**ArSE Questions**\n1. *Why is this hard?*\n   **Q:** Why can’t we just bound each zero directly?\n   **A:** Because the hypothesis controls global growth, not individual zeros.\n2. *What is the key insight?*\n   **Q:** What single move unlocks the proof?\n   **A:** Replace the series by a counting-function estimate.\n3. *Why does step N work?*\n   **Q:** Why does linear zero growth imply convergence here?\n   **A:** Because the tail compares to an integral with exponent greater than one.\n4. *What connects to this?*\n   **Q:** Where else does this method appear?\n   **A:** In Jensen formula, entire-function theory, and zero-density arguments.\n5. *Where is intuition wrong?*\n   **Q:** What false instinct should we avoid?\n   **A:** Thinking the zeros must be controlled one-by-one rather than statistically."
        manifest [{:id "replay01" :subject "Analysis"}]
        _ (.mkdirs (io/file (:frame/workspace-root frame-workspace)))]
    (with-redefs [conductor/log! (fn [entry] (swap! logs conj entry))
                  conductor/proof-state-root proof-root
                  conductor/discover-lean-artifacts (fn [_ _ _] [(.getAbsolutePath artifact)])
                  frames/init-frame-workspace! (fn [_] frame-workspace)
                  frames/emit-frame-receipt! (fn [& _] (str tmp-dir "/frame.json"))
                  reg/set-on-idle! (fn [f] (reset! idle-callback f))
                  reg/invoke-agent! (fn [_agent-id prompt _timeout-ms]
                                      (swap! prompts conj prompt)
                                      {:ok true :result ""})
                  apm-queue/load-apm-manifest (fn [] manifest)
                  apm-queue/load-problem-tex (fn [_] "Problem body")
                  apm-queue/emit-apm-evidence! (fn [& _] nil)
                  pb/make-proof-backend (fn [_config] backend)]
      (conductor/start-apm-conductor! nil :agent-id "codex-1" :problem-ids ["replay01"])
      (doseq [payload [observe propose target-check execute validate classify integrate]]
        (@idle-callback "codex-1" {:ok true :result payload}))
      (testing "the scripted run reaches batch completion"
        (is (nil? @conductor/!apm-conductor))
        (is (= 1 (:problems-done @conductor/!apm-state)))
        (is (= :integrate (-> @phase-history last :phase)))
        (is (= 5 (count (get-in (-> @phase-history last :phase-data) [:arse-questions]))))
        (is (map? (get-in (nth @phase-history 2) [:phase-data :proof-plan])))
        (is (map? (get-in (nth @phase-history 2) [:phase-data :formal-alignment])))
        (is (= 2 (count (get-in (nth @phase-history 3) [:phase-data :dependency-graph]))))
        (is (seq (get-in (nth @phase-history 3) [:phase-data :changelog])))
        (is (some #(= :problem-complete (:event %)) @logs))
        (is (some #(= :batch-complete (:event %)) @logs))))))

(deftest preflight-succeeds
  (is (:ok (conductor/run-apm-preflight!))))

(deftest execute-prompt-prefers-frame-local-paths
  (let [frame-workspace {:frame/workspace-root "/tmp/frame"
                         :frame/module-root "ApmCanaries.Frames.Replay01.Run1"
                         :frame/lean-root "/home/joe/code/apm-lean/ApmCanaries/Frames/Replay01/Run1"
                         :frame/shared-extension-root "/home/joe/code/apm-lean/ApmCanaries/Local"
                         :artifacts {:proof-plan "/tmp/frame/proof-plan.edn"
                                     :formal-alignment "/tmp/frame/formal-alignment.edn"
                                     :changelog "/tmp/frame/changelog.edn"
                                     :execute-notes "/tmp/frame/execute.md"
                                     :workspace-metadata "/tmp/frame/workspace.json"
                                     :lean-main "/home/joe/code/apm-lean/ApmCanaries/Frames/Replay01/Run1/Main.lean"
                                     :lean-scratch "/home/joe/code/apm-lean/ApmCanaries/Frames/Replay01/Run1/Scratch.lean"}}]
    (reset! conductor/!apm-state {:current-problem {:id "replay01"}
                                  :frame-workspace frame-workspace})
    (let [prompt (#'conductor/make-phase-prompt
                  {:id "replay01" :subject :analysis}
                  "Problem body"
                  :execute
                  {:observe "Observe"
                   :propose "Propose"})]
      (is (str/includes? prompt "/home/joe/code/apm-lean/ApmCanaries/Frames/Replay01/Run1/Main.lean"))
      (is (str/includes? prompt "/tmp/frame/proof-plan.edn"))
      (is (str/includes? prompt "Do not use shared scratch paths such as `ApmCanaries/Current.lean`")))))

;; =============================================================================
;; Test scaffold for circuit-breaker tests
;; =============================================================================

(defn- make-test-scaffold
  "Build reusable test scaffold with tmp dirs, atoms, and redefs bindings.
   Returns a map of :idle-callback, :prompts, :logs, :backend, :manifest,
   :artifact, :proof-root, and :redef-bindings (for with-redefs)."
  [& {:keys [problem-id manifest-entries artifact-content]
      :or {problem-id "cb01"
           manifest-entries [{:id "cb01" :subject "Analysis"}]
           artifact-content "import Mathlib\n\ntheorem cb_nontrivial (x : ℝ) : x = x := by rfl\n"}}]
  (let [tmp-dir (.toFile (java.nio.file.Files/createTempDirectory
                          "apm-cb" (make-array java.nio.file.attribute.FileAttribute 0)))
        proof-root (.getAbsolutePath (io/file tmp-dir "proof-state"))
        artifact-dir (io/file tmp-dir "lean-proofs" problem-id)
        _ (.mkdirs artifact-dir)
        artifact (io/file artifact-dir "Main.lean")
        _ (spit artifact artifact-content)
        idle-callback (atom nil)
        prompts (atom [])
        logs (atom [])
        phase-idx (atom 0)
        phase-history (atom [])
        backend (ScriptBackend. (atom {}) phase-idx phase-history)
        frame-workspace {:frame/id (str "frame-" problem-id)
                         :frame/workspace-root (str tmp-dir "/frame")
                         :frame/module-root (str "ApmCanaries.Frames." problem-id ".Run1")
                         :frame/lean-root (str tmp-dir "/apm-lean/ApmCanaries/Frames/" problem-id "/Run1")
                         :frame/shared-extension-root (str tmp-dir "/apm-lean/ApmCanaries/Local")
                         :artifacts {:proof-plan (str tmp-dir "/frame/proof-plan.edn")
                                     :formal-alignment (str tmp-dir "/frame/formal-alignment.edn")
                                     :changelog (str tmp-dir "/frame/changelog.edn")
                                     :execute-notes (str tmp-dir "/frame/execute.md")
                                     :workspace-metadata (str tmp-dir "/frame/workspace.json")
                                     :lean-main (.getAbsolutePath artifact)
                                     :lean-scratch (str tmp-dir "/apm-lean/ApmCanaries/Frames/" problem-id "/Run1/Scratch.lean")}}]
    (.mkdirs (io/file (:frame/workspace-root frame-workspace)))
    {:idle-callback idle-callback
     :prompts prompts
     :logs logs
     :backend backend
     :phase-history phase-history
     :artifact artifact
     :frame-workspace frame-workspace
     :proof-root proof-root
     :redef-bindings
     {#'conductor/log! (fn [entry] (swap! logs conj entry))
      #'conductor/proof-state-root proof-root
      #'conductor/discover-lean-artifacts (fn [_ _ _] [(.getAbsolutePath artifact)])
      #'frames/init-frame-workspace! (fn [_] frame-workspace)
      #'frames/emit-frame-receipt! (fn [& _] (str tmp-dir "/frame.json"))
      #'reg/set-on-idle! (fn [f] (reset! idle-callback f))
      #'reg/invoke-agent! (fn [_agent-id prompt _timeout-ms]
                             (swap! prompts conj prompt)
                             {:ok true :result ""})
      #'apm-queue/load-apm-manifest (fn [] manifest-entries)
      #'apm-queue/load-problem-tex (fn [_] "Problem body")
      #'apm-queue/emit-apm-evidence! (fn [& _] nil)
      #'pb/make-proof-backend (fn [_config] backend)}}))

;; =============================================================================
;; Circuit-breaker tests
;; =============================================================================

(deftest execute-redispatch-cap-forces-advance
  (testing "after max-execute-redispatches, conductor force-advances past execute"
    (let [{:keys [idle-callback logs redef-bindings]}
          (make-test-scaffold)]
      ;; No artifacts → fully-closed? is false, so the missing-full-record branch fires
      (with-redefs-fn (assoc redef-bindings
                             #'conductor/discover-lean-artifacts (fn [_ _ _] []))
        (fn []
          (conductor/start-apm-conductor! nil :agent-id "codex-1" :problem-ids ["cb01"])

          ;; Feed observe and propose normally
          (@idle-callback "codex-1"
           {:ok true :result "WHAT IS REALLY BEING ASKED\nTest.\n\nWHY IT IS HARD\nTest."})
          (@idle-callback "codex-1"
           {:ok true :result "THE KEY INSIGHT\nTest.\n\nTHE NAIVE APPROACH THAT FAILS\nTest."})
          (@idle-callback "codex-1"
           {:ok true :result valid-target-check-output})

          ;; Now in execute: return incomplete text (missing Stage 1-4 headings) repeatedly.
          ;; Each return should trigger a re-dispatch. After max-execute-redispatches,
          ;; the conductor should force-advance (and the validator will then reject,
          ;; but the cap event proves the loop terminated).
          (let [cap @#'conductor/max-execute-redispatches]
            (dotimes [_ (+ cap 2)]
              (when @conductor/!apm-conductor
                (@idle-callback "codex-1"
                 {:ok true :result "I wrote some notes about the proof but not in Stage 1-4 format."})))

            (testing "redispatch cap event was logged"
              (is (some #(= :execute-redispatch-cap (:event %)) @logs)))

            (testing "re-dispatch count reached the cap"
              (let [cap-entry (first (filter #(= :execute-redispatch-cap (:event %)) @logs))]
                (is (= cap (:redispatch-count cap-entry)))))))))))

(deftest timer-expiry-takes-priority-over-missing-full-record
  (testing "when lean timer expires, conductor advances even without full Stage 1-4 record"
    (let [{:keys [idle-callback logs redef-bindings]}
          (make-test-scaffold)]
      ;; No artifacts → fully-closed? is false
      (with-redefs-fn (assoc redef-bindings
                             #'conductor/discover-lean-artifacts (fn [_ _ _] []))
        (fn []
          (conductor/start-apm-conductor! nil :agent-id "codex-1" :problem-ids ["cb01"])

          ;; Feed observe and propose normally
          (@idle-callback "codex-1"
           {:ok true :result "WHAT IS REALLY BEING ASKED\nTest.\n\nWHY IT IS HARD\nTest."})
          (@idle-callback "codex-1"
           {:ok true :result "THE KEY INSIGHT\nTest.\n\nTHE NAIVE APPROACH THAT FAILS\nTest."})
          (@idle-callback "codex-1"
           {:ok true :result valid-target-check-output})

          ;; Backdate execute-start-ms to simulate timer expiry
          (swap! conductor/!apm-state assoc
                 :execute-start-ms (- (System/currentTimeMillis)
                                      @#'conductor/lean-floor-ms
                                      1000))

          ;; Return incomplete text — timer should override the missing-record check
          (@idle-callback "codex-1"
           {:ok true :result "Some partial notes, not full Stage 1-4 format."})

          (testing "timer-expired event was logged (not missing-full-record)"
            (is (some #(= :execute-timer-expired (:event %)) @logs))
            (is (not (some #(= :execute-missing-full-record (:event %)) @logs)))))))))

(deftest problem-timeout-skips-to-next-problem
  (testing "when overall problem timeout is exceeded, conductor skips to next problem"
    (let [{:keys [idle-callback logs redef-bindings]}
          (make-test-scaffold :manifest-entries [{:id "cb01" :subject "Analysis"}
                                                 {:id "cb02" :subject "Analysis"}])]
      (with-redefs-fn redef-bindings
        (fn []
          (conductor/start-apm-conductor! nil :agent-id "codex-1"
                                          :problem-ids ["cb01" "cb02"])

          ;; Feed observe normally
          (@idle-callback "codex-1"
           {:ok true :result "WHAT IS REALLY BEING ASKED\nTest.\n\nWHY IT IS HARD\nTest."})

          ;; Backdate problem-start-ms to simulate timeout
          (swap! conductor/!apm-state assoc
                 :problem-start-ms (- (System/currentTimeMillis)
                                      @#'conductor/problem-timeout-ms
                                      1000))

          ;; Next idle callback should trigger timeout and skip to cb02
          (@idle-callback "codex-1"
           {:ok true :result "THE KEY INSIGHT\nTest.\n\nTHE NAIVE APPROACH THAT FAILS\nTest."})

          (testing "problem-timed-out event was logged"
            (is (some #(= :problem-timed-out (:event %)) @logs)))

          (testing "conductor moved to cb02"
            (is (= "cb02" (get-in @conductor/!apm-state [:current-problem :id]))))

          (testing "problems-done was incremented"
            (is (= 1 (:problems-done @conductor/!apm-state))))

          (testing "batch result records the timeout"
            (is (= :timed-out (-> @conductor/!apm-state :batch-results first :classification)))))))))

(deftest retry-budget-abandons-problem-and-continues-to-next
  (testing "non-execute retry budget exhaustion writes an abandoned result and starts the next problem"
    (let [{:keys [idle-callback logs redef-bindings proof-root]}
          (make-test-scaffold :manifest-entries [{:id "cb01" :subject "Analysis"}
                                                 {:id "cb02" :subject "Analysis"}])]
      (with-redefs-fn redef-bindings
        (fn []
          (conductor/start-apm-conductor! nil :agent-id "codex-1"
                                          :problem-ids ["cb01" "cb02"])

          ;; Feed observe for cb01, then exhaust propose retry budget.
          (@idle-callback "codex-1"
           {:ok true :result "WHAT IS REALLY BEING ASKED\nTest.\n\nWHY IT IS HARD\nTest."})

          ;; Exhaust propose retry budget with repeated failures.
          (dotimes [_ (inc @#'conductor/max-phase-failures)]
            (@idle-callback "codex-1"
             {:ok false
              :error {:message "Propose phase transport failure"}}))

          (testing "first problem is marked abandoned"
            (is (some #(= :problem-abandoned (:event %)) @logs))
            (is (= :abandoned (-> @conductor/!apm-state :batch-results first :classification))))

          (testing "failure report is written"
            (is (.exists (io/file proof-root "apm-cb01-failure.edn"))))

          (testing "conductor continues with second problem"
            (is (= "cb02" (get-in @conductor/!apm-state [:current-problem :id])))
            (is (= :observe (get-in @conductor/!apm-state [:current-phase]))))

          (testing "batch itself remains active"
            (is (some? @conductor/!apm-conductor))))))))

(deftest execute-discipline-break-before-floor-triggers-stop-the-line
  (testing "execute failure budget exhaustion before the 15-minute floor stops the whole batch"
    (let [{:keys [idle-callback logs redef-bindings]}
          (make-test-scaffold :manifest-entries [{:id "cb01" :subject "Analysis"}
                                                 {:id "cb02" :subject "Analysis"}])]
      (with-redefs-fn (assoc redef-bindings
                             #'conductor/discover-lean-artifacts (fn [_ _ _] []))
        (fn []
          (conductor/start-apm-conductor! nil :agent-id "codex-1"
                                          :problem-ids ["cb01" "cb02"])
          (@idle-callback "codex-1"
           {:ok true :result "WHAT IS REALLY BEING ASKED\nTest.\n\nWHY IT IS HARD\nTest."})
          (@idle-callback "codex-1"
           {:ok true :result "THE KEY INSIGHT\nTest.\n\nTHE NAIVE APPROACH THAT FAILS\nTest."})
          (@idle-callback "codex-1"
           {:ok true :result valid-target-check-output})
          (dotimes [_ (inc @#'conductor/max-phase-failures)]
            (@idle-callback "codex-1"
             {:ok false
              :error {:message "Execute phase dependency graph must record formal dependencies, informal strategy triggers, why-they-apply-here cues, and Lean targets for every entry"}}))
          (is (some #(= :stop-the-line (:event %)) @logs))
          (is (nil? @conductor/!apm-conductor)))))))

(deftest arse-kick-fires-after-threshold-sorry-redispatches
  (testing "after arse-kick-threshold sorry re-dispatches, prompt switches to diagnostic"
    (let [{:keys [idle-callback logs prompts redef-bindings]}
          (make-test-scaffold)]
      ;; No artifacts + has-sorry? true → fully-closed? false, sorry? true.
      ;; htdp-record-complete? forced true so the cond reaches the sorry branch.
      (with-redefs-fn (assoc redef-bindings
                             #'conductor/discover-lean-artifacts (fn [_ _ _] [])
                             #'conductor/has-sorry? (fn [_] true)
                             #'conductor/htdp-record-complete? (fn [_ _ _] true))
        (fn []
          (conductor/start-apm-conductor! nil :agent-id "codex-1" :problem-ids ["cb01"])

          ;; Feed observe and propose
          (@idle-callback "codex-1"
           {:ok true :result "WHAT IS REALLY BEING ASKED\nTest.\n\nWHY IT IS HARD\nTest."})
          (@idle-callback "codex-1"
           {:ok true :result "THE KEY INSIGHT\nTest.\n\nTHE NAIVE APPROACH THAT FAILS\nTest."})
          (@idle-callback "codex-1"
           {:ok true :result valid-target-check-output})

          ;; First execute: feed complete Stage 1-4 to populate the execute record.
          ;; has-sorry? is true so fully-closed? is false → sorry re-dispatch.
          (let [full-stages "Stage 1 \u2014 THE CLEAN PROOF\n--------------------------------\n\nProof with sorry remaining.\n\nStage 2 \u2014 LEMMA DEPENDENCY GRAPH\n--------------------------------\n\n1. **Main lemma**\n   - **Formal dependency**: theorem X.\n   - **Informal dependency**: recognize pattern Y.\n   - **Why now**: cue Z.\n   - **Lean target**: `Goal`.\n   - **Critical path**: yes.\n\nStage 3 \u2014 LEAN FORMALIZATION\n--------------------------------\n\nsorry remains in main theorem.\n\nStage 4 \u2014 FORMAL-TO-INFORMAL REVISION\n--------------------------------\n\nRevision."
                threshold @#'conductor/arse-kick-threshold]

            ;; Feed the full-stage text (threshold + 1) times:
            ;; 1st populates the record, subsequent are sorry re-dispatches
            (dotimes [_ (+ threshold 2)]
              (when @conductor/!apm-conductor
                (@idle-callback "codex-1" {:ok true :result full-stages})))

            (testing "normal redispatches happen first"
              (is (some #(= :execute-redispatch (:event %)) @logs)))

            (testing "ArSE kick event fires after threshold"
              (is (some #(= :execute-arse-kick (:event %)) @logs)))

            (testing "ArSE kick prompt includes QP diagnostic questions"
              (let [arse-prompts (filter #(str/includes? % "QP-3 Structural probe") @prompts)]
                (is (seq arse-prompts))))))))))

(deftest sorry-check-returns-boolean-not-nil
  (testing "has-sorry? and artifact checks produce boolean values in log"
    (let [{:keys [idle-callback logs redef-bindings]}
          (make-test-scaffold)]
      ;; No artifacts → (some artifact-has-sorry? []) would return nil without the fix
      (with-redefs-fn (assoc redef-bindings
                             #'conductor/discover-lean-artifacts (fn [_ _ _] []))
        (fn []
          (conductor/start-apm-conductor! nil :agent-id "codex-1" :problem-ids ["cb01"])

          ;; Feed observe and propose
          (@idle-callback "codex-1"
           {:ok true :result "WHAT IS REALLY BEING ASKED\nTest.\n\nWHY IT IS HARD\nTest."})
          (@idle-callback "codex-1"
           {:ok true :result "THE KEY INSIGHT\nTest.\n\nTHE NAIVE APPROACH THAT FAILS\nTest."})
          (@idle-callback "codex-1"
           {:ok true :result valid-target-check-output})

          ;; Feed execute with no sorry mention and no artifacts
          (@idle-callback "codex-1"
           {:ok true :result "Some notes without the word s-o-r-r-y."})

          (testing "sorry? is boolean false, not nil"
            (let [execute-returns (filter #(= :execute-return (:event %)) @logs)]
              (is (seq execute-returns))
              (doseq [entry execute-returns]
                (is (boolean? (:sorry? entry))
                    (str "sorry? should be boolean, got: " (pr-str (:sorry? entry))))))))))))
