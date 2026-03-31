(ns futon3c.dev.apm-conductor-v2-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [futon3c.agency.registry :as reg]
            [futon3c.agents.apm-work-queue :as apm-queue]
            [futon3c.dev.apm-conductor-v2 :as v2]
            [futon3c.peripheral.proof-backend :as pb]
            [futon3c.peripheral.tools :as tools]))

;; =============================================================================
;; Test backend (minimal — v2 barely uses the proof backend)
;; =============================================================================

(deftype MinimalBackend [cache]
  tools/ToolBackend
  (execute-tool [_ tool-id args]
    (case tool-id
      :proof-load
      (let [pid (first args)]
        (swap! cache assoc pid {:proof/problem-id pid :proof/cycles []})
        {:ok true :result (get @cache pid)})
      :cycle-begin
      {:ok true :result {:cycle/id "C1"}}
      {:ok true :result nil})))

;; =============================================================================
;; Test scaffold
;; =============================================================================

(defn- make-v2-scaffold
  [& {:keys [manifest-entries]
      :or {manifest-entries [{:id "v2p01" :subject :analysis}]}}]
  (let [tmp-dir (.toFile (java.nio.file.Files/createTempDirectory
                          "apm-v2" (make-array java.nio.file.attribute.FileAttribute 0)))
        proof-root (.getAbsolutePath (io/file tmp-dir "proof-state"))
        artifact-dir (io/file tmp-dir "lean-proofs" "v2p01")
        _ (.mkdirs artifact-dir)
        idle-callback (atom nil)
        prompts (atom [])
        logs (atom [])
        backend (MinimalBackend. (atom {}))]
    {:idle-callback idle-callback
     :prompts prompts
     :logs logs
     :proof-root proof-root
     :artifact-dir artifact-dir
     :redef-bindings
     {#'v2/log! (fn [entry] (swap! logs conj entry))
      #'v2/proof-state-root proof-root
      #'v2/lean-proofs-root (.getAbsolutePath (io/file tmp-dir "lean-proofs"))
      #'reg/set-on-idle! (fn [f] (reset! idle-callback f))
      #'reg/invoke-agent! (fn [_agent-id prompt _timeout-ms]
                             (swap! prompts conj prompt)
                             {:ok true :result ""})
      #'apm-queue/load-apm-manifest (fn [] manifest-entries)
      #'apm-queue/load-problem-tex (fn [_] "Problem \\LaTeX body")
      #'apm-queue/emit-apm-evidence! (fn [& _] nil)
      #'pb/make-proof-backend (fn [_config] backend)}}))

;; =============================================================================
;; Sample agent output (what a good single-dispatch run looks like)
;; =============================================================================

(def ^:private good-solve-output
  "## Why it's hard

Students try to apply dominated convergence directly but there's no
dominating function that works globally.

## The key insight

Split the integral using a density argument: approximate the L1 function
by simple functions, handle each piece via the rectangle-average bound.

## Proof

Let f be in L1. By density of simple functions, for any epsilon > 0 there
exists a simple function s with ||f - s||_1 < epsilon. For simple functions,
the integral against phi(kx) decays by the rectangle-average bound. The
error term is controlled by epsilon. QED.

## Lean 4

```lean
import Mathlib

theorem oscillatory_decay (f : ℝ → ℝ) (hf : Integrable f)
    (phi : ℝ → ℝ) (hphi : BoundedRectangleAverages phi) :
    Filter.Tendsto (fun k => ∫ x, phi (k * x) * f x) Filter.atTop (nhds 0) := by
  sorry -- Mathlib lacks BoundedRectangleAverages; see boundary note below
```

**Mathlib boundary**: `BoundedRectangleAverages` is not in Mathlib. To close
this sorry, one would need to define rectangle averages and prove the decay
lemma for simple functions, then extend by density.

## Classification

**Classification**: partial (one sorry at Mathlib boundary).

**CONFIDENCE INVERSION**: The proof is trivially correct by density + decay,
but formalizing 'rectangle average decay' requires custom Mathlib infrastructure.

## Connections

This connects to Riemann-Lebesgue lemma, equidistribution theory, and
Weyl's theorem on uniform distribution.

## EXAM-DAY FIELD KIT

1. Density of simple functions in L1
2. Rectangle-average decay for simple functions
3. Epsilon/3 splitting argument

## ArSE Questions

1. *Why is this hard?*
   **Q:** Why can't we just apply DCT?
   **A:** No dominating function works globally — the decay is oscillatory.
2. *What is the key insight?*
   **Q:** What unlocks the proof?
   **A:** Approximate by simple functions; the oscillation kills simple integrals.
3. *Why does step N work?*
   **Q:** Why does the simple-function case decay?
   **A:** Because rectangle averages are bounded, and simple functions have finite support.
4. *What connects?*
   **Q:** Where else does this technique appear?
   **A:** Riemann-Lebesgue lemma, Weyl equidistribution, van der Corput estimates.
5. *Where is intuition wrong?*
   **Q:** What false instinct should we avoid?
   **A:** Thinking you need pointwise control. You only need L1 approximation.")

;; =============================================================================
;; Tests
;; =============================================================================

(deftest v2-single-dispatch-completes-proved-problem
  (testing "a clean solve with no sorry completes as proved in one dispatch"
    (let [{:keys [idle-callback logs prompts redef-bindings artifact-dir]}
          (make-v2-scaffold)]
      ;; Create a clean Lean artifact (no sorry)
      (spit (io/file artifact-dir "Main.lean")
            "import Mathlib\n\ntheorem clean (x : ℝ) : x = x := by rfl\n")
      (with-redefs-fn redef-bindings
        (fn []
          (v2/start-apm-conductor-v2! nil :agent-id "codex-1" :problem-ids ["v2p01"])
          ;; Agent returns clean solve output
          (@idle-callback "codex-1" {:ok true :result good-solve-output})

          (testing "problem completed as proved"
            (is (some #(= :problem-complete (:event %)) @logs))
            (let [complete-entry (first (filter #(= :problem-complete (:event %)) @logs))]
              (is (= "proved" (:classification complete-entry)))))

          (testing "extraction ran — phases and ArSE questions found"
            (let [result (first (:batch-results @v2/!state))]
              (is (:ok result))
              (is (= :proved (:classification result)))
              (is (pos? (get-in result [:extraction :phase-count])))
              (is (= 5 (get-in result [:extraction :arse-count])))))

          (testing "single dispatch prompt was sent"
            (is (= 1 (count @prompts)))
            (is (str/includes? (first @prompts) "Solve this problem completely")))

          (testing "conductor stopped after batch complete"
            (is (nil? @v2/!conductor))))))))

(deftest v2-sorry-kick-loop-then-partial
  (testing "sorry in Lean triggers kick loop, exhausts budget, completes as partial"
    (let [{:keys [idle-callback logs prompts redef-bindings artifact-dir]}
          (make-v2-scaffold)]
      ;; Create a Lean artifact WITH sorry
      (spit (io/file artifact-dir "Main.lean")
            "import Mathlib\n\ntheorem needs_work : True := by sorry\n")
      (with-redefs-fn redef-bindings
        (fn []
          (v2/start-apm-conductor-v2! nil :agent-id "codex-1" :problem-ids ["v2p01"])

          ;; Initial solve returns output mentioning sorry
          (@idle-callback "codex-1" {:ok true :result "Proof done. sorry remains in Main.lean."})

          ;; Feed sorry-kick responses until budget exhausted
          (let [max-kicks @#'v2/max-sorry-kicks]
            (dotimes [_ max-kicks]
              (when @v2/!conductor
                (@idle-callback "codex-1"
                 {:ok true :result "Tried closing sorry but Mathlib lacks the API. sorry still present."}))))

          (testing "sorry-kicks were logged"
            (is (some #(= :sorry-kick (:event %)) @logs)))

          (testing "ArSE kick fired after threshold"
            (is (some #(= :arse-kick (:event %)) @logs)))

          (testing "completed as partial"
            (is (some #(and (= :problem-complete (:event %))
                            (= "partial" (:classification %)))
                      @logs)))

          (testing "ArSE kick prompt includes diagnostic questions"
            (is (some #(str/includes? % "What is the exact Lean goal state") @prompts))))))))

(deftest v2-problem-timeout-skips-to-next
  (testing "overall problem timeout skips to next problem"
    (let [{:keys [idle-callback logs redef-bindings]}
          (make-v2-scaffold :manifest-entries [{:id "v2p01" :subject :analysis}
                                               {:id "v2p02" :subject :analysis}])]
      (with-redefs-fn redef-bindings
        (fn []
          (v2/start-apm-conductor-v2! nil :agent-id "codex-1"
                                      :problem-ids ["v2p01" "v2p02"])

          ;; Backdate problem-start-ms to simulate timeout
          (swap! v2/!state assoc
                 :problem-start-ms (- (System/currentTimeMillis)
                                      @#'v2/problem-timeout-ms
                                      1000))

          (@idle-callback "codex-1" {:ok true :result "Some output."})

          (testing "timeout event logged"
            (is (some #(= :problem-timed-out (:event %)) @logs)))

          (testing "moved to second problem"
            (is (= "v2p02" (get-in @v2/!state [:current-problem :id])))))))))

(deftest v2-extraction-parses-structured-data-from-natural-output
  (testing "run-extraction finds phases, dep graph, and ArSE questions"
    (let [extraction (#'v2/run-extraction good-solve-output)]
      (testing "phases extracted"
        (is (pos? (:phase-count extraction)))
        (is (contains? (:phases extraction) :classify)))

      (testing "ArSE questions extracted"
        (is (= 5 (:arse-count extraction))))

      (testing "classification detected"
        (is (= :partial (:classification extraction)))))))

(deftest v2-prompt-is-concise
  (testing "the solve prompt is much shorter than v1's inhabitation prompt"
    (let [problem {:id "test01" :subject :analysis :year 2001 :session :spring}
          v2-prompt (#'v2/make-solve-prompt problem "\\int f(x) dx = 0")
          v1-prompt (#'apm-queue/make-inhabitation-prompt problem "\\int f(x) dx = 0")]
      (is (< (count v2-prompt) (/ (count v1-prompt) 2))
          "v2 prompt should be less than half the length of v1"))))
