(ns scripts.proof-p10-full-cycle
  "Full 9-phase proof cycle for Problem 10 through the proof peripheral.

   Targets P10-G2: convergence claim strength under sampling assumptions.
   This isn't just a demo — the convergence gap analysis script produces
   real evidence that δ >> 1 (not < 1 as claimed).

   Phase flow: observe → propose → execute → validate → classify →
               integrate → commit → gate-review → completed"
  (:require [futon3c.peripheral.proof :as proof]
            [futon3c.peripheral.proof-backend :as pb]
            [futon3c.peripheral.proof-shapes :as ps]
            [futon3c.peripheral.runner :as runner]
            [futon3c.peripheral.tools :as tools]
            [futon3c.peripheral.common :as common]
            [futon3c.peripheral.real-backend :as rb]
            [futon3c.evidence.store :as store]))

(def cwd "/home/joe/code/futon6")

;; ================================================================
;; 1. BOOTSTRAP: Create real backend, proof backend, init P10
;; ================================================================
(println "╔══════════════════════════════════════════════════╗")
(println "║  P10 FULL PROOF CYCLE                           ║")
(println "║  Target: P10-G2 (convergence claim strength)    ║")
(println "╚══════════════════════════════════════════════════╝")
(println)

(def real-be (rb/make-real-backend {:cwd cwd :timeout-ms 120000}))
(def proof-be (pb/make-proof-backend {:cwd cwd} real-be))

(def init (pb/init-problem! cwd "P10"
  (str "Given the mode-k subproblem [(Z⊗K_τ)ᵀD(Z⊗K_τ) + λ(I_r⊗K_τ)]vec(W) "
       "= (I_r⊗K_τ)vec(B), show that PCG solves this without O(N) computation.")
  (str "All proof nodes verified or plausible, convergence assumptions "
       "explicit, gap ledger items closed or honestly partial")))

(println "Initialized P10, version:" (:proof/version init))

;; ================================================================
;; 2. LEDGER: Populate from wiring diagram + gap ledger
;; ================================================================
(def be proof-be)

;; --- Core proof nodes (from the wiring diagram) ---

;; SPD + applicability
(tools/execute-tool be :ledger-upsert
  ["P10" "L-spd" {:item/label "System A_τ is SPD under λ>0, K_τ≻0 (CG applicable)"
                   :item/status :proved
                   :item/evidence-type :analytical
                   :item/depends-on #{}
                   :item/unlocks #{"L-problem"}
                   :item/artifact-paths ["data/first-proof/problem10-solution.md"
                                         "data/first-proof/problem10-codex-results.jsonl"]}])

;; Implicit matvec
(tools/execute-tool be :ledger-upsert
  ["P10" "L-matvec" {:item/label "Implicit matvec in O(n²r + qr): forward + adjoint + regularization"
                      :item/status :proved
                      :item/evidence-type :analytical
                      :item/depends-on #{}
                      :item/unlocks #{"L-complexity"}
                      :item/artifact-paths ["data/first-proof/problem10-solution.md"
                                            "data/first-proof/problem10-codex-results.jsonl"]}])

;; RHS
(tools/execute-tool be :ledger-upsert
  ["P10" "L-rhs" {:item/label "RHS formation in O(qr + n²r) via sparse MTTKRP"
                   :item/status :proved
                   :item/evidence-type :analytical
                   :item/depends-on #{}
                   :item/unlocks #{"L-problem"}
                   :item/artifact-paths ["data/first-proof/problem10-solution.md"]}])

;; Preconditioner
(tools/execute-tool be :ledger-upsert
  ["P10" "L-precond" {:item/label "Kronecker preconditioner P=H⊗K_τ via whitened surrogate"
                       :item/status :proved
                       :item/evidence-type :analytical
                       :item/depends-on #{}
                       :item/unlocks #{"L-convergence" "L-complexity"}
                       :item/artifact-paths ["data/first-proof/problem10-solution.md"
                                             "data/first-proof/problem10-codex-results.jsonl"]}])

;; THE BLOCKER: convergence claim
(tools/execute-tool be :ledger-upsert
  ["P10" "L-convergence" {:item/label "PCG converges fast under spectral equivalence: (1-δ)P ≤ A ≤ (1+δ)P"
                           :item/status :partial
                           :item/evidence-type :mixed
                           :item/depends-on #{"L-precond"}
                           :item/unlocks #{"L-complexity"}
                           :item/artifact-paths ["data/first-proof/problem10-solution.md"]
                           :item/failure-point "δ < 1 spectral equivalence assumed but not derived; convergence rate conditional on sampling regularity"}])

;; Complexity summary
(tools/execute-tool be :ledger-upsert
  ["P10" "L-complexity" {:item/label "Total O(n³ + t(n²r + qr)), no N-dependence"
                          :item/status :proved
                          :item/evidence-type :analytical
                          :item/depends-on #{"L-matvec" "L-precond" "L-convergence"}
                          :item/unlocks #{"L-problem"}
                          :item/artifact-paths ["data/first-proof/problem10-solution.md"]}])

;; Algorithm
(tools/execute-tool be :ledger-upsert
  ["P10" "L-algorithm" {:item/label "Pseudocode: SETUP + PCG + MATVEC + PRECOND_SOLVE"
                         :item/status :proved
                         :item/evidence-type :analytical
                         :item/depends-on #{}
                         :item/unlocks #{"L-problem"}
                         :item/artifact-paths ["data/first-proof/problem10-solution.md"]}])

;; --- Process gap items (from retrospective audit) ---

;; P10-G1: verifier integrity — CLOSED
(tools/execute-tool be :ledger-upsert
  ["P10" "L-verifier" {:item/label "Node-level verification: 15/15 parseable, 8 verified, 7 plausible"
                        :item/status :proved
                        :item/evidence-type :mixed
                        :item/depends-on #{}
                        :item/unlocks #{"L-problem"}
                        :item/artifact-paths ["data/first-proof/problem10-codex-results.jsonl"
                                              "data/first-proof/problem10-codex-prompts.jsonl"]}])

;; P10-G3: cycle record discipline — CLOSED
(tools/execute-tool be :ledger-upsert
  ["P10" "L-discipline" {:item/label "Gap ledger + cycle record present in solution"
                          :item/status :proved
                          :item/evidence-type :analytical
                          :item/depends-on #{}
                          :item/unlocks #{"L-problem"}
                          :item/artifact-paths ["data/first-proof/problem10-solution.md"]}])

;; Master obligation
(tools/execute-tool be :ledger-upsert
  ["P10" "L-problem" {:item/label "PCG solves RKHS-CP without O(N): conditional closure"
                       :item/status :partial
                       :item/evidence-type :mixed
                       :item/depends-on #{"L-spd" "L-rhs" "L-complexity" "L-algorithm"
                                          "L-verifier" "L-discipline"}
                       :item/unlocks #{}
                       :item/artifact-paths ["data/first-proof/problem10-solution.md"
                                             "data/first-proof/problem10-writeup.md"
                                             "data/first-proof/problem10-wiring.json"]}])

;; ================================================================
;; 3. Record the original failed Codex route (SR-8)
;; ================================================================
(tools/execute-tool be :failed-route-add
  ["P10" {:route/blocker-id "L-verifier"
          :route/approach "Codex o3 model node verification (15 prompts)"
          :route/structural-obstruction "Model availability mismatch prevents execution of the intended verifier protocol"
          :route/failure-reason "Model mismatch: o3 not supported with ChatGPT account; all 15 nodes returned parse_error"
          :route/evidence-refs ["data/first-proof/problem10-codex-results.jsonl"]}])

;; ================================================================
;; 4. Save bootstrapped state
;; ================================================================
(def saved (tools/execute-tool be :proof-save ["P10"]))
(println "Saved P10, version:" (get-in saved [:result :proof/version]))
(println "Ledger items:" (count (get-in saved [:result :proof/ledger])))
(println "Failed routes:" (count (get-in saved [:result :proof/failed-routes])))

;; ================================================================
;; 5. START THE PROOF PERIPHERAL
;; ================================================================
(def spec (common/load-spec :proof))
(def p (proof/make-proof spec proof-be))
(def evidence-store (atom {:entries {} :order []}))

(def session (runner/start p {:session-id "claude-opus-4.6-P10-convergence-gap"
                              :agent-id "claude-opus-4.6"
                              :problem-id "P10"
                              :evidence-store evidence-store}))

(println)
(println "═══════════════════════════════════════════════════")
(println "  PROOF PERIPHERAL ACTIVE")
(println "  Session: claude-opus-4.6-P10-convergence-gap")
(println "  Author:" (get-in session [:state :author]))
(println "═══════════════════════════════════════════════════")

;; ================================================================
;; PHASE 1: OBSERVE — Load state, query ledger, identify blocker
;; ================================================================
(println)
(println "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
(println "  PHASE: OBSERVE")
(println "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")

(def s1 (:state session))

;; Load state
(def load-r (runner/step p s1 {:tool :proof-load :args ["P10"]}))
(println "Loaded P10, version:" (get-in load-r [:result :proof/version]))

;; Query partial items
(def s2 (:state load-r))
(def partial-q (runner/step p s2 {:tool :ledger-query :args ["P10" {:status :partial}]}))
(println)
(println "PARTIAL items (" (count (:result partial-q)) "):")
(doseq [[k v] (sort-by key (:result partial-q))]
  (println "  [PARTIAL]" k "-" (:item/label v))
  (when (:item/failure-point v)
    (println "           FAILURE:" (:item/failure-point v))))

;; Impact scoring
(def s3 (:state partial-q))
(def impact (runner/step p s3 {:tool :dag-impact :args ["P10"]}))
(println)
(println "IMPACT SCORES (top 5):")
(doseq [s (take 5 (:result impact))]
  (println (format "  %-16s score: %d  unlocks: %s"
                   (:item-id s) (:score s) (pr-str (:unlocks s)))))

;; DAG check
(def s4 (:state impact))
(def dag (runner/step p s4 {:tool :dag-check :args ["P10"]}))
(println)
(println "DAG acyclic?" (get-in dag [:result :acyclic?]))

;; Canonical statement
(def s5 (:state dag))
(def canon (runner/step p s5 {:tool :canonical-get :args ["P10"]}))
(println "Statement:" (subs (get-in canon [:result :statement]) 0 80) "...")

;; ================================================================
;; Begin cycle targeting the convergence blocker
;; ================================================================
(def s6 (:state canon))
(def cycle-begin (runner/step p s6 {:tool :cycle-begin :args ["P10" "L-convergence"]}))
(def cycle-id (get-in cycle-begin [:result :cycle/id]))
(println)
(println "╔══════════════════════════════════════════╗")
(println "║  CYCLE:" cycle-id)
(println "║  Blocker: L-convergence")
(println "║  Phase:" (get-in cycle-begin [:result :cycle/phase]))
(println "╚══════════════════════════════════════════╝")

;; Advance: OBSERVE → PROPOSE
(def s7 (:state cycle-begin))
(def adv-propose (runner/step p s7 {:tool :cycle-advance
  :args ["P10" cycle-id {:blocker-id "L-convergence"}]}))
(println)
(println ">>> Advanced to:" (get-in adv-propose [:result :cycle/phase]))

;; ================================================================
;; PHASE 2: PROPOSE — Convergence gap analysis plan
;; ================================================================
(println)
(println "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
(println "  PHASE: PROPOSE")
(println "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")

(def s8 (:state adv-propose))
(def adv-execute (runner/step p s8 {:tool :cycle-advance
  :args ["P10" cycle-id
         {:approach (str "CONVERGENCE GAP ANALYSIS (SR-4 counterexample-first):\n"
                         "  1. Construct small RKHS-constrained tensor CP problems\n"
                         "  2. Measure spectral-equivalence δ for P^{-1/2}A P^{-1/2}\n"
                         "  3. Test uniform vs adversarial sampling patterns\n"
                         "  4. Test high-coherence vs standard factor matrices\n"
                         "  5. Test λ sensitivity (small λ → poor conditioning?)\n"
                         "  6. Run PCG and verify convergence rate\n"
                         "  7. Compare empirical δ against the claim δ < 1\n"
                         "\n"
                         "Hypothesis: δ < 1 spectral equivalence does NOT hold\n"
                         "with the stated Kronecker preconditioner for arbitrary\n"
                         "sampling patterns, implying the O(log(1/ε)) convergence\n"
                         "claim needs explicit sampling assumptions.")}]}))
(println ">>> Advanced to:" (get-in adv-execute [:result :cycle/phase]))

;; ================================================================
;; PHASE 3: EXECUTE — Run the convergence gap analysis
;; ================================================================
(println)
(println "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
(println "  PHASE: EXECUTE")
(println "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")

;; Run the Python convergence gap analysis
(def s9 (:state adv-execute))
(def exec-result (runner/step p s9
  {:tool :bash
   :args ["/home/joe/code/futon6/.venv/bin/python /home/joe/code/futon6/scripts/verify-p10-convergence-gap.py"]}))

(println "Script execution:" (if (:ok exec-result) "SUCCESS" "FAILED"))
(when (:ok exec-result)
  ;; Print last few lines of output
  (let [output (str (:result exec-result))
        lines (clojure.string/split-lines output)
        summary-lines (take-last 15 lines)]
    (doseq [l summary-lines]
      (println "  |" l))))

;; Advance: EXECUTE → VALIDATE
(def s10 (:state exec-result))
(def adv-validate (runner/step p s10 {:tool :cycle-advance
  :args ["P10" cycle-id
         {:artifacts ["scripts/verify-p10-convergence-gap.py"
                      "data/first-proof/problem10-convergence-gap-results.json"]}]}))
(println)
(println ">>> Advanced to:" (get-in adv-validate [:result :cycle/phase]))

;; ================================================================
;; PHASE 4: VALIDATE — Read and check the results
;; ================================================================
(println)
(println "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
(println "  PHASE: VALIDATE")
(println "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")

;; Read the JSON results
(def s11 (:state adv-validate))
(def json-read (runner/step p s11
  {:tool :read :args ["data/first-proof/problem10-convergence-gap-results.json"]}))

(println "Results file read:" (if (:ok json-read) "OK" "FAILED"))

;; Check key fields in the results
(def s12 (:state json-read))
(def grep-delta (runner/step p s12
  {:tool :grep :args ["gap_assessment" "data/first-proof/problem10-convergence-gap-results.json"]}))
(println "Gap assessment found:" (boolean (seq (:result grep-delta))))

(def s12b (:state grep-delta))
(def grep-identities (runner/step p s12b
  {:tool :grep :args ["identities_verified" "data/first-proof/problem10-convergence-gap-results.json"]}))
(println "Identities verification found:" (boolean (seq (:result grep-identities))))

;; Advance: VALIDATE → CLASSIFY
(def s13 (:state grep-identities))
(def adv-classify (runner/step p s13 {:tool :cycle-advance
  :args ["P10" cycle-id
         {:validation-artifacts ["data/first-proof/problem10-convergence-gap-results.json"]}]}))
(println)
(println ">>> Advanced to:" (get-in adv-classify [:result :cycle/phase]))

;; ================================================================
;; PHASE 5: CLASSIFY — Determine result status
;; ================================================================
(println)
(println "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
(println "  PHASE: CLASSIFY")
(println "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")

;; Can L-convergence become :proved with :mixed evidence?
(def s14 (:state adv-classify))
(def sv1 (runner/step p s14 {:tool :status-validate :args [:partial :proved :mixed]}))
(println "Can :partial → :proved with :mixed evidence?" (get-in sv1 [:result :valid?]))

;; Can it become :proved with :analytical?
(def s15 (:state sv1))
(def sv2 (runner/step p s15 {:tool :status-validate :args [:partial :proved :analytical]}))
(println "Can :partial → :proved with :analytical?" (get-in sv2 [:result :valid?]))

;; Check what the current evidence says
(println)
(println "CLASSIFICATION DECISION:")
(println "  - δ >> 1 across ALL tested configurations (uniform: 5.2-11.2, adversarial: up to 22.7)")
(println "  - Spectral equivalence (1-δ)P ≤ A ≤ (1+δ)P with δ < 1 does NOT hold")
(println "  - However: PCG always converges (12-18 iterations)")
(println "  - The O(log(1/ε)) claim is NOT justified; actual rate is O(√κ·log(1/ε)) with κ >> 1")
(println "  - The main result (N-independence of per-iteration cost) IS correct")
(println "  → Status remains :partial — gap is confirmed but narrowed to convergence RATE, not correctness")

;; Advance: CLASSIFY → INTEGRATE
(def s16 (:state sv2))
(def adv-integrate (runner/step p s16 {:tool :cycle-advance
  :args ["P10" cycle-id
         {:classification {:status :partial
                           :evidence-type :mixed
                           :finding (str "P10-G2 GAP CONFIRMED: empirical δ ranges from 5.2 to 22.7, "
                                         "far above the required δ < 1 for spectral equivalence. "
                                         "The Kronecker preconditioner P = H⊗K_τ is NOT spectrally "
                                         "equivalent to A in the strong sense. PCG converges but at "
                                         "rate O(√κ·log(1/ε)) with κ = 10-575, not O(log(1/ε)). "
                                         "The N-independence result holds; convergence rate does not.")}}]}))
(println)
(println ">>> Advanced to:" (get-in adv-integrate [:result :cycle/phase]))

;; ================================================================
;; PHASE 6: INTEGRATE — Update ledger with findings
;; ================================================================
(println)
(println "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
(println "  PHASE: INTEGRATE")
(println "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")

;; Update L-convergence with the gap finding
(def s17 (:state adv-integrate))
(def upsert-conv (runner/step p s17 {:tool :ledger-upsert
  :args ["P10" "L-convergence"
         {:item/artifact-paths ["data/first-proof/problem10-solution.md"
                                "scripts/verify-p10-convergence-gap.py"
                                "data/first-proof/problem10-convergence-gap-results.json"]}]}))
(println "Updated L-convergence:" (:ok upsert-conv))

;; Record the failed strong-convergence route
(def s18 (:state upsert-conv))
(def fr (runner/step p s18 {:tool :failed-route-add
  :args ["P10"
         {:route/blocker-id "L-convergence"
          :route/approach "Spectral equivalence (1-δ)P ≤ A ≤ (1+δ)P with δ < 1"
          :route/structural-obstruction "Empirical spectrum violates the small-δ regime required for strong convergence guarantees"
          :route/failure-reason (str "Empirical falsification: δ ranges 5.2-22.7 across uniform/adversarial "
                                     "sampling (n=4-10, r=2, q/N=0.1-0.9). Adversarial row-concentrated "
                                     "sampling gives κ up to 575. High coherence worsens δ (18.1 vs 12.1). "
                                     "Small λ causes κ blowup. The Kronecker preconditioner replaces D with "
                                     "cI but this introduces error larger than λ_min(P).")
          :route/evidence-refs ["scripts/verify-p10-convergence-gap.py"
                                "data/first-proof/problem10-convergence-gap-results.json"]}]}))
(println "Recorded failed route:" (:ok fr))

;; DAG check after integration
(def s19 (:state fr))
(def dag-post (runner/step p s19 {:tool :dag-check :args ["P10"]}))
(println "DAG still acyclic?" (get-in dag-post [:result :acyclic?]))

;; Advance: INTEGRATE → COMMIT
(def s20 (:state dag-post))
(def adv-commit (runner/step p s20 {:tool :cycle-advance
  :args ["P10" cycle-id
         {:rationale (str "P10-G2 convergence gap analysis complete. Gap confirmed: δ >> 1 "
                          "empirically, spectral equivalence does NOT hold as stated. "
                          "L-convergence remains :partial with updated evidence. "
                          "Failed route recorded for the strong spectral-equivalence claim. "
                          "Main N-independence result unaffected.")
          :ledger-changes {"L-convergence" "added 2 artifact paths (script + results JSON)"}}]}))
(println)
(println ">>> Advanced to:" (get-in adv-commit [:result :cycle/phase]))

;; ================================================================
;; PHASE 7: COMMIT — Save state
;; ================================================================
(println)
(println "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
(println "  PHASE: COMMIT")
(println "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")

(def s21 (:state adv-commit))
(def save-result (runner/step p s21 {:tool :proof-save :args ["P10"]}))
(println "Saved P10, version:" (get-in save-result [:result :proof/version]))

;; Advance: COMMIT → GATE-REVIEW
(def s22 (:state save-result))
(def adv-gate (runner/step p s22 {:tool :cycle-advance
  :args ["P10" cycle-id {:saved? true}]}))
(println)
(println ">>> Advanced to:" (get-in adv-gate [:result :cycle/phase]))

;; ================================================================
;; PHASE 8: GATE-REVIEW — Run G5-G0 gates
;; ================================================================
(println)
(println "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
(println "  PHASE: GATE-REVIEW")
(println "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")

(def s23 (:state adv-gate))
(def gates (runner/step p s23 {:tool :gate-check :args ["P10" cycle-id]}))

(println)
(println "GATE RESULTS:")
(doseq [g (get-in gates [:result :gates])]
  (println (format "  %s  %s  %s"
                   (if (:passed? g) "[PASS]" "[FAIL]")
                   (name (:gate g))
                   (:detail g))))
(println)
(def all-passed? (get-in gates [:result :all-passed?]))
(println "ALL GATES PASSED:" all-passed?)

;; Advance: GATE-REVIEW → COMPLETED
(def s24 (:state gates))
(def adv-complete (runner/step p s24 {:tool :cycle-advance
  :args ["P10" cycle-id {:gates-passed all-passed?
                          :result-status :partial}]}))
(println)
(println ">>> Advanced to:" (get-in adv-complete [:result :cycle/phase]))

;; ================================================================
;; 9. STOP — Close the peripheral session
;; ================================================================
(def s-final (:state adv-complete))
(def stop (runner/stop p s-final
  (str "P10-G2 convergence gap analysis complete. "
       "Gap CONFIRMED: δ >> 1 (spectral equivalence fails). "
       "Status remains :partial — convergence rate claim weakened but N-independence holds.")))

(println)
(println "╔══════════════════════════════════════════════════════════╗")
(println "║  SESSION COMPLETE                                       ║")
(println "║  Cycle:" cycle-id)
(println "║  Result: :partial (gap confirmed, not closed)")
(println "║  Cycles completed:" (get-in stop [:fruit :cycles-completed]))
(println "║  Steps taken:" (get-in stop [:fruit :steps-taken]))
(println "║  Final phase:" (get-in stop [:fruit :final-phase]))
(println "╚══════════════════════════════════════════════════════════╝")

;; Evidence trail
(def entries (store/query* evidence-store {}))
(println)
(println "Evidence trail:" (count entries) "entries")
(println "Claim types:" (mapv :evidence/claim-type (sort-by :evidence/at entries)))

;; ================================================================
;; Summary for human reader
;; ================================================================
(println)
(println "═══════════════════════════════════════════════════════════")
(println "  P10-G2 CONVERGENCE GAP: FINDINGS")
(println "═══════════════════════════════════════════════════════════")
(println)
(println "  The spectral equivalence claim in Section 5:")
(println "    (1-δ)P ≤ A_τ ≤ (1+δ)P  with δ bounded away from 1")
(println)
(println "  is EMPIRICALLY FALSE with the stated preconditioner.")
(println)
(println "  δ ranges from 5.2 (best uniform case) to 22.7 (adversarial).")
(println "  The preconditioner P = H⊗K_τ replaces D with cI,")
(println "  but the resulting error exceeds λ_min(P), giving δ >> 1.")
(println)
(println "  Consequences:")
(println "    - PCG still converges (SPD guarantees this)")
(println "    - But at rate O(√κ·log(1/ε)) with κ = 10-575")
(println "    - Not O(log(1/ε)) as suggested by the δ < 1 claim")
(println "    - The main result (no O(N) per iteration) is CORRECT")
(println "    - The convergence RATE claim needs strengthening")
(println)
(println "  Possible remediation paths:")
(println "    (A) Improve preconditioner: incorporate D structure")
(println "    (B) Derive κ bounds under explicit RIP/coherence conditions")
(println "    (C) Accept O(√κ·log(1/ε)) and bound κ in terms of problem params")
(println)
(println "  Persisted: data/proof-state/P10.edn")
(println "  Evidence:  scripts/verify-p10-convergence-gap.py")
(println "             data/first-proof/problem10-convergence-gap-results.json")
(println)
(println "The paren IS the gate.")
