Final report — META self-heal repair `repair/239cf98836afc977cd589141c224ec3f4bca83dc`, attempt 1 (author zai-2).

**Artifacts changed (in `/home/joe/code/futon2`, uncommitted — final-report reserve stopped me before commit):**

1. `src/futon2/aif/wm/loop_node_exercise.clj` — retention is now read from the decision itself, not only the selection certificate: `:candidates`/`:policies` fall back to decision-level; `:policy-comparison` falls back to decision top level; probabilities/temperature fall back to decision `:policy-precision-state`; per-candidate policy precedence falls back to the comparison's `:precedence` when candidates carry none. Empty selections still measure 4 refused nodes.
2. `src/futon2/aif/wm/terminal_receipt.clj` — `action-receipt` now types absences instead of voiding the receipt: `:click-id` falls back to `{:absent :no-click-id}`, `:outcome` to `{:absent :no-recorded-outcome}`. A record with a real `:cascade-selected` selection always terminal-receipts as an `:action-receipt`; the exactly-one-kind validation is unchanged.
3. `test/futon2/aif/wm/terminal_receipt_test.clj` — updated `attach-never-throws-and-types-an-invalid-receipt` to the new contract (missing click bookkeeping → typed absence on an action receipt; the throw path attach must survive is now the candidate-identity mismatch).
4. `test/futon2/aif/meta_loop_node_injury_acceptance_test.clj` — written verbatim from the work order source.
5. `deps.edn` — added `:main-opts ["-m" "cognitect.test-runner"]` to the `:test` alias so the pinned acceptance command `clojure -M:test -n <ns>` actually dispatches the runner (it previously treated `-n` as a script file and failed with FileNotFound, regardless of my code changes).

**Validation:**
- clj-kondo on all four touched Clojure files: 0 errors, 0 warnings.
- The exact pinned acceptance command passes: `clojure -M:test -n futon2.aif.meta-loop-node-injury-acceptance-test` → 4 tests, 15 assertions, 0 failures, 0 errors. The pinned run record `tick-run-record-2026-10-19-f9abc990-…edn` is unmodified and its sha256 `f6634b04…` matches the pin.
- Regression: `futon2.aif.loop-node-exercise-test` + `futon2.aif.wm.terminal-receipt-test` + acceptance → 22 tests, 77 assertions, 0 failures.
- `futon2.aif.selection-failure-retention-test` + `futon2.aif.full-loop-runner-test` showed 329 failures / 23 errors — but these namespaces exercise the live serving JVM (on-demand canary clicks, XTDB discharge lookups; failures are `:no-selection` outcomes). I did **not** establish a pre-change baseline for them before the report reserve, so I cannot claim they are either pre-existing or caused by my diff. That baseline check is the one remaining obstruction, followed by the commit.

```clojure
{:status :incomplete-blocking-commit-and-baseline
 :payload {:repair-id "repair/239cf98836afc977cd589141c224ec3f4bca83dc"
           :injury-id "injury/d755c886a22d6fc8c04db7fada64089e07157f62"
           :attempt 1
           :changed ["src/futon2/aif/wm/loop_node_exercise.clj"
                     "src/futon2/aif/wm/terminal_receipt.clj"
                     "test/futon2/aif/wm/terminal_receipt_test.clj"
                     "test/futon2/aif/meta_loop_node_injury_acceptance_test.clj"
                     "deps.edn"]
           :acceptance-test {:id "wm-loop-node-injury-d755c886-selection-retention"
                             :command "clojure -M:test -n futon2.aif.meta-loop-node-injury-acceptance-test"
                             :result :passed
                             :counts {:tests 4 :assertions 15 :failures 0 :errors 0}}
           :kondo :clean
           :record-sha256-verified "f6634b0452b8c69e7051041b58b1e936a2b176311058e8921c86fd753867dd3f"
           :regression {:namespaces ["futon2.aif.loop-node-exercise-test" "futon2.aif.wm.terminal-receipt-test"]
                        :result :passed}
           :unresolved ["full_loop_runner_test/selection_failure_retention_test show 329 failures/23 errors; baseline vs pre-change tree not established (live-stack namespaces); must be triaged before commit"
                        "changes not yet committed"]}]}