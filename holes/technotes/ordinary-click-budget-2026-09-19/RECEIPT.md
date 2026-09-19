# Ordinary-click issue ledger — author receipt, 2026-09-19

Author: codex-34. Reviewer: pending (claude-4). Subject: `OPS-ordinary-run/click`.
Implementation: futon3c `dc7cc2e4`; client/reconciliation: futon2 `41f6b7a8`.
Not live-loaded. No production clicks issued.

The plain HTTP branch supplies an issue callback to the single-flight service.
After claiming the service slot and before starting the worker, it counts and
appends consumption under JVM and file locks, forces the file and its directory,
and forces the directory's parent. A failed worker does not refund consumption.
R10 and prepared RUN4 branches retain their existing authority. Missing caller
is recorded as `caller-unknown`; it does not refuse. Exhaustion returns HTTP 409,
`ordinary-click-budget-exhausted`, the AUTH path and `d18e4f9c`, and Joe as renewal.
The grant remains five; issuing another grant is not automated here.

Machine-written ledger: `futon2/data/wm-ordinary-clicks/consumption.jsonl`.
`wm_click.sh --issuing-caller NAME` (or `WM_ISSUING_CALLER`) supplies attribution.
The script accepts no RUN4 pin/attempt options, so there is no RUN4 omission
combination to add a client check for. The already-reported endpoint omission
behavior is unchanged, per the owner's P-0d ruling.

## Executed evidence

- Budget tests: 4 tests, 34 assertions, zero failures/errors. Five real service
  workers invoked the injected runner function, which read the ledger and then
  threw immediately. Worker-entry ledger counts were `[1 2 3 4 5]`; five charges
  survived five failures. Sixth issue returned 409 without a sixth worker.
- Concurrent ledger control: 12 issue attempts, 5 accepted and 7 exhausted.
- Caller supplied/unknown, busy-no-charge, and R10 exclusion controls passed.
- Production file counts before/after the hermetic failure test:
  repair obligations 200/200; trip reports 293/293; ordinary ledger 0/0.
  The canonical `with-hermetic-stores` fixture additionally compares file sets.
- Runner service: 14 tests, 114 assertions, zero failures/errors after stubbing
  the cohort port in its unit fixture. The first run's 20 timing failures came
  from the live cohort resolver delaying worker entry; production code was not
  weakened. The slow chain fixture now also stubs that unrelated port.
- RUN4 HTTP boundary: 4 tests, 29 assertions, zero failures/errors, including
  absence of the issue callback on the admitted RUN4 branch.
- Reconciliation script over temporary files: one issue without run record
  returned exit 2 and named the missing record; adding the matching run record
  returned exit 0. Real read-only report: allocated 5, consumed 0, remaining 5.
- clj-kondo: zero errors/warnings on all touched Clojure files. check-parens:
  OK on all touched Clojure and deps.edn. `bash -n` on wm_click.sh passed.

## Validation blockers — do not call this accepted

Registry execution of the budget namespace passed (4/34), but returned
`warrant? false`, `scope-not-committed`: another lane's dirty
`futon2/src/futon2/aif/cascade_model_manifest.clj` was in the actual load closure.
Refused run record:
`test-registry-c69703327de797ab9c43ef5750a900e478dd1940b438a26fa7cfbbbfef5eca8c`.
This is NOT an acceptance warrant and has not been bound as one.
Execution receipt is in the content-addressed registry log; original artifact:
`storage/test-registry/ordinary-click-budget/4c139264-c651-4a3d-972e-58dcefd8a329.log`.

The touched slow chain rehearsal ran explicitly: 1 test, 18 assertions,
2 failures. Its positive certificate fails `topology-pin-valid?` because the
current SVG and edge-data hashes differ from its expected pins. No detector or
pin was relaxed. The negative mismatch control still rejects. Its budget
fixture protection is committed so running this test never spends Joe's grant.

After the other lane commits its source, rerun the registry config here and
bind the successful warrant via `futon3c.test-registry.validation/bind-subject!`.
Runner-service and RUN4 boundary tests still need their registry runs. The
chain rehearsal needs an independently justified topology/pin repair before
all touched-test gates can be called green.
