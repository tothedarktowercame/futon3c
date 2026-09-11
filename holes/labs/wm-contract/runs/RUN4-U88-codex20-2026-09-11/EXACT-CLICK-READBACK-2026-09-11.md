# Exact U88 click readback

Click `wm-click-a6856c5f-1526-4dc8-8271-341aea4beb67` is no longer running. Read-only live runner status identifies this exact click, outcome `:incomplete`, binding unavailable and run record absent. Observed run UUID: `51d557a2-9caa-4727-814a-3841850b6550`. Wrapper attempt: `initialization-8c13130f-bed1-4eac-b2f3-261243c119dc`.

The canonical cohort retains checkpoints 001–006, with selection through adjudication explicitly `:pinned-selection-refused` / not reached. No 007 close exists. These are positive non-dispatch records: Codex-20/Zai-1 assignments did not become worker executions in this attempt. No task implementation or review result is established.

The initialization finding at `futon2/data/wm-repair-obligations/findings/repair-initialization-8c13130f-bed1-4eac-b2f3-261243c119dc-initialization-failed.edn` records `invalid close outcome`, `{:errors [:unknown-outcome]}`. This is a secondary closure failure after selection refusal. The original selection subtype is not preserved in the checkpoint prefix; do not infer `pinned-action-not-candidate` or another particular subtype.

Actual production reader calls, without reload or mutation:

- `run4-terminal-evidence/read-terminal-evidence-bundle` using retained reservation identity and started marker: refuses `:invalid-click-run-binding`.
- `full-loop-cohort/closed-execution` using pinned cohort authority and `attempt-001`: refuses `:closed-execution-unavailable`.

No run-record, projection, recording or task terminal was found under the configured roots. Queue is held `:series-step-refused`, cursor zero, completed entries empty. There is no grounded U88 result, acceptance or historical repair resolution to report.

Next bounded implementation: reproduce the pinned-refusal path in disposable storage, preserve its exact reason through failure handling, and make the cohort close contract handle the legitimate refusal without success semantics. Then determine the actual selection/admission discrepancy before another run. This report neither closes the old cohort nor authorizes reusing capacity. No admission, reset, retry, worker contact or restart was performed.

## Captured source hashes

- `/home/joe/run4/U88-codex20-20260911/bindings/click-run-binding-wm-click-a6856c5f-1526-4dc8-8271-341aea4beb67.edn`: `41fa2bba3ce1152e9760718354d9e0fde2ebde75ee891fc994f41d6a4d6dca20`
- `/home/joe/run4/U88-codex20-20260911/cohort/run4-u88-codex20-20260911-v1/attempt-001/001-time-step.edn`: `dbb35ac1f124d2192a35eb1c454abb92ba683823b8507bc86c5d06268fa8ad2c`
- `/home/joe/run4/U88-codex20-20260911/cohort/run4-u88-codex20-20260911-v1/attempt-001/002-selection.edn`: `a3507ba5c1b3551762892d565d5cf36fd925aa539658db075f3147effc746f89`
- `/home/joe/run4/U88-codex20-20260911/cohort/run4-u88-codex20-20260911-v1/attempt-001/003-construction.edn`: `385bea6fb57b0a2ac4cbf45ac9324fbefed0f617e1564548b5e283b329f36ed0`
- `/home/joe/run4/U88-codex20-20260911/cohort/run4-u88-codex20-20260911-v1/attempt-001/004-dispatch.edn`: `857372a723a31459a7f9df9ccc00154303edae310ce2ade11570c9b87be4f831`
- `/home/joe/run4/U88-codex20-20260911/cohort/run4-u88-codex20-20260911-v1/attempt-001/005-build.edn`: `27da11d09c69a4bd2ba9f953f3872b7119c04de70d9259d0fa43908226c70f3b`
- `/home/joe/run4/U88-codex20-20260911/cohort/run4-u88-codex20-20260911-v1/attempt-001/006-adjudication.edn`: `20527b9f022b7822216ad644183ebdd4bc2072b18cce19a57fb00a3621c46348`
- `/home/joe/run4/U88-codex20-20260911/queue/queue-state.edn`: `55021f7453b4042fb851031e1cd91a5aa4127a63ffccadcd14669312513facde`
