# Disabled initialization-close historical admission (single entry)

Binds the verified historical repair-verification for the last remaining open
non-environmental stop-line
`repair-initialization-8c13130f-bed1-4eac-b2f3-261243c119dc-initialization-failed`
(invalid close outcome / unknown-outcome; closure fix `0d1e203c`, zai-1 review
job `7cea2e60`). Historical actors codex-17 / zai-1; the eventual ordinary
U88 worker remains codex-20. No relabeling of any prior artifact.

## Receipts

- Qualification `repair-initialization-8c13130f-revalidation-20260911-v1`
  (`offline-evidence/…v1.qualification.edn`, sha256
  `48b522d5ada20351f5f6fdc95dfab39a86ed44cfccb5b51272884f763d0b4377`):
  exactly ONE check — the closure-regression var
  `authenticated-run4-pin-enters-normal-gated-runner-path` over an activated
  cohort (the `0d1e203c` regression: `:guardrail-refusal` outcome through
  `cohort/closed-execution`) — exit 0, summary `{:pass 14 :fail 0 :error 0}`.
  Selected via `clojure.test/test-vars` (v3 mechanics; no namespace/suite).
- Real zai-1 audit: Agency job `invoke-1789157036919-20272-b3acad58`
  (read-only tool calls): APPROVE with
  `HISTORICAL_VERIFICATION_SHA256: 48b522d5…`, all 3 source pins and the
  manifest pin recomputed.
- Verification (`offline-verification/…v1.verification.edn`, sha256
  `bd2fb35fb0e9381a3e4c5ccba4d17abfb8c96d48783efc3c9ea29978ea3a98e1`),
  actors `{:author "codex-17" :reviewer "zai-1"}`, minted by
  `run-init-close-verifier.clj` at then-current HEAD `2bcefdd8` (guard
  accepts `2bcefdd8`/`bf10b3d9`, both descendants of the fix; pinned sources
  byte-identical; first-commit `810be2a9` = fix base).

## Identity

series/cohort `run4-initialization-close-admission-20260911-v1`, attempt
`initialization-close-historical-admission-001`, casting
`{:author "codex-17" :reviewer "zai-1" :repair-reviewer "zai-1"}`; pins:
cohort `2251c474…`, task-pin `ee7462a4…`, series-pin `c55d80c6…`. Stores
under `/home/joe/run4/initialization-close-admission/`. All four templates
disabled (`:enabled? false` / `:activation :not-performed`).

## Disposable lifecycle gate (verified run)

`run_init_close_packet_gate.clj` (clj-kondo clean; log retained in
`init-close-gate-2026-09-11.log`): recreates the verification through the
real verifier with the committed qualification bytes and the REAL zai-1 job
fetched live, descendant HEAD guard, codex-17/zai-1 roster; activates the
cohort, materializes `server-config.disabled.edn`, `validate-applicable!`
→ the exact repair-id, one queue tick → `:held` /
`:terminal-evidence-incomplete`, binding `:verified` /
`:historical-verification-awaiting-validation`. Process exit 0; the gate's
own result map (label `run4-init-close-packet-gate`, review-job-id
`invoke-1789157036919-20272-b3acad58`) is the last line of the log.

## Coordinator-owned private install (single dispatch)

Materialize `server-config.disabled.edn` with the server credential and
mission/admissibility ports; attach the enabled historical action
(verification `bd2fb35f…`) and the four-key execution-cohort binding
(`2251c474…`, data-root `/home/joe/run4/initialization-close-admission/cohort`);
construct the single queue entry from `queue.disabled.edn`; call
`futon3c.wm.run4-series-queue/start!`. Admission yields awaiting-validation
only; init38690 and every prior packet/receipt (including the completed
pin-repair result 59ec8b1b and LIVE-RESULT) remain untouched.
