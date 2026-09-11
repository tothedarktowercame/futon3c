# Disabled first-repair (pinned-selection-refused) historical admission

Single frozen entry binding the verified historical repair-verification for
`repair-ea1-78cd8a42e23392a5ce1412e49d181c3e7c3ac95aaa40d4f54c5c9d8ef3ae08eb--attempt-001-pinned-selection-refused`
(the U88 click `wm-click-a6856c5f…` pinned-selection refusal, subtype
`:pinned-action-not-candidate` retained in the durable finding). Historical
authorship codex-17 (qualification author) / zai-1 (independent reviewer); no
relabeling to codex-20, who is intended INSIDE the eventual ordinary U88 run.

## Receipts

- Qualification `repair-ea1-78cd8a42-revalidation-20260911-v3`
  (`offline-evidence/…v3.qualification.edn`, sha256
  `629ae20a3530ce4cacd340b960e9fdd91e0d434f6cc1315e556da40dc67c289f`):
  the exact two focused vars for repair `bf10b3d9`
  (`authenticated-run4-pin-enters-normal-gated-runner-path`,
  `pinned-mission-identity-retains-real-proposer-action`), each exit 0,
  summary `{:pass 23 :fail 0 :error 0}` (matches the coordinator's
  2 tests / 23 assertions). v1/v2 outputs are retained untouched as receipts
  of two producer input defects (wrong cognitect `-v` selection; nonexistent
  `summarize-results` var) — never admitted, never reviewed.
- Independent zai-1 audit: Agency job `invoke-1789156072050-20265-61d62304`
  (done, real tool calls): APPROVE with
  `HISTORICAL_VERIFICATION_SHA256: 629ae20a…`.
- Verification (`offline-verification/…v3.verification.edn`, sha256
  `015b6f6a0a219e6f195553d5e5b02176d2c1edb86db38a2f835c26c1295c75d0`),
  minted by `run-first-repair-verifier.clj` at then-current explicit
  descendant HEAD `2bcefdd8` (docs-only descendant of `bf10b3d9`; pinned
  sources byte-identical; first-commit `0d1e203c`). The first verifier
  attempt refused on HEAD movement `bf10b3d9→2bcefdd8` by design; input was
  explicitly refreshed, not bypassed.

## Identity

series/cohort `run4-repair-pinned-selection-admission-20260911-v1`, attempt
`repair-pinned-selection-historical-admission-001`, casting
`{:author "codex-17" :reviewer "zai-1" :repair-reviewer "zai-1"}`,
cohort sha `efa8758f…`, series-pin sha `b3a61741…`, task-pin sha
`edf16e60…`. Stores under `/home/joe/run4/repair-pinned-selection-admission/`.
All templates disabled (`:enabled? false` / `:activation :not-performed`).

## Disposable lifecycle gate

`run_first_repair_packet_gate.clj` (clj-kondo clean; log retained in
`first-repair-gate-2026-09-11.log`): recreates the verification through the
real verifier with the committed qualification bytes and the REAL zai-1 job
fetched live, dynamic descendant HEAD, codex-17/zai-1 roster, cohort
activation, materialized config, `validate-applicable!` → exact repair-id,
one queue tick: `:held` / `:terminal-evidence-incomplete`, binding
`:verified` / `:historical-verification-awaiting-validation`. Passed,
exit 0.

## Coordinator-owned private install (single dispatch)

Materialize `server-config.disabled.edn` with the server credential and
mission/admissibility ports; attach the enabled historical action and
four-key execution-cohort binding from `execution-cohort.disabled.edn`;
construct the single queue entry from `queue.disabled.edn`; call
`futon3c.wm.run4-series-queue/start!`. Admission yields awaiting-validation
only; the separate stop-lines (initialization 8c13130f, init38690) and all
prior packets/receipts remain open and untouched.
