# Voxterm loop recovery — 2026-09-09

Author: codex-17, requested by Joe through Voxterm/Emacs.

## Completed repairs

- Voxterm `c1f9ede`: discover Codex models when the picker opens. The installed CLI cache already offered `gpt-6-astra`; the server and browser cached an older list. The live `/agency/runtimes` response now offers Astra. Two launcher regression tests pass.
- Voxterm `0b7dbcc`: parse queue EDN structurally, projecting only top-level parked records. Nearby nested reports previously mislabeled f200's reason and showed refuted f205 as a park in place of f202. Regression includes a 5,000-character nested report and a later misleading frame. Cache invalidation and visible parse failure are tested. Four Python tests total pass; JavaScript syntax, clj-kondo 0/0, check-parens and diff whitespace pass. Only the Voxterm Python service was restarted.
- apm-lean `a9bc248c`: committed the finished f205-guide statement repair. The old existential uniqueness over total functions on R was false because the hypotheses constrain only the nonnegative domain. The replacement asserts uniqueness on that domain. Independently compiled: exit 0, one existing sorry warning and the existing simpa suggestion. This repairs the statement, not its unfinished proof.
- futon3c `7195f7d0`: shared pin eligibility validation between the queue and guide observer, with unchanged problem identity required. The guide prompt now specifies the required pin fields and committing the source. The malformed f205 reply had valid receipt fields but omitted replacement pins, which produced repeated `:queue-problem-ineligible` failures. Adapter tests: 37/178, queue tests: 37/220, all passing. clj-kondo 0/0, check-parens and diff whitespace pass. Reloaded canonical master namespaces into the existing JVM.

## APM recovery evidence

The original guide job is `statement-repair-85226c8d61f815982bcd03123e35c2105ae9ea5a5abcd2b6fefdddaca1c81256`, terminal `done`. Its guide receipt and exact returned statement were preserved. The committed source was checked to contain that exact statement through `:= by` before supplying the missing committed source pins. See [pin-completion receipt](2026-09-09-m02A06-pin-completion.edn).

With the coordinator disabled and stopped and no tick claim, used `revise-voided-slot`, `persist-launch-plan!`, and the normal atomic state writer. No raw content-address editing. The repaired queue id is `4fa67e40c1a4b4303eb552bbd2cbf2111f6f87d5f9d504436afee3536133b524`; cursor 35 and frame ordinal 37 held, and m02A06 consumed exactly its one repair attempt. Resumed with `durable-coordinator/resume!`; epoch 22, f206/m02A06 entered preflight and produced a certified preflight state. At tick 44797 the regulator is running and the latest result is successful. No futon3c JVM restart. A later live check shows f206/m02A06 in `solve`.

## Topology recovery evidence

Topology stopped at 07:22 UTC after waiting 900 seconds for the guide's uncommitted m02A06 source. The worktree guard was correct. Once that finished repair was validated and committed, the validator passed: 326 items, 317 done, 1 open, 2 blocked, 6 superseded.

Resumed the existing supervisor using user service `topology-supervisor-recovery-20260909.service`, reviewer codex-10 as in its prior run. The loop resolved the missing codex-18 owner through its existing registry logic to codex-1, preserving author/reviewer separation. At 11:25 UTC it dispatched `critical-value-sard-zero-dimensional-borel-critical-image-1` to codex-6, job `invoke-1788953114277-16189-84ce767f`. Live Voxterm reports topology running. No guard or mathematical prerequisite was weakened. By 11:28 UTC the restarted loop had completed and independently reviewed that prerequisite (`fbfdf14a`, acceptance transition `06b419ee`), increasing done to 318 and reducing blocked to 1. It dispatched the newly unblocked `critical-value-sard-rectangular-local-probe-3`, job `invoke-1788953321954-16195-074a3f06`.

## Two historical park records still require disposition

- f200/m01J06: `:solver-session-mismatch`, expected session `01a08287-25de-7dc3-80f8-d6ca7f6c11a6`, actual nil. The existing fix `78226361` distinguishes absent session evidence on failed jobs from genuine concrete session drift.
- f202/m02A03: `:solver-remediation-required`. Rounds 23 and 24 failed before executing any tools because Codex refused to resume thread `01a08383-947f-7b11-be4f-0dc777fe5f6c` with an active writer. Durable job ids: `apm-role-ca1000194250b9388b1ebf2a4c685df00049863b89b5dc7781fa5bf43d57cf7c` and `apm-role-4ee1f31b83817db20f86feca97159227b14bc0992e5125d53f4e546cdb67be14`. Earlier typed submissions could finish a round while its CLI still held the writer. Existing `076cbb6e` waits for the prior job to terminate; `a6967bba` supplies the required job observer on the remediation-resume path. Neither failure establishes a mathematical refutation.

Re-ran `live-solver-rounds-test`: 34 tests, 149 assertions, zero failures/errors. Reloaded phase-status, live-solver-rounds and live-proof-phases from canonical master so the running process uses these fixes. Also re-ran `live-proof-phases-test`: 18 tests, 52 assertions, zero failures/errors. A control against the loaded JVM passed a running prior job to the dispatch boundary with throw-on-dispatch callbacks: it returned `:awaiting-prior-job-terminal`, preserved state, and did not dispatch. These are pre-existing commits, not new repairs claimed by this note.

The queue assigns both historical park dispositions to `:claude-supervisor`. No disposition has been impersonated, no retired frame requeued, and no residual cleared. Reviewer should use the preserved park records and decide whether to retain partial work or authorize a separate continuation; the forward campaign now runs independently of these decisions.
