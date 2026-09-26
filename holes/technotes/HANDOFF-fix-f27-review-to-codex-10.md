# Handoff: integrate `fix/f27-review` into master

From: Claude (Opus 5), 2026-08-24. For: codex-10. Context: `TN-opus-f27-review.md`.

## What it is

Branch `fix/f27-review`, worktree `/home/joe/code/futon3c-opus-f27`, four
commits on top of master `6a35f952`:

```
4142a0aa TN status
4faf7677 Review Guide store-mode deposits and bind the Student to the union snapshot
f1f8e568 TN
409569e4 Reset, archive, and pattern-gate the f27 student/promotion plumbing
```

`git diff --stat master..fix/f27-review`: 7 src files, 5 test files, 1 role
card, 1 technote. Src touched:

- `apm/workspace_lifecycle.clj` — new `reset-to-base!`, `archive-problem-source!`
- `apm/live_learning_phases.clj` — student reset/archive; guide `:candidates`
  validation; guide review stage (`guide-promotion-step!`); student binds to
  latest snapshot; requests carry `:base-revision`/`:problem-path`
- `apm/countdown_control.clj` — `publish-guide-promotion!`,
  `guide-review-state-path`, guide branch in `drive-live-learning-phase!`,
  snapshot verification from latest receipt, projection of the review job
- `apm/promotion_pipeline.clj` — `:candidate-patterns-missing` gate,
  `validate-guide-deposit`, `validate-review*` core
- `apm/live_promotion.clj` — `:review-pending` stage; deposit prompt wording
- `apm/live_job_driver.clj` — receipt provider may return
  `{:ok true :status :awaiting-terminal}` to defer certification
- `apm/frame_cycle_handlers.clj` — `latest-snapshot-receipt`,
  `snapshot-binding`, guide-snapshot evidence check
- `apm/queued_frame_adapter.clj` — guide card path → `claude-guide-v2.2.md`

## Steps

1. `git rebase master` on the branch (or merge — your call; rebase preferred
   so the four commits stay readable). Expect conflicts only in
   `live_learning_phases.clj` / `countdown_control.clj` if you have touched
   them since `6a35f952`. Nothing on the branch depends on JVM state.
2. Gates, in the branch's own process — **not** against :6768:
   - `clj-kondo --lint src/futon3c/apm test/futon3c/apm` → 0 errors
   - `emacs -Q --batch -l /home/joe/code/futon4/dev/check-parens.el --eval "(arxana-check-parens-cli)" -- --no-defaults <changed files>` → OK
   - `clojure -M:test -n futon3c.apm.promotion-pipeline-test -n futon3c.apm.live-promotion-test -n futon3c.apm.live-job-driver-test -n futon3c.apm.frame-cycle-handlers-test -n futon3c.apm.live-learning-phases-test -n futon3c.apm.countdown-control-test -n futon3c.apm.learning-loop-dry-run-test -n futon3c.apm.live-proof-phases-test -n futon3c.apm.workspace-lifecycle-test -n futon3c.apm.queued-frame-adapter-test -n futon3c.apm.memory-snapshot-test -n futon3c.apm.frame-cycle-contract-v2-test -n futon3c.apm.generated-contract-test`
     → expected **144 tests / 605 assertions / 3 failures**, all three in
     `bank-handler-rejects-a-different-frames-verify-receipt`. That test
     fails identically on plain master (verified by stashing the branch's
     handler src+test and re-running). Pre-existing; leave it or fix it
     separately, but do not attribute it to this branch.
   - Full `clojure -M:test` was still running at the time of writing; run it.
3. Fast-forward master, then reload into the serving JVM **from
   `/home/joe/code/futon3c` only** (`(require 'ns :reload)` over Drawbridge for
   the seven namespaces above, or `scripts/restore-http-routes.sh` if routes
   are affected — they are not). Never `load-file` the worktree copy.
4. Remove the worktree when merged: `git worktree remove /home/joe/code/futon3c-opus-f27`.

## Behaviour changes to know about

- A persisted Student request without `:base-revision` now **fails closed at
  activation** (`:student-workspace-base-unknown`). Any campaign prepared on
  older code and still in flight must be re-prepared, not patched.
- Each fresh Student attempt resets the Student worktree to base
  (`git reset --hard` + `git clean -fd`; `.lake` link survives). The problem
  file is archived first to `live/student-attempt-N-source/<blob>-Main.lean`
  and the receipt carries `:receipt/source`.
- Scribe deposits with an empty `:pattern-ids` are rejected at the deposit
  gate with one bounded schema repair.
- In store-mode, a Guide report with `:candidates` triggers an independent
  promotion-proctor review and a union snapshot before the Guide receipt is
  minted; the next Student attempt binds to that snapshot. The role card that
  tells the Guide to do this is `claude-guide-v2.2.md` (DRAFT). JIT campaigns
  pick it up via `queued-frame-adapter`; the per-frame one-off manifests
  (`f21/f22/f23-one-off-manifest-v1.edn`) still pin v2.1 by blob — **re-pin
  is Joe's call, not part of this integration.**

## Done when

Master carries the four commits (or their rebased equivalents), the gate
numbers above are reproduced on master, the JVM has reloaded from
`/home/joe/code/futon3c`, and you bell Joe back with the merge sha and the
test line.

## Addendum 2026-08-24T00:20Z (Opus, after seeing `b8c4503d` on the branch)

The branch has already been rebased onto master `7d9d4d70` and carries
`b8c4503d "Model and preserve fresh Student attempt boundaries"`: `reset-to-base!`
now stashes tracked+untracked state and pins it under
`refs/apm/preserved-student-attempts/<frame>/<problem>/<sha>` before the hard
reset, and the generated contract records the six new policy claims with a new
digest. Reviewed: sound, and a real improvement over file-only archiving.

One follow-up it needs: `live-learning-phases/prepare-student-workspace!` builds
the lease it passes to `reset-to-base!` as
`{:workspace/path … :base-revision … :problem/path …}` — no `:frame/id` /
`:problem/id` — so the preservation ref will currently read
`…/unknown-frame/unknown-problem/<sha>`. Add
`:frame/id (:frame-id request) :problem/id (:problem-id request)` to that map
(and to the expected lease in `fresh-student-attempt-resets-the-worktree-but-repairs-do-not`).
Not done from here: the worktree is now yours.

Full `clojure -M:test` on the branch (before `b8c4503d`): every failing or
erroring namespace is outside the branch's diff or environmental in a worktree
(hardcoded `/home/joe/code/futon3c/...` paths in `conductor_test`, gitignored
`data/apm-campaigns/...` fixtures in `countdown_readiness_test`, missing corpus
files, codex subprocess, HTTP). The only `futon3c.apm` failures on touched
code are the three in `bank-handler-rejects-a-different-frames-verify-receipt`
already noted as pre-existing on master.
