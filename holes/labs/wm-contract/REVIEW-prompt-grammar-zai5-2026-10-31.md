# Independent review — diagnosis prompt grammar fix (zai-5)

Reviewed: futon3c `8d8ac543fe0d23ca3ea9a68b66ae076d61d4076f` on
`review/p4-diagnosis-prompt-grammar` (base `d7963bc97`, ancestor-verified).
Author: zai-4 (dispatch codex-11 8640). Reviewer: zai-5, who diagnosed the
original missing-grammar defect (futon2 `3bbfeabe7`).

## Verdict: APPROVE-SHA `8d8ac543fe0d23ca3ea9a68b66ae076d61d4076f`

Scoped fix ready for integration, subject to the re-release checks listed
below. No material correction required.

## What I verified directly

1. **Grammar fidelity.** The prompt's inline grammar matches the real
   `futon2.aif.cause-scope/predicate?` exactly: map-only; ops exactly
   `#{:result-path-equals :result-path-present :result-path-absent}`;
   vector `:path`; `:value` required for equals via `contains?` (so nil is
   a valid value and a missing `:value` is refused); extra keys tolerated
   by the existing open-map verifier — and the test *documents* that
   tolerance rather than hiding it.
2. **Return-value vs original-record boundary.** `assert-repair` resolves
   the boundary, applies it to the pinned retained carrier, and evaluates
   `get-in` on the **return value**. The prompt states precisely this
   ("RETURN VALUE of the named boundary applied to the pinned retained
   carrier … NOT on the original record"). The present/absent vs nil/false
   distinction in the prompt matches the `::absent` sentinel in `healthy?`.
3. **No canned predicate, no conversion.** The prompt adds a no-prescription
   sentence; the test asserts `:path [:failure]` does NOT appear. No code
   converts the old fn-literal string; the test proves the actual
   `wm-click-0930ccf1` response string `"(fn [record] (nil? (:failure record)))"`
   is refused by the real verifier. No invented healthy semantics.
4. **Scope.** Commit touches exactly 3 files: the one prompt line in
   `meta_self_heal_adapters.clj`, the test namespace (new tests only,
   existing tests untouched), and the in-commit packet doc. Verifier,
   authority and acceptance machinery untouched; `run-opportunity!` not
   invoked.
5. **Independent test rerun** (isolated worktree, correct invocation
   `clojure -M:test -n futon3c.wm.meta-self-heal-adapters-test`; the
   `:test` alias's main-opts `-m cognitect.test-runner -e :slow` makes `-n
   <ns>` the narrow path — verified in deps.edn first, no `-M:test -e`):
   **52 tests / 419 assertions, 0 failures, 0 errors** — matches the
   author's claim. (Rate-limit log lines are from pre-existing stub-path
   tests, all green.)

## Accidental broad scan — honest accounting

The author reports one accidental `clojure -M:test -e '…'` invocation that
started the alias main-opts and was killed during startup, erroring on
missing namespace `futon2.aif.outer-cascade`. What is actually proved:
- **Consistent:** no `outer_cascade.clj` exists in futon2 src or test
  (checked); the worktree `git status` is clean — no test debris or
  modified files observed.
- **NOT independently evidenced:** no retained log or process capture of
  the killed scan exists in the worktree. "No tests executed / no file
  effects" rests on the author's report plus the clean tree; I could not
  confirm it from retained evidence. Given the diff is prompt-only and
  protected-state pins are elsewhere, this does not block the fix; it is an
  accuracy note, not a new blocker.

## Passing-grammar ≠ real diagnosis validity

These tests prove prompt/verifier agreement only. A real future diagnosis
actually returning a justified map under this prompt is **not yet
witnessed** — that witness belongs to the re-release, not this review.

## Remaining real re-release checks (not owned here)

1. Merge/reload of the prompt fix into the serving path (branch only today).
2. Re-release of the P4 diagnosis opportunity with the corrected prompt,
   and observation that the seat returns a valid map or a typed
   `:cause-scope-unretained` refusal — the first real-world witness.
3. Charge/attempt accounting on the re-release (13 credits remain, attempt
   unspent — unchanged by this review; no source repair beyond the prompt
   was authorized or performed).

No merge, reload, click, contact, live write or ledger edit performed by
this review. Review branch only.
