# Zai staffing transition — successor-v2-selection historical revalidation (2026-09-11)

Author: zai-2 (job invoke-1789148820027-20225-df620e28). Read-only preparation
only: no runtime source changes, no live capacity/admission/steps, no edits to
any file owned by the codex-12 packet-review job
(invoke-1789148567297-20222-5e86c8aa). The held initialization queue (e348efc5),
all receipts, and the completed `initialization-collision-admission-v1` are
untouched.

## Question

Can the next successor-v2-selection execution be truthfully staffed zai-2
(author) with independent zai-1 (reviewer) under the current
selector/materializer/store contract, while the retained historical verifier
`99b0fed181c7142a4d498abd08e9bb7dcf6247ec4decf3f41d6434a1724c31c6` keeps its
actual actors codex-10 (author) / codex-12 (reviewer)?

## Answer

**Not with the retained artifact; yes with a fresh zai-actors verification.**
Actor binding is by bytes, not by configuration:

- `futon2.aif.repair-obligation/admission-from!` copies `:actors` verbatim out
  of the verification artifact, whose identity is its sha256 (`99b0f…`). The
  artifact is immutable; relabeling its actors changes the sha and is refused
  everywhere downstream.
- `futon2.aif.full-loop-runner/historical-revalidation-entry` (the selector)
  requires `(= (get-in admission [:actors :author]) (:author casting))`,
  `(= (get-in admission [:actors :reviewer]) (:repair-reviewer casting))`, and
  author ≠ reviewer. Exact equality both ways: codex casting cannot select a
  zai-actors admission, and zai casting cannot select the `99b0f…` admission.
- When zai casting makes historical selection return nil, the runner falls
  through to the ordinary `repair-entry` — which this stop-line's authority
  does not permit. That fall-through is the trap; it is not a staffing path.
- `futon3c.wm.run4-historical-verification/admit!` — the materializer —
  accepts ANY distinct real agent ids: it requires only
  `(= reviewer (:agent-id review-job))` and `(not= author (:agent-id review-job))`
  plus a genuinely executed review job (state done, `FULL_LOOP_REVIEW: APPROVE`,
  `HISTORICAL_VERIFICATION_SHA256: <qualification-sha256>` marker,
  `:execution {:executed true …}` via
  `full-loop-runner/independent-review-evidence`). Nothing in the store,
  materializer, or selector binds actors to Codex lineage.

## Consumer check (disposable root, exit 0)

Script: `/tmp/zai-staffing-consumer-check.clj` (deleted root afterwards; not
committed — recreated verbatim below is unnecessary since every input it used
is named here). It cloned futon2 `--shared --no-checkout` at the pinned source
HEAD `810be2a9a19d70b054d9ef7ceb43a2349b7a923d`, copied the canonical finding
(sha-verified against the retained artifact), minted a fresh qualification
(`qualification/produce!`, plan schema `:wm/historical-qualification-plan-v1`,
check `:current-execution-authority` → `/bin/true`), then ran the real
`admit!` with author `zai-2`, reviewer `zai-1`, and a review job whose
`agent-id` is `zai-1`, then exercised the real consumer chain
`historical-verification-candidate` → `historical-revalidation-entry`:

```edn
{:immutable-artifact-sha-verified true
 :zai-admitted {:actors {:author "zai-2" :reviewer "zai-1"}
                :review {:job-id "disposable-zai-review-double" :verdict :approve}}
 :zai-candidate-actors {:author "zai-2" :reviewer "zai-1"}
 :zai-casting-selects-historical? true
 :codex-casting-on-zai-admission-selects? false
 :original-99b0f-actors {:author "codex-10" :reviewer "codex-12"}
 :zai-casting-selects-original-99b0f? false}
```

The review job was a **synthetic double** of an Agency job map. The contract
under test only checks the job's `agent-id`, verdict, execution evidence, and
the qualification-sha marker. A truthful production run must substitute the
**real** Agency job from an actual zai-1 dispatch; passing a forged map then
would be lying, not staffing.

## Exact fresh inputs a zai-staffed run needs (nothing smaller suffices)

1. **New verification-id**, e.g.
   `repair-successor-v2-selection-revalidation-zai-20260911-v1`, minted under a
   new packet directory (do not touch
   `RUN4-repair-successor-v2-selection-revalidation-2026-09-11/offline-verification/`).
2. **Qualification plan + output** via `run4-historical-qualification/produce!`
   pinning the three selector/materializer/store sources and check ids at the
   source HEAD (the check command population must be independently reviewed —
   it is executed for real).
3. **Real executed zai-1 review job**: bell zai-1 with the finding and
   qualification paths; its persisted result must open with
   `FULL_LOOP_REVIEW: APPROVE` and contain
   `HISTORICAL_VERIFICATION_SHA256: <fresh qualification sha256>`; the job map
   read back through `review-job-reader` must carry `:agent-id "zai-1"` and
   real execution evidence. This is the one genuinely new *executed* Zai act —
   it cannot be replaced by relabeling, token commits, or the synthetic double.
4. **`admit!` invocation** with `:author "zai-2" :reviewer "zai-1"` and the real
   job reader, producing the new `.verification.edn` under the new packet's
   offline-verification root.
5. **Cohort/config recast**: a new cohort.edn and server-config/historical-action
   template with `:casting {:author "zai-2" :reviewer "zai-1"
   :repair-reviewer "zai-1"}`, the new verification path/sha256, and a new
   queue entry — following the existing disabled-template pattern
   (`server-config.disabled.edn`, `historical-action.disabled.edn`,
   `queue.disabled.edn`) in a NEW packet directory, gated by independent review
   before any private installation (materialize → attach action + four-key
   execution-cohort → `futon3c.wm.run4-series-queue/start!`).
6. **Roster precondition**: at execution the runner roster must show `zai-2`
   and `zai-1` idle and invoke-ready (the roundtrip test injects exactly this
   shape for the cast agents); zai-1/zai-2 were idle invoke-ready at the fresh
   roster check cited in the dispatch.

## Executable next step (after packet gates and Joe's authorization)

Dispatch zai-1 with the review request for the fresh qualification (input 3),
then run `admit!` (input 4) and assemble the new disabled packet (input 5).
No command here restarts, resets, or resubmits anything held: the
initialization queue e348efc5 stays consumed, and `99b0f…` stays retained with
its true codex-10/codex-12 actors — selectable again only by a future
codex-cast execution, never by relabeling.

## Validation performed

- Consumer check above: process exit 0, assertions embedded in the result map.
- Immutable artifact sha re-verified in the same run (`99b0f…` unchanged).
- `git status` before commit: no runtime source or packet file modified by this
  job; only this note is added.
