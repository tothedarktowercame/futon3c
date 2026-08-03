# Pull-side receipts — recording the offered half of the pull channel

Opened 2026-08-03 by claude-12, on Joe's instruction. **Recording-side only:
this changes what is written, never what the system does.** No retrieval,
ranking, tool-availability or runner-facing change. That boundary is what makes
it shippable during a measurement programme.

## Why this is the fundamental

The V3 regime is: Zai runners solve problems → we check whether memories were
used appropriately → new memories get built → storage is reshaped. Step two is
currently impossible for the pull channel.

`use-receipt` (`futon2/src/futon2/aif/memory_contract.clj`) enforces
`used ⊆ surfaced`, where `surfaced` is the **dispatch-time offered set** — push
only. A runner that finds a memory mid-session via `memory_search` and uses it
produces a `USED` line for an id that was never "surfaced" by that definition.
So today we can count pull **uses** and have no record of pull **offers**.

That is a numerator without a denominator — V2 §2.3.2's exact failure
(*"we know what surfaced, not what was findable"*), which is the methodological
finding V2 is largely about. Building the `:push+pull` and `:pull-only` arms
without this means running arms whose effect cannot be measured.

claude-10's `cohort-guard` (`47166e23`) already refuses a `:push+pull` dispatch
that lacks `pull-surfaced-ids`, naming that exact field. **Use that field name.**

## The blocking discovery: there is no dispatch identity in the runner's tool path

Verified 2026-08-03 by reading the plumbing:

- Every `memory-backend/*` call in `zai_api.clj` (lines 467–520) receives only
  `{:agent-id … :session-id … :cwd …}`. No job-id.
- `make-invoke-fn` (line 1108) is constructed **once per seat**, closing over a
  `session-id-atom`. It is not per-dispatch.
- `session-id` is **per-seat, not per-dispatch** — measured today: zai-1's id is
  unchanged across S3 (a96J01) and S6 (a96A04); four distinct `turn-id`s and two
  problems commingle under one session-id.
- `turn-id` *is* threaded into the round context (`:turn-id (:turn-id ctx)`, used
  by `persist-round!`) and is the finest available discriminator. codex-2 used
  turn-ids to separate S3 from S6 when recovering the scribe corpus.

**So a pull event captured at the tool site today cannot be attributed to the
dispatch that caused it.** Fix this first; everything else is downstream.

## Build

### 1. A per-dispatch identifier must reach the tool-call site

Preferred: the Agency knows the job-id when it invokes; carry it to the invoke
call and thread it into the backend ctx as `:dispatch-id`. Check whether the
invoke protocol already passes it — if it does, this is threading, not
redesign.

Fallback if it genuinely cannot be carried: record `turn-id` on every pull
event **and** write one explicit dispatch→turn binding at invoke start, so the
join is recorded rather than inferred. State which route you took and why.

Do **not** use `session-id` as the dispatch key. It is measurably wrong.

### 2. A pull-offered receipt per memory-family tool call

On every `memory_search` / `pattern_memory` / `library_search` /
`evidence_graph` / `psr_search` call, append an evidence entry recording:

- the ids returned (the pull-offered set for that call),
- the tool name, the query/args, the round number,
- `:dispatch-id` (or turn-id + binding, per §1), agent-id, session-id,
- timestamp.

Per-call, not aggregated at end of run: **when** in the run a pull happened is
the whole point of the timing hypothesis (push fires at minimum information,
pull at maximum). An end-of-run aggregate destroys exactly the signal the arm
exists to measure.

Append-only, through the existing boundary (`evidence/boundary.clj`), so the
`:ok`/violation discipline and the cache postcondition apply as they do
everywhere else.

### 3. `pull-surfaced-ids` derivable per dispatch

Provide the accessor the guard and the sweeper need: given a dispatch id, the
union of pull-offered ids across that dispatch's calls.

### 4. The attribution path must union push and pull

`scripts/memory_outcome_sweeper.py` reads `offered_surfaced_ids(job_id)` (push
only) and hands it to `runner_gate`. That set must become
**push-surfaced ∪ pull-surfaced**.

**Do NOT weaken `use-receipt`.** `used ⊆ surfaced` is integrity machinery and
stays exactly as it is; the repair is that `surfaced` becomes complete, not that
the check becomes lenient. If you find yourself editing `memory_contract.clj`,
stop and report — that is a contract decision, not a fix.

## Acceptance

- A test replaying a realistic session: two `memory_search` calls returning
  overlapping id sets, one used by the runner. Assert the union is derivable,
  the runner's `USED` line for a pull-sourced id passes attribution, and the
  per-call rounds are distinguishable.
- A test that two dispatches on the **same seat** (the S3/S6 shape — one
  session-id, different turn-ids) do not commingle their pull-offered sets.
  This is the regression for today's finding; it must fail against the current
  code.
- `cohort-guard` licenses a `:push+pull` dispatch record once the field is
  populated, and still refuses when it is not.
- Explicit confirmation that no retrieval, ranking, tool-availability or
  runner-visible behaviour changed. Say what you checked.

## Gates

`clj-kondo` 0/0 on Clojure touched; `futon4/dev/check-parens.el`; existing
suites green; no serving-JVM reload. Note pre-existing failures by reproducing
them at clean HEAD in a detached worktree before claiming no regression — both
codex-5 and codex-6 did this today and it is now house practice.
