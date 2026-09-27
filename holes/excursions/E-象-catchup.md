# E-象-catchup — interpret the whole operator-turn corpus, and fold the labels into an approved core

Logged 2026-09-27 by claude-18, from a conversation with Joe. Not scheduled;
Joe: "hold this thought for another day".

## Where coverage stands (2026-09-27)

The harvested corpus (`storage/operator-turns/operator-turns-filtered.jsonl`)
has 12,880 operator turns from 2026-02-18 to 2026-09-23. 3,395 of them (26%)
have an analysis. That total combines two sources: the live records under
`~/.emacs-graph/session-turn-analysis/` and the historical blocks that
`scripts/turn_batch.py` built under `storage/operator-turns/batches/`.

| month | turns | analysed |
|---|---|---|
| Feb–Mar | 70 | 0 |
| Apr | 482 | 0 |
| May | 2,140 | 2 |
| Jun | 2,386 | 3 |
| Jul | 2,141 | 0 |
| Aug | 3,683 | 1,501 (41%) |
| Sep | 1,978 | 1,889 (96%) |

All of the coverage comes from the 2026-08-22..09-26 batch window plus live
capture. The earlier ~7,200 turns have almost no coverage.

Counting traps:
- `turn_id` is not unique. Counters like `claude-1-turn-1` restart with each
  session, and one id is reused across 22 rows. Match analyses to corpus rows
  by source text.
- Every Feb–May row has no `turn_id` at all. `turn_batch.py build` falls back
  to the row `id`, so a record still gets built.
- `turn_batch.py status` counts `pending` wrongly: it lists `.analysis.json`
  and `.candidates.json` files as records.

## The plan

Joe: "If we are already at 26% we might as well process the whole thing." So
the plan is to run 象 over every unanalysed epoch, not a sample. Use
`turn_batch.py build` with windows covering Feb–Aug 21, dispatch block by
block, and check each block before sending the next (see
batch-packets-invite-corner-cutting: one large job got scripted rather than
read). Since 2026-09-27, dispatch fails over from 象 (Kimi) to 象-sonnet when
one of them hits its usage limit (futon3c 9bf36803, 594fe15b), so a
multi-day run can continue through Kimi's 5-hour windows.

## The label problem: hierarchical, not flattened

The live set uses 22 intents, matching the `session-mode-turn-vocabulary`
core. The batch analyses use **634 distinct intent labels over 11,017
fragments**. The head overlaps the core (`report` 1,274, `propose` 1,095,
`explain` 912, `report-problem` 652, …). Below it are near-synonyms
(`direct`/`directive`, `request`/`ask-action`, `inform`/`report`) and a long
tail of one-offs (`propose-parallel-track`, `design-cross-check`,
`meta-observe`, `request-access`, …). Each batch labeller has drifted into its
own vocabulary.

Pinning the brief to the fixed vocabulary would stop the drift, but it would
also throw away the distinctions the tail is drawing. Joe's suggestion is
hierarchical modelling:

- **An approved core** of a small intent set, reviewed by Joe. It is the
  level every analysis is guaranteed to reach.
- **Labeller vocabularies below it.** Each seat or epoch keeps its own label
  set, and each label is mapped *into* a core intent (`propose-parallel-track`
  → `propose`, `directive` → `direct` → …). A mapping is itself a record: who
  proposed it and whether it is approved.
- Queries at the core level see the whole corpus in one vocabulary. Queries
  at a finer level keep the tail's distinctions. A label that recurs in the
  tail across labellers is a candidate for promotion into the core, just as
  pattern candidates become patterns (cascade-construction/lift-when-three-align).

With this in place, full-corpus processing doesn't depend on getting the
brief's vocabulary right first. Labels can be as varied as the labellers
make them, because the mapping into the core reconciles them afterwards.

## Open questions

- Is the core the existing 22, or does it get re-derived from the head of
  the 634?
- Does the mapping get written by a seat and approved by Joe, or reviewed by
  sampling (pilots-skip-full-review-cycles)?
- Pattern refs have the same shape (327 patterns cited, 105 of them five or
  more times). Does the same hierarchy apply to them, with patterns under
  families?

## Related

- E-convert-operator-turns-to-patterns.md: the interpreter seat and the brief.
- `scripts/turn_batch.py`, `scripts/turn_dispatch_reap.py`,
  `emacs/session-turn-analysis.el`.
