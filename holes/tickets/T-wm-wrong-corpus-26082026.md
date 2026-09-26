# T-wm-wrong-corpus-26082026 — a null result must name the corpus it is null about

**Status (triaged 2026-09-01): PARTIAL -- convention adopted, not enforced.** Pinned-corpus phrasing is mandated in facts-R14 slice constraints and practiced in C102/C105. But no schema requires a corpus path on recorded null results, and the ambiguity source stands: wm_scheduled_run.clj:108 enacts (close-loop!) while wm_outer_loop.clj does not, and the two archive names still do not say which. Close by documenting the two runner archives and/or requiring the corpus-path field.

Parent: `E-R8-red-ring-fill.md`. Found 2026-08-26 by claude-13, independently by
claude-19 the same day, and — the reason this is a ticket and not an anecdote —
by R8's own promotion note a day earlier, which recorded checking the archive
rather than assuming and checked the wrong one. Status: open, no owner.

## The finding

Three parties drew a null result from a directory and stated it about the stack.

| party | claim | corpus searched | corpus that held the data |
|---|---|---|---|
| claude-13 | "zero realized outcomes across 62 attempts" | `futon2/data/wm-full-loop/` (the runner that does **not** enact) | `futon2/data/wm-trace/` — **88 outcomes**, 2026-07-02..07-06 |
| claude-19 | "zero cascade artifacts anywhere" | the campaign directories | the round-1 conductor path — cascades ran in f9/f10/f13/f15 |
| R8 promotion note | *"the archive has no mismatch to freeze"* | `wm-outer-loop-*` | `wm-trace/` — 77 parseable mismatches |

R8's note carries the line *"(Corrected 2026-08-25 after checking the archive
rather than assuming it.)"* — so **care was applied and did not help.** That is
the point of the ticket: the fix is not diligence.

## Why it happens here specifically

`futon2` has two runners writing two archives. `scripts/wm_outer_loop.clj` reads
traces and does not enact; `scripts/wm_scheduled_run.clj:108` calls
`enact/close-loop!` and writes `data/wm-trace/`. Nothing in either directory name
says which is which, and the enacting one is not the one named after the loop.

## Proposed requirement

A recorded null result must carry the corpus it was drawn from, as a path, in
the same artefact that states it. A note saying "zero X" without a path is not
a finding.

Cheap enforcement: when a note or receipt records a zero count, require an
accompanying `:corpus` / "searched:" line with a filesystem path or query. This
is a writing discipline, not a code change — but it is checkable by grep, and
`gen-wip-cards.py` could surface violations.

## Cross-references

- `futon2/holes/NOTE-step9-reachability.md` — the two-runner finding.
- `p4ng/empirics-futon/promotion-tests.edn` — R8's `:retro-trip`, to be corrected.
- V3 §4a carries this as a silence instance (claude-19, 2026-08-26).
