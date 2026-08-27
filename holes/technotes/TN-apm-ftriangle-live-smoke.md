# TN-apm-ftriangle-live-smoke — F△, a live pre-go-live frame

Joe's proposal, 2026-08-27: before starting the machine, run one real frame
end to end on a trivially easy problem with a short turn budget. Not a unit
test — the whole system, live agents, real dispatch.

## Why this is the missing layer

Every serious defect found on 2026-08-27 passed its unit tests:

- `ebea1f84` fixed the delivery defect and was **committed but not loaded**; the
  live path kept the old behaviour until the JVM was reloaded.
- `85c26094` built the progress watchdog with 29 green tests and **nothing
  called it**.
- `ae438faa` proved three trace predicates with zero axioms and **nothing fed
  them a trace**.
- `dd2d5abd` gated closure correctly and **retroactively invalidated 25
  historical ledgers**, emptying the prior-memory pipeline.

Each is an adapter defect: the unit passes, the wiring is absent. A unit test
cannot see this, by construction — it constructs its own inputs. A live frame
cannot avoid seeing it.

F△ is the empirical complement to the Lean: the model says what must be true,
the fixtures say the mechanisms address real incidents, and F△ says the
assembled machine actually runs.

## What it must exercise to mean anything

A frame that merely closes proves little. F△ must traverse the gates added
this week, or it certifies a machine with its safety devices disconnected:

1. **Preflight and admission** — the frame is admitted against the loaded
   runtime, not the committed source.
2. **A real dispatch** to a real seat, with a terminal collected.
3. **The holdout** — the shelf must be non-empty and contain at least one
   same-problem memory that the depositor-truth gate withholds, and the
   attempt's receipt must show it excluded. A frame with an empty shelf proves
   nothing about the holdout, which is the f46 hole.
4. **The progress watchdog** — armed, and the semantic cursor observed
   advancing.
5. **Evidence durability** — force one repair (a deliberately invalid terminal)
   and assert the predecessor survives in `:superseded-terminals`.
6. **Trace assembly and closure** — the combined trace is assembled from
   durable state, the Lean checker accepts it, and closure certifies against a
   receipt bound to the trace digest.

Item 5 is the one most likely to be dropped as inconvenient. It is also the
only way F△ tests the repair path rather than the happy path, and the repair
path is where f46 and f48 lost their evidence.

## Pass criterion

Binary and stated in advance: **the frame closes with a checker-accepted
combined-trace receipt, and all six items above are evidenced in its ledger.**
Anything else is a fail, including a close that skipped an item.

## Isolation

F△ must not touch production state: its own campaign id, its own ledger root,
its own frame numbering, and no banking of solves or deposits into the
production shelf. Its memories are fixtures. A pre-go-live check that mutates
what it is checking is not a check.

## The problem

Trivial on purpose — Pythagoras, or anything one step from Mathlib. Difficulty
is not the variable under test; if F△ fails, the apparatus is broken, and that
inference only holds if the mathematics cannot be the cause. A 30-second turn
budget is the right order: long enough for a real dispatch, short enough that
the whole frame is minutes.

## The failure mode that would make it useless

A live test can fail for substrate reasons — Zai unavailable, a seat busy, a
network timeout. If those read as apparatus failures, F△ will cry wolf and be
ignored within a week, which is worse than not having it.

So F△ must classify its own failure before reporting: **apparatus** (a gate
refused, a receipt was missing, a namespace was stale) versus **substrate** (a
dispatch never landed, a turn timed out). That is the same distinction as
register defect M5, and F△ is a second reason to build it. Substrate failures
retry; apparatus failures block go-live.

## Sequencing

F△ depends on closure working, so it cannot be built before the ledger
regression (job `…2465` priority 1) is resolved — today F△ would fail at item 6
for a reason we already understand. Build it immediately after, and run it as
the last gate before the machine restarts.

## What it does not do

It is one frame on one easy problem. It proves the assembled machine runs; it
does not prove any invariant holds in general — that is what the Lean and the
incident fixtures are for. Its value is precisely that it is the only check
that sees the wiring.
