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

## F△'s own preflight — check the wiring before spending a single agent turn

Joe, 2026-08-27: do not run F△ until it has some hope of succeeding. A first
run that fails for a reason already known would discredit the gate before it
has earned anything.

Rather than rely on someone remembering that, F△ should refuse to dispatch. Its
first act is a static wiring check costing no agent time, and it aborts with
`:ftriangle-preconditions-unmet` naming the unmet item:

1. **Historical ledgers project valid.** Pick any completed frame ledger and
   assert `:projection/status :valid`. Today f28 of
   `jit-all-open-nontopology-v1` returns `:invalid` with
   `:frame-close-combined-trace-required`, so closure cannot succeed and F△
   must not dispatch.
2. **Priors are non-empty.** `campaign-prior-memories` over the declared
   lineage returns candidates, not 13 drops. Without this the shelf is empty
   and item 3 of the traversal is untestable.
3. **Loaded namespaces match the declared revision.** F△ runs against the JVM,
   not the tree. `generated-contract`, `campaign-trace`, `campaign-machine` and
   `countdown-control` must be the current ones — this is the C1 failure mode,
   where a correct fix sat unloaded for an hour.
4. **The watchdog is armed** for the coordinator F△ will use.
5. **The trace assembler issues a receipt** for a synthetic frame, and the Lean
   checker accepts it — proven before any agent is dispatched, since this is
   the step most likely to fail and the most expensive to discover late.
6. **A shelf fixture exists** containing at least one same-problem memory, so
   the holdout has something to withhold.

Only when all six pass does F△ dispatch a real turn. This makes a premature run
cost seconds instead of agent time, and — more usefully — turns the checklist
into something executable rather than a paragraph someone has to remember.

The preflight is also independently valuable: it is a cheap, honest answer to
"is the machine ready?", runnable at any time without starting anything.
