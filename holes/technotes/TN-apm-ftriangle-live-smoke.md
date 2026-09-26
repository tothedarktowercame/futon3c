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

## Seventh traversal item: the model actually dispatched (Joe, 2026-08-28)

F△ dispatches a real role turn, so it can check something no unit test can:
that the model a seat was **declared** with is the model it was **invoked**
with.

The gap this closes was found on 2026-08-28. `frame-seats` supports a `model`
and per-seat `:model` overrides; `mint-frame-seats!` assigns one only if a
caller passes it; `agent_pouch` adds `--model` only when a model is present;
and the `jit-all-open-v2` campaign config declared none. So every
`type=claude` seat — guide and analyst — inherited the CLI default. All 24 APM
role seats read `model=None` on the roster, and f49's guide burned Fable quota
that Joe had never intended to spend on frame roles.

Nothing was wrong-looking anywhere. A field was absent, and absence silently
meant "whatever the CLI happens to be set to".

**The check:** for each role F△ dispatches, assert the seat's declared model is
non-nil and equals the model the invocation actually used. A seat with no
declared model fails F△; a mismatch between declared and invoked fails F△ and
names both values.

This is the same property as the rest of F△: a declaration surviving the trip
to the running system. `authority-fields` dropped a declared key,
`job->terminal` dropped a declared observation, `campaign_trace` declared
producers that emitted nothing — and here a declaration was never made at all,
which is the degenerate case of the same defect.

**Sequencing:** this item can only be asserted once role models are declared
(codex-1, job `…3142-90203f38`). Add it after that lands, not before, or F△
fails on a condition nothing yet satisfies.

## Informational: provider usage headroom (Joe, 2026-08-28)

Pre-go-live should also report provider usage — **for information, not as a
gate.**

The distinction is deliberate. F△'s six preconditions are gates: unmet means
refuse to dispatch, because running would produce a meaningless result. Usage
headroom is different — you may legitimately start a campaign knowing a pause
is coming, and blocking that would be the machine dictating to the operator
rather than informing them. What you cannot afford is starting *without
knowing*, which is exactly what happened overnight on 2026-08-27: f49's guide
hit a five-hour Fable window nobody had looked at.

**Report, per provider seat type in the campaign's cast:**

- which model each role type will use (once declared — see the seventh
  traversal item);
- whatever quota or headroom figure the provider exposes;
- when the current window resets, if that is knowable.

**Report "unknown" honestly where it is unknown.** What is queryable differs by
provider — GLM exposes a usage panel, and the Claude CLI's limit is observable
only once hit, which is how we discovered this one. A confident-looking number
inferred from nothing would be worse than an explicit blank, and this codebase
has spent two days on fields that silently meant something other than they
appeared to.

The value is small and specific: an operator deciding whether to start a
campaign now or after a reset should be able to see the answer, rather than
discovering it six phases into a frame.
