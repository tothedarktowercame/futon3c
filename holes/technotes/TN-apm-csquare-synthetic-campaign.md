# TN-apm-csquare-synthetic-campaign — C□, ten frames back to back, no agents

Joe's proposal, 2026-08-28: a synthetic campaign that runs ~10 problems back to
back with no agent invocation, each problem moved from failing to passing
programmatically — solve `1 + ? = 2` and proceed to the next step in the cycle.

## Why F△ is not enough

F△ proves **a frame** can run. It does not prove **a campaign** can start or
sustain itself, and the gap is not theoretical:

- **A9** — `durable_coordinator` halts instead of arming when it finds no
  watchdog. Eight green F△ runs never found it.
- **A10** — F△ arms its own watchdog on its own coordinator, so it never
  traverses the production start path where A9 lives.

On 2026-08-28 that gap cost a night: the campaign ran unwatched, f49's
student-attempt-1 was dispatched at 22:50 and still open at 04:50 — six hours
against a thirty-minute budget — and then the machine disabled itself over a
condition it could have repaired in one call.

C□ is the campaign-level analogue of F△, and it is cheap enough to run often.

## What C□ uniquely exercises

Everything below is untested today by F△ or by unit tests:

1. **The production start path** — the campaign starts the way a real one does,
   through the coordinator registry, so A9-class defects surface.
2. **Queue sequencing across frames** — ten frames minted in order, exactly one
   active at any moment, each retired before the next mints, ending with an
   empty active slot. These were the assertions in
   `queued-frame-terminal-test`, deleted 2026-08-27 because its providers were
   all stubbed and its one unique assertion read a hardcoded constant. C□ is
   the non-vacuous version.
3. **Prior accumulation and lineage** — the shelf growing frame over frame, and
   `campaign-prior-memories` returning what it should as the series extends.
   The 2026-08-27 regression that emptied priors would have been caught here.
4. **Watchdog behaviour over a series** — armed at start, still armed at frame
   ten, and re-armed correctly across frame boundaries.
5. **Closure ten times** — combined-trace assembly and checker acceptance
   repeatedly, not once.

## The design decision that determines whether this is worth anything

**Programmatic solver, real gates.**

The solver is synthetic: a function that edits the workspace to close the
problem, no agent dispatched. Everything downstream must remain real — verify
genuinely runs Lean, the trace is genuinely assembled from durable state, the
Lean checker genuinely accepts or rejects, closure genuinely requires a receipt.

Concretely: each problem is a real Lean file containing a `sorry`; the synthetic
solver replaces that `sorry` with the one-line proof; `verify` then runs for
real and passes because the file really does compile. The problems go from
failing to passing because they *were* failing and now *are* passing — not
because a stub said so.

If instead the gates are stubbed to return success, C□ tests a mock of the
machine and will pass forever while the machine rots. That is the failure mode
that made `queued-frame-terminal-test` worthless.

## Pass criterion

Stated in advance and binary:

- ten frames minted in ascending order;
- exactly one active frame at any observation;
- each frame closed with a checker-accepted combined-trace receipt;
- priors non-empty and growing from frame two onward;
- watchdog armed continuously, verified at first and last frame;
- campaign ends `:batch-complete` with no active frame and no outstanding tick
  claim;
- total wall-clock small enough to run routinely — minutes, not hours, since no
  agent turn is spent.

Anything else is a fail, including a run that closes ten frames while skipping
one of the above.

## Isolation

Own campaign id, own ledger root, own corpus directory. **Its problems must not
live under `apm-lean/problems/`** — on 2026-08-27 F△'s smoke theorem was
committed there and would have entered the production corpus of 475 the moment
apm-lean was pushed. Own coordinator id; never touches
`jit-queue:jit-all-open-v2` or the other production entries. No banking, no
deposits into the production shelf.

## What C□ does not do

It spends no agent turns, so it says nothing about student behaviour, holdout
efficacy, memory transfer, or anything requiring a model. It tests the
apparatus that carries those things. F△ remains the check that a real dispatch
and a real terminal work; C□ is the check that the cycle repeats.

Run both before a production restart: F△ for depth on one frame, C□ for the
campaign path across ten.
