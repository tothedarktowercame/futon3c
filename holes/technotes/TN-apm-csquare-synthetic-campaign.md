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

## Pre-registered prediction (Joe, 2026-08-28, before C□ was built)

Joe's expectation, recorded in advance so the outcome is interpretable rather
than rationalised afterwards: **under current conditions C□ will likely start
failing**, because of the coordination problems observed overnight.

The specific mechanisms that make that prediction plausible, each already
evidenced:

- **A9** — no watchdog leads to a halt rather than an arm. C□ starts a campaign
  ten times over; if arming is at all flaky across frame boundaries, C□ halts
  rather than repairs.
- **M7 / committed-not-running** — the 2026-08-28 outage was a stale JVM. C□
  cannot detect that about itself unless it checks loaded-namespace currency,
  as F△'s preflight C3 does.
- **Watchdog re-arming across frames** — armed once at campaign start is not the
  same as armed at frame ten. Nothing has ever tested the second.
- **Tick claims and leases across frame boundaries** — A5 brackets a tick; A4
  drains a coordinator. Neither has been exercised over a ten-frame series.
- **Prior accumulation** — the shelf grows frame over frame, and the
  2026-08-27 closure-gate regression silently emptied it. C□ is the first thing
  that would notice mid-series.

Recording this in advance matters because the two outcomes carry different
information:

- **C□ fails** — confirms the coordination diagnosis, and the failure point
  names which mechanism. This is the expected result and is a success for the
  test.
- **C□ passes** — the coordination problems are narrower than the overnight
  evidence suggested, and the burden shifts back to explaining why f49 ran six
  hours unwatched. That would be the surprising result and should be treated
  with suspicion until the pass is shown to be non-vacuous, i.e. that C□ really
  did traverse all ten frames through real gates.

A pass obtained with stubbed gates is not a pass. See the design decision
above.

## C□ as a mutation harness (Joe, 2026-08-28)

C□'s first run passed: ten frames in 47 seconds, real Lean elaboration, real
checker acceptance, priors growing, watchdog armed, `:batch-complete`.

**That result is not meaningful on its own, and should not be reported as
though it were.** Joe: *"the fact that the synthetic campaign works should not
be taken as meaningful given that the real machine just failed badly. The
synthetic campaign ONLY succeeded because it managed some kind of happy path."*

He is right, and the same morning proves it: `jit-all-open-v2` also ran its
happy path — preflight, solve, verify, promote-solver and student-attempt-1 all
certified — and then failed at the guide phase. A harness that traverses the
same happy path and stops there tells us nothing we did not already know.

### What would make it meaningful

Use C□ the way `generated-contract-test` uses mutation: **a harness earns trust
by killing mutants, not by going green.** Take each failure the machine has
actually produced, inject it into C□ as a mutation, and require C□ to fail and
name the mechanism. A mutation C□ does not catch is a blind spot, stated
explicitly rather than discovered later in production.

The mutation set is not hypothetical — every entry below has been observed:

| # | mutation | observed as |
|---|---|---|
| 1 | a consumer does not classify a state the producer can emit | `delivering` unrecognised by `job_port/terminal-states`, 2026-08-28 |
| 2 | a terminal job rests at `pending` for some caller shape | non-seat callers, F△ run 5 |
| 3 | an observation is stripped in transit | `job->terminal` dropping `:trace/delivery-observation`, F△ run 6 |
| 4 | a withheld memory is served by some channel | f46 and f48 holdout breaches, 2026-08-27 |
| 5 | a successor is announced before its predecessor is archived | f46/f48 evidence loss |
| 6 | a coordinator runs with no watchdog armed | A9, overnight 2026-08-27 |
| 7 | terminal state is published before the delivery disposition commits | the race fixed by `6ad6d55b` |
| 8 | a role returns prose where a typed submission is required | f49 `guide-intervention-1`, `:submission nil` |
| 9 | loaded namespaces diverge from committed source | C1, the inert watchdog, the 2026-08-28 outage |

### The property

For each mutation: C□ **fails**, and its failure **names the mutated
mechanism** rather than surfacing as a timeout, a nil, or a generic halt. Two
of today's defects presented as "no terminal observed" and "typed submission
missing" — verdicts that pointed away from their causes.

A mutation C□ silently survives is the finding. Record it rather than removing
the mutation.

### Why this is worth more than another fix

Seven distinct delivery-protocol defects have surfaced in about twenty-four
hours (C1, non-seat callers, transit stripping, the terminal/delivery race, the
unrecognised `delivering` state, caller attribution, F△'s own pending
dispatch). That is not a run of bad luck in one subsystem; it is an unvalidated
subsystem. Fixing them one at a time and re-running the happy path will keep
producing green results and further defects.
