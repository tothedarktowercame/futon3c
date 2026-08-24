# TN: Library Loop review — one problem, seventeen frames, one honest sorry

Author: Claude (Fable 5), 2026-08-24. Companion to `TN-opus-f27-review.md`
(standard loop) and `TN-fable-F27-review.md` (repair clusters). Prompted by
Joe: the Library Loop "needs a similar level of scrutiny … I'm not sure we're
making adequate progress on these tasks (the Library Loop system is paused
because of that)."

## Method

Read the lane's entire recorded life: all 17 frame states under
`data/apm-lane/f9*/`, the coordinator `data/apm-lane/coordinators/t00J02.edn`,
the queue/runner/phases source, the apm-lean `ConstructionTargets/` bank and
`problems/t00J02` history on `repair/m97A06-energy-regularity`. Re-ran the
verify step myself: `lake env lean problems/t00J02/lean/Main.lean` → exit 0,
exactly one `declaration uses sorry` at 344:8, reproducing the status.json
`sorry_audit`. Numbers below are from those artifacts.

## What the Library Loop has actually touched: one problem

The lane machinery exists to serve the ~58 obstruction-classified topology
bundles (`library_lane.clj` docstring). Its entire recorded execution is
**t00J02**, on **one day**: 17 frames launched 2026-08-23 between 12:01 and
20:27 UTC, every one `problem/id "t00J02"`. The 18th launch died at 21:39 in
workspace preparation (`:workspace-probe-failed`), the coordinator sits at
`:regulator/status :failed` after 250 ticks, and nothing has run since. That
is the pause.

## The mathematics is real, and honestly corralled

t00J02 (genus-2 surface, smooth self-map, embedded handle circle, transverse
preimage components bounding disks ⇒ induced map on H₂ vanishes):

- Trajectory: `informal-only` (07-23) → formal statement banked (08-06) →
  `partial-banked` with exactly **1 sorry** (08-23), independently re-verified
  today. Library axioms audited per increment: `propext, Classical.choice,
  Quot.sound` only.
- The one sorry is *deliberate and load-bearing*:
  `OrientedSurfacePreimageDuality.Producer` — manifold Poincaré duality /
  intersection pairing for a compact surface, genuinely absent from mathlib.
  The module says so in its own docstring: "The actual Poincare-duality
  construction is intentionally the single field of `Producer`; all
  conversions … remain proved library lemmas." That is honest engineering,
  not a hidden gap.
- Discipline around it is good: the `ConstructionTargets` roll-up is
  sorry-free; the two deliberately-partial scaffolds are ledgered in
  `PARTIAL.md`; each duality seam has a paired `.md` contract; every bank
  carries an axiom audit.

**But the heavy mathematics did not come from the loop.** The deep modules —
`SingularSubdivision.lean` (3814 lines), `SingularHomologyConcrete.lean`
(1369), `Intersection.lean` (687), all sorry-free — were built on the
manually-PM'd track (claude-10 driving codex-3/codex-7 by bell+park) and
folded in on 08-22 (`ffc8f19a`). The lane's own 17 banked increments
(11:58–20:50 on 08-23, one commit per frame, frame→head mapping 1:1) are
perimeter work: interfaces, packagings, composition lemmas that route the
bridge theorem through ever-more-precise formulations of the Producer.
Genuine, verified, useful — and asymptotic to the interface. **The sorry
count went 1 → 1 across all seventeen frames.** What moved is the boundary's
precision (Producer refined to the concrete contrapositive
`NonzeroTopMapHasNonNullWinding`), not the remaining difficulty.

## What the day cost

- Coordinator `t00J02.edn` logs **21 failure/repair rounds** between 18:55
  and 21:39, each pinned to a futon3c repair commit in its `:repair/reason`.
- **48 commits** on the seven `library_lane_*` source files since 08-22;
  **150 futon3c commits** total on 08-23 (6 with explicit repair verbs in the
  subject; most of the rest are the same firefighting under feature-shaped
  names — see the coordinator's own reasons list).
- Failure taxonomy is the *same three uncontracted boundaries* as
  `TN-fable-F27-review.md`, now hit through the library lane's drivers
  (`library_lane_phases.clj` is one of the eight files calling the job routes
  directly):
  - **Missing oleans after bank** (×3: `DiskBoundedPreimageDuality`,
    `OrientedSurfacePreimageDuality`, `TransversePreimageDuality` — each time
    "targeted lake build restored banked olean"): bank advances the trunk but
    nothing rebuilds the certified libraries the consumer elaborates against.
  - **Path/authority resolution** (×5 consecutive
    `:library-role-card-path-invalid` rounds, 19:41–20:06): cluster 2 exactly.
  - **Workspace/terminal validity** (`:preflight-mutations-observed` ×4,
    `:workspace-dirty`, `:seat-mismatch`, a bank fast-forward race, and the
    final `:workspace-probe-failed` it died on).

So on its one run-day the loop banked ~14 interface-grade commits and
consumed ~21 in-run repairs plus a 48-commit machinery tail — **more
engineering went into keeping the loop alive than mathematics came out of
it**, while the week's deepest mathematics entered through the non-loop
track.

## The structural defect: the queue cannot see non-progress

`library_lane_queue.clj`: `landing-rulings #{:closed :partial-banked}`, with
`max-consecutive-non-landings 2` as the stop condition. Every perimeter
increment rules `partial-banked`, so **every interface tweak counts as a
landing and resets the counter**. The loop's own success metric is satisfied
by banking reformulations of the same open obligation indefinitely.
Seventeen frames on one problem is the f27 `retry-v1/-v2/-v3` pattern
magnified — and nothing in the machine can tell it from convergence. This is
why "are we making adequate progress?" cannot currently be answered from
inside the system: the measured thing (bank rulings) is not the real thing
(the remaining obligation).

## Diagnosis — a misappropriation of the standard loop's ceremony

(Sharpened with Joe, 2026-08-24.)

The frame/lease/receipt/coordinator apparatus was designed for the
**standard loop's** problem: multiple mutually-untrusting roles
(solver/student/guide/proctor) exchanging claims, where receipts,
independent review, and exact-snapshot binding are the *point* of the
experiment. The library lane has none of that structure — no student, no
memory snapshots, no adversarial exchange. Its verification is cheap,
objective, and local: `lake build`, sorry count, axiom audit. Wrapping
100-turn library construction in per-increment frame ceremony imported the
standard loop's costs without importing any of its benefits — and every
ceremony boundary (workspace prep, path authority, olean state after bank,
seat identity) is one of the uncontracted boundaries of
`TN-fable-F27-review.md`, so each frame paid roughly one repair.

The existence proof for the right grain is already in this repo's history:
`SingularHomologyConcrete` + `Intersection` (2,000+ sorry-free lines, the
deepest mathematics in the bank) were built by `start-codex-autorunner` —
one workspace, one Codex, ~130 turns, gates at turn boundaries, effectively
zero machine failures. The 08-22 burst in the t00J02 log (11 commits in 21
minutes, banked next morning) has the same signature and is the best work
the lane ever banked.

**Verdict on the original question:** the effort has produced something
real — a verified, well-ledgered construction-target library and a hard
problem reduced to one precisely-stated classical gap. What it has not
produced is evidence the frame-grained loop can run unattended or close
anything: 17 frames, one problem, core untouched, machine down. Progress on
the mathematics is adequate (via the autorunner-grain track); the machine is
not, and the pause is right until the rebuild below lands.
`partial-banked` may count as *progress*; it can never count as *success*.
**The success criterion is: close the demonstrator problems that required
deep extensions to mathlib. Nothing else is on the scoreboard.**

## Build plan — the simpler-but-better loop

Autorunner grain, with exactly the ceremony that earns its keep: register
construction targets as they are created, and make the agent restate its
strategy on a fixed cadence. Nothing else.

### Shape

One demonstrator problem = one long-lived workspace (an apm-lean worktree
off the trunk branch) = one Codex agent = one runner process. The runner is
a small script in the `codex-autowake` family (shell or babashka; **not** a
JVM coordinator — state lives in files, survives anything):

```
loop (turn budget, default 130):
  1. codex exec  — one turn against the standing goal prompt
  2. turn gates  (cheap, local, every turn):
     - targeted `lake build` of touched ConstructionTargets modules
     - `lake env lean` of the problem Main.lean; record sorry count
     - axiom audit (#print axioms) on new/changed CT declarations
     - a failed gate feeds back into the next turn's prompt; two
       consecutive gate failures on the same finding → pause for review
  3. every 20 turns: STRATEGY CHECKPOINT (see below)
  4. cooldown; stall pager as in codex-autowake
```

### The two ceremonies that stay

1. **Construction-target registration.** When a turn creates
   `ConstructionTargets/X.lean`, the runner requires before the next turn:
   the paired `X.md` seam doc; roll-up import (or a `PARTIAL.md` row if
   deliberately partial); axiom audit; and a row in the problem's
   `targets.edn` ledger `{:module :created-turn :status :obligation}`. This
   is what keeps the bank navigable — it is the discipline the current
   library already follows by convention; the runner makes it a gate.

2. **Strategy checkpoint, every 20 turns.** The agent must write
   `strategy-NN.md`: (a) the current statement of the remaining obligation
   (for t00J02: the `Producer` statement), (b) what was *reduced* — not
   restated — since the last checkpoint, (c) the plan for the next 20 turns.
   The runner digests (a). **If the obligation digest is unchanged across
   two consecutive checkpoints, the loop pauses for review instead of
   continuing** — this is the anti-reformulation valve the frame machine
   lacked, and it replaces `max-consecutive-non-landings`.

### Bank and review

- **Bank at checkpoints, not per commit.** A bank happens only at a strategy
  checkpoint with green gates. The bank step **rebuilds every CT module it
  advances before any consumer elaboration is ruled on** (contracts away the
  three missing-olean failures of 08-23), then fast-forwards the trunk and
  runs the existing status-recompute.
- **Review at bank time.** Author ≠ reviewer survives, at 20-turn grain: a
  Claude review of the diff + gate outputs + obligation delta before the
  bank lands. One review per checkpoint replaces per-frame
  proctor/verify/receipt ceremony.

### Success ledger

A small slate file the runner consumes, e.g.
`data/apm-lane/demonstrators.edn` — the handful of problems whose closure is
the criterion. Candidate slate (Joe to confirm): `t00J02` (one Producer
sorry away), one or two pure singular-homology problems (`t01A03`,
`t02A04`, `t94A07`), one intersection-theory (`t03J05`). Only `closed`
moves the scoreboard; the 52 obstruction-classified problems remain the
backlog the lane derivation (`library_lane.clj`) already computes.

### Salvage / drop from the existing lane machine

- **Keep:** `library_lane.clj` (lane derivation from status.json —
  independently good and already burned-in), the bank gates, the
  status-recompute, the axiom-audit convention, the obligation-digest idea
  above.
- **Drop for this lane:** frames, leases, per-increment worktrees,
  coordinator/regulator, role-card authority resolution, per-increment
  receipts (`library_lane_phases/coordinator/effects/launch/runner/queue`
  in their current form). They stay in place for the standard loop, where
  their receipts earn their cost.

### Acceptance for the rebuild

1. A dry run on t00J02 executes ≥20 turns end-to-end (gates + one strategy
   checkpoint) with **zero** futon3c repair commits during the run.
2. Runner state is files-only: kill it at any turn, restart, and it resumes
   at the next turn with no JVM involvement.
3. Regression for the olean killer: a bank that advances a CT module
   followed immediately by consumer elaboration passes without manual
   rebuild.
4. The checkpoint valve fires in anger: an unchanged obligation digest
   across two checkpoints demonstrably pauses the loop.
5. `demonstrators.edn` exists and a closure updates it through the ordinary
   status-recompute path.
