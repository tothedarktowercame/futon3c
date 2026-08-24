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

## Recommendations

1. **Do not relaunch until the three killers are contracted** (they are the
   f27-review ports, applied to this lane):
   (a) bank must rebuild the certified `ConstructionTargets` modules it just
   advanced before any consumer elaboration is ruled on;
   (b) role-card/path authority through one resolver with an existence check
   before dispatch;
   (c) one workspace-probe contract for prepare/collect (first and last
   failures of the day are both probe failures).
2. **Measure the obligation, not the ruling.** For library-lane problems the
   sorry count is pinned at 1 by design. Record, per bank, a digest of the
   *statement of the remaining Producer obligation*; progress = that
   statement getting strictly weaker (reduction), not merely restated
   (reformulation). Cap frames-per-problem-per-day; after N banks with an
   unchanged obligation digest, the queue should rule the problem parked for
   strategy, not keep landing.
3. **Treat the manual track as the critical path, the loop as its consumer.**
   The Producer needs surface Poincaré duality / intersection pairing; the
   only active attack is codex-3/codex-7 on `Intersection` +
   `SingularHomologyConcrete` under claude-10's PM seat. The loop's proper
   job is downstream: once the pairing exists, discharge Producers across the
   58-problem lane. Consider scoping the Producer to what t00J02 actually
   needs (the genus-2 case with concrete handles, reachable from the concrete
   singular-homology machinery) rather than full duality.
4. **Verdict on Joe's question.** The effort has produced something real: a
   verified, well-ledgered construction-target library and a hard problem
   reduced to one precisely-stated classical gap. What it has *not* produced
   is evidence the loop can run unattended or close anything: 17 frames, one
   problem, core untouched, machine down. Adequate progress on the
   mathematics (via the manual track); not yet an adequate *machine* — and
   the pause is the right call until 1–2 land.
