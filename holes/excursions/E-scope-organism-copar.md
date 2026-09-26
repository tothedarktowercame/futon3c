# E-scope-organism-copar — CLOSED: the CLean renderer now emits a domain copar

Date: 2026-07-09
Status: CLOSED (2026-07-09). `futon6/scripts/clean_to_lean.py` now emits a
CLean-supplied domain copar structurally: `:clean/shape :readings [:scope
:organism]` renders as `def readings : BV Reading := BV.copar (BV.atom
Reading.scope) (BV.atom Reading.organism)` (+ a commutativity `example`), and
`:clean/shape :co-app [[a b w]…]` (the cascade semilattice's meets) renders as
`def meets : List (Stp × Stp)` with a `BV.copar` witness. When neither is
declared the render is BYTE-IDENTICAL to the informal∥formal default, so the
102-paper / fold corpus is unchanged (verified: 26 proofs, 0-sorry, diff-clean).
Surfaced twice as load-bearing: the mission spec's scope∥organism reading AND
the cascade-fold's `co_app` overlaps (see the semilattice-fold prototype).
Originally: parked while we focused on `mission_clean.clj` one thing at a time.
Owner: Joe + claude-1
Relation: home for the copar gap surfaced while MAPping the outer-loop tracker
(missions modelled as DarkTower specs, per `futon2/holes/ct-wiring-explainer.html`
worked example ① and `~/code/mathlib4/DarkTower/Examples.lean §MissionExample`).

## The gap

A mission renders **0-sorry** through the *existing* CLean bridge
(`futon6/scripts/clean_to_lean.py`) today — validated 2026-07-09 on
`M-learning-loop` (head+identify discharged, map the live frontier,
derive→document hungry, `document` graded `:payoff`). No renderer change needed
for the 8-phase spine, the typed phase-holes, or the satiety grading.

The **one** place the render is short for missions is the copar reading:

- `clean_to_lean.py` hardcodes the CLean's dual reading as **informal ∥ formal**
  (the method-spine ∥ the comb) — same for every CLean, emitted as
  `def readings : BV Reading := BV.copar (BV.atom Reading.informal) (BV.atom Reading.formal)`.
- `§MissionExample` holds a *domain* copar: **scope ∥ organism** — the two
  simultaneous readings of one mission object (`def readings : BV Reading :=
  BV.copar (BV.atom Reading.scope) (BV.atom Reading.organism)`). This is
  load-bearing in the Lean exemplar; it is the mission's own dual reading, not
  the generic method∥comb one.

So the current mission-CLean carries `informal ∥ formal` (correct and 0-sorry),
while `scope ∥ organism` — the mission-specific reading — is parked as inert
metadata: `:clean/shape {:readings [:scope :organism]}`.

## Why deferred (not dropped)

The render is faithful and 0-sorry without it; `scope ∥ organism` is a
*fidelity* upgrade, not a correctness blocker. Doing it now would mean teaching
the generic renderer a per-CLean domain copar (a second `Reading` inductive, or
a parameterised copar), which touches the shared `clean_to_lean.py` used by the
102-paper corpus and the fold CLeans — scope creep against "one thing at a time."

## To close later

Teach `clean_to_lean.py` to emit a CLean-supplied domain copar when present:
read `:clean/copar` entries whose `:reading` is not `informal`/`formal` (or a
new `:clean/readings [:scope :organism]` key), emit the matching `Reading`
inductive + `def readings`, and keep the `informal ∥ formal` copar as a second
reading (or generalise `readings` to hold both). Regression-check that the
paper/fold CLeans still render 0-sorry. Then the mission-CLean carries both its
generic (method∥comb) and domain (scope∥organism) readings — matching
`§MissionExample` exactly.

Cross-ref: `mission_clean.clj` (the emitter this excursion was carved out of),
`futon3c/src/futon3c/logic/capability_star_map_extractor.clj`
(`structural-hole-report-from-frames` — the written/ghost/vacuous state the
emitter drives box-holes off), `E-mission-head.md` (the mission head/lifecycle).
