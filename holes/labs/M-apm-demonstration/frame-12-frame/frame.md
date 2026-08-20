# Frame: m03J01 (frame-12)

## Target
`problems/m03J01/lean/Main.lean` is 253 lines and carries exactly one executable
`sorry`, the last line (253), discharging the whole of `theorem apm_m03j01`.

The frozen theorem is a conjunction of TWO clauses:

1. **The abstract minimisation clause.** For every real Hilbert space `V` there
   is a continuous `T : (V →L[ℝ] ℝ) →L[ℝ] V` such that for each functional `f`,
   the quadratic `φ(v) = ½‖v‖² − f v` is bounded below, `T f` minimises it,
   the minimiser is unique, and `⟪T f, v⟫ = f v` for all `v`. That last part is
   Riesz representation falling out of the minimisation.
2. **The concrete PDE clause.** For an open, bounded, nonempty `Ω` and
   `F ∈ L²`, existence of a weak solution in `apm_m03J01_H01 n Ω`.

## Contract
Close the `sorry`, **or reduce it to strictly less residual and say precisely
what remains.**

One `sorry` covers both clauses, and they are of very different character —
clause 1 is abstract Hilbert-space theory that Mathlib supports well, clause 2 is
a Sobolev-space existence result. **Splitting into named per-clause `have`s, each
with its own `sorry` and recorded search, is itself a real result** even if
neither closes. Collapsing them into one hole hides which is which.

If the library lacks a notion you need, that is **not** a reason to stop: name it
as a construction target — what it is, what would have to be defined, roughly
what it costs — and build it if it is within your budget. See your card.

Any definition you introduce needs a proof it takes a non-trivial value in a
concrete case. A definition that compiles but cannot be shown to be about
anything is worse than none.

No statement defect was found by a prior pass. If you find one, that is a
reportable result and not a failure to solve — the immediately preceding frame
found one and it was the most valuable thing it produced.

## Acceptance
- The frozen statement of `apm_m03j01` is unchanged.
- Any close is axiom-clean; the executable `sorry` count strictly decreases,
  **or** the bundled `sorry` is replaced by named per-clause residuals.
- Whatever remains open is localised, with the nearest API and the searches that
  came back empty recorded beside it.
