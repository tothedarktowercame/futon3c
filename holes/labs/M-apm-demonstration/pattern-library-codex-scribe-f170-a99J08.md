# Codex Scribe pattern library — f170

These patterns were added because the reviewed mathematics-memory search found
no coherent parent for constructing Schwarz reflection through Mathlib's
rectangle-integral Morera interface or for globalizing a local linear bound on
a compact domain.

## math-formalization/prove-line-reflection-by-rectangle-morera

- **Trigger:** A function is continuous across a straight seam, holomorphic on
  each open side, and the two piecewise formulas agree on the seam, but no
  ready-made Schwarz reflection or differentiable gluing theorem matches the
  hypotheses.
- **Move:** Define the reflected function explicitly, prove continuity by
  pasting on closed half-domains, and prove vanishing rectangle boundary
  integrals.  Rectangles wholly on one side use the ordinary boundary-integral
  theorem; a crossing rectangle is split at the seam into two rectangles whose
  seam contributions cancel.  Feed the resulting `IsConservativeOn` statement
  and continuity to Mathlib's Morera equivalence.
- **Why it works:** Morera converts the difficult pointwise differentiability
  obligation on the seam into an integral identity.  The off-seam derivatives
  come from the two holomorphic formulas, while continuity and cancellation
  account for the seam without differentiating the piecewise definition there.

## math-formalization/globalize-a-local-linear-bound-on-a-compact-domain

- **Trigger:** A nonnegative quantity has an estimate `q z ≤ K * d z` near
  a distinguished point and is bounded on the whole compact domain, but the
  goal asks for one linear-in-distance estimate everywhere.
- **Move:** Choose a positive radius `r` for the local estimate and a global
  bound `M`.  Split on `d z < r`.  Use the local estimate inside; outside use
  `r ≤ d z` to derive `M ≤ (M / r) * d z`, then take `K + M / r` as the
  uniform coefficient.
- **Why it works:** Positivity of `r` makes division legitimate, compactness
  supplies `M`, and the distance lower bound outside the local neighborhood
  converts the constant global bound into the required linear form.

## math-formalization/extract-a-linear-norm-bound-from-differentiability-at-a-zero

- **Trigger:** A function is differentiable at a point where it vanishes, and
  a later estimate needs an explicit norm bound proportional to distance from
  that point rather than a derivative expansion.
- **Move:** Take the bound supplied by `DifferentiableAt.isBigO_sub.bound`,
  rewrite the function and input differences using the zero base point, replace
  the scalar coefficient by its absolute value, and unpack the eventual
  neighborhood into a metric ball.
- **Why it works:** Fréchet differentiability implies the increment is locally
  big-O of the input increment.  At a zero based at the origin, this is exactly
  a local inequality of the form `‖F z‖ ≤ C * ‖z‖`.
