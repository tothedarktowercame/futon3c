# Frame: a03J04 (frame-8, first frame under the two-promote specification)

## Target
`problems/a03J04/lean/Main.lean` in your checkout carries exactly one
executable `sorry` (verified at pin a92ffb6c, closing theorem
`apm_a03J04_bridge_weakL1_rpow_setIntegral`). Weak-L¹ to L^p (p<1)
embedding on `EuclideanSpace ℝ (Fin n)`: for f of weak-L¹ size M and
measurable finite-measure E, integrability on E plus
`∫_E |f|^p ≤ (1/(1-p)) · m(E)^(1-p) · M^p`. The constant was verified by
hand at review (split the layer-cake level integral at A = M/m(E)).
Scaffolding already proved in-file: the reciprocal level bound, the
restricted min-bound, and the exact Mathlib layer-cake identity.

## Contract
Work ONLY in your checkout (ENVIRONMENT block of your dispatch); commit
ONLY to the frame branch. Do not touch files outside `problems/a03J04/`.
Verify the hole state against YOUR BRANCH before working.

## Acceptance
1. `lake env lean problems/a03J04/lean/Main.lean` → exit 0 with ZERO
   "declaration uses `sorry`" warnings.
2. `#print axioms apm_a03J04_bridge_weakL1_rpow_setIntegral` → at most
   [propext, Classical.choice, Quot.sound].
3. Commit to the frame branch; reply with summary, commit shas, and the
   verbatim axiom output.

## Reporting honesty
A defective-statement or stale-premise report is a valid completed
attempt. Commit partial lemmas if the final theorem remains open; report
the exact residual in Lean terms — your residual is authoritative.
