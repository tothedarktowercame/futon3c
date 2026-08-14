# Maximum-modulus frontier bound API for a ball

- Memory level: lemma-location
- Confidence: one compiled use (`n=1`)
- Problem: `a97J07`
- Commit: `6f12f79c59d40b007e5d946ae198cf6b65e15737`
- Jobs: `invoke-1785936794200-80-c77d3618`, `invoke-1785936855225-82-84503052`
- Evidence-store IDs: `e-pull-offer-51bafee6-40da-45d7-a76f-e76849810fd4`, `e-pull-offer-66cb9a25-d8ae-4dd2-b28c-c3eac71f8b44`

For a complex function on a bounded ball, package differentiability on the
open ball and continuity on the closed ball with `DiffContOnCl.mk_ball`, then
apply `Complex.norm_le_of_forall_mem_frontier_norm_le`. Its arguments are the
bounded ball, the `DiffContOnCl` witness, a norm bound on `frontier ball`, and
membership of the evaluation point in the closure.

In `a97J07`, `frontier_ball_subset_sphere` converts the frontier hypothesis to
the unit-norm condition expected by the two semicircle assumptions.

Compiled witness: `APMa97J07.geometric_mean_bound`.

Honesty bound: `n=1`. This overlaps the established general
`DiffContOnCl`-packaging memory, so the new content is limited to the exact
maximum-modulus lemma location and argument shape.

Demand-side tags copied literally: `maximum modulus principle analytic
function disk boundary bound interior point Mathlib`; `maximum-modulus analytic
disk complex-analysis`.
