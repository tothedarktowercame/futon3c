# Reflection product converts two arc bounds to a geometric mean

- Memory level: strategy
- Confidence: one compiled use (`n=1`)
- Problem: `a97J07`
- Commit: `6f12f79c59d40b007e5d946ae198cf6b65e15737`
- Jobs: `invoke-1785936794200-80-c77d3618`, `invoke-1785936855225-82-84503052`
- Evidence-store IDs: `e-pull-offer-16c65e73-9f2b-4a33-bb28-9955a3ca6112`, `e-pull-offer-66cb9a25-d8ae-4dd2-b28c-c3eac71f8b44`

When an involution exchanges two boundary pieces carrying bounds `a` and `b`,
form the invariant product `g z = f z * f (-z)`. On either semicircle one
factor receives the upper bound and the reflected factor receives the lower
bound, hence `‖g z‖ ≤ a*b` on the entire frontier. Maximum modulus at zero gives
`‖f 0‖² ≤ a*b`, and `Real.le_sqrt` yields the geometric-mean bound.

Compiled witness: `APMa97J07.geometric_mean_bound`.

Honesty bound: `n=1`; this is not claimed as a recurring cross-problem pattern.
It is adjacent to the existing analytic zero-product reflection memory, but
distinct: this entry multiplies quantitative boundary bounds rather than
propagating vanishing.

Demand-side tags copied literally: `reflection symmetry product function upper
lower semicircle geometric mean two constants theorem`; `maximum-modulus
analytic disk complex-analysis`.
