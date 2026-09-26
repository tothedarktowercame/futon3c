# Transfer disk regularity through reflection and multiplication

- Memory level: tactic
- Confidence: one compiled use (`n=1`)
- Problem: `a97J07`
- Commit: `6f12f79c59d40b007e5d946ae198cf6b65e15737`
- Jobs: `invoke-1785936794200-80-c77d3618`, `invoke-1785936855225-82-84503052`
- Evidence-store IDs: `e-pull-offer-16c65e73-9f2b-4a33-bb28-9955a3ca6112`

For `g z = f z * f (-z)`, prove interior differentiability with
`DifferentiableOn.mul` and `hf_diff.comp differentiable_id.neg`, discharging
ball preservation by `simpa`. Prove closed-ball continuity similarly with
`ContinuousOn.mul`, `continuous_id.neg`, and composition. Then construct
`DiffContOnCl ℂ g (ball 0 1)` using `DiffContOnCl.mk_ball`.

Compiled witness: `APMa97J07.geometric_mean_bound`.

Honesty bound: `n=1`; the memory records one verified elaboration pattern, not
a cross-problem recurrence.

Demand-side tags copied literally: `reflection symmetry product function upper
lower semicircle geometric mean two constants theorem`.
