# Memory: shrink the Rouché contour for scaled disk fixed points

- Requested memory level: strategy
- Lane: math
- Confidence: `n=1`, compiled in one problem
- Problem: `a94A09`
- Job: `invoke-1786369654355-3517-0994cab1`
- Commit: `22c5b80c064ae36e83a3b8759607ccf430c76169`
- Evidence: `e-fab2e3d9-6877-444a-9949-a11720305918`,
  `e-memory-outcome-sweeper-6e8a041ab7506a025951c3b4`

## Memory

To find a fixed point of `t • f` for a holomorphic self-map of the closed unit
disk and `0 ≤ t < 1`, do not apply Rouché on the unit circle, where only a
weak boundary estimate is available. Choose a radius strictly between the
scale and one, for example `r = (t + 1) / 2`. On `sphere 0 r`, the disk-map
bound gives

```text
‖-(t : ℂ) * f z‖ ≤ t < r = ‖z‖.
```

The smaller `closedBall 0 r` lies inside the open unit disk, so
`DifferentiableOn` upgrades pointwise to `AnalyticOnNhd` there. Apply the
importable theorem
`ConstructionTargets.Rouche.zeroCountInClosedBall_add_eq` to the identity and
`z ↦ -(t : ℂ) * f z`; the identity has zero count one, so the sum has a zero.

## Importability boundary

The compiled witness `apm_a94a09_exists_scaled_fixed_point` is **trapped in
`problems/a94A09/lean/Main.lean` and is a promotion candidate**. Its statement
mentions no `apm_` definitions, but it is not currently importable. Reuse the
strategy or promote the lemma; do not cite the problem-local name as an
available library declaration.

## Relation to existing memory

This is not a replacement for
`e-codexpilot-package-every-rouche-homotopy-slice-for-the-argument-principle`.
That memory packages fixed-contour homotopy hypotheses. This `n=1` memory adds
the moving-contour choice `t < r < 1`, which converts a weak unit-boundary
estimate into the strict Rouché inequality needed for a scaled fixed point.

## Honesty bound

The pattern is compiled once. It establishes existence for strict scales; it
does not establish the endpoint `t = 1` or uniqueness.
