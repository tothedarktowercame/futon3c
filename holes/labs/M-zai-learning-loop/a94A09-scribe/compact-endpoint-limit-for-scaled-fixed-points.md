# Memory: compact endpoint limit for scaled fixed points

- Requested memory level: tactic
- Lane: math
- Confidence: `n=1`, compiled in one problem
- Problem: `a94A09`
- Job: `invoke-1786369654355-3517-0994cab1`
- Commit: `22c5b80c064ae36e83a3b8759607ccf430c76169`
- Evidence: `e-fab2e3d9-6877-444a-9949-a11720305918`,
  `e-memory-outcome-sweeper-6e8a041ab7506a025951c3b4`

## Memory

When fixed points are available for every strict radial contraction `t • f`,
recover a fixed point of `f` at the excluded endpoint by sequential
compactness. Take `t n = n / (n + 1)`, choose `z n` with
`z n = t n * f (z n)`, and regard every `z n` as lying in the compact closed
unit disk. Use `IsCompact.tendsto_subseq` to obtain a convergent subsequence.
Then combine:

- `tendsto_natCast_div_add_atTop` for `t n → 1`;
- `ContinuousOn` expressed through `nhdsWithin` for `f (z n) → f a`;
- product convergence and `tendsto_nhds_unique`.

This yields `f a = a`. If the contract excludes boundary fixed points, a
separate norm argument upgrades `a ∈ closedBall 0 1` to `a ∈ ball 0 1`.

## Importability boundary

The compiled witness `apm_a94a09_exists_closedBall_fixed_point` is **trapped
in `problems/a94A09/lean/Main.lean` and is a promotion candidate**. It mentions
no `apm_` definitions but is not importable. This memory is therefore a
re-derivation instruction until promotion.

## Honesty bound

This is one compiled complex-disk instance (`n=1`), not a general fixed-point
theorem. It assumes the strict-scale witnesses have already been constructed
and says nothing about uniqueness.
