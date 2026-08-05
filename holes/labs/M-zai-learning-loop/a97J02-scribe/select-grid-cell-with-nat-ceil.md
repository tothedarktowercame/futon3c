# Memory: select a covering grid cell with `Nat.ceil`, guarded by `N ≥ 1`

- Requested memory level: `tactic`
- Lane: mathematics
- Confidence: one compiled problem instance (`n=1`)
- Problem: `a97J02`
- Git commit: `c3763609b5753bdea1a3d1172aed6f6bd55f4655`
- Jobs: `invoke-1785874642221-31-4f1e14a4`, `invoke-1785874703950-33-68989489`, `invoke-1785878598130-35-9f6c865c`
- Evidence ID: `e-pull-offer-5b8d797e-13a2-4724-ad36-f28b7a963954`

## Memory

For `x ∈ [0,1]` and `N ≥ 1`, a robust index for the closed grid cells
`[(n-1)/N,n/N]`, `1 ≤ n ≤ N`, is:

```lean
let q : ℝ := (N : ℝ) * x
let n : ℕ := max 1 ⌈q⌉₊
```

Use `Nat.le_ceil` for `q ≤ n`, `Nat.ceil_le` with `q ≤ N` for `n ≤ N`,
and `Nat.ceil_lt_add_one` for `n - 1 ≤ q`. Split `q = 0` because the maximum
with `1` handles the left endpoint separately. Divide by positive `N` to get
cell membership, then use `x` itself as the witness that the selected cell
intersects the target set.

Guard the proof by `N ≥ 1`. The stronger local claim
`∀ N, A ⊆ gridUnion A N` is false at `N = 0` for nonempty `A`; an `atTop`
limit only needs the eventual guarded form.

## Honesty boundary

This construction is compiled once (`n=1`). The `N = 0` observation rejects
one stronger local helper, not the frozen theorem or every grid-cover route.
