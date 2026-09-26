# Memory: continuous Lp translations feed a Hölder pairing

- Level: `tactic`; lane: mathematics
- Confidence: documented cross-problem reuse (`n=2`)
- Problems: `a03J03`, `a97J03`
- Commit: `1f93f8652da95ff5b41f407b30dc1743d0ab1a2b`
- Jobs: `invoke-1785931097145-40-484817cb`, `invoke-1785931218106-42-c36f29f8`, `invoke-1785931526692-44-a2147f71`
- Evidence: `e-pull-offer-d11cb533-7ec2-46c4-b35e-523366cc5580`, `e-pull-offer-36021f8f-b0f5-431e-8fa7-a00bdaadd0c2`, `e-pull-offer-aee4baa0-25af-406c-815c-83b53f648039`

## Memory

Represent `x ↦ f (x-y)` by `Lp.compMeasurePreserving` using the continuous
map `y ↦ (x ↦ y-x)`. The theorem `Continuous.compMeasurePreservingLp` makes
this Lp-valued map continuous. Apply
`(ContinuousLinearMap.mul ℝ ℝ).lpPairing` to the fixed Lq representative, then
rewrite with `lpPairing_eq_integral` and the available a.e. coercion lemmas.

This avoids a separate pointwise Hölder estimate plus hand-built translation
seminorm limit: continuity is inherited from continuous bilinear pairing.

## Demand vocabulary

Grounds `translation continuity Lp symmetric-difference`, `convolution Holder
Lp translation`, and `Holder convolution Mathlib API integral product Lp Lq
conjugate exponent bounded bilinear`; literal terms are tags.

## Honesty bound

`n=2` is justified only by the compiled source `a03J03` and its explicit
reuse in compiled `a97J03`. It is one copied cross-problem chain, not two
independent discoveries.
