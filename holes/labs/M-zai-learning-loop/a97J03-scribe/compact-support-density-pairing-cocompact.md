# Memory: compact-support density plus Lp pairing gives cocompact vanishing

- Level: `strategy`; lane: mathematics
- Confidence: documented cross-problem reuse (`n=2`)
- Problems: `a03J03`, `a97J03`
- Commit: `1f93f8652da95ff5b41f407b30dc1743d0ab1a2b`
- Jobs: `invoke-1785931097145-40-484817cb`, `invoke-1785931218106-42-c36f29f8`, `invoke-1785931526692-44-a2147f71`
- Evidence: `e-pull-offer-bfe34a9f-2c35-470b-b0ae-63997cccdd1e`, `e-pull-offer-5992e3a0-5267-43e0-b220-e875f34b9ce4`

## Memory

Approximate both Lp representatives with continuous compactly supported
functions using `MemLp.exists_hasCompactSupport_eLpNorm_sub_le`. Their
Mathlib convolution has compact support. Expand the original pairing minus
the compact core into two bilinear error terms; bound each with
`ContinuousLinearMap.le_opNorm₂` and translation norm preservation. Choose
the two approximation radii with denominators enlarged by `+1`, so elementary
nonnegative arithmetic makes the combined error strictly smaller than ε.

The compact core is eventually zero on `cocompact`; the uniform error transfers
that vanishing to the original convolution.

## Demand vocabulary

Grounds the noisy query `convolution Lp Lq Holder inequality continuous
vanishes infinity translation`. Literal terms are tags. The broader existing
memory `e-codexpilot-prove-Holder-convolution-vanishes-at-infinity-by-compact-support-density`
is cited rather than duplicated.

## Honesty bound

The `n=2` claim is the compiled `a03J03` proof reused in compiled `a97J03`.
It does not claim that Mathlib itself provides this theorem; its convolution
source still listed the general result as TODO in this revision.
