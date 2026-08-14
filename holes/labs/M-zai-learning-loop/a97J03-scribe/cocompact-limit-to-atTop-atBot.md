# Memory: restrict a cocompact real limit to both ends

- Level: `lemma-location`; lane: mathematics
- Confidence: one compiled use (`n=1`)
- Problem: `a97J03`
- Commit: `1f93f8652da95ff5b41f407b30dc1743d0ab1a2b`
- Jobs: `invoke-1785931097145-40-484817cb`, `invoke-1785931218106-42-c36f29f8`, `invoke-1785931526692-44-a2147f71`
- Evidence: `e-pull-offer-68cd0f4f-bc45-4b98-822c-becfe1eb3086`, `e-pull-offer-6bb607c6-c4d5-43ca-92f2-d214d2d7dd5c`

## Memory

If `hv : Tendsto h (cocompact ℝ) (𝓝 a)`, obtain both requested real-end
limits directly:

```lean
hv.mono_left atTop_le_cocompact
hv.mono_left atBot_le_cocompact
```

This is the clean adapter between a C₀-style theorem and a statement phrased
as separate `+∞` and `-∞` limits.

## Demand vocabulary

Two empty subject lookups targeted
`e-codexpilot-prove-Holder-convolution-vanishes-at-infinity-by-compact-support-density`.
That literal identifier is tagged here and on the density memory as a
demand-side navigation term.

## Honesty bound

One compiled theorem uses this adapter (`n=1`); no recurrence claim is made.
