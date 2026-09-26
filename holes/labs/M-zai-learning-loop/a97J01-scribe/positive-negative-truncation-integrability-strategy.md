# Memory: bound positive and negative parts with sign-adapted truncations

- Requested memory level: `strategy`
- Lane: mathematics
- Confidence: one compiled problem instance (`n=1`)
- Problem: `a97J01`
- Git commit: `9435997fcf4e083eb3c0a2574a77f3d462923be7`
- Evidence IDs: `e-pull-offer-80040942-43aa-477a-9c36-4522c31b4d11`, `e-pull-offer-0142b56c-fcd2-4a29-ac8c-3a6bc6d11c41`

## Memory

When a hypothesis bounds absolute Bochner integrals on every bounded
measurable region where `f` is bounded, recover global `L¹` control by using
two sign-adapted exhaustions:

```text
A_n = {|x| ≤ n, 0 ≤ f x ≤ n}
B_n = {|x| ≤ n, -n ≤ f x ≤ 0}.
```

On `A_n`, nonnegativity turns the absolute integral bound into
`∫_{A_n} f ≤ c`. On `B_n`, nonpositivity turns it into
`-c ≤ ∫_{B_n} f`. Transport these to lintegral bounds for
`max (f x) 0` and `max (-f x) 0`, exhaust by MCT, and combine using
`|f x| = max (f x) 0 + max (-f x) 0`. A finite norm-lintegral then closes
`Integrable f` through `hasFiniteIntegral_iff_norm`.

The truncations simultaneously guarantee finite spatial measure, a uniform
function bound on each piece, monotonicity, and pointwise exhaustion.

## Demand-side vocabulary

This grounds the noisy query `positive part negative part fp pos part fneg
measurable function decomposition integral`; all literal terms are promotion
tags.

## Honesty boundary

The strategy has one compiled theorem (`n=1`). The phase-A memories
`math-informal/monotone-approximation` and
`math-informal/exhaustion-as-theorem` supported the direction, but they are
not a second compiled problem instance.
