# Memory: rectangular-contour residue theorem gap

- Requested memory level: `lemma-location`
- Lane: mathematics
- Confidence: one revision-scoped observation (`n=1`)
- Problem: `a96J08`
- Git commit: `37192e165973a5280114a45ca2b66bf8429e4c37`
- Jobs: `invoke-1785772892476-949-1c6b0d34` (phase A),
  `invoke-1785773411377-951-bce95078` (phase B)
- Evidence IDs: `e-bd7c225c-30e9-4164-90e0-4651524917b3`,
  `e-925e472d-f5e4-47d7-bad0-fee41c2cf68b`,
  `e-8225ad52-bbc9-4f5b-abf9-73ae83772208`,
  `e-5475c4e0-c87e-4ca4-8c4a-7905bc5eaedb`

## Memory

For a rectangular contour containing a pole, Mathlib at revision
`37192e165973a5280114a45ca2b66bf8429e4c37` did not expose a packaged
residue theorem matching the needed boundary integral. The nearest located
result was
`integral_boundary_rect_eq_zero_of_differentiableOn` in
`Mathlib.Analysis.Complex.CauchyIntegral`: Cauchy--Goursat for a function
differentiable on the closed rectangle, hence the zero-residue case.

The a96J08 proof therefore stopped at a precise bridge. Two recorded unblock
routes remain:

1. excise a small disk around the interior pole, apply Cauchy--Goursat on the
   punctured rectangle, and recover the small-circle residue contribution;
2. avoid the poles with shifted horizontal edges and prove that those edge
   integrals converge to the required principal value as the shift tends to
   zero.

## Application rule

Before starting a rectangle-and-poles formalization, search for both the
zero-residue rectangle theorem and a pole-bearing residue theorem. If only
the former is present, budget explicitly for disk excision or shifted-edge
convergence; a circle-integral residue lemma does not by itself discharge the
rectangular period-shift argument.

## Honesty boundary

This is an observed absence after the bounded Mathlib and memory searches
recorded above, not a timeless claim about Mathlib and not a proof that no
other route exists. It is one problem instance (`n=1`). Recheck the library
at a newer revision before reusing the absence claim.
