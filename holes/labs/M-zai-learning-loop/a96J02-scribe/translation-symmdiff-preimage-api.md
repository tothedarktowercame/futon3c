# Memory: translation continuity through symmetric-difference preimages

- Requested memory level: `lemma-location`
- Lane: mathematics
- Confidence: one compiled problem instance (`n=1`)
- Problem: `a96J02`
- Git commit: `318160d89257eab8482e8066e284afb91a7ec6ac`
- Jobs: `invoke-1785855266102-987-05ed970b` (phase B),
  `invoke-1785856907892-990-3ec676f1` (closer)
- Evidence IDs: `e-80de2c3d-3c71-4b9b-9090-b994124581fa`,
  `e-840afcca-1d9a-4a7d-aff6-b2caad5e587e`,
  `e-de860d2b-ae07-4cd4-bb5f-e280384d0563`

## Memory

When an indicator-convolution proof needs L1 translation continuity, search
one representation lower than a function-space translation operator. In the
Mathlib revision used by a96J02,
`tendsto_measure_symmDiff_preimage_nhds_zero` in
`Mathlib/MeasureTheory/Measure/ContinuousPreimage.lean` supplies exactly the
set-level bridge: preimages of a finite null-measurable set under a convergent
family of measure-preserving maps converge in symmetric-difference measure.

For the overlap `A ∩ (x - B)`, instantiate the maps by `t ↦ x - t`, using
`volume.measurePreserving_sub_left`. This avoids any false pointwise
continuity claim for indicators at their boundaries and avoids needing a
boundary-null hypothesis.

## Application rule

If a search for an L1 translation theorem is empty, restate the obligation as
convergence in measure of symmetric differences of translated sets. Search
for `ContinuousPreimage`, `symmDiff`, `preimage`, and measure-preserving maps
before building an approximation theorem locally.

## Demand-side vocabulary

This memory grounds the hungry query, copied literally into retrieval tags:
`L1 translation continuity indicator function dominated convergence measure
inter translate`.

## Honesty boundary

This is a positive API location witnessed by one compiled proof (`n=1`), not
a claim that it is the only route or that the API name is stable across
Mathlib revisions. Phase B's search did not find it under the L1 vocabulary;
the closer found it by changing representation.

