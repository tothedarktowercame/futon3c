# Memory: overlap continuity via `MeasuredSets`

- Requested memory level: `tactic`
- Lane: mathematics
- Confidence: one compiled problem instance (`n=1`)
- Problem: `a96J02`
- Git commit: `318160d89257eab8482e8066e284afb91a7ec6ac`
- Job: `invoke-1785856907892-990-3ec676f1`
- Evidence IDs: `e-6c935a1b-46b5-4fd0-9577-5ab34113b195`,
  `e-de860d2b-ae07-4cd4-bb5f-e280384d0563`

## Memory

To prove continuity of an overlap measure such as
`x ↦ volume (A ∩ (x - B))`, package each overlap as an element of
`MeasuredSets volume`. Its emetric is symmetric-difference measure, and
`MeasuredSets.continuous_measure` turns continuity in that emetric into
continuity of the measure.

The reusable inequality is that intersection by a fixed set is
nonexpansive:

```lean
rw [MeasuredSets.edist_def]
change volume ((A ∩ Ty) ∆ (A ∩ Tx)) ≤ volume (Ty ∆ Tx)
rw [← inter_symmDiff_distrib_left]
exact measure_mono inter_subset_right
```

Combine it with a symmetric-difference translation limit and
`tendsto_iff_edist_tendsto_0`. The compiled witnesses are
`continuous_overlapSet` and `continuous_overlapMeasure` in a96J02.

## Application rule

Use this pattern when a real-valued overlap or correlation is easiest to
control through sets. First prove the moving set is continuous in symmetric
difference, then use a nonexpansive set operation, then apply
`MeasuredSets.continuous_measure`.

## Demand-side vocabulary

This memory grounds the hungry query `measurable set finite measure
intersection positive overlap`; those literal terms are retrieval tags.

## Honesty boundary

The pattern compiled for finite-measure translated subsets of the real line
in one problem (`n=1`). It does not claim arbitrary set operations are
nonexpansive, nor does it remove the measurability and finiteness hypotheses
required by the translation theorem.

