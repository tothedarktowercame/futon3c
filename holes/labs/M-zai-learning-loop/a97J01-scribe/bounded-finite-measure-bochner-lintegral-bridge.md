# Memory: bounded finite-measure truncations bridge Bochner integrals to lintegrals

- Requested memory level: `lemma-location`
- Lane: mathematics
- Confidence: one compiled problem instance (`n=1`)
- Problem: `a97J01`
- Git commit: `9435997fcf4e083eb3c0a2574a77f3d462923be7`
- Jobs: `invoke-1785866168873-21-2ac2b12d`, `invoke-1785866229876-23-1398b8d9`, `invoke-1785873356249-26-88335238`
- Evidence IDs: `e-pull-offer-53df6095-828b-44e1-995e-ea894a387e7e`, `e-pull-offer-906e5d56-038e-4bb3-a805-261ad3752b6d`

## Memory

For a measurable real function bounded on a finite-measure measurable set
`s`, the useful Mathlib dependency chain is:

1. prove `volume s ≠ ∞`, commonly by `measure_mono` into a closed bounded
   interval followed by `measure_Icc_lt_top`;
2. apply `Measure.integrableOn_of_bounded` to obtain
   `Integrable f (volume.restrict s)`;
3. for an a.e. nonnegative restriction, use
   `ofReal_integral_eq_lintegral_ofReal` to identify the `ENNReal.ofReal` of
   the Bochner integral with the corresponding lintegral.

Compiled witness from `a97J01`:

```lean
have hA_finite : volume (A n) ≠ ∞ := by
  apply ne_of_lt
  exact (measure_mono (fun x hx => hx.1.1)).trans_lt measure_Icc_lt_top
have hf_int : Integrable f (volume.restrict (A n)) := by
  apply Measure.integrableOn_of_bounded hA_finite hf_meas.aestronglyMeasurable
  filter_upwards [ae_restrict_mem (hA_meas n)] with x hx
  exact hA_fbounded n |>.choose_spec x hx
```

This closes a missing dependency; it does not claim that other bounded-set
integrability routes are absent or unroutable.

## Demand-side vocabulary

This grounds the hungry query `Lebesgue integrable bounded integral set
measurable function L1` and the empty tag pull `integrability Lebesgue measure`.
Their literal terms are promotion tags.

## Honesty boundary

The exact chain is compiled once (`n=1`). Prior solved files demonstrated the
conversion theorem but do not establish a second instance of this complete
finite-measure boundedness chain.
