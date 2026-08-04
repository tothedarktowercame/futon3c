# Memory: Steinhaus by positive convolution and an open positive locus

- Requested memory level: `strategy`
- Lane: mathematics
- Confidence: one compiled problem instance (`n=1`)
- Problem: `a96J02`
- Git commit: `318160d89257eab8482e8066e284afb91a7ec6ac`
- Jobs: `invoke-1785854903455-984-ebd8887d`,
  `invoke-1785855266102-987-05ed970b`,
  `invoke-1785856907892-990-3ec676f1`
- Evidence IDs: `e-d1537c6c-8cef-4c79-8d5a-f42ab3816728`,
  `e-7fda96a7-4653-4e29-92bc-eab906ac784d`,
  `e-80de2c3d-3c71-4b9b-9090-b994124581fa`,
  `e-de860d2b-ae07-4cd4-bb5f-e280384d0563`

## Memory

For measurable positive-measure `A B : Set ℝ`, a compiled Steinhaus route
is:

1. intersect each set with a sufficiently large closed bounded interval to
   obtain positive finite-measure subsets `A'` and `B'`;
2. define the indicator convolution
   `F x = ∫ t, 1_A' t * 1_B' (x - t)`;
3. identify `F x` with `volume.real (A' ∩ (x - B'))` and prove continuity
   through symmetric-difference translation continuity;
4. use `integral_convolution` and `integral_indicator_one` to obtain
   `∫ x, F x = volume.real A' * volume.real B' > 0`;
5. conclude `F x > 0` somewhere, take a metric ball in the open set
   `{x | 0 < F x}`, and turn every positive overlap into a witness that
   `x ∈ A' + B' ⊆ A + B`.

This separates the analytic bridge (continuity), the global positivity
certificate (Fubini/convolution integral), and the pointwise sumset witness.

## Application rule

For a positive-measure sumset or overlap problem, look for a nonnegative
continuous correlation with a provably positive integral. Its positive locus
is open and nonempty; then prove directly that positivity implies membership
in the target set.

## Demand-side vocabulary

This memory grounds two hungry queries, whose literal vocabulary is carried
as tags: `Steinhaus theorem sumset measurable sets positive measure open
interval` and `sumset Minkowski sum additive combinatorics measure theory`.

## Honesty boundary

This strategy has one compiled instance (`n=1`). The informal neighboring
problem a99J02 confirmed the mathematical direction but supplied no Lean
artifact, so this is not an `n=2` cross-problem reuse claim.

