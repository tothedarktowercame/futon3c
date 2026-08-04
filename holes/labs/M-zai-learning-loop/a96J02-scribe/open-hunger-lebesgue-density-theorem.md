# Open hunger: Lebesgue-density theorem API for measurable sets

- Requested memory level: `open-hunger`
- Lane: mathematics / retrieval demand
- Confidence: one unmet query in one chain (`n=1`)
- Problem: `a96J02`
- Git commit: `318160d89257eab8482e8066e284afb91a7ec6ac`
- Jobs: `invoke-1785854903455-984-ebd8887d`,
  `invoke-1785856907892-990-3ec676f1`
- Evidence IDs: `e-7fda96a7-4653-4e29-92bc-eab906ac784d`,
  `e-de860d2b-ae07-4cd4-bb5f-e280384d0563`

## Hungry query

Literal query: `Lebesgue density theorem measurable set point of density`.

Proof stage: phase-A route reconnaissance for proving that a positive-measure
sumset contains an interval. The query sought a reusable Mathlib location and
application shape for obtaining a density point of a measurable subset of
`ℝ`.

## Outcome

The query returned no relevant memory. The closer later confirmed that
Mathlib has density/regularity APIs, but did not identify, exercise, or compile
a density-point route because the symmetric-difference convolution route had
already closed the theorem. Therefore the original demand is still open.

## What a future grounding should contain

A useful answer should name the exact Mathlib declaration(s), state their
measure/topology hypotheses, and include a small compiled witness that turns
positive measure into a usable density point on `ℝ`. It should also explain
whether that result alone suffices for a Steinhaus proof or what local overlap
estimate remains.

## Honesty boundary

This records unmet retrieval demand, not an assertion that Mathlib lacks the
theorem. The closer's consultation explicitly says density-theorem APIs exist;
they were discarded as unnecessary and were not investigated to the level
needed to answer this query.

