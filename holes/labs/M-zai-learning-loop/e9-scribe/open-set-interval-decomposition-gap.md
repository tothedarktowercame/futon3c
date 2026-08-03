# Draft: open-set interval decomposition gap

- Requested memory level: `lemma-location`
- Status: draft; awaiting operator review
- Confidence: single observed problem (`n=1`)
- Problem: `a96J04`
- Git commit: `1d014930e85e1b9a8a21dfc197cad7876baf468a`
- Evidence IDs: `e-9bc8848e-8cec-4b65-879b-ae8ccccc9949`,
  `e-4b7ea8b6-0d88-49a8-bb7c-b76fcbf30bbc`,
  `e-d7b604b2-24e4-4703-ad17-1bd756df25fa`,
  `e-afcdc7ee-2507-473b-95ac-8e74c2a7de67`

## Proposed memory

For a null-set proof that needs an arbitrarily small open superset, the
available Mathlib route in the checked a96J04 environment reaches outer
regularity through `Set.exists_isOpen_lt_of_lt`. In this session, the next
desired bridge—representing an open subset of `ℝ` as a countable union of
pairwise-disjoint open intervals—was not found as a packaged lemma.

Treat those as two distinct API locations:

1. use `Mathlib.MeasureTheory.Measure.Regular` and
   `Set.exists_isOpen_lt_of_lt` for the small open cover;
2. separately search or construct the one-dimensional open-component
   decomposition before attempting the finite absolute-continuity assembly.

## Evidence comparison

The turn stream first probes outer-regularity APIs, then records a successful
null-set-cover direction and a direct Mathlib source search for the interval
decomposition. The committed boundary comment and proof outline preserve the
same break point. The final summary names
`Set.exists_isOpen_lt_of_lt` as the available side and the decomposition as
the sole remaining bridge.

## Honesty boundary

This is an observed blocking search result for the Mathlib revision at commit
time, not a proof that no such theorem or derivation exists. In particular,
the countable-rational-basis construction was **not investigated in this
session**. Do not promote the draft as “unroutable” or as a timeless claim
about Mathlib.
