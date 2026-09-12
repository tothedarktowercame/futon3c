# Codex Scribe pattern library — f223

This pattern covers finite-order Taylor arguments in Mathlib when the goal is
a centered finite-difference estimate stated on a neighborhood filter.

## math-formalization/derive-centered-filter-estimates-from-finite-order-taylor-little-o

- **Trigger:** A `ContDiff` hypothesis of finite order is available, but the
  target is an `IsBigO` estimate at `nhds 0` for a centered expression using
  values at `x + h` and `x - h`.
- **Move:** Apply `taylor_isLittleO_univ`, compose its center-variable result
  with the translation `h ↦ x + h`, and normalize `taylorWithinEval` with
  `taylor_within_apply` and `iteratedDerivWithin_univ`.  Obtain the negative
  increment expansion by composing with `h ↦ -h`, add the two little-o
  remainders, weaken the sum to big-O, and add the highest surviving even
  Taylor term as a constant multiple of the comparison power.
- **Why it works:** Translation and negation tend to the required centers, so
  little-o is stable under both compositions.  Symmetric addition cancels all
  odd Taylor coefficients, while each remaining coefficient at the target
  order is explicitly big-O of the comparison monomial.
