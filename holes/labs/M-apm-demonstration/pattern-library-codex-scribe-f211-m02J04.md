# Codex Scribe pattern library — f211

These patterns cover assembling global variational inequalities from calibrated
regimes and turning a scalar derivative estimate into an endpoint comparison.

## math-informal/partition-global-optimization-by-an-attained-scalar-statistic

- **Trigger:** A global optimization problem has several lower-bound arguments,
  each valid only when a scalar statistic of the competitor lies in a specified
  range, and no single calibration is valid over all competitors.
- **Move:** Prove that the statistic is attained, partition its range at the
  calibration thresholds, and discharge each branch with its matching local
  bound.  Derive the weak and strict inequalities required by each branch from
  the same nested case split.
- **Why it works:** Attainment turns a function-space comparison into an
  exhaustive finite classification of one scalar value.  The local calibration
  lemmas remain independent, while the final global theorem becomes a small
  order-theoretic assembly rather than a new analytic argument.

## math-formalization/endpoint-comparison-from-a-derivative-sign-on-an-interval

- **Trigger:** A closed-form scalar calibration must be compared with its value
  at an endpoint, and its derivative has a sign only on the interior of a
  compact interval.
- **Move:** Use `antitoneOn_of_deriv_nonpos` (or the monotone analogue), supplying
  continuity on the closed interval, differentiability within the interior,
  and the derivative sign there.  Rewrite the derivative with a `HasDerivAt`
  witness before applying the scalar inequality, then compare the chosen point
  to the endpoint using membership in `Icc`.
- **Why it works:** The theorem packages the mean-value argument and exposes
  exactly the boundary regularity that is needed.  This avoids integrating the
  derivative or proving a bespoke endpoint inequality.
