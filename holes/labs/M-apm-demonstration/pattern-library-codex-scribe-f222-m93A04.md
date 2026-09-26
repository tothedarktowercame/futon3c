# Codex Scribe pattern library — f222

This pattern covers Fréchet differentiability of parameterized nonlinear
composition operators valued in a sup-norm continuous-map space when the
available differentiation-under-the-integral lemmas only have a fixed
codomain.

## math-formalization/uniformize-scalar-remainders-on-a-compact-box-before-integrating

- **Trigger:** Pointwise scalar derivative hypotheses describe a nonlinear
  kernel, but the goal is a Fréchet derivative in a `ContinuousMap` sup norm,
  so pointwise differentiation under an integral does not provide the needed
  uniform remainder estimate.
- **Move:** Rewrite `HasFDerivAt` using
  `hasFDerivAt_iff_isLittleO_nhds_zero` and build a compact box containing the
  parameter domain and all values on short perturbation segments.  Obtain a
  uniform modulus for the scalar derivative on that box.  Subtract the
  linearization from each scalar section, apply a convex-segment mean-value
  estimate to bound its increment uniformly, and then pass this pointwise
  bound through `norm_integral_le_integral_norm`, `integral_mono`, and
  `ContinuousMap.norm_le`.
- **Why it works:** Compactness upgrades continuity of the derivative to one
  modulus valid for every parameter and every value encountered by a small
  sup-norm perturbation.  The mean-value estimate converts that modulus into
  a uniform Taylor remainder, while integration over a probability-measure
  domain preserves the same constant; the resulting bound is precisely the
  little-o condition in the target sup norm.
