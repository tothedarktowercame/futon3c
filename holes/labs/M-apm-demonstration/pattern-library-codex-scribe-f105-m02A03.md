# Codex Scribe pattern library — f105

These patterns were added because the reviewed mathematics-memory search found
no coherent parent for passing a regularized singular kernel through an
integral or for constructing its parameter-independent logarithmic dominator.

## math-formalization/pass-a-regularized-singular-kernel-through-an-integral

- **Trigger:** A smooth regularization converges pointwise away from one
  singular point, and the target is convergence of its pairing with a compactly
  supported continuous function.
- **Move:** Use dominated convergence rather than trying to strengthen
  pointwise convergence at the singularity.  Discard the singular singleton
  almost everywhere, prove eventual measurability for the smooth
  regularizations, and dominate all sufficiently small regularization
  parameters by an integrable function built from the limiting singular
  kernel plus a compactly supported error term.
- **Why it works:** In a finite-dimensional real normed space a singleton has
  volume zero, so failure of pointwise convergence at the pole is irrelevant.
  Local integrability of the singular kernel and compact support of the test
  factor make the parameter-independent bound globally integrable.

## math-formalization/dominate-logarithmic-regularizations-by-the-limit-kernel

- **Trigger:** Dominated convergence requires a bound uniform in a parameter
  for expressions of the form `|log (r ^ 2 + ε ^ 2)|`, with `r > 0` and
  `0 ≤ ε ≤ 1`.
- **Move:** Split at `r ≤ 1`.  Near zero, sandwich the logarithm between
  `2 * log r` and `log 2`; away from zero, use
  `r ^ 2 + ε ^ 2 ≤ 2 * r ^ 2` and expand the logarithm of the product.
  Convert the two-sided estimates to an absolute-value bound only after the
  sign of `log r` is known.
- **Why it works:** The resulting estimate
  `|log (r ^ 2 + ε ^ 2)| ≤ 2 * |log r| + log 2` separates the integrable
  limiting logarithmic singularity from a bounded error independent of the
  regularization parameter.
