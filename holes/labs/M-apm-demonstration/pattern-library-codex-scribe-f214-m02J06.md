# Codex Scribe pattern library — f214

These patterns cover compactness of function classes in `L¹` when Mathlib has
Arzelà--Ascoli but no packaged Kolmogorov--Riesz theorem, and closedness of
sets defined through representatives of `L¹` equivalence classes.

## math-formalization/transport-arzela-ascoli-nets-to-l1-by-common-support

- **Trigger:** A family of integrable functions has uniform translation
  control and common compact support, but the compactness goal lives in `Lp`
  and no direct Fréchet--Kolmogorov theorem is available.
- **Move:** At one fixed smoothing scale, restrict the smoothed functions to
  their common compact domain and package them as
  `BoundedContinuousFunction`s.  Prove equicontinuity and pointwise boundedness,
  use `BoundedContinuousFunction.arzela_ascoli` to obtain a totally bounded
  sup-norm family, and map its compact closure into `L¹` by zero extension.
  Prove that this map is Lipschitz using the finite measure of the common
  support, then combine its finite nets with the uniform smoothing error.
- **Why it works:** Uniform distance from the original family to a totally
  bounded approximating family implies total boundedness.  Common compact
  support supplies both a compact Arzelà--Ascoli domain and the inequality
  `‖F-G‖₁ ≤ volume(K) * ‖F-G‖∞` needed to transport nets into `L¹`.

## math-formalization/close-lp-representative-classes-via-invariant-bounds-and-ae-subsequence

- **Trigger:** A subset of `Lp` is defined by the existence of a representative
  satisfying norm, translation-modulus, and support conditions, so ordinary
  pointwise passage to a norm limit is unavailable.
- **Move:** Express the norm and translation conditions intrinsically in `Lp`,
  using a measure-preserving composition linear isometry for translations, and
  pass their inequalities to the limit by continuity.  Extract an almost
  everywhere convergent subsequence from `Lp` convergence to pass the common
  support condition.  Finally choose an indicator-truncated representative on
  the support set and use `MemLp.toLp_eq_toLp_iff` to identify its quotient
  class with the limit.
- **Why it works:** Norm and isometric-translation inequalities are closed
  conditions in `Lp`; almost-everywhere subsequence convergence preserves a
  common support off a null boundary; and indicator truncation turns the
  resulting a.e. support statement into a representative with literal
  pointwise support.
