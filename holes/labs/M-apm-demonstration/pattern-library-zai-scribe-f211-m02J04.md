# Zai Scribe pattern library — f211 (m02J04 student arc)

Added because the reviewed mathematics-memory search returned no candidates
(receipt a34cf01ee56ad68e9fd0e675902c790d7d9f49c6835aaba3eb8f96f5798f713a),
so no existing pattern coherently parents this round's self-corrections.

## math-formalization/check-namespace-and-existence-before-bare-name

- **Trigger:** You reach for a lemma by a bare identifier (`contDiff_cosh`,
  `cosh_nonneg`) or retype a formula by string, and the elaborator answers
  `unknownIdentifier` / `unknown constant`.
- **Move:** Probe the identifier with `#check @name` and try the `Real.`
  namespace before anything else — unnamespaced hyperbolic identifiers are
  often Complex-only while the real version lives under `Real.`. If the
  guessed name truly does not exist (`Real.cosh_nonneg` is absent in current
  Mathlib), re-derive the fact from what does exist, e.g. nonnegativity of
  cosh from `zero_le_one.trans (Real.one_le_cosh x)`. Do not run
  `simp [Real.cosh]` on such goals: unfolding the definition loops
  (`Real.cosh.eq_1` is flagged as possibly looping).
- **Why it works:** Name errors are namespace or version facts, not
  mathematics; a one-line probe settles them, while `simp` on the unfolded
  definition fights the equation lemmas instead.

## math-formalization/probe-lemma-signatures-before-proof-assembly

- **Trigger:** You are assembling a chain of `ContDiff`/`Differentiable`
  facts and dot-notation fields fail (`Invalid field contDiff_deriv`), or a
  weakening like `ContDiff.of_le` is asked to change the *function* rather
  than the exponent.
- **Move:** `#check` each candidate lemma before composing. Current-Mathlib
  facts worth knowing: `ContDiff.deriv` does not exist — it is
  `ContDiff.deriv'`; `ContDiffAt.differentiableAt` takes `n ≠ 0` (not
  `1 ≤ n`); `Differentiable ℝ (deriv f)` for `ContDiff ℝ 2 f` comes from
  `ContDiff.differentiable_deriv_two`, not from `of_le`. `of_le` only
  lowers the smoothness index; it never transports differentiability from
  `f` to `deriv f`.
- **Why it works:** These are signature facts; guessing them produces
  type-mismatch noise that reads like a mathematical obstacle.

## math-formalization/supply-explicit-named-arguments-under-higher-order-unification

- **Trigger:** A composition lemma (`HasFDerivAt.comp_hasDerivAt`) reports
  `Application type mismatch … metavariables` although the shapes look
  right; the explicit point argument may come *first* in the signature.
- **Move:** Supply the base point and the outer function as named
  arguments — `(x := u)` for the point, `(l := fun p : ℝ × ℝ ↦ L p.1 p.2)`
  to fix the composition against a curried-looking goal. Accept that a
  `HasDerivAt.mul` (or similar algebraic lemma) produces its function in
  `Pi.mul` form: finish with `funext t; simp [Pi.mul_apply]` rather than
  fighting `exact` against the lambda form.
- **Why it works:** Higher-order unification cannot infer which lambda the
  composed function should be; naming the metavariables once is cheaper
  than reordering explicit arguments by trial.

## math-formalization/hand-build-multivariate-chain-rules-by-sections

- **Trigger:** You need `fderiv` of an uncurried `ℝ × ℝ → ℝ` function on a
  pair expressed through its partial derivatives, and no Mathlib lemma
  evaluates `fderiv` on a pair via partials; destructuring
  `DifferentiableAt.hasFDerivAt` with `obtain ⟨f, hf⟩` fails because it is
  a direct `HasFDerivAt`, not an `Exists`.
- **Move:** Use `DifferentiableAt.hasFDerivAt` as-is, then recover
  `ℓ (v, w) = v • ℓ (1, 0) + w • ℓ (0, 1)` by differentiating the two
  *sections* (`fun p ↦ ℓ (p, w)`, `fun p ↦ ℓ (v, p)`) with
  `hasFDerivAt_prodMk_left/right … |>.hasDerivAt` composed via
  `comp_hasDerivAt`, and finishing with `map_smul`/`map_add` linearity.
  This converts partial `deriv`s of sliced functions into components of the
  `fderiv` on the product.
- **Why it works:** Uniqueness of the derivative on each section pins the
  two components; smoothness hypotheses on the original two-variable map
  supply the section derivatives.

## math-formalization/verify-claimed-absence-against-the-actual-namespace

- **Trigger:** A plan is abandoned, or scoped as multi-session, because
  "Mathlib has no lemma for X" — especially differentiation under an
  interval integral or a fundamental-lemma-of-calculus-of-variations step.
- **Move:** Before budgeting around the absence, search the specific file
  and namespace: parametric differentiation under `intervalIntegral`
  *does* exist (`intervalIntegral.hasDerivAt_integral_of_dominated_loc_of_deriv_le`
  in `Analysis/Calculus/ParametricIntervalIntegral`). What is genuinely
  absent is the du Bois-Reymond / fundamental lemma of the calculus of
  variations — that must be built per problem from
  `intervalIntegral.integral_eq_zero_iff_of_le_of_nonneg_ae` with sign and
  truncation test functions. Absent-theorem and wrong-namespace are
  different residuals; only the first justifies a multi-session scope.
- **Why it works:** One absence claim congeals into a plan; a targeted
  namespace check either reopens the cheap route or correctly licenses the
  expensive one.
