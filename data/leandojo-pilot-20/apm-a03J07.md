

# APM a03J07: Maximum principle for the upper half-plane (Phragmén-Lindelöf)

## 1. Informal proof

**Why it's hard.** The upper half-plane is unbounded, so the classical maximum modulus principle for bounded domains does not apply directly. f is bounded on the boundary (the real axis) and bounded overall, but we cannot simply take "the boundary at infinity" — the maximum principle needs a compact domain with a full boundary.

**The key insight.** Introduce an auxiliary function that forces decay at infinity, apply the maximum principle on a large semicircular domain, then remove the auxiliary factor. The standard choice is f_ε(z) = f(z) · e^{iεz} for ε > 0, which decays in the upper half-plane (since |e^{iεz}| = e^{-εy} → 0 as y → ∞), then let ε → 0.

**Proof.**

Fix ε > 0 and define g_ε(z) = f(z) · e^{iεz}. Then:
- g_ε is continuous on H̄ and analytic on H.
- On the real axis (y = 0): |g_ε(x)| = |f(x)| · |e^{iεx}| = |f(x)| ≤ M.
- For z = x + iy with y > 0: |g_ε(z)| = |f(z)| · e^{-εy} ≤ B · e^{-εy}, where B = sup |f| (which is finite by the boundedness hypothesis).

Consider the semicircular region D_R = {z ∈ H : |z| < R}. Its boundary consists of the real segment [-R, R] and the upper semicircular arc C_R.

On [-R, R]: |g_ε| ≤ M (from the real-axis bound).

On C_R: |g_ε(Re^{iθ})| = |f(Re^{iθ})| · e^{-εR sin θ} ≤ B · e^{-εR sin θ}. Since sin θ > 0 for θ ∈ (0, π), this tends to 0 as R → ∞ for each fixed ε > 0. In particular, for R large enough, |g_ε| ≤ M on C_R.

By the maximum modulus principle on D_R (a bounded domain): |g_ε(z)| ≤ M for all z ∈ D_R.

Since R was arbitrary, |g_ε(z)| ≤ M for all z ∈ H, i.e., |f(z)| · e^{-εy} ≤ M.

Now let ε → 0⁺: e^{-εy} → 1 for each fixed z = x + iy, giving |f(z)| ≤ M. ∎

**What connects.** This is the simplest instance of the Phragmén-Lindelöf principle, which extends the maximum modulus principle to unbounded domains. The general principle says: if f is analytic on an unbounded domain, bounded on the boundary, and does not grow too fast at infinity, then the boundary bound holds throughout. The "not too fast" condition is automatically satisfied here because f is globally bounded. The auxiliary factor e^{iεz} (or e^{εiz²} in other variants) is the Phragmén-Lindelöf device — it penalises growth at infinity, allowing the maximum principle on large truncations, and is removed in a limiting argument. The result is fundamental in complex analysis and PDE: it underlies uniqueness theorems for the heat equation, the Carlson interpolation theorem, and the theory of Hardy spaces H^p(H).

## 2. Lean 4 theorem statement

```lean
import Mathlib.Analysis.Complex.AbsMax
import Mathlib.Analysis.Complex.Basic
import Mathlib.Analysis.SpecialFunctions.ExpDeriv
import Mathlib.Topology.ContinuousOn
import Mathlib.Order.Filter.Basic

open Complex Metric Set Filter Topology

noncomputable section

/-- The closed upper half-plane. -/
def upperHalfPlaneClosed : Set ℂ := {z : ℂ | 0 ≤ z.im}

/-- The open upper half-plane. -/
def upperHalfPlaneOpen : Set ℂ := {z : ℂ | 0 < z.im}

/-- The real axis as a subset of ℂ. -/
def realAxis : Set ℂ := {z : ℂ | z.im = 0}

/-- Main theorem: maximum principle for the upper half-plane.
    If f is continuous on the closed UHP, analytic on the open UHP,
    bounded overall, and |f| ≤ M on the real axis, then |f| ≤ M everywhere. -/
theorem max_principle_upper_half_plane
    {f : ℂ → ℂ} {M B : ℝ}
    (hf_cont : ContinuousOn f upperHalfPlaneClosed)
    (hf_holo : DifferentiableOn ℂ f upperHalfPlaneOpen)
    (hf_bounded : ∀ z ∈ upperHalfPlaneClosed, ‖f z‖ ≤ B)
    (hf_real : ∀ z : ℂ, z.im = 0 → ‖f z‖ ≤ M)
    (hM : 0 ≤ M) :
    ∀ z ∈ upperHalfPlaneClosed, ‖f z‖ ≤ M := by
  sorry

/-- Auxiliary function: g_ε(z) = f(z) · e^{iεz} decays in UHP. -/
lemma aux_function_decay
    {f : ℂ → ℂ} {B : ℝ} {ε : ℝ} (hε : 0 < ε)
    (hf_bounded : ∀ z ∈ upperHalfPlaneClosed, ‖f z‖ ≤ B)
    {R : ℝ} (hR : 0 < R) :
    ∀ z ∈ upperHalfPlaneClosed, ‖z‖ = R → z.im > 0 →
      ‖f z * exp (I * ε * z)‖ ≤ B * Real.exp (-ε * z.im) := by
  sorry

/-- On the semicircular arc, the auxiliary function is small for large R. -/
lemma aux_on_arc_small
    {f : ℂ → ℂ} {B M : ℝ} {ε : ℝ} (hε : 0 < ε) (hB : 0 ≤ B)
    (hf_bounded : ∀ z ∈ upperHalfPlaneClosed, ‖f z‖ ≤ B) :
    ∀ᶠ R in atTop, ∀ z ∈ upperHalfPlaneClosed, ‖z‖ = R →
      ‖f z * exp (I * ε * z)‖ ≤ M := by
  sorry

/-- Maximum modulus principle on a bounded domain (semicircular region). -/
lemma max_modulus_semicircle
    {g : ℂ → ℂ} {M : ℝ} {R : ℝ} (hR : 0 < R)
    (hg_cont : ContinuousOn g (closedBall 0 R ∩ upperHalfPlaneClosed))
    (hg_holo : DifferentiableOn ℂ g (ball 0 R ∩ upperHalfPlaneOpen))
    (hg_boundary : ∀ z ∈ frontier (ball 0 R ∩ upperHalfPlaneOpen) ∩
        (closedBall 0 R ∩ upperHalfPlaneClosed), ‖g z‖ ≤ M) :
    ∀ z ∈ closedBall 0 R ∩ upperHalfPlaneClosed, ‖g z‖ ≤ M := by
  sorry

/-- The auxiliary factor e^{iεz} has modulus e^{-εy} on the UHP. -/
lemma norm_exp_I_mul_eq
    {ε : ℝ} {z : ℂ} :
    ‖exp (I * ε * z)‖ = Real.exp (-ε * z.im) := by
  sorry

/-- As ε → 0⁺, the factor e^{-εy} → 1 for fixed y. -/
lemma exp_neg_eps_tendsto
    {y : ℝ} :
    Tendsto (fun ε : ℝ => Real.exp (-ε * y)) (𝓝[>] 0) (𝓝 1) := by
  sorry

end
```

## 3. Mathlib cross-references

**Types/structures:**
- `DifferentiableOn ℂ f S` — holomorphicity on an open set
- `ContinuousOn f S` — continuity on the closed upper half-plane
- `Complex.exp` — complex exponential
- `Complex.I` — imaginary unit
- `Complex.im`, `Complex.re` — real and imaginary parts
- `Metric.closedBall`, `Metric.ball` — disks for the semicircular truncation
- `Filter.𝓝[>]` — right-neighbourhood filter (for ε → 0⁺)
- `Set.inter` — intersection of half-plane with disk

**Key lemmas:**
- `Complex.norm_exp_ofReal_mul_I` / `Complex.abs_exp` — |e^{iεz}| = e^{-ε Im z} computation
- `Complex.isMaxOn_of_isCompact_of_continuousOn` — maximum modulus principle on compact sets (in `Mathlib.Analysis.Complex.AbsMax`)
- `Complex.norm_le_norm_of_forall_mem_frontier` — maximum principle: bound on boundary implies bound on interior
- `Real.exp_pos` — positivity of exp
- `Real.exp_le_exp` — monotonicity of exp
- `Real.tendsto_exp_neg_mul_atTop_nhds_zero` — e^{-εR sin θ} → 0 as R → ∞ for fixed ε, θ
- `ContinuousOn.mul` — product of continuous-on functions is continuous-on
- `DifferentiableOn.mul` — product of differentiable-on functions
- `differentiable_exp` — exp is entire
- `Differentiable.comp` — chain rule for differentiability
- `isCompact_closedBall` — closed balls are compact (for the truncation domain)
- `Filter.Eventually.mono` — manipulating eventually-holds statements
- `le_of_forall_pos_le_add` / `le_of_tendsto` — passing from |f|e^{-εy} ≤ M to |f| ≤ M as ε → 0

**Tactic hints:**
- `intro z hz` — fix a point in the upper half-plane
- `calc` — for the chain ‖f z‖ ≤ ‖f z‖ · e^{-εy} / e^{-εy} = ‖g_ε(z)‖ · e^{εy} ≤ M · e^{εy} and then ε → 0
- `gcongr` — for monotonicity-based norm bounds
- `positivity` — for ε > 0, R > 0, exp > 0
- `simp [Complex.norm_exp, Complex.mul_im]` — simplifying |e^{iεz}|
- `nlinarith` — combining inequalities in the ε → 0 limit
- `exact?` — finding the right maximum principle lemma
- `filter_upwards` — for the "for R large enough" arguments
- `continuity` — establishing continuity of the auxiliary function