

# APM a95A08: Length of image curve ≥ 2π, with equality iff Δ ⊆ D

## 1. Informal proof

**Why it's hard.** The length of C = f(∂Δ) involves ∫|f'(e^{iθ})| dθ, which depends on the global behaviour of f' on the circle — not just at the origin. The conditions f(0) = 0 and |f'(0)| = 1 normalise f at one point, but controlling the boundary integral requires connecting the interior derivative to the boundary derivative.

**The key insight.** The length is ℓ(C) = ∫₀²π |f'(e^{iθ})| dθ. By Parseval/power series: f'(z) = Σ naₙz^{n-1} with a₁ = f'(0), |a₁| = 1. Then ∫₀²π |f'(e^{iθ})|² dθ = 2π Σ n²|aₙ|² ≥ 2π|a₁|² = 2π. By Cauchy-Schwarz: ℓ(C)² = (∫|f'|)² ≤ 2π ∫|f'|², but we need the reverse — ℓ(C) = ∫|f'| ≥ 2π|a₁| = 2π directly from the mean value property.

Actually the cleanest: ℓ(C) = ∫₀²π |f'(e^{iθ})| dθ ≥ |∫₀²π f'(e^{iθ}) dθ|... no, that doesn't directly give 2π. Instead use: by Cauchy-Schwarz, (∫₀²π 1 · |f'| dθ)² ≤ 2π · ∫₀²π |f'|² dθ — this goes the wrong way.

Better: ℓ(C) = ∫₀²π |f'(e^{iθ})| dθ. Since |f'(e^{iθ})| ≥ 0, by Jensen's inequality or direct computation: ℓ(C) ≥ 2π · exp((1/2π)∫₀²π log|f'(e^{iθ})| dθ). By Jensen applied to the subharmonic function log|f'|: (1/2π)∫log|f'(e^{iθ})|dθ ≥ log|f'(0)| = log 1 = 0. So ℓ(C) ≥ 2π · e⁰ = 2π. (This uses the AM-GM/Jensen inequality for the exponential function and the mean value property for subharmonic functions.)

**Proof.**

*Part 1: ℓ(C) ≥ 2π.*

The length of C is:

ℓ(C) = ∫₀²π |f'(e^{iθ})| dθ.

Since f' is analytic and nonvanishing on a neighbourhood of Δ̄ (f is univalent, so f' ≠ 0), log|f'| is harmonic on Δ. By the mean value property for harmonic functions:

log|f'(0)| = (1/2π) ∫₀²π log|f'(e^{iθ})| dθ.

Since |f'(0)| = 1: (1/2π) ∫₀²π log|f'(e^{iθ})| dθ = 0.

By Jensen's inequality (log is concave, so exp is convex):

(1/2π) ∫₀²π |f'(e^{iθ})| dθ ≥ exp((1/2π) ∫₀²π log|f'(e^{iθ})| dθ) = e⁰ = 1.

Therefore ℓ(C) = ∫₀²π |f'(e^{iθ})| dθ ≥ 2π · 1 = 2π. ✓

*Part 2: ℓ(C) = 2π ⟹ Δ ⊆ D.*

Equality in Jensen requires |f'(e^{iθ})| = constant = 1 for all θ. Then f maps ∂Δ isometrically to C, and |f'| = 1 on ∂Δ. By the maximum and minimum modulus principles (|f'| is harmonic-like: log|f'| is harmonic), |f'| ≡ 1 on Δ̄. So f is a rotation: f(z) = e^{iα}z for some α. Then D = f(Δ) = Δ, and Δ ⊆ D. ✓

Contrapositive of second statement: ℓ(C) > 2π ⟹ |f'| is not identically 1 on ∂Δ ⟹ f is not a rotation ⟹ D ≠ Δ, and since f(0) = 0 with |f'(0)| = 1 but f not a rotation, D must "bulge" somewhere, potentially not containing Δ. More precisely: if Δ ⊆ D, then f⁻¹ : D → Δ satisfies f⁻¹(Δ) ⊆ Δ, f⁻¹(0) = 0, |(f⁻¹)'(0)| = 1/|f'(0)| = 1. By the Schwarz lemma applied to both f⁻¹|_Δ and f|_Δ (if Δ ⊆ D), both are Δ-self-maps fixing 0 with |derivative| = 1, forcing f(z) = e^{iα}z, giving ℓ(C) = 2π. ✓ ∎

**What connects.** This combines Jensen's inequality for subharmonic functions (giving ℓ(C) ≥ 2π), the equality case of Jensen (characterising constant |f'|), and the Schwarz lemma (characterising isometries of the disk). The result is a sharp isoperimetric-type inequality for univalent functions: among all conformal maps of the disk normalised by f(0) = 0 and |f'(0)| = 1, the rotation has the shortest boundary curve, and it's the only map whose image contains the unit disk. This connects to the Bieberbach conjecture (de Branges' theorem) and the theory of schlicht functions, where extremal problems for univalent maps are central.

## 2. Lean 4 theorem statement

```lean
import Mathlib.Analysis.Complex.Schwarz
import Mathlib.Analysis.Complex.AbsMax
import Mathlib.Analysis.SpecialFunctions.Log.Basic
import Mathlib.MeasureTheory.Integral.IntervalIntegral

open Complex Metric Set Real MeasureTheory

noncomputable section

/-- The length of the image curve: ℓ(C) = ∫₀²π |f'(e^{iθ})| dθ. -/
def curveLength (f : ℂ → ℂ) : ℝ :=
  ∫ θ in (0:ℝ)..(2 * π), ‖deriv f (exp (I * ↑θ))‖

/-- Part 1: ℓ(C) ≥ 2π. -/
theorem curve_length_ge_two_pi
    {f : ℂ → ℂ}
    (hf_holo : ∃ ε > 0, DifferentiableOn ℂ f (closedBall 0 (1 + ε)))
    (hf_inj : InjOn f (closedBall 0 1))
    (hf_zero : f 0 = 0)
    (hf_deriv : ‖deriv f 0‖ = 1) :
    2 * π ≤ curveLength f := by
  sorry

/-- Part 2: ℓ(C) > 2π ⟹ Δ ⊄ f(Δ). -/
theorem disk_not_subset_image_of_length_gt
    {f : ℂ → ℂ}
    (hf_holo : ∃ ε > 0, DifferentiableOn ℂ f (closedBall 0 (1 + ε)))
    (hf_inj : InjOn f (closedBall 0 1))
    (hf_zero : f 0 = 0)
    (hf_deriv : ‖deriv f 0‖ = 1)
    (hlen : 2 * π < curveLength f) :
    ¬(ball (0 : ℂ) 1 ⊆ f '' ball 0 1) := by
  sorry

/-- Jensen's inequality for the exponential:
    (1/2π) ∫ |f'(e^{iθ})| dθ ≥ exp((1/2π) ∫ log|f'(e^{iθ})| dθ). -/
lemma jensen_exp_circle
    {g : ℝ → ℝ} (hg : IntegrableOn g (Icc 0 (2 * π)) volume)
    (hg_pos : ∀ θ ∈ Icc (0:ℝ) (2 * π), 0 < g θ) :
    exp ((1 / (2 * π)) * ∫ θ in (0:ℝ)..(2 * π), log (g θ)) ≤
      (1 / (2 * π)) * ∫ θ in (0:ℝ)..(2 * π), g θ := by
  sorry

/-- Mean value property: (1/2π) ∫ log|f'(e^{iθ})| dθ = log|f'(0)|
    when log|f'| is harmonic (f' ≠ 0). -/
lemma harmonic_mean_log_deriv
    {f : ℂ → ℂ}
    (hf : DifferentiableOn ℂ f (closedBall 0 1))
    (hf'_ne : ∀ z ∈ closedBall (0 : ℂ) 1, deriv f z ≠ 0) :
    (1 / (2 * π)) * ∫ θ in (0:ℝ)..(2 * π),
      log ‖deriv f (exp (I * ↑θ))‖ = log ‖deriv f 0‖ := by
  sorry

/-- Equality case: ℓ(C) = 2π iff f is a rotation (Schwarz). -/
lemma length_eq_iff_rotation
    {f : ℂ → ℂ}
    (hf_holo : ∃ ε > 0, DifferentiableOn ℂ f (closedBall 0 (1 + ε)))
    (hf_inj : InjOn f (closedBall 0 1))
    (hf_zero : f 0 = 0) (hf_deriv : ‖deriv f 0‖ = 1) :
    curveLength f = 2 * π ↔
      ∃ α : ℝ, ∀ z ∈ ball (0 : ℂ) 1, f z = exp (I * ↑α) * z := by
  sorry

/-- If Δ ⊆ D and f(0) = 0, |f'(0)| = 1, then f is a rotation (Schwarz). -/
lemma rotation_of_disk_subset_image
    {f : ℂ → ℂ}
    (hf_holo : DifferentiableOn ℂ f (ball 0 1))
    (hf_inj : InjOn f (ball 0 1))
    (hf_zero : f 0 = 0) (hf_deriv : ‖deriv f 0‖ = 1)
    (h_subset : ball (0 : ℂ) 1 ⊆ f '' ball 0 1) :
    ∃ α : ℝ, ∀ z ∈ ball (0 : ℂ) 1, f z = exp (I * ↑α) * z := by
  sorry

end
```

## 3. Mathlib cross-references

**Types/structures:**
- `DifferentiableOn ℂ f S` — analyticity on a region
- `InjOn f S` — injectivity on a set (univalence)
- `Metric.closedBall`, `Metric.ball`, `Metric.sphere` — disks and circle
- `intervalIntegral` — for ∫₀²π
- `Complex.exp`, `Complex.I` — for e^{iθ}

**Key lemmas:**
- `Complex.norm_le_norm_of_mapsTo_ball_self` — Schwarz lemma: |f(z)| ≤ |z| for disk self-maps fixing 0. The engine for the equality case
- `Complex.eq_smul_of_mapsTo_ball_self` — Schwarz equality: |f'(0)| = 1 ⟹ f(z) = cz
- `Real.add_pow_le_pow_mul_pow_of_sq_le_sq` — not directly, but Jensen's inequality. May need `MeasureTheory.integral_exp_le` or `ConvexOn.smul_le_integral`
- `ConvexOn.le_integral` — Jensen's inequality for convex functions (exp is convex, gives ∫|f'| ≥ 2π exp(∫log|f'|/2π))
- `Real.log_le_sub_one_of_le` — or related log/exp inequalities
- `Complex.circleIntegral_sub_inv_of_mem_ball` — mean value property for harmonic functions via Cauchy formula
- `norm_deriv_eq_one_of_injective_schwarz` — not a standard name but the content of the equality case

**Tactic hints:**
- `have hmvp := harmonic_mean_log_deriv ...` — apply mean value property
- `have hjensen := jensen_exp_circle ...` — apply Jensen
- `calc 2 * π ≤ ... ≤ curveLength f` — chain the bounds
- `nlinarith [Real.exp_zero]` — for e⁰ = 1 arithmetic
- `by_contra h_subset; push_neg at h_subset` — contrapositive for part 2
- `exact Complex.eq_smul_of_mapsTo_ball_self ...` — applying Schwarz equality
- `positivity` — for π > 0, ‖f'‖ > 0
- `simp [curveLength]` — unfolding the length definition