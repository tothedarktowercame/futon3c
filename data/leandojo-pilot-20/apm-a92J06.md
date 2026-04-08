

# APM a92J06: Trigonometric integral via contour integration

## 1. Informal proof

**Why it's hard.** The integral ∫₀²π 1/(2 + cos θ) dθ has no elementary antiderivative in terms of standard trig functions — the standard substitution t = tan(θ/2) works but is tedious. The contour integral method is more systematic but requires converting the trigonometric integral into a rational function of a complex variable.

**The key insight.** Substitute z = e^{iθ}, so dθ = dz/(iz) and cos θ = (z + z⁻¹)/2. The integral becomes a contour integral of a rational function around the unit circle, evaluated by the residue theorem.

**Proof.**

*Step 1: Substitution.* Let z = e^{iθ}, θ ∈ [0, 2π]. Then:
- dz = ie^{iθ} dθ = iz dθ, so dθ = dz/(iz).
- cos θ = (e^{iθ} + e^{-iθ})/2 = (z + 1/z)/2.

The integral becomes:

$$\int_0^{2\pi}\frac{d\theta}{2+\cos\theta} = \oint_{|z|=1}\frac{1}{2+(z+z^{-1})/2}\cdot\frac{dz}{iz}.$$

*Step 2: Simplify the integrand.* The denominator is:

$$2 + \frac{z+z^{-1}}{2} = \frac{4z+z^2+1}{2z} = \frac{z^2+4z+1}{2z}.$$

So:

$$\oint_{|z|=1}\frac{1}{\frac{z^2+4z+1}{2z}}\cdot\frac{dz}{iz} = \oint_{|z|=1}\frac{2z}{z^2+4z+1}\cdot\frac{dz}{iz} = \oint_{|z|=1}\frac{2}{i(z^2+4z+1)}\,dz.$$

*Step 3: Factor the denominator.* z² + 4z + 1 = 0 gives z = (-4 ± √(16-4))/2 = -2 ± √3.

The roots are z₁ = -2 + √3 ≈ -0.268 and z₂ = -2 - √3 ≈ -3.732.

Only z₁ = -2 + √3 lies inside the unit disk (|z₁| = 2 - √3 < 1).

*Step 4: Compute the residue.* The integrand is f(z) = 2/(i(z - z₁)(z - z₂)). The residue at z₁ is:

$$\text{Res}_{z=z_1} f = \frac{2}{i(z_1-z_2)} = \frac{2}{i((-2+\sqrt{3})-(-2-\sqrt{3}))} = \frac{2}{i \cdot 2\sqrt{3}} = \frac{1}{i\sqrt{3}}.$$

*Step 5: Apply the residue theorem.*

$$\oint_{|z|=1}f(z)\,dz = 2\pi i \cdot \frac{1}{i\sqrt{3}} = \frac{2\pi}{\sqrt{3}}.$$

$$\boxed{\int_0^{2\pi}\frac{d\theta}{2+\cos\theta} = \frac{2\pi}{\sqrt{3}}.}$$

**What connects.** This is the prototypical application of the residue theorem to trigonometric integrals. The method works for any integral of the form ∫₀²π R(cos θ, sin θ) dθ where R is a rational function: substitute z = e^{iθ}, convert to a contour integral of a rational function around the unit circle, locate the poles inside |z| < 1, and sum residues. The denominator z² + 4z + 1 factors with one root inside and one outside the disk because the product of roots is 1 (by Vieta) and neither has modulus 1 (since 2 + cos θ > 0 means the integrand has no singularities on the circle). The general formula for ∫₀²π dθ/(a + cos θ) = 2π/√(a² - 1) for a > 1 follows by the same method.

## 2. Lean 4 theorem statement

```lean
import Mathlib.Analysis.Complex.CauchyIntegral
import Mathlib.Analysis.SpecialFunctions.Trigonometric.Basic
import Mathlib.MeasureTheory.Integral.IntervalIntegral

open Complex Real MeasureTheory Set

noncomputable section

/-- Main result: ∫₀²π 1/(2 + cos θ) dθ = 2π/√3. -/
theorem integral_inv_two_plus_cos :
    ∫ θ in (0 : ℝ)..2 * π, 1 / (2 + cos θ) = 2 * π / sqrt 3 := by
  sorry

/-- The contour integral form: the substitution z = e^{iθ} converts the
    trigonometric integral to ∮_{|z|=1} 2/(i(z²+4z+1)) dz. -/
lemma trig_integral_eq_contour :
    ∫ θ in (0 : ℝ)..2 * π, 1 / (2 + cos θ) =
      (∮ z in C(0, 1), 2 / (I * (z ^ 2 + 4 * z + 1))).re := by
  sorry

/-- The quadratic z² + 4z + 1 has roots -2 ± √3. -/
lemma quadratic_roots :
    ∀ z : ℂ, z ^ 2 + 4 * z + 1 = (z - (-2 + sqrt 3)) * (z - (-2 - sqrt 3)) := by
  sorry

/-- Only z₁ = -2 + √3 lies inside the unit disk. -/
lemma root_inside_disk :
    ‖(-2 : ℂ) + (sqrt 3 : ℝ)‖ < 1 := by
  sorry

lemma root_outside_disk :
    1 < ‖(-2 : ℂ) - (sqrt 3 : ℝ)‖ := by
  sorry

/-- The residue at z₁ = -2 + √3 is 1/(i√3). -/
lemma residue_at_z1 :
    (2 : ℂ) / (I * ((-2 + sqrt 3 : ℝ) - (-2 - sqrt 3 : ℝ))) =
      1 / (I * sqrt 3) := by
  sorry

/-- Applying the residue theorem:
    ∮ f dz = 2πi · Res(f, z₁) = 2πi · 1/(i√3) = 2π/√3. -/
lemma contour_integral_by_residue :
    ∮ z in C(0, 1), 2 / (I * (z ^ 2 + 4 * z + 1)) =
      2 * π / sqrt 3 := by
  sorry

/-- General formula: ∫₀²π dθ/(a + cos θ) = 2π/√(a²-1) for a > 1. -/
theorem integral_inv_a_plus_cos {a : ℝ} (ha : 1 < a) :
    ∫ θ in (0 : ℝ)..2 * π, 1 / (a + cos θ) =
      2 * π / sqrt (a ^ 2 - 1) := by
  sorry

end
```

## 3. Mathlib cross-references

**Types/structures:**
- `Complex.I` — the imaginary unit
- `circleIntegral` / `∮ z in C(c, r), f z` — contour integral around a circle (in `Mathlib.Analysis.Complex.CauchyIntegral`)
- `intervalIntegral` / `∫ θ in a..b, f θ` — interval integral for the trigonometric form
- `Real.cos`, `Real.sin` — trigonometric functions
- `Real.sqrt` — square root

**Key lemmas:**
- `Complex.circleIntegral_sub_inv_of_mem_ball` — Cauchy integral formula / residue computation: ∮ 1/(z-a) dz = 2πi for a inside the circle
- `Complex.circleIntegral_eq_zero_of_differentiableOn` — Cauchy theorem: ∮ f = 0 when f is analytic inside (used for the root outside the disk)
- `Real.cos_eq_half_exp_add_exp_neg` — cos θ = (e^{iθ} + e^{-iθ})/2 (for the substitution)
- `Complex.exp_mul_I` — e^{iθ} computation
- `Real.sq_sqrt` — (√a)² = a for a ≥ 0
- `Real.sqrt_pos_of_pos` — √3 > 0
- `intervalIntegral.integral_comp_mul_deriv` — substitution rule for interval integrals
- `Complex.norm_eq_abs` — for checking |z₁| < 1
- `norm_sub_le` / `norm_add_le` — triangle inequality for root location

**Tactic hints:**
- `norm_num [Real.sqrt_lt']` — for verifying |-2+√3| < 1 and |-2-√3| > 1
- `ring` — for z²+4z+1 = (z-z₁)(z-z₂) and algebraic simplifications
- `field_simp` — for clearing denominators in the residue computation
- `simp [Complex.exp_mul_I, Complex.cos, Complex.sin]` — for the z = e^{iθ} substitution
- `rw [circleIntegral_sub_inv_of_mem_ball]` — applying the residue formula
- `positivity` — for 2 + cos θ > 0 (since cos θ ≥ -1) and √3 > 0
- `nlinarith [Real.sq_sqrt (by norm_num : (3:ℝ) ≥ 0)]` — for √3 arithmetic
- `push_cast` — for ℝ ↔ ℂ coercions