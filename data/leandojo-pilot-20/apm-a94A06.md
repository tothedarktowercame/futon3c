

# APM a94A06: ∫₀^∞ x⁷/(1+x¹²) dx via pie-slice contour

## 1. Informal proof

**Why it's hard.** The integrand x⁷/(1+x¹²) is a rational function, but the degree 12 in the denominator means the poles are 12th roots of -1, distributed around the circle. A semicircular contour would enclose 6 poles — messy. The pie-slice trick exploits the rotational symmetry x ↦ xe^{iπ/6} of x¹² to relate the integral along the ray at angle π/6 back to the original integral, avoiding multiple residue computations.

**The key insight.** On the ray z = re^{iπ/6}, z¹² = r¹²e^{2πi} = r¹² and z⁷ = r⁷e^{7iπ/6}, so z⁷/(1+z¹²) = e^{7iπ/6} · r⁷/(1+r¹²). The integral along this ray is e^{7iπ/6} · e^{iπ/6} · I = e^{8iπ/6} · I where I is the original integral and the extra e^{iπ/6} comes from dz = e^{iπ/6}dr. The contour integral equals the real-axis piece minus the ray piece (accounting for direction), and the arc at infinity vanishes.

**Proof.**

*Step 1: Contour.* Let γ_R be the boundary of the pie-slice {z : 0 < arg z < π/6, |z| < R}, traversed counterclockwise:
- γ₁: real axis from 0 to R
- γ₂: arc from R to Re^{iπ/6}
- γ₃: ray from Re^{iπ/6} back to 0

Consider f(z) = z⁷/(1+z¹²).

*Step 2: Poles inside the contour.* The poles of f are the 12th roots of -1: z_k = e^{i(2k+1)π/12} for k = 0,...,11. In the sector 0 < arg z < π/6 = π/6, we need 0 < (2k+1)π/12 < π/6, i.e., 0 < 2k+1 < 2, so k = 0 only. The sole pole inside is z₀ = e^{iπ/12}.

*Step 3: Residue at z₀.* Since 1+z¹² has simple roots: Res(f, z₀) = z₀⁷/(12z₀¹¹) = 1/(12z₀⁴) = e^{-4iπ/12}/12 = e^{-iπ/3}/12.

*Step 4: Ray integral (γ₃).* Parametrise z = re^{iπ/6}, r from R to 0, dz = e^{iπ/6}dr:

∫_{γ₃} = ∫_R^0 (re^{iπ/6})⁷/(1+(re^{iπ/6})¹²) · e^{iπ/6} dr = -e^{8iπ/6} ∫_0^R r⁷/(1+r¹²) dr

since (e^{iπ/6})¹² = e^{2πi} = 1 and (e^{iπ/6})⁷ · e^{iπ/6} = e^{8iπ/6}.

*Step 5: Arc (γ₂) vanishes.* |f(Re^{iθ})| ≤ R⁷/(R¹²-1) = O(R⁻⁵), and arc length = πR/6, so |∫_{γ₂}| ≤ O(R⁻⁴) → 0.

*Step 6: Combine.* By the residue theorem:

∫_{γ₁} + ∫_{γ₂} + ∫_{γ₃} = 2πi · Res(f, z₀).

As R → ∞: I + 0 - e^{8iπ/6} I = 2πi · e^{-iπ/3}/12.

Now e^{8iπ/6} = e^{4iπ/3} = cos(4π/3) + i sin(4π/3) = -1/2 - i√3/2.

So I(1 - e^{4iπ/3}) = I(1+1/2+i√3/2) = I(3/2 + i√3/2).

And 2πi · e^{-iπ/3}/12 = (πi/6)(cos(π/3) - i sin(π/3)) = (πi/6)(1/2 - i√3/2) = (π/6)(√3/2 + i/2).

Equating real parts: I · 3/2 = π√3/12, so I = π√3/18 = π/(6√3).

Simplifying: I = π√3/18.

$$\boxed{\int_0^\infty \frac{x^7}{1+x^{12}}\,dx = \frac{\pi\sqrt{3}}{18} = \frac{\pi}{6\sqrt{3}}.}$$

**What connects.** This is a standard application of the pie-slice (sector) contour for integrals ∫₀^∞ xᵃ/(1+xⁿ) dx where the angle π/n is chosen so that the substitution z = re^{2πi/n} maps z^n back to r^n, reducing the ray integral to a multiple of the original. The general formula is ∫₀^∞ xᵃ/(1+xⁿ) dx = π/(n sin((a+1)π/n)) for -1 < a < n-1. Here a = 7, n = 12: π/(12 sin(8π/12)) = π/(12 sin(2π/3)) = π/(12 · √3/2) = π/(6√3) = π√3/18. ✓

## 2. Lean 4 theorem statement

```lean
import Mathlib.Analysis.SpecialFunctions.Integrals
import Mathlib.Analysis.SpecialFunctions.Trigonometric.Basic
import Mathlib.Analysis.Complex.CauchyIntegral
import Mathlib.MeasureTheory.Integral.IntervalIntegral

open Real MeasureTheory Set

noncomputable section

/-- Main result: ∫₀^∞ x⁷/(1+x¹²) dx = π√3/18. -/
theorem integral_x7_over_1_plus_x12 :
    Tendsto (fun R => ∫ x in (0:ℝ)..R, x ^ 7 / (1 + x ^ 12))
      Filter.atTop (𝓝 (π * sqrt 3 / 18)) := by
  sorry

/-- The sole pole in the sector 0 < arg z < π/6 is e^{iπ/12}. -/
lemma pole_in_sector :
    ∀ z : ℂ, 1 + z ^ 12 = 0 → 0 < z.arg → z.arg < π / 6 →
      z = Complex.exp (Complex.I * (π / 12)) := by
  sorry

/-- Residue computation: Res(z⁷/(1+z¹²), e^{iπ/12}) = e^{-iπ/3}/12. -/
lemma residue_at_pole :
    let z₀ := Complex.exp (Complex.I * (↑(π / 12) : ℂ))
    z₀ ^ 7 / (12 * z₀ ^ 11) =
      Complex.exp (-(Complex.I * (↑(π / 3) : ℂ))) / 12 := by
  sorry

/-- The arc integral vanishes as R → ∞. -/
lemma arc_integral_vanishes :
    Tendsto (fun R : ℝ =>
      ‖∫ θ in (0:ℝ)..(π/6),
        (R * Complex.exp (Complex.I * θ)) ^ 7 /
        (1 + (R * Complex.exp (Complex.I * θ)) ^ 12) *
        Complex.I * R * Complex.exp (Complex.I * θ)‖)
      Filter.atTop (𝓝 0) := by
  sorry

/-- The ray integral equals e^{8iπ/6} times the real integral. -/
lemma ray_integral_factor (R : ℝ) (hR : 0 < R) :
    ∫ r in (0:ℝ)..R,
      (r * Complex.exp (Complex.I * (π/6))) ^ 7 /
      (1 + (r * Complex.exp (Complex.I * (π/6))) ^ 12) *
      Complex.exp (Complex.I * (π/6)) =
    Complex.exp (Complex.I * (8 * π / 6)) *
      ∫ r in (0:ℝ)..R, ↑(r ^ 7 / (1 + r ^ 12)) := by
  sorry

/-- General formula: ∫₀^∞ xᵃ/(1+xⁿ) = π/(n sin((a+1)π/n))
    for -1 < a < n-1. -/
theorem integral_xa_over_1_plus_xn
    {a : ℝ} {n : ℕ} (hn : 1 ≤ n)
    (ha_low : -1 < a) (ha_high : a < ↑n - 1) :
    Tendsto (fun R => ∫ x in (0:ℝ)..R, x ^ a / (1 + x ^ (n : ℝ)))
      Filter.atTop (𝓝 (π / (↑n * sin ((a + 1) * π / ↑n)))) := by
  sorry

end
```

## 3. Mathlib cross-references

**Types/structures:**
- `Complex.exp`, `Complex.I` — complex exponential and imaginary unit
- `Complex.arg` — argument function
- `intervalIntegral` / `∫ x in a..b` — for the real integral
- `Filter.atTop` — for R → ∞ (improper integral)
- `Real.sqrt`, `Real.sin`, `Real.pi` — for the answer

**Key lemmas:**
- `Complex.circleIntegral_sub_inv_of_mem_ball` — Cauchy integral / residue formula in Mathlib
- `Complex.exp_mul_I` — e^{iθ} computation
- `Complex.exp_nat_mul` — (e^{iα})ⁿ = e^{inα}
- `Complex.norm_exp_ofReal_mul_I` — |e^{iθ}| = 1
- `intervalIntegral.integral_comp_mul_right` — substitution for parametrising the ray
- `Real.rpow_natCast` — x^n for real powers
- `norm_div_le` / `norm_pow` — for bounding the arc integrand
- `Real.sin_pi_div_three` — sin(π/3) = √3/2
- `Real.cos_pi_div_three` — cos(π/3) = 1/2
- `Real.sqrt_eq_iff_sq_eq` — for √3 manipulation

**Tactic hints:**
- `norm_num [Real.sin_pi_div_three, Real.sqrt]` — evaluating the final answer
- `ring` — for z¹² = (re^{iπ/6})¹² = r¹² · (e^{iπ/6})¹² = r¹²
- `field_simp` — clearing denominators in the residue
- `simp [Complex.exp_add, Complex.exp_mul_I]` — exponential arithmetic
- `calc` — for the chain equating contour integral to 2πi·Res
- `positivity` — for R > 0, π > 0, sin > 0
- `nlinarith [Real.sq_sqrt (show (3:ℝ) ≥ 0 by norm_num)]` — √3 arithmetic
- `gcongr` — for bounding the arc integral