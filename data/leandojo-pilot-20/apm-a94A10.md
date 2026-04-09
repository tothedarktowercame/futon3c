

# APM a94A10: f real iff z real implies f' never vanishes on ℝ

## 1. Informal proof

**Why it's hard.** The hypothesis "f(z) is real ⟺ z is real" is a topological condition on the preimage of ℝ under f, and connecting it to the non-vanishing of f' requires understanding how holomorphic maps behave near points where the derivative vanishes (they are locally n-to-1, which would create non-real points mapping to real values).

**The key insight.** If f'(x₀) = 0 at some real x₀, then f has a critical point: locally f(z) ≈ f(x₀) + c(z-x₀)ⁿ for some n ≥ 2. This n-to-1 local map sends n distinct branches near x₀ to the same real value. Since n ≥ 2, at least one branch goes into the upper half-plane while mapping to a real value — contradicting "f(z) real ⟹ z real."

**Proof.**

Suppose for contradiction that f'(x₀) = 0 for some x₀ ∈ ℝ. Since f is entire and nonconstant (a constant function has f(z) real for all z, violating "f(z) real ⟹ z real" unless the domain is restricted), f has a zero of order n ≥ 2 at x₀ for g(z) := f(z) - f(x₀). So:

g(z) = aₙ(z - x₀)ⁿ + aₙ₊₁(z - x₀)ⁿ⁺¹ + ···

with aₙ ≠ 0 and n ≥ 2.

Since f maps ℝ to ℝ (the forward direction of the hypothesis), f has real coefficients in its Taylor expansion at x₀ (by the Schwarz reflection principle, or directly: f⁽ᵏ⁾(x₀) ∈ ℝ for all k since f is real-valued on ℝ). So aₙ ∈ ℝ, aₙ ≠ 0.

The local mapping behaviour near x₀: g(z) = aₙ(z-x₀)ⁿ(1 + h(z)) where h(z) → 0, so g is locally an n-fold covering. Specifically, for small w near 0, the equation g(z) = w has exactly n solutions near x₀.

Now consider small real values w > 0 (or w < 0, choosing the sign so that w/aₙ > 0). The equation g(z) = w near x₀ has n solutions, which are approximately the n-th roots of w/aₙ:

z - x₀ ≈ (w/aₙ)^{1/n} · e^{2πik/n}, k = 0, 1, ..., n-1.

Since w/aₙ > 0, the n-th roots include (w/aₙ)^{1/n} · e^{2πi/n} for k = 1, which is not real (since n ≥ 2 and 2π/n ∈ (0, π)). This gives a non-real z with g(z) = w, hence f(z) = f(x₀) + w ∈ ℝ. But z ∉ ℝ — contradicting "f(z) real ⟹ z real." ∎

**What connects.** This is fundamentally about the local mapping theorem for holomorphic functions: near a critical point of order n, f is locally n-to-1, acting like z ↦ zⁿ. The hypothesis that f⁻¹(ℝ) = ℝ is a very strong topological constraint — it says the preimage of a line is a line. A holomorphic map that is locally n-to-1 (n ≥ 2) cannot preserve this linear structure because the n-fold branching sends multiple directions (including non-real ones) to the same real target. The result connects to the open mapping theorem (f is open, so f⁻¹(ℝ) being a real-analytic curve imposes rigidity), the Schwarz reflection principle (f has real coefficients), and the theory of branched coverings.

## 2. Lean 4 theorem statement

```lean
import Mathlib.Analysis.Complex.CauchyIntegral
import Mathlib.Analysis.Analytic.IsolatedZeros

open Complex

noncomputable section

/-- The "real iff real" hypothesis. -/
def RealIffReal (f : ℂ → ℂ) : Prop :=
  ∀ z : ℂ, (f z).im = 0 ↔ z.im = 0

/-- Main theorem: f'(x) ≠ 0 for all real x. -/
theorem deriv_ne_zero_on_reals
    {f : ℂ → ℂ}
    (hf : Differentiable ℂ f)
    (hreal : RealIffReal f) :
    ∀ x : ℝ, deriv f (↑x) ≠ 0 := by
  sorry

/-- f is nonconstant (follows from RealIffReal). -/
lemma nonconstant_of_realIffReal
    {f : ℂ → ℂ}
    (hf : Differentiable ℂ f)
    (hreal : RealIffReal f) :
    ¬∀ z w : ℂ, f z = f w := by
  sorry

/-- f has real Taylor coefficients at real points. -/
lemma taylor_coeff_real_at_real
    {f : ℂ → ℂ}
    (hf : Differentiable ℂ f)
    (hreal_fwd : ∀ x : ℝ, (f ↑x).im = 0)
    {x : ℝ} (n : ℕ) :
    (iteratedDeriv n f ↑x).im = 0 := by
  sorry

/-- Local n-to-1 structure at a critical point produces
    non-real preimages of real values. -/
lemma exists_nonreal_preimage_of_critical
    {f : ℂ → ℂ}
    (hf : Differentiable ℂ f)
    {x₀ : ℝ} (hcrit : deriv f ↑x₀ = 0)
    (hnc : ∃ n : ℕ, 2 ≤ n ∧ iteratedDeriv n f ↑x₀ ≠ 0) :
    ∃ z : ℂ, z.im ≠ 0 ∧ (f z).im = 0 := by
  sorry

/-- The order of vanishing of f - f(x₀) at x₀ is ≥ 2
    when f'(x₀) = 0. -/
lemma order_ge_two_of_deriv_zero
    {f : ℂ → ℂ}
    (hf : Differentiable ℂ f)
    (hf_nc : ¬∀ z, f z = f 0)
    {x₀ : ℝ} (hcrit : deriv f ↑x₀ = 0) :
    ∃ n : ℕ, 2 ≤ n ∧ iteratedDeriv n f ↑x₀ ≠ 0 ∧
      ∀ k, k < n → k ≥ 1 → iteratedDeriv k f ↑x₀ = 0 := by
  sorry

end
```

## 3. Mathlib cross-references

**Types/structures:**
- `Differentiable ℂ f` — entire function
- `Complex.im`, `Complex.re` — imaginary and real parts
- `Complex.ofReal` / `↑x` — embedding ℝ ↪ ℂ
- `iteratedDeriv n f z` — n-th derivative
- `deriv f z` — first derivative

**Key lemmas:**
- `AnalyticAt.order_eq_nat_iff` / `AnalyticAt.eventually_eq_zero_or_self` — order of vanishing at a point. In `Mathlib.Analysis.Analytic.IsolatedZeros`
- `AnalyticAt.eq_pow_mul` — local factorisation f(z) - f(z₀) = (z-z₀)ⁿ · h(z) with h(z₀) ≠ 0
- `Complex.ofReal_im` — (↑x).im = 0 (real embedding has zero imaginary part)
- `Complex.eq_conj_iff_real` — z is real iff z = z̄
- `Differentiable.iterate_deriv` — iterated derivatives of entire functions exist
- `Complex.conj_ofReal` — conjugation fixes reals (for Schwarz reflection)
- `DifferentiableAt.hasDerivAt` — extracting derivative data

**Tactic hints:**
- `by_contra h; push_neg at h` — assume f'(x₀) = 0 for contradiction
- `obtain ⟨n, hn, hne⟩ := order_ge_two_of_deriv_zero ...` — extract the order n ≥ 2
- `obtain ⟨z, hz_nonreal, hz_real_image⟩ := exists_nonreal_preimage_of_critical ...` — get the non-real preimage
- `exact absurd ((hreal z).mp hz_real_image) hz_nonreal` — final contradiction
- `simp [Complex.ext_iff, Complex.ofReal_im]` — relating real embedding to im = 0
- `nlinarith [Complex.sq_abs z]` — for norm computations in the local analysis
- `exact?` — finding the analytic order / isolated zeros lemma