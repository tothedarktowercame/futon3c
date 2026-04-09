

# APM a92J07: Entire function with imaginary part zero implying real part positive must be constant

## 1. Informal proof

**Why it's hard.** The Cauchy-Riemann equations u_x = v_y, u_y = -v_x tell us f = u + iv is entire (holomorphic on all of ℂ). The condition "v = 0 implies u > 0" is a topological constraint on the image of f — it says f never takes nonpositive real values. But converting "f omits a ray" into "f is constant" requires a non-trivial argument involving either Picard's theorem or a composition trick.

**The key insight.** The condition means f(ℂ) ⊆ ℂ \ (−∞, 0], so log ∘ f is a well-defined entire function (using the principal branch of log on ℂ \ (−∞, 0]). The real part of log f is log|f|, and the imaginary part is arg f ∈ (−π, π). Since Im(log f) is bounded, log f is constant by Liouville applied to e^{i · log f} (or directly), hence f is constant.

**Proof.**

*Step 1: f is entire.* Define f(z) = u(x,y) + iv(x,y) where z = x + iy. The Cauchy-Riemann equations u_x = v_y, u_y = -v_x hold everywhere, and u,v are C¹, so f is holomorphic on all of ℂ (entire).

*Step 2: f never takes values in (−∞, 0].* We need to show f(z) ∉ (−∞, 0] for any z. If f(z) is real and nonpositive, then v(x,y) = 0 and u(x,y) ≤ 0. But the hypothesis says v = 0 ⟹ u > 0, so u(x,y) > 0, contradiction. Also f(z) = 0 requires v = 0 and u = 0, contradicting u > 0. So f(ℂ) ⊆ ℂ \ (−∞, 0].

*Step 3: Compose with log.* The principal branch of log is holomorphic on ℂ \ (−∞, 0]. Since f(ℂ) ⊆ ℂ \ (−∞, 0], the composition g = log ∘ f is entire. We have g(z) = log|f(z)| + i · arg(f(z)) where arg ∈ (−π, π).

*Step 4: Apply Liouville.* The imaginary part of g satisfies |Im g| = |arg f| < π (bounded). Consider h(z) = e^{ig(z)}. Then |h(z)| = e^{-Im g(z)} ∈ (e^{-π}, e^π), so h is a bounded entire function. By Liouville's theorem, h is constant.

*Step 5: h constant implies g constant.* h(z) = e^{ig(z)} = C (constant). Differentiating: ig'(z)e^{ig(z)} = 0, so g'(z) = 0 for all z (since e^{ig} ≠ 0). Therefore g is constant.

*Step 6: Conclude.* g = log f constant implies f = e^g is constant. Since f = u + iv is constant, u and v are each constant. ∎

**What connects.** This problem is equivalent to: an entire function that omits all of (−∞, 0] (a ray) must be constant. This follows from the little Picard theorem (an entire function omitting two values is constant), since (−∞, 0] contains infinitely many omitted values. But the proof above avoids Picard by using the log composition trick, which reduces to Liouville. The same trick proves: if an entire function omits a ray, it is constant. More generally, if f(ℂ) misses any unbounded connected set, then a suitable branch of a multi-valued function composed with f produces a bounded entire function. The Cauchy-Riemann framing is just the real-variables way of saying "f is entire."

## 2. Lean 4 theorem statement

```lean
import Mathlib.Analysis.Complex.Liouville
import Mathlib.Analysis.Complex.CauchyIntegral
import Mathlib.Analysis.SpecialFunctions.Complex.Log
import Mathlib.Analysis.Calculus.Deriv.Basic

open Complex

noncomputable section

/-- The Cauchy-Riemann hypothesis: u_x = v_y and u_y = -v_x,
    with u, v continuously differentiable. -/
structure CauchyRiemann (u v : ℝ × ℝ → ℝ) : Prop where
  u_diff : ContDiff ℝ 1 u
  v_diff : ContDiff ℝ 1 v
  cr1 : ∀ p : ℝ × ℝ, fderiv ℝ u p (1, 0) = fderiv ℝ v p (0, 1)
  cr2 : ∀ p : ℝ × ℝ, fderiv ℝ u p (0, 1) = -(fderiv ℝ v p (1, 0))

/-- The topological constraint: v = 0 implies u > 0. -/
def ImageAvoidsRay (u v : ℝ × ℝ → ℝ) : Prop :=
  ∀ p : ℝ × ℝ, v p = 0 → 0 < u p

/-- Main theorem: u and v must be constant. -/
theorem cauchy_riemann_avoids_ray_implies_const
    {u v : ℝ × ℝ → ℝ}
    (hCR : CauchyRiemann u v)
    (hray : ImageAvoidsRay u v) :
    (∃ c₁ : ℝ, ∀ p, u p = c₁) ∧ (∃ c₂ : ℝ, ∀ p, v p = c₂) := by
  sorry

/-- Equivalent complex formulation: an entire function whose image
    avoids (−∞, 0] is constant. -/
theorem entire_avoids_ray_const
    {f : ℂ → ℂ}
    (hf : Differentiable ℂ f)
    (havoids : ∀ z, f z ∉ Set.Iic (0 : ℝ) ∨ (f z).im ≠ 0) :
    ∃ c : ℂ, ∀ z, f z = c := by
  sorry

/-- Cleaner: f(ℂ) ⊆ ℂ \ (−∞, 0] implies f constant. -/
theorem entire_omits_ray_const
    {f : ℂ → ℂ}
    (hf : Differentiable ℂ f)
    (homit : ∀ z, ¬((f z).im = 0 ∧ (f z).re ≤ 0)) :
    ∃ c : ℂ, ∀ z, f z = c := by
  sorry

/-- Step 2: f never hits (−∞, 0] under the CR + ray hypothesis. -/
lemma image_avoids_nonpositive_reals
    {u v : ℝ × ℝ → ℝ}
    (hray : ImageAvoidsRay u v) :
    ∀ p : ℝ × ℝ, ¬(v p = 0 ∧ u p ≤ 0) := by
  sorry

/-- Step 3: log ∘ f is entire when f omits (−∞, 0]. -/
lemma log_comp_entire
    {f : ℂ → ℂ}
    (hf : Differentiable ℂ f)
    (homit : ∀ z, ¬((f z).im = 0 ∧ (f z).re ≤ 0)) :
    Differentiable ℂ (Complex.log ∘ f) := by
  sorry

/-- Step 4: Im(log f) is bounded by π. -/
lemma im_log_bounded
    {f : ℂ → ℂ}
    (homit : ∀ z, ¬((f z).im = 0 ∧ (f z).re ≤ 0)) :
    ∀ z, |(Complex.log (f z)).im| < π := by
  sorry

/-- Step 4-5: e^{i·log f} is bounded entire, hence constant by Liouville. -/
lemma exp_i_log_bounded_entire
    {f : ℂ → ℂ}
    (hf : Differentiable ℂ f)
    (homit : ∀ z, ¬((f z).im = 0 ∧ (f z).re ≤ 0)) :
    ∃ M, ∀ z, ‖exp (I * Complex.log (f z))‖ ≤ M := by
  sorry

end
```

## 3. Mathlib cross-references

**Types/structures:**
- `Differentiable ℂ f` — entire function (complex differentiable everywhere)
- `ContDiff ℝ 1 u` — continuously differentiable (C¹)
- `fderiv ℝ u p` — Fréchet derivative of u at p (for Cauchy-Riemann in real form)
- `Complex.log` — principal branch of complex logarithm (holomorphic on ℂ \ (−∞, 0])
- `Complex.exp`, `Complex.I` — exponential and imaginary unit
- `Complex.im`, `Complex.re` — real and imaginary parts

**Key lemmas:**
- `Complex.differentiable_log` — log is holomorphic on ℂ \ (−∞, 0]. In `Mathlib.Analysis.SpecialFunctions.Complex.Log`
- `Differentiable.comp` — composition of differentiable functions (for log ∘ f)
- `Complex.Polynomial.liouville` / `Differentiable.apply_eq_apply_of_bounded` — Liouville's theorem. In `Mathlib.Analysis.Complex.Liouville`
- `Complex.abs_exp` — |e^z| = e^{Re z} (for bounding |e^{i·log f}|)
- `Complex.log_im` — Im(log z) = arg z ∈ (−π, π]
- `Complex.arg_lt_pi` / `Complex.neg_pi_lt_arg` — bounds on the argument
- `Differentiable.const_mul` — differentiability of scalar multiplication
- `Complex.exp_ne_zero` — e^z ≠ 0 (for the chain rule step)
- `hasDerivAt_log` — derivative of log at points away from the branch cut

**Tactic hints:**
- `intro p ⟨hv, hu⟩` — unpacking the ray-avoidance negation
- `exact absurd (hray p hv) (not_lt.mpr hu)` — deriving contradiction from v=0, u≤0
- `refine (Differentiable.comp ?_ hf)` — for log ∘ f differentiability
- `simp [Complex.abs_exp, Complex.mul_im]` — simplifying |e^{i·log f}|
- `exact ⟨Real.exp π, fun z => ...⟩` — providing the Liouville bound
- `nlinarith [Complex.neg_pi_lt_arg (f z), Complex.arg_le_pi (f z)]` — for |arg| < π
- `exact?` — finding Liouville's theorem
- `ext` — for showing f constant implies u, v constant (component-wise)
- `linarith` — combining the inequalities in the bounded argument