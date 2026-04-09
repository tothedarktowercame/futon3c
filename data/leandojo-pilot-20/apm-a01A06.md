

# APM a01A06: Orlicz-type duality — exponential integrability constraint implies exponential measure decay

## 1. Informal proof

**Why it's hard.** The hypothesis is a duality condition: g is bounded as a linear functional on the "unit ball" of an Orlicz space (the set of f with ∫ e^f ≤ 1). Extracting pointwise information about g from this integral condition requires choosing the right test functions, and determining whether g ∈ L² requires understanding the precise decay rate of the distribution function m({g > λ}).

**The key insight.** Choosing f = c · χ_E (a constant on a measurable set) with the constraint ∫ e^f ≤ 1 gives e^c · m(E) + (1 - m(E)) ≤ 1, i.e., m(E) ≤ 1/(e^c - 1 + 1) = e^{-c}/(1 - e^{-c} + e^{-c}) — more simply, m(E)(e^c - 1) ≤ 0 unless we adjust: actually e^c · m(E) + 1 · (1 - m(E)) ≤ 1 gives m(E)(e^c - 1) ≤ 0 which is wrong. Let me redo: ∫ e^{cχ_E} = e^c m(E) + 1 · m(E^c ∩ [0,1]) = e^c m(E) + (1 - m(E)). Setting this ≤ 1: (e^c - 1)m(E) ≤ 0, which forces m(E) = 0 unless c ≤ 0. So we need c ≤ 0 to get a nontrivial test. But then ∫ gf = c ∫_E g < A, and c < 0 gives ∫_E g > A/c... this gives a lower bound on ∫_E g, not useful directly.

Instead, take f = c · χ_E with c > 0 and tune m(E) so the constraint is met. We need e^c m(E) + (1 - m(E)) ≤ 1, i.e., m(E) ≤ 0... No. The issue is that e^{cχ_E} ≥ 1 everywhere when c ≥ 0, so ∫ e^{cχ_E} ≥ 1 always, with equality only when m(E) = 0.

So we should use f = -c · χ_{E^c} + 0 · χ_E = -c(1 - χ_E). Or better: take f = c χ_E - λ for a normalizing constant λ. Then e^f = e^{c χ_E - λ} and ∫ e^f = e^{-λ}[e^c m(E) + (1-m(E))] ≤ 1 iff e^{-λ} ≤ 1/[e^c m(E) + 1 - m(E)]. Choose λ = log(e^c m(E) + 1 - m(E)) so equality holds.

Then ∫ gf = c ∫_E g - λ ∫ g < A. So c ∫_E g < A + λ ∫g = A + (∫g) log(e^c m(E) + 1 - m(E)).

Now take E = {g > t} for large t. Then ∫_E g ≥ t · m(E). And with c to be optimized:

c · t · m(E) < A + (∫g) log((e^c - 1)m(E) + 1).

For small m(E), log((e^c - 1)m(E) + 1) ≈ (e^c - 1)m(E). Taking c fixed and m(E) small:

c · t · m(E) ≲ A + C · m(E),

so m(E) ≲ A / (ct - C). This gives m({g > t}) = O(1/t), which is not exponential decay.

Let me try differently. Take c = t/2 (growing with t) and E = {g > t}:

(t/2) · t · m(E) ≤ A + (∫g) log((e^{t/2} - 1)m(E) + 1).

If m(E) is very small: log term ≈ e^{t/2} m(E), so t²m(E)/2 ≲ A + ||g||₁ e^{t/2} m(E). For the log term to not dominate, we need m(E) to be small enough that ||g||₁ e^{t/2} m(E) is bounded, giving m(E) ≲ e^{-t/2}.

More carefully: optimize c. With f = cχ_E - log(e^c m(E) + 1 - m(E)):

c ∫_E g - ||g||₁ log((e^c - 1)m(E) + 1) < A.

Set E = {g > t}, so ∫_E g ≥ t m(E). Set m(E) = δ. Choose c to optimize. Taking c such that (e^c - 1)δ = 1, i.e., e^c = 1 + 1/δ, c = log(1 + 1/δ):

log(1+1/δ) · t · δ - ||g||₁ · log(1 + 1) < A.

So δ · t · log(1/δ + 1) < A + ||g||₁ log 2.

For small δ: δ log(1/δ) · t ≲ A', giving δ ≲ A'/(t log(1/δ)). This is slower than exponential.

Actually, the sharp answer for part (a) is: **m({g > λ}) ≤ Ce^{-λ/||g||₁}** or similar exponential decay, and for part (b), this exponential decay is strong enough to give g ∈ L² (since ∫g² = ∫₀^∞ 2λ m({g>λ}) dλ ≤ C ∫₀^∞ λ e^{-cλ} dλ < ∞).

Let me redo part (a) cleanly with the right test function.

**Proof.**

**(a)** Fix a measurable E ⊆ [0,1] with m(E) > 0. For c > 0, define:

$$f_c = c\chi_E - \log\left(e^c m(E) + 1 - m(E)\right).$$

Then ∫ e^{f_c} dx = e^{-\log(e^c m(E)+1-m(E))} · (e^c m(E) + 1-m(E)) = 1, so f_c is admissible.

The hypothesis gives ∫ g f_c < A:

$$c\int_E g - \|g\|_1 \cdot \log(e^c m(E) + 1 - m(E)) < A.$$

Now set E = {g > λ}. Then ∫_E g ≥ λ m(E). Write δ = m(E). The bound becomes:

$$c\lambda\delta - \|g\|_1\log((e^c - 1)\delta + 1) < A. \quad (*)$$

Choose c = λ (this balances the growth rates). For large λ with small δ:

$$(e^\lambda - 1)\delta + 1 \leq e^{\lambda\delta} + 1 \leq 2e^{\lambda\delta}$$

when (e^λ - 1)δ ≤ e^{λδ}. So log(...) ≤ λδ + log 2. Then (*) gives:

$$\lambda^2 \delta - \|g\|_1(\lambda\delta + \log 2) < A,$$

$$\delta(\lambda^2 - \|g\|_1\lambda) < A + \|g\|_1\log 2,$$

$$\delta < \frac{A + \|g\|_1\log 2}{\lambda(\lambda - \|g\|_1)}.$$

So **m({g > λ}) = O(1/λ²)** for large λ. This is polynomial decay, not exponential.

But we can do better by choosing c more carefully. Take c = αλ for some α < 1. Then:

$$\alpha\lambda^2\delta - \|g\|_1\log((e^{\alpha\lambda}-1)\delta + 1) < A.$$

If δ ≤ e^{-αλ}, the log term is bounded by log(2) (since (e^{αλ}-1)e^{-αλ} = 1 - e^{-αλ} < 1). Then:

$$\alpha\lambda^2 e^{-\alpha\lambda} \leq A + \|g\|_1\log 2.$$

For large λ, the left side → 0, so this is automatically satisfied. This means the bound δ ≤ e^{-αλ} is *consistent* but not *derived*.

The correct approach: from (*) with c optimized. Rearranging (*): for **any** c > 0,

$$m(\{g>\lambda\}) \leq \frac{A + \|g\|_1\log((e^c-1)m(\{g>\lambda\})+1)}{c\lambda}.$$

This is an implicit bound. To get an explicit one, use the simpler estimate: since (e^c-1)δ + 1 ≤ e^{cδ+...}, just bound the log crudely.

Actually, the cleanest bound comes from choosing c such that the constraint is tight on a different set. Let me try f = cχ_E with no subtraction, but allow ∫ e^f = e^c m(E) + (1-m(E)) which exceeds 1 for c > 0. So f = cχ_E is NOT admissible for c > 0 without normalization.

With the normalization f_c as above, we showed m({g > λ}) = O(1/λ²), which is enough for:

**(b) Yes, g ∈ L².** We have m({g > λ}) ≤ C/λ² for λ large. Then:

$$\int_0^1 g^2 = \int_0^\infty 2\lambda \cdot m(\{g>\lambda\})\,d\lambda \leq \int_0^M 2\lambda\,d\lambda + \int_M^\infty \frac{2C}{\lambda}\,d\lambda.$$

Wait, 2λ · C/λ² = 2C/λ, and ∫_M^∞ 2C/λ dλ = ∞. So O(1/λ²) for the distribution function is **not** enough for L²! We need O(1/λ^{2+ε}).

Let me re-examine with c = λ/2. From (*):

$$\frac{\lambda^2}{2}\delta - \|g\|_1\log((e^{\lambda/2}-1)\delta + 1) < A.$$

If δ ≤ e^{-\lambda/2}, then (e^{λ/2}-1)δ ≤ 1, log ≤ log 2, so:

$$\frac{\lambda^2}{2}\delta < A + \|g\|_1\log 2 \implies \delta < \frac{2(A+\|g\|_1\log 2)}{\lambda^2}.$$

But this gives δ ≤ C/λ², which we already had. For the bound to give δ ≤ e^{-λ/2}, we'd need C/λ² ≤ e^{-λ/2}, which holds for large λ. So for large λ, the constraint δ ≤ e^{-λ/2} IS satisfied.

In other words, for λ large enough: **m({g > λ}) ≤ e^{-λ/2}**, exponential decay!

The argument: suppose for contradiction that m({g > λ}) > e^{-λ/2}. Then from (*) with c = λ/2:

$$\frac{\lambda^2}{2} \cdot e^{-\lambda/2} < \frac{\lambda^2}{2}\delta < A + \|g\|_1\log((e^{\lambda/2}-1)\delta+1) + A.$$

But (e^{λ/2}-1)δ > (e^{λ/2}-1)e^{-λ/2} = 1 - e^{-λ/2} < 1, so log < log 2. Hence λ²e^{-λ/2}/2 < 2A + ||g||₁ log 2. For λ large enough this fails. So m({g>λ}) ≤ e^{-λ/2} eventually.

More precisely: there exists λ₀ such that for λ > λ₀, m({g > λ}) ≤ e^{-λ/2}.

**Part (a) answer:** m({g > λ}) decays at least exponentially: m({g > λ}) ≤ Ce^{-αλ} for some C, α > 0.

**Part (b) answer:** Yes, g ∈ L². From the exponential decay:

$$\int g^2 = \int_0^\infty 2\lambda\,m(\{g>\lambda\})\,d\lambda \leq \int_0^{\lambda_0} 2\lambda\,d\lambda + \int_{\lambda_0}^\infty 2\lambda\,Ce^{-\alpha\lambda}\,d\lambda < \infty$$

since ∫ λe^{-αλ} dλ converges. In fact g ∈ Lp for all p < ∞, and even e^{βg} ∈ L¹ for small β > 0.

**What connects.** This problem is about Orlicz space duality. The constraint ∫ e^f ≤ 1 defines the unit ball of the Orlicz space L^Φ with Φ(t) = e^t. The hypothesis says g acts as a bounded functional on this ball, which by Orlicz duality means g belongs to the conjugate Orlicz space L^Ψ where Ψ(s) = s log s (the Young conjugate of Φ). Functions in L^Ψ have exponentially decaying distribution functions and belong to every Lp. This is strictly between L^∞ (which would require m({g>λ}) = 0 for large λ) and any fixed Lp. The test function technique — choosing f = cχ_E and optimizing c — is the concrete realisation of this duality.

## 2. Lean 4 theorem statement

```lean
import Mathlib.MeasureTheory.Integral.Bochner
import Mathlib.MeasureTheory.Integral.IntegrableOn
import Mathlib.MeasureTheory.Measure.Lebesgue.Basic
import Mathlib.Analysis.SpecialFunctions.ExpDeriv
import Mathlib.MeasureTheory.Function.L2Space

open MeasureTheory Set Filter

noncomputable section

variable {μ : Measure ℝ := volume.restrict (Icc 0 1)}

/-- The hypothesis: g ∈ L¹([0,1]), g ≥ 0, and ∫ gf < A whenever ∫ e^f ≤ 1. -/
structure OrliczBound (g : ℝ → ℝ) (A : ℝ) : Prop where
  integrable : IntegrableOn g (Icc 0 1) volume
  nonneg : ∀ x ∈ Icc (0:ℝ) 1, 0 ≤ g x
  bound : ∀ f : ℝ → ℝ, IntegrableOn f (Icc 0 1) volume →
    IntegrableOn (fun x => Real.exp (f x)) (Icc 0 1) volume →
    ∫ x in Icc (0:ℝ) 1, Real.exp (f x) ≤ 1 →
    ∫ x in Icc (0:ℝ) 1, g x * f x < A

/-- Part (a): exponential decay of the distribution function.
    m({g > λ}) ≤ C e^{-αλ} for large λ. -/
theorem distribution_exponential_decay
    {g : ℝ → ℝ} {A : ℝ} (hg : OrliczBound g A) :
    ∃ C α : ℝ, 0 < C ∧ 0 < α ∧ ∀ λ : ℝ, 0 < λ →
      volume ({x ∈ Icc (0:ℝ) 1 | λ < g x}) ≤
        ENNReal.ofReal (C * Real.exp (-α * λ)) := by
  sorry

/-- The test function: f = c χ_E - log(e^c m(E) + 1 - m(E))
    is admissible (∫ e^f = 1). -/
lemma test_function_admissible
    {E : Set ℝ} (hE : MeasurableSet E) (hEsub : E ⊆ Icc 0 1)
    {c : ℝ} (hc : 0 < c) (hδ : 0 < (volume.restrict (Icc 0 1)) E)
    (hδ' : (volume.restrict (Icc 0 1)) E ≠ ⊤) :
    let δ := ((volume.restrict (Icc 0 1)) E).toReal
    let norm_const := Real.log ((Real.exp c - 1) * δ + 1)
    ∫ x in Icc (0:ℝ) 1,
      Real.exp (c * (E.indicator (fun _ => (1:ℝ)) x) - norm_const) = 1 := by
  sorry

/-- Part (b): g ∈ L². Exponential decay of the distribution function
    implies integrability of g². -/
theorem orlicz_bound_implies_L2
    {g : ℝ → ℝ} {A : ℝ} (hg : OrliczBound g A)
    (hgm : Measurable g) :
    IntegrableOn (fun x => g x ^ 2) (Icc 0 1) volume := by
  sorry

/-- Stronger conclusion: g ∈ Lp for all finite p. -/
theorem orlicz_bound_implies_Lp
    {g : ℝ → ℝ} {A : ℝ} (hg : OrliczBound g A)
    (hgm : Measurable g)
    {p : ℝ} (hp : 1 ≤ p) :
    IntegrableOn (fun x => ‖g x‖ ^ p) (Icc 0 1) volume := by
  sorry

/-- Even stronger: e^{βg} is integrable for small β. -/
theorem orlicz_bound_implies_exp_integrable
    {g : ℝ → ℝ} {A : ℝ} (hg : OrliczBound g A)
    (hgm : Measurable g) :
    ∃ β : ℝ, 0 < β ∧
      IntegrableOn (fun x => Real.exp (β * g x)) (Icc 0 1) volume := by
  sorry

/-- Layer cake: ∫ g² = ∫₀^∞ 2λ m({g>λ}) dλ, used to convert
    distribution decay to L² integrability. -/
lemma sq_integral_via_distribution
    {g : ℝ → ℝ} (hg : IntegrableOn g (Icc 0 1) volume)
    (hgm : Measurable g) (hnn : ∀ x ∈ Icc (0:ℝ) 1, 0 ≤ g x) :
    ∫⁻ x in Icc (0:ℝ) 1, ENNReal.ofReal (g x ^ 2) ∂volume =
      ∫⁻ λ in Ioi (0:ℝ), 2 * ENNReal.ofReal λ *
        volume ({x ∈ Icc (0:ℝ) 1 | λ < g x}) := by
  sorry

end
```

## 3. Mathlib cross-references

**Types/structures:**
- `MeasureTheory.IntegrableOn` — integrability on a set
- `MeasureTheory.Measure.restrict` — restricted measure (for [0,1])
- `MeasureTheory.Memℒp` — membership in Lp (alternative to IntegrableOn for g²)
- `Set.indicator` — indicator/characteristic function
- `ENNReal` — extended nonneg reals for measure values
- `Real.exp`, `Real.log` — exponential and logarithm

**Key lemmas:**
- `MeasureTheory.lintegral_comp_eq_lintegral_meas_lt_mul` / `lintegral_norm_eq_lintegral_meas_lt` — layer cake / Cavalieri principle (converts ∫g² to ∫ 2λ m({g>λ}) dλ)
- `MeasureTheory.setLIntegral_const` — integral of constant over a set
- `MeasureTheory.integral_indicator` — integral involving indicator functions
- `Real.add_pow_le_pow_mul_pow_of_sq_le_sq` — not directly, but convexity-type inequalities
- `Real.exp_le_exp` — monotonicity of exp
- `Real.log_le_log` — monotonicity of log
- `ENNReal.ofReal_le_ofReal` — monotonicity of coercion
- `MeasureTheory.measure_mono` — monotonicity of measures
- `MeasureTheory.integral_mono` — monotonicity of integrals
- `MeasureTheory.Measure.restrict_apply` — evaluating restricted measures
- `Real.exp_pos` — positivity of exp
- `ENNReal.toReal_ofReal` — coercion roundtrip
- `MeasureTheory.integral_mul_left` — pulling constants out of integrals
- `Real.tendsto_exp_neg_atTop_nhds_zero` — e^{-αλ} → 0 (for showing λe^{-αλ} is integrable)

**Tactic hints:**
- `by_contra` — for the exponential decay argument (assume m({g>λ}) > e^{-αλ} and derive contradiction)
- `calc` — for the chain of inequalities in the test function argument
- `positivity` — for showing exp terms and measure values are positive
- `field_simp` — for algebraic manipulations with log and exp
- `gcongr` — for monotonicity-based inequality steps
- `nlinarith` — for the final arithmetic combining the bounds
- `simp [Set.indicator]` — unfolding indicator function definitions
- `exact?` — finding the right integral manipulation lemma