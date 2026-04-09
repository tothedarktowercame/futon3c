

# APM a96A01: Lp quasi-norms for 0 < p ≤ 1, monotonicity, and the geometric mean limit

## 1. Informal proof

**Why it's hard.** For 0 < p < 1, ||·||_p is not a norm (fails triangle inequality), and the behaviour as p → 0⁺ is subtle — the "L⁰ norm" is not a standard object. Showing ||f||_p is decreasing in p and computing its limit requires tools from convexity applied to the function t ↦ t^p, which is *concave* for p < 1 (opposite to the p > 1 case).

**The key insight.** (a) Jensen's inequality for the concave function t ↦ t^{p/q} (with 0 < p/q < 1): (∫|f|^q)^{p/q} ≥ ∫|f|^p on a probability space, giving ||f||_p ≤ ||f||_q. (b)-(c) As p → 0⁺, (1/p)log∫|f|^p → ∫log|f| by L'Hôpital or differentiation under the integral, so ||f||_p = exp((1/p)log∫|f|^p) → exp(∫log|f|), the geometric mean.

**Proof.**

**(a) ||f||_p ≤ ||f||_q for 0 < p < q ≤ 1.**

Let r = q/p > 1 and s = r/(r-1) (conjugate exponent). Apply Hölder's inequality with exponents r and s to the function |f|^p and the constant 1 on [0,1]:

∫|f|^p = ∫|f|^p · 1 ≤ (∫(|f|^p)^r)^{1/r} · (∫1^s)^{1/s} = (∫|f|^q)^{p/q} · 1.

Taking (1/p)-th power of both sides: (∫|f|^p)^{1/p} ≤ (∫|f|^q)^{1/q}, i.e., ||f||_p ≤ ||f||_q. ✓

Since f ∈ L¹ and q ≤ 1: ||f||_q ≤ ||f||₁ < ∞, so ||f||_p < ∞ for all p ∈ (0,1].

The function p ↦ ||f||_p is decreasing, nonneg, and bounded below by 0, so L = lim_{p→0⁺} ||f||_p exists. ✓

**(b) L > 0 implies |f| > 0 a.e.**

Contrapositive: suppose E = {x : f(x) = 0} has m(E) > 0. Then:

∫|f|^p = ∫_{E^c} |f|^p.

Using the hint t^p ≤ 1 + t for t ≥ 0, p ∈ (0,1]: |f(x)|^p ≤ 1 + |f(x)|. So:

∫|f|^p ≤ ∫_{E^c} (1 + |f|) = m(E^c) + ∫_{E^c} |f| ≤ 1 + ||f||₁.

Thus ||f||_p = (∫|f|^p)^{1/p} ≤ (1 + ||f||₁)^{1/p}.

But also: ∫|f|^p = ∫_{E^c} |f|^p ≤ (m(E^c))^{1-p} (∫_{E^c} |f|)^p (Hölder or just |f|^p ≤ |f| since p ≤ 1 and |f| might be ≥ 1)... hmm, need a cleaner bound.

Directly: ∫|f|^p ≤ m(E^c) + ||f||₁ (using t^p ≤ 1+t). Since m(E) > 0, m(E^c) = 1-m(E) < 1. But we also need the limit.

Key observation: ||f||_p^p = ∫_{E^c} |f|^p → m(E^c) as p → 0 (since |f(x)|^p → 1 for x ∈ E^c where |f(x)| > 0, by DCT with dominator 1+|f|). So ||f||_p = (m(E^c))^{1/p}. Since m(E^c) < 1 and 1/p → ∞: L = lim (m(E^c))^{1/p} = 0. Contradicting L > 0. ✓

**(c) L ≥ exp(∫log|f|).**

Since L > 0 and |f| > 0 a.e. (by part b), log|f| is well-defined a.e. By Jensen (log is concave, applied on the probability space [0,1]):

(1/p) log(∫|f|^p) = (1/p) log(∫e^{p log|f|}) ≥ (1/p) · p · ∫log|f| = ∫log|f|.

Wait — Jensen for the convex function e^{·} gives ∫e^{p log|f|} ≥ e^{p∫log|f|}, so log(∫|f|^p) ≥ p∫log|f|, hence (1/p)log(∫|f|^p) ≥ ∫log|f|.

Exponentiating: ||f||_p = exp((1/p)log(∫|f|^p)) ≥ exp(∫log|f|).

Taking p → 0⁺: L ≥ exp(∫log|f|). ✓ ∎

**What connects.** This explores the Lp scale below p = 1, where the quasi-norm ||f||_p is decreasing in p (opposite to the p ≥ 1 behaviour on finite measure spaces, where ||f||_p increases). The limit L = lim_{p→0} ||f||_p = exp(∫log|f|) is the *geometric mean* of |f| — a fundamental quantity in information theory and multiplicative analysis. The inequality L ≥ exp(∫log|f|) becomes equality (the full computation shows L = exp(∫log|f|) when L > 0), connecting Lp theory to the AM-GM inequality in its continuous form. The concavity of t^p for p < 1 is what reverses the Jensen direction compared to p > 1.

## 2. Lean 4 theorem statement

```lean
import Mathlib.MeasureTheory.Integral.MeanInequalities
import Mathlib.MeasureTheory.Measure.Lebesgue.Basic
import Mathlib.Analysis.SpecialFunctions.Log.Basic
import Mathlib.Analysis.SpecialFunctions.Pow.Real

open MeasureTheory Set Filter Real

noncomputable section

variable {μ : Measure ℝ := volume.restrict (Icc 0 1)}

/-- The Lp quasi-norm for 0 < p: (∫|f|^p)^{1/p}. -/
def quasiNorm (f : ℝ → ℝ) (p : ℝ) : ℝ :=
  (∫ x in Icc (0:ℝ) 1, ‖f x‖ ^ p ∂volume) ^ (1 / p)

/-- (a) ‖f‖_p ≤ ‖f‖_q when 0 < p < q ≤ 1. -/
theorem quasiNorm_antitone
    {f : ℝ → ℝ}
    (hf : IntegrableOn f (Icc 0 1) volume)
    {p q : ℝ} (hp : 0 < p) (hpq : p < q) (hq : q ≤ 1) :
    quasiNorm f p ≤ quasiNorm f q := by
  sorry

/-- The limit L = lim_{p→0⁺} ‖f‖_p exists. -/
theorem quasiNorm_tendsto_exists
    {f : ℝ → ℝ}
    (hf : IntegrableOn f (Icc 0 1) volume) :
    ∃ L : ℝ, Tendsto (quasiNorm f) (𝓝[>] 0) (𝓝 L) := by
  sorry

/-- (b) L > 0 implies |f| > 0 a.e. -/
theorem ae_pos_of_limit_pos
    {f : ℝ → ℝ}
    (hf : IntegrableOn f (Icc 0 1) volume)
    {L : ℝ} (hL : 0 < L)
    (hLim : Tendsto (quasiNorm f) (𝓝[>] 0) (𝓝 L)) :
    ∀ᵐ x ∂(volume.restrict (Icc 0 1)), 0 < ‖f x‖ := by
  sorry

/-- (c) L ≥ exp(∫ log|f|). -/
theorem limit_ge_geometric_mean
    {f : ℝ → ℝ}
    (hf : IntegrableOn f (Icc 0 1) volume)
    (hf_pos : ∀ᵐ x ∂(volume.restrict (Icc 0 1)), 0 < ‖f x‖)
    {L : ℝ} (hL : 0 < L)
    (hLim : Tendsto (quasiNorm f) (𝓝[>] 0) (𝓝 L)) :
    exp (∫ x in Icc (0:ℝ) 1, log ‖f x‖ ∂volume) ≤ L := by
  sorry

/-- Jensen for the convex exponential:
    ∫|f|^p = ∫e^{p·log|f|} ≥ e^{p·∫log|f|}. -/
lemma jensen_exp_integral
    {f : ℝ → ℝ}
    (hf_pos : ∀ᵐ x ∂(volume.restrict (Icc 0 1)), 0 < ‖f x‖)
    (hlog : IntegrableOn (fun x => log ‖f x‖) (Icc 0 1) volume)
    {p : ℝ} (hp : 0 < p) :
    exp (p * ∫ x in Icc (0:ℝ) 1, log ‖f x‖ ∂volume) ≤
      ∫ x in Icc (0:ℝ) 1, ‖f x‖ ^ p ∂volume := by
  sorry

/-- The hint: t^p ≤ 1 + t for t ≥ 0 and 0 < p ≤ 1. -/
lemma rpow_le_one_add {t : ℝ} (ht : 0 ≤ t) {p : ℝ} (hp : 0 < p) (hp1 : p ≤ 1) :
    t ^ p ≤ 1 + t := by
  sorry

/-- ∫|f|^p → m({f ≠ 0}) as p → 0⁺ by DCT. -/
lemma integral_rpow_tendsto_measure
    {f : ℝ → ℝ}
    (hf : IntegrableOn f (Icc 0 1) volume) :
    Tendsto (fun p : ℝ => ∫ x in Icc (0:ℝ) 1, ‖f x‖ ^ p ∂volume)
      (𝓝[>] 0)
      (𝓝 (volume.restrict (Icc 0 1) {x | f x ≠ 0}).toReal) := by
  sorry

end
```

## 3. Mathlib cross-references

**Types/structures:**
- `MeasureTheory.IntegrableOn f (Icc 0 1) volume` — f ∈ L¹([0,1])
- `Real.rpow` — `t ^ p` for real p (via `Real.rpow`)
- `Real.log`, `Real.exp` — logarithm and exponential
- `Filter.𝓝[>] 0` — right-neighbourhood filter for p → 0⁺
- `∀ᵐ x ∂μ` — almost everywhere

**Key lemmas:**
- `MeasureTheory.integral_mul_le_Lp_mul_Lq` — Hölder's inequality (for part (a), rewriting the Jensen argument as Hölder with exponents q/p and its conjugate)
- `ConvexOn.integral_le` / Jensen's inequality — for convex function exp on a probability space. Needed for part (c)
- `Real.rpow_le_rpow` — monotonicity of rpow
- `Real.rpow_natCast` — connecting rpow to pow
- `Real.add_pow_le_pow_mul_pow_of_sq_le_sq` — concavity-type inequality for p < 1
- `tendsto_integral_of_dominated_convergence` — DCT for the p → 0 limit (|f|^p → 1_{f≠0})
- `Real.rpow_zero` — t^0 = 1 for t > 0
- `Real.log_rpow` — log(t^p) = p·log t
- `MonotonOn.tendsto_nhdsWithin` — monotone bounded function has a limit

**Tactic hints:**
- `have h_holder := integral_mul_le_Lp_mul_Lq ...` — Hölder for part (a)
- `calc quasiNorm f p = ... ≤ ... = quasiNorm f q` — monotonicity chain
- `apply tendsto_integral_of_dominated_convergence (fun x => 1 + ‖f x‖)` — DCT for the limit
- `simp [Real.rpow_zero]` — |f|^0 = 1
- `gcongr` — for monotonicity of rpow
- `positivity` — for p > 0, ‖f‖ > 0
- `nlinarith [rpow_le_one_add (norm_nonneg (f x)) hp hp1]` — using the hint
- `exact?` — finding Jensen or Hölder
- `field_simp` — for 1/p arithmetic
- `filter_upwards` — for a.e. statements