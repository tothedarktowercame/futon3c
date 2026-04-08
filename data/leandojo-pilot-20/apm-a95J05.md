

# APM a95J05: Convergence in measure iff ∫|arctan fₙ| → 0

## 1. Informal proof

**Why it's hard.** The equivalence connects a topological mode of convergence (in measure) to an integral condition involving a specific nonlinear function (arctan). Neither direction is immediate: convergence in measure doesn't control integrals in general, and integral convergence doesn't control pointwise behaviour without extra structure.

**The key insight.** arctan is bounded (|arctan t| < π/2) and continuous with arctan(0) = 0, and |arctan t| > 0 iff t ≠ 0. On a *finite* measure space, convergence in measure is equivalent to convergence in the metric d(f,g) = ∫ φ(|f-g|) dμ for any bounded continuous function φ with φ(0) = 0 and φ(t) > 0 for t > 0. Here φ = |arctan| serves this role.

**Proof.**

**(⟹) Convergence in measure ⟹ ∫|arctan fₙ| → 0.**

Fix ε > 0. Since |arctan t| < π/2 for all t:

∫ |arctan fₙ| = ∫_{|fₙ|≤δ} |arctan fₙ| + ∫_{|fₙ|>δ} |arctan fₙ|

≤ arctan(δ) · μ(Ω) + (π/2) · μ({|fₙ| > δ}).

Choose δ small enough that arctan(δ) · μ(Ω) < ε/2. Then choose N such that μ({|fₙ| > δ}) < ε/π for n ≥ N (by convergence in measure). Then ∫|arctan fₙ| < ε/2 + ε/2 = ε. ✓

**(⟸) ∫|arctan fₙ| → 0 ⟹ convergence in measure.**

Fix ε > 0. For any δ > 0:

∫ |arctan fₙ| ≥ ∫_{|fₙ|>ε} |arctan fₙ| ≥ arctan(ε) · μ({|fₙ| > ε}).

So μ({|fₙ| > ε}) ≤ ∫|arctan fₙ| / arctan(ε).

Since ∫|arctan fₙ| → 0 and arctan(ε) > 0 (for ε > 0):

μ({|fₙ| > ε}) → 0. ✓ ∎

**What connects.** This characterises convergence in measure via integrals of bounded continuous "gauge functions." The function φ(t) = |arctan t| works because it satisfies three properties: (1) φ is bounded (< π/2), which controls the large-deviation term; (2) φ(0) = 0, making the integral small when fₙ is small; (3) φ(t) > 0 for t > 0, ensuring the integral detects nonzero values. Any function with these properties (e.g., t/(1+t), min(t,1)) would work equally well. The equivalence holds specifically on *finite* measure spaces — on infinite measure spaces, convergence in measure is not metrizable by a single integral condition. This result connects to the Ky Fan metric d(f,g) = ∫ min(|f-g|, 1) dμ (or ∫ |f-g|/(1+|f-g|) dμ), which metrises convergence in measure on finite measure spaces.

## 2. Lean 4 theorem statement

```lean
import Mathlib.MeasureTheory.Integral.Bochner
import Mathlib.MeasureTheory.Measure.MeasureSpace
import Mathlib.Analysis.SpecialFunctions.Trigonometric.Basic

open MeasureTheory Filter Real

noncomputable section

variable {Ω : Type*} [MeasurableSpace Ω] {μ : Measure Ω} [IsFiniteMeasure μ]

/-- Convergence in measure: μ({|fₙ| > ε}) → 0 for all ε > 0. -/
def TendstoInMeasureZero (f : ℕ → Ω → ℝ) (μ : Measure Ω) : Prop :=
  ∀ ε : ℝ, 0 < ε →
    Tendsto (fun n => μ {x | ε < |f n x|}) atTop (𝓝 0)

/-- Main theorem: convergence in measure to 0 iff ∫|arctan fₙ| → 0. -/
theorem tendstoInMeasure_iff_integral_arctan
    {f : ℕ → Ω → ℝ}
    (hf : ∀ n, Measurable (f n)) :
    TendstoInMeasureZero f μ ↔
      Tendsto (fun n => ∫ x, |arctan (f n x)| ∂μ) atTop (𝓝 0) := by
  sorry

/-- (⟹) Convergence in measure implies ∫|arctan fₙ| → 0. -/
theorem integral_arctan_tendsto_of_tendstoInMeasure
    {f : ℕ → Ω → ℝ}
    (hf : ∀ n, Measurable (f n))
    (hconv : TendstoInMeasureZero f μ) :
    Tendsto (fun n => ∫ x, |arctan (f n x)| ∂μ) atTop (𝓝 0) := by
  sorry

/-- (⟸) ∫|arctan fₙ| → 0 implies convergence in measure. -/
theorem tendstoInMeasure_of_integral_arctan_tendsto
    {f : ℕ → Ω → ℝ}
    (hf : ∀ n, Measurable (f n))
    (hint : Tendsto (fun n => ∫ x, |arctan (f n x)| ∂μ) atTop (𝓝 0)) :
    TendstoInMeasureZero f μ := by
  sorry

/-- Markov-type bound: μ({|fₙ| > ε}) ≤ ∫|arctan fₙ| / arctan(ε). -/
lemma measure_gt_le_integral_arctan_div
    {f : Ω → ℝ} (hf : Measurable f)
    {ε : ℝ} (hε : 0 < ε) :
    μ {x | ε < |f x|} ≤
      ENNReal.ofReal ((∫ x, |arctan (f x)| ∂μ) / arctan ε) := by
  sorry

/-- arctan is bounded: |arctan t| < π/2 for all t. -/
lemma abs_arctan_lt_pi_div_two (t : ℝ) :
    |arctan t| < π / 2 := by
  sorry

/-- Splitting bound: ∫|arctan fₙ| ≤ arctan(δ)·μ(Ω) + (π/2)·μ({|fₙ|>δ}). -/
lemma integral_arctan_split
    {f : Ω → ℝ} (hf : Measurable f)
    {δ : ℝ} (hδ : 0 < δ) :
    (∫ x, |arctan (f x)| ∂μ) ≤
      arctan δ * (μ Set.univ).toReal +
      (π / 2) * (μ {x | δ < |f x|}).toReal := by
  sorry

/-- arctan is monotone and arctan(0) = 0. -/
lemma arctan_zero : arctan 0 = 0 := by
  sorry

lemma arctan_pos {t : ℝ} (ht : 0 < t) : 0 < arctan t := by
  sorry

end
```

## 3. Mathlib cross-references

**Types/structures:**
- `MeasureTheory.IsFiniteMeasure μ` — finite measure space (μ(Ω) < ∞)
- `MeasureTheory.Measure` — the measure
- `Measurable f` — measurability of fₙ
- `Filter.Tendsto`, `Filter.atTop` — convergence along ℕ
- `ENNReal` — extended nonneg reals for measure values

**Key lemmas:**
- `MeasureTheory.meas_ge_le_mul_pow_snorm` / Markov/Chebyshev — μ({|f|>ε}) ≤ ∫g(|f|)/g(ε) for monotone g. Core of the (⟸) direction
- `MeasureTheory.integral_mono` — monotonicity of integrals (for splitting)
- `MeasureTheory.integral_add_compl` — ∫ = ∫_{|f|≤δ} + ∫_{|f|>δ}
- `MeasureTheory.set_integral_le_integral` — ∫_E |g| ≤ ∫ |g|
- `Real.arctan_zero` — arctan(0) = 0
- `Real.arctan_lt_pi_div_two` — arctan(t) < π/2
- `Real.arctan_strictMono` — arctan is strictly monotone
- `Continuous.arctan` / `Real.continuous_arctan` — arctan is continuous (for measurability of composition)
- `Measurable.arctan` — arctan ∘ f is measurable when f is
- `ENNReal.ofReal_div_of_pos` — for the division in the Markov bound
- `IsFiniteMeasure.measure_univ_lt_top` — μ(Ω) < ∞

**Tactic hints:**
- `constructor` — splitting the iff
- `intro ε hε` — for the ε argument in convergence in measure
- `calc μ {x | ε < |f n x|} ≤ ... ≤ ∫|arctan fₙ| / arctan ε` — Markov chain for (⟸)
- `calc ∫|arctan fₙ| ≤ arctan(δ)·μ(Ω) + (π/2)·μ({|fₙ|>δ}) < ε/2 + ε/2` — split for (⟹)
- `gcongr` — for monotonicity of arctan
- `positivity` — for arctan(ε) > 0
- `linarith [abs_arctan_lt_pi_div_two (f n x)]` — bounding |arctan f| < π/2
- `exact?` — finding the Markov inequality or integral splitting lemma
- `simp [arctan_zero]` — simplifying arctan at 0