

# APM a92J03: The Fourier transform maps L¹ to C₀ (Riemann-Lebesgue)

## 1. Informal proof

**Why it's hard.** Parts (a) and (b) are straightforward applications of dominated convergence and the triangle inequality, but part (c) — showing f̂(ξ) → 0 as |ξ| → ∞ — is the Riemann-Lebesgue lemma, which requires an approximation argument. The decay at infinity comes from oscillatory cancellation, which is invisible to pointwise estimates.

**The key insight.** For a single indicator function 1_{[a,b]}, the Fourier transform is (e^{ibξ} - e^{iaξ})/(iξ), which is O(1/|ξ|) → 0. This extends by linearity to step functions, and then by density (step functions are dense in L¹) and the uniform bound ||f̂||_∞ ≤ ||f||₁ to all of L¹.

**Proof.**

**(a) f̂ is continuous.** Fix ξ₀ ∈ ℝ. For ξ → ξ₀:

|f̂(ξ) - f̂(ξ₀)| = |∫ f(x)(e^{ixξ} - e^{ixξ₀}) dx| ≤ ∫ |f(x)| · |e^{ixξ} - e^{ixξ₀}| dx.

The integrand satisfies |f(x)(e^{ixξ} - e^{ixξ₀})| ≤ 2|f(x)| (integrable dominator) and converges to 0 pointwise (since e^{ixξ} → e^{ixξ₀} for each x). By the dominated convergence theorem, the integral → 0. ✓

**(b) ||f̂||_∞ ≤ ||f||₁.** For any ξ:

|f̂(ξ)| = |∫ f(x)e^{ixξ} dx| ≤ ∫ |f(x)| · |e^{ixξ}| dx = ∫ |f(x)| dx = ||f||₁.

Taking sup over ξ gives ||f̂||_∞ ≤ ||f||₁. ✓

**(c) f̂(ξ) → 0 for step functions.** Let f = Σ_{j=1}^N c_j 1_{[a_j, b_j]}. Then:

f̂(ξ) = Σ c_j ∫_{a_j}^{b_j} e^{ixξ} dx = Σ c_j (e^{ib_jξ} - e^{ia_jξ})/(iξ).

Each term is O(1/|ξ|) → 0 as |ξ| → ∞. Since the sum is finite, f̂(ξ) → 0. ✓

**Deduction: f̂ ∈ C₀(ℝ) for all f ∈ L¹.** We show f̂(ξ) → 0 as |ξ| → ∞ for general f ∈ L¹. Fix ε > 0. Since step functions (finite linear combinations of interval indicators) are dense in L¹(ℝ), choose a step function s with ||f - s||₁ < ε/2. Then:

|f̂(ξ)| ≤ |ŝ(ξ)| + |(f̂ - ŝ)(ξ)| ≤ |ŝ(ξ)| + ||f - s||₁ < |ŝ(ξ)| + ε/2.

By part (c), |ŝ(ξ)| < ε/2 for |ξ| large enough. So |f̂(ξ)| < ε for |ξ| large. Combined with parts (a) and (b), f ↦ f̂ is a bounded linear map from L¹(ℝ) into C₀(ℝ). ✓ ∎

**What connects.** This is the Riemann-Lebesgue lemma, one of the most fundamental results in Fourier analysis. It says the Fourier transform F : L¹(ℝ) → C₀(ℝ) is a bounded linear operator with ||F|| ≤ 1. The map is injective (by the Fourier inversion theorem) but not surjective (its image is a proper closed subalgebra of C₀). The proof strategy — verify on a dense subclass, then extend by density using a uniform bound — is the paradigmatic approximation argument in functional analysis. The same argument proves Riemann-Lebesgue on ℝⁿ, on the circle (where it gives the decay of Fourier coefficients), and in any locally compact abelian group.

## 2. Lean 4 theorem statement

```lean
import Mathlib.MeasureTheory.Integral.Bochner
import Mathlib.MeasureTheory.Integral.IntegrableOn
import Mathlib.Analysis.Fourier.FourierTransformDeriv
import Mathlib.Topology.ContinuousFunction.ZeroAtInfty
import Mathlib.Order.Filter.Basic

open MeasureTheory Complex Set Filter Topology

noncomputable section

/-- The Fourier transform: f̂(ξ) = ∫ f(x) e^{ixξ} dx. -/
def fourierTransform (f : ℝ → ℂ) (ξ : ℝ) : ℂ :=
  ∫ x, f x * exp (I * x * ξ) ∂volume

/-- (a) The Fourier transform is continuous. -/
theorem fourierTransform_continuous
    {f : ℝ → ℂ} (hf : Integrable f volume) :
    Continuous (fourierTransform f) := by
  sorry

/-- (b) Sup bound: ‖f̂‖_∞ ≤ ‖f‖₁. -/
theorem fourierTransform_norm_le
    {f : ℝ → ℂ} (hf : Integrable f volume) (ξ : ℝ) :
    ‖fourierTransform f ξ‖ ≤ ∫ x, ‖f x‖ ∂volume := by
  sorry

/-- (c) For interval indicators, f̂(ξ) → 0 as |ξ| → ∞. -/
theorem fourierTransform_indicator_tendsto_zero
    {a b : ℝ} (hab : a < b) :
    Tendsto (fun ξ => fourierTransform (Set.indicator (Icc a b) (fun _ => (1 : ℂ))) ξ)
      (cocompact ℝ) (𝓝 0) := by
  sorry

/-- For finite linear combinations of interval indicators. -/
theorem fourierTransform_step_tendsto_zero
    {n : ℕ} {c : Fin n → ℂ} {a b : Fin n → ℝ}
    (hab : ∀ i, a i < b i) :
    Tendsto (fun ξ => fourierTransform
      (fun x => ∑ i, c i * (Icc (a i) (b i)).indicator (fun _ => (1 : ℂ)) x) ξ)
      (cocompact ℝ) (𝓝 0) := by
  sorry

/-- Step functions are dense in L¹(ℝ). -/
lemma step_functions_dense_L1
    {f : ℝ → ℂ} (hf : Integrable f volume) {ε : ℝ} (hε : 0 < ε) :
    ∃ (n : ℕ) (c : Fin n → ℂ) (a b : Fin n → ℝ),
      (∀ i, a i < b i) ∧
      ∫ x, ‖f x - ∑ i, c i * (Icc (a i) (b i)).indicator (fun _ => (1 : ℂ)) x‖ < ε := by
  sorry

/-- Riemann-Lebesgue: f̂(ξ) → 0 as |ξ| → ∞ for all f ∈ L¹. -/
theorem riemann_lebesgue
    {f : ℝ → ℂ} (hf : Integrable f volume) :
    Tendsto (fourierTransform f) (cocompact ℝ) (𝓝 0) := by
  sorry

/-- Full theorem: the Fourier transform maps L¹ into C₀(ℝ).
    f̂ is continuous, bounded by ‖f‖₁, and vanishes at infinity. -/
theorem fourier_maps_L1_to_C0
    {f : ℝ → ℂ} (hf : Integrable f volume) :
    Continuous (fourierTransform f) ∧
    (∀ ξ, ‖fourierTransform f ξ‖ ≤ ∫ x, ‖f x‖ ∂volume) ∧
    Tendsto (fourierTransform f) (cocompact ℝ) (𝓝 0) := by
  sorry

/-- The Fourier transform as a bounded linear map L¹ → C₀.
    Operator norm ≤ 1. -/
theorem fourierTransform_norm_le_one :
    ∀ (f : ℝ → ℂ), Integrable f volume →
      ∀ ξ, ‖fourierTransform f ξ‖ ≤ ‖∫ x, ‖f x‖ ∂volume‖ := by
  sorry

end
```

## 3. Mathlib cross-references

**Types/structures:**
- `MeasureTheory.Integrable f volume` — f ∈ L¹(ℝ)
- `Complex.exp`, `Complex.I` — exponential kernel e^{ixξ}
- `Filter.cocompact ℝ` — filter for |ξ| → ∞
- `Filter.Tendsto` — convergence along a filter
- `Set.indicator` — characteristic function of an interval
- `ContinuousMap.ZeroAtInfty` — the space C₀(ℝ) (in `Mathlib.Topology.ContinuousFunction.ZeroAtInfty`)

**Key lemmas:**
- `MeasureTheory.tendsto_integral_of_dominated_convergence` — DCT: the engine of part (a). Pointwise convergence + integrable dominator ⟹ integral converges
- `MeasureTheory.norm_integral_le_integral_norm` — |∫ f| ≤ ∫ |f|, the engine of part (b)
- `Complex.norm_exp_ofReal_mul_I` / `Complex.abs_exp_ofReal_mul_I` — |e^{ixξ}| = 1
- `intervalIntegral.integral_exp_mul_complex` — ∫_a^b e^{icx} dx = (e^{icb} - e^{ica})/(ic), the engine of part (c)
- `Integrable.sub` — integrability of f - s for the density argument
- `MeasureTheory.SimpleFunc.tendsto_approxOn` / density of step functions in L¹ — for the approximation step
- `Filter.Tendsto.add` / `Filter.Tendsto.sub` — algebra of limits
- `norm_add_le` — triangle inequality for the ε/2 argument
- `Real.tendsto_inv_atTop_nhds_zero` — 1/|ξ| → 0 (for indicator transforms)
- `VectorFourier.fourierIntegral` — Mathlib's Fourier transform definition (in `Mathlib.Analysis.Fourier.FourierTransformDeriv`), which may differ by convention
- `MeasureTheory.Integrable.norm` — ‖f‖ integrable when f integrable

**Tactic hints:**
- `apply tendsto_integral_of_dominated_convergence (fun x => ‖f x‖)` — for part (a), applying DCT
- `calc` — for the ε/2 chain in the density argument
- `simp [Complex.norm_exp_ofReal_mul_I]` — simplifying |e^{ixξ}| = 1
- `field_simp` — for (e^{ibξ} - e^{iaξ})/(iξ) manipulations
- `gcongr` — for monotonicity in norm bounds
- `exact norm_integral_le_integral_norm _ _` — for part (b)
- `intro ε hε` — starting the Riemann-Lebesgue ε argument
- `obtain ⟨s, hs_approx⟩ := step_functions_dense_L1 hf (half_pos hε)` — density
- `linarith` — combining the ε/2 bounds
- `filter_upwards` — for cocompact filter arguments
- `continuity` — establishing continuity of the integrand in ξ