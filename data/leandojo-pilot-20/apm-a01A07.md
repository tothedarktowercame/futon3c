

# APM a01A07: L¹ convergence of analytic functions implies uniform convergence

## 1. Informal proof

**Why it's hard.** The hypothesis is very weak — only L¹ convergence on disks (area integral), not pointwise or uniform. For generic continuous functions, L¹ convergence implies nothing about uniform convergence. The gap seems unbridgeable without exploiting the rigid structure of analytic functions.

**The key insight.** The mean value property for analytic functions converts area integrals into pointwise values: |g(z₀)| ≤ (1/πr²) ∫_{D(z₀,r)} |g| dA. Applied to g = fₙ - fₘ (analytic), this bounds sup-norm by L¹ norm, upgrading L¹(disk) convergence to uniform convergence on compacts.

**Proof.**

*Step 1: fₙ → F uniformly on compact subsets.* Fix compact K ⊂ Ω. Choose r > 0 with D(z,r) ⊂ Ω for all z ∈ K (r = dist(K,∂Ω)/2). For z ∈ K, the mean value property gives fₙ(z) = (1/πr²) ∫_{D(z,r)} fₙ dA. So:

fₙ(z) - F(z) = (1/πr²) ∫_{D(z,r)} (fₙ - F) dA + [F(z) − (1/πr²) ∫_{D(z,r)} F dA] + [(1/πr²) ∫_{D(z,r)} F dA − F(z)].

Wait — cleaner: |fₙ(z) - fₘ(z)| = (1/πr²)|∫_{D(z,r)} (fₙ-fₘ) dA| ≤ (1/πr²) ∫_{D(z,r)} |fₙ-fₘ| dA. Let D_K be a disk containing the r-neighbourhood of K. Then sup_K |fₙ-fₘ| ≤ (1/πr²) ∫_{D_K} |fₙ-fₘ| dA ≤ (1/πr²)[∫_{D_K} |fₙ-F| + ∫_{D_K} |fₘ-F|] → 0. So {fₙ} is uniformly Cauchy on K, converging uniformly to some limit.

For the limit to be F: choose r small enough that |(1/πr²) ∫_{D(z,r)} F − F(z)| < ε (uniform continuity of F on a compact neighbourhood). Then |fₙ(z) - F(z)| ≤ (1/πr²) ∫_{D(z,r)} |fₙ-F| + ε. The first term ≤ (1/πr²) ∫_{D_K} |fₙ-F| → 0 uniformly in z ∈ K. So fₙ → F uniformly on K.

*Step 2: F is analytic.* F is the uniform limit on compacts of analytic functions. By Morera's theorem: for any triangle Δ ⊂ Ω, ∮_Δ F dz = lim ∮_Δ fₙ dz = 0 (Cauchy's theorem for each fₙ; limit passes through by uniform convergence). So F is analytic.

*Step 3: fₙ' → F' uniformly on compacts.* For z ∈ K, choose ρ = dist(K,∂Ω)/2. By the Cauchy integral formula for derivatives: fₙ'(z) - F'(z) = (1/2πi) ∮_{|w-z|=ρ} (fₙ(w)-F(w))/(w-z)² dw. So |fₙ'(z)-F'(z)| ≤ (1/ρ) sup_{|w-z|=ρ} |fₙ(w)-F(w)| ≤ (1/ρ) ||fₙ-F||_{∞, K_ρ} → 0 uniformly in z ∈ K, by Step 1 applied to the compact set K_ρ. ∎

**What connects.** This is a strengthened Weierstrass convergence theorem: the classical version assumes uniform convergence on compacts as hypothesis, while this problem proves that for analytic functions, the much weaker L¹(disk) convergence automatically upgrades to uniform convergence. The engine is the mean value property / sub-mean value inequality for analytic functions, which controls pointwise values by integral averages. The same principle underlies Montel's theorem and the theory of normal families. The result generalises to any class of functions satisfying elliptic regularity estimates (harmonic functions, solutions of elliptic PDE), where interior estimates bound sup-norms by L¹ norms.

## 2. Lean 4 theorem statement

```lean
import Mathlib.Analysis.Complex.CauchyIntegral
import Mathlib.Analysis.Complex.AbsMax
import Mathlib.MeasureTheory.Integral.IntegrableOn
import Mathlib.Topology.UniformSpace.UniformConvergenceTopology
import Mathlib.Order.Filter.Basic

open MeasureTheory Complex Metric Set Filter Topology

noncomputable section

variable {Ω : Set ℂ} (hΩ : IsOpen Ω)

/-- L¹ convergence on disks: ∫_D |fₙ - F| dA → 0 for every closed disk D ⊂ Ω. -/
def L1ConvergesOnDisks (f : ℕ → ℂ → ℂ) (F : ℂ → ℂ) (Ω : Set ℂ) : Prop :=
  ∀ (z : ℂ) (r : ℝ), 0 < r → closedBall z r ⊆ Ω →
    Tendsto (fun n => ∫ w in closedBall z r, ‖f n w - F w‖ ∂volume)
      atTop (𝓝 0)

/-- Mean value inequality: |g(z₀)| ≤ (1/πr²) ∫_{D(z₀,r)} |g| dA
    for g analytic. Controls pointwise values by L¹ averages. -/
lemma norm_le_integral_div_area
    {g : ℂ → ℂ} {z₀ : ℂ} {r : ℝ} (hr : 0 < r)
    (hg : DifferentiableOn ℂ g (closedBall z₀ r)) :
    ‖g z₀‖ ≤ (1 / (Real.pi * r ^ 2)) *
      ∫ w in closedBall z₀ r, ‖g w‖ ∂volume := by
  sorry

/-- Step 1: fₙ → F uniformly on compact subsets. -/
theorem tendstoUniformlyOn_of_L1_on_disks
    {f : ℕ → ℂ → ℂ} {F : ℂ → ℂ}
    (hf : ∀ n, DifferentiableOn ℂ (f n) Ω)
    (hF : ContinuousOn F Ω)
    (hconv : L1ConvergesOnDisks f F Ω)
    {K : Set ℂ} (hK : IsCompact K) (hKΩ : K ⊆ Ω) :
    TendstoUniformlyOn f F atTop K := by
  sorry

/-- Step 2: F is analytic (Morera via uniform convergence). -/
theorem differentiableOn_limit
    {f : ℕ → ℂ → ℂ} {F : ℂ → ℂ}
    (hΩ : IsOpen Ω)
    (hf : ∀ n, DifferentiableOn ℂ (f n) Ω)
    (hF : ContinuousOn F Ω)
    (hconv : L1ConvergesOnDisks f F Ω) :
    DifferentiableOn ℂ F Ω := by
  sorry

/-- Cauchy estimate: ‖g'(z₀)‖ ≤ (1/ρ) sup_{|w-z₀|=ρ} ‖g(w)‖. -/
lemma deriv_norm_le_sup_div_radius
    {g : ℂ → ℂ} {z₀ : ℂ} {ρ : ℝ} (hρ : 0 < ρ)
    (hg : DifferentiableOn ℂ g (closedBall z₀ ρ))
    {M : ℝ} (hM : ∀ w ∈ closedBall z₀ ρ, ‖g w‖ ≤ M) :
    ‖deriv g z₀‖ ≤ M / ρ := by
  sorry

/-- Step 3: fₙ' → F' uniformly on compact subsets. -/
theorem tendstoUniformlyOn_deriv
    {f : ℕ → ℂ → ℂ} {F : ℂ → ℂ}
    (hΩ : IsOpen Ω)
    (hf : ∀ n, DifferentiableOn ℂ (f n) Ω)
    (hF : ContinuousOn F Ω)
    (hconv : L1ConvergesOnDisks f F Ω)
    {K : Set ℂ} (hK : IsCompact K) (hKΩ : K ⊆ Ω) :
    TendstoUniformlyOn (fun n => deriv (f n)) (deriv F) atTop K := by
  sorry

/-- Full theorem packaged. -/
theorem L1_disk_convergence_analytic_upgrade
    {f : ℕ → ℂ → ℂ} {F : ℂ → ℂ}
    (hΩ : IsOpen Ω)
    (hf : ∀ n, DifferentiableOn ℂ (f n) Ω)
    (hF : ContinuousOn F Ω)
    (hconv : L1ConvergesOnDisks f F Ω) :
    DifferentiableOn ℂ F Ω ∧
    (∀ K, IsCompact K → K ⊆ Ω → TendstoUniformlyOn f F atTop K) ∧
    (∀ K, IsCompact K → K ⊆ Ω →
      TendstoUniformlyOn (fun n => deriv (f n)) (deriv F) atTop K) := by
  sorry

end
```

## 3. Mathlib cross-references

**Types/structures:**
- `DifferentiableOn ℂ f Ω` — holomorphicity on Ω (complex differentiability implies analyticity in Mathlib)
- `ContinuousOn F Ω` — continuity of the limit
- `TendstoUniformlyOn f F atTop K` — uniform convergence on K along ℕ
- `Metric.closedBall z r` — closed disk of radius r centered at z
- `MeasureTheory.volume` — Lebesgue measure on ℂ ≅ ℝ²
- `Filter.atTop` — the cofinite filter on ℕ
- `Filter.Tendsto` — convergence along a filter
- `IsCompact`, `IsOpen` — topological predicates

**Key lemmas:**
- `Complex.norm_integral_le_of_norm_le` — bound on contour integrals by sup of integrand
- `Complex.circleIntegral.integral_sub_inv_of_mem_ball` — Cauchy integral formula in Mathlib
- `Complex.deriv_circleIntegral` / related — derivative via Cauchy formula
- `DifferentiableOn.mono` — restriction of differentiability
- `IsCompact.exists_pos_forall_lt` — extracting uniform constants from compact sets (for the radius r)
- `IsCompact.subset_interior_iff` / `IsCompact.dist_subset_closure` — compact K has positive distance to ∂Ω
- `MeasureTheory.integral_mono` — monotonicity of integrals (for bounding L¹ norms)
- `MeasureTheory.integral_add_norm` — triangle inequality for integrals
- `tendstoUniformlyOn_iff` — characterisation of uniform convergence in terms of ε-δ
- `Complex.differentiableOn_of_forall_circleIntegral_eq_zero` — Morera's theorem (may not be packaged in Mathlib; would need to be stated)
- `Metric.isCompact_closedBall` — closed disks are compact
- `IsCompact.isBounded` — compact sets are bounded
- `Real.volume_closedBall` — volume of a disk (πr²)

**Tactic hints:**
- `intro` / `obtain` / `rcases` — unpacking hypotheses and filter convergence
- `calc` — for the chain of inequalities in the mean value estimate
- `gcongr` — for monotonicity-based inequality steps (e.g., bounding integrals)
- `positivity` — for πr² > 0 and similar
- `exact?` / `apply?` — finding the right Cauchy integral / mean value lemma
- `simp [closedBall]` — set membership simplification
- `linarith` / `nlinarith` — arithmetic combination of ε-δ bounds
- `filter_upwards` — working with the atTop filter for the convergence conclusion