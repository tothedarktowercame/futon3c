

# APM a93J03: Borel-Cantelli gives a.e. absolute convergence of a series

## 1. Informal proof

**Why it's hard.** Each fₙ is only controlled in a distributional sense (the set where |fₙ| exceeds n⁻² has small measure), not pointwise or in any Lp norm. Converting this into a.e. absolute convergence of the *series* requires showing that the "bad" behaviour happens on a negligible set.

**The key insight.** Borel-Cantelli: since Σ μ(Aₙ) ≤ Σ 2⁻ⁿ = 1 < ∞, almost every x belongs to only finitely many Aₙ. For such x, |fₙ(x)| ≤ n⁻² for all large n, and Σ n⁻² < ∞ gives absolute convergence by comparison.

**Proof.**

*Step 1: Apply Borel-Cantelli.* Define Aₙ = {x : |fₙ(x)| > n⁻²}. We have Σₙ μ(Aₙ) ≤ Σₙ 2⁻ⁿ = 1 < ∞. By the Borel-Cantelli lemma, μ(lim sup Aₙ) = 0, where lim sup Aₙ = ⋂ₘ ⋃_{n≥m} Aₙ is the set of x belonging to infinitely many Aₙ.

*Step 2: Pointwise bound outside the null set.* Let x ∉ lim sup Aₙ. Then x ∈ Aₙ for only finitely many n. So there exists N(x) such that for all n ≥ N(x), x ∉ Aₙ, i.e., |fₙ(x)| ≤ n⁻².

*Step 3: Absolute convergence.* For such x:

Σₙ |fₙ(x)| = Σₙ₌₁^{N-1} |fₙ(x)| + Σₙ₌ₙ^∞ |fₙ(x)| ≤ (finite sum) + Σₙ₌ₙ^∞ n⁻².

Since Σ n⁻² = π²/6 < ∞ (convergent p-series with p = 2 > 1), the tail Σ_{n≥N} n⁻² < ∞. So Σₙ |fₙ(x)| < ∞, i.e., the series is absolutely convergent.

*Step 4: Conclude.* Since μ(lim sup Aₙ) = 0, for a.e. x ∈ ℝ, Σₙ |fₙ(x)| < ∞. ∎

**What connects.** This is a direct application of the first Borel-Cantelli lemma combined with comparison to a convergent p-series. The two summability conditions — Σ μ(Aₙ) < ∞ (for Borel-Cantelli) and Σ n⁻² < ∞ (for the comparison) — work together: the first ensures that a.e. x eventually has |fₙ(x)| ≤ n⁻², and the second ensures that Σ n⁻² converges to provide the a.e. absolute convergence. The argument generalises: if μ({|fₙ| > aₙ}) ≤ bₙ with Σ bₙ < ∞ and Σ aₙ < ∞, then Σ fₙ converges absolutely a.e. The specific choice aₙ = n⁻² and bₙ = 2⁻ⁿ is just one instance where both sums converge.

## 2. Lean 4 theorem statement

```lean
import Mathlib.MeasureTheory.Measure.MeasureSpace
import Mathlib.Topology.Algebra.InfiniteSum.Basic
import Mathlib.Analysis.PSeries
import Mathlib.Order.Filter.Basic

open MeasureTheory Set Filter

noncomputable section

/-- Main theorem: Σ fₙ(x) converges absolutely for a.e. x. -/
theorem ae_summable_of_measure_decay
    {f : ℕ → ℝ → ℝ}
    (hfm : ∀ n, Measurable (f n))
    (hmeas : ∀ n, volume {x : ℝ | (↑n + 1)⁻² < |f n x|} ≤
      ENNReal.ofReal ((2 : ℝ)⁻¹ ^ n)) :
    ∀ᵐ x ∂(volume : Measure ℝ),
      Summable (fun n => f n x) := by
  sorry

/-- Stronger: Σ |fₙ(x)| < ∞ a.e. (absolute convergence). -/
theorem ae_absolutely_summable_of_measure_decay
    {f : ℕ → ℝ → ℝ}
    (hfm : ∀ n, Measurable (f n))
    (hmeas : ∀ n, volume {x : ℝ | (↑n + 1)⁻² < |f n x|} ≤
      ENNReal.ofReal ((2 : ℝ)⁻¹ ^ n)) :
    ∀ᵐ x ∂(volume : Measure ℝ),
      Summable (fun n => ‖f n x‖) := by
  sorry

/-- Borel-Cantelli: Σ μ(Aₙ) < ∞ ⟹ μ(lim sup Aₙ) = 0. -/
lemma borel_cantelli
    {A : ℕ → Set ℝ}
    (hA : ∀ n, MeasurableSet (A n))
    (hsum : ∑' n, volume (A n) ≠ ⊤) :
    volume (limsup A atTop) = 0 := by
  sorry

/-- The measure sum Σ 2⁻ⁿ is finite. -/
lemma measure_sum_geometric_finite
    {f : ℕ → ℝ → ℝ}
    (hmeas : ∀ n, volume {x : ℝ | (↑n + 1)⁻² < |f n x|} ≤
      ENNReal.ofReal ((2 : ℝ)⁻¹ ^ n)) :
    ∑' n, volume {x : ℝ | (↑n + 1)⁻² < |f n x|} ≠ ⊤ := by
  sorry

/-- Outside lim sup Aₙ: for large n, |fₙ(x)| ≤ n⁻². -/
lemma eventually_le_inv_sq_of_not_mem_limsup
    {f : ℕ → ℝ → ℝ}
    {x : ℝ}
    (hx : x ∉ limsup (fun n => {x : ℝ | (↑n + 1)⁻² < |f n x|}) atTop) :
    ∃ N, ∀ n, N ≤ n → |f n x| ≤ (↑n + 1)⁻² := by
  sorry

/-- Comparison: Σ n⁻² < ∞ (p-series with p=2). -/
lemma summable_inv_sq :
    Summable (fun n : ℕ => ((↑n + 1 : ℝ)⁻¹) ^ 2) := by
  sorry

/-- If |fₙ(x)| ≤ n⁻² eventually, then Σ |fₙ(x)| < ∞. -/
lemma summable_of_eventually_le_inv_sq
    {f : ℕ → ℝ → ℝ} {x : ℝ} {N : ℕ}
    (hbound : ∀ n, N ≤ n → |f n x| ≤ (↑n + 1)⁻²) :
    Summable (fun n => ‖f n x‖) := by
  sorry

end
```

## 3. Mathlib cross-references

**Types/structures:**
- `MeasureTheory.volume` — Lebesgue measure on ℝ
- `Filter.limsup` — lim sup of a sequence of sets
- `Filter.atTop` — cofinite filter on ℕ
- `Summable` — absolute summability of a sequence
- `ENNReal` — extended nonneg reals for measure values
- `∀ᵐ x ∂μ` — for almost every x

**Key lemmas:**
- `MeasureTheory.measure_limsup_eq_zero` — Borel-Cantelli lemma: Σ μ(Aₙ) < ∞ ⟹ μ(lim sup Aₙ) = 0. In `Mathlib.MeasureTheory.Measure.MeasureSpace`
- `Real.summable_nat_rpow_inv` — Σ n⁻ˢ converges for s > 1 (p-series). Or `Real.summable_nat_rpow` with appropriate sign convention
- `summable_of_summable_norm` — absolute summability implies summability
- `Summable.of_nonneg_of_le` — comparison test: if |aₙ| ≤ bₙ eventually and Σ bₙ < ∞ then Σ aₙ abs. converges
- `summable_of_sum_le` / `Summable.comp_injective` — summability comparisons
- `ENNReal.tsum_le_tsum` — bounding Σ μ(Aₙ) ≤ Σ 2⁻ⁿ
- `ENNReal.summable` — all ℝ≥0∞-valued series are (unconditionally) summable
- `tsum_geometric_of_lt_one` — Σ rⁿ = 1/(1-r) for 0 ≤ r < 1
- `Set.mem_iInter` / `Set.mem_iUnion` — membership in lim sup sets
- `Filter.eventually_atTop` — ∃ N, ∀ n ≥ N characterisation

**Tactic hints:**
- `filter_upwards [measure_limsup_eq_zero ...]` — converting μ(lim sup) = 0 to ∀ᵐ
- `intro x hx` — fix a point outside the null set
- `obtain ⟨N, hN⟩ := eventually_le_inv_sq_of_not_mem_limsup hx` — extract the threshold
- `exact summable_of_eventually_le_inv_sq hN` — conclude by comparison
- `apply Summable.of_nonneg_of_le` — comparison test
- `positivity` — for (n+1)⁻² > 0 and 2⁻ⁿ > 0
- `norm_num` — for 1/2 < 1 (in the geometric series)
- `gcongr` — for the tsum comparison Σ μ(Aₙ) ≤ Σ 2⁻ⁿ
- `simp [abs_le]` — for |fₙ(x)| ≤ n⁻² ↔ membership conditions
- `omega` — for natural number arithmetic