

# APM a92J02: Convergence in measure implies a.e. convergence of a subsequence

## 1. Informal proof

**Why it's hard.** Convergence in measure is a global/averaged condition (the *measure* of the bad set is small), while a.e. convergence is a pointwise condition (each *individual point* eventually behaves). The full sequence need not converge a.e. (the typewriter sequence converges in measure to 0 but not a.e.), so extracting a subsequence requires a careful diagonal/summability argument.

**The key insight.** Choose a subsequence f_{n_k} so fast that m({|f_{n_k} - f| > 1/k}) < 2^{-k}. Then Σ m(E_k) < ∞ where E_k = {|f_{n_k} - f| > 1/k}, so Borel-Cantelli gives: a.e. x belongs to only finitely many E_k, meaning f_{n_k}(x) → f(x) for a.e. x.

**Proof.**

*Step 1: Extract the subsequence.* Since f_n → f in measure, for each k ∈ ℕ and ε = 1/k, there exists N_k such that n > N_k implies m({|f_n - f| > 1/k}) < 2^{-k}. Define n_1 = N_1 + 1. Inductively, choose n_{k+1} > max(n_k, N_{k+1}) + 1. Then n_1 < n_2 < ··· and:

$$m\left(\{x : |f_{n_k}(x) - f(x)| > 1/k\}\right) < 2^{-k} \quad \text{for all } k.$$

*Step 2: Apply Borel-Cantelli.* Define E_k = {x : |f_{n_k}(x) - f(x)| > 1/k}. Then Σ_{k=1}^∞ m(E_k) < Σ 2^{-k} = 1 < ∞. By the Borel-Cantelli lemma, m(lim sup E_k) = 0, where lim sup E_k = ⋂_{j=1}^∞ ⋃_{k=j}^∞ E_k is the set of points belonging to infinitely many E_k.

*Step 3: Pointwise convergence off the null set.* Let x ∉ lim sup E_k. Then x ∈ E_k for only finitely many k, so there exists K such that for all k ≥ K, x ∉ E_k, meaning |f_{n_k}(x) - f(x)| ≤ 1/k. Since 1/k → 0, f_{n_k}(x) → f(x).

Therefore f_{n_k} → f a.e. ∎

**What connects.** This is a foundational result in measure theory, used constantly: it bridges convergence in measure (a topological condition in the space of measurable functions) and a.e. convergence (a pointwise condition). The proof is a prototype of the "fast subsequence + Borel-Cantelli" technique that appears throughout probability and analysis. The Borel-Cantelli lemma is the engine: it converts summable measure bounds into an a.e. statement. The result combined with its partial converse (a.e. convergence implies convergence in measure on finite measure spaces, by Egorov) gives the complete picture of the relationship between these two modes of convergence. In probability, this is the statement that convergence in probability implies a.s. convergence along a subsequence.

## 2. Lean 4 theorem statement

```lean
import Mathlib.MeasureTheory.Measure.MeasureSpace
import Mathlib.MeasureTheory.Measure.NullMeasurable
import Mathlib.Order.Filter.Basic
import Mathlib.Topology.Algebra.InfiniteSum.Basic

open MeasureTheory Set Filter

noncomputable section

/-- Convergence in measure: m({|fₙ - f| > ε}) → 0 for each ε > 0. -/
def TendstoInMeasure (μ : Measure ℝ) (f : ℕ → ℝ → ℝ) (g : ℝ → ℝ) : Prop :=
  ∀ ε : ℝ, 0 < ε →
    Tendsto (fun n => μ {x | ε < |f n x - g x|}) atTop (𝓝 0)

/-- The fast subsequence: choose n_k so that m({|f_{n_k} - f| > 1/k}) < 2^{-k}. -/
lemma exists_fast_subsequence
    {μ : Measure ℝ} {f : ℕ → ℝ → ℝ} {g : ℝ → ℝ}
    (hconv : TendstoInMeasure μ f g) :
    ∃ φ : ℕ → ℕ, StrictMono φ ∧
      ∀ k : ℕ, μ {x | (1 : ℝ) / (↑k + 1) < |f (φ k) x - g x|} <
        ENNReal.ofReal ((1 : ℝ) / 2 ^ k) := by
  sorry

/-- Borel-Cantelli: if Σ μ(Eₖ) < ∞ then μ(lim sup Eₖ) = 0. -/
lemma measure_limsup_eq_zero_of_summable
    {μ : Measure ℝ} {E : ℕ → Set ℝ}
    (hE : ∀ k, MeasurableSet (E k))
    (hsum : ∑' k, μ (E k) ≠ ⊤) :
    μ (limsup E atTop) = 0 := by
  sorry

/-- Outside lim sup Eₖ, the subsequence converges pointwise. -/
lemma tendsto_of_not_mem_limsup
    {f : ℕ → ℝ → ℝ} {g : ℝ → ℝ} {φ : ℕ → ℕ}
    {E : ℕ → Set ℝ}
    (hE : ∀ k, E k = {x | (1 : ℝ) / (↑k + 1) < |f (φ k) x - g x|})
    {x : ℝ} (hx : x ∉ limsup E atTop) :
    Tendsto (fun k => f (φ k) x) atTop (𝓝 (g x)) := by
  sorry

/-- Main theorem: convergence in measure implies a.e. convergence
    of a subsequence. -/
theorem tendstoInMeasure_imp_ae_tendsto_subseq
    {μ : Measure ℝ} {f : ℕ → ℝ → ℝ} {g : ℝ → ℝ}
    (hfm : ∀ n, Measurable (f n))
    (hgm : Measurable g)
    (hconv : TendstoInMeasure μ f g) :
    ∃ φ : ℕ → ℕ, StrictMono φ ∧
      ∀ᵐ x ∂μ, Tendsto (fun k => f (φ k) x) atTop (𝓝 (g x)) := by
  sorry

end
```

## 3. Mathlib cross-references

**Types/structures:**
- `MeasureTheory.Measure` — the measure μ
- `MeasureTheory.MeasureSpace` — measure space structure
- `Filter.atTop` — the cofinite filter on ℕ
- `Filter.limsup` — lim sup of a sequence of sets (⋂_j ⋃_{k≥j} E_k)
- `Filter.Tendsto` — convergence along a filter
- `ENNReal` — extended nonneg reals (for measure values)
- `StrictMono` — strictly monotone function (for the subsequence)
- `∀ᵐ x ∂μ` — for almost every x (abbreviation for `∀ᶠ x in ae μ`)

**Key lemmas:**
- `MeasureTheory.measure_limsup_eq_zero` — Borel-Cantelli lemma: if Σ μ(Eₖ) < ∞ then μ(lim sup Eₖ) = 0. This is the core tool. In `Mathlib.MeasureTheory.Measure.MeasureSpace`
- `MeasureTheory.TendstoInMeasure` — Mathlib may already define this (in `Mathlib.MeasureTheory.Function.ConvergenceInMeasure`)
- `MeasureTheory.TendstoInMeasure.exists_seq_tendsto_ae` — Mathlib likely has this exact theorem packaged! In `Mathlib.MeasureTheory.Function.ConvergenceInMeasure`
- `Filter.Tendsto.exists_forall_ge` — extracting N from a limit (for choosing the subsequence)
- `ENNReal.summable` — all ℝ≥0∞-valued series are summable (for applying tsum)
- `ENNReal.tsum_lt_top` — sufficient conditions for finite sum
- `summable_geometric_of_lt_one` — Σ 2^{-k} < ∞
- `Set.mem_iInter` / `Set.mem_iUnion` — membership in lim sup = ⋂⋃
- `Metric.tendsto_atTop` — ε-N characterisation of convergence in a metric space
- `StrictMono.nat` — constructing strictly monotone sequences inductively

**Tactic hints:**
- `choose φ hφ_mono hφ_bound using exists_fast_subsequence hconv` — extract the subsequence
- `refine ⟨φ, hφ_mono, ?_⟩` — package the result
- `filter_upwards [measure_limsup_eq_zero ...]` — convert μ(lim sup) = 0 to ∀ᵐ
- `intro x hx` — fix a point outside the null set
- `rw [Metric.tendsto_atTop]` — unfold convergence
- `obtain ⟨K, hK⟩ := hx` — extract the index beyond which x ∉ E_k
- `calc` — for |f_{n_k}(x) - f(x)| ≤ 1/k < ε
- `positivity` — for 1/k > 0, 2^{-k} > 0
- `omega` / `linarith` — for index arithmetic (n_{k+1} > n_k)
- `gcongr` — for bounding measures monotonically
- `exact?` — finding the Borel-Cantelli lemma