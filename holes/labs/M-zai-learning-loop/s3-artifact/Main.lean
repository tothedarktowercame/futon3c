/-
! # APM a96J01: Uniformly convergent series of nonneg continuous functions
! whose sup-norms diverge

For each positive integer `n`, let `fₙ : [0,1] → ℝ` be continuous with
`fₙ ≥ 0`. We construct such a sequence where the partial sums
`∑_{n=1}^N fₙ` converge uniformly on `[0,1]`, yet
`∑ sup fₙ = ∞`.

**Answer: yes, such a sequence exists.**

## Construction

Partition `[0,1)` into adjacent intervals accumulating at `1`.
For `n : ℕ` (indexed from 0), let
`Iₙ = [aₙ, bₙ]` where `aₙ = 1 - 2⁻ⁿ` and `bₙ = 1 - 2⁻⁽ⁿ⁺¹⁾`.
So `I₀ = [0, 1/2]`, `I₁ = [1/2, 3/4]`, `I₂ = [3/4, 7/8]`, …

Define `fₙ` as a tent (triangular bump) on `Iₙ` with peak height `1/(n+1)`
at the midpoint of `Iₙ`, zero off `Iₙ`.

Then:
- Each `fₙ` is continuous and nonneg.
- `sup fₙ = 1/(n+1)`, so `∑ sup fₙ = ∑ 1/(n+1) = ∞` (harmonic series).
- Since the `Iₙ` have pairwise disjoint interiors, for each `x ∈ [0,1]`
  at most one `fₙ(x)` is nonzero, so the tail is bounded by `1/(N+2) → 0`,
  giving uniform convergence.
-/

import Mathlib

open Set Filter Topology

noncomputable section

/-- Left endpoint of the n-th interval: `1 - 2⁻ⁿ`. -/
def a (n : ℕ) : ℝ := 1 - (2 : ℝ)⁻¹ ^ n

/-- Right endpoint of the n-th interval: `1 - 2⁻⁽ⁿ⁺¹⁾`. -/
def b (n : ℕ) : ℝ := 1 - (2 : ℝ)⁻¹ ^ (n + 1)

/-- Midpoint of the n-th interval. -/
def mid (n : ℕ) : ℝ := (a n + b n) / 2

/-- Half-width of the n-th interval (= width of the n-th interval / 2). -/
def hw (n : ℕ) : ℝ := (b n - a n) / 2

/-- The tent function: peak `1/(n+1)` at `mid n`, zero off `[a n, b n]`. -/
def f (n : ℕ) (x : ℝ) : ℝ :=
  (1 / ((n + 1 : ℕ) : ℝ)) * max 0 (1 - |x - mid n| / hw n)

/-! ## Basic interval arithmetic -/

lemma pow_half_pos (n : ℕ) : 0 < (2 : ℝ)⁻¹ ^ n := by positivity

lemma a_def (n : ℕ) : a n = 1 - (2 : ℝ)⁻¹ ^ n := rfl

lemma b_def (n : ℕ) : b n = 1 - (2 : ℝ)⁻¹ ^ (n + 1) := rfl

/-- Width of the n-th interval is `2⁻⁽ⁿ⁺¹⁾`. -/
lemma width_eq (n : ℕ) : b n - a n = (2 : ℝ)⁻¹ ^ (n + 1) := by
  show (1 - (2:ℝ)⁻¹^(n+1)) - (1 - (2:ℝ)⁻¹^n) = (2:ℝ)⁻¹^(n+1)
  have h : (2 : ℝ)⁻¹ ^ n = (2 : ℝ)⁻¹ ^ (n + 1) * 2 := by
    rw [pow_succ]; field_simp
  linarith [h]

/-- Intervals are adjacent: `b n = a (n + 1)`. -/
lemma b_eq_a_succ (n : ℕ) : b n = a (n + 1) := rfl

/-- Half-width is `2⁻⁽ⁿ⁺²⁾`. -/
lemma hw_eq (n : ℕ) : hw n = (2 : ℝ)⁻¹ ^ (n + 2) := by
  show ((1 - (2:ℝ)⁻¹^(n+1)) - (1 - (2:ℝ)⁻¹^n)) / 2 = (2:ℝ)⁻¹^(n+2)
  have hw1 : (1 - (2:ℝ)⁻¹^(n+1)) - (1 - (2:ℝ)⁻¹^n) = (2:ℝ)⁻¹^(n+1) := width_eq n
  rw [hw1]
  have h : (2 : ℝ)⁻¹ ^ (n + 1) = 2 * (2 : ℝ)⁻¹ ^ (n + 2) := by
    rw [pow_succ]; ring
  linarith [h]

/-- Intervals have positive width. -/
lemma a_lt_b (n : ℕ) : a n < b n := by
  show 1 - (2:ℝ)⁻¹^n < 1 - (2:ℝ)⁻¹^(n+1)
  have : (2 : ℝ)⁻¹ ^ (n + 1) < (2 : ℝ)⁻¹ ^ n := by
    rw [pow_succ]
    have hhalf : (0:ℝ) < (2:ℝ)⁻¹ := by norm_num
    nlinarith [mul_pos (pow_pos hhalf n) hhalf]
  linarith

/-- Half-width is positive. -/
lemma hw_pos (n : ℕ) : 0 < hw n := by
  rw [hw_eq]; exact pow_half_pos (n + 2)

/-- The midpoint lies in the interval. -/
lemma a_le_mid (n : ℕ) : a n ≤ mid n := by
  rw [mid]; linarith [a_lt_b n]

lemma mid_le_b (n : ℕ) : mid n ≤ b n := by
  rw [mid]; linarith [a_lt_b n]

/-! ## Properties of the tent functions -/

/-- Each tent function is nonneg. -/
lemma f_nonneg (n : ℕ) (x : ℝ) : 0 ≤ f n x := by
  unfold f
  have : 0 ≤ max 0 (1 - |x - mid n| / hw n) := le_max_left 0 _
  positivity

/-- Each tent function is bounded above by `1/(n+1)`. -/
lemma f_le (n : ℕ) (x : ℝ) : f n x ≤ 1 / ((n + 1 : ℕ) : ℝ) := by
  unfold f
  have h_le1 : max 0 (1 - |x - mid n| / hw n) ≤ 1 := by
    apply (max_le_iff).mpr ⟨by norm_num, by
      have : 0 ≤ |x - mid n| / hw n := div_nonneg (abs_nonneg _) (le_of_lt (hw_pos n))
      linarith⟩
  have h_pos : 0 < (1 / ((n + 1 : ℕ) : ℝ)) := by positivity
  calc (1 / ((n + 1 : ℕ) : ℝ)) * max 0 (1 - |x - mid n| / hw n)
      ≤ (1 / ((n + 1 : ℕ) : ℝ)) * 1 := mul_le_mul_of_nonneg_left h_le1 h_pos.le
    _ = 1 / ((n + 1 : ℕ) : ℝ) := mul_one _

/-- The tent function attains its peak value `1/(n+1)` at the midpoint. -/
lemma f_mid (n : ℕ) : f n (mid n) = 1 / ((n + 1 : ℕ) : ℝ) := by
  unfold f
  rw [sub_self, abs_zero, zero_div, max_eq_right (by norm_num)]
  ring

/-- Each tent function is continuous. -/
lemma f_continuous (n : ℕ) : Continuous (f n) := by
  unfold f
  apply Continuous.mul
  · exact continuous_const
  · apply Continuous.max continuous_const
    have : Continuous (fun x => 1 - |x - mid n| / hw n) := by
      continuity
    exact this

/-! ## Interval ordering and midpoint bounds -/

/-- `a 0 = 0`. -/
lemma a_zero : a 0 = 0 := by simp [a]

/-- `a n` is nondecreasing (intervals march rightward). -/
lemma a_mono (n : ℕ) : a n ≤ a (n + 1) := by
  show 1 - (2:ℝ)⁻¹^n ≤ 1 - (2:ℝ)⁻¹^(n+1)
  have : (2 : ℝ)⁻¹ ^ (n + 1) ≤ (2 : ℝ)⁻¹ ^ n := by
    rw [pow_succ]
    have hhalf : (0:ℝ) < (2:ℝ)⁻¹ := by norm_num
    nlinarith [mul_pos (pow_pos hhalf n) hhalf]
  linarith

/-- All left endpoints are in `[0, 1)`. -/
lemma a_nonneg (n : ℕ) : 0 ≤ a n := by
  show (0:ℝ) ≤ 1 - (2:ℝ)⁻¹^n
  have h_half : (0:ℝ) ≤ (2:ℝ)⁻¹ := by norm_num
  have h_le1 : (2:ℝ)⁻¹^n ≤ 1 := pow_le_one₀ h_half (by norm_num : (2:ℝ)⁻¹ ≤ 1)
  linarith

lemma a_le_one (n : ℕ) : a n ≤ 1 := by
  show 1 - (2:ℝ)⁻¹^n ≤ (1:ℝ)
  have : (0:ℝ) ≤ (2:ℝ)⁻¹^n := (pow_pos (by norm_num : (0:ℝ) < (2:ℝ)⁻¹) n).le
  linarith

/-- All right endpoints are ≤ 1. -/
lemma b_le_one (n : ℕ) : b n ≤ 1 := by
  show 1 - (2:ℝ)⁻¹^(n+1) ≤ (1:ℝ)
  have : (0:ℝ) ≤ (2:ℝ)⁻¹^(n+1) := (pow_pos (by norm_num : (0:ℝ) < (2:ℝ)⁻¹) (n+1)).le
  linarith

/-- The midpoint is in `[0, 1]`. -/
lemma mid_mem (n : ℕ) : mid n ∈ Set.Icc (0:ℝ) 1 := by
  rw [Set.mem_Icc]
  refine ⟨?_, ?_⟩
  · have ha : 0 ≤ a n := a_nonneg n
    linarith [a_le_mid n]
  · have hb : b n ≤ 1 := b_le_one n
    linarith [mid_le_b n]

/-- `a` is monotone: `a j ≤ a k` when `j ≤ k`. -/
lemma a_monotone {j k : ℕ} (hjk : j ≤ k) : a j ≤ a k := by
  induction' hjk with k hk ih
  · rfl
  · exact le_trans ih (a_mono k)

/-- If `m > n` then `b n ≤ a m` (intervals don't overlap in interiors). -/
lemma le_of_lt_of_interval {n m : ℕ} (hnm : n < m) : b n ≤ a m := by
  rw [b_eq_a_succ n]
  exact a_monotone (Nat.succ_le_of_lt hnm)

/-! ## Support characterization and disjointness -/

/-- `mid n - hw n = a n`. -/
lemma mid_sub_hw (n : ℕ) : mid n - hw n = a n := by
  rw [mid, hw]; ring

/-- `mid n + hw n = b n`. -/
lemma mid_add_hw (n : ℕ) : mid n + hw n = b n := by
  rw [mid, hw]; ring

/-- `f n x > 0` implies `x` is in the open interval `(a n, b n)`. -/
lemma f_pos_imp (n : ℕ) {x : ℝ} (hfx : 0 < f n x) : a n < x ∧ x < b n := by
  unfold f at hfx
  have h_pos_coeff : 0 < (1 / ((n + 1 : ℕ) : ℝ)) := by positivity
  have h_max_pos : 0 < max 0 (1 - |x - mid n| / hw n) :=
    (mul_pos_iff_of_pos_left h_pos_coeff).mp hfx
  have h_inner : 0 < 1 - |x - mid n| / hw n := by
    by_contra h_neg
    push_neg at h_neg
    have : max 0 (1 - |x - mid n| / hw n) = 0 := max_eq_left h_neg
    linarith
  have h_abs_lt : |x - mid n| / hw n < 1 := by linarith
  have h_abs : |x - mid n| < hw n := by
    rwa [div_lt_iff₀ (hw_pos n), one_mul] at h_abs_lt
  rw [abs_sub_lt_iff] at h_abs
  refine ⟨?_, ?_⟩
  · calc a n = mid n - hw n := (mid_sub_hw n).symm
      _ < x := by linarith
  · calc x < mid n + hw n := by linarith
      _ = b n := mid_add_hw n

/-- At most one `f n x` is positive for any given `x`.
    If `f n x > 0` and `f m x > 0` then `n = m`. -/
lemma f_unique_pos {n m : ℕ} {x : ℝ} (hn : 0 < f n x) (hm : 0 < f m x) : n = m := by
  by_contra h_ne
  obtain (hnm | hmn) := lt_or_gt_of_ne h_ne
  · -- n < m: x < b n ≤ a m < x is a contradiction
    obtain ⟨hax_n, hxb_n⟩ := f_pos_imp n hn
    obtain ⟨hax_m, hxb_m⟩ := f_pos_imp m hm
    have : b n ≤ a m := le_of_lt_of_interval hnm
    linarith
  · -- m < n: symmetric
    obtain ⟨hax_n, hxb_n⟩ := f_pos_imp n hn
    obtain ⟨hax_m, hxb_m⟩ := f_pos_imp m hm
    have : b m ≤ a n := le_of_lt_of_interval hmn
    linarith

/-- If `f n x > 0`, then `f m x = 0` for all `m ≠ n`. -/
lemma f_eq_zero_of_ne {n m : ℕ} {x : ℝ} (hmn : m ≠ n) (hn : 0 < f n x) : f m x = 0 := by
  by_contra h_ne
  have hm_pos : 0 < f m x := lt_of_le_of_ne (f_nonneg m x) (Ne.symm h_ne)
  exact hmn (f_unique_pos (n := n) (m := m) hn hm_pos).symm

/-- At each point, the sum `Σ f n x` has at most one nonzero term.
    More precisely, `f n x ≠ 0` implies `f m x = 0` for `m ≠ n`. -/
lemma f_nonzero_unique {n : ℕ} {x : ℝ} (hn : f n x ≠ 0) : ∀ m, m ≠ n → f m x = 0 := by
  intro m hmn
  have hn_pos : 0 < f n x := lt_of_le_of_ne (f_nonneg n x) (Ne.symm hn)
  exact f_eq_zero_of_ne hmn hn_pos

/-! ## Pointwise summability and the limit function -/

/-- At each point, at most one term is nonzero, so the pointwise sum exists. -/
lemma f_summable (x : ℝ) : Summable (fun n => f n x) := by
  by_cases hex : ∃ n, f n x ≠ 0
  · obtain ⟨n, hn⟩ := hex
    exact (hasSum_single n (f_nonzero_unique hn)).summable
  · push_neg at hex
    have hzero : ∀ n, f n x = 0 := hex
    have : (fun n => f n x) = (0 : ℕ → ℝ) := by funext n; exact hzero n
    rw [this]
    exact summable_zero

/-- The limit function (pointwise sum). -/
def g (x : ℝ) : ℝ := ∑' n, f n x

/-- For x in `[0,1]`, the tail beyond N has at most one nonzero term, bounded by `1/(N+1)`. -/
lemma f_tail_le (N : ℕ) (x : ℝ) :
    ∑' k, f (k + N + 1) x ≤ 1 / ((N + 2 : ℕ) : ℝ) := by
  by_cases hex : ∃ k, f (k + N + 1) x ≠ 0
  · obtain ⟨k, hk⟩ := hex
    have hunique : ∀ j, j ≠ k → f (j + N + 1) x = 0 := by
      intro j hj
      have hjk : (j + N + 1 : ℕ) ≠ (k + N + 1) := by omega
      exact f_nonzero_unique hk (j + N + 1) hjk
    have hsum : ∑' j, f (j + N + 1) x = f (k + N + 1) x := by
      exact (hasSum_single k hunique).tsum_eq
    rw [hsum]
    have hle := f_le (k + N + 1) x
    have hidx : (k + N + 1 : ℕ) + 1 = k + N + 2 := by omega
    rw [hidx] at hle
    have hge : (N + 2 : ℕ) ≤ k + N + 2 := by omega
    have hfrac : 1 / ((k + N + 2 : ℕ) : ℝ) ≤ 1 / ((N + 2 : ℕ) : ℝ) := by
      have h_cast_le : ((N + 2 : ℕ) : ℝ) ≤ ((k + N + 2 : ℕ) : ℝ) := Nat.cast_le.mpr hge
      exact div_le_div₀ (by norm_num) (by norm_num : (1:ℝ) ≤ 1) (by positivity) h_cast_le
    linarith
  · push_neg at hex
    have hzero : ∀ k, f (k + N + 1) x = 0 := hex
    have hsum : ∑' k, f (k + N + 1) x = 0 := by
      apply HasSum.tsum_eq
      convert hasSum_single 0 (fun j _ => hzero j) using 1
      exact (hzero 0).symm
    rw [hsum]
    positivity

/-! ## Main theorem -/

/-- The partial sums converge uniformly on `[0,1]` to the pointwise sum `g`. -/
theorem apm_a96J01 :
    ∃ (f : ℕ → ℝ → ℝ),
      (∀ n, Continuous (f n)) ∧
      (∀ n, ∀ x ∈ Set.Icc (0:ℝ) 1, 0 ≤ f n x) ∧
      (∃ (g : ℝ → ℝ),
        TendstoUniformlyOn
          (fun N x => ∑ k ∈ Finset.range (N + 1), f k x) g Filter.atTop (Set.Icc 0 1)) ∧
      ¬ Summable (fun n => ⨆ x : Set.Icc (0:ℝ) 1, f n x.1) := by
  refine ⟨f, f_continuous, ?_, ?_, ?_⟩
  · exact fun n x hx => f_nonneg n x
  · -- Uniform convergence
    refine ⟨g, ?_⟩
    rw [Metric.tendstoUniformlyOn_iff]
    intro ε hε
    obtain ⟨N, hN⟩ : ∃ N : ℕ, 1 / ((N + 2 : ℕ) : ℝ) < ε := by
      obtain ⟨q, hq⟩ := exists_nat_one_div_lt hε
      refine ⟨q + 1, ?_⟩
      apply lt_of_le_of_lt _ hq
      have hge : ((q : ℝ) + 1) ≤ ((q + 3 : ℕ) : ℝ) := by norm_num
      exact div_le_div₀ (by norm_num) (by norm_num) (by positivity) hge
    exact eventually_atTop.mpr ⟨N, fun M hM x hx => by
      rw [Real.dist_eq]
      have hsum_split : ∑' k, f k x =
          (∑ k ∈ Finset.range (M + 1), f k x) + ∑' k, f (k + M + 1) x := by
        symm
        exact (f_summable x).sum_add_tsum_nat_add (M + 1)
      have htail : ∑' k, f (k + M + 1) x ≤ 1 / ((M + 2 : ℕ) : ℝ) := f_tail_le M x
      have htail_bound : 1 / ((M + 2 : ℕ) : ℝ) ≤ 1 / ((N + 2 : ℕ) : ℝ) := by
        apply div_le_div₀ (by norm_num) (by norm_num : (1:ℝ) ≤ 1) (by positivity)
        exact Nat.cast_le.mpr (Nat.add_le_add_right hM 2)
      have hge0 : 0 ≤ ∑' k, f (k + M + 1) x :=
        le_trans (f_tail_le M x) (div_nonneg (by norm_num) (Nat.cast_nonneg _))
      calc |g x - ∑ k ∈ Finset.range (M + 1), f k x|
          = |(∑' k, f k x) - ∑ k ∈ Finset.range (M + 1), f k x| := rfl
        _ = |∑' k, f (k + M + 1) x| := by rw [hsum_split]; ring_nf; rw [abs_of_nonneg hge0]
        _ ≤ ∑' k, f (k + M + 1) x := le_abs_self _
        _ ≤ 1 / ((M + 2 : ℕ) : ℝ) := htail
        _ ≤ 1 / ((N + 2 : ℕ) : ℝ) := htail_bound
        _ < ε := hN⟩
  · -- Divergence of sup series
    intro hs
    have h_not_summable : ¬ Summable (fun (n : ℕ) => (1 : ℝ) / ((n + 1 : ℕ) : ℝ)) := by
      intro h
      exact Real.not_summable_one_div_natCast ((summable_nat_add_iff 1).mp h)
    have h_sup_lower : ∀ n,
        1 / ((n + 1 : ℕ) : ℝ) ≤ ⨆ x : Set.Icc (0:ℝ) 1, f n x.1 := by
      intro n
      have h_fmid : f n (mid n) = 1 / ((n + 1 : ℕ) : ℝ) := f_mid n
      have hle : f n ((⟨mid n, mid_mem n⟩ : Set.Icc (0:ℝ) 1).1) ≤
          ⨆ x : Set.Icc (0:ℝ) 1, f n x.1 :=
        le_ciSup (fun _ => f_le n _) ⟨mid n, mid_mem n⟩
      exact h_fmid ▸ hle
    have h_sup_nonneg : ∀ n, 0 ≤ (⨆ x : Set.Icc (0:ℝ) 1, f n x.1) := fun n =>
      (h_sup_lower n).trans (by positivity)
    exact h_not_summable (hs.of_nonneg_of_le h_sup_nonneg h_sup_lower)