

# APM a95J07: Vitali covering lemma (5×) and image of critical set has measure zero

This is essentially the same as a94A05. I'll give the statements with the specific formulation used here.

## 1. Informal proof

**Why it's hard.** Part (a) requires extracting a disjoint subcollection from an arbitrary (possibly uncountable) collection of intervals such that the 5× inflations still cover the union — a greedy algorithm with a careful domination argument. Part (b) applies (a) to show f(D) has measure zero for an *arbitrary* function f, which is surprising since no regularity of f is assumed beyond differentiability at points of D.

**The key insight.** (a) Greedily select intervals by decreasing length; any unselected interval is intersected by a selected one of at least half its length, placing it inside the 5× inflation. (b) At each x ∈ D, f'(x) = 0 means f maps a small interval around x to a tiny interval; cover D with such intervals, apply (a) to extract a disjoint subcollection, and the total image measure is bounded by ε times the total length.

**Proof.**

**(a) Vitali covering lemma.** Order intervals by length. Let d₁ = sup{|I| : I ∈ A}. Pick I₁ ∈ A with |I₁| > d₁/2. Remove all intervals meeting I₁. Let d₂ = sup of remaining lengths, pick I₂ with |I₂| > d₂/2. Continue inductively.

The Iₖ are pairwise disjoint by construction. If the process terminates (dₖ = 0 or A is exhausted), done. Otherwise, Σ|Iₖ| ≤ 1 (disjoint in (0,1)), so |Iₖ| → 0.

Claim: ∪A ⊆ ∪Iₖ'. Let x ∈ J for some J ∈ A. If J = Iₖ for some k, done. Otherwise J was excluded at some step k because J ∩ Iₖ ≠ ∅ and |Iₖ| ≥ dₖ/2 ≥ |J|/2. Since J and Iₖ overlap and |Iₖ| ≥ |J|/2, every point of J is within |J|/2 + |Iₖ| ≤ |Iₖ| + |Iₖ| + |Iₖ|/2 = 5|Iₖ|/2 of the center of Iₖ. So J ⊆ Iₖ'. ✓

**(b) f(D) has measure zero.** Fix ε > 0. For each x ∈ D, f'(x) = 0 gives: ∃δₓ > 0 such that |f(y) - f(x)| < ε|y - x| for |y - x| < δₓ. Let Jₓ = (x - δₓ, x + δₓ) ∩ (0,1). Then f(Jₓ) is contained in an interval of length < 2εδₓ = ε|Jₓ|.

Apply (a) to {Jₓ : x ∈ D}: extract disjoint J₁, J₂, ... with D ⊆ ∪Jₖ'. Since f is differentiable (hence continuous) at x, the image f(Jₖ') is contained in an interval of length ≤ 5ε|Jₖ| (the 5× interval has length 5|Jₖ|, and f maps it with slope < ε).

m*(f(D)) ≤ Σ m*(f(Jₖ')) ≤ Σ 5ε|Jₖ| ≤ 5ε · 1 = 5ε.

Since ε > 0 was arbitrary, m*(f(D)) = 0. ✓ ∎

**What connects.** Same as a94A05: the Vitali covering lemma is the geometric engine, and part (b) is the 1D Sard theorem for C⁰ functions. The result says the image of the critical set has measure zero without assuming any global regularity — only pointwise differentiability with zero derivative. This is sharp: the assumption f'(x) = 0 (not just f'(x) exists) is essential, as a Lipschitz function with f' ≠ 0 can have f(D) with positive measure for a different definition of "critical."

## 2. Lean 4 theorem statement

```lean
import Mathlib.MeasureTheory.Measure.Lebesgue.Basic
import Mathlib.Analysis.Calculus.Deriv.Basic

open Set MeasureTheory Metric

noncomputable section

/-- The 5× inflation of an open interval (α,β): same center, 5× length. -/
def inflate5 (I : Set ℝ) : Set ℝ :=
  let α := sInf I
  let β := sSup I
  let c := (α + β) / 2
  let r := (β - α) / 2
  Ioo (c - 5 * r / 2) (c + 5 * r / 2)

/-- (a) Vitali covering lemma with factor 5. -/
theorem vitali_covering_5
    {A : Set (Set ℝ)}
    (hA : ∀ I ∈ A, ∃ a b, a < b ∧ I = Ioo a b)
    (hA_sub : ∀ I ∈ A, I ⊆ Ioo 0 1) :
    ∃ S : ℕ → Set ℝ,
      (∀ k, S k ∈ A ∨ S k = ∅) ∧
      Pairwise (Disjoint on S) ∧
      ⋃₀ A ⊆ ⋃ k, inflate5 (S k) := by
  sorry

/-- The critical set: points where f is differentiable with f' = 0. -/
def criticalSet (f : ℝ → ℝ) : Set ℝ :=
  {x ∈ Ioo 0 1 | DifferentiableAt ℝ f x ∧ deriv f x = 0}

/-- (b) The image of the critical set has measure zero. -/
theorem measure_image_criticalSet_eq_zero
    {f : ℝ → ℝ} :
    volume (f '' criticalSet f) = 0 := by
  sorry

/-- At a critical point, f maps small intervals to tiny intervals. -/
lemma image_small_near_critical
    {f : ℝ → ℝ} {x : ℝ}
    (hx : x ∈ criticalSet f)
    {ε : ℝ} (hε : 0 < ε) :
    ∃ δ > 0, diam (f '' (Ioo (x - δ) (x + δ) ∩ Ioo 0 1)) ≤ ε * (2 * δ) := by
  sorry

/-- Disjoint intervals in (0,1) have total length ≤ 1. -/
lemma sum_length_disjoint_le_one
    {S : ℕ → Set ℝ}
    (hS_sub : ∀ k, S k ⊆ Ioo 0 1)
    (hS_disj : Pairwise (Disjoint on S)) :
    ∑' k, volume (S k) ≤ 1 := by
  sorry

/-- Outer measure of the image is bounded by ε times total length
    via the covering. -/
lemma measure_image_le_eps_of_covering
    {f : ℝ → ℝ} {ε : ℝ} (hε : 0 < ε)
    {S : ℕ → Set ℝ}
    (hS_disj : Pairwise (Disjoint on S))
    (hS_sub : ∀ k, S k ⊆ Ioo 0 1)
    (hcover : criticalSet f ⊆ ⋃ k, inflate5 (S k))
    (himage : ∀ k, diam (f '' inflate5 (S k)) ≤ 5 * ε * volume (S k) |>.toReal) :
    volume (f '' criticalSet f) ≤ ENNReal.ofReal (5 * ε) := by
  sorry

end
```

## 3. Mathlib cross-references

**Types/structures:**
- `MeasureTheory.volume` — Lebesgue measure / outer measure
- `DifferentiableAt ℝ f x` — differentiability at a point
- `deriv f x` — derivative value
- `Set.Ioo a b` — open intervals
- `Metric.diam` — diameter of a set
- `Pairwise (Disjoint on S)` — pairwise disjointness

**Key lemmas:**
- `Vitali.exists_disjoint_subfamily_covering_enlargment` — Mathlib's Vitali covering lemma (if available). In `Mathlib.MeasureTheory.Covering.Vitali`
- `HasDerivAt.tendsto_slope_zero` — f'(x) = 0 means (f(y)-f(x))/(y-x) → 0 (for the small-image lemma)
- `Metric.tendsto_nhds` — ε-δ from derivative = 0
- `MeasureTheory.measure_iUnion_le` — σ-subadditivity
- `MeasureTheory.measure_mono` — monotonicity
- `Real.volume_Ioo` — m(Ioo a b) = b - a
- `ENNReal.tsum_le_of_sum_le` — bounding infinite sums
- `Metric.diam_mono` — diam of subset ≤ diam of superset
- `Set.image_subset` — f(A) ⊆ f(B) when A ⊆ B

**Tactic hints:**
- `intro ε hε` — starting the measure-zero ε argument
- `choose δ hδ_pos hδ_bound using fun x hx => image_small_near_critical hx hε` — covering intervals
- `obtain ⟨S, hS_mem, hS_disj, hS_cover⟩ := vitali_covering_5 ...` — apply Vitali
- `calc volume (f '' criticalSet f) ≤ Σ volume (f '' inflate5 (S k)) ≤ 5ε · Σ volume (S k) ≤ 5ε` — chain
- `gcongr` — for monotonicity
- `positivity` — for ε > 0, δ > 0
- `linarith` — for interval length arithmetic
- `simp [inflate5]` — unfolding the inflation