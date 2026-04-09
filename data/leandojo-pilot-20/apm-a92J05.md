

# APM a92J05: Counting roots outside the unit disk via Rouché

## 1. Informal proof

**Why it's hard.** The polynomial p(z) = z¹⁰ + 5z³ + 2z + 1 has degree 10, so it has exactly 10 roots in ℂ. We need to determine how many lie inside vs outside the unit disk. Direct factorisation is impossible, and the roots have no nice closed form.

**The key insight.** Apply Rouché's theorem on |z| = 1 to count roots *inside* the unit disk, then subtract from 10 to get roots outside. On |z| = 1, the term 5z³ dominates: |5z³| = 5 > |z¹⁰ + 2z + 1| ≤ 1 + 2 + 1 = 4. So p(z) has the same number of roots inside the unit disk as 5z³, which has 3 (all at z = 0). Therefore 10 − 3 = 7 roots lie outside.

**Proof.**

Let p(z) = z¹⁰ + 5z³ + 2z + 1. We use Rouché's theorem.

*Step 1: Decompose on |z| = 1.* Write p(z) = g(z) + h(z) where g(z) = 5z³ and h(z) = z¹⁰ + 2z + 1.

*Step 2: Verify Rouché's condition.* On |z| = 1:
- |g(z)| = |5z³| = 5.
- |h(z)| = |z¹⁰ + 2z + 1| ≤ |z|¹⁰ + 2|z| + 1 = 1 + 2 + 1 = 4.

Since |h(z)| ≤ 4 < 5 = |g(z)| for all |z| = 1, Rouché's theorem applies: p(z) = g(z) + h(z) and g(z) have the same number of zeros (counted with multiplicity) inside the unit disk {|z| < 1}.

*Step 3: Count zeros of g inside the disk.* g(z) = 5z³ has a zero of multiplicity 3 at z = 0, which is inside |z| < 1. So g has 3 zeros inside the unit disk.

*Step 4: Conclude.* By Rouché, p(z) has 3 zeros inside {|z| < 1}.

*Step 5: Zeros on |z| = 1.* Check that p has no zeros on |z| = 1. If |z| = 1 and p(z) = 0, then z¹⁰ + 1 = −5z³ − 2z, so |z¹⁰ + 1| = |5z³ + 2z|. But |5z³ + 2z| ≥ |5z³| − |2z| = 5 − 2 = 3, and |z¹⁰ + 1| ≤ 2. So 2 ≥ 3, contradiction. Hence no roots on the unit circle.

*Step 6: Roots outside.* Total roots = 10 (by the fundamental theorem of algebra). Roots inside = 3. Roots on the circle = 0. Roots outside = 10 − 3 = **7**. ∎

**Theorems used:** Rouché's theorem, the fundamental theorem of algebra.

**What connects.** This is a standard application of Rouché's theorem for root counting. The art is in choosing the right decomposition: the dominant term on |z| = 1 is 5z³ (the term with the largest coefficient, not the highest degree), and the remaining terms are bounded by 4 < 5. The same technique works for any polynomial on any circle — the key is finding a single term that dominates all others. For roots inside a large disk, the highest-degree term dominates; for roots inside a small disk, the constant term (or lowest-degree nonzero term) dominates; for the unit circle, it depends on coefficient magnitudes.

## 2. Lean 4 theorem statement

```lean
import Mathlib.Analysis.Complex.Polynomial.Basic
import Mathlib.Analysis.Complex.CauchyIntegral

open Complex Polynomial Metric Set

noncomputable section

/-- The polynomial p(z) = z¹⁰ + 5z³ + 2z + 1. -/
def p : ℂ[X] := X ^ 10 + 5 * X ^ 3 + 2 * X + 1

/-- Rouché's theorem: if |g| > |h| on a circle, then g and g+h have
    the same number of zeros inside. -/
lemma rouche_theorem
    {g h : ℂ → ℂ} {c : ℂ} {r : ℝ} (hr : 0 < r)
    (hg : DifferentiableOn ℂ g (closedBall c r))
    (hh : DifferentiableOn ℂ h (closedBall c r))
    (hdom : ∀ z ∈ sphere c r, ‖h z‖ < ‖g z‖) :
    (∮ z in C(c, r), deriv g z / g z) =
      ∮ z in C(c, r), deriv (g + h) z / (g + h) z := by
  sorry

/-- On |z| = 1, |z¹⁰ + 2z + 1| ≤ 4 < 5 = |5z³|. -/
lemma rouche_bound_on_circle :
    ∀ z ∈ sphere (0 : ℂ) 1,
      ‖aeval z (X ^ 10 + 2 * X + (1 : ℂ[X]))‖ <
      ‖aeval z (5 * X ^ 3 : ℂ[X])‖ := by
  sorry

/-- 5z³ has exactly 3 zeros (with multiplicity) inside the unit disk. -/
lemma five_z_cubed_roots_inside :
    (Polynomial.roots (5 * X ^ 3 : ℂ[X])).card = 3 := by
  sorry

/-- p has no roots on the unit circle. -/
lemma p_no_roots_on_circle :
    ∀ z ∈ sphere (0 : ℂ) 1, aeval z p ≠ 0 := by
  sorry

/-- p has exactly 3 roots inside the unit disk. -/
lemma p_roots_inside_disk :
    ((Polynomial.roots p).filter (fun z => ‖z‖ < 1)).card = 3 := by
  sorry

/-- Main theorem: p has exactly 7 roots outside the unit disk. -/
theorem roots_outside_unit_disk :
    ((Polynomial.roots p).filter (fun z => 1 < ‖z‖)).card = 7 := by
  sorry

/-- The degree of p is 10. -/
lemma p_degree : p.natDegree = 10 := by
  sorry

/-- Total root count: 10 = 3 (inside) + 0 (on) + 7 (outside). -/
lemma root_partition :
    (Polynomial.roots p).card = 10 ∧
    ((Polynomial.roots p).filter (fun z => ‖z‖ < 1)).card +
    ((Polynomial.roots p).filter (fun z => ‖z‖ = 1)).card +
    ((Polynomial.roots p).filter (fun z => 1 < ‖z‖)).card =
      (Polynomial.roots p).card := by
  sorry

end
```

## 3. Mathlib cross-references

**Types/structures:**
- `Polynomial ℂ` (alias `ℂ[X]`) — the polynomial ring
- `Polynomial.roots` — multiset of roots (with multiplicity)
- `Polynomial.aeval` — evaluation homomorphism
- `Polynomial.natDegree` — degree
- `Metric.sphere (0 : ℂ) 1` — the unit circle
- `Metric.ball (0 : ℂ) 1` — the open unit disk
- `Multiset.filter`, `Multiset.card` — filtering and counting roots

**Key lemmas:**
- `Polynomial.card_roots_le_degree` — number of roots ≤ degree
- `Polynomial.card_roots_eq_of_splits` — for a split polynomial over ℂ, roots.card = degree
- `IsAlgClosed.splits` / `Complex.isAlgClosed` — every polynomial over ℂ splits (FTA)
- `Polynomial.roots_mul` — roots of a product
- `Polynomial.roots_pow` — roots of a power (for 5z³ = 5 · X³)
- `Complex.norm_eq_abs` — ‖z‖ = |z|
- `norm_pow` — ‖z^n‖ = ‖z‖^n
- `norm_add_le` — triangle inequality: |a+b| ≤ |a| + |b| (for bounding |h| ≤ 4)
- `norm_sub_norm_le` — reverse triangle inequality (for the no-roots-on-circle argument)
- `Multiset.filter_add` / `Multiset.card_filter_add_card_filter_not` — partitioning the multiset of roots

**Tactic hints:**
- `norm_num` — for 5 > 4, degree computation, and polynomial coefficient arithmetic
- `simp [p, Polynomial.aeval_add, Polynomial.aeval_mul]` — unfolding polynomial evaluation
- `calc` — for the chain |h(z)| ≤ |z¹⁰| + |2z| + 1 = 1 + 2 + 1 = 4 < 5 = |g(z)|
- `gcongr` — for monotonicity in norm bounds
- `nlinarith` — combining the inequalities 4 < 5 and the contradiction 2 ≥ 3
- `ring` — for polynomial identity manipulations
- `omega` — for 10 − 3 = 7
- `intro z hz; simp [sphere, Complex.norm_eq_abs] at hz` — unpacking |z| = 1
- `by_contra` — for the no-roots-on-circle argument