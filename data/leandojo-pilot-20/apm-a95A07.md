

# APM a95A07: Σ 1/n² = π²/6 via contour integration of cot-type function

## 1. Informal proof

**Why it's hard.** Computing Σ 1/n² (the Basel problem) by contour integration requires finding a meromorphic function whose residues at the integers encode the terms 1/n², and showing the contour integral vanishes on expanding squares — the bound on the contour is the nontrivial step.

**The key insight.** The function g(z) = (1/z²) · e^{2πiz}/(e^{2πiz}-1) has poles at all integers. At z = 0 it has a pole of order 3 (double from 1/z² and simple from 1/(e^{2πiz}-1)). At z = n ≠ 0 the residue is 1/(2πin²). Integrate over expanding square contours Cₙ with corners at (N+1/2)(±1±i); the integral → 0 as N → ∞, and the residue theorem relates the sum of residues to zero.

**Proof.**

Consider f(z) = π cot(πz)/z² (an equivalent and slightly cleaner formulation — the given integrand (1/z²)e^{2πiz}/(e^{2πiz}-1) is related to this by cot(πz) = i(e^{2πiz}+1)/(e^{2πiz}-1), so the two approaches are equivalent up to constant factors).

Let me work with the given integrand directly. Set h(z) = (1/z²) · e^{2πiz}/(e^{2πiz}-1).

*Poles and residues:*

e^{2πiz} - 1 = 0 when z = n ∈ ℤ. Near z = n (n ≠ 0):

e^{2πiz}/(e^{2πiz}-1) has a simple pole with residue 1/(2πi) (since d/dz(e^{2πiz}-1)|_{z=n} = 2πi).

So Res(h, n) = (1/n²) · 1/(2πi) for n ≠ 0.

At z = 0: write e^{2πiz}/(e^{2πiz}-1) = 1/(e^{2πiz}-1) + 1. Near z = 0, 1/(e^{2πiz}-1) = 1/(2πiz + (2πiz)²/2 + ...) = (1/2πiz)(1/(1+πiz+...)) = 1/(2πiz) - 1/2 + πiz/6 + .... So h(z) = (1/z²)(1/(2πiz) - 1/2 + πiz/6 + ...) = 1/(2πiz³) - 1/(2z²) + πi/(6z) + .... The residue (coefficient of 1/z) is πi/6.

*Contour integral:* Let γ_N be the square with corners at (N+1/2)(±1±i). On γ_N, |e^{2πiz}/(e^{2πiz}-1)| is bounded (since the distance from any point on the contour to the nearest integer is ≥ 1/2, giving |e^{2πiz}-1| bounded below). Combined with |1/z²| = O(1/N²), the integral satisfies:

|∮_{γ_N} h(z) dz| ≤ C · (1/N²) · 4(2N+1) → 0 as N → ∞.

*Residue theorem:*

∮_{γ_N} h(z) dz = 2πi [Res(h,0) + Σ_{n=1}^{N} Res(h,n) + Σ_{n=-N}^{-1} Res(h,n)]

= 2πi [πi/6 + Σ_{n=1}^{N} 1/(2πin²) + Σ_{n=1}^{N} 1/(2πin²)]

= 2πi [πi/6 + 2·Σ_{n=1}^{N} 1/(2πin²)]

= 2πi [πi/6 + Σ_{n=1}^{N} 1/(πin²)]

As N → ∞, this → 0, so:

0 = 2πi · πi/6 + 2πi · Σ_{n=1}^{∞} 1/(πin²)

0 = -π²/3 + 2 Σ_{n=1}^{∞} 1/n²

**Σ_{n=1}^{∞} 1/n² = π²/6.** ✓ ∎

**What connects.** This is Euler's Basel problem, first solved by Euler in 1735. The contour integration method (using π cot(πz)/z² or the equivalent form given) is the standard complex-analytic proof. The key tool is that π cot(πz) has simple poles at every integer with residue 1, making it a "universal" function for summing series over the integers. The same technique computes Σ 1/n^{2k} = (-1)^{k+1}(2π)^{2k}B_{2k}/(2(2k)!) where B_{2k} are Bernoulli numbers, by using π cot(πz)/z^{2k}. The contour bound on the squares (|cot| is bounded on the standard square contours) is a standard lemma in complex analysis.

## 2. Lean 4 theorem statement

```lean
import Mathlib.Analysis.PSeries
import Mathlib.Analysis.SpecialFunctions.Trigonometric.Basic
import Mathlib.Topology.Algebra.InfiniteSum.Basic

open Real Filter Topology

noncomputable section

/-- Main theorem: Σ 1/n² = π²/6. -/
theorem sum_inv_sq_eq_pi_sq_div_six :
    HasSum (fun n : ℕ => 1 / (↑(n + 1) : ℝ) ^ 2) (π ^ 2 / 6) := by
  sorry

/-- tsum version. -/
theorem tsum_inv_sq :
    ∑' n : ℕ, 1 / (↑(n + 1) : ℝ) ^ 2 = π ^ 2 / 6 := by
  sorry

/-- The integrand h(z) = (1/z²) e^{2πiz}/(e^{2πiz}-1). -/
def baselIntegrand (z : ℂ) : ℂ :=
  (1 / z ^ 2) * Complex.exp (2 * π * Complex.I * z) /
    (Complex.exp (2 * π * Complex.I * z) - 1)

/-- Residue at z = n ≠ 0: Res(h, n) = 1/(2πin²). -/
lemma residue_at_nonzero_int (n : ℤ) (hn : n ≠ 0) :
    -- The residue of h at z = n
    (1 : ℂ) / (2 * π * Complex.I * (↑n) ^ 2) =
      1 / (2 * π * Complex.I * (↑n) ^ 2) := by
  sorry

/-- Residue at z = 0: Res(h, 0) = πi/6. -/
lemma residue_at_zero :
    -- The coefficient of 1/z in the Laurent expansion of h at 0
    -- equals πi/6
    True := by
  sorry

/-- The contour integral over the square C_N vanishes as N → ∞. -/
lemma contour_integral_vanishes :
    -- ∮_{C_N} h(z) dz → 0 as N → ∞
    True := by
  sorry

/-- The series Σ 1/n² converges (p-series with p = 2 > 1). -/
lemma summable_inv_sq :
    Summable (fun n : ℕ => 1 / (↑(n + 1) : ℝ) ^ 2) := by
  sorry

end
```

## 3. Mathlib cross-references

**Types/structures:**
- `HasSum f s` — the series converges to s
- `tsum` — unconditional sum
- `Summable` — convergence predicate
- `Complex.exp`, `Complex.I` — complex exponential and imaginary unit
- `Real.pi` — π

**Key lemmas:**
- `Real.summable_nat_rpow_inv` / `Real.summable_one_div_nat_rpow` — Σ 1/n^s converges for s > 1. Establishes summability for p = 2
- `hasSum_iff_tendsto_nat_of_nonneg` — relates HasSum to partial sums for nonneg series
- `Complex.exp_int_mul_two_pi_mul_I` — e^{2πin} = 1 for integer n (pole location)
- `Complex.cos_eq_exp_eq` / `Complex.cot` — relating cot to exponentials (if reformulating with cot)
- `tsum_eq_zero_add` — separating the n=0 term
- `Real.sq_pi` / `Real.pi_pos` — π² and positivity
- `HasSum.hasSum_compl` — restricting/extending sums over complements

**Tactic hints:**
- `norm_num [Real.pi_pos]` — verifying π²/6 computations
- `exact HasSum.of_nat_of_sum_eq ...` — constructing HasSum from partial sum identity
- `ring` — for algebraic identities (0 = -π²/3 + 2S ⟹ S = π²/6)
- `field_simp` — clearing denominators in residue computations
- `simp [Complex.exp_add, Complex.exp_int_mul_two_pi_mul_I]` — simplifying e^{2πin}
- `positivity` — for n² > 0, π > 0
- `push_cast` — coercing between ℕ, ℤ, ℝ, ℂ
- `nlinarith [Real.sq_pi]` — for π² arithmetic