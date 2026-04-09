

# APM a00J04: Components of the complement of a polynomial lemniscate

## 1. Informal proof

**Why it's hard.** The level set L = {|p(z)| = 1} can be topologically complex — self-intersecting with multiple loops — and counting connected components of its complement requires linking the curve's topology to the polynomial's algebraic degree. You cannot simply "see" the answer.

**The key insight.** Every component of ℂ \ L is either sublevel ({|p| < 1}) or superlevel ({|p| > 1}) by continuity and connectedness. The maximum modulus principle kills bounded superlevel components, giving exactly one superlevel component. The minimum modulus principle forces each sublevel component to contain a root of p, giving at most N sublevel components.

**Proof.** Let p have degree N ≥ 1.

*Step 1: Each component is purely sublevel or superlevel.* If U is a connected component of ℂ \ L, then |p| is continuous on U and never equals 1. Since U is connected and (0,∞) \ {1} = (0,1) ∪ (1,∞) is disconnected, |p(U)| lies entirely in one piece.

*Step 2: Exactly one superlevel component.* Suppose U is a bounded open connected set with |p| > 1 on U. Then ∂U ⊂ L, so |p| = 1 on ∂U. By the maximum modulus principle, |p(z)| ≤ max_{∂U} |p| = 1 for z ∈ U, contradicting |p| > 1. So every superlevel component is unbounded. Since |p(z)| → ∞ as |z| → ∞, the set {|z| > R} ⊂ {|p| > 1} for R large enough. This set is connected, and every unbounded superlevel component meets it, so there is exactly one superlevel component.

*Step 3: Each sublevel component contains a root.* Let U be a sublevel component. Since |p| → ∞, U is bounded. On the compact closure, |p| attains its minimum. On ∂U ⊂ L, |p| = 1; on U, |p| < 1. So the minimum is in U. If p never vanished on U, then 1/p would be holomorphic on U with |1/p| = 1 on ∂U and |1/p| > 1 on U, contradicting the maximum modulus principle. So p has a root in U.

*Step 4: Count.* Distinct components contain distinct roots (they are disjoint). p has at most N roots. So: at most N sublevel + 1 superlevel = N + 1 components. ∎

**What connects.** This problem connects to potential theory (L is a lemniscate, log|p| is subharmonic), polynomial dynamics (the bound N+1 is the first step in understanding Julia set topology), and the argument principle. The bound is sharp: p(z) = ∏(z − aₖ) with widely separated roots gives exactly N sublevel components plus one superlevel component.

## 2. Lean 4 theorem statement

```lean
import Mathlib.Analysis.Complex.Polynomial.Basic
import Mathlib.Analysis.Complex.AbsMax
import Mathlib.Topology.Connected.Basic

open Complex Polynomial Set Topology Metric

noncomputable section

variable {p : ℂ[X]} (hp : 0 < p.natDegree)

def lemniscate (p : ℂ[X]) : Set ℂ :=
  {z : ℂ | ‖Polynomial.aeval z p‖ = 1}

def superlevel (p : ℂ[X]) : Set ℂ :=
  {z : ℂ | 1 < ‖Polynomial.aeval z p‖}

def sublevel (p : ℂ[X]) : Set ℂ :=
  {z : ℂ | ‖Polynomial.aeval z p‖ < 1}

lemma isPreconnected_subset_superlevel_or_sublevel
    {S : Set ℂ} (hS : IsPreconnected S) (hSL : Disjoint S (lemniscate p)) :
    S ⊆ superlevel p ∨ S ⊆ sublevel p := by
  sorry

lemma not_isBounded_superlevel_component
    {U : Set ℂ} (hU : IsOpen U) (hC : IsConnected U)
    (hS : U ⊆ superlevel p) (hB : frontier U ⊆ lemniscate p) :
    ¬Bornology.IsBounded U := by
  sorry

lemma isConnected_superlevel (hp : 0 < p.natDegree) :
    IsConnected (superlevel p) := by
  sorry

lemma isBounded_sublevel (hp : 0 < p.natDegree) :
    Bornology.IsBounded (sublevel p) := by
  sorry

lemma exists_root_in_sublevel_component
    {U : Set ℂ} (hU : IsOpen U) (hC : IsConnected U)
    (hS : U ⊆ sublevel p) (hB : Bornology.IsBounded U) :
    ∃ z ∈ U, Polynomial.aeval z p = 0 := by
  sorry

lemma card_roots_le :
    (Polynomial.roots p).toFinset.card ≤ p.natDegree := by
  sorry

theorem connectedComponents_complement_lemniscate_le :
    Nat.card (ConnectedComponents (lemniscate p)ᶜ) ≤ p.natDegree + 1 := by
  sorry

end
```

## 3. Mathlib cross-references

**Types/structures:**
- `Polynomial ℂ` (alias `ℂ[X]`) — the polynomial ring
- `Polynomial.aeval` — polynomial evaluation as an algebra hom
- `Polynomial.roots` — multiset of roots
- `Polynomial.natDegree` — degree as a natural number
- `ConnectedComponents` — type of connected components of a topological space
- `Nat.card` — cardinality as a natural number
- `Bornology.IsBounded` — boundedness predicate
- `IsConnected`, `IsPreconnected` — connectivity predicates
- `Metric.closedBall`, `Metric.ball` — metric balls

**Key lemmas:**
- `Complex.norm_eq_abs` — ‖z‖ = Complex.abs z
- `Polynomial.aeval_eq_sum_range` — evaluation as a sum
- `Polynomial.degree_pos_of_ne_zero` — positive degree facts
- `Polynomial.card_roots_le_degree` — number of roots ≤ degree
- `Multiset.toFinset_card_le_card` — toFinset doesn't increase cardinality
- `Complex.isMaxOn_of_isCompact_of_continuous` / `Complex.norm_le_norm_of_forall_norm_le` — maximum modulus principle ingredients in `Mathlib.Analysis.Complex.AbsMax`
- `IsPreconnected.image` — image of a preconnected set under continuous map is preconnected
- `isPreconnected_iff_subset_of_disjoint` — characterisation of preconnectedness via disjoint open covers
- `IsCompact.exists_isMinOn` — minimum on compact sets
- `Polynomial.tendsto_atTop` / `Polynomial.tendsto_norm_atTop` — |p(z)| → ∞ as |z| → ∞
- `isConnected_compl_of_isBounded` — connectivity of complement of bounded set (for the unbounded superlevel region)
- `frontier_subset_closure` — boundary is in closure
- `Nat.card_le_of_injective` — bound cardinality via injection

**Tactic hints:**
- `by_contra` — for the maximum modulus contradiction
- `exact?` / `apply?` — finding the right maximum principle lemma
- `positivity` — for norm positivity goals
- `simp [lemniscate, superlevel, sublevel]` — unfolding definitions
- `rcases` — destructuring existence statements from root-finding
- `calc` — for the chain of inequalities in the counting step
- `omega` — for the final N + 1 arithmetic