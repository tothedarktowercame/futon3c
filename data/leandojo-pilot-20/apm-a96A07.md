

# APM a96A07: Injective on the boundary implies injective on the disk

## 1. Informal proof

**Why it's hard.** Injectivity on the boundary ∂Δ does not trivially extend to the interior — the function could fold the interior even while being injective on the boundary. The proof requires connecting boundary behaviour to interior behaviour via the argument principle, which counts preimages using winding numbers.

**The key insight.** For any w ∈ D (the bounded region enclosed by C = g(Γ)), the number of preimages of w under g in Δ equals the winding number of g(Γ) around w, which is 1 (since g is injective on Γ, g(Γ) is a simple closed curve, and w is inside it). So g takes each value in D exactly once. For w ∉ D̄, the winding number is 0, so g doesn't take those values.

**Proof.**

*Step 1: g(Γ) = C is a Jordan curve.* Since g is continuous and injective on Γ (a simple closed curve), C = g(Γ) is also a simple closed curve. By the Jordan curve theorem, C bounds a bounded domain D and an unbounded domain.

*Step 2: g(Δ) ⊆ D̄.* Since Δ̄ is connected and g is continuous, g(Δ̄) is connected. Since g(∂Δ) = C = ∂D, and g(Δ̄) is a connected compact set whose boundary maps to ∂D, the interior g(Δ) must lie in one component of ℂ \ C. Since g(Δ) is bounded (g(Δ̄) is compact), g(Δ) lies in the bounded component D. So g(Δ) ⊆ D.

*Step 3: Counting preimages via the argument principle.* For w ∈ D \ g(Γ) = D (since w ∉ C), the number of solutions to g(z) = w in Δ (counted with multiplicity) equals:

N(w) = (1/2πi) ∮_Γ g'(z)/(g(z)-w) dz.

This equals the winding number of C = g(Γ) around w. Since C is a simple closed curve and w ∈ D (the interior), ind(w; C) = ±1 (depending on orientation). Since g is analytic (hence orientation-preserving — g' ≠ 0 generically), we get N(w) = 1.

*Step 4: g is injective on Δ.* N(w) = 1 means each w ∈ D has exactly one preimage in Δ (counted with multiplicity). If the preimage has multiplicity > 1, then g'(z₀) = 0 at that preimage, but then nearby values would have multiple distinct preimages (by the local mapping theorem), contradicting N = 1 for nearby values. So each preimage has multiplicity 1, meaning g is injective.

*Step 5: g maps Δ onto D.* For any w ∈ D, N(w) = 1, so w has a preimage in Δ. So g(Δ) = D, and g : Δ → D is bijective. ✓ ∎

**What connects.** This is a fundamental result in conformal mapping theory: injectivity on the boundary of a disk implies injectivity on the disk for analytic functions. It's a consequence of the argument principle, which converts the topological property (winding number = 1 for a simple curve) into the analytic property (exactly one preimage). The result is used in the constructive aspects of the Riemann mapping theorem and in the study of schlicht functions. The key tools are: Jordan curve theorem (g(Γ) bounds a domain), the argument principle (counting preimages), and the local mapping theorem (multiplicity 1 ⟹ injectivity).

## 2. Lean 4 theorem statement

```lean
import Mathlib.Analysis.Complex.CauchyIntegral
import Mathlib.Topology.Connected.Basic

open Complex Metric Set

noncomputable section

/-- Main theorem: g injective on ∂Δ ⟹ g injective on Δ with g(Δ) = D. -/
theorem injOn_disk_of_injOn_circle
    {g : ℂ → ℂ}
    (hg : ∃ ε > 0, DifferentiableOn ℂ g (closedBall 0 (1 + ε)))
    (hg_inj_bdy : InjOn g (sphere (0 : ℂ) 1)) :
    InjOn g (ball (0 : ℂ) 1) := by
  sorry

/-- g maps Δ onto the bounded domain D enclosed by g(∂Δ). -/
theorem surjOn_domain_of_injOn_circle
    {g : ℂ → ℂ}
    (hg : ∃ ε > 0, DifferentiableOn ℂ g (closedBall 0 (1 + ε)))
    (hg_inj_bdy : InjOn g (sphere (0 : ℂ) 1)) :
    ∃ D : Set ℂ, IsOpen D ∧ IsConnected D ∧ Bornology.IsBounded D ∧
      frontier D = g '' sphere (0 : ℂ) 1 ∧
      g '' ball (0 : ℂ) 1 = D := by
  sorry

/-- Full statement: g is a bijection from Δ to D. -/
theorem bijOn_of_injOn_circle
    {g : ℂ → ℂ}
    (hg : ∃ ε > 0, DifferentiableOn ℂ g (closedBall 0 (1 + ε)))
    (hg_inj_bdy : InjOn g (sphere (0 : ℂ) 1)) :
    ∃ D : Set ℂ, IsOpen D ∧
      BijOn g (ball (0 : ℂ) 1) D := by
  sorry

/-- The number of preimages of w in Δ equals the winding number
    of g(∂Δ) around w. -/
lemma preimage_count_eq_winding
    {g : ℂ → ℂ}
    (hg : DifferentiableOn ℂ g (closedBall 0 1))
    {w : ℂ} (hw : w ∉ g '' sphere (0 : ℂ) 1) :
    (1 / (2 * π * I)) * ∮ z in C(0, 1), deriv g z / (g z - w) =
      -- number of solutions to g(z) = w in ball 0 1 (with multiplicity)
      ↑(Finset.card (sorry : Finset ℂ)) := by
  sorry

/-- Image of a Jordan curve under an injective continuous map
    is a Jordan curve. -/
lemma jordan_curve_image
    {g : ℂ → ℂ} (hg_cont : ContinuousOn g (sphere (0 : ℂ) 1))
    (hg_inj : InjOn g (sphere (0 : ℂ) 1)) :
    -- g(∂Δ) is a simple closed curve bounding a simply connected domain
    ∃ D : Set ℂ, IsOpen D ∧ IsConnected D ∧ Bornology.IsBounded D ∧
      frontier D = g '' sphere (0 : ℂ) 1 := by
  sorry

/-- The winding number of a simple closed curve around an interior
    point is ±1. -/
lemma winding_number_simple_curve
    {C : Set ℂ} (hC : sorry /- C is a simple closed curve -/)
    {D : Set ℂ} (hD : IsOpen D) (hD_bdy : frontier D = C)
    {w : ℂ} (hw : w ∈ D) :
    -- |winding number| = 1
    True := by
  sorry

/-- g(Δ) ⊆ D: the image of the interior lies in the bounded
    component of ℂ \ g(∂Δ). -/
lemma image_interior_subset_bounded_component
    {g : ℂ → ℂ}
    (hg : ContinuousOn g (closedBall 0 1))
    (hg_inj_bdy : InjOn g (sphere (0 : ℂ) 1))
    {D : Set ℂ} (hD : IsOpen D)
    (hD_bdy : frontier D = g '' sphere (0 : ℂ) 1) :
    g '' ball (0 : ℂ) 1 ⊆ D := by
  sorry

end
```

## 3. Mathlib cross-references

**Types/structures:**
- `DifferentiableOn ℂ g S` — analyticity on a region
- `InjOn g S` — injectivity on a set
- `BijOn g S T` — bijection from S to T
- `Metric.sphere`, `Metric.ball`, `Metric.closedBall` — circle, disk, closed disk
- `frontier D` — boundary of D
- `IsConnected`, `Bornology.IsBounded` — topological properties of the image domain

**Key lemmas:**
- `Complex.circleIntegral_sub_inv_of_mem_ball` — Cauchy integral formula / argument principle core
- `IsPreconnected.image` — continuous image of connected set is connected (for g(Δ̄) connected)
- `IsCompact.image` — compact image of compact set (for g(Δ̄) compact, hence bounded)
- `IsCompact.isBounded` — compact ⟹ bounded
- `DifferentiableOn.continuousOn` — analytic ⟹ continuous
- `ContinuousOn.image_subset` — image containment under continuous maps
- `isCompact_closedBall` — closed disk is compact

**Tactic hints:**
- `have h_cont := hg.continuousOn` — extract continuity from analyticity
- `have h_conn := IsPreconnected.image ... h_cont` — g(Δ̄) is connected
- `have h_bdd := (isCompact_closedBall ..).image h_cont |>.isBounded` — g(Δ̄) bounded
- `obtain ⟨D, hD_open, hD_conn, hD_bdd, hD_bdy⟩ := jordan_curve_image ...` — Jordan CT
- `have := preimage_count_eq_winding ...` — argument principle for counting
- `exact?` — finding the argument principle or Jordan curve lemma
- `by_contra h; push_neg at h` — for injectivity by contradiction
- `linarith` — for winding number = 1 arithmetic