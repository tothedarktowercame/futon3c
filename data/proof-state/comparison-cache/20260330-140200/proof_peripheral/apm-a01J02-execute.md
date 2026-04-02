Stage 1 — THE CLEAN PROOF
--------------------------------

*(a)* The Lebesgue σ-algebra on `ℝ` is the completion of the Borel σ-algebra with respect to Lebesgue measure. Hence for every Lebesgue measurable set `A` there exist Borel sets `B₁ ⊆ B₂` such that `B₁ ⊆ A ⊆ B₂` and `m(B₂ \ B₁) = 0`. Take `B = B₁ ∪ (B₂ ∩ A)`; it is Borel and differs from `A` only inside the null set `B₂ \ B₁`, so `m(A \triangle B) = 0`.

*(b)* Approximate `f` by a sequence of simple functions `s_k = ∑ α_{k,i} χ_{E_{k,i}}` that converges pointwise a.e. to `f`, with each `E_{k,i}` Lebesgue measurable. By part (a) pick Borel sets `B_{k,i}` such that `m(E_{k,i} \triangle B_{k,i}) = 0`. Let `t_k = ∑ α_{k,i} χ_{B_{k,i}}`; each `t_k` is Borel measurable and `t_k = s_k` almost everywhere. Hence `t_k → f` a.e., so defining `g(x) = limsup_k t_k(x)` yields a Borel-measurable function that equals `f` outside the null set where convergence fails.

Stage 2 — LEMMA DEPENDENCY GRAPH
--------------------------------

1. **Completion lemma** (Mathlib: `Measure.Completion.indicator` / `measure_eq_iff`)  
   Type: `∀ A, MeasurableSet A → ∃ B ∈ 𝔅, μ(A Δ B) = 0`. Critical for part (a).
2. **Simple approximation** (Mathlib: `ae_measurable.approx_simple_function`)  
   Gives `s_k → f` a.e. with measurable `E_{k,i}`. Depends on measurability of `f`.
3. **Replacing measurable sets by Borel reps** (custom instantiation using lemma 1).  
   Converts each `E_{k,i}` into `B_{k,i}` with `μ(E_{k,i} Δ B_{k,i}) = 0`.
4. **Limit of Borel simple functions** (Mathlib: closure of `Borel` under pointwise limits).  
   Shows `g = limsup t_k` is Borel and matches `f` a.e. Critical to close part (b).

Stage 3 — LEAN FORMALIZATION
----------------------------
- Added `apm-lean/lean-proofs/a01J02/Main.lean` with two theorems: `exists_borel_ae_eq` (using `NullMeasurableSet.toMeasurable_ae_eq` plus `ae_eq_set` to show symmetric-difference measure zero) and `exists_borel_function_on_Icc` (using `NullMeasurable.aemeasurable` and the `mk` version of an `AEMeasurable` function).  
- `lake build` now succeeds with the usual pre-existing warnings from other targets, so the new file type-checks without `sorry`s.

Stage 4 — FORMAL-TO-INFORMAL REVISION
-------------------------------------
- The Lean proof shows that the heavy lifting in part (a) is just invoking `toMeasurable`: once you cite `NullMeasurableSet.toMeasurable_ae_eq`, the symmetric-difference estimate is immediate via `ae_eq_set`. Emphasize that the completion property is the key—there’s no constructive approximation needed.  
- For part (b), the only real work is recognizing that “Lebesgue measurable” means `NullMeasurable`, so you can immediately call `hf.aemeasurable.mk`. The pitfall is worrying about explicitly modifying `f`; Lean demonstrates that the abstract `mk` operator already gives the desired Borel version.

Stage 6 — INTEGRATE
-------------------
- **Connections**  
  - Completion arguments (Borel reps + null sets) are the same ones that power Vitali’s convergence theorem, Dunford–Pettis compactness, and de la Vallée Poussin criteria for uniformly integrable families.  
  - `AEMeasurable.mk` is the standard tool in probability when replacing a random variable by a Borel version before taking expectations or conditional expectations.  
  - Translating a.e. equalities into symmetric-difference measure-zero statements via `ae_eq_set` reappears whenever you compare σ-algebras or prove uniqueness of Radon–Nikodym derivatives.
- **EXAM-DAY FIELD KIT**  
  1. `NullMeasurableSet.toMeasurable_ae_eq` + `ae_eq_set` ⇒ `m((A\B) ∪ (B\A)) = 0`.  
  2. Simple-function approximation: every null-measurable function is an a.e. limit of simples with measurable supports.  
  3. `AEMeasurable.mk`: produces a Borel measurable version of a function that agrees a.e. with the original.  
  4. Union-of-null sets lemma: if `μ(E)=μ(F)=0`, then `μ(E ∪ F)=0` (used for the symmetric difference).  
  5. Restriction trick: Lebesgue measure on `[0,1]` is `volume.restrict (Icc 0 1)`, so all theorems hold verbatim on that domain.
- **ArSE Questions**  
  1. *Why is this hard?* Because it hinges on remembering that Lebesgue measurability is the completion of the Borel σ-algebra, so you must explicitly invoke completion machinery.  
  2. *What is the key insight?* Use the provided completion APIs (`toMeasurable`, `AEMeasurable.mk`) rather than constructing approximations by hand.  
  3. *Why does step N work?* `toMeasurable μ A =ᵐ A` directly translates into the two null-difference statements via `ae_eq_set`, exactly matching the problem’s requirement.  
  4. *What connects to this?* The same completion ideas underlie Vitali convergence, Dunford–Pettis, de la Vallée Poussin, and probabilistic arguments about modifying random variables on null sets.  
  5. *Where is intuition wrong?* It feels like you must manually approximate sets/functions by nicer ones, but the surprise is that Mathlib’s completion lemmas already do that work automatically once you invoke them.
