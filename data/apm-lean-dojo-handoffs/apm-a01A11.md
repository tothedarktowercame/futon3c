# LeanDojo Handoff: a01A11

problem_id: a01A11
dojo_state: dojo_ready
generated_at: 2026-04-06T20:27:54.032729294Z

## Status
- `dojo_state`: `dojo_ready`
- `build_command`: `cd /home/joe/code/apm-lean && lake build ApmCanaries`
- `remaining_sorries`: `5`

## Scope
- Work in the existing target file first.
- Do not weaken theorem statements silently.
- If a statement is structurally false, repair it minimally and record the change.
- If the file is too skeletal, stop and send it back for Codex scaffolding.

## Remaining Holes
1. `unknown` — `/home/joe/code/apm-lean/lean-proofs/a01A11/Main.lean:13`
   Line with `sorry`: 13
   Local context:
```lean
/-! # APM a01A11: Cauchy transform of compact K ⊂ ℝ
    g(z) = ∫_K 1/(t-z) dt

    Skeleton with localized sorry instances.
```
2. `cauchyTransform_differentiableAt` — `/home/joe/code/apm-lean/lean-proofs/a01A11/Main.lean:35`
   Line with `sorry`: 37
   Local context:
```lean
-- Composition: (infDist > 0) → (uniform bound 2/d²) → (DCT derivative)
theorem cauchyTransform_differentiableAt (z : ℂ) (hz : z ∉ ofReal '' K) :
    DifferentiableAt ℂ (cauchyTransform K) z := by
  sorry -- DCT for complex parameter integral. Blocker: wiring
```
3. `cauchyTransform_tendsto_neg_vol` — `/home/joe/code/apm-lean/lean-proofs/a01A11/Main.lean:43`
   Line with `sorry`: 46
   Local context:
```lean
theorem cauchyTransform_tendsto_neg_vol :
    Tendsto (fun z : ℂ => z * cauchyTransform K z)
      (cocompact ℂ) (nhds (↑(-(volume K).toReal) : ℂ)) := by
  sorry -- DCT for cocompact limit. Same blocker as (a):
```
4. `cauchyTransform_not_entire` — `/home/joe/code/apm-lean/lean-proofs/a01A11/Main.lean:52`
   Line with `sorry`: 62
   Local context:
```lean
-- Step 1: G is constant (Liouville)
  -- Need: IsBounded (range G). From: G→0 at ∞ + continuous on compact.
  have hG_bounded : Bornology.IsBounded (range G) := by
    sorry -- G(z) → 0 at ∞ (from h_decay + hG_eq) ⟹
```
5. `cauchyTransform_not_entire` — `/home/joe/code/apm-lean/lean-proofs/a01A11/Main.lean:52`
   Line with `sorry`: 73
   Local context:
```lean
intro z
    -- G is constant, so G z = G w for all w. Pick w → ∞.
    -- G w → 0, so G z = 0.
    sorry -- from hG_const + h_decay via Filter.Tendsto + const
```

## Current Blocker Summary
Summary:
- Target file: `lean-proofs/a01A11/Main.lean` (contains the Cauchy-transform setup plus three unresolved goals).
- Build command run: `lake build ApmCanaries` (succeeds, only unrelated lint warnings).
- Status: file still has three `sorry`s (parts (a), (c), and the Liouville-boundedness lemma); 0 lemmas were closed in this pass.
- Blocker: completing the proof requires differentiating under the integral sign and applying dominated convergence for complex parameter integrals (to show analyticity and the cocompact limit), as well as a bounded-range lemma to invoke Liouville. Mathlib currently lacks the parameter-integral holomorphic/dominated-convergence API needed to discharge these sorries, so the proof cannot be finished in this closing pass.

## Supporting Files
- Target main: /home/joe/code/apm-lean/lean-proofs/a01A11/Main.lean
- Informal outline: /home/joe/code/futon3c/data/apm-informal-proofs/apm-a01A11.md
- TeX source: /home/joe/code/storage/apm/a01A11.tex

## Suggested Next Step
LeanDojo can work directly on the named local holes above.
