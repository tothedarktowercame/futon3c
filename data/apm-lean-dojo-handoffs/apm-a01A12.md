# LeanDojo Handoff: a01A12

problem_id: a01A12
dojo_state: deep_mathlib_blocked
generated_at: 2026-04-06T20:27:54.033420999Z

## Status
- `dojo_state`: `deep_mathlib_blocked`
- `build_command`: `cd /home/joe/code/apm-lean && lake build ApmCanaries`
- `remaining_sorries`: `3`

## Scope
- Work in the existing target file first.
- Do not weaken theorem statements silently.
- If a statement is structurally false, repair it minimally and record the change.
- If the file is too skeletal, stop and send it back for Codex scaffolding.

## Remaining Holes
1. `step2_image` — `/home/joe/code/apm-lean/lean-proofs/a01A12/Main.lean:52`
   Line with `sorry`: 53
   Local context:
```lean
-- Step 2: squaring maps RHP onto slit plane
-- HtDP: w ∈ RHP ⟹ arg(w) ∈ (-π/2,π/2) ⟹ arg(w²) ∈ (-π,π) ⟹ w² ∉ (-∞,0]
lemma step2_image : MapsTo (· ^ 2 : ℂ → ℂ) {w | 0 < w.re} {ζ : ℂ | (0:ℝ) < ζ.re ∨ ζ.im ≠ 0} := by
  sorry -- argument doubling
```
2. `step4_to_wedge` — `/home/joe/code/apm-lean/lean-proofs/a01A12/Main.lean:57`
   Line with `sorry`: 59
   Local context:
```lean
-- HtDP: arg(η) ∈ (-π,π) ⟹ arg(η^{1/4}) ∈ (-π/4,π/4)
lemma step4_to_wedge (η : ℂ) (hη : η ≠ 0 ∧ η.arg ≠ Real.pi) :
    |((η : ℂ) ^ (1/4 : ℂ)).arg| < Real.pi / 4 := by
  sorry -- arg of cpow = arg/4
```
3. `conformalMap_maps_H_to_A` — `/home/joe/code/apm-lean/lean-proofs/a01A12/Main.lean:62`
   Line with `sorry`: 64
   Local context:
```lean
-- The composed map sends H into A
theorem conformalMap_maps_H_to_A (z : ℂ) (hz : 0 < z.im) :
    conformalMap z ∈ wedgeSlit := by
  sorry -- compose steps 1-4, verify slit maps to slit
```

## Current Blocker Summary
Blocked: even after filling in the simple lemmas (`step1_maps` and `step2_inj_RHP` in `lean-proofs/a01A12/Main.lean`), the remaining sorries (`step2_image`, `step4_to_wedge`, and the final mapping/classification statements) still require a branch-cut/`Complex.arg` API for `cpow` on slit domains. Mathlib lacks those wedge/slit lemmas, so the conformal-map construction cannot proceed further in this frame.

## Supporting Files
- Target main: /home/joe/code/apm-lean/lean-proofs/a01A12/Main.lean
- Informal outline: /home/joe/code/futon3c/data/apm-informal-proofs/apm-a01A12.md
- TeX source: /home/joe/code/storage/apm/a01A12.tex

## Suggested Next Step
Do not hand this to LeanDojo first; it appears to need new library/theory development.
