# LeanDojo Handoff: a02J01

problem_id: a02J01
dojo_state: dojo_ready
generated_at: 2026-04-06T20:27:54.043604803Z

## Status
- `dojo_state`: `dojo_ready`
- `build_command`: `cd /home/joe/code/apm-lean && lake build ApmCanaries`
- `remaining_sorries`: `3`

## Scope
- Work in the existing target file first.
- Do not weaken theorem statements silently.
- If a statement is structurally false, repair it minimally and record the change.
- If the file is too skeletal, stop and send it back for Codex scaffolding.

## Remaining Holes
1. `schur_pointwise` — `/home/joe/code/apm-lean/lean-proofs/a02J01/Main.lean:53`
   Line with `sorry`: 64
   Local context:
```lean
-- Step 3: ∫ ‖k‖ ≤ 1, so the bound is ∫ ‖k‖·u²
  -- Blocker: integral_mul_le_Lp_mul_Lq_of_nonneg needs
  -- HolderConjugate.two_two + MemLp for √|k| and √|k|·|u|
  sorry
```
2. `schur_fubini` — `/home/joe/code/apm-lean/lean-proofs/a02J01/Main.lean:74`
   Line with `sorry`: 86
   Local context:
```lean
--    via integral_mul_left or pulling u(y)² out of inner integral
  -- 3. Bound: ∫_x |k(x,y)| dx ≤ 1 a.e. y (column bound)
  -- 4. integral_mono: ∫_y u(y)² · (∫_x |k|dx) ≤ ∫_y u(y)² · 1
  sorry
```
3. `schur_test` — `/home/joe/code/apm-lean/lean-proofs/a02J01/Main.lean:91`
   Line with `sorry`: 106
   Local context:
```lean
-- Mathlib path for eLpNorm comparison:
  -- eLpNorm_le_eLpNorm_of_ae_bound or rpow_le_rpow monotonicity
  -- after converting eLpNorm² to integrals via eLpNorm_rpow_eq_lintegral
  sorry
```

## Current Blocker Summary
Planning-only / blocked.

Evidence:
- After `lake build ApmCanaries`, inspecting `lean-proofs/a02J01/Main.lean` shows three unresolved holes: `schur_pointwise`, `schur_fubini`, `schur_test`. Each sorry is already annotated with the missing ingredients (see comments immediately above the sorry lines). In particular, `schur_pointwise` needs a Bochner-version of Cauchy–Schwarz/Hölder (`integral_mul_le_Lp_mul_Lq` for the pair of functions `√|k|` and `√|k|·|u|`), while `schur_fubini` needs an integrable-swap lemma for the kernel to justify Fubini on `(Fin d → ℝ) × (Fin d → ℝ)` with the Lebesgue product measure.
- Those APIs are not currently available in Mathlib; there is no ready lemma that turns the row/column bounds into the pointwise and Fubini inequalities required for the Schur test. Consequently the final comparison `‖Tu‖₂ ≤ ‖u‖₂` cannot be formalized with the present library.
- Logged the blocker in `/home/joe/code/futon3c/data/apm-codex-cleanup-log.edn` and rebuilt the manifest (`futon3c/dev/build_apm_problem_manifest.sh`).

## Supporting Files
- Target main: /home/joe/code/apm-lean/lean-proofs/a02J01/Main.lean
- Informal outline: /home/joe/code/futon3c/data/apm-informal-proofs/apm-a02J01.md
- TeX source: /home/joe/code/storage/apm/a02J01.tex

## Suggested Next Step
LeanDojo can work directly on the named local holes above.
