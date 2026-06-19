# LeanDojo Handoff: a01J03

problem_id: a01J03
dojo_state: dojo_ready
generated_at: 2026-04-06T21:21:19.084480879Z

## Status
- `dojo_state`: `dojo_ready`
- `build_command`: `cd /home/joe/code/apm-lean && lake build ApmCanaries`
- `remaining_sorries`: `2`

## Scope
- Work in the existing target file first.
- Do not weaken theorem statements silently.
- If a statement is structurally false, repair it minimally and record the change.
- If the file is too skeletal, stop and send it back for Codex scaffolding.

## Remaining Holes
1. `deriv_sum_of_monotone` — `/home/joe/code/apm-lean/lean-proofs/a01J03/Main.lean:22`
   Line with `sorry`: 30
   Local context:
```lean
∀ᵐ x ∂volume, HasDerivAt f (derivativeSum fns x) x := by
  -- Skeleton: use that each monotone function is a.e. differentiable (Lebesgue's theorem)
  -- and exchange derivative with summation via dominated convergence of difference quotients.
  sorry
```
2. `deriv_sum_of_monotone_ae` — `/home/joe/code/apm-lean/lean-proofs/a01J03/Main.lean:33`
   Line with `sorry`: 42
   Local context:
```lean
classical
  have h := deriv_sum_of_monotone f fns hmono hsummable hf
  -- TODO: turn the `HasDerivAt` witness delivered by `h` into a statement about `deriv f`.
  sorry
```

## Current Blocker Summary
Summary:
- Edited `lean-proofs/a01J03/Main.lean`: replaced the placeholder `True` lemma with concrete definitions (`pointwiseSum`, `derivativeSum`) and two precise theorems capturing the almost everywhere derivative equality, each ending in localized `sorry`s with explanatory sketches.
- Build: `lake build ApmCanaries` (succeeds; only the existing unrelated warnings).
- Status: `dojo_ready` — two explicit local holes remain (the `HasDerivAt`-a.e. lemma and its corollary translating to `deriv f = ∑ deriv fₙ`).

Next handoff:
1. Prove `deriv_sum_of_monotone` by combining the a.e. differentiability of monotone functions with dominated convergence on difference quotients; this needs precise control of the tails of `fns`.
2. Deduce `deriv_sum_of_monotone_ae` from the first lemma by extracting actual derivatives on the full-measure set where `HasDerivAt` holds.

## Supporting Files
- Target main: /home/joe/code/apm-lean/lean-proofs/a01J03/Main.lean
- Informal outline: /home/joe/code/futon3c/data/apm-informal-proofs/apm-a01J03.md
- TeX source: /home/joe/code/storage/apm/a01J03.tex

## Suggested Next Step
LeanDojo can work directly on the named local holes above.
