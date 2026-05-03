# LeanDojo Handoff: a02J02

problem_id: a02J02
dojo_state: dojo_ready
generated_at: 2026-04-06T20:27:54.044953686Z

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
1. `rpow_sub_le` — `/home/joe/code/apm-lean/lean-proofs/a02J02/Main.lean:42`
   Line with `sorry`: 44
   Local context:
```lean
-- rpow version still sorry
lemma rpow_sub_le {p : ℝ} (hp : 1 ≤ p) (a b : ℝ) :
    |a - b| ^ p ≤ 2 ^ (p - 1) * (|a| ^ p + |b| ^ p) := by
  sorry
```
2. `radon_riesz` — `/home/joe/code/apm-lean/lean-proofs/a02J02/Main.lean:47`
   Line with `sorry`: 73
   Local context:
```lean
-- Brezis-Lieb, tendsto_Lp_of_tendsto_ae_of_tendsto_norm — none found).
  -- The closest: tendsto_Lp_of_tendsto_ae requires UnifIntegrable + UnifTight
  -- as hypotheses, which is a DIFFERENT theorem (Vitali convergence).
  sorry
```

## Current Blocker Summary
Planning-only / blocked.

Evidence:
- After rebuilding (`lake build ApmCanaries`), the target file `lean-proofs/a02J02/Main.lean` still contains two `sorry`s. The first, `rpow_sub_le`, is the real-exponent version of the inequality `|a-b|^p ≤ C (|a|^p + |b|^p)`; only the natural-exponent variant is currently proven in the file, and Mathlib lacks the corresponding `Real.rpow` inequality needed to finish it. The second, `radon_riesz`, is the actual Radon–Riesz/Brezis–Lieb convergence theorem; the inline comment explains it would require a Fatou/Brezis–Lieb argument plus an `L^p`-Radon–Riesz API that Mathlib doesn’t yet provide.
- Because these two ingredients are missing, the proof cannot be completed in this pass. The blocker has been logged in `/home/joe/code/futon3c/data/apm-codex-cleanup-log.edn` (see the new `{:problem-id "a02J02" ...}` entry), and the manifest has been regenerated via `futon3c/dev/build_apm_problem_manifest.sh`.

## Supporting Files
- Target main: /home/joe/code/apm-lean/lean-proofs/a02J02/Main.lean
- Informal outline: /home/joe/code/futon3c/data/apm-informal-proofs/apm-a02J02.md
- TeX source: /home/joe/code/storage/apm/a02J02.tex

## Suggested Next Step
LeanDojo can work directly on the named local holes above.
