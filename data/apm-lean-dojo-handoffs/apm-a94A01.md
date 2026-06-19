# LeanDojo Handoff: a94A01

problem_id: a94A01
dojo_state: dojo_ready
generated_at: 2026-04-07T14:01:14.579143122Z

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
1. `ae_limit_indicator_eq_indicator` — `/home/joe/code/apm-lean/ApmCanaries/Frames/A94A01/Apm_v2_a94A01_1775002871618/Main.lean:16`
   Line with `sorry`: 23
   Local context:
```lean
(hlim : ∀ᵐ x ∂volume,
      Tendsto (fun n => (Eₙ n).indicator (fun _ => (1 : ℝ)) x) atTop (nhds (f x))) :
    ∃ E : Set ℝ, MeasurableSet E ∧ f =ᵐ[volume] E.indicator (fun _ => (1 : ℝ)) := by
  sorry
```
2. `tendsto_setIntegral_of_indicator_limit` — `/home/joe/code/apm-lean/ApmCanaries/Frames/A94A01/Apm_v2_a94A01_1775002871618/Main.lean:26`
   Line with `sorry`: 36
   Local context:
```lean
{g : ℝ → ℝ} (hg : Integrable g volume) :
    Tendsto (fun n => ∫ x in Eₙ n, g x ∂volume)
      atTop (nhds (∫ x in E, g x ∂volume)) := by
  sorry
```
3. `tendsto_eLpNorm_indicator_sub_iff_tendsto_measure_symmDiff` — `/home/joe/code/apm-lean/ApmCanaries/Frames/A94A01/Apm_v2_a94A01_1775002871618/Main.lean:40`
   Line with `sorry`: 52
   Local context:
```lean
p volume)
        atTop (nhds 0) ↔
      Tendsto (fun n => volume (Eₙ n ∆ E)) atTop (nhds 0) := by
  sorry
```

## Current Blocker Summary
No close-pass summary was available.

## Supporting Files
- Target main: /home/joe/code/apm-lean/ApmCanaries/Frames/A94A01/Apm_v2_a94A01_1775002871618/Main.lean
- Informal outline: /home/joe/code/futon3c/data/apm-informal-proofs/apm-a94A01.md
- TeX source: /home/joe/code/storage/apm/a94A01.tex

## Suggested Next Step
LeanDojo can work directly on the named local holes above.
