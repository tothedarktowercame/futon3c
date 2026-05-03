# LeanDojo Handoff: a93J07

problem_id: a93J07
dojo_state: dojo_ready
generated_at: 2026-04-07T14:01:14.577435394Z

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
1. `hurwitz_eventually_nonvanishing_on_sphere` — `/home/joe/code/apm-lean/ApmCanaries/Frames/A93J07/Apm_v2_a93J07_1775570229422/Main.lean:17`
   Line with `sorry`: 26
   Local context:
```lean
(hclosed : Metric.closedBall z₀ r ⊆ Ω)
    (hboundary : ∀ z ∈ Metric.sphere z₀ r, f z ≠ 0) :
    ∀ᶠ n in atTop, ∀ z ∈ Metric.sphere z₀ r, F n z ≠ 0 := by
  sorry
```
2. `hurwitz_zero_persistence` — `/home/joe/code/apm-lean/ApmCanaries/Frames/A93J07/Apm_v2_a93J07_1775570229422/Main.lean:30`
   Line with `sorry`: 39
   Local context:
```lean
{m : ℕ} (hm : analyticOrderAt f z₀ = m + 1) :
    ∃ r > 0, Metric.closedBall z₀ r ⊆ Ω ∧
      ∀ᶠ n in atTop, ∃ z ∈ Metric.ball z₀ r, F n z = 0 := by
  sorry
```
3. `locallyUniformLimit_injective_or_constant` — `/home/joe/code/apm-lean/ApmCanaries/Frames/A93J07/Apm_v2_a93J07_1775570229422/Main.lean:43`
   Line with `sorry`: 50
   Local context:
```lean
(hconv : TendstoLocallyUniformlyOn F f atTop Ω)
    (hinj : ∀ n, InjOn (F n) Ω) :
    InjOn f Ω ∨ ∃ c : ℂ, ∀ z ∈ Ω, f z = c := by
  sorry
```

## Current Blocker Summary
No close-pass summary was available.

## Supporting Files
- Target main: /home/joe/code/apm-lean/ApmCanaries/Frames/A93J07/Apm_v2_a93J07_1775570229422/Main.lean
- Informal outline: /home/joe/code/futon3c/data/apm-informal-proofs/apm-a93J07.md
- TeX source: /home/joe/code/storage/apm/a93J07.tex

## Suggested Next Step
LeanDojo can work directly on the named local holes above.
