# LeanDojo Handoff: a01A01

problem_id: a01A01
dojo_state: dojo_ready
generated_at: 2026-04-06T21:18:19.367386420Z

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
1. `unitSpike_nonneg` — `/home/joe/code/apm-lean/ApmCanaries/Frames/A01A01/Apm_v2_a01A01_1774990784258/Main.lean:52`
   Line with `sorry`: 57
   Local context:
```lean
intro x
  -- The indicator is nonnegative and the spike height is positive.
  -- TODO: unwrap the indicator and turn this into a short calculation.
  sorry
```
2. `unitSpike_integral` — `/home/joe/code/apm-lean/ApmCanaries/Frames/A01A01/Apm_v2_a01A01_1774990784258/Main.lean:59`
   Line with `sorry`: 64
   Local context:
```lean
spikeHeight (Nat.unpair k).1 / stageTiles (Nat.unpair k).1 := by
  classical
  -- The integral equals `height × length` because the spike is constant on its tile.
  sorry
```
3. `unitSpike_limsup` — `/home/joe/code/apm-lean/ApmCanaries/Frames/A01A01/Apm_v2_a01A01_1774990784258/Main.lean:66`
   Line with `sorry`: 70
   Local context:
```lean
∀ R > 0, (∃ᶠ k in Filter.atTop, R ≤ unitSpike k x) := by
  classical
  -- For each stage `m`, exactly one spike contains `x`; its height grows like `m²`.
  sorry
```
4. `unitSpike_limsup` — `/home/joe/code/apm-lean/ApmCanaries/Frames/A01A01/Apm_v2_a01A01_1774990784258/Main.lean:66`
   Line with `sorry`: 84
   Local context:
```lean
classical
    -- Each stage contributes `(m+1)^2 / 2^{m+1}`, which tends to zero.
    -- Conclude using the explicit `unitSpike_integral` formula.
    sorry
```
5. `exists_spikes_Rn` — `/home/joe/code/apm-lean/ApmCanaries/Frames/A01A01/Apm_v2_a01A01_1774990784258/Main.lean:94`
   Line with `sorry`: 102
   Local context:
```lean
classical
  -- Pull back `Construction.unitSpike` along the first coordinate and damp it
  -- outside the unit cube; all required estimates mirror the `[0,1]` case.
  sorry
```

## Current Blocker Summary
Summary:
- Edited `ApmCanaries/Frames/A01A01/Apm_v2_a01A01_1774990784258/Main.lean`: defined the unit-interval measure, packaged the desired properties in `SpikeCertificate`, built an explicit dyadic spike sequence (`unitSpike`) with supporting lemmas, and stated the `[0,1]` and `ℝⁿ` existence theorems with localized `sorry`s.
- Build: `lake build ApmCanaries` (succeeds; only the pre-existing unrelated warnings).
- Status: `dojo_ready` — five explicit local holes remain (`unitSpike_nonneg`, `unitSpike_integral`, `unitSpike_limsup`, the limit proof inside `unitIntervalCertificate`, and the ℝⁿ lift `exists_spikes_Rn`).

Next handoff:
1. Prove `unitSpike_nonneg` by unfolding `Set.indicator`; everything is manifestly nonnegative because both the height and indicator values are ≥ 0.
2. Compute `unitSpike_integral`, showing the integral equals `spikeHeight m / stageTiles m`, then use it to finish the tendsto statement in `unitIntervalCertificate`.
3. Establish `unitSpike_limsup` via the subsequence indexed by stages, noting each stage contributes a spike containing any fixed `x`.
4. For `exists_spikes_Rn`, compose `unitSpike` with the first coordinate of `ℝⁿ` (and trim outside the unit cube) to transfer the previously proved properties.

## Supporting Files
- Target main: /home/joe/code/apm-lean/ApmCanaries/Frames/A01A01/Apm_v2_a01A01_1774990784258/Main.lean
- Informal outline: /home/joe/code/futon3c/data/apm-informal-proofs/apm-a01A01.md
- TeX source: /home/joe/code/storage/apm/a01A01.tex

## Suggested Next Step
LeanDojo can work directly on the named local holes above.
