# LeanDojo Handoff: a03J04

problem_id: a03J04
dojo_state: dojo_ready
generated_at: 2026-04-06T20:52:18.907834036Z

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
1. `layer_cake_identity` — `/home/joe/code/apm-lean/ApmCanaries/Frames/A03J04/Main.lean:38`
   Line with `sorry`: 45
   Local context:
```lean
classical
  -- TODO: expand `layer_cake_le` to absolute values and link the coercions
  -- between extended-real and real measures.
  sorry
```
2. `weakL1_to_localLp` — `/home/joe/code/apm-lean/ApmCanaries/Frames/A03J04/Main.lean:48`
   Line with `sorry`: 58
   Local context:
```lean
-- Skeleton: split the integral at the threshold `a := M / m(E)`
  -- using `layer_cake_identity`, then control each part by the weak bound.
  -- Remaining work: translate the combinatorics of the formal proof.
  sorry
```
3. `weakL1_integral_bound` — `/home/joe/code/apm-lean/ApmCanaries/Frames/A03J04/Main.lean:121`
   Line with `sorry`: 131
   Local context:
```lean
classical
  -- Skeleton: pair `weakL1_to_localLp` with `weak_L1_split_bound` and
  -- reorganize the algebra arising from the layer-cake splitting.
  sorry
```

## Current Blocker Summary
Summary:
- Edited `ApmCanaries/Frames/A03J04/Main.lean`: introduced `Rn`, the distribution function, weak-`L¹` control data, and the part (a)–(c) theorem statements with localized `sorry`s plus proof skeleton comments; retained the proven helper lemmas (`layer_cake_le`, `weak_L1_split_bound`) for later wiring.
- Build: `lake build ApmCanaries` (succeeds; only existing unrelated warnings).
- File status: `dojo_ready` – exactly 3 explicit local holes remain (`layer_cake_identity`, `weakL1_to_localLp`, `weakL1_integral_bound`), all sitting in the real proof path with clear TODO notes.
- Next handoff: specialize `layer_cake_le` to `|f|` under Lebesgue measure for part (a), then push the weak-`L¹` bound through the layer-cake split (using `weak_L1_split_bound`) to discharge parts (b) and (c).

## Supporting Files
- Target main: /home/joe/code/apm-lean/ApmCanaries/Frames/A03J04/Main.lean
- Informal outline: /home/joe/code/futon3c/data/apm-informal-proofs/apm-a03J04.md
- TeX source: /home/joe/code/storage/apm/a03J04.tex

## Suggested Next Step
LeanDojo can work directly on the named local holes above.
