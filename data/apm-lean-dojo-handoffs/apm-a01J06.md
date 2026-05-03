# LeanDojo Handoff: a01J06

problem_id: a01J06
dojo_state: dojo_ready
generated_at: 2026-04-06T21:22:57.157504602Z

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
1. `zeroCountLinear_of_expBound` — `/home/joe/code/apm-lean/lean-proofs/a01J06/Main.lean:295`
   Line with `sorry`: 298
   Local context:
```lean
lemma zeroCountLinear_of_expBound (data : ProblemData) :
    data.zeroCountLinear := by
  -- TODO: Apply Jensen's formula with the `ExpTypeBound` hypothesis.
  sorry
```
2. `weight_summable_of_zeroCountLinear` — `/home/joe/code/apm-lean/lean-proofs/a01J06/Main.lean:301`
   Line with `sorry`: 305
   Local context:
```lean
(data : ProblemData) (h : data.zeroCountLinear) :
    Summable data.weight := by
  -- TODO: Sum the geometric tail using `data.dyadicShell`.
  sorry
```

## Current Blocker Summary
Summary:
- Edited `lean-proofs/a01J06/Main.lean`: kept the existing dyadic-shell framework and added the two missing analytic pillars—`zeroCountLinear_of_expBound` (Jensen-type counting) and `weight_summable_of_zeroCountLinear` (dyadic summability)—along with the final theorem that chains them together.
- Build: `lake build ApmCanaries` (succeeds; only the pre-existing unrelated warnings).
- Status: `dojo_ready` — two explicit local holes remain (`zeroCountLinear_of_expBound` and `weight_summable_of_zeroCountLinear`).

Next handoff:
1. Prove `zeroCountLinear_of_expBound` via Jensen’s formula for entire functions of exponential type, extracting the linear bound on the counting function.
2. Use that linear bound in `weight_summable_of_zeroCountLinear` to carry out the dyadic-shell summation and conclude the weighted zero series converges, closing `apm_a01J06_main`.

## Supporting Files
- Target main: /home/joe/code/apm-lean/lean-proofs/a01J06/Main.lean
- Informal outline: /home/joe/code/futon3c/data/apm-informal-proofs/apm-a01J06.md
- TeX source: /home/joe/code/storage/apm/a01J06.tex

## Suggested Next Step
LeanDojo can work directly on the named local holes above.
