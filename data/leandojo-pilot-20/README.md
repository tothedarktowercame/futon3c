# LeanDojo Pilot — 20 APM Problems

**For:** Rob (superpod / LeanDojo-v2 integration)
**From:** Joe + Claude (futon3c APM conductor v3 batch)
**Date:** 2026-04-05

## What this is

20 UT Austin prelim exam problems in analysis, each with:
1. Complete informal proof
2. Lean 4 theorem statements (all `sorry`-bodied, real Mathlib types)
3. Mathlib cross-references (specific lemma names, types, tactic hints)

## How to use with LeanDojo

**Option A: Prove the sorry statements.**
Each file contains Lean 4 declarations with `sorry` bodies. Extract
the `lean` code blocks, place them in a Lean project importing Mathlib,
and point LeanDojo's prover at the sorry goals. The Mathlib cross-refs
section tells you which lemmas to retrieve.

**Option B: Use as retrieval cues for fine-tuning.**
The Mathlib cross-references section lists specific API names. Use these
to extract the relevant Mathlib source files for training data. Each
problem maps to a cluster of Mathlib lemmas — this is the "retrieval
surface" for that problem.

**Option C: Prove with DeepSeek-Prover-V2 via LeanDojo-v2.**
```python
from lean_dojo_v2.agent.hf_agent import HFAgent
from lean_dojo_v2.trainer.sft_trainer import SFTTrainer

agent = HFAgent(trainer=SFTTrainer(
    model_name="deepseek-ai/DeepSeek-Prover-V2-7B"))
agent.setup_github_repository(
    url="https://github.com/tothedarktowercame/apm-lean",
    commit="HEAD")
agent.prove()  # targets: the sorry theorems
```

## Problem list

| ID | Mathlib refs | Sorry count | Topic |
|----|-------------|-------------|-------|
| a00J04 | 19 | 7 | Polynomial lemniscate complement components |
| a01A06 | 18 | 6 | Lp/measure theory |
| a03J07 | 17 | 6 | Analysis |
| a95J05 | 15 | 8 | Analysis |
| a94A06 | 15 | 6 | Analysis |
| a01A07 | 15 | 6 | Analysis |
| a92J07 | 14 | 7 | Analysis |
| a92J05 | 14 | 8 | Analysis |
| a95J03 | 13 | 7 | Analysis |
| a95A08 | 13 | 6 | Analysis |
| a93J03 | 13 | 7 | Analysis |
| a92J02 | 13 | 4 | Lp embedding (finite vs discrete measure) |
| a96A01 | 12 | 7 | Analysis |
| a92J03 | 12 | 8 | Analysis |
| a95J07 | 11 | 5 | Analysis |
| a95A07 | 11 | 6 | Analysis |
| a94A10 | 11 | 5 | Analysis |
| a92J06 | 11 | 8 | Analysis |
| a92J04 | 11 | 5 | Analysis |
| a96A07 | 10 | 9 | Analysis |

Total: 20 problems, ~120 sorry targets, 4-19 Mathlib refs per problem.

## Success metric

If LeanDojo closes ≥10 sorry across these 20 problems, that validates
the retrieval-cue approach. If it closes ≥30, the superpod pipeline
is competitive with the Claude+Codex two-agent workflow.

## Source

These files live in `futon3c/data/leandojo-pilot-20/` and were generated
by `futon3c.dev.apm-conductor-v3/batch!` with `make-informal-lean-prompt`.
The full batch is running against all 489 problems.
