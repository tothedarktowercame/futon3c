# REPORT — judge_within: blind Haiku verdict on combined-rule within misses (2026-10-15)

One script (`moves/judge_within.py`), one run, no production change.  The
judge saw each miss blind: the act's words and kind, its turn's operator
fragments (and reply acts for reply acts), then two candidates labelled A
and B in per-act seeded random order — never told which was the rule's and
which the labeller's.  When the rule predicted nothing, "none of the above"
took its place.  Prompt fixed (printed by the script; fixed text in
`PROMPT`), one Haiku call per miss, 20 s timeout, continue past failures.

## Headline numbers

**777 misses** (the 10-14 report's baseline was ~467; the shard-12 scope
pass and wave-5 edits changed the gold since), 333 fragment / 444 reply.
777 calls, **0 errors**, 1 unparseable, median 5.3 s per call.

| judge sides with | overall | fragment | reply |
|---|---|---|---|
| **the labeller** | **322** (41%) | 213 (64%) | 109 (25%) |
| **the rule** | **307** (39%) | 54 (16%) | 253 (76%) |
| **neither** | **147** (19%) | 66 (20%) | 81 (18%) |

By confidence: high 291 (labeller 173, rule 105, neither 13); medium 436
(rule 180, labeller 131, neither 125); low 49 (evenly spread).

## The ten instructive misses, judged

1. claude-4-130-s1 **neither** — the acceptance of a 2-hour trace does not
   answer the DiagramProver proposal.  2. claude-13-10-r0 **labeller** —
   the sort of example companies is the kernel proposal's work.  3.
   claude-2-103-s6 **labeller** — "a pushout." completes s5's "conceptual
   blend" explanation.  4. claude-13-27-r1 **labeller** — the reply quotes
   the report-problem's words and answers it.  5. claude-2-103-r5
   **labeller** — "what they would change in the 1 October draft" answers
   the draft-not-final qualify.  6. claude-2-109-s13 **labeller** — the
   hedge refers to the War Machine example's iterations.  7.
   codex-18-36-r0 **labeller** — the reply names codex-19's problems.
   8. claude-18-80-s4 **neither** — a reason for the learning story,
   answers neither candidate.  9. claude-4-130-r0 **rule** — the reply
   tags "(the 2-hour limit)", the acceptance act.  10. claude-4-236-r1
   **neither** — "(your emulate rule)" names a parent neither candidate is.

## Reading

The judge's "why" is about the words: of a random 40, **35 cite the acts'
content** ("quotes s5's wording", "names the codex-19 problems"), 2 cite
both, 2 pure position ("the nearest").  It reasons like the labeller, not
like the rule.  Where it sides with the rule (mostly reply acts), the
labeller's edge is often a same-turn fragment that shares vocabulary but
is not what the paragraph answers — plausibly wrong labels.  Where it
sides with the labeller (mostly fragment acts, 64%), the position rule
lacks exactly what the words show: a fragment that answers, completes or
qualifies a specific earlier fragment, not the nearest framing one.

Could the judge stand in for the labeller on a fresh turn?  Not yet: it
judges between two given parents, and on a fresh turn the label-free rules
(R1, R4, R5) supply only positional candidates, which the judge rightly
discounts.  But as an adjudicator of disagreements between a positional
rule and a word-sensitive labeller it is already doing the job: its
fragment verdicts side with the labeller 4:1, its reply verdicts with the
rule 2.3:1, and its sentences are checkable against the texts above.

Run record: `data/judge-within/run-20261015T172111Z.jsonl` (777 lines,
A/B mapping, raw verdicts, seconds).  Self-test:
`python3 -I moves/judge_within.py --self-test`.
