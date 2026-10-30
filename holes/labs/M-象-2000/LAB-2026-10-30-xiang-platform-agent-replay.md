# 象 Platform agent replay

**Date:** 2026-10-30

**Status:** one isolated Sonnet case validated after one repair

## Question

Can 象 retain its file reads, pattern search and existing validation protocol
while each interpretation runs as a fresh Claude Code process billed through
Claude Platform rather than an accumulating subscription session?

## Isolation

`scripts/xiang_platform_agent_replay.py` takes an intact historical 象 brief
from the `象-sonnet` Claude Code JSONL. It copies the request, draft and input
sidecars into a new experiment directory and rewrites every request path in
the prompt. The historical published analysis is absent until after the model
exits. Claude Code runs with `--bare`, `--restricted`,
`--no-session-persistence`, no write/edit tools, and only read/search tools.
The proposed JSON is passed to the existing `session_turn_analysis.py complete`
command against the copied request, so publication cannot touch the live turn
store.

## Runs

The intact historical case was `turn-aUQibF` (three sentences about click
approval and plain-language narrative progress reports).

1. The first run cost $0.094948. The model returned a plausible interpretation
   in the draft's flat-fragment shape. The validator rejected it: `sentences
   must be an array`. The restricted agent had not obtained the template.
2. Merely allowing the read-only template command did not change that result;
   a second run cost $0.089462 and received the same rejection.
3. The harness then supplied the actual output of the existing `template`
   command in the prompt. The model produced the correct sentence-wrapped
   schema, but the validator rejected one display-cue budget: five of nine
   words were marked where the maximum was four.
4. A single stateless repair request received that exact validator diagnostic,
   the rejected analysis and the template. It removed the excess cue and the
   real validator accepted and published the analysis beside the copied
   request. The successful initial-plus-repair run cost **$0.138160**.

The successful result used the same three sentence boundaries and the same
three intents as the historical published analysis: `approve`, `constrain`,
`constrain`. It is not a ten-case quality result; compaction left only one
intact historical prompt in the `象-sonnet` session. It establishes that the
stateless Platform-funded tool route can satisfy the real publication
validator without access to the historical answer.

## Consequence

The production candidate is a fresh Platform-authenticated Claude Code process
per 象 job, with the exact template supplied up front and a bounded repair loop
driven by validator diagnostics. A larger replay cohort must generate briefs
from frozen records rather than depend on retained Claude Code prompts. Live
routing should not change until that cohort measures validation rate, semantic
agreement, pattern citations, repair frequency, latency and cost.
