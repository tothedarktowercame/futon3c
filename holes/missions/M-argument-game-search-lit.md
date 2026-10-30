# Mission: M-argument-game-search-lit — literature catch-up for E-argument-game-search
**Status:** IDENTIFY → MAP (deep-research run 2026-10-30, Opus researchers; report in
`holes/excursions/deep-research-argument-game-search.md`)
**Parent:** `holes/excursions/E-argument-game-search.md` (IDEA, not critical path; Joe's go
is required before any *building*). This mission is the survey the excursion itself asks
for: "prior literature … should be read before building anything *(not yet surveyed)*".
**Date:** 2026-10-30 (claude, cloud session)

## IDENTIFY — the question

The excursion asks whether AlphaZero-style search (self-play + MCTS + learned policy/value)
can help build or judge argument structures, cascades, or s-expression evaluations, with a
language model as move proposer inside a game engine that holds the moves and the win
condition. Six claims in it are made **from memory, unchecked**. This mission replaces
memory with sources, and finds the prior art that would make steps 1–3 either redundant,
cheap, or doomed.

Scope rule: *read, do not build.* Output is a report and a corrected excursion, nothing on
the critical path.

## MAP — six threads, one researcher each

| # | Thread | What it must settle for the excursion |
|---|--------|---------------------------------------|
| T1 | Learned search for theorem proving and program synthesis | Which systems pair a policy/value net with tree search over proofs or programs (HTPS, AlphaProof, DeepSeek-Prover, DreamCoder, AlphaDev, LeanDojo lines); what reward they use; what hardware; what failed. Is "Lean is a poor first domain" borne out? |
| T2 | Argument games and formal argumentation | Dialogue games for Dung semantics (grounded/preferred games, proof procedures), AIF/AIF² ontology, Pease et al. 2017, game-theoretic argumentation, ICCMA solvers, 2023–26 LLM+argumentation work. Does a "pure Dung game" with a computed win already exist? |
| T3 | Expertise, chunking, and search saved | Verify de Groot, Chase & Simon, Simon's chunk count, Gobet templates, Klein RPD; AlphaZero policy-alone strength; Expert Iteration's dual-process framing; "nodes expanded" as the measure. Is "experts search less, not deeper" what the sources say? |
| T4 | Active inference policy search | Verify hierarchy / deep temporal models, sophisticated inference (Friston 2021), habit prior E (Friston 2016), Fountas 2020 MCTS + habitual net, Millidge 2020 bootstrapped G, amortised inference; planning-as-inference links to MCTS. What is genuinely open about *where training data comes from*? |
| T5 | LLM as move proposer, engine as verifier | Tree-of-Thoughts, RAP, LLM-MCTS, FunSearch, AlphaGeometry, self-play with verifiers, generator–verifier gap, Goodhart/reward hacking against learned judges (Rob's caution), CPU-only AlphaZero on small games (OpenSpiel). |
| T6 | Arithmetic expression games and neural execution limits | Countdown/Stream of Search, symbolic regression by MCTS, equation games; Learning to Execute (2014) and successors, neural algorithmic reasoning, length generalisation. Does step 1 (arithmetic s-expression game) already exist as a benchmark, and does "learn to recognise, not execute" have support? |

## Questions the report must answer (ranked)

1. For each memory-claim in the excursion (§"Experts think in patterns", §"How would a living
   thing compute G?"): confirmed, corrected, or unsupported, with citation.
2. Does the arithmetic s-expression game (step 1) already exist as a benchmark with published
   baselines? If so, name it and the numbers.
3. Does a computed-win Dung game (step 2) already exist? Name the proof procedure.
4. What does the literature say about search on a *judged* reward (Rob's caution)?
5. What is the cheapest published CPU-only AlphaZero result on a small game, and how many
   cores/hours did it take?
6. Which of the three "cheap first steps" is made redundant, cheaper, or inadvisable by what
   was found, and which has no precedent (the genuinely open part).

## DERIVE / ARGUE / VERIFY — deferred

Deferred until the report is read. The only derivation this mission licenses is editing
the excursion's "from memory" paragraphs to cite sources.

## Method

`anthropic-skills:deep-research`: six parallel Opus researchers (one per thread), one
report writer, run inline within one turn (`E-deep-research-hardening.md`: background
`Workflow` fan-outs do not survive turns; inline does). Notes are kept beside the report so
claims can be traced to their extraction.
