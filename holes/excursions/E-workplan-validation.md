# E-workplan-validation: validate the model-made workplan history

Excursion of [M-repl-circus](../missions/M-repl-circus.md), tagged onto its
DOCUMENT step. Joe, 2026-11-13, on migrating the :7094 workplan store into
futon1b: "Let's keep them, but include in M-repl-circus that we'll want to pass
to a workplan validation stage later, it should be easy, e.g., a small
E-workplan-validation tagged onto the DOCUMENT step for M-repl-circus".

**Status:** OPEN — IDENTIFY. Opened by claude-13, 2026-11-13. Nothing built.

## What is being validated

The workplan migration (plan: codex-12, 2026-11-13, kept privately under
`~/storage/workplan-migration-2026-11-13/PLAN.md`) carries the model-made part
of the :7094 history into futon1b as **unverified** records, never promoted
to accepted:

- 416 `serves` links (item → benefit line) with 418 reasons from three reader
  methods;
- 438 historical item readings (116 user-story readings, 322 "pending"
  narratives);
- 400 lexicon readings, surveys and corrections;
- 593 referents (model-proposed relations between lexicon words).

Joe's acts (3 introductions, word rules, the E-R14 closure acceptance) and
claude-13's two referent rejections are human verdicts and are not in scope.

## The stage

A pass that gives each model-made record a verdict, recorded as an Arxana
assessment on the record (accept, reject, or still candidate) with actor,
policy and reason, so the history stays and its standing changes. Likely
shape: a reviewer per record family, with Joe deciding the policy for
`serves` links and readings and spot-checking a sample; referents and lexicon
surveys judged against the lexicon as it now stands.

Questions for IDENTIFY: which families matter for "what's ready and worth
doing" (serves and readings feed Giraffe and the War Machine; referents feed
the lexicon); whether a reading superseded by a newer Giraffe reading needs a
verdict at all; and what sample size makes Joe's spot-check meaningful.

## Relation to M-repl-circus

The migration itself is closure item 3 (load the :7094 records of record,
then stop the JVM). This validation does not block that item or the mission's
VERIFY; it is part of DOCUMENT: before the mission closes, its write-up says
which of the carried-over records have been validated and which remain
unverified history.
