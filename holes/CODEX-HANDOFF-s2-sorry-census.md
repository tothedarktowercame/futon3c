# CODEX-HANDOFF — S2: sorry census + construction-target triage

Mission: `holes/missions/M-codex-sorry-loop.md` S2. Prepared 2026-07-28
by claude-6. **Delivery: Agency bell from claude-6. Bell claude-6 back
with summary + commit SHAs.** Owner reviews after landing.

## Goal

A triage of every `sorry` in apm-lean (348 occurrences / 142 files at
`26be1cb`) that turns the raw count into a ranked work queue of
**construction targets** — the missing-Mathlib-lemma class that the
M-codex-sorry-loop pilots will patch. Lexical + context analysis only
(the hole-count-is-lexical precedent); **no lake builds** — this is a
reading task, not a compiling task.

## Files

`:in` (READ-ONLY): `/home/joe/code/apm-lean` (do not modify, do not
build). Context worth reading first:
`futon3c/holes/labs/M-zai-learning-loop/cohort-2-prereg.md` (the
curriculum lane; the local-Young-inequality target — YoungL2.lean, 2
sorries, unblocks a95J08 + a96A04) and the problem-id conventions
visible in apm-lean's structure.

`:out` (create under `futon3c/holes/labs/M-codex-sorry-loop/`):
- `sorry-census-20260728.edn` — every occurrence: file, line, problem
  id (from path/filename conventions), enclosing declaration name and
  statement (extracted textually), classification, and for
  missing-lemma cases the *statement of the missing dependency* as best
  extractable from surrounding comments/code.
- `construction-targets.md` — the ranked top 5 with a one-paragraph
  work packet each.
- `s2-note.md` (≤40 lines): distribution summary + ranking rationale.

## Classification vocabulary (fixed)

- `:missing-mathlib-lemma` — the proof is blocked on a lemma/API that
  plausibly belongs in (an extension of) Mathlib; the statement is
  extractable. THE pilot work class.
- `:hard-proof-step` — the mathematics is present but a step resists;
  no missing dependency identified.
- `:statement-issue` — the theorem statement itself is doubted/known
  wrong (look for nearby comments).
- `:scaffold` — sorry in commented-out/WIP scaffolding, dead files, or
  intentionally deferred parts.
- `:unclassified` — honest residue; count it, don't force it.

## Ranking criterion (preregistered here)

Rank `:missing-mathlib-lemma` targets by, in order: (1) number of
distinct problems unblocked (a target appearing in several proofs
outranks a one-off); (2) extractability of a clean standalone statement
(a target the pilot can attack as one lemma file); (3) proximity to
existing Mathlib API (a plausible extension, not a research program).
The Young inequality target is expected to rank at/near the top —
if the census DISAGREES, say so and why; the criterion outranks the
expectation.

## Acceptance checklist

- [ ] Census EDN parses; total occurrence count reconciles with
      `grep -rc sorry` (348 at `26be1cb`; state the delta if HEAD moved).
- [ ] Every row classified; classification counts in the note;
      `:unclassified` reported honestly.
- [ ] Top-5 targets each have: the missing statement (Lean-ish sketch
      acceptable), files/problems unblocked, ranking scores per the
      criterion, and a suggested lemma-file location.
- [ ] apm-lean untouched (`git -C /home/joe/code/apm-lean status`
      clean).
- [ ] `git diff --stat` in futon3c shows only this packet's `:out`
      files.
- [ ] Bell claude-6 with summary + commit SHAs.

## Out of scope

Proving anything; lake builds; editing apm-lean; dispatching pilots
(S3 is claude-6's, gated on this census + prereg).
