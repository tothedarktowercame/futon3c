# Move grammar mining (Zai campaign, 2026-10-11 → 2026-10-13 02:04Z)

Joe (2026-10-11): 64% of the monthly Zai budget is unspent and resets
2026-10-13 02:04Z. Use it to mine "the grammar of moves made by both myself
and agents, beyond the core intents", so that later inference can be fast,
classical or Kimi-backed, and scope-based, as futon6's math scopes are.

## What the application needs from it (why this is not a survey)

1. **Answer incidences for multi-opener turns** (SPEC-open-items HOLE 19). The
   kernel closes a port only through an explicit link or the sole-port rule;
   112 of 575 openers are reachable that way. A labelled paragraph ↔ opener
   incidence set over real turns is both the test set for any joint-coverage
   rule (codex-68's certificate) and the training set for a cheap linker.
2. **Moves beyond kinds.** The kind table has 20-odd kinds; the same kind
   covers "status check" and "commission", "approve" and "approve with
   amendment". A move vocabulary with counts tells which distinctions the
   adjacency table should make.
3. **Scopes.** Constraints, "let's focus on X", mission/excursion naming,
   "back to Y": acts that open a region in which later moves are read. The
   futon6 analogy: a quantifier scope governs what a later step may refer to.
   A scope structure over sessions is what a scope-based linker would use.

## Discipline

- Offline mining over settled records, Joe's proposal; not a live reader.
- Every label carries a verbatim quote from the text it labels; a label whose
  quote is not a substring (whitespace-normalised) is void (as judge.py).
- Output is a sidecar per record, never a change to the record:
  `turn-<id>.json.moves.json` (pilot: under `data/moves-pilot/`, not the live dir).
- Measures fixed before scaling: precision of incidences against the labelled
  rows (CHECKPOINT-2026-10-11-false-open-ports, 24 rows; claude-16's census
  rows); cost per record from `scripts/zai-usage` before/after the pilot.
- Pilot of 20 records on one seat first; scale to five seats only on the
  pilot's numbers.

## Sidecar schema (`moves.schema.md` beside this file)
