# SHARD-NOTE — moves-scale3 shard-8 (batch 3 wave 4, last), 60 records (zai-1, 2026-10-12)

## Counts

60 sidecars, 176 acts (167 operator fragments, 9 reply acts across the 2
records that have any), 6 answers (3 closes / 3 keeps).  Scopes: opens 19,
within 52, closes 0.  open_ports/in_force references: 4, to 4 distinct ids
(this shard's records are mostly pre-stamping fragments-only turns with
fewer cross-turn links resolvable from their own open_ports lists).
0 unlabelled.  `validate.py *.moves.json`: **exit 0, total valid=176,
void=0** across all 60.  No 429s encountered before the commit.

## Move vocabulary

39 distinct labels, 11 one-offs, from moves/vocabulary.json:
report-problem/observed x25, clarify/context x19, propose/directive x15,
clarify/assertion x10, propose/extend x9, delegate/to-agent x7,
approve/plain x7.  Invented one-offs: propose/sequencing, collect/idea,
explain/interpretation, explain/justification, report-problem/suspected,
report-problem/retrospective, withdraw/correction (all dialogue moves;
no topic labels).

## Scope findings

19 opens — the richest wave for standing rules and framings, including:
"formal requirements in Lean" (turn-181), "failures as critical incidents"
(turn-157), "debugger, not thin path" (turn-157), "forward direction only"
(turn-151), "make the work progressive" (turn-136), "run end to end, no
side quests" (turn-150), "supersede with PROOF-2b" (turn-97),
"codex team by component" (turn-159), "coalesce names into core set"
(turn-324), "standing curiosity" (turn-96), "stepper per-REPL only"
(turn-314).  52 within, almost all against same-record governing
fragments.

## Five most-cited openers

Each cited once (this shard's open_ports references are sparse):
claude-17-turn-7-f-s2-0 (pink lighter rule), claude-17-turn-314-f-s2-1
(stepper per-REPL), claude-17-turn-285-f-s1-0 (demo turn),
codex-10-turn-6-f-s6-0 (evaluation strategy delegation), plus the same-
record chains above which dominate.

## Input anomalies

- turn-GbsFoA has two fragments whose entire text is the literal token
  "QUOTE" — upstream quoting artifact; labelled from the token itself
  (kind from fragments, quote = "QUOTE").
- One mission_id IS present (turn-Dinwic, M-landscape-positioning; also
  turn-3mROV9, M-diagramprover) despite the packet saying wave 4 has
  none — these two are later records mixed in; labelled normally.
- Duplicate fragment text under two intents again (turn-KdEYHP).

## Time

~3 h (60 mostly single-digit-fragment records; fast mode via a shared
simple() builder).
