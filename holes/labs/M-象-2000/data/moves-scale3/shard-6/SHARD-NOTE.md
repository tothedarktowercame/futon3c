# SHARD-NOTE — moves-scale3 shard-6 (batch 3 wave 3, reply-rich), 42 records (zai-1, 2026-10-12)

## Counts

42 sidecars, 261 acts (75 operator fragments, 186 reply acts), 100 answers
(50 closes / 50 keeps).  Scopes: opens 15, within 63, closes 0.
open_ports/in_force references: 45, to 16 distinct ids.  0 unlabelled.
`validate.py *.moves.json`: **exit 0, total valid=261, void=0** across all 42.

## Move vocabulary

40 distinct labels, 14 one-offs, nearly all from moves/vocabulary.json:
report/evidence x40, report/done x30, approve/plain x23, report/in-flight
x18, qualify/caveat x17, propose/next-step x16, ask-action/one-decision
x12, constrain/standing-rule x10.  Invented one-offs (dialogue moves, no
topics): propose/reframe, clarify/assertion, report-problem/retrospective,
report/observed-output, ask-action/test-script, report/diagnosis
(vocabulary), explain/reflection/interpretation/decline.  Batch-2
no-topic discipline kept throughout.

## Five most-cited openers (open_ports / in_force targets)

1. claude-2-turn-42-f-s1-2 "all of these artifacts are supposed to be
   demonstrating an application of active inference" x9 — the standing
   frame for all paper/code work in that session.
2. claude-2-turn-60-f-s1-1 "keep the implementation simple" x5.
3. claude-2-turn-52-f-s1-1 "design patterns in the paper are meant to
   describe [AI] in easy-to-understand language" x5 — patterns as
   plain-language output.
4. claude-6-turn-9-f-s1-0 "summarise more succinctly" x4.
5. claude-17-turn-380-f-s1-0 (start next hour with improvements) x3.

## Scope findings

15 opens: standing rules (campaign S dropped; one seat per turn; use
latest shared configs; succinct summaries; no bellbacks per turn),
plans (efficiency-first formalisation; step back to fundamentals;
research the theory; behavior-first certification), framings (max-neef
discriminator tree; simple implementation first; wm-click-acts as
authority).  63 within, mostly against an opener of the same record or
an in_force/open_ports id.  0 closes again this shard.

## Input anomalies

- No mission_id in any record (wave 3, pre-stamping sessions), as
  announced.
- Several heading-only reply acts whose content is a table or follows in
  unmarked prose (turn-76NiTi r-1, turn-GC5j2q r-2, turn-HOhCfB r-1):
  labelled low-confidence with notes.
- Truncated ~120-char `acts` beside full `reply_acts` as in batches 1-2;
  quotes chosen inside the shortest text per id.
- Operator typos preserved verbatim (wotking/areement/mulitple/tye).

## Time

~4 h (42 records, all reply-rich; quote verification and in_force target
checks dominated).
