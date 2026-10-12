# SHARD-NOTE — move-grammar batch 2 (single-opener) shard 3 (zai-3, 2026-10-11)

25 sidecars, `validate.py` exit 0, **void 0** on all 25 (167 acts).

## Counts

- 167 acts (63 operator fragments, 104 reply acts), 0 unlabelled.
- Answers: 62 closes / 50 keeps / 0 unclear (112 incidences).
- Scoped acts: 16 (opens: defaults-not-rulings, diff-inspect-apply,
  giant-cascade-first, one-row-at-a-time, move-grammar-campaign,
  discard-rotten-implementation, familiar-conversation, application-led-build,
  M-象-2000-takeover, compatibility-surface; no explicit closes).
- Vocabulary is dialogue-act only (per the batch-2 instruction): 40 distinct
  moves, dominated by qualify/caveat 17, report/findings 13, report/done 12,
  explain/answer-question 10, explain/reasoning 10.  New beyond batch 1's
  dialogue-act set: propose/order-of-work, propose/tentative, propose/directive
  (seed), report/in-flight, report/dispatched, report-problem/self-audit,
  continue/resume-work, prioritize/refocus, extend/deepen, ask-action/two-decisions,
  qualify/scope, clarify/ask-fact.  No topic-named moves.

## Sole-port-rule observations (the point of this batch)

- 7 records where the single opener is NOT fully closed by any paragraph:
  turn-JesiNP (propose kept open by r-0 caveat and r-3 slipperiness),
  turn-WkDyhJ (r-5 keeps the "which cascade" question open),
  turn-XDzSjm (f-s7-0 continued but stops at VALUE-COMPILATION: keeps),
  turn-n4qtiD (the familiar-conversation goal recorded, gap kept open),
  turn-oUIVIz (delegation done, bellback keeps it owed),
  turn-gjm3LH (semilattice objection answered, corrections in flight).
  These are exactly the rows a sole-port closer would mis-close.
- Multi-fragment single-opener records (opener + non-opening fragments):
  turn-tyLl52 (1 opener + 5 constrains/explains), turn-CBKG8g, turn-XDzSjm —
  answers point only at the opening fragment where that is what is borne on.

## Input oddities

- Same as batch 1: kernel `:acts` texts are 120-char prefixes; quotes for
  kernel-recognised acts are fitted into the prefix (builder unchanged).
- turn-TFlLbx r-0 embeds a second marked paragraph inline (㊥ after ㊣); the
  record's reply_acts treats them as one act — labelled as one, noted here.
- Operator typos kept verbatim: "semilatticr", "donr", "discusdion",
  "mini g" (turn-gjm3LH, turn-TFlLbx).
- turn-WkDyhJ r-1 ("Every link carries its kind...") is a one-line paragraph
  with no bearing on the opener; no answers.

## Time

About 1.5 h: inputs ~40 min, labelling ~35 min (dialogue-act vocabulary made
it faster than batch 1), validation + note + commit ~15 min.
