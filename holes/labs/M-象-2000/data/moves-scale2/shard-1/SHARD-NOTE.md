# SHARD-NOTE — moves-scale2 shard-1 (batch 2, single-opener), 68 records (zai-1, 2026-10-11)

## Counts

68 sidecars, 428 acts (268 operator-fragment, 160 reply-paragraph), 217
answers (104 closes / 113 keeps), 76 scoped acts (opens/within chains; no
explicit closes case), 0 unlabelled.  `validate.py *.moves.json`:
**exit 0, total valid=428, void=0** across all 68.

## Move vocabulary

71 distinct labels, 25 one-offs.  Seeds dominate: ask-action/one-decision
x31, report/done x30, qualify/caveat x24, report/diagnosis x23,
approve/plain x19, verify/checked-live x14.  Propose labels follow the
batch-2 rule (how the paragraph acts, not its subject): propose/next-step
x22, propose/tentative x5, propose/directive x2, propose/alternative,
propose/extend x5, propose/redirect-scope x2; no topic labels.  Four
early propose/acceptance-plan labels were renamed propose/next-step in
self-review.  Other reused families: report/evidence x24, report/design
x21, explain/mechanism x16, report/dispatched x5, report/in-flight x4,
report/gap x5, retract/correction x2, disagree/correction x2.

## Sole-port rule test data (single-opener incidences)

These records have exactly one opening fragment, and the incidences show
the sole-port rule's failure modes: in 14 records a paragraph answers 2
fragments (one opener + one non-opening fragment), e.g. turn-07igHH r-0
(opener + explain), turn-LhnBVw r-0 (opener + explore), turn-XwITxi r-0
(opener + two more).  In many records the opener is NEVER answered by any
reply paragraph (the whole reply is process talk: e.g. turn-ArDfbO,
turn-D1BUn9, turn-86oO5M -- the ask closes only via a report/done act),
and in others the closer is a late paragraph, not the first (turn-70TgiY:
11 paragraphs before the cross-tab result closes the opener).  These are
the cases a sole-port linker must not assume.

## Input anomalies

Same as batch 1 (truncated ~120-char `acts` beside full `reply_acts`;
quotes chosen inside the shortest text per id).  One record (turn-9RyYna)
has an answer target in `previous_reply_acts` from a turn two back
(claude-17-turn-505-r-5) that the validator rejects as an answer target
-- it is outside both the current fragments and `previous_reply_acts`,
so the answer was relabelled to a current-fragment target.

## Time

~7 h of seat time (68 records at ~5.5 min each, including the truncated-
text quote constraint and the batch-2 label-discipline self-review).
