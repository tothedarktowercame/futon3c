# SHARD-NOTE — move-grammar batch 3 shard 3 (zai-3, 2026-10-12)

69 sidecars, `validate.py` exit 0, **void 0** on all 69 (179 acts).

## Counts

- 179 acts: 154 operator fragments, 25 reply acts (5 records have marked
  replies: fbasTo, klsV1g, uaXTGA, yQrOe6, s2Efa8, i8R6ir, iNViGU, lO8uk2 —
  8 actually); 0 unlabelled.
- Answers: 13 closes / 4 keeps.  Scopes: opens 3, within 21, closes 2.
- open_ports targets used: within 6, answers 2, closes 0 — most `within`
  point at the record's own fragments (nearest governing opener); the
  session-long stale opener lists (see below) limited honest cross-turn
  scope links.
- Missions: futonzero (claude-18) 15 records, G-over-cascades (claude-12) 20,
  landscape (claude-11/16) 28, 象-2000 (claude-13) 3, warrant-limit 1, +2
  mixed (dH7njo claude-18, i8R6ir claude-16).

## Vocabulary

54 distinct moves, all dialogue-act; consolidated vocabulary first
(propose/tentative 11, approve/plain 11, constrain/for-now 11,
explain/answer-question 10, challenge/grievance 8, qualify/caveat 8,
constrain/standing-rule 7, report-problem/observed 7, verify/status-check 7,
ask-action/commission 6).  Invented, with reasons: challenge/grievance (the
G-over-cascades rupture turns: 143, 145, 146, 150, 151, 155 — none of
challenge/other's examples fit), challenge/premise (teO5Ga, pvP9Kx, mVytXT),
challenge/term (xLg1HX "honest gap"), challenge/trust (lYZVem),
redirect/requeue, redirect/change-of-plan, redirect/method, redirect/back-to-plan
(mozTbR), withdraw/stop-work (qNwyRu, eSNh0d), prioritize/focus / sequence /
reorder / trade-off, collect/ask-option (vocabulary seed), verify/status-check,
verify/check-understand, report/status-check ("please report" turns),
report/observation, explain/observation, extend/possibility, defer/until.

## Most-cited openers (across answers/within/closes)

1. claude-11-turn-2-f-s2-0 "unpushed repos are a liability" ×3 (landscape:
   the push/repo-hygiene turns all sit within it).
2. claude-12-turn-4-f-s1-0 "please run it and report back." ×2 (the
   "please report" turns dSsyla/k0AnLN are within it).
3–5. claude-12-turn-44-f-s1-0 (classify excursions), claude-13-turn-6-r-2,
claude-12-turn-15-f-s2-0 ×1 each.

## Input oddities

- Stale opener lists: the kernel holds 10–12 ports open from early turns
  that were clearly settled sessions ago (e.g. claude-18's "my 象 lighter is
  still pink" from 09-27 appears in every futonzero record).  This is the
  false-open problem the mission already knows; I did not link to them.
- Kernel kinds occasionally odd: turn-wQOVV4 f-s2-1 "if we interpret..." is
  split off as clarify though it completes the prioritize fragment; kept
  within the prioritize act.
- Typos kept verbatim ("mulitple", "thatvis", "bske", "misdiins", "yo",
  "wirk", "evifence", "ther parser", "figue out hiw").
- turn-zMtvV3 (M-warrant-limit) is a single fragment with no open ports;
  its record's reply_acts is empty.

## Time

About 2 h: inputs ~50 min (69 records, mostly fragments-only, fast to read),
labelling ~50 min, build/validate/note/commit ~20 min.
