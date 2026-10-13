# SHARD-NOTE — move-grammar batch 3 wave 4, shard 10 (zai-3, 2026-10-12)

60 sidecars, `validate.py` exit 0, **void 0** on all 60 (178 acts).

## Counts

- 178 acts, all operator fragments (this wave is fragments-only: no record
  has marked reply acts), 0 unlabelled; answers 0 (nothing to answer).
- Scopes: opens 4 (rewind-demos, M-象-2000-focus, evidence-landscape-heads,
  build-process-understanding), within 49, closes 2 (both in-record:
  kXMi8w f-s3-0 closes f-s2-0; uChO9h f-s5-0 closes f-s3-0).
- open_ports/in_force targets used: 0 — the `within` links all point at the
  record's own fragments (nearest governing opener).  The kernel's open-port
  lists for these sessions are months-stale (claude-17's turn-2..23 and
  claude-1's turn-1..88 ports held open through turn 100+), so linking to
  them would manufacture scope relations the dialogue does not show.
- Sessions: claude-17 ×23 (stepper/rewind demo + facade post-mortem),
  claude-1 ×30 (PROOF-2 / War Machine build troubles), codex-10 ×5,
  claude-14 ×3.

## Vocabulary

46 distinct dialogue-act moves; consolidated vocabulary first
(report-problem/observed 17, constrain/for-now 13, constrain/standing-rule 7,
disagree/correction 7, report/observation 7, propose/tentative 7,
approve/plain 7, propose/directive 6, explain/reasoning 6,
continue/resume-work 6).  Invented (with reasons): collect/quote-back
(q7JPOG — quoting the agent's words back to locate a misunderstanding),
verify/check-understand and clarify/assertion / clarify/scope / clarify/ask-fact
(vocabulary has check-understand but these record Joe's assertive
"that's not my concept" clarifications), redirect/refocus (uChO9h),
disagree/premise (vUUqAY, yVubZK — challenging a claim, not correcting a
fact), qualify/scope (nOAm5F "I don't care HOW").

## Most-cited openers

None reach citation count >0 (see above): all `within` are in-record.  For
the record, the recurring stale opener sets themselves are the interesting
signal: claude-17-turn-7-f-s1-1/f-s2-0 (the pink 象 lighter, 09-27) and
claude-1-turn-1-f-s1-0 (the session's opening delegate) appear in nearly
every record of their sessions.

## Input oddities

- Massive staleness: open_ports caps at 24 but shows ports from the first
  turns of the session through its end — the false-open problem at its
  clearest.  Several in_force entries have kind None and empty text
  (claude-17-turn-549-f-s2-0 etc.).
- Fragment ids of the codex-10 records embed the record name
  (`codex-10-turn-9@turn-jADdxR-f-s1-0`) — unusual, kept verbatim.
- claude-1-turn-93's f-s3-1 text embeds a quote of PROOF-2a's charter
  sentence; kept as one fragment.
- Typos kept verbatim ("effucuent", "xhecked", "Surel", "exend",
  "evindence", "specifed", "deficiences", "eachclick").
- turn-w8IBDL f-s5-0's text is truncated mid-sentence by the kernel
  ("Now the policy…"); quote taken from what is there.

## Time & budget

About 1.5 h.  No 429s encountered; work finished before the 02:04Z reset.
