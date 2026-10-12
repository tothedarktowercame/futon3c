# SHARD-NOTE — batch 3 wave 3, shard 7 (zai-2, 2026-10-12)

`python3 -I ../../../moves/validate.py *.moves.json`: exit 0, **41 files
valid, void=0** (quotes substring-verified at build; the builder replicated
the validator's authority order: reply_acts → acts → fragments, with
open_ports + in_force joined as answer/scope targets).

## Counts
- 238 acts labelled, 0 unlabelled; 154 answers: **92 closes / 62 keeps**.
- Scoped: 19 opens, 127 within, **10 closes** (first shard with real closes:
  one-word approvals closing prior decisions).
- Targets: **0 open_ports ids, 53 in_force ids** used as answer/within/closes
  targets — the standings carry the cross-turn structure here, not the ports.
- 8 acts quote whole texts of 4-7 chars ("yes", "Yes", "(setup)", "(next)",
  "(tracking)", "(limits)", "(go?)...", "(the test)" partially) — under the
  8-char minimum but the full text; flagged, not unlabelled.

## Invented labels (counts, reasons)
- `reframe/other` x1 (turn-YCVI7M r-1): redirects the criterion from
  coverage to STOP-rate; no vocabulary label fits.
- `approve/other` x3: agreement that also does dialogue work beyond plain
  (converts a hunch into a test; refocuses the limit criterion).
- Everything else is vocabulary.json (report/built, report/design,
  report/analysis, report/correction, report-problem/self-correction,
  clarify/assertion, qualify/scope, retract/correction...).

## Most-cited openers (answer/within/closes targets)
1. claude-17-turn-377-f-s2-0 "run it for an hour and check quality" — 12
2. claude-17-turn-421-f-s1-0 "🈸:Yes" — 8
3. claude-14-turn-2-f-s2-0 "progress by morning" — 6
4. claude-17-turn-402-f-s1-0 "🈸:yes 1" — 4
5. claude-17-turn-499-f-s1-0 "Informally: yes please" — 3

## Oddities in the inputs
- No mission_id on any record (wave 3 predates mission stamping), as
  announced.
- Same text under two kinds recurs (turn-VCSjvG f-s1-0 report/verify).
- One record's fragment carries its own id in `within` (turn-xJ4TAP
  f-s1-0); harmless but odd.
- Several one-word operator approvals are full decisions ("yes 1", "Yes",
  "move now") — the closes rest on in_force/previous context, which the
  kernel's standings make checkable.
- `:acts` 120-char truncation cost one quote each in turn-lgdyI6 (noted per
  act).

## Time
About 4.5 h of seat time.
