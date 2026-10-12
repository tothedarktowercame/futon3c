# SHARD-NOTE — move-grammar batch 3, shard 2 (zai-2, 2026-10-12)

`python3 -I ../../../moves/validate.py *.moves.json`: exit 0, **69 files
valid, void=0** (every quote substring-verified at build; the builder
replicated the validator's authority order: reply_acts → acts → fragments,
with open_ports joined as answer/scope targets).

## Counts
- 219 acts labelled, 0 unlabelled; 19 answers (12 closes / 7 keeps).
- Scoped: 17 opens, 125 within, 0 closes; **2 targets were open_ports ids**
  (claude-12-turn-43-f-s1-0 "what is going on with the excursions",
  claude-12-turn-68-f-s1-0 "let's hot load now") — both kept, not closed.
- 59 distinct labels, nearly all from vocabulary.json; batch-3 fragments are
  mostly operator turns, so reply acts are few (24 across 5 records).

## Invented labels (with reasons)
- `agree/other` x1 (claude-11-turn-53-f-s1-1): an explicit concession move
  ("ok, so there is C++ involved after all"); vocabulary has no agree labels.
- `explain/evidence` x1 (claude-12-turn-148-f-s5-0): pointing at the buffer
  as evidence for a claim; closest vocab label is explain/other, but the
  dialogue work is evidential, not background.
- Everything else maps to vocabulary.json (incl. report/status-update,
  report/context, clarify/assertion, qualify/uncertainty).

## Most-cited openers (within/answers)
1. claude-13-turn-16-f-s2-0 ("demand side") — 4 withins
2. claude-18-turn-79-f-s1-0 (teach-maths-to-humans) — 14 withins
3. claude-18-turn-80-f-s1-0 (retrace 2002→2026) — 16 withins
4. claude-18-turn-85-f-s3-0 (what is maths useful for) — 9 withins
5. claude-13-turn-31-f-s1-0 (Alinery+Mastra dive) — 8 withins

## Oddities in the inputs
- `:acts` entries are 120-char truncated and win authority when an id
  appears there, so quotes come from the prefix (noted per act where the key
  sentence fell past the cut: turn-ZT897u f-s3-1, turn-VW7Fev f-s5-1).
- Same text under two kinds (turn-KdQqLM f-s1-0 report + f-s1-1 verify) —
  both labelled; the duplication is the kernel's, not the text's.
- Two acts quote their whole text of 5-6 chars ("right", "(then)", "E.g.",
  "he did —", "(if needed)", "Hm...", "Many.") — under the 8-char quote
  minimum but unavoidable without inventing text; flagged here rather than
  leaving the acts unlabelled.
- Turn-Z4xoYC's fragment mentions an absolute ~/code path; kept verbatim
  (input record, not my output).
- open_ports lists reach the 12-cap on 59/69 records; several clearly-stale
  ports (e.g. "my 象 lighter in emacs is still pink") never close in this
  shard — the kernel's sole-port question, not a labelling gap.

## Time
About 5 h of seat time.
