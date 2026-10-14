# SHARD-NOTE — moves-scale3 shard-12 (batch 3 wave 5), 81 records (zai-1, 2026-10-14)

## Counts

81 sidecars, 213 acts (all operator fragments; 0 reply acts — none of the
81 records has any, as announced).  0 answers.  Scopes: opens 7,
within 3, closes 0.  0 open_ports/in_force references (this wave's records
are from pre-stamping sessions whose `open_ports` lists are almost entirely
the long-running markdown-overlays / lighter chain from the same session,
which no fragment in this shard acts inside).  0 unlabelled.
`validate.py *.moves.json`: **exit 0, total valid=213, void=0** across all 81.

## Move vocabulary

39 distinct labels, 8 one-offs, from moves/vocabulary.json: approve/plain
x20, clarify/assertion x18, report/context x16, constrain/stand-rule x13,
clarify/context x13, ask-action/commission x12.  Invented one-offs:
verify/assertion, delegate/interview, clarify/analogy, explain/analogy
(distinct from clarify/analogy: explains via analogy rather than clarifies
by one).  No topic labels.

## Scope findings

7 opens: "open M-象-cascade" (mission chartering), "password check as
first classical tool", "scope the BMR alternatives", "build (c) then (b)",
"demo first, held items later", "code, no software" (the standing frame
for the claude-3 positioning session, cited as `within` by the later
"no software" assertion), and the M-象-cascade charter.

## Most-cited openers

All this shard's `within` links are to same-record fragments (3 total);
no cross-record opener was cited.  The long-lived openers in the inputs'
`open_ports` (markdown-overlays, lighter status, upstream PR) govern none
of this wave's fragments, correctly: these are positioning and ops turns,
not tool-development turns.

## Input anomalies

- `in_force` is empty for most records despite the packet's claim that
  "since this wave the in_force rows all carry text (the standings index
  now covers every record)" — the first record sampled had an empty
  list.  Possibly the coverage claim applies to shard 13 only, or to a
  later snapshot.
- One turn_id referenced in my dump pass (claude-3-turn-23) matched no
  input file — it was a misread of the sed window (the record
  turn-X9jG3N with those fragments exists and was labelled normally);
  no sidecar is missing.
- No mission_id in any record, as announced.

## Time

~2 h (81 mostly-1-3-fragment records; wave-5 bulk builder).

## Scope pass (2026-10-14)

Before: 3 within links across 213 acts.  After: **97** within links
(14 opens).  All 32 records in scope-pass.txt now carry at least one
within link (every one of the 32 was patchable: none had "nothing governs
anything" — each had a frame fragment the rest elaborated).  All 97
targets are same-record fragment ids; 0 in_force, 0 open_ports (this
shard's in_force lists are empty and its open_ports chains govern other
sessions' tool work, not these positioning/ops turns).  Validator: exit
0, valid=213, void=0, unchanged quotes and moves verified by the build
sidecar untouched.
