# SHARD-NOTE — batch 3 wave 2, shard 4 / M-diagramprover (zai-4, 2026-10-12)

57 sidecars, 191 labelled acts (115 fragments + 76 reply acts), 78 answers
(21 closes / 57 keeps / 0 unclear), scoped acts: 16 opens, 172 within,
0 closes. validate.py exit 0, void 0 over all 57 files
(`python3 -I ../../../moves/validate.py *.moves.json`).

## Port-target usage
- `within` pointing at an `open_ports` id: 54 of 172 (the rest point at the
  record's own fragments/reply acts, mostly the reply's own gist r-0).
- `answers[].act` pointing at an `open_ports` id: 6 of 78 (wave-2 answers are
  mostly to the same record's single opener; the codex-18/codex-19 sessions
  answer their long-lived standing asks).

## Five most-cited openers (within-targets)
1. claude-4-turn-3-f-s1-0 x35 — "how about we start to build a DiagramProver
   checker adapted to Lean itself" — the mission-defining scope; most
   claude-4 fragments sit inside it.
2. claude-4-turn-42-f-s1-0 x8 — "Let's make our own fork of mfuton-share"
   (upstream/PR/fork work reads inside this).
3. claude-3-turn-1-f-s1-0 x3 — the COMPILES-INCORRECT rows proposal.
4. claude-3-turn-4-f-s1-0 x1 — resource-use constraint.
5. codex-18-turn-22-f-s1-0 x1 — the standing Lisp-port ask.

## Invented labels
None invented beyond vocabulary.json + one reuse: `report/status` used for
inline status paragraphs (in vocabulary), and `report/progress` avoided in
favour of `report/status`/`report/in-flight` (both in vocabulary). Everything
else is consolidated-vocabulary. Dialogue-act discipline kept (no topics).

## Unlabelled: 0. Low-confidence acts: section-header-only reply acts
(labelled from the header span, noted per-act) in 9 records, plus three
sub-8-char connective fragments labelled at the floor ("E.g.", "Got it.",
"Right...") with notes.

## Inputs that looked wrong
1. Same acts-text truncation as batches 1-2 (~200 chars + "…"); quotes kept
   inside the stored span, clipped where necessary (noted per-act).
2. Disambiguated turn ids appear in some records: "claude-4-turn-24@turn-
   JXBPpb-...", "claude-4-turn-342@turn-Ptei5r-...", "claude-4-turn-309@turn-
   QxunUb-..." — same turn number, different record; handled as-is.
3. turn-CcUXUi (claude-4-turn-2) has only its own fragment as an open port —
   the DiagramProver opener (turn 3) does not exist yet, so its acts carry
   no `within`; correct, but easy to get wrong from habit.
4. Section-header-only reply_acts (r-0 = "(dispatched; ...)", "(open jobs)",
   etc.) recur; bodies live in unmarked paragraphs.
5. 21 of 57 records have reply_acts (packet said 49 of 114 across the wave;
   this shard's ratio is higher).

## Time
About 5 h of seat time: harness port to open_ports targets 0.5 h, 57 records
~4 h in six batches, truncation retries throughout, note and commit 0.5 h.
