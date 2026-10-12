# Packet: move-grammar labelling, batch 3 — futon missions, scopes across turns

From claude-16 (owner, M-象-2000), 2026-10-12. Mode: work. Read first:
`holes/labs/M-象-2000/moves/README.md`, `moves/moves.schema.md`, and the
batch-2 note for your seat number (`data/moves-scale2/shard-<N>/SHARD-NOTE.md`).
Work only under `holes/labs/M-象-2000/data/moves-scale3/shard-<N>/` (N in your
requisition line). No changes to records, `~/.emacs-graph`, any store, `src/`,
or other shards.

## What this batch is for

Batch 1 and 2 covered one session at a time. This batch covers the operator
turns of five futon missions (M-landscape-positioning, M-futonzero-prelim-
practice, M-G-over-cascades, M-象-2000, M-warrant-limit; 208 records), and
most of these records have **no marked reply paragraphs** (24 of 208 do), so
the work is mostly Joe's fragments: their moves, and above all their
**scopes** — the constraints, focus statements and plans that govern how later
turns are read. That relation is what the kernel lacks.

## What changed in the inputs

- `mission_id`: the mission the turn was taken in.
- `open_ports`: up to 12 openers from earlier turns of the same session that
  the kernel still holds open at this record (act id, kind, text, since),
  oldest first. **These ids are valid targets** for `answers[].act` and for
  `scope.within` / `scope.closes`, alongside the record's own fragments and
  `previous_reply_acts`. Use them when a fragment or paragraph answers,
  continues, or sits inside something opened earlier. The validator accepts
  them (moves/validate.py, this commit).
- Records with an empty `reply_acts` have no reply acts to label: label the
  fragments only, and do not invent reply acts (an unmarked reply paragraph
  is not an act).

## Labels

Use the consolidated vocabulary first: `moves/vocabulary.json` (`kinds` →
labels with counts and examples). Invent a label only when none fits, and
say why in the note. **A move names how the act works in the dialogue, never
its subject** (`propose/tentative`, `propose/directive`, `constrain/standing-
rule` are moves; `propose/aif-integration` is a topic and is wrong).

## Scopes, precisely

For every act, fill `scope`:
- `opens`: a short name when the act starts a region later turns are read
  in — a standing constraint ("no second agent reads turns"), a focus ("the
  hook first"), a named mission/excursion, a plan ("P1 then P2").
- `within`: the act id (a fragment of this record, a `previous_reply_acts`
  id, or an `open_ports` id) of the opener that governs how this act should
  be read, when one does. Prefer the nearest governing opener.
- `closes`: the opener this act ends ("back to the plan", "drop that").
Null when none. A scope quote is the act's own `quote`; nothing extra.

## Task

For every `shard-<N>/input/<record>.input.json`, write
`shard-<N>/<record>.moves.json` per the schema: one entry per fragment in
`fragments` and per act in `reply_acts` (if any); `answers` for reply acts as
in batch 2, now also against `open_ports`; every `quote` a verbatim span of
the act's own text (copy, do not paraphrase; 8+ characters).

Run from `data/moves-scale3/shard-<N>/`:
`python3 -I ../../../moves/validate.py *.moves.json` until exit 0, void 0.

## Note

`shard-<N>/SHARD-NOTE.md`, under 50 lines: counts (acts, answers by effect,
scoped acts by opens/within/closes, how many `within`/`answers` targets were
`open_ports` ids, unlabelled with reasons); invented labels with counts and
reasons; the five most-cited openers (act id, text) in your shard; anything
in the inputs that looked wrong; time taken.

Commit your shard's sidecars and note only; touched files only; never
`git commit -a`. Bell claude-16 back with the sha and the validator line.
