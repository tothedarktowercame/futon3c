# Packet: move-grammar pilot, 20 records (Zai, one seat)

From claude-16 (owner, M-象-2000), 2026-10-11. Mode: work. Read first, in this
order: `holes/labs/M-象-2000/moves/README.md`, `moves/moves.schema.md`. Work
only under `holes/labs/M-象-2000/data/moves-pilot/`. No changes to records, to
`~/.emacs-graph`, to any store, or to `src/`.

## Task

For each of the 20 inputs in `data/moves-pilot/input/<record>.input.json`
(fields: record, turn_id, session_id, created_at, agent_id, source_text,
fragments [{id intent text}], reply_text, previous_reply_acts), write
`data/moves-pilot/<record>.moves.json` per `moves.schema.md`:

- one entry per operator fragment (act id = the fragment's `id`) and per
  marked reply paragraph (act id `<turn_id>-r-<i>`, i = index among marked
  paragraphs in `reply_text`, 0-based; a paragraph is marked when its first
  character is one of ㊥㊭㊣🈚㊟🈖🈯㊢㊩㊬🈹🈳🈲🈸㊯㊝㊮🈕🈰🈝🈘㊫🈡);
- `move` as `<kind>/<move>`, using the seeds when they fit, inventing when not;
- for reply paragraphs, `answers`: every operator fragment of this record (or an
  act in `previous_reply_acts`) the paragraph bears on, each with a verbatim
  quote **from the paragraph** and an effect closes / keeps / unclear;
- `scope`: opens / closes / within, null when none;
- every `quote` a verbatim span of the act's own text, 8+ characters. A quote
  that is not in the text makes the label void: copy, do not paraphrase.

Then run `python3 -I ../../moves/validate.py *.moves.json` from
`data/moves-pilot/` until it exits 0 with no void labels, and
`python3 -I ../../moves/score.py --moves . --labels ../../moves/checkpoint-labels.json`
(if the harness's scorer takes those arguments; read its `--help`).

## Note

`data/moves-pilot/PILOT-NOTE.md`, under 60 lines: the move vocabulary you used
with counts; which schema fields were ambiguous and how you resolved them;
cases where a paragraph answered several openers or several paragraphs one
opener; scopes you found; the validator and scorer output; how long it took.

Commit the 20 sidecars and the note; touched files only; never `git commit -a`.
Bell claude-16 back with the sha, the validator totals and the scorer line.
