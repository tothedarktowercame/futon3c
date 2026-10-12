# Packet: the resume brief (one mission's open state, in the first prompt), classical

From claude-16 (owner, M-象-2000), 2026-10-12. Mode: work. Python 3 stdlib
(`python3 -I`) plus one Emacs command. Read first: minus40
`docs/SPEC-open-items.md` §0 (Joe's acceptance sense, verbatim) and HOLE 24.

## Why

Joe (2026-10-11): "it would be great if jumping into an agent chat felt like
continuing a familiar conversation, even if the seeds of that conversation
came from months ago." Nothing yet puts the stored state in front of a reader
at the moment it matters. The brief does, from the record fields that went
live on 2026-10-11, with no model.

## Deliverable 1: `holes/labs/M-象-2000/brief/resume_brief.py`

`resume_brief.py --mission M-象-2000 [--seat claude-16] [--since ISO] [--dir DIR] [--out FILE]`

Conversation = records in `~/.emacs-graph/session-turn-analysis/turn-*.json`
with `mission_id` equal to `--mission` (field being added by
`P-mission-id-on-record.md`; until records carry it, fall back to `--seats
a,b,c` given explicitly and say in the output header that the join is by seat,
not mission). Across all sessions. Print, as markdown, in this order, each
section with its count in the heading:

1. **Seed**: the mission file's `**Operator-voice anchor` paragraphs
   (`futon3c/holes/missions/<mission>.md`), newest first, max 3, verbatim.
2. **Owed to the operator** (open ports authored by agents): from the latest
   ported record of each session, `ports.still_open` entries whose act author
   is not "operator": act kind, first 120 chars of text, date, session short
   id, age in days.
3. **Asked by the operator, no answer linked**: `still_open` entries authored
   by "operator", same columns. Head this section exactly "no answer linked",
   never "open" (claude-15, 2026-10-12: asks answered but never linked must
   not read as nags).
4. **In force**: ids in `standings.in_force` of the latest ported records,
   resolved to kind + text via the record's `acts`; constraints first.
5. **Adopted, not applied**: `standings.adopted_not_applied`, same.
6. **Obligations**: `obligations` rows, debtor → creditor, source text.
7. **Recently closed** (last 7 days): `ports.closed_this_turn` entries with
   their `inferred` witness rule where present (so a reader can see why).
8. **Facts**: write the line "facts: not read (no minus40 fact read in this
   version)". Typed absence; do not omit the section.

Dedupe by act id across records (an act listed on two records counts once;
overlapping-turn edge noted in the mission). Sort each section newest first.

**Record the brief as shown** (measurement groundwork; Joe, 2026-10-12: "we
don't have a way to measure what works and what doesn't yet"): write
`~/.emacs-graph/session-turn-analysis/briefs/brief-<mission>-<seat>-<ISO>.json`
with `{mission, seat, at, sections: {name: [act ids]}, record_ids}` so later
turns can be joined to what was shown. No metric is computed here.

## Deliverable 2: Emacs command, `futon3c/emacs/xiang-brief.el`

`M-x xiang-resume-brief`: in a `*claude-repl:<seat>*` or `*codex-repl:<seat>*`
buffer, reads the seat from the buffer name, asks for the mission (default:
the roster's mission id for that seat via `GET /api/alpha/agents`), runs
`resume_brief.py --mission M --seat S` and inserts the output at point. Autoload
cookie; no key binding; one ERT test on the buffer-name parse.

## Tests

`brief/test_resume_brief.py` with inline fixtures: two sessions on one
mission, one agent port open, one operator ask without a link, one standing,
one closed port with an inferred witness, one act duplicated on two records
(counted once); the "no answer linked" heading; the briefs JSON written.

Commit scripts, test, el file; touched files only; never `-a`. Bell claude-16
back with the sha, the test output, and the brief for M-象-2000 run with
`--seats claude-16,claude-17,claude-14` (paste the first 40 lines).
