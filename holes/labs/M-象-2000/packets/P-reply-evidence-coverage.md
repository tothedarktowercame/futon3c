# Packet: coverage of agent-reply evidence against turn records (discovery, read-only)

From claude-16 (owner, M-象-2000), 2026-10-11. Mode: work. Discovery only: no
store writes, no changes to records, no changes to src/.

## Why

`holes/labs/M-象-2000/LAB-2026-10-11-false-open-ports-discovery.md` (74ac5997):
analysed operator turn records have no `reply_text` for 10-09 to 10-10 (0 of
210) and for everything before 10-04, so ports were computed with no reply
acts and almost every operator fragment stays open. The agent's reply does
exist on futon1b :7073 as agent-chat/turn evidence (role "assistant",
`unified-text`, `segment-final`, `turn-id` like `claude-15-turn-21`), chained
by `in-reply-to` from the operator turn's `emacs-…` evidence (example:
`emacs-05223cff34a5a5da070ac4540ac0143d`). Before anyone backfills, find out
how well that evidence maps onto the records.

## Goal

For every analysed operator record in `~/.emacs-graph/session-turn-analysis`
created 2026-10-09 to 2026-10-11, and for a sample of 100 from 2026-10-01 to
2026-10-03, say whether exactly one agent-reply evidence entry on :7073 is the
reply to that turn, and by which join:

1. the record's `evidence_id` → its operator evidence → the assistant entry
   whose `in-reply-to` is that id (or the next entry in the chain);
2. failing that, same `session_id` and the first assistant chat-turn after the
   record's `created_at` and before the next operator record in that session;
3. none found.

Report one table per join (count), plus: how many records have an
`evidence_id` at all; how many assistant entries are split into segments
(`segment-index` > 0) and whether `unified-text` on the final segment carries
the whole reply; any record where joins 1 and 2 disagree.

## Files

- Records: `~/.emacs-graph/session-turn-analysis/turn-*.json` (one dot only;
  skip `.analysis.json`, `.draft.json`, `.candidates.json`).
- Store: GET `http://127.0.0.1:7073/api/alpha/evidence/<id>` with
  `accept: application/edn`. Find the query route for "evidence by subject or
  in-reply-to" in `~/code/futon1b` (read its HTTP routes) rather than guessing;
  if there is none, say so and use the chain walk only.
- Reader of record: `src/futon3c/xiang/turn_store.clj`; the reply arrives via
  `attach-happened!` in `src/futon3c/xiang/turn_service.clj` (for context only).

## Acceptance

- A note `holes/labs/M-象-2000/LAB-2026-10-11-reply-evidence-coverage.md` with
  the tables, the join rules as implemented, three worked examples (one per
  join outcome) with record id and evidence ids, and the date/time of the read.
- The script that produced it, under `holes/labs/M-象-2000/scripts/`, Python 3,
  stdlib only, run with `python3 -I`. No writes to any store, record, or src/.
- Commit only those two files (shared tree: `git add <paths>`, never `-a`).
- Bell claude-16 back with a summary and the commit sha.

## Not in scope

Backfilling `reply_text`, recomputing ports, or changing how brackets link.
Those are the next packets, decided on your table.
