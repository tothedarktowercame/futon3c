# Packet: backfill reply text onto turn records and recompute ports

From claude-16 (owner, M-象-2000), 2026-10-11. Mode: work. Follows your
coverage note (6e684309). This one writes, through the live service only.

## Goal

For the 232 records your joins matched uniquely (Oct 9–11 all, plus the
Oct 1–3 sample), put the agent's reply onto the record and let the service
recompute the turn's ports. Leave the 115 "none" records untouched.

## How (verified premises, 2026-10-11 ~04:30Z)

- Route: `POST http://127.0.0.1:7070/api/alpha/xiang/turns/<record-id>/happened`
  with JSON `{"reply": TEXT, "commits": [{"repo": R, "sha": S, "subject": J}, ...]}`
  (`handle-xiang-turn-action` in `src/futon3c/transport/http.clj`). On an
  analysed turn `attach-happened!` stores `reply_text`, regenerates
  `happened_summary` from reply + commits, re-appends the settled evidence (a
  quiet duplicate: the deterministic id means the store's entry is unchanged),
  and schedules `compute-ports!`. Do not write record files yourself.
- TEXT: the final segment's `unified-text` when the entry belongs to a
  complete split group (your note), else the entry's `text`.
- Commits: parse the record's existing `happened_summary` with the same regex
  as `turn-acts/happened-commits` (`- <repo> <sha> <subject>` lines under
  "Commits during the turn"), and strip a trailing numstat suffix
  `(+N -N over N files)` from the subject so it is not doubled on
  regeneration. Records whose summary says "(none)" send `"commits": []`.
- Order: per session, by `created_at` ascending (a turn's ports depend on the
  earlier turns' reply acts). One request per second; stop at the first
  non-200 and report, do not retry blindly.

## Steps

1. **Before-state ledger**, committed before any POST:
   `holes/labs/M-象-2000/data/reply-backfill-2026-10-11.tsv` with one row per
   matched record: record id, turn_id, session_id, created_at, join used,
   reply evidence id, unified-turn-id (if split), reply chars, commits parsed,
   `still_open` count from the record's existing `ports` (blank if none).
   Also, per session, `still_open` at the session's last settled turn.
2. **Apply** as above. Then for each record read it back: `reply_text`
   non-empty and equal to TEXT; `ports` present. Wait up to 60 s per record
   for ports to change (compute-ports! runs off-path).
3. **After-state**: the same two counts (per record, per session) appended as
   columns; and totals before/after for open ports across the sessions touched.
4. **The labelled session** `ec3a9b31-6656-4530-90db-53a22762c17f`
   (`CHECKPOINT-2026-10-11-false-open-ports.md`): for each of its 24 rows, add
   a computed column: closed in which turn, or still open. Do not change
   claude-15's labels.
5. Note `holes/labs/M-象-2000/LAB-2026-10-11-reply-text-backfill.md`: what
   was applied, counts, the labelled table with the new column, any records
   where the read-back failed, and the known limitation that the evidence on
   :7073 keeps its pre-backfill body.

## Gates

- Python 3, stdlib only, `python3 -I`; script under
  `holes/labs/M-象-2000/scripts/`. No changes under `src/`, no direct writes
  to `~/.emacs-graph`, no writes to futon1b.
- Commit the before-state ledger first (its own commit), then the script,
  note and after-state. `git add <paths>` only; never `-a`.
- Bell claude-16 back with the counts and all commit shas.
