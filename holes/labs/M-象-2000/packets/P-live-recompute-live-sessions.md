# Packet: reload 059c473b + 89912f2d and recompute ports for live sessions only

From claude-16 (owner, M-象-2000), 2026-10-11. Mode: work. Same reload and
recompute procedure as your earlier notes, with one difference from Joe's
ruling (2026-10-11): recompute **live sessions only**, not history.

## Steps

1. Reload from master through `scripts/proof-eval.sh`: `futon3c.logic.xiang`,
   `futon3c.xiang.turn-acts`, `futon3c.xiang.turn-service`. Receipt: a
   synthetic turn with an `agree` fragment yields an `:approve` act; a
   synthetic ㊬ reply act has `:port? false`.
2. Live sessions = for each agent in `GET http://localhost:7070/api/alpha/agents`,
   its current `session-id` (field `agent/session-id` or `session-id`). Records
   to recompute: every settled record in `~/.emacs-graph/session-turn-analysis`
   whose `session_id` is one of those. List the sessions and record counts in
   the note before writing.
3. Before-state ledger (commit first): per record `still_open` count and the
   count of its `still_open` entries with `-r-` ids of kind `verify`; per
   session tail totals.
4. Recompute in per-session `created_at` order, `{:session-cap 10000}`, one
   call per second. The eight formerly refusing records are only recomputed if
   they fall in a live session; list which did.
5. After-state: same columns; tail totals before/after; agent-verify count at
   tails expected 0; refusals; any record whose open count grew with a line
   each.
6. Note `holes/labs/M-象-2000/LAB-2026-10-11-live-recompute-live-sessions.md`.
   Ledger first; touched files only; never `-a`.

Bell claude-16 back with shas, the live-session list and the totals.
