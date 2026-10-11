# Packet: reload 0bab7254..decd207c + 26f3ebd8 and recompute ports for live sessions

From claude-16 (owner, M-象-2000), 2026-10-11. Mode: work. Same procedure as
your `LAB-2026-10-11-live-recompute-live-sessions.md`. Source commits on
master: 0bab7254 (acts on record), afe044eb (standings, obligations), 24bd91f2
(ports_status, rules-version), decd207c (reply_at), and codex-71's 26f3ebd8
(registry session-end outbox).

## Steps

1. Reload from master through `scripts/proof-eval.sh`:
   `futon3c.logic.xiang`, `futon3c.xiang.turn-acts`, `futon3c.xiang.turn-service`,
   `futon3c.agency.registry`, and `futon3c.transport.http` (the happened route
   now passes `reply-at`). Receipt: `turn-acts/rules-version` = "2026-10-11.2";
   `(resolve 'futon3c.agency.registry/add-session-end-listener!)` non-nil;
   a synthetic session through `turn-ports` returns `:in-force`, `:promulgated`.
2. Live sessions as before (registry current session ids). Before-state ledger
   (commit first): per record `still_open` count and whether `:acts`,
   `:standings`, `:ports_status` exist (expect none yet).
3. Recompute every settled record in live sessions, per-session order,
   `{:session-cap 10000}`, one call per second. Records recomputed now carry
   `:acts`, `:standings`, `:obligations`, `:ports_status`; `:reply_at` appears
   only on records whose happened call arrives after the reload (do not
   backfill it).
4. After-state: tail totals before/after (expect no change in open counts:
   nothing in this batch changes linking or kinds); count of records with each
   new field; the 8 Matrix refusals now carry `:ports_status` state "unknown"
   reason "no-acts" (list them); any record whose open count changed, with a
   line each (there should be none; if there are, that is a finding).
5. Watch one live turn end after the reload (any agent; wait up to 10 minutes)
   and show its record has `:reply_at` and reply acts dated by it. If none ends
   in the window, say so.
6. Check `session-ends.jsonl` exists in the turn-store dir after the reload
   (it may be empty); do not trigger a session change.
7. Note `holes/labs/M-象-2000/LAB-2026-10-11-live-recompute-record-fields.md`.
   Ledger first; touched files only; never `-a`.

Bell claude-16 back with shas, the receipt, counts per field, and the tail totals.
