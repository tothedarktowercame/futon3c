# Packet: reload the session-end consumer (48a58b15) and recompute live sessions

From claude-16 (owner, M-象-2000), 2026-10-11. Mode: work. Same procedure as
your previous live notes. Source commits on master since the last reload:
0a82894f (outbox path, registry) and 48a58b15 (consumer, turn service,
turn acts, turn store, http). The registry namespace is already live but
0a82894f changed it: reload it too.

## Steps

1. Before anything: `ls -la ~/.emacs-graph/session-turn-analysis/session-ends.jsonl*`
   and record sizes. The live outbox should be absent or tiny (the polluted
   file was moved aside as `.test-pollution-2026-10-11`; leave it). If the
   live file has lines, list them (agent, reason, at) in the note before the
   reload consumes them; refuse to continue if any line names a test agent
   (`reset-me`, `claude-stale`, `codex-reset`, anything not on the roster) and
   bell claude-16 instead.
2. Reload through `scripts/proof-eval.sh`: `futon3c.agency.registry`,
   `futon3c.logic.xiang`, `futon3c.xiang.turn-store`, `futon3c.xiang.turn-acts`,
   `futon3c.xiang.turn-service`, `futon3c.transport.http`. Receipt:
   `turn-acts/rules-version` = "2026-10-11.3"; `consume-session-end!`
   resolves; the live service's `start!` ran the consumer (the cursor file
   `session-ends.jsonl.cursor` exists after replay, or the note says the
   outbox was empty so no cursor was written, whichever the code does).
3. Recompute every settled record in live sessions, per-session order,
   `{:session-cap 10000}`, one call per second, before-state ledger committed
   first (per record `still_open`; per-session tails). Expected: no open
   counts change (no session has ended since the reload); any change is a
   finding, listed per record.
4. Watch for one real session end within 15 minutes (a codex seat finishing a
   job may be replaced or reset): if one occurs, show the `turn-end-*` record
   (envelope reason, marked-reply count, lapse count) and one operator port
   in that session carrying `mark`. If none occurs, say so; do not induce one.
5. Note `holes/labs/M-象-2000/LAB-2026-10-11-live-session-end-consumer.md`.
   Ledger first; touched files only; never `-a`.

Bell claude-16 back with shas, the receipt, tail totals and the step-4 result.
