# T-car3-queue-routing — Phase 1 design note

Status: model-only proposal for ratification. No implementation is included in
this phase.

## Proposed mechanism

1. **Durable per-agent turn queue**
   - Introduce an Agency turn queue keyed by recipient agent id, with entries of
     the model shape: `{:id :from :to :surface :msg-id :seq :accepted-at ...}`.
   - Reuse the useful parts of `futon3c.agents.tickle-queue`: atom-backed
     enqueue/drain discipline, bounded completed history, and explicit terminal
     states. Do **not** reuse its task-assignment schema directly: Tickle's queue
     is a work-pool plus idle-bell dispatcher, while Car-3 needs FIFO per-agent
     turn serialization and reply routing. The primitive to copy is the
     single-authority enqueue/drain shape, not the task model.
   - Queue entries terminalize as exactly one of `:processed`, `:failed`,
     `:stale`, `:deduped`, or `:reconciled`. This satisfies no-drop without
     pretending every accepted item must do work.

2. **Ordered drain**
   - Replace lock-then-timeout semantics with a per-agent drain loop: accept
     quickly, enqueue durably, then drain one entry at a time in `:accepted-at` /
     `:seq` order.
   - A busy agent never causes the caller turn to block on the agent lock; the
     queue accepts the turn and returns the queue/job id.

3. **Sequence markers**
   - Assign a monotone per-recipient `:seq` at accept time, stored on the queue
     entry and echoed in bell/reply metadata.
   - Replays with the same `:msg-id` may reuse the original entry/seq and
     terminalize as `:deduped`.
   - Crossed bells are detectable when distinct `:msg-id`s claim the same seq or
     when a reply's seq is older than the recipient's known frontier; stale work
     must terminalize as `:stale` or `:reconciled`, not normal `:processed`.

4. **Per-surface reply routing**
   - Persist origin `:surface` and caller identity on the queue entry. Reply
     delivery reads the queue entry, not ambient session/global caller state.
   - Surface-specific delivery adapters then route by the persisted origin:
     `emacs-repl` back to that REPL request, `bell` back to the caller agent,
     `irc` back to the IRC channel/nick, etc.

5. **Operator-turn idempotency**
   - `:msg-id` is the dedupe key. A second accepted turn with the same msg-id is
     recorded and terminalized as `:deduped`; it must not invoke the agent again.
   - The queue keeps a bounded or durable `msg-id -> terminal entry` index per
     target agent.

## Capture gaps to close in Phase 2

- Some current HTTP/bell paths still do not reliably supply a real caller
  (`http-caller`); reply routing can only be correct when caller identity is
  captured at accept time.
- Operator/repl turns need a stable message id from the surface. If Emacs does
  not provide one, the server must assign one at accept time and surface it back
  to the caller for replay safety.
- Current invoke-job records do not persist a per-recipient sequence marker.
  Phase 2 must add `:seq` to the accepted job/turn record before any invoke.
- Historical session ownership is not a sufficient routing authority; reply
  routing must rely on the queued turn's persisted origin surface and caller.

## Model summary

The model in `test/futon3c/agency/car3_queue_model_test.clj` ratifies:

- C3-Q1 ordered drain: terminal outcomes for an agent follow accepted order.
- C3-Q2 no-drop: every accepted turn has an explicit terminal outcome.
- C3-R1 per-surface reply routing: processed replies return to origin surface.
- C3-D1 idempotency: a msg-id is processed at most once; duplicates dedupe.
- C3-C1 causal reconciliation: seq markers are present, unique for distinct
  turns, and stale/crossed turns are marked stale/reconciled rather than
  delivered as normal work.
