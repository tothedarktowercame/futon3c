# T-car3-phase2-impl — durable turn queue (flag-gated, load-dark)

Parent: `M-agency-hardening.md`. Phase 2 of Car-3, after Phase-1 model was
**ratified-with-amendment** by claude-6 (2026-06-10). Branch base:
`codex/car3-queue` (has the Phase-1 model + design note). Reviewer: claude-6.

## Ratified design (implement this; do not redesign)

Per-agent durable FIFO turn queue: **accept → enqueue → drain**, replacing the
`(locking !lock)`-then-timeout in `make-claude-invoke-fn`. Persist
`{:id :from :to :surface :msg-id :seq :accepted-at}` on each queue entry;
**route replies FROM the persisted entry, never from ambient session/caller
state** (the crossing fix). Reuse `agents/tickle_queue.clj`'s atom-backed
single-authority enqueue/drain discipline + bounded history — NOT its
task-assignment schema. Dedup by `:msg-id` → `:deduped`.

## C3-C1 AMENDMENT (mandatory — Phase-1 model was wrong here)

Phase-1's `check-c1` defined staleness as "seq below the **global max** of all
accepted turns," which flags every non-latest turn stale and contradicts C3-Q1
ordered drain. **Corrected:** staleness = "seq below the **frontier already
drained** at this turn's processing time" — i.e. a genuinely out-of-order / late
/ crossed arrival. Under strict FIFO this only triggers on real anomalies;
normal sequential turns ALL process in order. **Fix the model's `check-c1` AND
its conforming trace** (t1→t2→t3 all `:processed` in accepted order;
`:stale`/`:reconciled` reserved for an explicitly late/crossed arrival) before
implementing, and keep the model green.

## Rollout: FLAG-GATED, LOAD-DARK (hard requirement)

This touches the hottest path in a live system. The impl MUST be gated behind a
flag (e.g. env `FUTON3C_CAR3_QUEUE`, **default OFF**). When OFF, behaviour is
byte-for-byte the current path (lock-then-invoke) — so claude-6 can reload the
new code into the live JVM with ZERO behaviour change, then flip the flag
deliberately later. Do NOT change the default path's behaviour when the flag is
off. No global rewrite that takes effect on load.

## Deliverables

1. `src/futon3c/agency/turn_queue.clj` (or similar): the queue — `accept!`
   (enqueue + assign monotone per-recipient `:seq` + msg-id dedup), `drain!`
   (FIFO, one at a time, invoke via existing registry path), terminal states
   `:processed|:failed|:stale|:deduped|:reconciled`, and a `reply-route` that
   reads the entry's origin surface/caller.
2. Flag-gated wiring at the invoke entry point(s) so that when ON, same-agent
   turns go through the queue; when OFF, the current path is unchanged.
3. Corrected Phase-1 model (`car3_queue_model_test.clj`) + impl tests for the
   queue (ordered drain, no-drop, per-surface routing, msg-id dedup, frontier
   staleness).
4. Note any capture-gaps that block full correctness (caller capture, stable
   msg-id source, `:seq` persistence) — flag, don't fake.

## Gates

clj-kondo clean; check-parens; `clojure -X:test` green (model + impl). The
flag-OFF path must pass the existing transport/registry tests unchanged.

## In-flight constraints (HARD)

- No JVM restart/kill, no Drawbridge reload — claude-6 owns the live reload
  (will load-dark with flag OFF, then flip with Joe's ok).
- Worktree: `git worktree add ../futon3c-car3-impl -b codex/car3-impl codex/car3-queue`.
  Commit there, NO push/merge.

## Done = bell claude-6 back

with: branch + sha, gate results, the flag name + proof the OFF path is
unchanged, changed files, the queue API, and any capture-gaps. claude-6 reviews,
reloads dark, and schedules the flag flip with Joe.
