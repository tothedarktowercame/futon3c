# T-car3-queue-routing — durable queue, ordered drain, per-surface reply routing

Parent: `M-agency-hardening.md` (W2 execution guarantees, invariant #5/#6).
Field-validated by `M-agency-hardening-datapoints.md` (5 live symptoms).
Dispatched by claude-6 (in-flight mechanic), 2026-06-10. Reviewer: claude-6.

## Problem (field-observed, not theory)

Same-agent turns serialize on a per-agent `(locking !lock)` in
`make-claude-invoke-fn` (dev.clj); a long-held lock + the relay/job timeout
**drops** the waiting turn ("you missed my bell, resend"). Replies interleave
because all callers resume one session and output goes to that one conversation,
not back to the surface that sent the turn. The 5 datapoints:
1/3 missed/dropped bells; 2 operator-turn replay (no idempotency); 3 bell/REPL
interleaving (no causal order); 4 stale re-flag; 5 crossed bells needing a manual
3rd reconciling bell.

## Invariant set (the spec)

- **C3-Q1 Ordered drain** — turns to a single agent are processed in accepted order.
- **C3-Q2 No-drop** — a turn to a busy agent is durably enqueued and eventually
  processed OR explicitly marked failed/stale; never silently dropped by a
  lock-timeout.
- **C3-R1 Per-surface reply routing** — a turn's reply is delivered to the surface
  that originated it (REPL turn → that REPL; bell → its caller).
- **C3-D1 Operator-turn idempotency** — a turn with a given message-id is processed
  at most once (replays/duplicates deduped).
- **C3-C1 Causal self-reconciliation** — each bell carries a causal/sequence marker
  so crossed (concurrent) and stale (superseded) bells are *detectable* and
  reconcile without a manual extra bell.

## PHASE 1 — LOGIC MODEL FIRST, THEN STOP FOR RATIFICATION

Per the futonic discipline AND because this touches the hottest path in a live
system, do the **logic-model only** first, then **bell claude-6 and STOP** — do
NOT write impl until claude-6 ratifies the model.

- `test/futon3c/agency/car3_queue_model_test.clj`: a core.logic/pldb (or
  data-driven) model over an abstract trace of turns
  `{:id :from :to :surface :msg-id :seq :accepted-at}` plus a queue/drain
  process. Assert: a conforming trace satisfies C3-Q1/Q2/R1/D1/C1; and a
  per-invariant adversarial trace (out-of-order, dropped, duplicate msg-id,
  crossed, stale) is caught/handled by exactly the relevant invariant.
- A short **design note** (in the ticket dir or as a comment block) proposing the
  CONCRETE mechanism: where the durable per-agent queue lives (reuse
  `agents/tickle_queue.clj` enqueue/drain primitives if they fit — I-4), how seq
  markers are assigned and compared, where reply-routing reads the origin
  surface, and what supplies the message-id. Flag any capture-gaps (fields not
  yet persisted) rather than assuming them.
- **Bell claude-6 with the model + design note. STOP.** Implementation is a
  separate phase after ratification (it will touch `make-claude-invoke-fn`/the
  invoke path/job ledger and must be reloaded with extreme care — claude-6 owns
  that).

## In-flight constraints (HARD)

- No JVM restart/kill, no Drawbridge reload — claude-6 owns the live reload.
  Phase 1 is model + design only; you only edit + `clojure -X:test`.
- Branch from the reloaded base:
  `git worktree add ../futon3c-car3 -b codex/car3-queue integration/agency-hardening`.
  Commit there. No push, no merge.
- Gates for Phase 1: clj-kondo clean; check-parens; `clojure -X:test` green on the
  model ns.

## Done (Phase 1) = bell claude-6 back

with: branch + sha, the model test result, the design note (mechanism + chosen
primitives + any capture-gaps), and an explicit "ready for ratification — impl
not started." claude-6 reviews the model, ratifies or adjusts the invariant set,
then scopes Phase 2 (impl).
