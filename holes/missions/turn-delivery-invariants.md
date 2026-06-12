# Turn-delivery invariants — logic model (M-agency-hardening)

**Status:** DERIVE → ARGUE. Logic-model-before-code for the Agency hot path.
**Date:** 2026-06-12 · owner: claude (this session)
**Drivers:**
- Joe: *"REPL users should be getting the same (or better!) guarantees as agents do.
  If I put in a turn, I should get a reply to that turn, it shouldn't get lost!!"*
- claude-1 datum: *every async bell arrived **exactly twice** (fable-2 handoff, owner
  golden-round gold, both codex-3 ledger reports); one synchronous whistle paired
  cleanly first try.* → duplication is in **bell delivery**, not agent behavior.

These two reports are the **same defect class**: more than one independent path feeds
one pouch. The fix is one principle — **one durable queue, one drainer, exactly once.**

---

## The five invariants

**D1 — Exactly-once delivery.** For each accepted turn `(recipient, msg-id)`, the
recipient's pouch is fed via `feed-turn!` **exactly once**.

**D2 — No-loss / durability.** Every accepted turn is durably enqueued, survives
restart (restored from disk; `resume-pending-drainers!` re-spawns drainers), and is
removed from the queue only after a *terminal* status (processed / failed / stale /
deduped).

**D3 — Single-writer.** For a given pouch, exactly one thread — the agent's dedicated
drainer — ever calls `feed-turn!`. No other path touches the pouch.

**D4 — Reply-routed-to-originating-turn.** Each turn carries its own reply-route; the
terminal result is delivered to the originator of **that** turn and no other
(bell → return bell / auto-bellback; REPL → the held-open SSE channel). No cross-turn
reply leakage.

**D5 — Operator-priority without bell-starvation.** Operator (REPL) turns drain ahead
of agent bells via a priority lane, but the lane is fair enough that agent bells are
not starved indefinitely.

---

## Where today's code stands against each (grounded)

| Inv | Bells today | REPL `invoke-stream` today |
|---|---|---|
| **D1** | **HOLDS** — measured exactly-once under load (see Measurement 2026-06-12) | n/a (no queue at all) |
| **D2** | OK — durable queue + restore + `resume-pending-drainers!` | **VIOLATED** — transient `.submit invoke-executor`; lost on evict/disconnect/timeout, no requeue |
| **D3** | OK — single drainer thread | **VIOLATED** — `reg/invoke-agent!` direct → `feed-turn!` is a 2nd writer racing the drainer on the per-pouch lock |
| **D4** | OK — `mark-terminal!` delivers to the turn's waiter / auto-bellback | partial — SSE sink installed on the *registry* (`set-invoke-event-sink!`), not bound to a queued turn |
| **D5** | n/a | absent — no priority |

### D1 — MEASURED: holds. The "twice" is the auto-bellback, not a delivery double.

The two-dispatcher hypothesis (accept-async queue **and** conductor/tickle both firing)
was **tested with a live instrument and DISPROVEN.** Instrument: `[invoke-trace]` at the
`invoke-agent!` chokepoint (registry.clj ~670, flag `FUTON3C_INVOKE_TRACE`, writes
`/tmp/invoke-trace.log` with `{agent, msg-id, thread, preview}`).

**Measurement (2026-06-12):**
- 1 owner→fable-2 bell → **1** delivery (`turn-drainer-fable-2`).
- 1 codex-3→claude-1 bell (faithful repro of claude-1's "ledger report") → **1** delivery
  (`turn-drainer-claude-1`).
- **4 overlapping** codex-3→fable-2 bells → **4** deliveries, every one via
  `turn-drainer-fable-2`, exactly once. The conductor/tickle dispatcher **never fired**.
  Alongside: 3 lines `agent=codex-3 Surface: auto-bellback 🔔 fable-2 finished job …`.

**Conclusion:** delivery is exactly-once, even under concurrent load. claude-1's "every
bell arrived exactly twice" is **request + auto-bellback**: every A→B bell yields (1) B
receives the request, (2) A receives a `🔔 finished job` bellback. An agent that both
sends and receives sees both and reads it as "twice." This is precisely the **crossing**
effect in `CLAUDE.md` ("a reply and a new request both arrive as 'a bell from X'"), and is
why the **whistle paired cleanly — synchronous, crossing-immune, no separate bellback.**
No delivery bug. (Any true double claude-1 saw was a transient OOM-recovery artifact —
the durable fixes `load-state` clears `:draining` + `resume-pending-drainers!` already
address recovery; a stale `:draining` lock on codex-2 is leftover residue, harmless.)

Ruled out structurally too: `pop-next!` is atomic-remove (turn_queue.clj:216); the inner
`accept-and-drain!` is correctly gated by `*drained-by-outer*` on the drainer thread
(dev.clj:3797).

---

## The fix (one principle, three edits)

**E1 — DROPPED (not warranted).** Premised on a two-dispatcher delivery double that the
measurement above disproved. The conductor/tickle path does not, in fact, produce a
second delivery for an inbound bell; D1 already holds. If a future trace ever shows two
`invoke-trace` lines (different threads) for one `(recipient, msg-id)`, revisit — but on
current evidence there is nothing to fix here. The perceived "twice" is the auto-bellback
(an agent-protocol/UX matter, addressed by the CLAUDE.md whistle-to-reconcile guidance and
the clear `Surface: auto-bellback / 🔔 finished job` labeling), not a queue change.

**E2 — Route REPL `invoke-stream` through `accept-async!` (D2, D3, D4). ✅ IMPLEMENTED +
VERIFIED 2026-06-12.** `handle-invoke-stream` (http.clj) now branches on
`repl-through-queue?` (flag `FUTON3C_REPL_THROUGH_QUEUE`, default OFF, load-dark; requires
drainer-v2). ON: the turn is enqueued via `turn-queue/accept-async!` with a `:process-fn`
that sets the SSE `sink-fn` **inside** itself (drainer thread, turn-exclusive ⇒ no
cross-talk) under `*drained-by-outer* true`, and a `:finalize-fn` that emits the terminal
`done` event and closes the channel. OFF: the original direct `reg/invoke-agent!` on
`invoke-executor`, byte-for-byte.
Live verification (concurrent REPL + bell at fable-2):
- **D3**: ON ⇒ both turns feed via `turn-drainer-fable-2` (was: REPL on `invoke-worker`,
  bell on `turn-drainer` — the collision). Single-writer achieved.
- **D4**: REPL channel received `result:"ack-repl-E2"` (its own), not the bell's — no
  cross-talk.
- OFF regression: legacy reply still delivered via `invoke-worker`, unchanged.
- Gates: clj-kondo errors 0; check-parens OK.
Self-authored + self-verified; the flag-OFF default is the review gate (Joe opts in).

**E3 — Operator-priority lane (D5). DEFERRED — future idea (Joe, 2026-06-12).** Not built.
With E2 live, an operator REPL turn already gets the loss fix (single-writer + durable +
reply-routed); it just queues **FIFO with bells** rather than jumping ahead. E3 would make
operator turns enqueue at the head (or a high-priority sub-queue the drainer checks first),
with a fairness bound so bells aren't starved. Parked as a future enhancement, not a gap.

All three are **flag-gated, load-dark** (OFF path byte-for-byte unchanged), per the
durable-queue discipline already used for drainer-v2.

---

## Verification gates (before INSTANTIATE → DONE)

1. **D1 confirm + fix:** instrument `invoke-agent!` entry with `{recipient, msg-id,
   dispatch-source}`. Before: doubled bells show two sources (accept-async drainer +
   conductor/tickle) for one msg-id. After E1: each `(recipient, msg-id)` appears once.
   *(This gate also confirms the D1 root-cause hypothesis — do not mark it confirmed
   until the two sources are observed.)*
2. **Replay claude-1's scenario:** fable-2 handoff / owner gold / codex-3 ×2 each feed
   the pouch once; whistle still clean.
3. **REPL no-loss (D2+D4):** submit a REPL turn, kill the curl mid-flight → the turn
   still completes and is delivered, and **exactly one** reply returns (no double, no
   loss).
4. **REPL single-writer (D3):** under concurrent bell load, assert only the drainer
   thread calls `feed-turn!` for that pouch (no invoke-executor entries).
5. **OFF-path:** flag OFF ⇒ byte-for-byte identical to current behavior.
6. **clj-kondo + check-parens + turn-queue tests** green (AGENTS.md gates).

---

## Notes
- Pairs with `typed_bells_invariants.clj` (TB-1..7) — same executable-model discipline;
  a core.logic/pldb model of D1 (one msg-id → one feed across N dispatchers) is the
  natural companion if we want the invariant machine-checked.
- This extends M-agency-hardening's "no turn silently dropped" guarantee **from agents
  to the operator**, and folds in the duplicate-delivery defect as a D1 violation.
