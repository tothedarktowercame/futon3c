# Agency invariants

Cross-Agency invariants for futon3c, modeled on `futon1a/docs/invariants.md`.
Each invariant has a one-line **statement**, traceability (pattern → implementation
→ **proof test**), and a **cheap tripwire** (when/where it's checked — never
per-turn). Research origin: `holes/excursions/E-agency-invariants.md`. Delivery
detail: `holes/missions/turn-delivery-invariants.md` (D1–D5).

The discipline (from futon1a): *an invariant without a proof test is a wish.*

## A0: Operator-turn delivery

- Statement: a turn you put in gets a reply — it is never silently dropped.
- Pattern: `futon-theory/durability-first` + single-writer queue (D2 no-loss + D4 reply-routed).
- Implementation: `src/futon3c/transport/http.clj` `handle-invoke-stream` → `turn-queue/accept-async!` (E2); `agency/turn_queue.clj`.
- Tripwire: **A3 boot gate** asserts the path is active (the failure mode was the path silently *off*).
- Proof: live-verified (E2 two-writer test, 2026-06-12). TODO: a regression test for the routing.
- Status: **implemented** (E2); flag `FUTON3C_REPL_THROUGH_QUEUE`, default true.

## A1: Single-writer

- Statement: exactly one drainer thread ever writes a given agent's pouch (D3).
- Pattern: `futon-theory/single-source-of-truth`.
- Implementation: `agency/turn_queue.clj` (`spawn-drainer!` / per-agent drainer, `*drained-by-outer*`).
- Tripwire: assert-no-second-drainer at spawn (candidate — not yet a guard).
- Proof: live-verified (E2 check: REPL + bell both feed via `turn-drainer-<agent>`). TODO: test.
- Status: **implemented**; tripwire candidate.

## A2: Roster preservation  ✅

- Statement: the persisted roster never loses agents unexpectedly; restore reconstructs it.
- Pattern: `futon-theory/counter-ratchet` (ported from futon1a `core/invariants.clj`).
- Implementation: `agency/roster_store.clj` `refuse-roster-clobber?` / `persist-registry!`
  (drop > `roster-drop-tolerance` is refused unless `FUTON3C_ROSTER_ALLOW_DROP`); `--fresh`
  suspends persistence entirely.
- Tripwire: **counter-ratchet at the persist chokepoint** — O(1), fires only on a registry
  mutation; refuses the write + logs loud, snapshot preserved.
- Proof: `test/futon3c/agency/roster_store_test.clj` (`counter-ratchet-refuses-unexpected-bulk-roster-drop`,
  `-allows-single-agent-deregistration`, `-allow-drop-escape-permits-bulk-drop`) — 8 tests / 29 assertions green.
- Status: **DONE + reviewed** (codex-4 `0da6705`; live-verified). Makes the roster-clobber bug structurally impossible.

## A3: Hardening active  ✅

- Statement: the durable-queue hardening is active whenever Agency serves.
- Pattern: fail-loud boot integrity gate (futon1a I2: "startup succeeds completely or fails loudly").
- Implementation: `agency/invariants.clj` `queue-hardening-status` / `warn-queue-hardening!`
  (checks the trio: `FUTON3C_DURABLE_QUEUE` / `DRAINER_V2` / `REPL_THROUGH_QUEUE`, all default true);
  called at boot (`dev/bootstrap.clj`); surfaced on `/health` (`http.clj`, `queue-hardening` field).
- Tripwire: **boot-time assertion** (once) + **`/health` surface** (on demand) — glaring warning +
  red health when any gate is off. Zero per-turn cost.
- Proof: `test/futon3c/agency/invariants_test.clj` — 2 tests / 7 assertions green; live-verified
  (forcing `DRAINER_V2=false` → `ok? false, degraded [:drainer-v2]`).
- Status: **DONE + reviewed** (codex-4 `ebefef8`; live-verified). Makes a stale-env-resume degradation visible, not silent.

## A4: Exactly-once delivery

- Statement: each accepted turn is delivered to its recipient exactly once (D1).
- Pattern: `futon-theory/fail-loud`.
- Implementation: `agency/turn_queue.clj` (msg-id dedup at `accept!`).
- Tripwire: the `[invoke-trace]` instrument (`registry.clj`, flag `FUTON3C_INVOKE_TRACE`) can sample
  `(recipient,msg-id)` for duplicates off the hot path.
- Proof: measured (2026-06-12: isolated + concurrent bells deliver exactly once; the perceived
  "twice" was auto-bellback, not a delivery double).
- Status: **candidate** — lower priority (no acute violation; instrument exists).

## The principle

These replace *"default the flag true and hope a resume keeps it"* with *"assert the
property; fail loud (or flag red) the instant it doesn't."* All tripwires fire at **coarse
events** — persist (A2), boot + on-demand health (A3), sampled trace (A4) — so there is **no
per-turn cost**, exactly as futon1a shows is achievable.

## References
- `futon1a/docs/invariants.md`, `futon1a/src/futon1a/core/invariants.clj` (the model)
- `holes/excursions/E-agency-invariants.md` (research)
- `holes/missions/turn-delivery-invariants.md` (D1–D5 delivery detail)
