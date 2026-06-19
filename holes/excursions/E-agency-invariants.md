# E-agency-invariants — research: futon3c hardening as guaranteed invariants

Date: 2026-06-14
Status: RESEARCH ONLY (Joe). Started from reading futon1a's invariant docs/code.
Question: can we specify futon3c Agency features (durable-queue trio, restore,
single-writer, operator-turn delivery) as **invariants with cheap tripwires** —
so we get *guarantees* at all times rather than depending on flags defaulting
true? Tripwires must be **inexpensive** (no per-turn cost).

## What futon1a does (the prior art)

futon1a (`docs/invariants.md`, `src/futon1a/core/invariants.clj`, CLAUDE.md) has
**5 named invariants**, each a one-line *statement* + full traceability
(theory pattern → storage pattern → implementation → a **proof test**):

| | Invariant | Statement |
|---|---|---|
| I0 | Persistence | what you save is what you get back |
| I1 | Identity | one entity per identity, no ambiguity |
| **I2** | **Integrity** | **startup succeeds completely or fails loudly** |
| I3 | Hierarchy | errors surface at the layer that caused them |
| I4 | Debugging | any bug diagnosable in < 10 min |

Three enforcement mechanisms — all **cheap** because they fire at *coarse events*,
never per-read:

1. **Single chokepoint.** All writes go through `run-write!` / the layered pipeline
   (L4 model → L3 authz → L2 integrity → L1 identity → L0 durable). Invariants are
   enforced in *one* place, not scattered.
2. **Counter-ratchet** (`enforce-counter-ratchet!`). A pure O(affected-ids) guard on
   each write tx-plan: protected-class counts (entity/relation/…) must not drop;
   a drop throws a typed **Layer-2 integrity error**. Has a declared
   `allow-drop-classes` escape hatch — so the invariant is precisely *"no
   **unexpected** drop."* Fail-loud, not silent.
3. **Boot integrity gate** (I2) + **health endpoint** (I4). Startup verifies integrity
   and *fails loudly* if not; `diag/health.clj` surfaces invariant state on demand.

The shape: **assert-or-fail-loud at boot + a monotonicity ratchet at the write
chokepoint + an inspectable health surface.** None of it is per-operation expensive.

## Why this fits futon3c's recent failures

Every Agency bug this week was a **silent degradation** — exactly what fail-loud
invariants prevent:
- **Roster clobber** (8 → 4): a count dropped silently. A counter-ratchet on the
  roster persist would have *blocked* it (unless `--fresh` declared allow-drop).
- **Flag-loss on resume** (durable-queue/drainer-v2/E2 reverted to off): the
  hardening silently deactivated. A boot integrity gate asserting "queue path
  active" would have failed loud instead of quietly dropping operator turns.
- **Operator-turn drop**: a D-invariant (turn-delivery) violated with no signal.

And we already have the *statements* drafted: `turn-delivery-invariants.md` D1–D5
(exactly-once, no-loss, single-writer, reply-routed, operator-priority).

## Candidate futon3c invariants + their cheap tripwires

| | Invariant (statement) | Cheap tripwire (when it fires) |
|---|---|---|
| **A0** | Operator turn → reply, never silently dropped (E2/D-set) | boot gate asserts E2+drainer-v2+durable-queue all active |
| **A1** | Exactly one drainer writes a pouch (D3) | assert at drainer-spawn: no second drainer for an agent |
| **A2** | Restore reconstructs the persisted roster; agent count never drops unexpectedly across reboot | **roster counter-ratchet** at persist/restore (the futon1a mechanism, ported) |
| **A3** | The durable-queue hardening is active whenever Agency serves | **boot integrity gate**: assert the trio on; fail-loud / red health if not |
| **A4** | Exactly-once delivery (D1) | already measurable via the invoke-trace instrument; no per-turn cost if sampled |

The two highest-value, lowest-cost ones (they catch the exact bugs Joe hit):

- **A2 roster ratchet** — port `enforce-counter-ratchet!` to `roster_store/persist-registry!`:
  refuse to persist a roster whose agent count is below the on-disk snapshot's,
  unless an explicit allow-drop is set (`--fresh` sets it). O(1), fires only on
  persist (a coarse event). *This is the single most valuable tripwire* — it makes
  "--fresh is non-destructive" a guarantee rather than a comment, and a stale-env
  resume can't quietly shrink the roster.
- **A3 boot gate** — at `bootstrap`, after start-agents, assert the durable-queue
  trio is active; if any is off, log a glaring warning (or set a red `/health`
  flag). Replaces *"default true and hope"* with *"asserted active or visibly
  broken."* Fires once, at boot.

## The reframe

The point Joe is making: today's robustness is **"default the flag true and hope
the resume keeps it."** The invariant version is **"assert the property holds, and
fail loud (or flag red) the instant it doesn't."** The futon1a evidence is that
this can be done **inexpensively** — at boot (once), at the persist chokepoint
(O(1) ratchet), and via an on-demand health surface — with *zero per-turn cost*.

## If pursued (not now — research only)

1. Lift `turn-delivery-invariants.md` D1–D5 + A2/A3 into a futon3c `docs/invariants.md`
   in the futon1a format (statement + traceability + proof test per invariant).
2. Build A2 first (port the counter-ratchet to the roster persist) — highest value,
   smallest surface, directly prevents the clobber.
3. Add A3 boot gate (assert-or-flag the hardening trio).
4. Each gets a **proof test** (futon1a discipline: an invariant without a test is a
   wish).

## References
- `futon1a/docs/invariants.md`, `futon1a/src/futon1a/core/invariants.clj`, `futon1a/README.md`
- `futon3c/holes/missions/turn-delivery-invariants.md` (D1–D5)
- The week's failures: roster clobber + flag-loss-on-resume (M-agency-hardening notes)
