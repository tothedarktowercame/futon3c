# Mission: Fulab Logic — core.logic Invariants Beyond Portfolio

**Date:** 2026-03-10
**Status:** NOT STARTED
**Blocked by:** None (portfolio/logic.clj operational; proof peripheral
instantiated; Tickle watchdog running; Emacs-side reazon invariants landed
in agent-chat-invariants.el)
**Cross-ref:** M-portfolio-inference (predecessor pattern), M-proof-peripheral
(proof domain), M-tickle-overnight (Tickle domain)

## Motivation

`portfolio/logic.clj` proved that core.logic works well in futon3c: build a
pldb from live state, express structural properties as goals, query for
violations. But that coverage stops at the portfolio boundary. The domains
with the most structural constraints — proof peripherals, Tickle coordination,
peripheral hop topology — have no relational invariant layer.

Meanwhile, `agent-chat-invariants.el` now provides runtime invariant checking
on the Emacs side (marker ordering, session consistency, turn timing, with
reazon as an extension point). The JVM side should have a parallel capability:
express invariants as core.logic goals, snapshot live state into a fact DB,
and query for violations.

The gap is not "we have no tests." The existing `invariants_test.clj` checks
I-1 through I-5 via grep. The gap is that **runtime** structural properties
— proof ledger consistency, Tickle scan-page-escalate ordering, hop topology
compliance — are enforced procedurally in scattered locations but never
expressed as queryable relations.

### Why core.logic and not just Malli

Malli validates shapes (is this map well-formed?). Core.logic validates
*relationships* (does this combination of facts imply a contradiction?).

Examples of things Malli cannot express:
- "Every DAG dependency of a `:proved` item is itself `:proved` or `:axiom`"
- "Every escalation has a prior page which has a prior scan"
- "No two active sessions share an agent-id"
- "If a cycle reached phase P, all required-outputs for phases before P exist"

These are relational properties over sets of facts. Core.logic is the right
tool.

## Domains

### 1. Proof Peripheral — proof_logic.clj

The proof peripheral has the richest structural constraints (SR-1 through
SR-8) currently enforced procedurally in `proof_backend.clj` and
`proof_shapes.clj`. Expressing them as core.logic goals makes them
queryable and composable.

**Relations:**

```clojure
(pldb/db-rel ledger-itemo item-id status evidence-class)
(pldb/db-rel dag-edgeo from-id to-id)
(pldb/db-rel cycleo cycle-id phase)
(pldb/db-rel cycle-outputo cycle-id phase output-key)
(pldb/db-rel failed-routeo snapshot-id route-id)
(pldb/db-rel snapshot-ordero earlier-id later-id)
```

**Goals:**

| Goal | Checks | Source constraint |
|------|--------|-------------------|
| `status-evidence-valido` | `:proved` requires `:analytical` or `:mixed` evidence | SR-5 |
| `proved-deps-provedo` | DAG deps of `:proved` items are `:proved`/`:axiom` | SR-2 + DAG |
| `dag-acyclico` | No cycles in dependency DAG | SR-3 (Kahn's) |
| `phase-outputs-completeo` | Required outputs present for all prior phases | CR-1 |
| `failed-routes-monotonico` | Failed routes append-only across snapshots | SR-8 |

**build-db:** Snapshot from `proof-state/{problem-id}.edn` (ledger, DAG, cycles,
failed routes). One DB per problem.

**File:** `src/futon3c/peripheral/proof_logic.clj`
**Test:** `test/futon3c/peripheral/proof_logic_test.clj`

### 2. Tickle — tickle_logic.clj

Tickle's design principle is **umwelt darkness**: it infers agent liveness
from evidence timestamps, never from agent internals. This is currently a
social contract documented in the namespace docstring. Core.logic can make it
mechanically checkable.

**Relations:**

```clojure
(pldb/db-rel agento agent-id)
(pldb/db-rel scannedo agent-id timestamp)
(pldb/db-rel pagedo agent-id timestamp)
(pldb/db-rel escalatedo agent-id timestamp)
(pldb/db-rel last-evidenceo agent-id timestamp)
(pldb/db-rel stallo agent-id)
```

**Goals:**

| Goal | Checks |
|------|--------|
| `escalation-chain-valido` | Every escalation has a prior page, every page has a prior scan |
| `stall-evidence-alignedo` | Agent marked stalled iff last evidence older than threshold |
| `no-orphan-escalationo` | No escalation for an agent not in the registry |

**build-db:** Snapshot from evidence store (query for Tickle-authored entries
with tags `:scan`, `:page`, `:escalate`) plus registry snapshot.

**File:** `src/futon3c/agents/tickle_logic.clj`
**Test:** `test/futon3c/agents/tickle_logic_test.clj`

### 3. Peripheral Hop Topology — peripheral_logic.clj

The peripheral hop protocol has entry/exit sets declared in `peripherals.edn`.
Runtime hops should respect the declared topology. Currently validated
procedurally in `social/peripheral.clj` via `validate-hop`. Core.logic can
check the full topology for structural soundness.

**Relations:**

```clojure
(pldb/db-rel peripheralo pid)
(pldb/db-rel exit-seto from-pid to-pid)
(pldb/db-rel entry-seto to-pid from-source)
(pldb/db-rel observed-hopo from-pid to-pid session-id)
```

**Goals:**

| Goal | Checks |
|------|--------|
| `hop-valido` | Observed hop respects source exit set and target entry set |
| `reachable-fromo` | Transitive reachability between any two peripherals |
| `dead-end-freeo` | No peripheral has an empty exit set (stuck agent) |
| `entry-exit-symmetryo` | If A lists B in exit set, B lists A (or `:from-any`) in entry set |

**build-db:** Load `peripherals.edn` for declared topology, evidence store for
observed hops (evidence entries with `:peripheral` subject type).

**File:** `src/futon3c/social/peripheral_logic.clj`
**Test:** `test/futon3c/social/peripheral_logic_test.clj`

## Architecture

### Pattern: follow portfolio/logic.clj

Each domain module follows the same shape:

```clojure
(ns futon3c.peripheral.proof-logic
  (:require [clojure.core.logic :as l]
            [clojure.core.logic.pldb :as pldb]))

;; 1. Relations (fact schema)
(pldb/db-rel ledger-itemo item-id status evidence-class)
;; ...

;; 2. build-db: snapshot live state → pldb
(defn build-db [proof-state] ...)

;; 3. Goals: structural properties
(defn status-evidence-valido [item-id] ...)

;; 4. Queries: return violations
(defn query-violations [db] ...)
```

### Runner: invariant_logic.clj

A thin runner that calls all domain modules:

```clojure
(ns futon3c.invariants.logic
  (:require [futon3c.peripheral.proof-logic :as proof-logic]
            [futon3c.agents.tickle-logic :as tickle-logic]
            [futon3c.social.peripheral-logic :as peripheral-logic]))

(defn check-all
  "Run all core.logic invariant checks against current system state.
   Returns {:domain [{:goal :tag :msg} ...] ...} or empty map if clean."
  [system-snapshot]
  ...)
```

This can be wired into:
- `clojure -X:test` (as test assertions)
- REPL diagnostics (`(check-all (snapshot-system))`)
- Tickle's own scan cycle (check invariants as part of periodic health scan)
- HTTP endpoint (`GET /api/alpha/invariants`)

### Emacs ↔ JVM symmetry

| Layer | Tool | Checks |
|-------|------|--------|
| Emacs (agent-chat-invariants.el) | reazon | Buffer markers, session state, turn timing |
| JVM (proof_logic.clj etc.) | core.logic | Proof ledger, Tickle chains, hop topology |

Both follow the same pattern: snapshot state → build fact DB → query goals →
report violations. The Emacs side checks client-side invariants; the JVM side
checks server-side invariants. They complement rather than overlap.

A future bridge: reazon checks in Emacs could query the JVM invariant endpoint
to surface server-side violations in the REPL buffer.

## Files

### New files

| File | Lines (est.) | Purpose |
|------|-------------|---------|
| `src/futon3c/peripheral/proof_logic.clj` | ~150 | Proof ledger/DAG/cycle invariants |
| `src/futon3c/agents/tickle_logic.clj` | ~100 | Scan/page/escalate chain, umwelt darkness |
| `src/futon3c/social/peripheral_logic.clj` | ~80 | Hop topology validation |
| `src/futon3c/invariants/logic.clj` | ~60 | Runner that aggregates all domains |
| `test/futon3c/peripheral/proof_logic_test.clj` | ~120 | Proof invariant tests |
| `test/futon3c/agents/tickle_logic_test.clj` | ~80 | Tickle invariant tests |
| `test/futon3c/social/peripheral_logic_test.clj` | ~60 | Hop topology tests |

### Modified files

| File | Change |
|------|--------|
| `test/futon3c/architecture/invariants_test.clj` | Add test that loads and runs `invariants.logic/check-all` |

## Completion Criteria

- [ ] `proof_logic.clj`: SR-5 (status-evidence), DAG deps, phase outputs — all expressible as goals
- [ ] `tickle_logic.clj`: escalation chain ordering, stall-evidence alignment
- [ ] `peripheral_logic.clj`: hop validity, dead-end detection, entry-exit symmetry
- [ ] `invariants/logic.clj` runner aggregates all three domains
- [ ] All tests pass via `clojure -X:test`
- [ ] At least one invariant catches a real (or synthetically injected) violation in tests
- [ ] REPL helper in `dev.clj`: `(check-invariants)` runs all checks against live state

## Sequencing

Start with **proof_logic.clj** — it has the richest data (ledger items, DAG
edges, cycle phases, failed routes) and the most structural constraints to
express. The proof state files in `data/proof-state/` provide real test data.

Then **tickle_logic.clj** — smaller surface area but architecturally
interesting because it makes the umwelt-darkness property checkable.

Then **peripheral_logic.clj** — mostly topology checks against the static
`peripherals.edn`, plus observed-hop validation from evidence.

The runner and `dev.clj` helper come last since they're just wiring.
