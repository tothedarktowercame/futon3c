# Mission: Structural Law — Universal Invariants as Self-Representing Stack Layer

**Date:** 2026-03-10
**Status:** IDENTIFY
**Cross-ref:** M-self-representing-stack (predecessor), M-three-column-stack (three
columns), M-fulab-logic (domain-specific invariants), M-invariant-violations (ledger)
**Owner:** futon3c (core.logic infrastructure), with dependencies on futon4 (Arxana
hypergraph for Column 2 representation), futon1a (evidence store)

## Motivation

We now have four operational core.logic invariant layers:

| Domain | File | Status |
|--------|------|--------|
| Portfolio | `portfolio/logic.clj` | Operational (predecessor) |
| Tickle | `agents/tickle_logic.clj` | Operational, 0 violations |
| Agency | `agency/logic.clj` | Operational, 5 violations cataloged |
| Proof | `peripheral/proof_logic.clj` | Operational, tested |

All four follow the same pattern: snapshot → build-db → goals → query-violations.
And all four express the same small set of structural properties, just projected
onto different vocabularies:

| Meta-invariant | Tickle | Agency | Proof |
|---|---|---|---|
| Graph symmetry | escalation backed by page | hop exit ↔ entry | depends-on ↔ unlocks |
| Status discipline | scan → page → escalate | idle ↔ invoking | open → partial → proved |
| Phase ordering | scan before page before escalate | entry before work before exit | observe → ... → completed |
| Required outputs | page needs scan evidence | — | each phase needs outputs |
| Existence | scanned agents registered | hop targets exist | deps exist in ledger |
| Dependency satisfaction | — | — | proved needs proved deps |

That's five or six patterns total. The domain-specific files are projections of
these patterns onto domain-specific fact databases.

**The insight from M-self-representing-stack:** these meta-invariants are a layer
of the self-representing stack. They're how the stack checks its own structural
law — the same machinery applied to proof state, agent coordination, mission
lifecycle, and code structure (the three columns).

**The practical problem:** three agents (claude-1, claude-2, codex-1) and no
automated way to decompose "what needs doing" into agent-shaped work items.
If violations are obligations, and structural law is universal across domains,
then the conductor should dispatch work using the same invariant vocabulary
everywhere.

## Approach: All Coding via Codex Handoff

This mission runs as a meta-mission: strategy and decomposition happen in
conversation (Claude), all coding is Codex handoffs via GitHub issues following
the scope-bounded-handoff pattern (CLAUDE.md §Codex Handoff Protocol).

This is both pragmatic (token budget) and a test of the dispatch pattern the
mission is trying to formalize.

## Phase 1: IDENTIFY — Enumerate the Universal Invariants

**Goal:** Confirm the meta-invariant set is complete by checking it against
every domain we've built, plus Missions (which we haven't instrumented yet
but have the most evidence for).

### Handoff 1.1: Mission lifecycle invariant layer

The richest untapped domain. Missions have phases (IDENTIFY → MAP → DERIVE →
ARGUE → VERIFY → INSTANTIATE), required outputs per phase, dependencies between
missions, status discipline (blocked/active/complete). We've done dozens of
missions — this is our best evidence source.

- `:in` — existing `*_logic.clj` files (pattern), sample M-*.md files
- `:out` — `src/futon3c/peripheral/mission_logic.clj` + test
- Invariants: phase ordering, required outputs, cross-mission deps, status
  discipline, blocker consistency

### Handoff 1.2: Verify meta-invariant completeness

After mission_logic.clj lands, diff all five domain logic files. Are there
invariants in any domain that don't map to a meta-invariant? If so, the
meta-invariant set needs extending.

- `:in` — all five `*_logic.clj` files
- `:out` — `docs/structural-law-inventory.md` (enumeration + mapping table)

## Phase 2: DERIVE — Extract Universal Combinators

**Goal:** Factor the meta-invariants into reusable goal combinators that
domain logic files can call, reducing each domain to a thin projection.

### Handoff 2.1: Structural law combinators

Extract the meta-invariants as parameterized core.logic goals.

- `:in` — all five `*_logic.clj` files, `docs/structural-law-inventory.md`
- `:out` — `src/futon3c/logic/structural_law.clj` + test
- Functions (sketch):
  - `(graph-symmetry-checko rel-a rel-b id-a id-b)` — if rel-a holds, rel-b holds
  - `(phase-ordering-checko phase-rel phase-order entity-id)` — phases advance in order
  - `(required-outputs-checko output-rel requirements entity-id phase)` — outputs present
  - `(existence-checko ref-rel entity-rel ref-id)` — referenced things exist
  - `(dependency-satisfaction-checko dep-rel status-rel required-status)` — deps meet bar

### Handoff 2.2: Refactor one domain to use combinators

Pick the simplest domain (probably Agency — fewest invariants) and refactor it
to call `structural_law.clj` combinators. Prove the abstraction compresses
without losing domain specificity.

- `:in` — `agency/logic.clj`, `structural_law.clj`
- `:out` — modified `agency/logic.clj` + green tests

### Handoff 2.3: Refactor remaining domains

Apply the same refactor to tickle_logic, proof_logic, mission_logic. Portfolio
is the predecessor and may stay as-is.

- `:in` — `structural_law.clj`, all domain logic files
- `:out` — modified domain files + green tests

## Phase 3: ARGUE — Violations as Obligations, Conductor Integration

**Goal:** Close the loop: violations detected by structural law become
dispatchable obligations. The FM conductor can assign work to idle agents
based on what the invariant layers surface.

### Handoff 3.1: Violation → Obligation mapping

Define how violations map to work items. Not every violation is actionable by
an agent — some need human judgment (the V-1..V-5 "should :explore accept
entry from anywhere?" is a design question). Classify violations by
actionability.

- `:in` — `M-invariant-violations.md`, all domain logic files
- `:out` — `src/futon3c/logic/obligation.clj` + test
- Key types: `:auto-fixable` (agent can resolve), `:needs-review` (human),
  `:informational` (context only)

### Handoff 3.2: Aggregate invariant runner

A thin runner that calls all domain logic modules and produces a unified
violation/obligation report.

- `:in` — all domain logic files, `obligation.clj`
- `:out` — `src/futon3c/logic/invariant_runner.clj` + test
- Wire into: REPL helper `(check-invariants)`, HTTP endpoint (optional)

### Handoff 3.3: FM conductor integration

Connect the invariant runner to `fm.clj`'s dispatch loop. When an agent goes
idle, the conductor checks for actionable violations (`:auto-fixable`
obligations) and pages the agent with the highest-priority one.

- `:in` — `fm.clj`, `invariant_runner.clj`, `obligation.clj`
- `:out` — modified `fm.clj` + integration test

### Handoff 3.4: Self-representing stack wiring

The structural law layer IS the self-representing stack layer for Column 2
(development process). Wire the invariant runner's output into Arxana
hyperedges so violations are navigable in the hypergraph browser.

- `:in` — `invariant_runner.clj`, Arxana hyperedge API
- `:out` — `src/futon3c/logic/arxana_bridge.clj` + test

## Completion Criteria

- [ ] Meta-invariant set enumerated and verified against 5+ domains
- [ ] `structural_law.clj` provides reusable combinators
- [ ] At least 3 domain logic files refactored to use combinators
- [ ] Violations classified by actionability
- [ ] FM conductor dispatches work from invariant violations
- [ ] REPL `(check-invariants)` runs all domains against live state
- [ ] Structural law violations navigable in Arxana (Column 2 integration)

## Relationship to Three-Column Stack

| Column | How structural law applies |
|--------|---------------------------|
| Knowledge (math) | Proof invariants: DAG integrity, status-evidence, phase outputs |
| Development (missions) | Mission invariants: phase ordering, deps, blocker consistency |
| Code (reflection) | Could add: namespace dep cycles, protocol coverage, dead vars |

The meta-invariants are column-agnostic. `graph-symmetry-checko` works on
proof DAGs, mission dependency graphs, and namespace dependency graphs. The
self-representing stack is the stack checking all three columns with the same
structural law.

## Risk: Premature Abstraction

The five domain logic files may not actually share as much structure as the
table above suggests. Phase 1 (IDENTIFY) exists specifically to test this
before committing to Phase 2 (DERIVE). If the meta-invariant mapping is
forced or lossy, we stop at Phase 1 and keep domain-specific files as-is.
The domain files already work. Abstraction is only worth it if it compresses.
