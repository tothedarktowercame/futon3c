# Mission: Invariant Violations Ledger

**Date:** 2026-03-10
**Status:** MAP
**Cross-ref:** M-fulab-logic, M-self-representing-stack, M-three-column-stack,
TN-dev-clj-decomposition

## Motivation

The core.logic invariant layers are now operational across three domains:

| Domain | Namespace | Status |
|--------|-----------|--------|
| Portfolio | `portfolio/logic.clj` | Operational (predecessor) |
| Tickle coordination | `agents/tickle_logic.clj` | Operational, 0 violations on live system |
| Agency | `agency/logic.clj` | Operational, 5 violations on live system |

This mission tracks violations surfaced by these layers and any future
invariant domains (proof_logic, peripheral_logic per M-fulab-logic).

The principle: invariant layers IDENTIFY structural issues; this ledger MAPS
them; fixes are scoped and applied systematically rather than depth-first.

## Active Violations

### V-1..V-5: Peripheral Entry/Exit Asymmetry (agency/logic.clj)

The `query-entry-exit-asymmetry` check found 5 cases where a peripheral
declares a `:hop-X` exit but the target peripheral's entry set doesn't
include `:from-<source>` or `:from-any`.

All 5 share a common pattern: `:explore` and `:edit` have restrictive
entry sets that don't account for hops from peripherals that list them
as exit targets.

| ID | Source | Target | Exit condition | Target entry set | Notes |
|----|--------|--------|---------------|------------------|-------|
| V-1 | `:mission` | `:explore` | `:hop-explore` | `#{:default :from-reflect}` | Missing `:from-mission` |
| V-2 | `:chat` | `:explore` | `:hop-explore` | `#{:default :from-reflect}` | Missing `:from-chat` |
| V-3 | `:proof` | `:explore` | `:hop-explore` | `#{:default :from-reflect}` | Missing `:from-proof` |
| V-4 | `:chat` | `:edit` | `:hop-edit` | `#{:from-test :user-request :from-explore}` | Missing `:from-chat` |
| V-5 | `:mission-control` | `:explore` | `:hop-explore` | `#{:default :from-reflect}` | Missing `:from-mission-control` |

**Root cause:** `:explore` was originally reachable only from `:reflect`
(the PAR loop) and `:default` (initial entry). As more peripherals added
`:hop-explore` exits, the entry set wasn't updated to match.

**Fix options:**
- (a) Add `:from-any` to `:explore` entry — it's the general-purpose starting
  peripheral, arguably should accept entry from anywhere
- (b) Add specific `:from-X` entries for each source
- (c) These hops only work via `:user-request` override at runtime — document
  this as intentional and suppress the asymmetry check for user-request paths

**Likely resolution:** Option (a) — `:explore` is the "home" peripheral,
analogous to a shell prompt. Restricting entry to it seems like an oversight
rather than a design choice.

### Tickle Domain: 0 violations

Live system check: all invariants hold.

- Escalation chains backed by pages: clean
- Pages backed by scans: clean
- Stall-evidence alignment: clean
- Registry eligibility: clean

### Agency Registration Domain: 0 violations

Live system check:

- No untyped agents
- No duplicate sessions
- No route inconsistencies
- No proxy shadowing

## Domains Not Yet Instrumented

Per M-fulab-logic, these domains have been identified but not yet implemented:

| Domain | Proposed file | Key invariants |
|--------|--------------|----------------|
| Proof ledger | `peripheral/proof_logic.clj` | DAG acyclicity, status-evidence validity, phase output completeness |
| Peripheral hop topology | `social/peripheral_logic.clj` | Reachability, dead-end detection (partially covered by agency/logic.clj) |

Note: some of `peripheral_logic.clj`'s proposed scope is now covered by
`agency/logic.clj` (hop validation, entry/exit symmetry, dead-end detection).
The remaining unique contribution would be observed-hop validation from
evidence and transitive reachability analysis.

## Compositional Questions

The invariant layers are also surfacing architectural questions that go
beyond individual violations:

1. **Is the FM conductor a Tickle configuration?** Both scan for idle agents,
   page via IRC, respect cooldowns. If Tickle's spec is parameterized over
   obligation source, FM conductor may be redundant. Requires comparing
   `tickle_logic.clj` invariants against FM conductor behavior.

2. **Should `:explore` be the universal entry peripheral?** V-1..V-5 suggest
   yes. This is a design question, not a bug — the invariant layer surfaced
   it, the answer requires architectural judgement.

3. **Cross-domain invariants:** Can we express properties that span domains?
   E.g., "an agent paged by Tickle must be registered in the agency with
   `:coordination/execute` capability" — this crosses tickle_logic and
   agency/logic. Currently each domain checks its own slice; cross-domain
   composition is the next step.

## Process

- Violations are numbered V-N globally across all domains
- Each violation records: source check, affected data, root cause, fix options
- Fixes are batched — not applied one-at-a-time during exploration
- New invariant domains add their violations to this ledger as they come online
- Resolved violations move to a "Resolved" section with commit reference
