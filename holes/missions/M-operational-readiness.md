# Mission: Operational Readiness (M-operational-readiness)

Date: 2026-02-20
Status: Active

## Goal

Make the Agency API and monitoring infrastructure robust enough for
unattended overnight multi-agent operation. After this mission, a human
can start `make dev` + `make tickle` + `make codex-autowake`, walk away,
and return to find evidence of useful work completed — with full
traceability from mission goal through pattern selection to test evidence.

## Argument

**IF** the Agency exposes a complete CRUD lifecycle for agents (register,
lookup, deregister), rich health/monitoring endpoints, and evidence query
filtering by author —

**HOWEVER** the current API is missing deregister, per-agent health details,
uptime tracking, evidence counts, and backend-level author filtering —

**THEN** implement these as a batch of R11 scope-bounded Codex handoffs,
each independently testable and deployable —

**BECAUSE** each gap blocks a specific overnight operational need:
- No deregister → stale agents accumulate, confusing Tickle
- No per-agent health → cannot distinguish live from zombie agents
- No uptime → cannot detect silent restarts
- No evidence count → cannot verify evidence accumulation
- No author filter in backend → Tickle can't efficiently query per-agent

## Scope

### Issues (R11 Handoffs)

| # | Title | Status | PSR | PUR | Commits |
|---|-------|--------|-----|-----|---------|
| #10 | ClosedChannelException cleanup | done | e-6f3ba8d1 | e-2fec7001 | 520604c |
| #16 | Tickle self-exclusion | done | (pre-mission) | retroactive | 4d7f876 |
| #17 | GET /api/alpha/agents/:id | done | (pre-mission) | retroactive | 610c85a |
| #18 | DELETE /api/alpha/agents/:id | done | e-d16c51a4 | retroactive | fac1779 |
| #19 | /health per-agent details | done | e-c54ae926 | retroactive | f3114b3 |
| #20 | :query/author in EvidenceQuery | done | e-1faabc85 | retroactive | f3114b3 |
| #21 | /health uptime + started-at | done | e-480944d1 | retroactive | a06a38b |
| #22 | /health evidence count | done | e-f32b2785 | retroactive | 020d876 |

### Pattern

All handoffs use **R11 (scope-bounded-handoff)**: explicit `:in`/`:out` files,
function signatures, test expectations, criteria checklist. The pattern was
selected because it gives Codex unambiguous scope boundaries and
machine-verifiable criteria, preventing drift.

### Agents

- **claude-1**: Mission design, issue creation, PSR/PUR emission, review
- **codex-1**: Implementation via autowake loop
- **tickle-1**: Liveness monitoring during execution

### Infrastructure

- `scripts/codex-autowake` — autowake loop with futonic evidence emission
- `scripts/tickle-start` — watchdog monitoring agent activity
- `make status` — monitoring dashboard

## Gates

### G0: API Surface Complete
All CRUD operations for agents exist (POST, GET, GET/:id, DELETE).
Health endpoint reports agents, sessions, evidence, uptime, per-agent details.
Evidence query supports author filtering at backend level.

### G1: Test Evidence
`clojure -X:test` passes with 0 failures. Each issue adds at least 1 test.
Total test count tracks upward across the mission.

### G2: Evidence Landscape
Every issue has a PSR in the evidence store (pattern selection recorded).
Every completed issue has a PUR (outcome recorded).
Autowake lifecycle events carry mission-id, pattern-id, commit-sha.

### G3: Operational
`make dev` + `make tickle` + `make codex-autowake` runs unattended.
Tickle detects stalls and pages. Codex processes issues from the queue.
`make status` shows live health at any time.

## Checkpoints

### Checkpoint 0: Federation + Tickle Smoke (2026-02-20 12:34Z)

Inter-Agency federation working. Proxy dispatch confirmed end-to-end
(Linode → laptop Codex). Tickle watchdog running standalone via HTTP
evidence backend. First Codex handoff (#14) completed.

**Test state:** 686 tests, 2230 assertions, 0 failures

### Checkpoint 1: Autowake Loop Operational (2026-02-20 13:00Z)

Codex autowake loop running. Issues #16, #17 completed by Codex.
GitHub issue queue with `codex` label driving task selection.
Issues closed on success, relabeled on failure.

**Test state:** 691 tests, 2245 assertions, 0 failures

### Checkpoint 2: Futonic Layer 0+1 (2026-02-20 13:18Z)

Evidence entries enriched with pattern-id, mission-id, commit-sha.
PSR evidence entries emitted for all queued issues. Evidence queryable
by tag (`?tag=psr`). Mission context in issue close comments.

**Test state:** 695 tests, 2267 assertions, 0 failures

### Checkpoint 3: Batch Completion (2026-02-20 ~13:30Z)

All 8 mission issues completed (6 by Codex, 2 pre-mission). Gate G0 (API
surface) met: CRUD agents, enhanced health, evidence author filtering. Gate G1
(test evidence) met: 700 tests, 0 failures. Evidence landscape write side
complete with PSRs, PURs, checkpoints for all issues.

**Test state:** 700 tests, 2292 assertions, 0 failures

### Checkpoint 4: Evidence Read Side (2026-02-20 ~14:00Z)

Codex autowake prompt now consumes evidence from the store before each cycle:
- PSR for current issue (pattern rationale, context, decision)
- Recent PURs from mission (outcomes, files touched — learning context)
- Mission health (agent count, uptime)

This closes the evidence loop: agents both emit and consume evidence.
Write side (Layers 0-4) + read side = full evidence landscape.
