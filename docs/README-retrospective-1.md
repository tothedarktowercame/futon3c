# Futon Stack Retrospective #1 — 2026-02-21

> What we tried, what worked, what failed, and what next.
> Based on git log analysis across futon3, futon3a, futon3b, and futon3c.

## Timeline

| Repo | Commits | Span | Focus |
|------|---------|------|-------|
| **futon3** | 747 | Nov 2025 → Feb 2026 | The original monolith |
| **futon3a** | 33 | Jan → Feb 2026 | Pattern search + querying |
| **futon3b** | 28 | Feb 8-10 2026 | Gate pipeline (3 intense days) |
| **futon3c** | 155 | Feb 9-21 2026 | Real-time coordination |

## Phase 1: The Monolith (futon3, Nov 2025 – Jan 2026)

Everything lived in futon3. Transport, patterns, AIF, MUSN, fucodex, fuclaude,
hypertext, embeddings, HUD, labs, forum, drawbridge, bells. ~500 commits in
January alone.

### What worked

- **Pattern library + sigils** — the vocabulary stuck and moved into all three
  child repos
- **fucodex/fuclaude wrapper scripts** — established the CLI invocation pattern
  still in use today
- **IRC as human surface** — this never stopped being the right idea
- **AIF as pattern selection engine** — the concept held, even if the
  implementation kept shifting

### What failed / got abandoned

- **MUSN stream** (Jan 12) — experimental Codex-to-MUSN translator, superseded
  quickly
- **Hypertext v0** (Jan 6) — plumbing and demo scripts, never went further
- **Bell architecture** — `agency/bell` over WS/HTTP. Replaced by dispatch
  relay in futon3c
- **Forum** (HTTP :5050, WS :5055) — the whole service. Replaced by evidence
  landscape
- **The monolith itself** — futon3 was doing too much. Led to the three-way
  split

## Phase 2: The Split (Jan-Feb 2026)

futon3 budded into three organisms:

| Repo | Focus | Outcome |
|------|-------|---------|
| **futon3a** | Pattern search, notions, compass, meme layer | Stable, quiet. 33 commits. Does its job. |
| **futon3b** | Gate pipeline (G5-G0), L1 glacial loop | Intense 3-day sprint (Feb 8-10). Works. Mostly dormant since. |
| **futon3c** | Real-time coordination | Where all the action moved. 155 commits in 13 days. |

### What worked about the split

- Each repo has a clear concern and timescale
- futon3b could be built in 3 days because the scope was tight
- futon3c could move fast without breaking pattern search or gates

### What didn't work

- futon3b has been dormant since Feb 10. The gate pipeline exists but nothing
  flows through it yet. The `bridge/submit-to-gates!` wiring in futon3c exists
  but hasn't been exercised.
- futon3a's notions search works but is used ad-hoc, not integrated into any
  automated flow.

## Phase 3: futon3c — The Sprint (Feb 9-21)

155 commits in 13 days, mission-driven development.

### Missions completed

- **M-agency-refactor** — single registry, typed IDs, atomic state transitions
- **M-peripheral-model** — mode shapes, hop protocol, peripheral specs
- **M-forum-refactor -> Evidence Landscape** — replaced Forum entirely
- **M-peripheral-behavior** — the backward-arrow verification framework
- **M-dispatch-peripheral-bridge** — route selection + peripheral dispatch
- **M-transport-adapters** — HTTP + WS transports wired to live registry
- **M-IRC-stability** — 6 failure modes fixed
- **M-mission-control** — portfolio observation peripheral

### Missions in progress

- **M-proof-peripheral** — proof cycle machine works, FM-001 bootstrapped, but
  no full cycle run yet
- **ALFworld peripheral** — deterministic runner works, 10 patterns discovered,
  but peripheral integration is partial

### The 4 attempts at cross-machine agent connectivity

1. **Federation proxy** (Feb 20) — `announce!` sends agent registrations to
   peers, peer creates proxy invoke-fn. Bug: proxy was hitting `/dispatch`
   (doesn't exist). Bug: namespaced capabilities were truncated. Both fixed
   Feb 21 in Codex's branch.

2. **External WS bridge scripts** (Feb 21 morning) —
   `scripts/codex_ws_invoke_bridge.clj`, separate JVM process connects back via
   WS. Works but adds unnecessary overhead when on the same machine.

3. **In-JVM WS bridge connecting to self** (Feb 21) —
   `start-codex-ws-bridge!` in dev.clj. Same JVM connecting to its own WS
   endpoint. Correctly identified as unnecessary ("why not just run inline?").

4. **Outbound WS from laptop + inline invoke** (Feb 21, current) — Laptop
   dials outbound to linode, registers codex-1 there. Linode has inline
   invoke-fns with evidence emission. Under test.

Each attempt uncovered real bugs (dead endpoint, broken serialization, no
evidence, no logging) that had to be fixed before the next could work.

## What's Working (Feb 21)

- **Registry** — single atom, typed IDs, atomic transitions. Solid.
- **Evidence landscape** — XTDB-backed, queryable, replaces Forum. Solid.
- **Blackboard** — Emacs buffer projection. Works for mission, agents, invoke
  status.
- **IRC transport** — dispatch relay with mention-gating. Works.
- **Drawbridge /eval** — the agent-to-agent bridge. Works.
- **Peripheral model** — explore/edit/test/deploy/reflect envelopes with hop
  protocol. Works.
- **Cycle machine** — generic 9-phase engine. Proven on mission + ALFworld.
- **Proof peripheral** — 15 tools, ledger, DAG, canonical versioning. Built,
  untested on a real problem.

## What's Broken or Untested (Feb 21)

- **Cross-machine Codex invoke via IRC** — being tested, 4th attempt
- **Evidence emission during invocations** — just committed, untested in
  production
- **FM-001 proof cycle** — state exists but no cycles have run
- **Gate pipeline flow** — futon3b gates exist but nothing submits to them
- **Tickle watchdog** — implemented but not running (needs `start-tickle!`)
- **ALFworld as peripheral** — runner works standalone, not tested through the
  peripheral dispatch

## What Next

### Immediate (today/tomorrow)

1. Get Codex IRC round-trip confirmed working
2. Re-introduce claude-1 with inline invoke + evidence
3. Verify evidence entries are queryable after an invoke

### Short-term (this week)

4. Run FM-001 through one full SPEC-to-FALSIFY cycle via the proof peripheral
5. Start Tickle so stalled agents get paged automatically
6. Add registry watch so Emacs `*agents*` buffer stays current

### Medium-term

7. Wire gate pipeline: proof peripheral results -> futon3b G5 submission
8. ALFworld as the second peripheral test (after proof)
9. Feed evidence from invocations into mission-control portfolio reviews
