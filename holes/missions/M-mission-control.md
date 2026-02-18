# Mission: Mission Control Peripheral

**Date:** 2026-02-18
**Status:** IDENTIFY (mission proposal)
**Blocked by:** None (mission peripheral complete, evidence landscape
wired, futon5 devmaps operational)

## Motivation

The mission peripheral (M-mission-peripheral, completed today) handles
*inside* a single mission: phase-gated cycles, obligation tracking,
evidence emission, state snapshots. But nothing manages the *portfolio*
of missions across repos.

Today's coordination infrastructure is hand-maintained and disconnected:

| What exists | Where | Problem |
|-------------|-------|---------|
| 17 mission files | `futon3c/holes/missions/` | Status in prose, not queryable |
| 11 wiring diagrams | `futon5/data/missions/` | Structural specs, no coverage map |
| Implementation inventory | `futon3c/holes/qa/` | Hand-maintained, drifts |
| Next-missions doc | `futon3c/holes/qa/` | Static list, no prioritization engine |
| Mana tracking | `futon5/data/nonstarter.db` | SQLite, no connection to missions |
| Alleycat scorecard | `futon3c/holes/missions/` | One-off validation, not continuous |

The evidence landscape can already receive mission state snapshots
(`:claim-type :observation`, `:ref/type :mission`). But nobody *reads*
them at the portfolio level. Nobody cross-references missions against
devmaps. Nobody surfaces what's blocked on mana versus what's actionable.

**The gap:** There is no peripheral for the question "what should we
work on next, and why?"

## Theoretical Anchoring

### 1. The Three Timescales

The futon stack operates at three timescales (social-exotype.edn,
coordination-exotype.edn, evidence-landscape-exotype.edn):

| Timescale | AIF Loop | What Happens |
|-----------|----------|-------------|
| Social (real-time) | Agency, peripherals, forum | Agents coordinate, hand off work |
| Task (fast) | Gate pipeline G5->G0 | Work gets validated and persisted |
| Glacial (slow) | Library evolution, canonicalization | Patterns emerge from accumulated evidence |

Mission control operates at the **boundary between task and glacial**.
Individual missions are task-timescale (they produce work). The portfolio
is glacial-timescale (it evolves as missions complete, patterns emerge,
and priorities shift). Mission control bridges these: it reads task-level
evidence (mission snapshots) and glacial-level structure (devmaps,
patterns) to make portfolio decisions.

### 2. Shared Constraint Inputs

The futon5 wiring diagrams identify three shared constraint inputs that
appear across all mission diagrams:

- **I-patterns** (`:config`, `:glacial`, constraint) — the pattern
  library. "How to do things well."
- **I-registry** (`:config`, `:slow`, constraint) — agent capabilities.
  "Who can do what."
- **I-missions** (`:config`, `:slow`, constraint) — mission specs.
  "What counts as success."

Mission control is the component that *manages* I-missions. Today this
constraint input is populated by hand (writing `.md` files). Mission
control makes it structural: missions as evidence entries, devmap
coverage as a queryable property, prioritization as a repeatable process.

### 3. Futonic Logic at the Portfolio Level

The futonic loop applies at every level of recursion. At the portfolio
level:

```
(futonic-loop
  (input   象  := portfolio state — missions, devmaps, mana, evidence)
  (choose  部  := select which missions to activate/create/park)
  (articulate 咅 := make prioritization explicit — war bulletin)
  (if (forms 鹽) := do available resources (mana) + mission specs compose?)
  (and (sense 香) := perceive portfolio health during review)
  (and (regulate 🔮) := stay within mana budget, respect dependencies)
  (then
    (act-within 未知 := allocate resources, scaffold missions)
    (evaluate 味 := did the portfolio improve? coverage increase?))
  (else
    (apply 捨 := park the review, record what's blocked)))
```

### 4. Devmap as Structural Specification

Futon5's wiring diagrams define *what should exist* as validated EDN
structures with ports, components, edges, and invariants (I1 boundary,
I3 timescale ordering, I4 exogeneity, I6 compositional closure). Each
component in a wiring diagram either:

- **Has a mission** — work is in progress or complete
- **Has no mission** — a gap in coverage
- **Is shaped but not wired** — shapes exist but no implementation

The difference between "what the devmap says should exist" and "what
missions have addressed" is the **roadmap**. Mission control makes this
difference computable.

### 5. Corneli (2014) Table 24 at the Portfolio Level

Table 24's entity grammar applies at the portfolio level:

- **X** (project) = the portfolio itself (all missions across repos)
- **P** (problem) = devmap gaps — components without missions
- **J** (conjecture) = "this mission will address this devmap component"
- **S** (solution) = mission proposals with obligations
- **H** (heuristic) = patterns for portfolio management (prioritization,
  resource allocation, dependency ordering)
- **E** (evidence) = mission state snapshots, completion evidence,
  mana records

### 6. Sospeso Protocol as Portfolio Constraint

The sospeso protocol (futon5 AGENTS.md) provides the resource model:
confidence `p` in `{0.3, 0.6, 0.8, 0.95}` determines mana cost.
Low-confidence missions donate more to the pool; high-confidence
missions consume less. Mission control reads the mana balance to
determine what's affordable and surfaces gated missions.

## What This Mission Produces

1. **Mission control peripheral** — a peripheral with tools for
   portfolio-level observation, prioritization, and resource allocation.
   Reads mission state snapshots from the evidence landscape, devmaps
   from futon5, and mana from nonstarter.db.

2. **Devmap coverage map** — a queryable structure showing which devmap
   components have missions, which missions are complete, which have
   gaps. The difference between devmap and missions is the roadmap.

3. **Portfolio state as evidence** — portfolio reviews emitted as
   evidence entries (`:claim-type :observation`, `:ref/type :portfolio`),
   making portfolio health inspectable from the evidence landscape.
   War bulletins become evidence, not markdown files.

4. **Mana-aware prioritization** — missions ranked by impact (devmap
   coverage, dependency unblocking) weighted by mana cost (sospeso
   confidence). "What gives us the most coverage per mana spent?"

5. **Cross-repo mission inventory** — automatic scanning of mission
   files across futon3c, futon5, and other repos, with status
   extraction and dependency tracking.

## Scope In

- Portfolio-level peripheral with tools for mission inventory, devmap
  coverage, mana queries, prioritization, and bulletin emission
- Cross-repo mission scanning (futon3c, futon5, futon3a, futon3b)
- Devmap-to-mission coverage analysis (futon5 wiring diagrams)
- Mana balance queries (futon5 nonstarter.db)
- Portfolio state snapshots as evidence entries
- War bulletin emission as evidence entries
- Integration with the existing mission peripheral (reads its snapshots)

## Scope Out

- Modifying the mana/sospeso system itself — that stays in futon5
- Automatic mission execution — mission control prioritizes, agents execute
- Visualization (Arxana/futon4) — mission control produces data, viewers render
- Cross-mission learning queries — future evidence landscape feature
- Automated devmap generation — devmaps are authored in futon5
- Multi-agent resource negotiation — future Agency concern

## Derivation Path

1. **IDENTIFY** — this document (mission proposal)
2. **MAP** — survey existing portfolio tracking: futon5 devmap structure,
   nonstarter mana system, mission file conventions, QA inventory format,
   evidence landscape query API
3. **DERIVE** — design the mission control tools and domain shapes;
   decide whether to use the cycle machine or a simpler peripheral
   structure; implement cross-repo scanning and devmap coverage
4. **ARGUE** — justify the tool set, the relationship to the evidence
   landscape, and the mana integration approach
5. **VERIFY** — validate portfolio queries against real mission data;
   verify devmap coverage is correct; test mana integration
6. **INSTANTIATE** — wire into the peripheral registry, produce first
   portfolio review as evidence

## Source Material

| Source | What We Take |
|--------|-------------|
| `futon5/data/missions/*.edn` | Devmap structure: ports, components, edges, invariants |
| `futon5/src/futon5/ct/mission.clj` | Validation functions, composition, port types |
| `futon5/src/nonstarter/db.clj` | Mana operations: balance, summary, record |
| `futon5/AGENTS.md` | Sospeso protocol: confidence bins, donation formula |
| `futon3c/holes/missions/M-*.md` | Mission file conventions: status, blocked-by, derivation step |
| `futon3c/holes/qa/implementation-inventory.md` | Component status tracking: wired/shaped/referenced |
| `futon3c/holes/qa/next-missions.md` | Tech debt + candidate missions format |
| `futon3c/src/futon3c/peripheral/cycle.clj` | Generic cycle machine (if used) |
| `futon3c/src/futon3c/peripheral/mission.clj` | Mission state snapshot format |
| `futon3c/src/futon3c/evidence/store.clj` | Evidence query API |
| `futon5/data/missions/f3c-grounding-functor.edn` | nLab grounding for mission types |

## Key Design Decisions (To Be Resolved)

### Cycle machine or simpler peripheral?

The mission peripheral uses the 9-phase cycle machine because individual
missions follow a development cycle. Mission control might not — a
portfolio review is more like: survey → assess → prioritize → allocate →
report. But the cycle machine's phases could map:

| Cycle Phase | Portfolio Meaning |
|------------|------------------|
| observe | Survey missions, devmaps, mana, evidence |
| propose | Identify gaps, propose prioritization |
| execute | Create/update missions, allocate mana |
| validate | Check coherence (no over-commitment, dependencies met) |
| classify | Assess portfolio health |
| integrate | Update portfolio state in evidence landscape |
| commit | Persist portfolio state |
| gate-review | Sanity check portfolio decisions |

This reuses the generic engine and gets Table 25 auto-tagging for free.
The alternative is a simpler peripheral with custom dispatch. To be
resolved in MAP/DERIVE.

### Mission file scanning: parse or evidence?

Two approaches to building the mission inventory:

**A) File scanning** — read `.md` files, extract status from YAML-like
headers. Works immediately but is fragile (parsing prose) and doesn't
benefit from the evidence landscape.

**B) Evidence-first** — missions register themselves as evidence entries
when created/updated. The inventory is a query. Requires missions to
emit evidence on status change — which the mission peripheral already
does via snapshots. But existing missions (pre-peripheral) would need
backfill.

**C) Hybrid** — scan files for discovery, use evidence for active
missions. File scanning catches legacy missions; evidence queries catch
missions running through the peripheral. To be resolved in DERIVE.

### Devmap integration: read-only or bidirectional?

Mission control could:
- **Read-only** — query devmaps, report coverage. Devmaps stay in futon5.
- **Bidirectional** — propose devmap updates (new components, coverage
  annotations) back to futon5.

Read-only is simpler and respects the repo boundary. Bidirectional
requires cross-repo writes. Start with read-only, extend later.

### Mana integration: query or manage?

- **Query** — read mana balance, surface gated missions. Mana management
  stays in futon5.
- **Manage** — allocate mana to missions, record sospeso events.
  Requires cross-repo writes to nonstarter.db.

Start with query. Mission control tells you what's affordable; the
agent decides whether to spend. Mana allocation could be added later.

## Relationship to War Room

The war room pattern (from futon3's `holes/war-room.md`) was the
predecessor to mission control. War rooms tracked decisions (WR-*),
bulletins, and mission status in a single markdown file.

Mission control supersedes this by making war room artifacts structural:
- **War bulletins** become evidence entries with portfolio observations
- **Decisions** become evidence entries with argumentation tags
- **Status tracking** becomes evidence queries over mission snapshots

The war room markdown file becomes unnecessary once mission control
emits portfolio evidence. The same information is available via
`?claim-type=observation&subject-type=portfolio`.

## Decision Log

- [ ] Cycle machine vs simpler peripheral structure
- [ ] Mission file scanning: parse files vs evidence-first vs hybrid
- [ ] Devmap integration: read-only vs bidirectional
- [ ] Mana integration: query-only vs manage
- [ ] Portfolio state shape (what fields, what evidence structure)
- [ ] Cross-repo access pattern (shell out? shared filesystem? API?)
- [ ] Whether to backfill existing missions as evidence entries
