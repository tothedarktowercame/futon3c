# Mission: Stack Inhabitation
Status: parked

**Date:** 2026-04-10
**Status:** IDENTIFY
**Owner:** Joe + Claude (cross-stack)
**Cross-ref:** M-repl-wins-over-cli (entry ceremony, TESTING), M-hypergraph-operator
  (futon5a, IDENTIFY), E-sorry-initialization, E-Vasilopita, E-repl-evidence-turns,
  E-candidate-queue-upsampling, E-sorry-lifecycle-maintenance, E-evidence-explorer
**Repos:** futon0 (analysis/excursions, joe-hud), futon3c (Agency, REPL, evidence),
  futon3a (pattern retrieval), futon4 (Arxana browser, candidate invariants),
  futon5a (hypergraph operator, pocketwatch)
**Patterns:** surface-earns-inhabitation, inhabitation-feeds-evolution

## 1. IDENTIFY

### Motivation

The futon stack has extensive infrastructure: Agency with multi-agent routing,
evidence stores, pattern libraries, Arxana hypergraph browser, candidate
invariant queues, mission documents, excursion logs, a Bayesian model (Click 1),
and as of today a working Emacs REPL with context retrieval notifications and
an evidence landscape browser.

The discrepancy: most of this infrastructure is **uninhabited**. The Candidate
Invariant queue has 65 entries but no processing pipeline. The excursions are
explorations without missions. The evidence browser just started showing data
today. The Click model has one edge (commit-time -> recording) but no
connection to the hypergraph. The pattern library retrieves patterns per turn
but doesn't feed them back into decisions.

The two core patterns diagnose this precisely:

**surface-earns-inhabitation:** A productive interface that nobody uses is dead
infrastructure. The surface must earn inhabitation by being less friction than
the primitive alternative. Uninhabited infrastructure is dead infrastructure.

**inhabitation-feeds-evolution:** Inhabiting a peripheral generates the data
that evolves the peripheral. Without inhabitation, the Baldwin loop starves at
the source. A peripheral that generates no data because it's not inhabited
cannot improve.

The mission is to establish a **portfolio of inhabitation** — identify every
surface in the stack that should be inhabited, make each one earn its
inhabitation (surface-earns-inhabitation), and wire the evidence from
inhabitation into the evolution loop (inhabitation-feeds-evolution).

### Theoretical anchoring

- **Active Inference (futon2):** Each surface is a peripheral in the AIF loop.
  Inhabitation = the agent enters the peripheral and operates within its
  constraints. The evidence generated is the observation that updates beliefs.
- **Baldwin effect (inhabitation-feeds-evolution):** Fast loop (social): agent
  inhabits, produces evidence, encounters friction. Slow loop (glacial):
  designer reads evidence, reshapes peripheral. Without the fast loop, the
  slow loop has no data.
- **Sorry topology (E-sorry-initialization):** Each uninhabited surface is a
  typed hole. The sorry has a type (what value is missing) and a maintenance
  cost (what it costs to leave uninhabited). The portfolio ranks sorrys by
  (maintenance cost * probability of closing).
- **Cyberant foraging (futon2):** The ant must deposit, not just forage. Joe
  must inhabit the depositing surfaces (evidence browser, candidate queue,
  mission control), not just the foraging surfaces (REPL chat, code editing).
- **JSDQ constraint violations:** cargo-implies-depositing applies here too.
  The stack has cargo (built infrastructure) that needs depositing (inhabitation).

### Scope in

**Surfaces to inhabit (the portfolio):**

| Surface | Current state | Inhabitation target |
|---------|---------------|---------------------|
| Emacs REPL (cr/cx) | Inhabited (today!) | Maintain — entry ceremony works |
| Evidence browser (Arxana) | Just connected today | Browse sessions as conversations, not logs |
| Candidate invariants (65) | Unprocessed queue | Triage pipeline: promote, defer, or compost |
| Context retrieval (notifications) | Working, post-hoc | Wire into pre-hoc pattern selection |
| Context lookback (*context* HUD) | Working | Queryable (E-evidence-explorer) |
| Hypergraph operator (futon5a) | IDENTIFY phase | Clicks + Ticks against sorry topology |
| Joe HUD (futon0) | Click 1 operational | Connect to evidence turns, hypergraph observables |
| Mission control | Exists but uninhabited | Surface tensions, compute coverage, generate missions |
| Pattern library (futon3a) | Retrieval works | Feedback loop: pattern use -> pattern evolution |
| Excursion pipeline | 6 excursions, mixed status | Convert to missions or close explicitly |

**The portfolio invariant:** Every surface in the table either (a) is
inhabited and generating evidence, or (b) has an explicit sorry with a
maintenance cost estimate and a plan to close or decommission.

### Scope out

- Building new surfaces (enough exist — the problem is inhabitation, not creation)
- Full AIF loop simulation (use the vocabulary, don't rebuild the engine)
- Revenue / commercial work (that's JSDQ territory, not stack territory)
- Rebuilding futon1a (the evidence store works, use it)

### Completion criteria

1. Every surface in the portfolio table has a status: inhabited, planned, or
   explicitly deferred with maintenance cost
2. The Candidate Invariant queue has a triage pipeline: at least 20 of 65
   candidates processed (promoted, deferred, or composted)
3. The evidence browser renders sessions as conversations (not server logs)
   and supports copy-to-clipboard of evidence URIs
4. At least one Click is operational against the hypergraph (not just
   commit-time -> recording)
5. At least one Tick fires a notification when a sorry constraint is violated
6. The excursion pipeline is triaged: each excursion is either promoted to
   a mission, merged into this mission, or explicitly parked
7. Evidence from all inhabited surfaces flows to the evidence store and is
   browsable in Arxana
8. A war bulletin documents the findings and strategic assessment

### Relationship to other missions

| Mission | Relationship |
|---------|-------------|
| M-repl-wins-over-cli | Prerequisite (TESTING) — entry ceremony for the primary surface |
| M-hypergraph-operator | Sub-mission — Clicks and Ticks against the sorry topology |
| M-self-representing-stack | Parent vision — the stack that models itself |
| M-portfolio-inference | Downstream — uses inhabitation evidence as signal |
| M-structural-law | Related — candidate invariants are structural laws |

### Source material

| Source | Role |
|--------|------|
| futon3/library/peripherals/surface-earns-inhabitation.flexiarg | Core pattern |
| futon3/library/peripherals/inhabitation-feeds-evolution.flexiarg | Core pattern |
| futon0/analysis/excursions/*.md | Prior explorations (6 excursions) |
| futon4/futon-stack-invariant-model.edn | Candidate invariant data |
| futon3c/docs/repl-parity-claims.edn | Ratchet claims for REPL surface |
| futon5a/data/alignment.edn | Hypergraph sorry topology |
| futon5a/holes/missions/M-hypergraph-operator.md | Sub-mission spec |
| futon3c/holes/missions/M-repl-wins-over-cli.md | Entry ceremony mission |
| futon0/analysis/notebooks/recording_model.clj | Click 1 template |

### Owner and dependencies

- **Joe:** Direction, testing, inhabitation (the human in the loop)
- **Claude (REPL agent):** Implementation, wiring, evidence pipeline
- **Cross-stack:** Changes touch futon0, futon3c, futon3a, futon4, futon5a

## 2. MAP — _pending_

Survey questions:
- Q1: For each surface in the portfolio, what is the current entry friction?
- Q2: Which excursions should be promoted to sub-missions of this mission?
- Q3: What is the triage protocol for the 65 candidate invariants?
- Q4: What evidence would demonstrate that inhabitation-feeds-evolution is
  working (the Baldwin loop is producing data that improves surfaces)?

## Checkpoint 1 — 2026-04-10

**What was done:**

- REPL surface inhabited: `cr`/`cx` entry ceremony working, session resume,
  evidence logging from first turn, context retrieval notifications
- Evidence browser connected: `arxana-evidence-server` auto-discovers Agency,
  sessions render as conversations (not server logs), deduplication, evidence
  URIs with identifiers, RET opens entry detail
- Mission Control browser: `GET /api/alpha/missions` endpoint (98 missions),
  `arxana-browser-missions.el` with portfolio view (sorted by status), by-status
  grouping, drill-down, RET opens mission .md file. Title extraction added.
- `*context*` HUD panel: lookback buffer showing last 10 turns' pattern
  retrieval results, projected to HUD frame
- HUD frame: dynamic layout with all available buffers (agents, processes,
  context, invoke), side-window suppression via advice
- War Bulletin 6 + WR-4 (inhabit before building)
- M-stack-inhabitation mission spec (this document)
- M-hypergraph-operator mission spec (futon5a)
- E-evidence-explorer excursion logged

**Status of completion criteria:**

1. Portfolio table status — partial (REPL: inhabited, Evidence: inhabited,
   Missions: inhabited, Context: inhabited. Remaining: candidate invariants,
   hypergraph operator, joe-hud, mission control semantic layer, pattern
   library feedback, excursion pipeline)
2. Candidate invariant triage — not started (0/65)
3. Evidence browser as conversations — done
4. Hypergraph Click — not started
5. Sorry constraint Tick — not started
6. Excursion triage — not started
7. Evidence flowing from all surfaces — partial (REPL yes, others not yet)
8. War bulletin — done (Bulletin 6)

**What this checkpoint reveals:**

The surfaces are lexically inhabited — you can browse evidence, missions, and
invariants as separate flat views. But they are not semantically connected.
The self-representing stack thesis (M-self-representing-stack) requires that
browsing a mission shows its evidence, its invariants, its tensions, and its
connections to other missions. Currently these are three separate inventories
in the same browser, not a hypergraph.

M-self-representing-stack was marked complete at the design level (DERIVE+ARGUE
done) but the INSTANTIATE phase — making MC artifacts navigable as hyperedges
in Arxana — was never finished. The lexical views we built today are the
infrastructure for that instantiation, but the semantic wiring (mission →
evidence produced, mission → invariants it upholds, mission → missions it
depends on) is the gap.

**The diagnostic:** We have nodes (missions, evidence entries, invariants).
We don't have edges (which mission produced which evidence, which invariant
is upheld by which mission, which sorry is addressed by which action). The
Arxana hypergraph model supports edges natively — the missing piece is
populating them from the data we already have.

**Next:** Either push into MAP (survey the edge types needed) or address the
semantic gap directly by connecting evidence sessions to missions and
invariants to mission completion criteria.

## 3–7. _Pending MAP_
