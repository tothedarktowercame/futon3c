# Mission: Structural Law — Universal Invariants as Self-Representing Stack Layer

**Date:** 2026-03-10
**Status:** MAP
**Cross-ref:** M-self-representing-stack (predecessor), M-three-column-stack (three
columns), M-fulab-logic (domain-specific invariants), M-invariant-violations (ledger)
**Owner:** futon3c (core.logic infrastructure), with dependencies on futon4 (Arxana
hypergraph for Column 2 representation), futon1a (evidence store)

## Motivation

We now have five operational core.logic invariant layers:

| Domain | File | Status |
|--------|------|--------|
| Portfolio | `portfolio/logic.clj` | Operational (predecessor) |
| Tickle | `agents/tickle_logic.clj` | Operational, 0 violations |
| Agency | `agency/logic.clj` | Operational, 5 violations cataloged |
| Proof | `peripheral/proof_logic.clj` | Operational, tested |
| Codex Code | `agents/codex_code_logic.clj` | Operational, tested |

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

## Theoretical Anchoring

This mission sits at the intersection of several prior lines of work:

- **Self-representing stack:** `M-self-representing-stack` established the idea
  that the stack should describe and check itself, not only host ad hoc local
  tools. Structural law is one such self-description layer.
- **Three-column stack:** `M-three-column-stack` provides the broader frame in
  which structural law can eventually project across knowledge, development,
  and code reflection rather than staying trapped in one domain vocabulary.
- **Domain-specific invariant layers:** `portfolio/logic.clj`,
  `agents/tickle_logic.clj`, `agency/logic.clj`, and
  `peripheral/proof_logic.clj` are evidence that recurring law families already
  exist in operational form, even if they have not yet been normalized into one
  shared inventory.
- **Adjacent possible / excursion discipline:** the mission inherits the idea
  that a lawful system should constrain action without collapsing discovery.
  Structural law therefore has to account for both conformance and legitimate
  excursion into adjacent possible structure.

## Scope In

- enumerate the currently evidenced meta-invariant families across live FUTON
  domains;
- classify laws by scope, status, kind, home, evidence, and enforcement
  surface in one canonical inventory;
- incorporate mission-layer evidence so Missions stop being the major missing
  domain in the inventory;
- identify firm exemplars for each active family and track candidate families
  that are real pressures but not yet operational;
- prepare the next implementation seams: `mission_logic.clj`, meta-invariant
  completeness checking, and later structural-law combinators.

## Scope Out

- full implementation of every domain logic layer before the inventory is
  useful;
- promoting every useful gate into foundational always-on law without runtime
  evidence;
- building a complete Invariant Peripheral cycle machine during Phase 1;
- follow-on consumers such as `M-aif-head`, except insofar as this mission
  needs to name them as downstream dependencies;
- broad repo-cleanup or workspace-governance automation beyond what is needed
  to classify candidate law families.

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

Note from the 2026-03 repo cleanup pass: we also surfaced a candidate
meta-invariant family not yet covered by the current table:

- work should happen in atomic, inspectable units with clear custody
  (canonical repo, explicit checkout/check-in, artifact locality, bounded
  archaeology).

This is not yet an operational invariant family. Treat it as a candidate
to test during Phase 1 rather than a settled abstraction.

Also note: `futon1a` already contributes recovered operational invariants
that are not hypothetical at all: startup contracts, layered error
hierarchy, and authorization/identity discipline. Phase 1 should treat
those as existing evidence for the inventory, not as future invention.

Phase 1 should also distinguish foundational invariants from optional
validation gates. `futon4` contributes real Reazon-backed checks, but
because they are configurable they are evidence of useful gate design,
not yet evidence of always-on substrate law in the same sense as
`futon1a`.

Phase 1 should also classify each discovered law by registry dimensions
such as scope, status, kind, home, evidence, and enforcement surface.
Otherwise the inventory cannot distinguish local operational laws,
bypassable laws, candidate stack laws, and violated mission claims.

Another useful note from this pass: there are multiple dimensions of the
"adjacent possible." One important dimension is defined by peripherals
themselves. A peripheral is a constrained action envelope: it presents a
small lawful action surface and omits many actions that remain possible
in reality. In this sense peripherals are already invariant-shaped. A
choose-your-own-adventure book does not offer "go get a glass of
lemonade" even though that action exists in the wider world; it offers
"turn to page 74" or "turn to page 33." Structural law should eventually
account for these constrained adjacent-possible surfaces, not only for
explicit validation checks after the fact.

Cross-reference: this connects directly to `M-portfolio-inference`, which
defines the adjacent possible as a computable boundary between what exists
and what is structurally enabled, and to the Futon4 docbook note on
`portfolio/logic.clj`, where `adjacento` is the concrete query for this
boundary. Structural law should eventually relate these two readings:
adjacent possible as relational readiness, and adjacent possible as
peripheral action-envelope.

One more distinction matters here: the Futon stack is not trying to
"enforce the script" in the sense of forcing all work to stay inside the
current mission or devmap wording. That is exactly why the stack also
has the notion of an Excursion as a datatype. Missions name intended
through-lines; excursions name lawful departures into the adjacent
possible when the work reveals something worth following that was not
already explicit in the mission text. Structural law therefore has to
support constrained discovery, not only conformance. A good invariant
surface should make some moves possible, others impossible, and still
leave room for legitimate excursion when the adjacent possible reveals
something real.

### Control Posture for Phase 1

We now have enough material to stop treating Phase 1 as a vague survey
and start treating it as a control problem. The point of IDENTIFY is not
to spin out every interesting follow-on. The point is to establish one
stable law inventory that later work can consume without re-opening the
foundational question every time.

Current input artifacts:

- `docs/structural-law-inventory.sexp` — the canonical working draft of
  the inventory itself. This is the primary Phase 1 output and should be
  treated as the main registry seed, not as an optional side note.
- `futon7/provisional-ledger.md` — auxiliary context about repo role,
  adjacent-system study, public/private boundary, and comparative
  discipline. This is useful evidence for candidate families such as
  repo-role-clarity, artifact-custody, and contextual legibility, but it
  is not itself the law registry.

Immediate control target:

1. One canonical inventory of operational, candidate, and violated laws.
2. Clear separation between laws evidenced now and interesting but still
   downstream extrapolations.
3. A bounded follow-up queue, so new missions consume structural law
   rather than redefining it ad hoc.
4. At least one concrete next implementation seam, but no premature
   attempt to build the whole organism before the law surface is stable.

What counts as "control" at the M-structural-law level:

- the inventory is the authoritative source for the law families and
  their classification;
- follow-on missions cite the inventory rather than silently introducing
  competing law vocabularies;
- contextual notes, excursions, and adjacent-system comparisons are
  explicitly marked as evidence or pressure, not confused with the law
  registry itself;
- the mission can say, in a disciplined way, which questions belong to
  IDENTIFY and which are valid but downstream.

This matters because there are already plausible follow-ons. For
example, `futon2/holes/M-aif-head.md` is a legitimate downstream
consumer of structural law: it wants the Mission Peripheral to consult a
generative model of invariants. But that mission should not become part
of Phase 1 by drift. It depends on structural law being controlled first.

So the discipline for the rest of Phase 1 is:

- keep strengthening `structural-law-inventory.sexp` as the canonical
  registry seed;
- use `provisional-ledger.md` and similar notes as evidence for pressure
  and boundary conditions;
- record follow-ons like `M-aif-head` as downstream consumers or
  spinoffs;
- do not let the existence of a compelling next architecture substitute
  for finishing the invariant inventory.

## Source Material

- `futon3c/src/futon3c/portfolio/logic.clj`
- `futon3c/src/futon3c/agents/tickle_logic.clj`
- `futon3c/src/futon3c/agency/logic.clj`
- `futon3c/src/futon3c/peripheral/proof_logic.clj`
- `futon3c/src/futon3c/agents/codex_code_logic.clj`
- `futon3c/src/futon3c/peripheral/mission.clj`
- `futon3c/src/futon3c/peripheral/mission_backend.clj`
- `futon3c/src/futon3c/peripheral/mission_shapes.clj`
- `futon3c/test/futon3c/agents/codex_code_logic_test.clj`
- `futon3c/test/futon3c/peripheral/mission_test.clj`
- `futon3c/test/futon3c/peripheral/mission_backend_test.clj`
- `futon3c/docs/structural-law-inventory.sexp`
- `futon7/provisional-ledger.md`
- `futon4/holes/mission-lifecycle.md`
- `futon2/holes/M-aif-head.md` as a downstream consumer reference, not Phase 1
  scope

### Handoff 1.1: Mission lifecycle invariant layer

The richest untapped domain. Missions have phases (IDENTIFY → MAP → DERIVE →
ARGUE → VERIFY → INSTANTIATE), required outputs per phase, dependencies between
missions, status discipline (blocked/active/complete). We've done dozens of
missions — this is our best evidence source.

Before `mission_logic.clj` is written, the current Mission Peripheral should be
treated as an evidence source rather than a blank target domain. The mission
layer already binds several laws in live code: blocker existence at cycle
start, linear phase ordering, required outputs per phase, obligation status
discipline, and save-time state snapshots.

Just as importantly, some mission checks are not yet foundational law. DAG
acyclicity is currently an explicit query/gate surface rather than a mandatory
write-time rejection, and GF/GD documentation checks live behind `:gate-check`
rather than an unavoidable substrate path. `mission_logic.clj` should preserve
that distinction instead of silently upgrading gates into foundational law.

- `:in` — existing `*_logic.clj` files (pattern); `src/futon3c/peripheral/mission.clj`;
  `src/futon3c/peripheral/mission_backend.clj`;
  `src/futon3c/peripheral/mission_shapes.clj`;
  `test/futon3c/peripheral/mission_test.clj`;
  `test/futon3c/peripheral/mission_backend_test.clj`;
  `futon4/holes/mission-lifecycle.md`; sample `M-*.md` files
- `:precondition` — `docs/structural-law-inventory.sexp` records the already
  operational mission-layer laws before the logic layer tries to generalize
  them
- `:out` — `src/futon3c/peripheral/mission_logic.clj` + test
- Invariants to encode first: phase ordering, required outputs, blocker
  consistency, status discipline
- Keep separate for now: DAG acyclicity and GF/GD-style documentation gates,
  unless runtime enforcement is strengthened first

### Handoff 1.2: Verify meta-invariant completeness

This handoff should run alongside Handoff 1.1 rather than waiting for every
domain logic layer to be finished. The inventory itself is already useful as an
operational control surface: it tells us which laws are firm, which are only
candidate, which are gates, and which repos provide the strongest exemplars.

So Handoff 1.2 is not just a retrospective diff after `mission_logic.clj`
lands. It is the beginning of a read-heavy invariant registry surface:
classification, exemplar selection, completeness checking, and promotion
pressure from candidate law to operational law. In that limited sense it is a
proto-Invariant Peripheral, but not yet a cycle machine. The canonical source
for that surface remains `docs/structural-law-inventory.sexp`.

As `mission_logic.clj` comes online, diff the five existing domain logic files
plus the new mission logic file. At that point there should be six domain
logic files in view. Are
there invariants in any domain that don't map to a meta-invariant? Are there
candidate families in the inventory that now have enough firm exemplars to
promote? If so, the meta-invariant set or its classifications need extending.

- `:in` — the five existing `*_logic.clj` files plus
  `src/futon3c/peripheral/mission_logic.clj`
- `:out` — `docs/structural-law-inventory.sexp` (enumeration + mapping table,
  including cleanup-derived candidate invariants)
- `:working-products` — family-to-domain mapping table; exemplar list for each
  live family; gap list distinguishing "missing implementation" from "missing
  vocabulary"
- `:discipline` — at least one firm exemplar per family before using that
  family as a steering abstraction for follow-on work
- `:follow-on` — if the registry surface becomes operationally central, promote
  it into a dedicated Invariant Peripheral later rather than inventing one
  prematurely now

## Phase 2: MAP — Survey Existing Structural-Law Surfaces

**Goal:** inventory the infrastructure, data artifacts, and existing projection
surfaces that structural law can already use, before deciding what must still
be built.

### Infrastructure Inventory

- **Live invariant logic layers:** five `core.logic` projections already exist:
  `portfolio/logic.clj`, `agents/tickle_logic.clj`, `agency/logic.clj`,
  `peripheral/proof_logic.clj`, and `agents/codex_code_logic.clj`. Each uses
  the same broad pattern:
  snapshot/build-db → logic relations/goals → query-violations.
- **Mission-layer pre-logic surface:** the Mission Peripheral already enforces
  blocker existence, phase order, required outputs, status discipline, and
  save-time snapshots in `mission.clj`, `mission_backend.clj`, and
  `mission_shapes.clj`, even though `mission_logic.clj` does not yet exist.
- **Evidence substrate:** `futon3c.evidence.backend/EvidenceBackend` already
  provides append/get/exists/query/forks/delete/all; `evidence/store.clj`
  provides the public append/query/reply-chain/fork surface on top of atom or
  XTDB-backed persistence.
- **Peripheral evidence emission:** `peripheral/cycle.clj` can already emit
  step evidence plus snapshot evidence via domain snapshot hooks. Mission and
  Proof both use this surface.
- **Portfolio review / backfill surface:** `mission_control_backend.clj`
  already backfills mission inventory into `EvidenceEntry` form, computes
  portfolio diffs from review snapshots, and exports typed tensions for
  hyperedge creation.
- **Hypergraph / enrichment surface:** `portfolio/logic.clj` already models
  hyperedges and invariants relationally; `enrichment/query.clj` already queries
  futon1a hyperedges by endpoint or type over HTTP.
- **Browser surface:** futon4 already has Arxana browser, docbook export,
  relation/tail browsers, and a web evidence viewer. Structural-law output does
  not yet feed those surfaces directly, but the browsing substrate exists.

### Existing Data Inventory

- `docs/structural-law-inventory.sexp` currently contains 9 operational
  families, 8 candidate families, and repo seeds for 8 repos. It also now
  carries a 9-entry operational family map and an 8-entry candidate-family
  watchlist for Handoff 1.2.
- Local evidence ledger artifacts already exist in repo form:
  `docs/mission-evidence.edn` contains 6 evidence entries and
  `docs/wiring-evidence.edn` contains 7.
- Persisted mission-state examples already exist under
  `data/mission-state/`: `M-dispatch-demo.edn` and `M-gauntlet-p3.edn`.
  These are concrete mission-domain state artifacts, not hypothetical shapes.
- Mission Control already knows how to backfill mission inventory into
  evidence-entry form and how to compare stored portfolio review snapshots.
- Futon1a already exposes snapshot save/restore surfaces used by futon4, and
  futon3c already expects hyperedge query surfaces at `/api/alpha/hyperedges`
  for enrichment lookup.
- What we have *not* done in this MAP pass is a live census of current XTDB
  evidence/hyperedge contents. The current inventory is therefore grounded in
  code surfaces and local artifact samples, not a runtime database count.

### Ready vs Missing

| Ready (no new code needed) | Missing (actual work) |
|---|---|
| Five operational invariant logic files already exist. | `src/futon3c/peripheral/mission_logic.clj` does not exist yet. |
| Mission backend already enforces a core mission-law subset. | No shared `structural_law.clj` combinator layer yet. |
| Canonical registry seed exists in `docs/structural-law-inventory.sexp`. | No `invariant_runner.clj` aggregating all domains yet. |
| Evidence storage/query protocol already exists via `EvidenceBackend` and `store.clj`. | No `obligation.clj` mapping violations into actionability classes yet. |
| Mission/Proof peripherals already emit snapshot evidence. | No direct structural-law -> Arxana hyperedge bridge yet. |
| Mission Control already produces backfill evidence, portfolio diffs, and typed tensions. | No live `mission_logic` projection to compare against the other logic layers yet. |
| futon4/futon1a browser + hyperedge/snapshot surfaces already exist. | No explicit live XTDB census of current structural-law-relevant evidence/hyperedges yet. |
| Handoff 1.2 working set already lives inside the registry. | No dedicated Invariant Peripheral tool surface yet, and none is required for MAP. |

### Survey Questions

**Q1. Which invariant domains already have operational logic projections?**

Answer: Portfolio, Tickle, Agency, Proof, and Codex Code already have
`core.logic` projections. Mission does not yet have a logic file, but it does
have live backend enforcement that is strong enough to serve as an evidence
source.

**Q2. What persistence and evidence APIs already exist for structural law?**

Answer: `EvidenceBackend` already supports append/get/exists/query/forks/delete/all;
`store.clj` already provides public append/query/reply-chain/fork helpers; cycle
peripherals already emit step and snapshot evidence; Mission Control already
backfills inventory into evidence entries and diffs review snapshots.

**Q3. What browser / graph surfaces already exist for structural-law output?**

Answer: futon4 already provides Arxana browser, docbook navigation/export,
relation/tail browsing, and a web evidence viewer. futon1a already provides
snapshot save/restore APIs, and futon3c already queries futon1a hyperedges for
enrichment via `/api/alpha/hyperedges?end=` and `?type=`.

**Q4. What concrete structural-law-relevant data artifacts already exist?**

Answer: the registry seed (`structural-law-inventory.sexp`), 2 on-disk mission
state examples, 2 repo-local evidence ledgers (`mission-evidence.edn`,
`wiring-evidence.edn`), and the mission-control backfill/portfolio snapshot
shapes. These are enough to ground Phase 2 MAP even without a live XTDB census.

**Q5. What changed the scope or approach during MAP?**

Answer: the biggest surprise is that Mission is less "missing" than it first
looked. The mission layer already binds several core laws operationally in the
backend, so the immediate task is extraction into `mission_logic.clj`, not greenfield
invention. The second surprise is that `structural-law-inventory.sexp` is
already functioning as a proto-registry surface, which means Handoff 1.2 can
start now without waiting for a dedicated Invariant Peripheral.

### Surprises / Scope Corrections

- Mission-law extraction is a thinner step than originally feared because the
  backend already enforces part of the target law surface.
- Gate surfaces (GF/GD, configurable Reazon checks, explicit DAG queries) are
  materially different from foundational always-on laws and must stay distinct.
- MAP is currently artifact-centric rather than runtime-census-centric. If later
  work needs actual live evidence/hyperedge counts from XTDB, that should be a
  separate MAP subtask rather than an implicit assumption here.

## Phase 3: DERIVE — Extract Universal Combinators

**Goal:** Factor the meta-invariants into reusable goal combinators that
domain logic files can call, reducing each domain to a thin projection.

### Handoff 2.1: Structural law combinators

Extract the meta-invariants as parameterized core.logic goals.

- `:in` — the five existing `*_logic.clj` files plus
  `src/futon3c/peripheral/mission_logic.clj`, `docs/structural-law-inventory.sexp`
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

## Phase 4: ARGUE — Violations as Obligations, Conductor Integration

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

- [ ] Meta-invariant set enumerated and verified against 5+ domains, with firm
  exemplars recorded for each active family
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
