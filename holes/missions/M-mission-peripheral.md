# Mission: General Mission Peripheral

**Date:** 2026-02-18
**Status:** VERIFY complete, INSTANTIATE next
**Blocked by:** None (proof peripheral is operational; evidence landscape
is persistent; futonic logic is specified)

## Motivation (✺)

The proof peripheral (commit `7d1a5d0`, 15 tools, 9-phase cycle) works
for mathematical proof development. But the same cycle structure applies
to *code development missions*: observe the codebase, propose an approach,
execute the change, validate with tests, classify the outcome, integrate
into the evidence landscape, commit, and gate-review.

Today, code missions are tracked in markdown files (`holes/missions/M-*.md`),
coordinated via the war room (`war-room.md`), and executed with ad hoc
PSR/PUR discipline. This works but lacks the structural enforcement the
proof peripheral provides: phase-gated tools, mandatory outputs per phase,
DAG-tracked obligations, automatic evidence emission.

**Evidence when coding isn't just "unit tests pass."** It includes design
decisions (why protocol-first?), corpus checks (what did the pattern
library say?), framing validations (is the decomposition right?), and
the reasoning context around each step. The evidence facets technote
(`docs/evidence-facets.md`) documents how these can be faceted and queried.
This mission makes the faceting *automatic* by wiring it into the cycle
machine.

## Theoretical Anchoring

Three sources converge on the same design:

### 1. Corneli (2014) Table 24 — Entity Grammar

Table 24's entity relation diagram provides the grammar for missions.
Evidence facets (technote §"Application: Code Development Missions")
maps Table 24 entities to evidence fields:

- **X** (project/object) → mission or step (`evidence/subject`)
- **P** (problem) → open obligation (ledger item)
- **J** (conjecture) → hypothesis (`claim-type :hypothesis`)
- **S** (solution) → proposed approach (`claim-type :goal`)
- **H** (heuristic) → pattern from library (`evidence/pattern-id`)
- **S ↩ H** (solution uses heuristic) → PSR recording pattern selection

The thesis's future work (§10.6) calls for "computational agents that
are able to navigate the relevant mathematical structures (as outlined
Table 24)." The mission peripheral implements this navigation for code
development, not just mathematics.

### 2. Corneli (2014) Table 25 — Critical Apparatus

Table 25 provides the legibility layer — the para-development dimensions
that make mission evidence inspectable beyond "did the tests pass?"

The evidence facets technote (§"Feature Grid") maps all 7 layers of
Table 25 against existing futonic coverage. The mission peripheral
addresses the **weak/absent** dimensions by auto-tagging evidence:

| Phase | Natural Table 25 Sigils |
|-------|------------------------|
| observe | ♟ getting info, 👁️ sensory perception |
| propose | 🔀 intuition/association, 🗨️ argumentation |
| execute | 💾 software, 📝 written language |
| validate | 🔺 logic/deduction, ⚓ concrete applications |
| classify | 🧠 personal comprehension, ♕ self-discovery |
| integrate | 🔑 collaborative knowledge, 🛠️ organization |
| commit | ❖ consistency, 📥 gradual accumulation |
| gate-review | 🎖️ quality, ♔ constructive feedback |

This auto-classification means the feature grid fills in progressively
as agents do normal work. The para-development dimensions become
queryable without extra annotation burden.

### 3. Futonic Logic (`futon-theory/futonic-logic.flexiarg`) — The Loop

The futonic loop (§3) is the abstract form that both proof cycles and
code missions instantiate:

```
(futonic-loop
  (input   象  := configuration entering the cycle)
  (choose  部  := select decomposition regime)
  (articulate 咅 := make the decomposition explicit — PSR)
  (if (forms 鹽) := potential (鹵) + container (皿) compose)
  (and (sense 香) := perceive salience during execution)
  (and (regulate 🔮) := stay within budget, modulate amplitude)
  (then
    (act-within 未知 := execute under uncertainty)
    (evaluate 味 := evaluate by felt difference — PUR))
  (else
    (apply 捨 := set down, contain, park with recorded blocker)))
```

**Mapping onto the proof peripheral's 9 phases:**

| Futonic Loop | Proof Phase | Code Mission Phase |
|---|---|---|
| input 象 | — | Mission definition enters the cycle |
| choose 部 | observe | Read codebase, evidence landscape, select decomposition |
| articulate 咅 | propose | Write PSR: choose pattern, commit to approach |
| forms 鹽 | — | Requirements (鹵) + evidence protocol (皿) compose? |
| sense 香 | execute | During implementation, perceive whether it's working |
| regulate 🔮 | validate | Stay within scope, tests pass, budget check |
| act-within 未知 | execute | Write code under uncertainty |
| evaluate 味 | classify | Write PUR: evaluate outcome by felt difference |
| — | integrate | Update DAG, emit evidence entries |
| — | commit | Persist state, git commit |
| — | gate-review | G5→G0 checklist |
| apply 捨 | (any phase) | Park mission, record blocker, 味→未@0 |

**Key insight from futonic logic:** The 味→未@0 boundary rule (§8)
governs when to park a mission step. When evaluation approaches a
boundary (tests fail in unexpected ways, the decomposition feels wrong,
confidence drops below threshold), the system does not push through —
it recognizes it has entered 未知 and shifts to containment mode
(戒 restraint, 圍 contain, 包 wrap). This is the structural form of
the proof peripheral's gate-review rejection.

**A7 (compositional salience) explains mission readiness:** A mission
step becomes actionable when the agent perceives that potential (鹵 =
requirements, patterns, prior evidence) and container (皿 = evidence
protocol, recording shapes, test infrastructure) compose under the
current decomposition regime (部). Without 皿, the agent cannot perceive
whether the work is succeeding. This is why the evidence landscape
must be wired into the cycle *before* execution, not bolted on after.

## What This Mission Produces

1. **Mission cycle machine** — generalized from proof peripheral's
   9-phase cycle to handle code development. Same phase-gated tool
   access, mandatory outputs per phase, automatic evidence emission.

2. **Mission ledger** — DAG of obligations for a code mission, reusing
   proof_dag.clj algorithms (acyclicity, impact scoring, reachability).
   Obligations are things like "design the protocol", "implement backend
   A", "implement backend B", "write tests", "update CLAUDE.md".

3. **Auto-tagging by phase** — each evidence entry emitted during a
   cycle phase is automatically tagged with the Table 25 sigils for
   that phase (see table above), plus mission/step tags from the
   faceting system.

4. **Evidence-as-issue** — mission obligations can serve as issues.
   Each ledger item is an evidence entry with `claim-type :goal` or
   `:tension`, queryable via the evidence API. This is the path to
   the evidence landscape superseding GitHub issues for futonic
   projects.

5. **Futonic loop vocabulary annotations** — each phase annotated with
   its futonic logic role (象/部/咅/鹽/香/味/🔮/捨), making the
   abstract loop concrete and inspectable in evidence queries.

## Scope In

- Generalize proof peripheral cycle machine for code missions
- Mission ledger with DAG structure (reuse proof_dag.clj)
- Phase-gated tool access for code missions (different tool set from
  proof cycles — git, test runners, evidence queries)
- Auto-tagging: Table 25 sigils per phase, futonic loop role per phase
- Evidence emission: each phase transition produces an evidence entry
- Mission definition as self-referential evidence entry (facetization
  technote §"Self-Referential Faceting")
- Integration with existing PSR/PUR/PAR skills (they become phase-
  specific actions rather than standalone commands)

## Scope Out

- Mathematical proof cycles — already handled by proof peripheral
- Multi-agent dispatch within missions — needs M-agency-refactor
- Visualization of mission DAGs — needs futon4/Arxana work
- Sigil-based issue creation on GitHub — future ergonomic
- Cross-mission learning queries — future evidence landscape feature
- The futonic loop template for AGENTS.md — noted in futonic-logic
  next-steps but separate concern

## Derivation Path

This mission follows the derivation xenotype:

1. **IDENTIFY** — this document (mission proposal)
2. **MAP** — survey proof peripheral implementation, evidence facets
   technote, futonic-logic.flexiarg, war room mission tracking
3. **DERIVE** — extract the generalizable parts of the proof peripheral
   cycle machine; design mission-specific phase outputs and tool gates
4. **ARGUE** — write ARGUMENT.flexiarg establishing why the
   generalization is valid (proof cycles and code missions are both
   instances of the futonic loop)
5. **VERIFY** — validate the mission exotype against the social
   exotype; confirm the cycle machine handles edge cases (mission
   parking, 味→未@0 transitions, multi-step missions)
6. **INSTANTIATE** — build it

## Source Material

| Source | What We Take |
|--------|-------------|
| `proof.clj` + `proof_backend.clj` | Cycle state machine, phase gating, tool dispatch |
| `proof_dag.clj` | DAG algorithms (acyclicity, impact, reachability) |
| `proof_shapes.clj` | Malli shapes for domain objects (generalize to mission domain) |
| `docs/evidence-facets.md` | Facetization scheme, Table 24/25 mapping, auto-classification table |
| `library/futon-theory/futonic-logic.flexiarg` | Abstract loop, vocabulary, A7, 味→未@0 rule |
| `holes/war-room.md` | Mission tracking structure, IF/HOWEVER/THEN/BECAUSE format |
| War Bulletin 1 | Validation that the proof peripheral's observe/action split works |

## Key Design Decisions

### Code mission phases vs proof phases

The proof peripheral's 9 phases (observe → propose → execute →
validate → classify → integrate → commit → gate-review → completed)
are generic enough to work for code missions. The differences are in
**tool gates** and **mandatory outputs**:

| Phase | Proof Tools | Code Mission Tools |
|-------|------------|-------------------|
| observe | ledger-query, dag-impact, corpus-check, read | evidence-query, git-log, grep, glob, read, corpus-check |
| propose | (read-only) | (read-only) + PSR emission |
| execute | read, write, bash | read, write, bash, git operations |
| validate | read, bash-readonly | test-runner, bash-readonly, lint |
| classify | status-validate, ledger-query | PUR emission, status-validate |
| integrate | ledger-upsert, dag-check, failed-route-add | ledger-upsert, dag-check, evidence-emit |
| commit | proof-save | git-commit, mission-save |
| gate-review | gate-check | gate-check (same G5→G0) |

### Mission obligations as evidence entries

Each obligation in the mission ledger is also an evidence entry:

```clojure
{:evidence/type :coordination
 :evidence/claim-type :goal      ;; or :tension for blockers
 :evidence/subject {:ref/type :evidence :ref/id "mission-xtdb-backend"}
 :evidence/body {:item/id "design-protocol"
                 :item/label "Design EvidenceBackend protocol"
                 :item/status :open
                 :item/depends-on #{}
                 :item/unlocks #{"impl-atom" "impl-xtdb"}}
 :evidence/tags [:project/evidence-landscape :step/1
                 :sigil/progressive-problem-solving]}
```

This makes the evidence landscape the single source of truth for both
mission state and mission evidence. GitHub issues become a rendering
surface, not the canonical store.

### 味→未@0 as mission parking

When a code mission step hits a boundary (tests fail unexpectedly,
the decomposition feels wrong, scope creep detected), the cycle
machine applies 捨:

```clojure
;; 味→未@0 transition
{:evidence/type :coordination
 :evidence/claim-type :tension
 :evidence/subject {:ref/type :evidence :ref/id "step-3-xtdb"}
 :evidence/body {:transition :味→未@0
                 :boundary "tests pass but AtomBackend semantics
                            differ from XtdbBackend on ordering"
                 :containment [:戒 :圍]
                 :action "park step, record blocker, don't force"}
 :evidence/tags [:discipline/boundary-rule :sigil/feedback]}
```

This makes mission parking a first-class evidence entry, not a silent
status change in a markdown file.

## War Room Impact

If this mission succeeds:

- **M-agency-refactor** becomes expressible as a mission ledger DAG
  with obligation tracking and evidence emission
- **War Room Decisions** (WR-*) become evidence entries with
  argumentation sigils (🗨️), queryable across futons
- **War Bulletins** become evidence entries with their natural sigils
  (♟ getting info, 🧠 comprehension, ♔ feedback)
- **Cross-mission learning** becomes a facet query:
  `?tag=discipline/boundary-rule&type=coordination` returns all
  味→未@0 transitions across all missions

## Relationship to Evidence Landscape Superseding GitHub Issues

The evidence facets technote notes that the evidence landscape can
include and supersede GitHub issues. The mission peripheral makes this
concrete:

1. Mission = evidence entry (self-referential, `claim-type :goal`)
2. Steps = evidence entries (linked via `in-reply-to`)
3. Obligations = evidence entries (DAG-tracked, status-validated)
4. Work evidence = evidence entries (PSR/PUR/PAR, auto-tagged by phase)
5. Decisions = evidence entries (IF/HOWEVER/THEN/BECAUSE, argumentation)
6. Boundaries = evidence entries (味→未@0 transitions, containment)

GitHub issues become one possible rendering of items 1-3. The evidence
landscape is the canonical store. `gh issue create` becomes an optional
downstream projection, not the source of truth.

## MAP: Ancestral Evidence and Traceability

*Derivation xenotype step 2. Survey existing implementations as
ancestral patterns. Identify what generalizes, what's domain-specific,
and where the seams are.*

### Ancestral Implementation: Proof Peripheral

The proof peripheral (4 source files, 1 test file) is the primary
ancestor. Analysis separates generalizable mechanism from proof-specific
content.

#### proof.clj — Cycle State Machine (GENERALIZABLE)

The `ProofPeripheral` record implements `PeripheralRunner` with three
lifecycle methods: `start`, `step`, `stop`. The cycle state machine is:

```
Setup (nil phase)
  ↓ cycle-begin
:observe → :propose → :execute → :validate → :classify →
:integrate → :commit → :gate-review → :completed
  ↓
cycles-completed++ → phase back to nil
```

**Generalizable mechanism:**
- Phase gating: `current-phase-tools` → tool containment check → reject or dispatch
- Evidence creation per step: every tool invocation produces an evidence entry
- Operation classification: every tool tagged `:observe` or `:action`
- State threading: state flows through start → step* → stop
- Cycle counting: tracks completed cycles per session

**What changes for code missions:**
- Phase names and order (9 phases may not be the right decomposition)
- The setup phase tool set (code missions need different bootstrap tools)
- Evidence tagging (`:proof/operation-kind` → `:mission/operation-kind`)

#### proof_backend.clj — Tool Implementations (MIXED)

The `ProofBackend` record implements `ToolBackend` with 15 proof tools
plus 6 delegated tools (read, write, bash, bash-readonly, glob, grep).

**Generalizable patterns:**
- Cache-over-disk persistence: atom-backed in-memory cache with disk save/load
- Tool dispatcher: `cond` dispatch on tool-id, delegating unknown tools to real-backend
- Delegated tools: the 6 generic tools (read/write/bash/glob/grep/bash-readonly) pass through to the real backend unchanged

**Proof-specific tools (need code mission equivalents):**

| Proof Tool | Purpose | Code Mission Equivalent |
|---|---|---|
| proof-load / proof-save | State persistence | mission-load / mission-save |
| ledger-query / ledger-upsert | Obligation tracking | Same (obligations generalize) |
| dag-check / dag-impact | Dependency analysis | Same (DAG algorithms are generic) |
| canonical-get / canonical-update | Problem statement versioning | mission-spec-get / mission-spec-update |
| cycle-begin / cycle-advance | Cycle control | Same (generalize names) |
| cycle-get / cycle-list | Cycle query | Same |
| failed-route-add | Dead end recording | Same (code missions have dead ends too) |
| status-validate | Status transition checking | Same (different status values) |
| gate-check | G5→G0 checklist | Same (different gate criteria) |
| corpus-check | futon3a pattern search | Same (useful for code missions) |

**Observation:** Most tools generalize with only their domain content
changing. The dispatcher pattern, persistence pattern, and delegation
pattern are all reusable.

#### proof_dag.clj — DAG Algorithms (FULLY GENERALIZABLE)

Seven pure functions over immutable ledger data:

| Function | Algorithm | Domain-Agnostic? |
|---|---|---|
| `acyclic?` | Kahn's algorithm | YES |
| `impact-scores` | BFS transitive closure | YES |
| `impact-score` | Single-item BFS | YES |
| `reachable-from` | BFS via `:item/unlocks` | YES |
| `depends-chain` | BFS via `:item/depends-on` | YES |
| `dangling-refs` | Set difference | YES |
| `edge-consistency?` | Symmetric edge check | YES |

**No changes needed.** These work for any DAG with `:item/depends-on`
and `:item/unlocks` edges. Code mission obligations use the same
structure.

**Decision resolved:** proof_dag.clj generalizes to code missions
without modification.

#### proof_shapes.clj — Malli Schemas (MIXED)

**Generalizable shapes:**
- `OperationKind` = `[:enum :observe :action]` — applies to all peripherals
- `phase-allowed-tools` dispatch table pattern — any phase-gated workflow
- `phase-transitions` ordering pattern — any linear state machine
- `phase-required-outputs` enforcement — any phase with mandatory deliverables
- `tool-operation-kind` classification map — any tool set

**Proof-specific shapes (need code mission equivalents):**

| Proof Shape | Code Mission Shape |
|---|---|
| `ItemStatus` = `[:enum :proved :partial :open :false :numerically-verified]` | `[:enum :done :partial :open :blocked :abandoned]` |
| `EvidenceClass` = `[:enum :analytical :numerical :mixed]` | `[:enum :test :review :assertion :mixed]` |
| `CyclePhase` (9 phases) | TBD (same 9 or different?) |
| `LedgerItem` (proof obligation fields) | Mission obligation fields |
| `CanonicalStatement` (problem + closure criterion) | Mission spec (success criteria + scope) |
| `CycleRecord` (proof cycle tracking) | Mission cycle tracking |
| `FailedRoute` (structural obstruction) | Failed approach (with rationale) |
| `ProofState` (top-level container) | MissionState (top-level container) |

### Ancestral Patterns: Coordination Library

12 coordination patterns from futon3b/library/coordination/ map onto
the mission cycle:

| Pattern | Gate | Cycle Phase Mapping |
|---|---|---|
| `task-shape-validation` | G5 | **observe** — validate mission spec shape |
| `intent-to-mission-binding` | G5 | **observe** — bind intent to success criteria |
| `capability-gate` | G4 | **observe** — check agent is authorized |
| `assignment-binding` | G4 | **observe** — ensure explicit assignment |
| `mandatory-psr` | G3 | **propose** — require PSR before execution |
| `pattern-search-protocol` | G3 | **propose** — how to query pattern library |
| `bounded-execution` | G2 | **execute** — stay within budget |
| `artifact-registration` | G2 | **execute/commit** — register outputs |
| `mandatory-pur` | G1 | **classify** — evaluate outcome against criteria |
| `cross-validation-protocol` | G1 | **validate** — coordinate critical checks |
| `session-durability-check` | G0 | **commit** — ensure session reconstructable |
| `par-as-obligation` | G0 | **gate-review** — require PAR before close |

All 12 patterns apply to code missions without modification. They are
*coordination* patterns, not proof patterns — they constrain the process,
not the domain.

### Ancestral Patterns: Futon Theory

| Pattern | What It Provides | Cycle Mapping |
|---|---|---|
| `proof-path` | 8-phase audit trail (CLOCK_IN → PROOF_COMMIT) | The granular work unit within a cycle phase |
| `agent-contract` | 5 agent behavior requirements | Structural contract for any agent running cycles |
| `mission-scoping` | Boundary definition (criteria, scope, owner, deps) | Pre-cycle: defines what the cycle machine operates on |
| `mission-lifecycle` | State machine (:greenfield → :done) | Meta-cycle: mission state across multiple cycles |
| `futonic-logic` | Abstract loop + vocabulary (象/部/咅/鹽/香/味/🔮/捨) | The theoretical form that both proof and code cycles instantiate |
| `retroactive-canonicalization` | NAMING → SELECTION → CANALIZATION | How tensions from mission evidence feed the glacial loop |
| `structural-tension-as-observation` | Three tension signals | What the glacial loop observes in accumulated mission evidence |
| `xenotype-portability` | IDENTIFY → MAP → DERIVE → ARGUE → VERIFY → INSTANTIATE | The methodology we're using *right now* to build this mission |

### Traceability Table: What We Take From Where

| Component to Build | Ancestor Source | Relationship | Changes Needed |
|---|---|---|---|
| Phase gating mechanism | proof.clj `current-phase-tools` + `phase-allows-tool?` | EXTRACT + GENERALIZE | Remove proof-specific phase names; make phases configurable |
| Cycle state machine | proof.clj `step` method | EXTRACT + GENERALIZE | Phase order becomes a parameter, not a constant |
| Tool dispatch | proof_backend.clj `execute-tool` | REUSE | Same cond pattern, different tool IDs |
| Delegated tools | proof_backend.clj (6 tools to real-backend) | REUSE UNCHANGED | read/write/bash/glob/grep/bash-readonly |
| State persistence | proof_backend.clj cache-over-disk | EXTRACT + GENERALIZE | Different file paths, same mechanism |
| DAG algorithms | proof_dag.clj (all 7 functions) | REUSE UNCHANGED | Domain-agnostic already |
| Operation classification | proof_shapes.clj `tool-operation-kind` | REUSE + EXTEND | Add mission-specific tools to the map |
| Phase-required-outputs | proof_shapes.clj `phase-required-outputs` | REUSE PATTERN | Different output keys per phase |
| Gate checklist | proof_backend.clj `tool-gate-check` | ADAPT | Same G5→G0 structure, different gate criteria |
| Evidence emission | proof.clj (per-step evidence) | REUSE + EXTEND | Add Table 25 auto-tagging |
| Obligation shapes | proof_shapes.clj `LedgerItem` | DERIVE NEW | Different status values, different fields |
| Mission state shape | proof_shapes.clj `ProofState` | DERIVE NEW | MissionState with obligations + cycles |
| Corpus search | proof_backend.clj `tool-corpus-check` | REUSE UNCHANGED | Same futon3a integration |
| PSR emission | mandatory-psr.flexiarg | WIRE INTO propose phase | PSR becomes a phase action, not standalone |
| PUR emission | mandatory-pur.flexiarg | WIRE INTO classify phase | PUR becomes a phase action, not standalone |
| PAR emission | par-as-obligation.flexiarg | WIRE INTO gate-review | PAR becomes a gate-review obligation |
| Budget enforcement | bounded-execution.flexiarg | WIRE INTO execute phase | Same constraint, different budget units |
| Table 25 auto-tagging | evidence-facets.md feature grid | NEW | Phase → sigil mapping table |
| Futonic loop annotations | futonic-logic.flexiarg §3 | NEW | Phase → vocabulary mapping |
| 味→未@0 parking | futonic-logic.flexiarg §8 | NEW | Boundary detection + containment |

### The Generalization Seam

The key architectural insight: the proof peripheral is already
**two things bolted together**:

1. **A generic cycle machine** — phase gating, tool dispatch, evidence
   emission, state threading, cycle counting
2. **A proof domain layer** — ledger items, canonical statements,
   failure routes, status policies

The generalization separates these layers:

```
┌─────────────────────────────────┐
│     Domain Layer (pluggable)    │
│  ┌────────┐  ┌───────────────┐  │
│  │ Proof  │  │ Code Mission  │  │
│  │ Domain │  │    Domain     │  │
│  └────┬───┘  └──────┬────────┘  │
│       │              │          │
├───────┴──────────────┴──────────┤
│     Cycle Machine (shared)      │
│  Phase gating, tool dispatch,   │
│  evidence emission, DAG ops,    │
│  state persistence, gate check  │
└─────────────────────────────────┘
```

The cycle machine becomes a protocol or configuration-driven engine.
The domain layer provides:
- Phase names and order
- Tool restrictions per phase
- Required outputs per phase
- Domain shapes (obligation structure, state container)
- Gate criteria (what G5→G0 check for this domain)
- Auto-tagging rules (Table 25 sigils, futonic loop vocabulary)

The proof peripheral becomes: `(make-proof proof-domain-config)`.
The mission peripheral becomes: `(make-mission code-mission-domain-config)`.
Both use the same cycle machine underneath.

### MAP Assessment

**Readiness:** High. The proof peripheral's implementation is clean
and the generalization seam is clear. The DAG algorithms need no
changes. The coordination patterns apply directly. The theoretical
anchoring (futonic logic, Table 24/25) provides the design vocabulary.

**Risk:** The 9-phase cycle may not be the right decomposition for
code missions. The proof cycle's phases (observe → propose → execute →
validate → classify → integrate → commit → gate-review) might need
splitting or merging. This should be resolved in DERIVE.

**Open question from MAP:** Should the cycle machine be a *protocol*
(each domain implements its own runner) or a *configuration* (one
generic runner parameterized by domain config)? The configuration
approach is simpler but less flexible. The protocol approach allows
domains to override individual phase behaviors. The proof peripheral's
current implementation is closer to "protocol" (hard-coded phases in
proof.clj), but the structure already resembles configuration (phase
tables in proof_shapes.clj).

## DERIVE — Extraction and Construction

### Approach: Configuration-Driven Cycle Machine

**Decision:** Configuration route, not protocol. The cycle machine is a
single generic engine (`cycle.clj`) parameterized by a `CycleDomainConfig`
map. Both proof and mission peripherals instantiate the same engine with
different domain configurations. This was the user's preference and
matches the existing structure — proof_shapes.clj already contains the
phase tables that become configuration data.

**Autoconf hook:** The `CycleDomainConfig` includes an optional
`:autoconf-fn` that is called during `start`, allowing domain-specific
configuration refinement from context. For missions, this is currently
a pass-through but reserves the slot for future auto-scoping from
mission spec documents.

### Files Created

#### `cycle.clj` — Generic Cycle Machine (extracted from proof.clj)

The cycle machine implements `PeripheralRunner` and provides:
- Phase-gated tool dispatch (structural enforcement)
- Operation classification (`:observe` / `:action` tagging)
- Evidence enrichment with Table 25 auto-tags via `:phase-tags-fn`
- Cycle state tracking (current phase, cycle ID, cycles completed)
- Domain state initialization via `:state-init-fn`
- Fruit extraction via `:fruit-fn` and exit context via `:exit-context-fn`
- Autoconf hook called on start

The `CycleDomainConfig` requires:
```
:domain-id, :phase-order, :phase-tools, :setup-tools, :tool-ops,
:required-outputs, :cycle-begin-tool, :cycle-advance-tool,
:fruit-fn, :exit-context-fn
```

Optional: `:state-init-fn`, `:phase-tags-fn`, `:autoconf-fn`, `:state-snapshot-fn`

#### `mission_shapes.clj` — Domain Shapes for Code Missions

Defines Malli shapes and phase configuration:
- `ObligationStatus` — `[:enum :done :partial :open :blocked :abandoned]`
- `EvidenceClass` — `[:enum :test :review :assertion :mixed]`
- `Obligation` — reuses DAG structure from proof (`item/depends-on`, `item/unlocks`)
- `MissionSpec` — versioned mission specification with scope-in/scope-out
- `CycleRecord`, `FailedApproach`, `MissionState` — composite shapes
- `phase-allowed-tools` — tool gates per phase (adapted from proof)
- `phase-required-outputs` — mandatory outputs before phase advance
- `phase-sigil-tags` — Table 25 auto-tags per phase
- `mission-tool-operation-kinds` — observe/action classification

Key difference from proof: `:assertion` evidence alone cannot yield `:done`
(vs proof's `:numerical` evidence cannot yield `:proved`).

#### `mission_backend.clj` — Tool Implementations

Mirrors proof_backend.clj structure with mission-domain tools:
- 17 mission tools: mission-load/save, obligation-query/upsert,
  dag-check/impact, mission-spec-get/update, cycle-begin/advance/get/list,
  failed-approach-add, status-validate, gate-check, corpus-check,
  evidence-query
- 6 delegated tools: read, glob, grep, bash, bash-readonly, write
- Same cache-over-disk persistence pattern as proof backend
- Reuses `proof_dag.clj` algorithms unchanged for DAG operations

#### `mission.clj` — Domain Config and Factory

Wires mission_shapes into the cycle machine:
- `mission-domain-config` — the `CycleDomainConfig` map
- `setup-tools` — tools available between cycles
- `autoconf` — context-dependent config refinement (currently pass-through)
- `state-init` — adds `:mission-id` to cycle state
- `fruit` / `exit-context` — session output extraction
- `phase-tags` — delegates to `ms/phase-sigil-tags` for Table 25 auto-tags
- `make-mission` — factory function (1-arity mock, 2-arity with backend)

#### `peripherals.edn` — Added `:mission` Spec

The mission peripheral is registered alongside proof with 23 tools
and `:full-codebase` scope.

### Phase Resolution

**Decision:** The 9-phase cycle is retained for code missions.
IF the proof cycle's 9 phases (observe → propose → execute → validate →
classify → integrate → commit → gate-review → completed) map naturally
to code development workflows,
HOWEVER some phases might seem proof-specific (e.g., "classify"),
THEN we adapt the semantics while keeping the phase names:
- `:observe` — examine codebase, identify blockers, query evidence
- `:propose` — design approach, check pattern library
- `:execute` — write code, run commands
- `:validate` — run tests, check build
- `:classify` — assess result (done/partial/blocked), validate status transition
- `:integrate` — update obligations DAG, record failed approaches
- `:commit` — save mission state
- `:gate-review` — run G5→G0 gate checklist
BECAUSE the phases represent a general development cycle that applies
to any structured work, and keeping the same phases simplifies the
generic cycle machine.

### Table 25 Auto-Tagging Scheme

**Decision:** Hardcoded per phase. Each phase gets fixed Table 25 sigil
tags that are automatically applied to evidence entries during that phase.

| Phase | Sigil Tags |
|-------|-----------|
| observe | `:sigil/getting-information`, `:sigil/perception` |
| propose | `:sigil/argumentation`, `:sigil/intuition` |
| execute | `:sigil/software`, `:sigil/written-language` |
| validate | `:sigil/logic-deduction`, `:sigil/concrete-applications` |
| classify | `:sigil/personal-comprehension`, `:sigil/self-discovery` |
| integrate | `:sigil/collaborative-knowledge`, `:sigil/organization` |
| commit | `:sigil/consistency`, `:sigil/gradual-accumulation` |
| gate-review | `:sigil/quality`, `:sigil/constructive-feedback` |

This makes the Table 25 dimensions queryable on evidence entries without
manual annotation.

### Evidence Landscape Integration — State Snapshots

**Implemented:** The cycle machine and mission peripheral now emit state
snapshots as evidence entries, making mission state inspectable from the
evidence landscape without loading EDN files or being inside a peripheral
session.

#### Changes

**`evidence.clj`** — Added `make-snapshot-evidence` helper. Creates
`:claim-type :observation` entries with domain-specific subject, body,
and tags. The generic cycle machine calls this when `:state-snapshot-fn`
returns non-nil.

**`cycle.clj`** — `dispatch-step` extended with snapshot emission after
successful tool execution. When the domain's `:state-snapshot-fn` returns
a snapshot map `{:snapshot/subject ... :snapshot/body ... :snapshot/tags ...}`,
the cycle machine:
1. Creates snapshot evidence via `make-snapshot-evidence`
2. Appends it to the evidence store
3. Chains it to the step evidence via `:evidence/in-reply-to`
4. Returns `:snapshot-evidence` alongside `:evidence` in the step result

**`mission.clj`** — Added `state-snapshot` function that fires on
`:mission-save` only. Returns:
- `:snapshot/subject` → `{:ref/type :mission :ref/id <mission-id>}`
- `:snapshot/body` → summary: mission ID, version, obligation counts
  by status, cycles count, failed approaches count, current phase/cycle
- `:snapshot/tags` → `[:mission/<id> :snapshot]`

**`shapes.clj`** — Added `:mission` to `PeripheralId` enum. This was
required for `:mission` peripheral spec validation in peripherals.edn.

#### Why `:mission-save` is the Right Trigger

The snapshot fires when state hits disk, not on every tool call. This is
the moment the operational state becomes durable — the snapshot captures
a consistent, persisted view. Other tools modify in-memory cache only;
snapshotting those intermediate states would produce noise.

#### Inspectability Patterns Enabled

**Pattern A (implemented):** Query `?claim-type=observation&subject-type=mission`
returns the latest state snapshots for all missions. The Arxana viewer
can render obligation status, cycle progress, and current phase.

**Pattern B (future):** Obligation-level evidence trails. Each obligation
becomes a subject that work evidence references. Not yet implemented.

**Pattern C (implemented):** Session-to-mission correlation via
`:evidence/tags [:mission/M-test]` on snapshot entries.

### Test Results

- **37 tests, 99 assertions, 0 failures, 0 errors** across cycle_test.clj
  and mission_test.clj
- **18 existing proof tests still pass** (regression confirmed)
- Tests cover: lifecycle (start/stop), phase gating, phase transitions,
  cycle completion, evidence enrichment with operation-kind tagging,
  Table 25 sigil tag propagation, autoconf hook invocation, unclassified
  tool rejection, full cycle walk, domain config validation, shapes
  validation, tool classification coverage, state snapshot emission on
  mission-save, snapshot absence on non-save tools

### DERIVE Assessment

**Status:** Complete. The generic cycle machine is extracted and both
the test domain and mission domain instantiate it successfully. The
code compiles and all tests pass.

**What remains for ARGUE:**
- Argue that configuration-driven approach is better than protocol
- Argue that the 9 phases are sufficient for code missions
- Argue the Table 25 tag assignments against Table 25 semantics

**What remains for VERIFY:**
- Integration test with real backend (not mock)
- Verify that proof.clj could be refactored to use cycle.clj
  (proving the extraction preserved behavior)
- Test evidence persistence through XTDB backend

**What remains for INSTANTIATE:**
- Refactor proof.clj to use the generic cycle machine
- Wire mission peripheral into Agency routing
- Build the autoconf function that reads mission spec documents

## ARGUE — Justification of Design Decisions

### Argument 1: Configuration over Protocol

**IF** the proof peripheral and mission peripheral share the same lifecycle
(start → step* → stop), the same phase-gated tool dispatch, the same
evidence enrichment, and the same cycle tracking,

**HOWEVER** the two domains differ in their tool sets, status enumerations,
phase semantics, gate criteria, and auto-tagging rules,

**THEN** the cycle machine should be *configuration-driven* (one generic
`CyclePeripheral` parameterized by a `CycleDomainConfig` map) rather than
*protocol-driven* (each domain implementing `PeripheralRunner` directly),

**BECAUSE:**

1. **The shared logic is substantial.** `dispatch-step` (phase gating →
   operation classification → tool dispatch → evidence emission → state
   update) is ~60 lines that would be duplicated verbatim in each domain.
   The proof.clj and cycle.clj implementations are structurally identical —
   every line in proof.clj's `dispatch-step` maps to a line in cycle.clj's,
   with configuration lookups replacing hard-coded references.

2. **Domain differences are purely data.** Phase order, tool gates, required
   outputs, operation kinds, and sigil tags are all maps and sets — data, not
   behavior. The only behavioral hooks (fruit-fn, exit-context-fn, state-init-fn,
   phase-tags-fn, autoconf-fn) are narrow functions passed in the config.

3. **Autoconf provides the flexibility that protocol would.** If a domain
   needs runtime configuration refinement (e.g., adjusting tool gates based
   on the mission spec), the `:autoconf-fn` hook is called during `start`.
   This is equivalent to overriding `start` in a protocol implementation but
   scoped to configuration rather than the full lifecycle.

4. **Testing becomes combinatorial.** The generic cycle machine can be tested
   once with a synthetic domain (test-config in cycle_test.clj), and each
   real domain only needs to test its configuration data (does the config
   validate? are tools classified? do phases cover all tools?). This is
   33 tests covering both the engine and the mission domain, vs what would
   be 33 tests × N domains if each implemented the protocol independently.

5. **New domains are declarative.** Adding a new cycle-based peripheral
   (e.g., a research peripheral, a deployment peripheral) requires writing
   a config map and a backend — no new `PeripheralRunner` implementation.

**Counter-argument addressed:** "Configuration is less flexible than
protocol — what if a domain needs to override phase transition logic?"
The cycle machine's phase transitions are linear (configurable via
`:phase-order`). If a domain needs non-linear transitions (branching,
looping), that would require a different engine, not a configuration
override. The futonic loop explicitly specifies linear phase traversal
(部 = decomposition regime, traversed in order). Non-linear cycles
would be a different 象 (configuration) entirely.

### Argument 2: Retaining 9 Phases for Code Missions

**IF** the proof peripheral's 9 phases (observe → propose → execute →
validate → classify → integrate → commit → gate-review → completed)
were designed for mathematical proof development,

**HOWEVER** code development missions have different activities (reading
code, writing code, running tests, updating DAGs, saving state),

**THEN** the same 9 phases should be retained for code missions with
adapted semantics,

**BECAUSE:**

1. **The phases describe a general development cycle, not a proof-specific
   one.** Observe (understand the problem) → propose (design an approach) →
   execute (do the work) → validate (check it worked) → classify (assess
   the outcome) → integrate (update the knowledge base) → commit (persist) →
   gate-review (quality check) is the natural rhythm of any deliberate
   structured work. Corneli (2014) Table 24 maps the same structure:
   W (request) → H (heuristic) → S (solution) → E (ephemera, evidence).

2. **The futonic loop mandates this structure.** The futonic-logic.flexiarg
   specifies 部 (decomposition regime) as a linear phase sequence with
   味 (evaluation) at each transition. The 9 phases instantiate this:
   each phase has required outputs (味) that must be satisfied before
   the next phase begins. Reducing to fewer phases would lose evaluation
   points; adding more would add friction without adding signal.

3. **Empirical evidence from proof.** The proof peripheral has been used
   for real work (First Proof Problem 1, corpus calibration). The phase
   structure forced useful discipline: observe caught a framing failure
   that unstructured work missed; classify forced explicit status assessment
   rather than implicit "it seems done." These benefits transfer directly
   to code missions.

4. **Phase names are intentionally abstract.** "Classify" doesn't mean
   "classify a mathematical result" — it means "assess the outcome of
   execution against the obligation." For code: "did the tests pass?
   is the obligation done, partial, or blocked?" "Integrate" doesn't
   mean "integrate a proof step" — it means "update the obligation DAG
   and record any failed approaches." The semantics are general.

5. **Tool gates map naturally.** The phase-allowed-tools for code missions
   (mission_shapes.clj) restrict tools in ways that prevent real errors:
   you can't write code during observe (forces understanding first), you
   can't modify obligations during execute (prevents mid-stream scope
   changes), you must save before gate-review (ensures persistence).

### Argument 3: Table 25 Auto-Tag Assignments

**IF** Table 25 (Corneli 2014) defines 7 layers of para-development
activity dimensions across phase-like transitions,

**HOWEVER** the mapping from our 9 phases to Table 25's dimensions is
not one-to-one (Table 25 has different phase granularity),

**THEN** each phase should receive fixed sigil tags from the most
relevant Table 25 dimensions,

**BECAUSE:**

1. **Auto-tagging makes dimensions queryable without manual effort.** An
   agent working in the `:observe` phase automatically gets
   `:sigil/getting-information` and `:sigil/perception` tags on every
   evidence entry. These tags become query facets: "show me all evidence
   tagged with getting-information" surfaces the observation work across
   all missions.

2. **The assignments follow Table 25's semantics.** Layer 1 (system-experience
   alignment) maps observe→getting-information because observe is where
   the agent gathers data about the system. Layer 5 (quality & sustainability)
   maps gate-review→quality because gate-review is where quality checks
   run. The mappings are not arbitrary — they reflect what actually happens
   in each phase.

3. **Hardcoded-per-phase is the right starting granularity.** Future
   refinement (per-tool tags, per-mission-type tags) can be added via the
   autoconf hook without changing the engine. Starting with phase-level
   tags provides immediate value with zero configuration burden.

### Argument 4: Evidence Landscape as Mission Inspectability Layer

**IF** mission state currently lives in two places — operational state in
`data/mission-state/{id}.edn` (for tool execution) and evidence entries
in the evidence store (as side-effect exhaust from peripheral sessions),

**HOWEVER** a user or agent wanting to "jump into a mission and know
exactly what state it was in" must currently load the EDN file, which is
invisible to the evidence landscape and not queryable from outside,

**THEN** the mission peripheral should emit *mission state snapshots*
as evidence entries at key lifecycle points, making mission state fully
inspectable through the evidence landscape,

**BECAUSE:**

1. **The evidence landscape is already the canonical query surface.** The
   HTTP endpoints (`GET /api/alpha/evidence`), the store API (`query*`),
   and the viewer (Arxana Labs) all read from the evidence store. Mission
   state that only exists in EDN files is invisible to all of these.

2. **The evidence facets technote already describes this pattern.** The
   "self-referential faceting" section (evidence-facets.md lines 55-78)
   shows projects and steps as evidence entries, with work evidence
   referencing them via `:evidence/subject`. Mission obligations are
   exactly this: structured sub-goals that should be evidence entries
   themselves, so that work done on each obligation links back to it.

3. **The dual persistence is complementary, not redundant.**
   - **EDN state** is *operational* — compact, fast to read, structured
     for tool implementations. The backend reads it to answer queries,
     check DAG acyclicity, validate transitions.
   - **Evidence entries** are *historical* — each state change is recorded
     with timestamp, author, and causal chain. They support "what happened
     when" queries that the current-state EDN cannot answer.
   - **State snapshots** bridge the two: periodic evidence entries (emitted
     on `:mission-save`) capture the full MissionState, making the latest
     snapshot queryable as evidence without replacing the operational store.

4. **This enables three concrete inspectability patterns:**

   **Pattern A: Mission overview from outside.**
   Query `?subject-type=mission&subject-id=M-test&claim-type=observation`
   returns the latest state snapshot. The viewer renders obligations,
   cycles completed, current phase, failed approaches — without loading
   the EDN file or being inside a peripheral session.

   **Pattern B: Obligation-level evidence trail.**
   Each obligation becomes an evidence entry (`claim-type :goal`, subject
   referencing the mission). Work evidence uses `:evidence/subject` →
   `{:ref/type :evidence, :ref/id "O-main"}` to link to the obligation.
   Query `?subject-id=O-main` returns all evidence about that obligation:
   which cycles targeted it, what approaches were tried, what failed.

   **Pattern C: Session-to-mission correlation.**
   The `:evidence/session-id` field already links evidence to peripheral
   sessions. The `:evidence/tags` field can carry `:mission/M-test` and
   `:obligation/O-main`. Cross-referencing session evidence with mission
   state shows "which sessions worked on which obligations" — the missing
   link between transient sessions and persistent missions.

5. **Implementation is minimal.** The cycle machine already emits start,
   step, and stop evidence. Adding state snapshots requires:
   - A new `:state-snapshot-fn` in `CycleDomainConfig` (optional, called
     on mission-save steps)
   - An evidence entry with `:claim-type :observation` and `:evidence/body`
     containing a summary of the current MissionState (not the full state,
     which is large — a summary: obligation counts by status, current cycle,
     phase, failed approach count)
   - Tags: `:mission/{id}` for mission-level queries
   - Subject: `{:ref/type :mission, :ref/id "M-test"}`

   This is ~20 lines of code in the cycle machine, one new optional
   config key, and immediate queryability of mission state from the
   evidence landscape.

6. **The futonic loop supports this interpretation.** The 捨 (set-down)
   vocabulary describes the moment a cycle boundary is reached and the
   system must decide whether to continue or park. The state snapshot
   at each `:commit` phase is precisely this: a record of where the
   mission stands at the moment of set-down, enabling informed resumption.

### Argument 5: How DERIVE Artifacts Relate to the Evidence Landscape

The DERIVE step itself should have been evidence. The four files created
(cycle.clj, mission_shapes.clj, mission_backend.clj, mission.clj)
represent four `:step` evidence entries under the mission
`M-mission-peripheral`, with the DERIVE section of this document as
a `:conclusion`. The fact that we are *describing* this connection
rather than *using* it demonstrates the gap: the mission peripheral
doesn't yet exist operationally, so this mission is being executed
with ad hoc PSR/PUR discipline rather than structural enforcement.

This is exactly the bootstrapping problem the mission proposal
identified: "code missions are tracked in markdown files, coordinated
via the war room, and executed with ad hoc PSR/PUR discipline."

When the mission peripheral is operational (INSTANTIATE step), the
derivation of the *next* peripheral (e.g., a research peripheral,
a deployment peripheral) will be trackable through the evidence
landscape:

```
Query: ?subject-type=mission&subject-id=M-research-peripheral
→ Evidence chain: IDENTIFY goal → MAP steps → DERIVE steps →
  ARGUE observations → VERIFY steps → INSTANTIATE conclusion
→ Each step linked to obligations, phase tags, sigil tags
→ Failed approaches recorded and queryable
→ State snapshot shows current phase and obligation status
```

This is the self-improving loop: each mission executed through the
peripheral generates the evidence that makes future missions more
inspectable and resumable.

## VERIFY — Validation Against the Social Exotype

*Derivation xenotype step 5. Validate the derived exotype against the
social exotype and peripheral infrastructure. Confirm the extraction
preserved behavior, the evidence protocol is sound, and edge cases
(mission parking, multi-cycle sessions) are handled.*

### Verification 1: Proof Expressibility as CycleDomainConfig

**Claim:** The extraction from proof.clj to cycle.clj preserved all
behavior. proof.clj's hardcoded logic is fully expressible as a
`CycleDomainConfig` map.

**Test:** `proof-is-expressible-as-cycle-domain-config` constructs a
proof-domain config from `proof_shapes.clj` data:

```clojure
{:domain-id        :proof
 :phase-order      ps/phase-order
 :phase-tools      ps/phase-allowed-tools
 :setup-tools      #{:proof-load :proof-save :ledger-query ...}
 :tool-ops         ps/proof-tool-operation-kinds
 :required-outputs ps/phase-required-outputs
 :cycle-begin-tool :cycle-begin
 :cycle-advance-tool :cycle-advance
 :fruit-fn         (fn [state] {...})
 :exit-context-fn  (fn [state] {...})}
```

This config passes `valid-domain-config?`, confirming that every piece of
proof.clj's behavior maps to a configuration field. The proof peripheral
*could* be refactored to use the cycle machine with zero behavioral change.

**Structural comparison:** `proof-config-and-mission-config-share-structure`
verifies both domains share 9 phases, start with `:observe`, end with
`:completed`, and use the same `:cycle-begin` / `:cycle-advance` tools.

### Verification 2: Round-trip (←) Backward Verification

**Claim:** The mission peripheral's evidence passes the ← verification
framework — every tool invocation, scope boundary, evidence type, and
structural invariant checked against the peripheral spec.

**Tests:**
- `mission-passes-backward-verification` — setup tools (mission-load,
  obligation-query) produce well-formed evidence: 4 entries (goal + 2
  steps + conclusion), correct claim-type sequence.
- `mission-backward-verification-with-cycle` — active cycle (cycle-begin
  → obligation-query → cycle-advance) produces 6 entries with correct
  claim-type sequence.

The round-trip framework (`round_trip.clj`) checks:
- Tool set adherence: every tool in evidence is in the spec's tool set
- Scope boundaries: every arg respects the spec's scope
- Evidence type: all entries are `:coordination` (correct for mission)
- Structure: starts with `:goal`, ends with `:conclusion`, single session

### Verification 3: Proof-Tree Invariants

**Claim:** Mission evidence satisfies all 7 proof-tree invariants
established in the integration test suite.

**Test:** `mission-evidence-satisfies-proof-tree-invariants` runs a full
mission session (start → load → query → cycle-begin → advance → stop)
with an evidence store, then projects a thread and checks:

1. **Tree validity** — every `:evidence/in-reply-to` references an
   existing entry in the projection. ✓
2. **Root invariant** — a `:goal` entry exists as the thread root. ✓
3. **No orphans** — all entries share the session subject. ✓
4. **Claim ordering** — no `:conclusion` replies to another
   `:conclusion`. ✓
5. **Author tracking** — `:thread/participants` is non-empty. ✓
6. **Monotonic timestamps** — entries are non-decreasing by time. ✓
7. **Count consistency** — `:thread/entry-count` matches actual count. ✓

Thread status is `:closed` after stop.

### Verification 4: Edge Cases

**Mission parking (味→未@0):**
`mid-cycle-stop-produces-valid-evidence` starts a cycle, enters
`:observe` phase, then stops with reason "boundary reached: 味→未@0".
Verifies:
- Fruit records `{:final-phase :observe, :cycles-completed 0}` — the
  exact state at parking time
- Stop evidence captures the reason
- Evidence entry validates against `EvidenceEntry` shape

This confirms the cycle machine supports the futonic 捨 (set-down)
gracefully — an agent can park a mission mid-cycle and the state is
fully recoverable.

**Multi-cycle sessions:**
`multi-cycle-session-increments-counter` completes two full cycles in
one session. Verifies:
- First cycle: `cycles-completed` → 1, `current-phase` → nil (cleared)
- Second cycle: enters `:observe` again, completes → `cycles-completed` → 2
- Fruit captures final count

This confirms the cycle machine resets correctly between cycles and
supports the real mission pattern: multiple obligation cycles per session.

**Snapshot evidence chaining:**
`snapshot-evidence-chains-to-step-evidence` verifies that the snapshot
evidence entry emitted on `:mission-save` chains to the step evidence
via `:evidence/in-reply-to`, and that `get-reply-chain*` can reconstruct
the ancestor chain. This confirms snapshots are first-class participants
in the evidence thread, not orphaned side-emissions.

### VERIFY Assessment

**Test results:** 28 mission tests, 110 assertions, 0 failures.
116 total tests across mission + cycle + proof + registry + round-trip +
integration + social, 456 assertions, 0 failures.

**What VERIFY confirmed:**
- The generic cycle machine is a valid extraction of proof.clj
- Mission evidence passes the ← backward verification framework
- All 7 proof-tree invariants hold on mission evidence
- Mission parking (味→未@0) is graceful and recoverable
- Multi-cycle sessions work correctly
- State snapshots chain properly in the evidence thread

**What remains for INSTANTIATE:**
- Refactor proof.clj to use the generic cycle machine (the VERIFY step
  proved this is possible — the config passes `valid-domain-config?`)
- Wire mission peripheral into Agency routing
- Build the autoconf function that reads mission spec documents
- End-to-end test with real backend (file I/O, not mock)

## Decision Log

- [x] Confirm that proof_dag.clj algorithms generalize to code missions
  without modification (acyclicity, impact scoring work the same way)
  **RESOLVED in MAP:** Yes. All 7 functions are pure, domain-agnostic
  graph algorithms. They work with any `:item/depends-on` / `:item/unlocks`
  DAG structure.
- [x] Design code mission tool gates (what tools are available per phase)
  **RESOLVED in DERIVE:** Adapted from proof. Key differences:
  `:observe` adds `:evidence-query`; `:execute` adds `:bash`;
  `:classify` adds `:obligation-query`; `:integrate` adds `:obligation-upsert`;
  `:commit` uses `:mission-save`.
- [ ] Decide whether PSR/PUR skills become phase-specific or remain
  standalone (both? PSR in propose phase, PUR in classify phase, but
  also callable independently?)
- [x] Design the auto-tagging scheme for Table 25 sigils (hardcoded
  per phase or configurable per mission type?)
  **RESOLVED in DERIVE:** Hardcoded per phase via `phase-sigil-tags` map.
  Future: configurable per mission type via autoconf.
- [ ] Decide granularity: one cycle per mission step, or one cycle
  per mission with sub-cycles per step?
- [x] Evaluate whether mission peripheral should be a 7th peripheral
  type alongside explore/edit/test/deploy/reflect/proof, or a
  meta-peripheral that orchestrates the others
  **RESOLVED in DERIVE:** 8th peripheral type (alongside alfworld).
  Registered in peripherals.edn with own spec and tool set.
- [x] **From MAP:** Protocol vs configuration for cycle machine
  generalization.
  **RESOLVED in DERIVE:** Configuration route. CycleDomainConfig
  parameterizes a single generic CyclePeripheral. Autoconf hook
  for context-dependent refinement.
- [x] **From MAP:** Are the 9 phases the right decomposition for code
  missions?
  **RESOLVED in DERIVE:** Yes. Same 9 phases with adapted semantics.
  Phase names are general enough. The key insight is that "classify"
  maps to "assess the outcome" and "integrate" maps to "update the
  obligation DAG" — both are meaningful for code development.
- [x] **From MAP:** Code mission ItemStatus values.
  **RESOLVED in DERIVE:** `[:enum :done :partial :open :blocked :abandoned]`
  Implemented in mission_shapes.clj.
- [x] **From MAP:** Code mission EvidenceClass.
  **RESOLVED in DERIVE:** `[:enum :test :review :assertion :mixed]`
  Implemented in mission_shapes.clj. `:assertion` evidence cannot
  yield `:done` (structural enforcement).
- [x] **From ARGUE:** Configuration vs protocol justification.
  **RESOLVED in ARGUE:** Configuration is correct. Shared logic is
  substantial (~60 lines dispatch-step), domain differences are purely
  data, autoconf provides flexibility, testing is combinatorial.
- [x] **From ARGUE:** 9-phase retention justification.
  **RESOLVED in ARGUE:** Phases describe general development cycle.
  Empirical evidence from proof confirms discipline value. Phase names
  are intentionally abstract. Tool gates map naturally.
- [x] **From ARGUE:** Table 25 auto-tag assignment justification.
  **RESOLVED in ARGUE:** Hardcoded per phase. Assignments follow
  Table 25 semantics. Phase-level is right starting granularity.
- [x] **From ARGUE:** Evidence landscape integration — emit mission
  state snapshots as evidence entries at lifecycle points (mission-save).
  **RESOLVED in DERIVE:** `:state-snapshot-fn` config key added to
  CycleDomainConfig. Mission snapshot fires on `:mission-save`, emitting
  `:claim-type :observation` entries with obligation summary. Tested:
  snapshot appears in evidence store with correct subject/body/tags.
- [ ] **From ARGUE:** Obligation-level evidence — obligations as
  evidence entries with `{:ref/type :mission}` subject, enabling
  per-obligation evidence trails queryable from outside.
