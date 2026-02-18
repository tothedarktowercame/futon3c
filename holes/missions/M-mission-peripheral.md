# Mission: General Mission Peripheral

**Date:** 2026-02-18
**Status:** IDENTIFY complete, MAP in progress
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

## Decision Log

- [x] Confirm that proof_dag.clj algorithms generalize to code missions
  without modification (acyclicity, impact scoring work the same way)
  **RESOLVED in MAP:** Yes. All 7 functions are pure, domain-agnostic
  graph algorithms. They work with any `:item/depends-on` / `:item/unlocks`
  DAG structure.
- [ ] Design code mission tool gates (what tools are available per phase)
- [ ] Decide whether PSR/PUR skills become phase-specific or remain
  standalone (both? PSR in propose phase, PUR in classify phase, but
  also callable independently?)
- [ ] Design the auto-tagging scheme for Table 25 sigils (hardcoded
  per phase or configurable per mission type?)
- [ ] Decide granularity: one cycle per mission step, or one cycle
  per mission with sub-cycles per step?
- [ ] Evaluate whether mission peripheral should be a 7th peripheral
  type alongside explore/edit/test/deploy/reflect/proof, or a
  meta-peripheral that orchestrates the others
- [ ] **From MAP:** Protocol vs configuration for cycle machine
  generalization. Protocol = each domain implements PeripheralRunner.
  Configuration = one generic runner parameterized by domain config.
  Current proof.clj is closer to protocol but structured like config.
- [ ] **From MAP:** Are the 9 phases the right decomposition for code
  missions? Resolve in DERIVE by examining actual code mission workflows.
- [ ] **From MAP:** Code mission ItemStatus values. Proposal:
  `[:enum :done :partial :open :blocked :abandoned]` — need to validate
  against actual mission lifecycle states.
- [ ] **From MAP:** Code mission EvidenceClass. Proposal:
  `[:enum :test :review :assertion :mixed]` — captures how evidence
  was obtained.
