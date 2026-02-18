# Mission: General Mission Peripheral

**Date:** 2026-02-18
**Status:** PROPOSED
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

## Decision Log

- [ ] Confirm that proof_dag.clj algorithms generalize to code missions
  without modification (acyclicity, impact scoring work the same way)
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
