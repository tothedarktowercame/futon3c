# Technote: Evidence Facetization

Date: 2026-02-18

## Problem

Evidence entries accumulate across sessions, repos, and timescales. A single
session might contribute evidence to three repos (futon1a, futon3, futon3c),
and a project might span many sessions. We need ways to slice evidence into
meaningful views — by project, by step within a project, by pattern, by
zone of work — without assuming these facets align with session boundaries.

## Existing Schema Mechanisms

The EvidenceEntry shape already has three natural facet axes:

### 1. `evidence/subject` (ArtifactRef)

Single-valued, typed reference to what this evidence is *about*:

```clojure
ArtifactRefType = [:enum :pattern :mission :component :gate
                         :session :agent :thread :evidence]
```

The `:mission`, `:component`, and `:evidence` types are particularly
relevant. A PSR done during a mission step could reference the step
directly:

```clojure
{:ref/type :evidence, :ref/id "step-3-xtdb-backend"}
```

Queryable via `GET /api/alpha/evidence?subject-type=mission&subject-id=X`.

### 2. `evidence/tags` (vector of keywords)

Multi-valued, cross-cutting. Any entry can belong to many facets
simultaneously:

```clojure
[:project/evidence-landscape :step/3 :repo/futon3c :psr]
```

Tags are the natural mechanism for evidence that participates in multiple
zones — e.g., a commit that touches futon1a and futon3c simultaneously,
or a PAR that reflects on cross-repo work.

### 3. `evidence/in-reply-to` (chain linking)

Single-parent causal chain. Already used for PSR → PUR linking. Can also
express project → step hierarchy when the subject field carries the
project context instead.

## Self-Referential Faceting

A project or step can itself be an evidence entry, making the landscape
self-describing:

```
Project:  {:evidence/type :coordination
           :evidence/claim-type :goal
           :evidence/body {:title "Evidence landscape implementation"}
           :evidence/id "proj-evidence-landscape"}

Step 1:   {:evidence/type :coordination
           :evidence/claim-type :step
           :evidence/in-reply-to "proj-evidence-landscape"
           :evidence/body {:title "Design EvidenceBackend protocol"
                           :ordinal 1}
           :evidence/id "step-1-backend-protocol"}

Step 3:   {:evidence/type :coordination
           :evidence/claim-type :step
           :evidence/in-reply-to "proj-evidence-landscape"
           :evidence/body {:title "Implement XTDB backend"
                           :ordinal 3}
           :evidence/id "step-3-xtdb-backend"}
```

Work evidence then references the step via subject, with tags for
redundant cross-referencing:

```
PSR:      {:evidence/type :pattern-selection
           :evidence/subject {:ref/type :evidence
                              :ref/id "step-3-xtdb-backend"}
           :evidence/pattern-id :agent/pause-is-not-failure
           :evidence/tags [:project/evidence-landscape :step/3]
           :evidence/id "psr-001"}

PUR:      {:evidence/type :pattern-outcome
           :evidence/in-reply-to "psr-001"
           :evidence/subject {:ref/type :evidence
                              :ref/id "step-3-xtdb-backend"}
           :evidence/tags [:project/evidence-landscape :step/3]
           :evidence/id "pur-001"}
```

The `in-reply-to` field stays free for causal chains (PSR → PUR), while
`subject` carries the project context. Tags enable flat queries without
chain-walking.

## Query Patterns

| Question | Query |
|----------|-------|
| All evidence in project X | `?subject-id=proj-X` or `?tag=project/X` |
| All evidence in step 3 | `?subject-id=step-3` |
| Step chain for project X | `GET /evidence/proj-X/chain` |
| All PSRs across projects | `?type=pattern-selection` |
| All PARs for an author | `?type=reflection&author=claude` |
| Cross-repo evidence | `?tag=repo/futon1a` (multiple tags per entry) |

## Tension: Single Subject vs Multi-Zone

The subject field is single-valued. An evidence entry that contributes to
multiple projects (or spans repos) must choose one subject and rely on
tags for the rest. This is adequate for most cases — most evidence has a
primary context — but doesn't model symmetric multi-zone participation
well.

Options if this becomes a real constraint:

1. **Tags are sufficient** — subject for primary, tags for secondary.
   Queries use tags for cross-cutting views. This is the simplest and
   probably correct for now.

2. **Add `evidence/contexts`** — a vector of ArtifactRefs, replacing
   single-valued subject. More expressive but complicates the schema and
   every query.

3. **External collection entries** — a "collection" evidence entry that
   references other entries (like a playlist). The collection itself is
   evidence, queryable and chainable.

4. **Fork for multi-zone** — `evidence/fork-of` already exists for
   branching. A single piece of work that lands in two zones could be
   represented as a fork: same content, different subject, linked via
   fork-of.

## Facet Context: Who Sets It?

Evidence needs to know what project/step/zone it belongs to. Possible
sources:

- **Explicit command**: `/project set evidence-landscape` sets a session
  variable that subsequent PSR/PUR/PAR entries inherit as tags.
- **Git branch inference**: `feature/evidence-landscape` → project tag.
  Works for single-repo work, not for cross-repo sessions.
- **CLAUDE.md annotation**: Project metadata in the repo's CLAUDE.md,
  automatically picked up by peripheral skills.
- **Per-command argument**: `/psr --step step-3 stuck on testing`.
  Most precise but highest friction.
- **Ambient from devmap**: If the devmap tracks active missions, the
  current mission(s) become default tags.

These are not mutually exclusive. A practical default might be:
git branch (if clean) + explicit override via command.

## Scope of Session vs Zone

Session boundaries don't align with project/zone boundaries:

- A single session may touch 3 repos (futon1a, futon3, futon3c)
- A single project may span 10 sessions over weeks
- A PAR reflects on a session but the work it describes may belong to
  multiple projects

This means `session-id` is a facet (useful for "what happened in this
conversation") but not *the* facet. Project, step, and zone are
orthogonal groupings that cut across sessions.

The evidence landscape already supports this — `session-id`, `subject`,
and `tags` are independent fields. The question is purely about
conventions and tooling for setting them consistently.

## API Gaps

To fully support facet queries, the HTTP API needs:

1. **Tag filtering**: `GET /api/alpha/evidence?tag=project/X` — filter
   entries that contain a specific tag. Currently tags are stored but
   not queryable via HTTP params.

2. **Pattern-id filtering**: `GET /api/alpha/evidence?pattern-id=agent/pause`
   — already in the data, not yet a query param.

3. **Subject key fix**: The GET endpoint currently checks `:ref/type`
   and `:ref/id` in post-filtering but the subject is stored with those
   keys inside `:evidence/subject`. Needs alignment (the XTDB Datalog
   query should handle nested map matching, or post-filter should use
   the correct path).

4. **Aggregation**: "How many entries per project?" or "Which patterns
   are used most?" — not essential for facet browsing but useful for
   dashboards.

## Application: Proof Stepper Ancillary Evidence

The proof peripheral (futon3c) maintains a formal proof tree: ledger
obligations, cycle phases, DAG dependencies, gate checks. But the
*reasoning context* around each proof step is richer than the tree
captures. Evidence facets provide this ancillary layer.

### Two kinds of stepper output

1. **Proof state** (the formal tree) — what's proved, what's open,
   what's blocked. Lives in `data/proof-state/{problem-id}.edn`.

2. **Evidence landscape** (ancillary context) — *why* each decision
   was made, what patterns were applied, what the corpus said. Lives
   in the evidence store, faceted and queryable.

The proof tree says "L-preconditions is :open, L-bridge is blocked."
The evidence landscape says *why*: "corpus-check returned no domain
signal, but discipline pattern `evidence-over-assertion` fired, and
we recorded a PSR documenting the decision to investigate rather than
assume."

### Facet mapping for proof steps

| Facet | In the stepper | Example |
|-------|---------------|---------|
| `subject` | The proof obligation being worked on | `{:ref/type :evidence :ref/id "L-preconditions"}` |
| `tags` | Cross-cutting context | `[:project/first-proof :step/1 :problem/P1 :discipline/framing-check]` |
| `in-reply-to` | Causal chain within the cycle | corpus-check → reframe decision → new approach |
| `evidence/type` | Coordination for corpus results, pattern-selection for discipline | PSR: "chose `evidence-over-assertion` because asserting mu ~ mu_0 without checking" |

### Two layers of corpus signal

The `:corpus-check` tool queries futon3a's ANN index during the
observe/propose phases of a proof cycle. Calibration on First Proof
Problem 1 (issue #11) revealed two independent signal layers:

**Domain signal** — "Is Cameron-Martin valid for Phi^4_3?" Requires
ArXiv-level content (Hairer 2014, Barashkov-Gubinelli 2020). Currently
RED: the pattern index doesn't contain this. When the superpod wiring
diagram embeddings are indexed, domain queries gain structural search.

**Discipline signal** — "Are you checking preconditions before building
on them?" This is a reasoning pattern already in the 852-pattern library.
Queries like "evidence before assertion" or "verify preconditions before
derivation" return useful patterns *today*.

The discipline layer prevents false confidence even without domain data.
It doesn't tell you the answer is NO, but it tells you to **stop and
investigate** rather than TryHarder on an unresolved assumption. The
domain layer, when available, tells you *what* the investigation finds.

The two layers compose: discipline patterns prevent false confidence
*now*, domain data catches specific errors *when available*. The stepper
gets incrementally more powerful as the corpus grows, but it's already
useful on day one.

### Evidence as learning across problems

When the stepper replays on a new problem, the evidence landscape from
previous problems is searchable. "Last time we hit a framing failure,
what discipline patterns helped?" becomes a facet query:

```
?tag=discipline/framing-check&type=pattern-selection
```

This returns PSRs from P1, P3, or any problem where a framing check
was applied — along with the PURs recording whether it worked. The
stepper learns from its own evidence trail.

### Concrete example: First Proof P1

Problem 1 asked whether the Phi^4_3 measure is equivalent to its
smooth shift. We answered YES (wrong); the correct answer is NO
(mutually singular). The error was a framing failure at Step 1.

In the stepper with evidence facets, the session would produce:

```clojure
;; 1. Corpus-check fires during observe phase
{:evidence/type :coordination
 :evidence/claim-type :step
 :evidence/subject {:ref/type :evidence :ref/id "L-preconditions"}
 :evidence/body {:tool :corpus-check
                 :args ["Phi^4_3 measure equivalent to Gaussian free field"]
                 :result {:neighbors [...] :source :futon3a}
                 :proof/operation-kind :observe}
 :evidence/tags [:project/first-proof :problem/P1 :step/1
                 :discipline/framing-check]}

;; 2. Discipline pattern fires (even without domain signal)
{:evidence/type :pattern-selection
 :evidence/subject {:ref/type :evidence :ref/id "L-preconditions"}
 :evidence/pattern-id :agent/evidence-over-assertion
 :evidence/body {:rationale "Asserting mu ~ mu_0 without evidence;
                             this is a precondition, not a given."}
 :evidence/tags [:project/first-proof :problem/P1 :step/1 :psr]}

;; 3. Decision: keep L-preconditions open, investigate
{:evidence/type :coordination
 :evidence/claim-type :step
 :evidence/subject {:ref/type :evidence :ref/id "L-preconditions"}
 :evidence/body {:decision :investigate-not-assume
                 :framing-status :unresolved
                 :blocked ["L-bridge"]}
 :evidence/tags [:project/first-proof :problem/P1 :step/1
                 :discipline/framing-gate]}
```

Steps 2 and 3 are ancillary evidence that doesn't live in the proof
tree but is critical for understanding *why* the stepper chose to
investigate rather than derive. Without evidence facets, this context
is lost between sessions.

## Application: Code Development Missions

The proof stepper application above shows evidence facets for *mathematical*
proof work. But the same machinery applies to *code development* missions.
Evidence when coding isn't just "unit tests pass" — it includes design
decisions, pattern selections, framing checks, corpus queries, and the
reasoning context around each step.

### Table 24 as Entity Grammar

Corneli (2014, §10.2, Table 24) presents an entity relation diagram for
PlanetMath 3.0 that provides the anchoring grammar. Key entities:

| Symbol | Entity | Evidence Analog |
|--------|--------|-----------------|
| X | project/object | Mission or step (`evidence/subject`) |
| A | article | Component or module under development |
| P | problem | Open question / obligation |
| J | conjecture | Hypothesis (`claim-type :hypothesis`) |
| S | solution | Proposed approach (`claim-type :goal`) |
| Q | question | Tension or blocker (`claim-type :tension`) |
| C | correction | Bug fix or design revision (`claim-type :correction`) |
| R | review | PAR or code review (`type :reflection`) |
| T | post | Forum post or coordination entry |
| H | heuristic | Pattern from the library (`evidence/pattern-id`) |
| G | group | Agent team / ad hoc collaboration |
| U | user | Agent or human author (`evidence/author`) |
| W | request | Task dispatch or handoff |
| E | ephemera | Session-scoped working notes |

Key relations map directly to evidence fields:

- **X → X♯** (project update) → `evidence/in-reply-to` chains
- **X → X′** (fork) → `evidence/fork-of`
- **X ⊧ X⋆** (outcome) → PUR entries linked to PSR
- **A ← P ← J ← S** (structure) → subject/tag hierarchy
- **S ↩ H** (solution uses heuristic) → PSR recording pattern selection

The thesis observes: "Any object, or activity, that takes place within the
system should be thought of as being part of some project. This carries over
to the 'micro' level, so that individual proof steps or computations are to
be thought of as small projects." This is exactly the self-referential
faceting described above — projects and steps as evidence entries, with
work evidence referencing them via `subject` and `tags`.

### Computational Agents and Table 24

The thesis's future work section (§10.6) explicitly calls for
"computational agents that are able to navigate the relevant mathematical
structures (as outlined Table 24), able to apply 'standard' and 'social'
problem solving heuristics, sufficiently metacognitively aware as to be
able to set and solve problems by generating and applying new heuristics
in the form of design patterns — and ideally able to annotate, reflect
on, diagnose, and extend the overall process."

This is precisely what the evidence landscape provides: agents navigate
the entity grammar (via faceted evidence queries), apply patterns (via
PSR), record outcomes (via PUR), and reflect on the process (via PAR).
The evidence landscape is the instrumentation layer that makes Table 24's
grammar *computational* rather than descriptive.

### Concrete mapping: code mission evidence

A code development mission (e.g., "implement XTDB backend for evidence")
produces the same evidence structure as a proof step:

```clojure
;; Mission definition (self-referential)
{:evidence/type :coordination
 :evidence/claim-type :goal
 :evidence/body {:title "Implement XTDB evidence backend"
                 :ordinal 1}
 :evidence/id "mission-xtdb-backend"}

;; Design decision (not captured by tests)
{:evidence/type :pattern-selection
 :evidence/subject {:ref/type :evidence :ref/id "mission-xtdb-backend"}
 :evidence/pattern-id :code-coherence/protocol-first
 :evidence/body {:rationale "Define EvidenceBackend protocol before
                             implementation — 7 operations, two backends"}
 :evidence/tags [:project/evidence-landscape :step/1 :repo/futon3c]}

;; Corpus check (what did the pattern library say?)
{:evidence/type :coordination
 :evidence/claim-type :observation
 :evidence/subject {:ref/type :evidence :ref/id "mission-xtdb-backend"}
 :evidence/body {:tool :corpus-check
                 :query "protocol design for pluggable backends"
                 :result {:patterns [:code-coherence/protocol-first
                                     :agent/evidence-over-assertion]}}
 :evidence/tags [:project/evidence-landscape :step/1
                 :discipline/design-check]}

;; Outcome (tests pass, but that's not the whole story)
{:evidence/type :pattern-outcome
 :evidence/in-reply-to "psr-protocol-first"
 :evidence/subject {:ref/type :evidence :ref/id "mission-xtdb-backend"}
 :evidence/body {:outcome :success
                 :evidence "534 tests pass, AtomBackend + XtdbBackend
                            both conform, persistence shim wired through
                            runtime-config"}
 :evidence/tags [:project/evidence-landscape :step/1 :repo/futon3c]}
```

The critical insight: "534 tests pass" is a fact, but the *design
decision* to use a protocol with two backends, the *corpus check* that
confirmed the pattern, and the *reasoning* about why this approach was
chosen — these are evidence that tests alone don't capture. Faceted
queries like `?tag=discipline/design-check&type=coordination` surface
this reasoning context for future missions.

## Appendix F: Critical Apparatus and Sigils

Corneli (2014, Appendix F) introduces a "critical apparatus" — Table 25
— with chess piece mnemonics for *para-mathematical* activities:

| Sigil | Activity |
|-------|----------|
| ♟ | Getting information |
| ♙ | Giving information |
| ♗ | Reputation building |
| ♖ | Relationship development |
| ♘ | Recreation |
| ♕ | Self-discovery |
| ♔ | Constructive feedback |

The thesis applies these sigils to code developer discussions, annotating
11 Planetary GitHub tickets to demonstrate that "much in the same way in
which the 'grammar' in Table 24 could be used to parse mathematical
activities, Table 25 can be used as part of a working language that
describes the 'para-mathematical' activities that are involved in
building and improving PlanetMath."

### Relevance to evidence facets

The critical apparatus suggests a faceting dimension we don't yet
capture: the *meta-activity* of each evidence entry. A PSR selecting
`evidence-over-assertion` is ♟ (getting information) and ♔ (constructive
feedback) simultaneously. A PAR is ♕ (self-discovery). A forum post
answering a question is ♙ (giving information).

These could map to evidence tags:

```clojure
:evidence/tags [:project/X :step/3
                :sigil/getting-information
                :sigil/constructive-feedback]
```

Or to a dedicated field if the vocabulary stabilizes:

```clojure
:evidence/sigils [:getting-information :constructive-feedback]
```

### The PlanetMath workforce problem, resolved

The thesis notes that the critical apparatus was designed for a context
with "too few people interested in software development." The futonic
system inverts this constraint: AI agents provide the development
workforce, while the critical apparatus provides the *legibility layer*
that makes their work understandable and inspectable. The evidence
landscape is the instrumentation that Corneli (2014) called for — agents
that "annotate, reflect on, diagnose, and extend the overall process."

The futonic rewrites demonstrate why this legibility matters. Getting
architecture right early (futon1a being a good example) saves enormous
rework. Evidence facets — especially design decisions, discipline
checks, and corpus queries — provide the inspectable trail that helps
both humans and agents judge whether an approach is sound *before*
building on it.

## Feature Grid: Table 25 vs Futonic Coverage

Table 25 is richer than the chess piece mnemonics alone. It has three
layers: participant activities (chess pieces), quality dimensions
(emoji sigils), and system concerns. Each issue in the Planetary
tracker was coded with whichever sigils applied — e.g., #381 (SQLite
instead of MySQL) was coded ✺ motivation because it lowered the
contributor onboarding barrier, not just because it was a database
swap. #88 (developer docs) was coded ♙ giving information.

The same approach applies to futonic commits and issues. Below is a
feature grid mapping each Table 25 dimension to what we already have
and what's missing.

### Participant Activities (Chess Pieces)

| Sigil | Activity | Futonic Coverage | Gaps |
|-------|----------|-----------------|------|
| ♟ | Getting information | `/rap`, `/psr` (corpus search), `corpus-check` tool, `ants/white-space-scout`, `agent/scope-before-action`, evidence GET queries | Well covered. The discipline/domain signal split (§ above) refines this further. |
| ♙ | Giving information | `/par`, PUR records, evidence POST, `contributing/*` patterns, `corps/working-where-others-can-see`, README/CLAUDE.md authoring | Well covered. Not explicitly *tagged* as information-giving though. |
| ♗ | Reputation building | `agent/commitment-varies-with-confidence`, gate pipeline (quality assurance), pattern library maturity levels | **Weak.** No agent reputation/credibility tracking. No way to ask "which agent's PURs have the highest success rate?" |
| ♖ | Relationship development | Forum participation, IRC bridge, agency registry, detach/reattach model | **Moderate.** Relationships are implicit in session co-participation. Not tracked as evidence. |
| ♘ | Recreation | `iching/*` (64 patterns), `iiching/*` (256 exotype patterns) | **Absent as explicit concern.** The I Ching patterns serve an associative/generative function but aren't framed as recreation. |
| ♕ | Self-discovery | PAR (explicitly this), `agent/state-is-hypothesis`, `corps/carrying-ones-own-question`, `corps/letting-the-trace-teach` | **Strong.** PAR is the primary self-discovery mechanism. Could be enriched by cross-session PAR queries. |
| ♔ | Constructive feedback | PUR (outcome evaluation), gate pipeline rejection, forum corrections, `agent/escalation-cost-vs-risk` | **Moderate.** Feedback exists but isn't structured for easy retrieval. "Show me all feedback on my design decisions" is hard. |

### Quality Dimensions

| Sigil | Dimension | Futonic Coverage | Gaps |
|-------|-----------|-----------------|------|
| ✋ | Relevance | Pattern search relevance scores, `agent/scope-before-action` | Relevance of evidence entries isn't scored. A PAR from 6 months ago and one from yesterday have equal weight. |
| ❦ | Quality | Gate pipeline (G5→G0), `code-coherence/*` patterns, `stack-coherence/*` | Quality is binary (pass/fail gates). No graded quality signal on evidence entries themselves. |
| ✨ | Scalability | Architecture patterns, `exotic/live-sync-source-truth` | Scalability as a *tag* on decisions isn't captured. "This design decision was made for scalability reasons" isn't faceted. |
| ❖ | Consistency | `stack-coherence/*`, `devmap-coherence/*`, 17 IFR patterns (f0-f7 sati through upekkha) | Best-covered quality dimension. The IFR stack is essentially a consistency framework. |
| ✺ | Motivation | Not explicitly tracked | **Missing.** "Why are we doing this?" is captured in PSR rationale but not queryable as a dimension. The ✺ sigil in Planetary #381 marks "this helps contributors get started" — a motivational concern, not a technical one. |

### System Concerns

| Sigil | Concern | Futonic Coverage | Gaps |
|-------|---------|-----------------|------|
| ☕ | Community | Forum, IRC bridge, multi-agent coordination, agency registry | Community dynamics aren't evidence-tracked. Who participated, how often, in what capacity — all implicit. |
| ⚓ | Concrete applications | `agent/evidence-over-assertion`, test discipline, proof stepper | Good in spirit but not tagged. "This evidence demonstrates a concrete application" vs "this is theoretical" isn't distinguished. |
| ⁂ | End-user focus | — | **Missing.** Developer-tool-for-developers doesn't naturally surface this, but it matters for futon4/Arxana UX decisions. |

### Coverage Summary

Strong coverage: ♟ ♙ ♕ ❖ — getting/giving information, self-discovery,
consistency. These are the core PSR/PUR/PAR/RAP loop and the coherence
patterns.

Moderate coverage: ♖ ♔ ❦ ☕ — relationships, feedback, quality,
community. The mechanisms exist but aren't faceted for retrieval.

Weak/absent: ♗ ♘ ✋ ✨ ✺ ⚓ ⁂ — reputation, recreation, relevance,
scalability, motivation, concrete applications, end-user focus. These
are the "para-development" dimensions that Table 25 surfaces and that
pure-technical tracking misses.

## Applying the Apparatus to Commits and Issues

The Appendix F approach — coding each issue with applicable sigils —
translates directly to how we create and tag commits and issues.

### Issue coding

Each GitHub issue could carry sigil tags indicating which Table 25
dimensions it engages:

```
## #42: Add evidence tag filtering to HTTP API

Sigils: ♟ (enables getting information from evidence landscape)
        ✨ (scalability — needed as evidence volume grows)
        ❖ (consistency — aligns API capabilities with stored data)

...issue body...
```

Or as GitHub labels: `sigil:getting-info`, `sigil:scalability`,
`sigil:consistency`. This makes the para-development dimension
searchable: "show me all issues motivated by scalability concerns"
becomes a label filter.

### Commit coding

Commits are already tagged via evidence (PSR/PUR entries reference
the work). Adding sigil tags to the evidence entry that accompanies
a commit enriches the faceted view:

```clojure
{:evidence/type :coordination
 :evidence/claim-type :step
 :evidence/body {:commit "abc1234"
                 :title "Add tag filtering to evidence GET endpoint"
                 :sigils [:getting-information :scalability :consistency]}
 :evidence/tags [:project/evidence-landscape :step/3
                 :sigil/getting-information :sigil/scalability]}
```

The sigils become queryable tags. "What work has been done for
motivation/onboarding reasons?" → `?tag=sigil/motivation`. "Which
commits involved constructive feedback?" → `?tag=sigil/feedback`.

### PAR as ♕ + ♔ generator

PARs naturally produce ♕ (self-discovery: "what did I learn?") and ♔
(constructive feedback: "what should change?"). Auto-tagging PARs with
these sigils makes the apparatus self-populating — agents don't need to
manually classify, the evidence type implies the sigil.

| Evidence Type | Default Sigils |
|---------------|---------------|
| `pattern-selection` | ♟ (always), ♔ (when selecting a discipline pattern) |
| `pattern-outcome` | ♔ (always), ♕ (when outcome was surprising) |
| `reflection` | ♕ (always), ♔ (always) |
| `coordination` | ♙ (always) |
| `gate-traversal` | ❦ (always), ❖ (always) |
| `forum-post` | ♙ or ♟ (depends on direction) |
| `conjecture` | ♕ (always) |

This auto-classification means the feature grid fills in progressively
as agents do normal work, rather than requiring explicit annotation of
every commit and issue.

## Decision Log

*Decisions deferred. This technote captures the design space for future
reference.*

- [ ] Choose primary facet context mechanism (explicit command, git
  branch, CLAUDE.md, ambient)
- [ ] Decide if single subject + tags is sufficient or if
  `evidence/contexts` is needed
- [ ] Add tag and pattern-id query params to HTTP API
- [ ] Design `/project` command (or equivalent) for setting context
- [ ] Consider Arxana viewer facet navigation (filter by project/step
  in the evidence timeline)
- [ ] Wire `:corpus-check` evidence entries into the facet system
  (currently recorded as step evidence in proof peripheral; needs
  subject/tag population from cycle context)
- [ ] Add `:discipline/framing-check` and `:discipline/framing-gate`
  as standard tags for stepper sessions
- [ ] Build facet query for cross-problem discipline pattern reuse
- [ ] Map Table 24 entity types to EvidenceEntry claim-types formally
  (some are 1:1, some need new claim-types like `:request`)
- [ ] Evaluate sigils (Table 25) as evidence tags vs dedicated field
- [ ] Define standard tags for code mission evidence
  (`:discipline/design-check`, `:discipline/architecture-review`, etc.)
- [ ] Build facet query for cross-mission design decision reuse
  (`?tag=discipline/design-check&type=coordination`)
- [ ] Implement sigil auto-classification for evidence types
  (evidence type → default sigil tags, per table above)
- [ ] Add GitHub label set for Table 25 sigils
  (`sigil:getting-info`, `sigil:giving-info`, `sigil:reputation`,
  `sigil:relationships`, `sigil:recreation`, `sigil:self-discovery`,
  `sigil:feedback`, `sigil:relevance`, `sigil:quality`,
  `sigil:scalability`, `sigil:consistency`, `sigil:motivation`)
- [ ] Address ♗ reputation gap: agent credibility scoring from
  PUR success/failure history
- [ ] Address ✺ motivation gap: explicit "why" tagging for design
  decisions that aren't technically motivated
- [ ] Consider ♘ recreation dimension — is there a role for
  associative/generative play in agent development? (The I Ching
  and exotype patterns hint at this but aren't framed as such)
