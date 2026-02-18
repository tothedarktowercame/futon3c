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
