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
