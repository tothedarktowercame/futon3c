# PLAN H5 — populate the graph before exploiting it (design note)

*claude-19, 2026-08-26. Successor to PLAN-apm-cascade-demo-instance §H5,
made specific by the H4 judgement (`holes/f42a-H4-judgement-2026-08-26.md`)
and the real run (`holes/f42a-cascade-run-cap1000.edn`). Ordering per Joe:
H0 → H1 → H4 → H2 → **H5** → H3, H3 last.*

## What H4 found, restated as a graph fact

On f42 the cascade's entire why-hop expansion — 48 memories — arrives through
one node, `math-strategy/missing-dependency-protocol`, and the same 48 arrive
on f10 and f15. Two things make that node a hub:

1. **Four library files declare `@why math-strategy/missing-dependency-protocol`**
   (`futon3/library/`, all authored 2026-08-17, `53ef3cd`):
   `math-formalization-CA/measure-integration-api`,
   `math-formalization-CV/holomorphic-disk-api`,
   `math-formalization-FA/weak-convergence-hilbert`,
   `math-formalization-CV/frontier-bound-from-arc-hypotheses`.
   The expander walks `has-semantic-why` *outgoing* from seed patterns, so any
   shelf touching an API pattern reaches the hub in one hop.
2. **48 reviewed memories are attached to the hub and to nothing else.** 41 are
   process rules from the codex-sorry-loop era; 7 are mathematical statements
   whose subject is a specific API region, attached to the *protocol under
   which they were mined* rather than to what they are about.

The store's own note on the hub says what it is for: "Mathlib lemma not found,
missing theorem after search, API gap, bounded search budget" — a protocol. An
API index pattern "exists to serve" a search protocol only in the sense that
it was written during one; that is provenance, not a why.

## Two levers, and they do different things

| lever | what changes | effect on the f42 cascade | who decides |
|---|---|---|---|
| **H5a — repoint the four `@why` edges** | library files; watcher syncs to `pattern/has-semantic-why` | the hub stops being why-reachable from API patterns; why-hop count on f42 drops from 48 to whatever the remaining seed out-edges reach (H0 re-run measures it) | **Joe** — the edges are his authored declarations (`cascade-formation-patterns.md` Pattern 4: repointing is a reviewed editorial act with the file as source of truth) |
| **H5b — reassign the 7 mathematical statements** | substrate attachments (`review-attachment!` verdict `:reassign`, reviewer ≠ depositor) | none by itself on the why-hop count (the hub still carries 41); makes the 7 reachable by pattern-arm recall for their actual API, and by a why-hop from any seed that reaches that API pattern | claude-19 proposes; a Codex reviewer seat applies after dry-run review |

H5a is the one that makes descent narrow. H5b is the one that puts content
where a reader would look for it. Both are population, not design (V2 §4.6).

### H5a — proposal for Joe (not executed)

Repoint the four `@why` lines from `math-strategy/missing-dependency-protocol`
to **`math-informal/reduce-to-known-result`** — which is the hub's *own* `@why`.
An API index exists so that a step can be reduced to a known result; the
missing-dependency protocol is what you run when that reduction fails. That
makes the protocol a sibling of the API patterns under `reduce-to-known-result`,
not their parent. `frontier-bound-from-arc-hypotheses` (a CV pattern coined for
f42's crux) may deserve `@why math-formalization-CV/holomorphic-disk-api`
instead — it is a specialisation of disk-boundary reasoning, and its `@see-also`
already says so.

Acceptance for H5a: after the watcher syncs, `scripts/apm-cascade-dry-run.sh`
over the f42 snapshot reports why-hop routes from more than one pattern **or**
why-hop 0 with an explicit record that the seeds' out-edges now reach no
attached pattern. Either is a result; 48-from-one-hub again is a failed sync.

### H5b — the seven reassignments (proposal; verify each hook before applying)

| memory | mined on | proposed patterns |
|---|---|---|
| `e-codexpilot-poisson-ae-convergence-bridge` | a94J04 | `math-formalization-CA/measure-integration-api` |
| `e-codexpilot-integral-minkowski-eLpNorm-bochner` | a96A04 | `math-formalization-CA/lp-norm-comparison` |
| `e-codexpilot-lemniscate-sublevel-components-inject-into-roots` | lemniscate | `math-formalization-GN/connectedness-component-api` |
| `e-codexpilot-lemniscate-superlevel-preconnected` | lemniscate | `math-formalization-GN/connectedness-component-api` |
| `e-codexpilot-zeroCountInClosedBall-homotopy-invariant` | rouche-root-count-transfer | `math-formalization-CV/holomorphic-disk-api` (+ `math-formalization-CA/divisor-sum-to-root-count-without-monic` if its hook fits) |
| `e-codexpilot-bridge-logarithmic-derivative-circle-integral-to-divisor-count` | rouche-root-count-transfer | same as above |
| `e-codexpilot-lift-the-circle-submean-bound-to-a-disk-area-bound` | a01A07 | `math-formalization-CV/holomorphic-disk-api` |

**Drop the hub from these seven, do not multi-attach.** Multi-attachment is
representable and V2 §4.6 argues for it, but here the hub attachment records
the mining context, which `:provenance` already carries; keeping it keeps the
star for the memories whose only real subject is the API. The review residual
should say exactly that.

Constraints from `memory_lifecycle.clj`: the review evidence must be authored
by the reviewer identity that invokes `review-attachment!` (`:agent-id` ctx =
evidence author = session), reviewer ≠ memory author (the codex-lane memories
carry author `claude-6`), `:domain :mathematics`, verdict `:reassign` with the
new `pattern-ids`. `scripts/review_codex_lane_attachments.clj` has the ctx,
dry-run and commit shape but only `approve|reject`; it needs `reassign` +
`--pattern-ids`. None of the seven is on the live campaign's shelf (0 of f42's
104 final entries are hub members), so no snapshot's `candidate-visible?`
check is affected.

## Acceptance for H5 as a whole: f42b

Re-run `scripts/apm-cascade-dry-run.sh` over the same f42 snapshot after
H5a/H5b and commit `holes/f42b-cascade-run-cap1000.edn`. Report beside f42a:
why-hop count, distinct why-reachable patterns, co-incidence count, and
whether the seven reassigned statements now arrive by a why-hop from a seed
that shares their API pattern. The claim available afterwards is still only
"built with structure", not "used" — H3 remains last.

## Not in H5

- Homes for the 41 process rules. They are honestly about the protocol; the
  problem is the protocol being everyone's parent (H5a), not their attachment.
- Any change to what a student is handed (H2 covers the shelf; H3 the cascade).

---

*Status, 2026-08-26 (claude-19):* **H5a done** (Joe authorised; futon3 `1b75c1f`;
relations retracted and re-ingested; hub in-degree 0). **H5b done** (codex-20,
seven reassignments, hub 47 → 40). **f42c**: why-hop 0, co-incidence 96.
Measurements before/after with V2's instrument and the why-graph:
`holes/labs/M-apm-demonstration/analysis/h5/NOTE-H5-before-after-2026-08-26.md`.
Acceptance met in its second form (why-hop 0 with the reason recorded). Two
new items for the plan: (i) the expander needs a sibling route (hop 0, same
pattern, not on the shelf) before H3 is worth revisiting; (ii) the population
step that would make descent narrow is `@why` edges among API patterns and
memories on the `math-informal`/`math-strategy` tier — not more attachments.
