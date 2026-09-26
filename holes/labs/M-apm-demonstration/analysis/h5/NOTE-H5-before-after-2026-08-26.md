# H5 — the store measured before and after, with V2's instruments and one new one

*claude-19, 2026-08-26. Joe: "please repoint, but please do measurements per
memory whitepaper v2 before and after." Three store states, all frozen here:*

| state | what had happened | attachments capture | why-relations capture |
|---|---|---|---|
| **before** | nothing (`system-as-of=2026-08-26T16:40:00Z`, before H5b step 2 began at 16:42Z) | `memory-assert-asof-2026-08-26T1640Z.edn` | `why-relations-before-h5a-2026-08-26.edn` |
| **after H5b** | seven hub statements reassigned to their API patterns (codex-20, reviewer `codex-20`, `review_codex_lane_attachments.clj --verdict reassign`) | `memory-assert-after-h5b-2026-08-26.edn` | same as before (H5b does not touch relations) |
| **after H5a** | four `@why math-strategy/missing-dependency-protocol` declarations repointed (futon3 `1b75c1f` for the three tracked files; `frontier-bound-from-arc-hypotheses.flexiarg` repointed in place, untracked as it was); the four stale relation documents retracted via `futon3c.watcher.multi/retract-documents!`; the four files re-ingested via `futon3c.watcher.file-ingest/ingest-flexiarg!` | `memory-assert-after-h5a-2026-08-26.edn` | `why-relations-after-h5a-2026-08-26.edn` |

`system-as-of` on `/api/alpha/hyperedges` is honoured (the as-of and live
captures differ exactly in the seven reassigned edges), so the "before"
state is a read of the store's own history, not a reconstruction.

## Instrument 1 — V1/V2's connectivity meter (memory/assert graph)

`holes/labs/M-typed-memories/connectivity_meter.bb` is the instrument behind
V1's λ₂ table and V2 §4.5–4.6. Its Jacobi sweeps did not finish on a 696-row
export (killed at 8 min), so `laplacian_meter.py` here reproduces its exact
definitions — nodes = memories ∪ patterns ∪ distilled `e-` targets; edges =
memory→pattern and memory→distills, deduplicated; reviewed = some version is
`:current` and `:reviewed`; components over reviewed edges; unnormalised
`L = D − A` on the largest component; λ₂ = second-smallest eigenvalue — with
numpy (`futon6/.venv`).

| | before | after H5b | after H5a |
|---|---:|---:|---:|
| current reviewed edges | 484 | 484 | 484 |
| distinct memories with a current reviewed attachment | 277 | 277 | 277 |
| attachments per memory | {1: 260, 2: 17} | {1: 260, 2: 17} | {1: 260, 2: 17} |
| distinct patterns carrying memories | 56 | 56 | 56 |
| hub (`missing-dependency-protocol`) attachments | 47 | 40 | 40 |
| reviewed components | 501 | 501 | 501 |
| largest reviewed component (nodes / edges / patterns) | 342 / 422 / 21 | 342 / 422 / 21 | 342 / 422 / 21 |
| λ₂ of largest component | 0.024095 | 0.023939 | 0.023939 |
| edge types in it | attachment, distills | same | same |

Readings. H5b moved seven attachments between patterns inside the same
component (λ₂ 0.0241 → 0.0239, a rounding-level change); H5a touches no
memory/assert edge, so this instrument cannot see it at all — which is the
first result: **V2's structural instrument does not observe the pattern
language's own edges.** Its graph is memory↔pattern; `@why` lives in
`pattern/has-semantic-why` relations, a different table.

The second result is about V2 itself. V2 §4.6 (2026-08-01) measured "a
forest of stars … the largest component of the patterns-only projection is a
single hyperedge", λ₂ = 1.0 by construction. Today the largest reviewed
component has **342 nodes and 21 patterns**, joined by 17 multi-attached
memories and the `distills` edges, with λ₂ ≈ 0.024 — below V1's 0.1 floor, in
the direction V2 §4.5 predicted (λ₂ falls as richness grows). The forest of
stars is no longer the store's shape; the APM campaign populated it. That
sentence in V2 §4.6 is now a dated observation and V3 §4a should say so.

(The hub count here is 47 before rather than the expander's 48: the meter
requires `:state :current`, the expander's `reviewed-attachment?` does not,
and one hub attachment has a superseded reviewed version. The expander
dedups by memory, so its route count is unaffected; noted for whoever
compares the two.)

## Instrument 2 — the why-graph (new; `why_graph_metrics.py`)

The expander's BFS, replayed offline: outgoing `has-semantic-why` from f42's
23 seed patterns; a reached pattern counts only if it carries a current
reviewed attachment.

| | before | after H5b | after H5a |
|---|---:|---:|---:|
| why relations | 45 | 45 | 45 |
| hub in-degree | 4 | 4 | **0** |
| why-reachable patterns from f42 seeds | 4 | 4 | 3 |
| … with attachments | 1 (the hub) | 1 (the hub) | **0** |
| memories reachable by why-hop | 47 | 40 | **0** |

## Instrument 3 — the expander itself (f42a → f42b → f42c)

`scripts/apm-cascade-dry-run.sh` over the same f42 snapshot, cap 1000:

| run | after | available | why-hop | via | co-incidence |
|---|---|---:|---:|---|---:|
| f42a (`77a1bac0`) | — | 103 | 48 | hub only | 55 |
| f42b (`holes/f42b-cascade-run-cap1000.edn`) | H5b | 96 | 41 | hub only | 55 |
| f42c (`holes/f42c-cascade-run-cap1000.edn`) | H5a + H5b | 96 | **0** | — | 96 |

Two things fell out of f42b that the plan did not predict:

1. **The seven reassigned statements are now invisible to the cascade.**
   Their new patterns (`measure-integration-api`, `holomorphic-disk-api`,
   `connectedness-component-api`) are f42 *seed* patterns, and the expander
   offers routes from seed patterns to *other* patterns only — a memory
   attached to a seed pattern but absent from the shelf has no route (it is
   neither a leaf nor a why-hop nor a co-incidence). Before H5b they arrived
   as why-hops from the hub; after it they arrive nowhere. The sibling gap:
   the cascade never offers "the other memories on the patterns you already
   have", which is the one expansion a reader of a pattern language would
   expect first.
2. **The co-incidence count is unchanged at 55 → 96 only by the loss of
   why routes**: the same 96 memories were available by co-incidence all
   along; cheapest-route dedup credited 41 of them to the why route while it
   existed. Co-incidence is the store's real connective tissue, exactly as
   the PLAN's whitepaper check warned.

## What H5 established

- **H5a did what it was for.** The protocol is no longer every API pattern's
  parent; from an API-heavy shelf the why-graph now reaches
  `reduce-to-known-result` and two siblings, none of which carries a memory.
  That is the honest state of the pattern language over this store: 45
  authored edges over 43 patterns, memories on 56 patterns, and — after the
  hub is removed — **no pattern that is both why-reachable from a working
  shelf and populated**.
- **Descent cannot narrow until the middle exists.** The next population
  step is not more attachments but why-edges *among* the API patterns and
  memories on the `math-informal` / `math-strategy` tier they now point to;
  until then a why-hop cascade over this store is empty and a co-incidence
  cascade is a flood of 96.
- **The expander needs a sibling route** (hop 0, same pattern, not on the
  shelf) before H3 is worth revisiting; without it, attaching a memory
  *correctly* removes it from the cascade.
- **V2 §4.6's shape claim is superseded**: the store has one 342-node
  reviewed component today.

## Reproduce

    H=holes/labs/M-apm-demonstration/analysis/h5
    /home/joe/code/futon6/.venv/bin/python $H/laplacian_meter.py $H/memory-assert-asof-2026-08-26T1640Z.edn
    python3 $H/why_graph_metrics.py $H/why-relations-after-h5a-2026-08-26.edn $H/memory-assert-after-h5a-2026-08-26.edn holes/f42a-cascade-run-cap1000.edn
    scripts/apm-cascade-dry-run.sh data/apm-campaigns/jit-all-open-nontopology-v1/jit-all-open-nontopology-v1-f42/snapshots/f42-solver-memory.edn 1000 /tmp/f42c-rerun.edn
