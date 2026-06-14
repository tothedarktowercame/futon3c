# E-mo-query-phylogeny — MathOverflow as a query phylogeny + QA benchmark

**Excursion (charter; longer-term, gated). Spun 2026-06-14 from a Joe↔claude-2
thread. Application of M-typed-holes' queries-as-scopes projection.**

## HEAD (Joe, 2026-06-14, verbatim sense)

MathOverflow gives *something akin to the pattern phylogeny, just at the level of
queries* — a rough map of the kinds of questions research mathematicians actually
ask. From that we can **generate queries-as-scopes to answer them from the
literature** and **prove against the MO corpus** (MO's accepted answers are the
ground truth). If that works, we reshape our own queries and **answer novel
questions in ArSE**.

## Why this is real, not hand-wavy

The processed corpus is already on disk: `storage/superpod-mo-processed.tar.gz`
(`mo-processed-gpu/`, RJ Meyers' superpod GPU pipeline, 2026-02-27): **95,321 MO
Q&A pairs**, 10,442 tags, 2.15M LaTeX fragments; with `hypergraphs.json`,
`scopes.json`, `entities.json`, `relations.json`, `thread-wiring-ct.jsonl`, and
`embeddings.npy` + a faiss index + a GNN model.

**The load-bearing finding:** MO's `scopes.json` uses the **SAME `hx/` hyperedge
schema as our golden-graphs** — e.g.
`{"hx/type":"quant/universal","hx/ends":[{"role":"quantifier","text":"all"},{"role":"symbol","latex":"i>0"}], …}`
and `{"hx/type":"bind/let","hx/ends":[…,{"role":"symbol","latex":"A"},{"role":"type","text":"a commutative ring with"}]}`.
So **no translation layer is needed**: the same `ScopeQuery`/typed-hole machinery
(`mathlib4/DarkTower/ScopeQuery.lean`, `scripts/scope_query_dogfood.py`) applies
to MO scopes directly. A question's scopes ARE partial-hyperedge patterns.

## The two things MO uniquely provides

1. **A query phylogeny** — aggregate the `hx/type` distribution across the 95k
   questions' scopes ⇒ the taxonomy of question-SHAPES mathematicians use
   (`quant/universal`, `bind/let`, `constrain/relation`, …). Pattern-phylogeny,
   at the level of queries.
2. **A ground-truth QA benchmark** — each MO thread is a Q→accepted-A pair. That
   makes queries-as-scopes QA *falsifiable*: did the scope-answer match the real
   accepted answer? A labeled validation set, not a demo.

## The arc (stages)

1. **Profile the query phylogeny** — `hx/type` (+ tag) distribution over MO
   scopes. Cheap, descriptive, no ML. Output: the taxonomy of question-kinds.
2. **Queries-as-scopes QA, validated** — reshape a sample of questions into
   scope-queries, answer from the literature (our CT hypergraph / substrate-2a /
   the MO corpus's own hypergraph), and **score against the MO accepted answers**.
   Two depths: *structural* (the `scope_query_dogfood` unifier over scopes) and
   *semantic* (embed + faiss-search the prepared `.npy`/index).
3. **Novel QA in ArSE** — once (2) validates, reshape our own queries and answer
   novel questions through ArSE (typed-bell query=open hole, answer=fill).

## First step (cheap, bounded — when picked up)

Stage 1 micro: extract `scopes.json` (selectively from the tarball), aggregate
`hx/type` counts → the query phylogeny; pick ~3 questions, reshape into
structural scope-queries, answer + eyeball against the MO accepted answer. No ML,
self-contained. Becomes a 4th IDENTIFY example for M-typed-holes (a real external
question answered as a scope).

## Constraints / dependencies

- **Longer-term + heavy:** 1.5 GB tarball (slow to stream; extract JSONs
  selectively, never unpack whole); the semantic depth needs numpy/faiss + maybe
  the embed model. Scope deliberately.
- **Coordinate with claude-1** before any substantial pipeline — this is futon6
  superpod-processed data, adjacent to the mining campaign.
- Builds on **M-typed-holes** (queries-as-scopes = projection #5; ArSE = the
  coord/illocutionary-hole projection). `substrate-2a` = mined math.ct corpus;
  MO is a *third* store of the same `hx/` shape.

## Acceptance (when run)

The query phylogeny (a real `hx/type` distribution), ≥1 MO question answered as a
scope-query and checked against its accepted answer, and an honest read on
whether structural-only suffices or semantic matching is needed. Commit artifacts.

*Cross-refs:* `holes/missions/M-typed-holes.md`,
`holes/missions/M-typed-holes-example-scope-query.md`,
`scripts/scope_query_dogfood.py`, `mathlib4/DarkTower/ScopeQuery.lean`,
`storage/superpod-mo-processed.tar.gz`.
