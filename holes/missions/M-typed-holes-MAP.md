# M-typed-holes — MAP (2026-06-14)

Status: complete

*Phase 2 per `futon4/holes/mission-lifecycle.md`: survey what exists; produce
facts, not decisions. The detailed IDENTIFY is the charter + the four worked
exemplars + the residue analysis + the mathlib audit. This MAP inventories the
territory the unified `(typed-hole, fill)` datatype must cover, as a
ready-vs-missing map, and answers the survey questions. All findings are
concrete (artifacts/counts/shas from this session), not speculation.*

## 1. Inventory — existing infrastructure (per projection + substrate + Lean)

| projection | existing implementation | where |
|---|---|---|
| mining (satiety) | scope detector emitting `hx/` hyperedges; `:hungry-for` grades | `scripts/dp_capabilities/scopes.py`, `nlab-wiring.detect_scopes`; futon6 `data/mission-triples/*.edn` |
| proofs (sorries) | sorry-typing; distributed-proofreaders | `E-substrate-2-sorry-typing`, `M-distributed-proofreaders` (futon6) |
| grounding (C-SYM-GROUND) | binder/appositive/def-eq grounding; the classifier | futon6 `dp_paper_view.py`, `dp_capabilities/binders.py`, `anatomy_v0_sweep.py` |
| combs (`:composes`) | mined wiring; BV-combs typing | `mission_triple_miner.py`; BV-combs excursion (futon6 `9cd66b5`, `holes/bv-comb-typing.edn`) |
| queries (scopes) | structural scope-query runtime; the hyperedge store | `scripts/scope_query_dogfood.py`; XTDB / substrate-2 store (:7071); golden-graphs |
| coord (typed bells) | typed-bell protocol + ArSE Q&A + bell router | `M-typed-bells` (`b4ed15f`), `README-arse.md`, `src/futon3c/peripheral/arse.clj`, `agency/bell_router.clj` |
| **the datatype (all)** | **`(typed-hole, fill)` formalised** — 9/10 concepts | **`mathlib4/DarkTower/`**: TypedHole/Fill/Comb/Discharge/ScopeQuery/BV + Examples + FirstFlightsExample (0 sorry, `lake build` green) |

Shared substrate fact: the **`hx/` hyperedge schema is uniform** across the
golden-graphs, the scope detector, AND the MO processed corpus (`hx/type`,
`hx/ends [{role …}]`, `hx/labels`) — one schema, several stores.

## 2. Inventory — existing data (counts)

- **Typed holes already mined:** `:hungry-for :parse`×80, `:payoff`×26, `:canon`×2,
  `:bundling`×2, `:role`×1 (futon6 mission-triples).
- **Mined CT (substrate-2a-in-progress):** hand-authored golden-graphs (8 papers,
  `hx/` schema); futon6 dp goldens (261 papers, ~66% grounded pooled); the live
  laptop structure-mining run (in progress, ~today).
- **`:composes` wiring:** 4 missions carry it (M-first-flights 23 ckpts, +3);
  all linear chains (BV-combs finding).
- **MO processed corpus:** `storage/superpod-mo-processed.tar.gz` — 95,321 Q&A
  pairs, 10,442 tags, 2.15M LaTeX fragments, scopes in our `hx/` schema, +
  embeddings/faiss/GNN.
- **Coordination Q&A:** ArSE backs onto typed bells (9 illocutions:
  query/answer/assert/challenge/agree/define/retract/suggest/request).

## 3. Ready vs Missing

| READY (no new code) | MISSING (the actual work) |
|---|---|
| typed-hole datatype in Lean (DarkTower, 9/10) | the 10th: `illocutionary-hole` (wire typed-bells ↔ typed-hole) |
| `hx/` scope schema, uniform across stores | a **unified fill operator** runtime (today: 6 separate fills) |
| hole-type already mined (`satiety`/`:hungry-for`) | satiety→`:composes` projection in the *live* miner (the futon6 first rung; gated, claude-1) |
| structural scope-query runtime (the dogfood) | the ArSE ↔ ScopeQuery ↔ store **proving loop** (answer-by-query, recorded) |
| ArSE + typed bells (the coord runtime) | substrate-2a import of math.ct (gated on the mining wrap) |
| mined CT stores (golden-graphs; goldens; MO) | MO query-phylogeny + QA benchmark (`E-mo-query-phylogeny`) |
| 4 worked exemplars (mission/paper/fold/query) | faithful-fold (pattern-parameterised); `Sum`-typed paper store; BV proof-theory |

## 4. Survey questions (answered with concrete findings)

- **Q1 — One datatype or six?** Conceptually six implementations historically;
  now **one** Lean datatype (DarkTower, 9/10 concepts) typing all of them — but
  the six *runtime* surfaces are still separate. The unification is proven and
  exemplified, not yet runtime-shared.
- **Q2 — Do the stores share a schema?** **Yes.** golden-graphs, the scope
  detector, and the MO corpus all use the `hx/` hyperedge schema; substrate-2 is
  the hyperedge store. So the query layer is schema-uniform across stores — no
  per-store translation.
- **Q3 — Is the hole-type already produced by the pipeline?** **Yes** —
  `satiety`/`:hungry-for` is the mined hole-type; it just isn't projected onto the
  `:composes` wiring (BV-combs gap #1).
- **Q4 — Is there a runtime that answers a scope-query over a store?** Structural:
  **yes** (`scope_query_dogfood`, verified on real golden-graph; matches the Lean
  `ScopeQuery.answers`). Semantic: the MO corpus ships embeddings + faiss but no
  unified production query engine exists yet.
- **Q5 — Fill: one operator or many?** Many (six). Lean `Fill` formalises the
  *atomic* grain (`PFunctor.comp`) and `FirstFlightsExample.fold` the *fold*
  grain; **no single runtime fill** wired across the projections.
- **Q6 — What gates the proving arc?** (a) substrate-2a import (gated on the
  laptop mining wrap, ~today); (b) the ArSE↔ScopeQuery wiring (unbuilt). Neither
  is conceptual — both are wiring.

## 5. Surprises (recorded before DERIVE locks in)

- **MO scopes are already in our `hx/` schema** (discovered this session) — the
  external-corpus QA needs *no* translation layer. Big enabler.
- **The datatype is free from mathlib:** a typed hole *is* a `PFunctor`
  (Poly position+directions) — so formalisation was wrapping, not building.
- **Two domains collapse to one query layer over two stores** (substrate-2 = the
  stack; substrate-2a = mined math.ct; MO = a third store) — missions and papers
  are not two theories.
- **The grounding "Greek 18.2%" backlog was stale** — measure-first beats
  assume (relevant discipline for DERIVE).

## 6. Query-substrate status — addendum (2026-07-04)

Survey of the XTDB query layer under projection #5 (queries/scopes), added after
MAP closed to feed the store-agnostic-query-layer question DERIVE inherits.
Verified against source; file:line trail in-session.

- **Engine version.** Every store is **XTDB 1.24.x / Datalog** (`futon1a`,
  `futon3b`, `futon1/apps/*`); all query sites are `{:find … :where …}`. **XTQL
  is XTDB 2.x** — absent from the tree, no usage, no migration note. So "adopt
  XTQL" ≠ a query-layer tweak; it is a 1.x→2.x engine migration
  (RocksDB/LMDB → columnar). The store-agnostic layer is the seam where a future
  2.x on-ramp would live, if ever wanted.
- **"Graph query ≫ read text" is already true for the cheap grain.** Measured on
  the live store: bound-type count-pushdown (`/census`) and ids-only-index +
  lazy-pull answer "how many / which X" over 100k+ rows in ~ms; the evidence
  backend hits **~10 ms vs 15–20 s** on the *same* store purely by staying
  index-bound. The primitive that justifies the ambition exists and is proven.

**Answerable by one query today vs. needs work** (sharpens Q4 and the §3 MISSING column):

| one graph query today | the query-layer gap |
|---|---|
| type population (`/census`); all-of-type (`?type=&limit=`); 1-hop `?end=` (exact); 1-hop ego; thread / reply-chain projection | server-side **multi-hop / prefix / join** — none exist |
| ns→vars prefix sweep; code-structure `db-as-of(commit)` — **Drawbridge/REPL Datalog only** | those are **not HTTP-exposed**; the API's `?end=` is exact-match only |
| — | **recursive rules absent** — "callers of / transitive deps of" has no `:rules`; multi-hop degrades to app-side **N+1 HTTP** (`enrich-file`) |
| — | **time-travel over code *contents*** is an open defect (code graph is a HEAD mirror; `db-as-of` covers commits/evidence, not code text — `E-substrate-2-timetravel`) |

**Net for DERIVE.** Stores + `hx/` schema are ready (as MAP found) and the cheap
query grain is fast; the unbuilt piece the "answer-by-query" loop needs is
**server-side traversal** (multi-hop, prefix, recursive rules, HTTP-exposed
`db-as-of`) — today these live only in the REPL. That is the query-layer half of
the ArSE↔ScopeQuery↔store proving loop MAP handed forward.

## Exit

Every survey question has a concrete answer; the ready/missing table is complete.
**What MAP hands to DERIVE:** the core design problem is now sharp — *not* the
datatype (it exists, 9/10, Lean-checked) but the **single runtime `fill`
operator** that the six projections share, and the **ArSE↔ScopeQuery↔store
proving loop** that makes "answer a query against a store" = "fill a typed hole"
in production. DERIVE designs those two, plus the `illocutionary-hole` wiring that
ties ArSE (coord) into the datatype. The stores and schema are ready; the work is
the shared operator + the loop.

*Cross-refs:* `M-typed-holes.md` (charter/IDENTIFY), `*-lean-manifest.edn`,
`*-mathlib-audit.edn`, the four `*-example-*.md`, `E-mo-query-phylogeny.md`,
`futon4/holes/mission-lifecycle.md` (the phase spec).
