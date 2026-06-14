# Mission: M-typed-holes

**Status:** IDENTIFY (charter, 2026-06-14) — spun out of a Joe↔claude-2 design
conversation. No implementation yet; this records the unification + the first
data-backed rung.
**Owner:** claude-2 (charter author); Joe + agents for refinement/review.
**Repo:** futon3c (coordination/stack home, next to `M-typed-bells`); consumes
futon6 mining data and futon5a theory.

## HEAD (Joe, 2026-06-14, verbatim sense)

A **"hyperedge with holes"** is the thing we keep building all over the stack
without naming it. We have **five — six with `M-typed-bells` — separate
vocabularies for the same structure**, each with its own consumer. The thing to
create is **M-typed-holes with full generality**: one `(typed-hole, fill)`
datatype, with the existing six surfaces as projections of it. It is
**practical** (a half-dozen real consumers, `M-first-flights` the one that tries
to be formal — ML/cascade-based once the cascade lands), **theoretically
backed** (`E-the-dark-tower-2`, Poly), and **data-backed** (the math.ct mining
already emits typed holes).

## The gap

The same object — *a node carrying a set of typed slots awaiting fillers* — is
reimplemented six times, and the **fill** operation (supply a slot with a
well-typed filler) is reimplemented six times with it. Six vocabularies for one
structure means six engines where there should be one, and no shared query/
composition algebra across them.

## The unified datatype (the claim)

A **typed hole is a polynomial-functor position-with-directions.** A polynomial
`p = Σ_{i ∈ p(1)} y^{p[i]}` is exactly "a set of positions (nodes), each
carrying a set of *typed directions* (holes)." So:

- a **hyperedge-with-typed-holes** = a position with directions = an element of **Poly**;
- the **hole's type** = the type of a direction (in the mining: `:hungry-for X`);
- **fill** = substitution / composition in Poly; a **comb/lens** = a Poly morphism;
  **query→answer**, **sorry→proof**, **ungrounded→binder** are the *counit*
  (the discharge/fill that consumes a hole).

This makes `E-the-dark-tower-2`'s substrate (Poly; the differentiable /
tangent-categories ↔ Poly program) the **native semantics** of M-typed-holes,
not a separate theory leg — "differentiable" gets a referent (the tangent at a
position), and the typed-hole datatype is where it bites.

## The six projections (one structure, six rooms)

| # | Consumer | The hole | Its type | "fill" = | Anchor |
|---|---|---|---|---|---|
| 1 | CT mining | a hungry node | `:hungry-for :parse/:payoff/:canon/…` | the cascade feeds it | futon6 mission-triples (`:parse`×80, `:payoff`×26, `:canon`×2, `:bundling`×2, `:role`×1) |
| 2 | Proofs | a `sorry` | the proposition owed | discharge with a proof | `E-substrate-2-sorry-typing`, `M-distributed-proofreaders` |
| 3 | Grounding | an ungrounded symbol (C-SYM-GROUND) | the concept it should bind | the use→binder edge | futon6 `dp_paper_view` grounding (this session, 0809.2517 79%→92%) |
| 4 | Wiring / combs | a `:composes` hole `⟨A;-;B⟩` | dom/cod boundary | compose a sub-process | `M-first-flights`; BV-combs excursion (futon6 `9cd66b5`) |
| 5 | Queries / scopes | a query variable end | the role/kind it sits in | unify against the store | Arxana `(Joe knows ?)`; XTDB hypergraph store |
| 6 | Coordination | a `type=query` bell | illocutionary expectation (IATC) | a `type=answer --ref` bell | `M-typed-bells` (COMPLETE, `b4ed15f`); ArSE |

The single prize: today **fill** is six functions (cascade-feed, proof-discharge,
symbol-ground, comb-compose, query-answer, bell-reply). M-typed-holes makes **one
fill operator with six projections** — and the instant query/answer *is* that
operator, the XTDB query layer and the cascade are the same engine.

## The three legs (the same object, three views)

- **Practical** — the six consumers; `M-first-flights` the formal primary
  (cascade → ML later); the query layer Joe wants over XTDB is projection #5.
- **Theoretical** — Poly / `E-the-dark-tower-2`: positions-and-directions, fill =
  composition, comb = lens; turns-as-typed-processes.
- **Data** — the mining already *emits* typed holes (satiety), so the datatype is
  populated, not hypothetical.

## First rung (data-backed, buildable, bounded)

**Project `satiety :hungry-for` onto the `:composes` interface.** The BV-combs
excursion's gap #1 was "`:composes` edges carry no typed interface." But the
miner *already mines the hole-type* — via `satiety`/`:hungry-for` — it just
isn't wired onto the composition layer. So the first increment is: surface the
already-mined hole-types as the dom/cod on the wiring edges (and as the query
type for projection #5). This proves the unified datatype against real data
(the 4 wiring missions / `M-first-flights`), deterministically, with no new
mining. It is the smallest step that demonstrates "typed holes are already in
the store; make them first-class."

## Acceptance (charter → first increment)

- This charter + `M-typed-holes-lean-manifest.edn` (the concepts to formalise,
  no formalisation yet) + the first-rung spec. **[this commit]**
- First increment: the satiety→`:composes` projection demonstrated on the 4
  wiring missions, surfacing the typed holes onto the wiring (and as L1 query
  types). Verify deterministically (isolation, not corpus dashboards — see the
  determinism car).

## Worked examples (IDENTIFY)

- `M-typed-holes-example-mission-head.md` — **E-mission-head** semi-formalised as
  a BV-typed wiring diagram (phase `seq` × two-readings `copar` × coherence wires
  × typed holes: ghost-line / open-arrow / `:hungry-for` / failed-fill). It has
  no mined `:composes`, so the wiring is constructed from its anatomy
  (`anatomy-of-a-futonic-mission.md`) — which makes it *exercise `par`/`copar`*
  (unreachable on the linear mined skeletons) and shows active-inference =
  hole-filling (prediction error = unfilled typed hole).
- `M-typed-holes-example-first-flights.md` — **M-first-flights** types the *fill
  operation itself*: the "cascade → sorry → wiring" gain-of-function, with all
  three terms on disk (`first-flights-cascade.edn`, the typed-hole arrow, an
  explicit `(typed hole, term, wiring)` triple in `first-flights-wiring.edn`).
  Shows `fill` is two-grained (atomic Poly substitution **+** cascade-driven
  graph-rewrite fold), the fold grain being the cascade→ML leg.

## Lean leg (DarkTower) — status (2026-06-14)

Wave 1 of the Lean formalisation landed in `mathlib4/DarkTower/`, all reviewed
PASS (claude-2, real gate: read + decls verified), **0 sorry**, `lake build
DarkTower` green. **7/10 manifest concepts now formalised** — see
`M-typed-holes-lean-manifest.edn` for the per-concept file+sha.

| file | concepts | sha |
|---|---|---|
| `DarkTower/TypedHole.lean` | typed-hole, hole-type, satiety-grading | `a420bd1` |
| `DarkTower/Fill.lean` | fill, fill-laws (unit+assoc as Equivs) | `b23232d` |
| `DarkTower/Comb.lean` | comb (dependent lens; full category Poly) | `b64d3c1` |
| `DarkTower/Discharge.lean` | discharge-duality (comonad coalgebra) | `75e4725` |
| `DarkTower/ScopeQuery.lean` | scope-as-query (finite unifier; `(Joe knows ?)` by rfl; `queryComb`) | `1f85ad2` |

**8/10 manifest concepts formalised.** Handoffs + audit:
`M-typed-holes-lean-handoffs.md`, `M-typed-holes-mathlib-audit.edn`,
`M-typed-holes-lean-wave2-design.md`.
**Remaining:** `bv-hole-algebra` (T6 — greenfield, no prior formalisation in any
prover; design in `*-wave2-design.md` §T6); `illocutionary-hole` (outside
mathlib — IATC, a labelled-transition layer).

## Relations

- **BV-combs excursion** (futon6 `9cd66b5`, `holes/bv-comb-typing.edn`) — the
  comb = hyperedge-with-holes finding + gap-list this mission generalises.
- **`M-typed-bells`** (futon3c, COMPLETE, `b4ed15f`) — projection #6; the IATC
  illocution model (Corneli et al. 2017) is the coordination-channel instance.
- **`E-substrate-2-sorry-typing`** — projection #2 (sorries as typed holes).
- **futon6 `dp_paper_view` grounding** — projection #3 (C-SYM-GROUND as a hole).
- **`E-the-dark-tower-2`** (futon5a) — the Poly / differentiable-substrate theory
  leg; M-typed-holes is its concrete data-bearing surface.
- **iiching pattern language** (futon5 `tools/iiching`, `resources/iiching-ct`) —
  the eventual home for the Lean manifest entries as lifted CT patterns.
- **XTDB hypergraph store / Arxana** — projection #5 (query = scope = hole).
