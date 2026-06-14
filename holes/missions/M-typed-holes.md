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

## HEAD — AIF workup (carried back 2026-06-14; "best of class")

*This mission was developed the **non-AIF "as planned" route**: it had the prose
HEAD above but skipped the AIF lifeform workup (recast-as-design-pattern, sigil,
birth vitals, failure conditions). That route is a **shortcut / graceful
degradation** of the full HEAD→AIF path — fine when feasibility is high, at the
cost of leaving the **adoption gate unstated**. The counterfactual
`../excursions/E-typed-holes-aif-alternate.md` ran the AIF path; both routes
reached the **same design**, but the AIF route uniquely produced the failure
conditions + death clause below. Carrying them back makes this the best-of-class
version; per that excursion, DERIVE is **not** re-run (both routes agree).*

**HEAD as a design pattern (AIF terminal vocabulary — recast for the sigil):**

**IF:** the stack keeps building a slot waiting to be filled six times — a query
waiting for an answer, a sorry waiting for a proof, an ungrounded symbol waiting
for a binder, a comb hole, a hungry mined node, a type-query bell.

**HOWEVER:** they are built separately; the shared datatype (the typed hole) and
the shared operation (fill) are invisible; there is no shared query layer; and
ArSE, the witness store, sits starved.

**THEN:** name the one datatype — a typed hole, a Poly position with typed
directions — and the one operation, fill, with the six surfaces as projections,
and record every fill in ArSE so it is witnessed.

**BECAUSE:** one operator is deterministic and testable where six are racy and
un-auditable; asking a question and proving a theorem become the same act; and a
self-representing stack needs its central datatype to be a single thing.

## HEAD — sigil, birth vitals & failure conditions

**Sigil (computed 2026-06-14 — `futon5/scripts/head_exotype_probe.py` via `.venv`,
`--emit-health` → `M-typed-holes.health.json`):**
- whole-HEAD exotype **`00011000`** (bit-confidence 0.33), nearest anchor
  **`iching/hexagram-45-cui`** — 萃 *Cui*, "Gathering Together / Massing" — an
  **exact bit-match** (cos 0.321). The HEAD lands on the hexagram for *gathering
  scattered things into one*, which is exactly what the mission does.
- xenotype-32 (per-clause): `00101000·01001000·00001001·00001000`, mean-conf 0.40,
  xenotype-completeness 0.89. The per-clause CT anchors are apt:
  **IF → 需 Xu "Waiting"** (cos 0.415, strongest — the clause is literally about a
  slot *waiting* to be filled); **HOWEVER → 比 Bi "Holding Together"**; **THEN → 解
  Jie "Deliverance"**; **BECAUSE → 屯 Zhun "Difficulty at the Beginning"**.
- health: *"alive with moderate signal"* — consistent with the born-late-and-healthy
  vitals below. (This is the sigil's CT-domain hook: the HEAD's exotype is an
  i-ching/iiching pattern address, joinable to the iiching CT pattern lexicon.)

**Birth vitals:** born *late and healthy* — the datatype is already 9/10
formalised in DarkTower, so the risk is **adoption, not feasibility**.

**Failure conditions (the adoption gate — the AIF route's unique contribution):**
1. **dead if** no projection actually routes through the single `fill` (it stays
   six implementations — the unification is decorative);
2. **dead if** ArSE stays starved — fills are not witnessed, so "proof = witnessed
   fill" never lands;
3. **dead if** the query layer never runs over a real store (substrate-2a / MO).

**Death clause:** declare M-typed-holes dead, publicly, if by an agreed deadline
none of the six projections is re-routed through the shared `fill` **and** no fill
is witnessed in ArSE.

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
- `M-typed-holes-example-scope-query.md` — **queries-as-scopes, dogfooded** on
  real mined CT (`scripts/scope_query_dogfood.py` over the 0809.2517 golden
  graph): a query is a partial hyperedge, answering = filling its holes from the
  store; the Lean `ScopeQuery.answers` spec and this runtime agree on real data
  ("which concept grounds A?" → `azumaya-algebra`). The third example genre —
  mission, paper, **and query** handled by one machinery. First rung of the
  prove-theorems-by-query arc (`substrate-2` = our stack; `substrate-2a` = mined
  math.ct; theorem = a scope/hole, proof = a witnessed fill via ArSE).
- `../excursions/E-typed-holes-example-self.md` — **the reflexive/live one**:
  M-typed-holes typed as a typed-hole object *itself*, mid-flight — IDENTIFY/MAP
  sated, DERIVE…DOCUMENT as ghost `TypedHole`s. The only case with open holes
  watched live (writing the next phase = filling a hole); a *paired excursion* so
  the recursion bottoms out. The framework typing its own defining mission.
- `../excursions/E-typed-holes-aif-alternate.md` — **the counterfactual**:
  M-typed-holes down the HEAD→AIF lifeform path (sigil/vitals/death-clause +
  ARGUE-as-aif⁺-battle), compared to the "as planned" route. Finding: both routes
  reach the *same* design (inevitability backed twice), but the AIF route uniquely
  yields **failure conditions + a death clause** — the adoption gate to carry back
  into the charter (Joe's deferred HEAD+AIF plan).

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
| `DarkTower/BV.lean` | bv-hole-algebra (BV syntax + congruence + medial/switch; greenfield) | `d1e0888` |

**9/10 manifest concepts formalised** — `lake build DarkTower` green, 0 sorry
across all. Handoffs + audit: `M-typed-holes-lean-handoffs.md`,
`M-typed-holes-mathlib-audit.edn`, `M-typed-holes-lean-wave2-design.md`.
**Remaining:** `illocutionary-hole` only (outside mathlib — IATC speech-acts, a
labelled-transition layer tying typed-bells to the typed-hole datatype).
**Caveat on BV:** syntax + structural congruence are faithful (seq non-comm); the
`medial`/`switch` rules are schematic — exact Guglielmi-BV fidelity + cut-elim
deferred (a future proof-theory pass).

**By-example demonstration** (`DarkTower/Examples.lean`, `36b44b7`, reviewed PASS,
0 sorry): the SAME vocabulary types **both reasoning domains** Joe named —
`MissionExample` (E-mission-head: lifecycle `BV.seq` chain + scope∥organism
`BV.copar` + ghost `TypedHole`) and `PaperExample` (0809.2517: ungrounded `H` as
a `TypedHole`, grounding as `Discharge`/`Fill`, "which concept grounds H?" as a
`ScopeQuery` answering `HopfAlgebra` by rfl). `(typed-hole, fill)` is
domain-agnostic — missions and papers are two instantiations, not two theories.
This is the "futon6 first rung **by example**, not a rollout."

**Three exemplars now in Lean** (all reviewed PASS, 0 sorry):
- `Examples.lean` `MissionExample` (E-mission-head — mission domain) — `36b44b7`
- `Examples.lean` `PaperExample` (0809.2517 grounding — paper domain) — `36b44b7`
- `FirstFlightsExample.lean` (M-first-flights — the cascade→sorry→wiring **fold**
  grain: hungry want-port → folded; cascade-select = `ScopeQuery.answers`;
  `:jointly-with` = `BV.copar`) — `58fb0b7`

Future tightenings: `PaperExample` store is single-node-typed (use
`Sum Symbol Concept`); add a `queryComb`-on-exemplar line.

## MAP (2026-06-14) — see `M-typed-holes-MAP.md`

Phase 2 per `futon4/holes/mission-lifecycle.md` (survey, not design). Headline of
the ready-vs-missing map: **ready** — the typed-hole datatype (Lean DarkTower,
9/10), the uniform `hx/` scope schema across stores, the mined hole-types
(`satiety`), the structural scope-query runtime, ArSE/typed-bells, and the mined
CT + MO stores. **Missing** — a *single runtime `fill` operator* shared across the
six projections (today there are six separate fills), the ArSE↔ScopeQuery↔store
**proving loop**, the `illocutionary-hole` wiring, the live satiety→`:composes`
projection, and substrate-2a import (gated). **What MAP hands to DERIVE:** the
design problem is the **shared `fill` operator + the proving loop**, not the
datatype (which exists). Full inventory, survey Q&A (Q1–Q6), and surprises in
`M-typed-holes-MAP.md`.

## DERIVE (2026-06-14) — see `M-typed-holes-DERIVE.md`

Phase 3. Designs the MAP-identified problem: a **single runtime `fill(hole,
filler, witness)`** the six projections share (D1), a **store-agnostic query
layer** over the uniform `hx/` schema (D2), **typed fills only** (D3, the
precision gate), **proof = witnessed fill** via an ArSE record (D4/I5),
`illocutionary-hole` realised as **ArSE wiring** not new formalism (D5), and the
proving loop **designed now, built gated** behind the mining wrap (D6). Six
checkable invariants (I1–I6), entity/relation types, data-flow, and a light
fidelity contract (route the six projections through one `fill`; drop nothing).
**Deferred (Joe):** this mission has no HEAD and no AIF workup — post-DERIVE, add
both and optionally re-derive to see how the front matter reshapes the structure.

## ARGUE (2026-06-14) — see `M-typed-holes-ARGUE.md`

Phase 4 ("as planned"; an AIF-driven re-run is queued for comparison). Three
applied patterns — `single-authority-registration` (one `fill`, enforced
pre-hoc not post-hoc → D1/D3), `construct-an-explicit-witness` (proof = built
witness → D4/I5), `exotype-determines-behaviour` (the hole's type determines
behaviour) — + 2 catch-up PSRs. Coherence: DERIVE is the *runtime shadow* of the
already-formalised Lean datatype (every decision has a proved correlate).
Trade-offs accepted (recall for precision; per-store opt for schema uniformity;
abstract-illocution for ArSE-by-use; immediacy for gated-build). The alternative
(keep six fills) is confronted: post-hoc reconciliation is racy/untestable, so
unifying makes the disunity impossible by construction — the design reads
*inevitable*. Elevator pitch: *the system built "a slot waiting to be filled" six
times; build it once, and asking a question = proving a theorem = filling the
slot, witnessed.*

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
