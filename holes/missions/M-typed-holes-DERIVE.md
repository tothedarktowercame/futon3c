# M-typed-holes — DERIVE (2026-06-14)

Status: complete

*Phase 3 per `futon4/holes/mission-lifecycle.md`: design the solution. MAP's
handoff: the datatype exists (Lean DarkTower, 9/10) — the design problem is the
**single runtime `fill` operator** the six projections share, the
**ArSE↔ScopeQuery↔store proving loop**, and the **`illocutionary-hole` wiring**.
Exit target: implementable from this section alone.*

## Entity types

| entity | identity | source |
|---|---|---|
| **TypedHole** | a position id + its typed directions (`hx/` node) | mined (`satiety`), authored (sorry/ghost), or derived |
| **HoleType** | what a hole accepts (`:hungry-for X`; a direction-type) | mined (satiety) / declared |
| **Filler** | a candidate value typed by a HoleType (concept, proof, store-node, pattern, phase, reply) | per projection |
| **Fill** | the act/edge `(hole, filler, witness) → discharged` | the one universal op |
| **Store** | a set of `hx/` hyperedges queried by scopes | substrate-2 (stack), substrate-2a (math.ct), MO, golden-graphs |
| **Scope** | a partial hyperedge (hole-bearing pattern) = a query | authored / reshaped from a question |
| **ArSE thread** | a `type=query` bell + its `type=answer --ref` reply | the coordination record of a fill |

## Relation types

- `:has-hole` (node → TypedHole) — binary.
- `:hungry-for` (TypedHole → HoleType) — binary; the satiety grade.
- **`:fills` (Filler → TypedHole, witness)** — the *shared* edge; n-ary (filler,
  hole, witness/justification). Every projection's discharge is an instance.
- `:answers` (Scope × Store → [binding]) — query-answer; ≡ the fills of the
  scope's holes from the store (the `ScopeQuery` bridge).
- `:discharges` (Fill → polarity) — the discharge-duality reading
  (`sorry↔proof` / `query↔answer` / `ungrounded↔binder`).

## Invariant rules (checkable)

- **I1 — well-typed fill:** a `:fills (f → h)` holds only if `type(f)` matches
  `h`'s `:hungry-for`. (The precision gate.)
- **I2 — total-or-honest:** every hole is `:full` (filled) or recorded open
  (`:hungry-for …` / `irreducible-debt`). No silent drop.
- **I3 — one operator, N views:** there is ONE runtime `fill`; each projection is
  a *view/adapter* into it, never a reimplementation.
- **I4 — answer = fill:** `answers(scope, store)` ≡ the fills of `scope`'s holes
  from `store` (the Lean `ScopeQuery` lemma, runtime-side).
- **I5 — proof = witnessed fill:** a production answer to a query-as-scope emits
  an ArSE typed-bell pair (`query` → `answer --ref`); the record IS the witness.
- **I6 — schema uniformity:** every Store exposes the `hx/` hyperedge schema, so
  one query layer serves all stores (no per-store translation).

## Data flow

```
mining ──emits typed holes (satiety)──▶ Store (hx/: substrate-2 / -2a / MO)
                                            │
   Scope (partial hyperedge, a query) ─────▶ answers(scope, store)   [I4 = fill]
                                            │
                                   well-typed fillers  [I1]
                                            │
                       ArSE: query-bell ──▶ answer-bell --ref  [I5 = the witness]
                                            │
                                   discharged hole (satiety :full)  [I2]
```
The six projections are the same path, differently sourced: cascade-feed
(mining) · discharge (proof) · ground (symbol) · compose (comb) · answer (query)
· reply (bell) — all route through the single `:fills` op [I3].

## Design decisions (IF / HOWEVER / THEN / BECAUSE)

- **D1 — one `fill`, not six.** IF each projection keeps its own fill, HOWEVER
  all are `(hole, filler)→discharge` over `hx/`, THEN define ONE runtime
  `fill(hole, filler, witness)` with per-projection adapters, BECAUSE the
  unification's value *is* the shared operator (MAP missing #1); six fills = six
  bug surfaces and no shared query layer.
- **D2 — store-agnostic query layer.** IF we special-case substrate-2/-2a/MO,
  HOWEVER all are `hx/` (MAP surprise #1), THEN parameterise the query engine by
  store (store is an argument), BECAUSE per-store engines duplicate one engine.
- **D3 — typed fills only (precision gate).** IF any filler may fill any hole,
  HOWEVER that reintroduces false-grounding, THEN enforce I1 (filler-type =
  hole-type), BECAUSE precision-over-recall is the campaign's settled discipline
  (appositive/def-eq gates; claude-1's rulings).
- **D4 — proof = *witnessed* fill, not definitional.** IF `answers ≡ fills` only
  by definition (the Lean alias), HOWEVER that hides whether the runtime really
  composes, THEN keep the definitional identity as the spec but require the ArSE
  record per production answer (I5), BECAUSE a proof must be witnessed, not
  asserted.
- **D5 — `illocutionary-hole` = ArSE wiring, not new formalism.** IF we formalise
  it abstractly in Lean, HOWEVER ArSE + typed bells already implement
  query/answer at runtime and M-typed-bells is COMPLETE, THEN realise it by
  wiring the typed-bell pair onto `(hole, fill)` (query-bell = open hole,
  `answer --ref` = fill), BECAUSE the 10th concept activates by USE (the proving
  arc), not by abstract definition.
- **D6 — design now, build gated.** IF we build the substrate-2a proving loop
  now, HOWEVER the mining is gated + heavy, THEN DERIVE the design now and stage
  the build behind the mining wrap + claude-1 coordination, BECAUSE design is
  CPU-light and unblocks fast execution (MAP gating finding).

## View / UI

- **ArSE surface** (existing): `type=query`/`type=answer --ref` bells = the
  fill loop's user-facing interface; the bell router threads them.
- **scope-query CLI** (`scripts/scope_query_dogfood.py`, to generalise): pose a
  scope, see the fills.
- **paper-anatomy overlay** (existing): holes render as ungrounded/hungry marks
  (gray) vs filled (green) — the (hole, fill) state, already visible.

## Wiring diagram (sketch — for a futon5 AIF+ pass later)

Ports: `Store` (in: hx/ edges; out: bindings) · `fill` (in: hole+filler; out:
discharged + ArSE record) · `ArSE` (in: query; out: answer/ref). Timescales:
mining = glacial; scope-answer = fast; ArSE exchange = social. Exogeneity: the
external corpus (substrate-2a / MO) is exogenous; substrate-2 (the stack) is
endogenous (the self-model). Closure: a fill closes a hole; the ArSE record
closes the Q&A. A full futon5 diagram is deferred (optional per lifecycle).

## Fidelity contract (extension mission — light GF)

Preserve / adapt / drop over the six existing projections:
- **Preserve:** each projection's current behavior + precision gates (grounding
  binders, typed-bell illocutions, the mined satiety).
- **Adapt:** route each projection's discharge through the single `:fills` op
  (D1) — same behavior, shared implementation.
- **Drop:** nothing. Tripwire: the gate papers' grounded% and the typed-bell
  invariants must not regress when a projection is re-routed through `fill`.

## Deferred (Joe, 2026-06-14) — HEAD + AIF workup, then maybe re-derive

This mission was started **without a HEAD and without the AIF workup** (sigil /
vitals / failure-conditions — the E-mission-head "organism reading"). Post-DERIVE
plan: **(1)** add a HEAD; **(2)** do the AIF workup; **(3)** optionally
**re-derive** to see how a HEAD-seeded, AIF-grounded start changes the structure
— a controlled experiment in whether the front matter reshapes the derivation.
Recorded now so it isn't lost; not done in this pass.

## Exit

Entity/relation types, six checkable invariants (I1–I6), the data-flow, six
justified decisions (D1–D6), views, and the (light) fidelity contract are set.
Someone could implement the shared `fill` + the ArSE↔ScopeQuery↔store loop from
this. **What DERIVE hands to ARGUE:** justify *why* the single-`fill`/witnessed-
answer design is right (not just workable) — pattern cross-ref + the plain-language
argument — and confront the alternative (keep six fills) head-on.

*Cross-refs:* `M-typed-holes.md`, `M-typed-holes-MAP.md`,
`E-typed-holes-example-self.md` (this fills its `:DERIVE` ghost),
`mathlib4/DarkTower/` (the formalised datatype the runtime mirrors),
`README-arse.md` / `M-typed-bells.md` (the ArSE runtime), `futon4/holes/mission-lifecycle.md`.
