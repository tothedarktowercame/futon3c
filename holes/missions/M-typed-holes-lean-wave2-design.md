# M-typed-holes — Lean wave 2 design note (T5 ScopeQuery, T6 BV)

Status: complete

*The two greenfield manifest concepts. The mathlib audit found NO prior
formalisation for either (scope-as-query: Yoneda substrate only; bv: source
literature only, no prover anywhere). They need a design pass before dispatch —
this is it. Each section is scoped to a dispatchable DarkTower handoff. House
rules as in `M-typed-holes-lean-handoffs.md` (import Mathlib; structure + id/comp
where it fits; literature-grounded; lake build clean; minimise sorry).*

## T5 · DarkTower/ScopeQuery.lean — scope-as-query (difficulty L)

**The object.** A query is a partial hyperedge; answering it = filling its holes
from a store. Build it as a small, computable model and connect it to `Comb`
(a query IS a comb whose holes are the variable ends) and `TypedHole`.

**Design (minimal, computable):**
```
-- a signature: kinds and roles are just index types the caller supplies
structure Sig where
  Kind : Type
  Role : Type
-- a ground hyperedge over a node type N
structure HEdge (S : Sig) (N : Type) where
  kind : S.Kind
  ends : List (S.Role × N)
-- a query = a hyperedge whose ends may be variables (holes) or bound nodes
abbrev QEnd (S) (N) (V) := S.Role × (V ⊕ N)
structure Query (S : Sig) (N V : Type) where
  kind : S.Kind
  ends : List (QEnd S N V)
-- a store is a finite collection of ground hyperedges
abbrev Store (S) (N) := List (HEdge S N)
-- answering = unify the pattern against each store edge -> variable bindings
def answers [DecidableEq S.Kind] [DecidableEq S.Role] [DecidableEq N]
    (q : Query S N V) (db : Store S N) : List (V → Option N) := …
```
- `answers` is the **fill** of the query's holes (cross-ref `Fill`/`Comb`): a
  match binds each `V` hole to a store node; non-matching edges drop.
- **Connect** : a `Query` with all-bound ends is a `HEdge`; a `Query` with `k`
  variable ends is a `k`-hole comb over the store. State that "answering = fill"
  as a lemma relating `answers` to `Comb`/`Fill` (prove what's clean).
- **Scope/defer:** keep it first-order (no nested queries / recursion) and
  finite (List store, decidable eq). The presheaf/Yoneda framing
  (`CategoryTheory.Functor.RepresentableBy`, `Yoneda.lean`) is the *semantics* —
  cite it in the docstring; do NOT build the presheaf instance this pass.
- **Literature:** Spivak Poly (queries as Poly maps into the store); nLab
  "representable presheaf"; mathlib `Yoneda.lean`. (This is the Arxana/XTDB
  `(Joe knows ?)` generalisation — role-keyed N-ary patterns.)
- **Acceptance:** `Sig`/`HEdge`/`Query`/`Store`/`answers` compile; ≥1 worked
  example (a 2-edge store, a 1-variable query, the expected binding by `rfl`/
  `decide`); the answers↔fill lemma stated; ≤2 sorries (TODO'd).

## T6 · DarkTower/BV.lean — bv-hole-algebra (difficulty L, greenfield)

**The object.** BV (Guglielmi's system, the calculus of structures): three
connectives over atoms — `seq` ⟨S;T⟩ (non-commutative), `copar` (S,T) and `par`
[S,T] (both commutative), with a self-dual unit — plus the **medial** rule, the
deep-inference move that interleaves seq with par/copar. Over typed holes, this
is the *algebra of hole-patterns* (seq = sequential fill / path; copar = conjoin
holes; par = alternatives).

**Design (syntax + congruence + medial; defer proof theory):**
```
inductive BV (A : Type) where        -- structures over atoms A
  | unit               : BV A         -- the self-dual unit ∘
  | atom   : A        → BV A
  | seq    : BV A → BV A → BV A       -- ⟨S ; T⟩  non-commutative
  | copar  : BV A → BV A → BV A       -- (S , T)  commutative
  | par    : BV A → BV A → BV A       -- [S , T]  commutative
-- structural congruence: assoc of all three; comm of copar/par; unit laws
inductive Cong {A} : BV A → BV A → Prop where … (reflexive/symm/trans/cong)
-- the medial rule (the heart): [ (S,U) ; (T,V) ]  ⟶  ( [S;T] , [U;V] )  (schematic)
inductive Step {A} : BV A → BV A → Prop where
  | medial … | switch … | cong (h : Cong a b) : Step a b
```
- **Minimal target:** the `BV` inductive + `Cong` (as a `Prop` relation, not a
  quotient — simpler) + the `Step` relation carrying **medial** and **switch**.
  Prove a couple of sanity lemmas (e.g. `seq` associativity is in `Cong`; a small
  medial instance). This alone is a *novel* formalisation — none exists.
- **Defer (state as TODO):** cut-elimination / decidability of derivability /
  the full splitting theorem. Do NOT attempt; note them.
- **Connect:** map BV connectives onto hole-patterns over `Comb`/`Fill` in the
  docstring (seq=sequential fill, copar=conjoin, par=alternative) — the bridge
  to the rest of DarkTower; a formal functor is future work.
- **Literature:** Guglielmi, *A System of Interaction and Structure*
  (arXiv:cs/9910023) — the definitional source for BV + medial; Guglielmi
  *Deep Inference* overview; the categorical model (BV-categories) for the
  semantics. Flag: highest-uncertainty task; greenfield.
- **Acceptance:** `BV` + `Cong` + `Step` (with medial + switch) compile; ≥2
  sanity lemmas proved; cut-elim explicitly deferred with TODO; ≤3 sorries.

## Dispatch note
Both are L and independent (distinct files). T5 is the higher-value/lower-risk
of the two (it's the query layer Joe wants, and it composes with the landed
Comb/Fill). T6 is the riskier, more novel one — dispatch alone, expect iteration,
and treat a *compiling syntax + congruence + medial* as success even if the proof
theory stays TODO. Recommend dispatching T5 first; T6 when an agent is free for a
longer, more exploratory run.
