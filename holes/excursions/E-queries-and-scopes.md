# E-queries-and-scopes — the relationship between queries and scopes

**Excursion (synthesis). Spun 2026-06-15 from a Joe↔claude-3 thread picking up
M-typed-holes projection #5.** This document takes the **relationship itself** as
its subject — not a worked example (that is
`M-typed-holes-example-scope-query.md`) and not an outward application (that is
`E-mo-query-phylogeny.md`), but the duality those two presuppose and never state
in one place.

## HEAD (Joe, 2026-06-15, verbatim sense)

We keep saying "queries-as-scopes" as if it were one word. Dive into the
relationship: in what sense *is* a query a scope, where do the two come apart,
and what is the precise thing each one is?

## The thing that forced the excursion: "scope" is two words

The stack uses **scope** in two senses that the slogan "queries-as-scopes"
silently fuses:

- **Scope-as-environment** (`M-mission-scopes-into-substrate-2.md`): a
  *scope-tree-of-centres* — a region that **binds names**. Mission phases,
  sub-scopes, the bound concepts inside them. This is a **context**: the set of
  things in view, available to be referred to. It is the **store** side.
- **Scope-as-partial-hyperedge** (`mathlib4/DarkTower/ScopeQuery.lean`, titled
  literally *"Scope as query"*): a role-keyed hyperedge **some of whose ends are
  typed holes**. `QEnd S N V := S.Role × (V ⊕ N)` — each end is either a bound
  node (`Sum.inr n`) or a variable hole (`Sum.inl v`). This is a **probe**: a
  slot-structure waiting to be matched. It is the **query** side.

These are not the same object, and conflating them is what made "queries-as-
scopes" feel slippery. The relationship between queries and scopes is the
relationship **between these two senses**:

> A **query** is a scope-as-partial-hyperedge (sense 2) **resolved against** a
> scope-as-environment (sense 1). Answering binds the holes of the probe from
> the names in the context.

The reason the slogan still earns its keep: **both senses are the same
datatype** — a (typed-hole) hyperedge. Sense-1 is a *store of (mostly ground)*
hyperedges; sense-2 is *one partial* hyperedge. The query layer is the one place
where the two meet, and the meeting is `M-typed-holes`' `fill`.

## The crisp statement (object vs. probe)

With the two senses separated, the relationship has a clean categorical shape,
and it is **already in the Lean**, not aspirational:

- A scope (sense 2) **is an object**: `Query S N V` — a partial hyperedge. Its
  holes are `QueryHole q` (`ScopeQuery.lean:146`), the variable ends; the
  polynomial interface whose directions are exactly those holes is `holePoly q`
  (`:150`).
- A query **is that object used as a morphism** — a *probe into the store*.
  `queryComb q : Comb Fill.I (holePoly q)` (`:158`) makes this literal: a query
  is a **comb (a Poly morphism / lens)** whose target directions are precisely
  the variable ends. The docstring's grounding is Yoneda/representable-presheaf
  (Spivak–Niu *Poly*, arXiv:2312.00990, `:12–22`): *"a partial hyperedge is
  represented by its probes/maps into the store."*

So the relationship in one line:

> **A scope is the object; a query is the scope read as a representable probe.**
> Same data, two readings — exactly the `M-typed-holes` move (one datatype, the
> verb is which way you point it). A scope *sits*; a query *asks*. The "?" end is
> the only syntactic difference, and it is a hole either way — unfilled-because-
> waiting (scope) vs. unfilled-because-asking (query).

## Answering = filling = the counit

The third leg, and the one that ties projection #5 back into the unified `fill`:

- `answers q db` (`:127`) = `db.filterMap (answerEdge q)` — the successful finite
  unifications of the probe against the store.
- `fills q db` (`:163`) is an **alias**, and `answers_eq_fills` (`:168`) proves
  `answers = fills` **by `rfl`**. This is not a metaphor bolted on afterward: in
  the model, *answering a query* and *filling a scope's holes* are
  definitionally one operation.
- In `M-typed-holes`' vocabulary this is the **counit / discharge** — the fill
  consumes the hole. `query→answer` sits in the charter's projection table
  (`M-typed-holes.md:101`) right beside `sorry→proof` and `ungrounded→binder` as
  *the same counit*. This excursion is where that table entry is cashed out.

The boundary case sharpens it: a scope with **no** holes is not a query at all —
`toHEdge? q` (`:140`, via `boundEnds?` `:132`) succeeds exactly when every end is
bound, returning a ground `HEdge`. **No holes ⇒ a fact, not a question.** So
"query-ness" is precisely "has ≥1 open direction," and `QueryHole` is the witness
set. A scope becomes a query the instant it carries an unfilled typed hole and is
pointed at a store.

## Dogfood status (what is real today)

Not hypothetical. `scripts/scope_query_dogfood.py` runs the `answers` semantics
as a runtime over the real mined CT for arXiv 0809.2517 (16 hyperedges,
`futon6/.../0809.2517-proof-goal-decomposition.edn`). Spec (Lean `answers`) and
runtime **agree on real data** — e.g. `Q1 "which concept grounds A?" →
azumaya-algebra`, the *same* answer `PaperExample`'s `Discharge`/`ScopeQuery`
produces in Lean. One faithful extension on the runtime side: a role hole is
**set-valued** (`Q2` returns all three subgoals) — the Lean `answers` is the
per-edge positional core; the runtime adds set-valued role lookup on top
(`M-typed-holes-example-scope-query.md:54`).

Arxana's `(? knows ?)` is the **fully-open scope** — the maximally general query
(`Q4`), every end a hole — and the binary-triple case of the role-keyed N-ary
edge. So Arxana's query model is a projection of this one, not a separate thing.

## Why this matters (IF / HOWEVER / THEN / BECAUSE)

**IF** the stack treats "scope" as one word, **HOWEVER** it names both a binding
*environment* (mission scope-trees, substrate-2) and a *partial hyperedge* (the
ScopeQuery probe), **THEN** state the relationship explicitly — a query is the
partial-hyperedge scope resolved against the environment scope, both being the
one typed-hole datatype, with answering = filling = counit — **BECAUSE**
otherwise the two stores (`substrate-2` = the self-model; `substrate-2a` = mined
math.ct) look like they need two query layers when they need one, and the
"theorem = scope/query, proof = witnessed fill in ArSE" arc loses its footing.

## What this opens (next rungs, not done here)

1. **The store is also a scope.** This excursion separated environment-scope from
   probe-scope, but `Store` in the Lean is just `List (HEdge)` — a flat bag, not a
   tree-of-centres. The `M-mission-scopes-into-substrate-2` scope-tree (nesting,
   bound-name regions) is **richer** than `Store`. Open question: does answering
   need the tree (lexical-scope shadowing, name resolution up the centres), or is
   the flat store enough for the corpus case? Likely flat for `substrate-2a`,
   tree for `substrate-2` (missions nest; papers mostly don't).
2. **Variable scope ≠ name scope.** The `V` (query variables) in `ScopeQuery` are
   the *holes*; the `N` (nodes) are the *names in the environment*. The unifier's
   `Binding.insert` (`:76`) enforces that repeated `V` agree — that is **variable
   scope** (one `?c` means one thing across the edge). Worth a line in the
   datatype: the two "scopes" are V-scope (the query's own holes) and N-scope (the
   store's bound names); a fill is a map `V → N` honouring both.
3. **queryComb on real data.** `queryComb` is defined but the dogfood answers via
   `answers`, not by composing the comb. Build the bridge: show the dogfood's
   fills factor through `queryComb` (the Poly-morphism reading), so the "query =
   probe" claim is executable, not just typed. This is the natural `substrate-2a`
   rung once mining wraps.

## Relations

- **`M-typed-holes.md`** — projection #5 (queries/scopes); this excursion cashes
  out that table row.
- **`M-typed-holes-example-scope-query.md`** — the worked example / dogfood this
  generalises.
- **`E-mo-query-phylogeny.md`** — the outward application (MO `scopes.json` shares
  the `hx/` schema, so this same machinery answers MO questions); the benchmark
  rung.
- **`M-mission-scopes-into-substrate-2.md`** — the source of the *environment*
  sense of "scope" (the scope-tree-of-centres); rung #1 above is its hook.
- **`mathlib4/DarkTower/ScopeQuery.lean`** — the spec (`Query`, `answers`,
  `queryComb`, `answers_eq_fills`, `toHEdge?`).
- **`E-the-dark-tower-2`** (futon5a) — the Poly substrate the object/probe
  reading lives in.
