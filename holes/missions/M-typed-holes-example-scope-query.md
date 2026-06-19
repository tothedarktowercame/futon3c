# M-typed-holes — worked example (IDENTIFY): queries-as-scopes, dogfooded

*Third example genre (Joe, 2026-06-14): after a **mission** (E-mission-head) and
a **paper** (PaperExample / M-first-flights), a live **query-as-scope** answered
against real mined CT — so the IDENTIFY shows missions, papers, AND
queries-as-scopes handled by the **same** machinery. Micro-dogfood, no waiting on
the full substrate-2a mining: it runs over an existing local golden graph.*

## The dogfood

`scripts/scope_query_dogfood.py` runs the `ScopeQuery` semantics — the Lean spec
in `mathlib4/DarkTower/ScopeQuery.lean` — as a *runtime* over the structure-mined
CT for arXiv 0809.2517 (`futon6/holes/golden-graphs/0809.2517-proof-goal-decomposition.edn`,
16 hyperedges). A **query is a partial hyperedge** (bound role→node constraints +
role→variable holes); **answering = unifying against the store**, the holes filled
by matching nodes. Output (reproduce: `python3 scripts/scope_query_dogfood.py`):

```
Q1  which concept grounds symbol A?
  scope:  {:kind :bind (subject :A) (concept ?c)}
  answer: ['azumaya-algebra']

Q2  what does the goal decompose into?
  scope:  {:kind :decomposes-into (goal :goal-statement) (subgoal ?s)}
  answer: ['obligation-1', 'obligation-2', 'obligation-3']

Q3  what is the goal's hypothesis?
  scope:  {:kind :goal (hypothesis ?h)}
  answer: ['H-Azumaya-algebra']

Q4  every bind: which subjects bind which concepts?  (Arxana (? r ?) style)
  scope:  {:kind :bind (subject ?x) (concept ?c)}
  answer: ['A', 'azumaya-algebra']
```

## One machinery, three genres

The point is that **nothing here is query-specific** — it is the typed-hole
datatype again, a third way:

| genre | the object | the hole | the fill |
|---|---|---|---|
| mission (E-mission-head) | lifecycle hyperedge | ghost phase | write the phase |
| paper (PaperExample) | binder hyperedge | ungrounded `H` | the binder concept |
| **query (this)** | a **partial hyperedge** | the `?`-variable end | the matching store node |

`Q1` is literally the paper grounding question (`which concept grounds A` →
`azumaya-algebra`) asked as a query — the *same answer* the `PaperExample`
`Discharge`/`ScopeQuery` produces in Lean, now run over the real mined graph.
`Q4` is Arxana's `(? knows ?)` generalised to a role-keyed N-ary edge. So the
spec (Lean `ScopeQuery.answers`) and the runtime (this script) agree, on real
data: **answering a query = filling a scope's holes from the store.**

(The runtime generalises the minimal Lean unifier in one way: a role hole is
set-valued — `Q2` returns all three subgoals — which is what real "which X?"
questions want. The Lean `answers` is the per-edge positional core; the runtime
adds the set-valued role lookup on top. Faithful extension, not a divergence.)

## Bridge to the proving arc (substrate-2 / substrate-2a)

This is the first rung of the bigger arc (Joe): **prove theorems by
queries-as-scopes over the mined CT, recorded in ArSE.** The naming that keeps it
clean: **`substrate-2`** = our representation of *the stack itself* (missions,
flights, the self-model); **`substrate-2a`** = the mined *external* corpus
(math.ct — the research-programme domain). Same query layer, two stores.

In that frame a **theorem is a query (a scope/hole)** and a **proof is a
witnessed fill** — answered via **ArSE** (the typed-bell Q&A substrate;
`type=query` opens the hole, `type=answer --ref` fills it — the `illocutionary-hole`
projection, activated by *use*). This dogfood answers from a flat in-memory store;
the full version answers from `substrate-2a` once the laptop structure-mining
wraps (later today) — at which point the *same* `ScopeQuery` scope, the *same*
`(typed-hole, fill)`, runs over the whole corpus through ArSE.

*Cross-refs:* `M-typed-holes.md` (charter), `mathlib4/DarkTower/ScopeQuery.lean`
(the spec), `M-typed-holes-example-mission-head.md`,
`M-typed-holes-example-first-flights.md`, `scripts/scope_query_dogfood.py`,
`README-arse.md` / `M-typed-bells.md` (the ArSE / typed-bell runtime).
