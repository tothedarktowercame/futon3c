# M-typed-holes — worked example (IDENTIFY): M-first-flights, the fill *process*

Status: complete

*Second semi-formalisation (companion to `M-typed-holes-example-mission-head.md`).
Where E-mission-head typed a mission **as** a typed-hole object, M-first-flights
types the **fill operation itself** — the "cascade → sorry → wiring diagram"
gain-of-function — because it ran that pipeline live and left all three terms as
real artifacts. Sources: `futon3c/holes/flights/first-flights-cascade.edn`,
`first-flights-wiring.edn`; `futon6/holes/early-closures.md` §"cascade → sorry →
wiring as a *process*"; the mined `data/mission-triples/M-first-flights.edn`
(23-checkpoint `:composes` chain, typed in the BV-combs excursion `9cd66b5`).*

## 1. Why this example

M-first-flights' subject is the Wright-brothers move — *reshape the data before
the longer flight* — and to do it the mission ran the **cascade → sorry → wiring**
gain-of-function and recorded its own triple. So unlike E-mission-head it gives
the **fill operation as a process**, with every term on disk:

> `first-flights-wiring.edn`, line 1: *"Triple: (typed hole arr-7535a5b6-e59,
> term flight-pretty-print+migration, THIS DIAGRAM)."*

That is `(hole, filler, fill)` written out by the system itself.

## 2. cascade → sorry → wiring, typed as `fill`

`early-closures.md` states the pipeline is a **graph-transformation** ("poor
man's protein folding"): the cascade proposes candidate design-patterns; we
**select the subset** that fits; each selected pattern is a **graph-rewrite rule**
(its `THEN` clause = LHS→RHS) acting on the **topology of the sorry**; applying
the rules **folds** the sorry-graph into the wiring diagram. In typed-hole terms:

| pipeline stage | typed-hole reading | artifact witness |
|---|---|---|
| **sorry** | the **typed hole**: disconnected pieces + an *unfilled target* | the minted arrow `arr-7535a5b6-e59`; wiring `:want-port` `:satiety {:hungry-for :payoff}` |
| **cascade** | the **menu of candidate fillers** (design-patterns), typed by what they differentiate | `first-flights-cascade.edn` — an Alexander semilattice with `:differentiates` / `:jointly-with` edges |
| **select** | the **type-check**: keep patterns whose rewrite-LHS matches the sorry topology | "we do not use all 27 — we select the two whose rules actually wire the topology" |
| **wiring** | the **fill**: a graph-rewrite *fold* of the sorry-graph into a construction | `first-flights-wiring.edn` — boundary ports = the hole's endpoints verbatim; interior nodes each `:witness`'d (filled) or `:hungry-for` (residual hole) |

So **`fill` here is a graph-rewrite fold, not an atomic substitution.** That is the
key contribution: the manifest's `:fill` has **two grains** —
1. the **atomic** Poly substitution (`PFunctor.comp`; the Lean handoff T2), and
2. the **fold** grain — cascade-selected rewrite-rules folding a sorry-topology
   into a construction (this). "Learning is *which patterns fold which
   sorry-topologies into which constructions*" — i.e. grain 2 is exactly the
   cascade → ML leg.

## 3. BV typing (two layers)

**(a) The mined progress wiring** — `M-first-flights.edn`'s `:composes` chain is a
linear `seq` of 23 checkpoints (the lifecycle ran IDENTIFY→…→INSTANTIATE, then
the Phase-A exits and the live write):
`⟨ckpt-00 ; ckpt-01 ; … ; ckpt-22⟩` — and (BV-combs excursion) its boundary-type
proxy is present at only 2/23 nodes, so every comb over it is `:skeleton-only`.
The mining recorded the *checkpoint chain* but not the *folds* that produced each
checkpoint.

**(b) The fold (the real gain-of-function)** — typed from the wiring artifact:

```edn
{:mission "M-first-flights"
 :fill/grain :graph-rewrite-fold
 :sorry        ;; the typed hole = minted arrow: have -> want, want unfilled
 {:hole "arr-7535a5b6-e59"
  :bv/seq [{:port :have :satiety :full
            :form "flight-records-organs-typed-grounds-in-prose"}
           {:port :want :satiety {:hungry-for :payoff}        ;; <- the hole
            :form "flights-as-anatomy-corpus-canonical-organ-order"}]}
 :cascade      ;; menu of candidate fillers (patterns); :jointly-with = copar/par
 {:artifact "first-flights-cascade.edn"
  :select [:scan-coherence/mission-anchored-scan
           :correspondence-coherence/mission-unlocks-eoi]
  :unused-count 25
  :edges #{:differentiates :jointly-with}}      ;; jointly-with EXERCISES par/copar
 :wiring       ;; the fold: ports (hole endpoints) + interior heart, each node typed
 {:artifact "first-flights-wiring.edn"
  :heart :derive-argue-verify                    ;; = field-read / act / measure
  :nodes-filled  [:schema-v04 :logic-model :substrate-roundtrip]   ;; :witness'd
  :nodes-hungry  [:want-port :pretty-print]}}    ;; residual typed holes
```

Two BV notes: the cascade's **`:jointly-with`** edge is a genuine `par`/`copar`
(n-ary parallel) — *hand-authored here, but the schema says v1 never emits it* —
so M-first-flights, like E-mission-head, **exercises the connectives the mined
`:composes` can't** (excursion gap #3), this time from the cascade side. And the
wiring's interior is the **`derive ; argue ; verify` heart**, which the artifact
notes equals **`field-read ; act ; measure`** — the active-inference loop. So the
fill-fold *is* an AIF cycle: the sorry is prediction error, the fold is action,
the witness is measurement.

## 4. What this contributes to M-typed-holes (IDENTIFY)

1. **`fill` is two-grained.** atomic Poly substitution (T2) *and* cascade-driven
   graph-rewrite fold (here). The manifest entry `:fill` must cover both; the
   `:fill-laws` should hold for the atomic grain and *bound* the fold grain.
2. **The triple is the datatype, written by the system.** `(typed hole, term,
   wiring)` on disk is `(hole, filler, fill)` — direct evidence the
   M-typed-holes object is already what the stack produces.
3. **cascade = ML leg.** "which patterns fold which sorry-topologies into which
   constructions" is the learnable map; this is where Joe's "once we have the
   cascade, that part tries to be ML-based" attaches to the typed-hole datatype.
4. **par/copar again unreachable in the mined wiring, present in the cascade.**
   Reinforces the first-rung priority: surface the holes (`satiety`/`:jointly-with`)
   that the artifacts carry onto the `:composes` layer.

## In Lean (DarkTower)

The landed DarkTower types instantiate this exemplar's two `fill` grains:

| this doc (cascade→sorry→wiring) | DarkTower Lean |
|---|---|
| **sorry** = unfilled want-port (typed hole) | `TypedHole` with hungry `satiety` (`TypedHole.lean`) |
| **cascade** = candidate-pattern menu + select | `ScopeQuery` store + `answers` (the select = the matching answer) (`ScopeQuery.lean`) |
| **wiring/fold** = graph-rewrite filling the hole | `Fill`/`Discharge` (the fold grain) |
| `:jointly-with` (n-ary parallel) | `BV.copar` (the connective the mined linear `:composes` can't emit — gap #3) |
| the mined `:composes` chain | `BV.seq` of checkpoint atoms |

The **two grains of `fill`** (§ above) are now concrete: the *atomic* grain is
`DarkTower/Fill.lean` (`PFunctor.comp` + the unit/assoc laws); the *fold* grain
(cascade-selected graph-rewrite) is `DarkTower/FirstFlightsExample.lean`
(`58fb0b7` + fold `6d47cd0d`, reviewed PASS, 0 sorry) — the worked instantiation
of this exemplar: the hungry want-port `TypedHole` folds to not-hungry;
cascade-select = `ScopeQuery.answers` (by rfl); `:jointly-with` = `BV.copar`.
The fold is now **one applied operation** `fold q db h` (not two snapshots),
*conditioned on the cascade*: `fold_selected : fold selectedQuery cascadeStore
wantPortHole = foldedPort`, and the negative witness `fold_empty_store_leaves_hungry`
(empty cascade ⇒ hole stays hungry — the rewrite is not unconditional). So
"answering = filling" and "folding a sorry-topology into a wiring" are the same
`fill`, two grains, both in Lean. (Residual idealization: `fold` conditions on
selection-existence, not yet on which pattern — a future faithful step.)

*Cross-refs:* `M-typed-holes.md`, `M-typed-holes-example-mission-head.md`,
`M-typed-holes-lean-manifest.edn` (`:fill`, `:scope-as-query`),
`mathlib4/DarkTower/FirstFlightsExample.lean` (T8, the fold grain),
futon6 `holes/bv-comb-typing.edn`, futon6 `holes/early-closures.md`.
