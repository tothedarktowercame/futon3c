# E-distributed-formalisation — formalising paper skeletons hole-by-hole

**Excursion (synthesis / charter). Spun 2026-06-15, Joe↔claude-3, relaying a
Joe↔Rob exchange.** Caps the day's chain: `E-queries-and-scopes` →
`E-question-driven-scope-coverage` → the IATC Argument-to-Clojure pilot
(`close-reading/iatc-clojure/PILOT-NOTES.md`) → here.

## HEAD (Joe, 2026-06-15, verbatim sense)

`M-typed-holes` could give the IATC EDN some lightweight checking. Probably not an
easy "put in the paper, get out a proof" — but maybe the **structure-first** idea
could develop formal proofs of a **skeleton** of a paper, or even just segments:
the **Distributed Proofreaders** idea again, but **Distributed Formalisation**.

## The lineage (why "again")

- **Distributed Proofreaders** — volunteer book digitisation: per-page correction
  passes; quality is a property of the *revision process*, never a single perfect
  snapshot (the Moran-1971 *because*-clause lesson).
- **`M-distributed-proofreaders`** (futon6) — that same loop, work-unit =
  *recognition*: mine → measure loss → fix the worst defect class → re-mine,
  anytime, never perfect.
- **Distributed Formalisation** (this excursion) — the *same process model*,
  work-unit = a **formal fill**: discharge one typed hole with a proof.

Same never-perfect, quality-as-process discipline; new unit of work. And it
matches Rob's "real-time on build" exactly: his neo4j/pgvector refreshes each Lean
build; a Distributed-Formalisation store refreshes each time **a hole is filled** —
the fill *is* the build event.

## The unit of distribution is the typed hole

The IATC argument graph (`E-iatc-model`; the pilot's 13 EDN graphs) turns the
monolithic "formalise this paper" into a **graph of explicit, typed, largely
independent obligations**, with dependency edges giving the order. Proofreaders
distributed *pages*; Distributed Formalisation distributes **holes**. This is
`M-typed-holes` made operational: the `(typed-hole, fill)` datatype, where a
`:missing-warrant` **is** a Lean `sorry`, and ArSE is the witness store —
*proof = witnessed fill* (projections #2 sorry→proof and #6 illocutionary/ArSE).

## The bridge artifact: the skeleton-with-sorries

Promote the IATC argument graph to a **Lean proof-skeleton**: `:claim` nodes →
theorem/lemma statements; each `:missing-warrant` → a `sorry`. This object lives
in *both* worlds at once — it is our typed-hole EDN **and** a Lean file with
holes. It is the concrete thing to build, and it is exactly "the skeleton of a
paper" the HEAD asks for.

## The graded ladder (each rung cheap and independently useful)

1. **Well-formedness** — `iatc_argcheck.bb` (exists): endpoints resolve, holes
   explicit, subproofs nested.
2. **Argument-*form*** — lift the checker to **typed-hole compositional** checks
   via `M-typed-holes`' DarkTower `Fill`/`Comb`/`Discharge` laws (fill unit+assoc;
   discharge = counit): do the fills compose, is each `:conclusion` licensed by
   the hole-shape of its premises+warrant? Checks the *form*, not the *truth* —
   catches structurally-broken arguments before any Lean effort is spent.
3. **Lean skeleton-with-sorries** — lift the IATC graph to Lean statements + a
   `sorry` per `:missing-warrant`; confirm it builds (with sorries).
4. **Distributed sorry-filling** — the Distributed-Formalisation loop: a pool
   (Codex/Lean, or humans) discharges holes; each fill witnessed in ArSE.
5. **(limit) Full formalisation** — the asymptote, not the entry fee.

Most of the value is in the lower rungs **at scale** (9.7k papers Lean will never
reach). The holes always mark honestly *where you are on the ladder*.

## The hard part: types first (Rob's lesson, independently arrived at)

A hole is formally fillable only once the objects it talks about are **typed**.
Rob's hard-won lesson on his Lean work: *"the mistake was not defining the Lean
types first."* So the right granularity is **segment / skeleton, not whole paper**:
a segment is formalisable when its `:object`/`:definition` nodes are reachable
(e.g., already in mathlib). The IATC `:object`/`:definition` nodes are the seeds of
that type layer. (We arrived at the same "structure/scope-first" commitment from
the informal side — see the seed-hierarchy-first method of
`E-question-driven-scope-coverage`.)

## Map and territory

arXiv IATC graphs are a **map** — where mathematics' arguments run, and where
their warrants are owed. Lean is verified **territory**. The typed hole is the
**road** between them: a `:missing-warrant` on the map is a formalisation target in
the territory; matching one to a discharging Lean lemma (structurally via the
argument graph, semantically via embeddings) is the join. We already hold the
infrastructure analog of Rob's stack: **`substrate-2`** (the Arxana hypergraph,
:7071) ≈ his neo4j; the **CT/MO embeddings + faiss / futon3a** ≈ his pgvector;
**`ScopeQuery`** (`E-queries-and-scopes`) ≈ the query layer. The IATC pilot added
the missing piece — the *inference* graph, not just the object graph.

## First rung (buildable, bounded)

Pick **one segment of one pilot paper** (`0905.0595` / `0807.1872` / `1005.2653`)
whose `:object`/`:definition` nodes are mathlib-reachable. Lift its IATC skeleton
to Lean statements + `sorry`s at each `:missing-warrant`; confirm `lake build` is
green (with sorries). That *is* the bridge artifact. Then a small distributed
fill on the easiest hole, witnessed in ArSE. **Honest scope:** this proves the
*pipeline* (IATC graph → Lean skeleton → distributed fill), not a hard theorem.

## What this is NOT

Not autoformalisation ("paper in, proof out"). It is an *unverified-but-structured*
layer that **points into** the verified one, with the holes marking honestly what
is owed vs. done. That is precisely what Lean alone cannot give — Lean only sees
what is already formalised; the IATC map sees the whole corpus and where it would
need to be.

## Challenging steps & who's the user (the honest counterweight)

The ladder above reads optimistically; this section is the feasibility gate and
the value question, so the excursion isn't a pitch.

**Which steps are genuinely hard.** Rungs 1–2 (well-formedness; argument-*form*
checking) are tractable — the checker and the DarkTower Fill/Discharge laws exist.
But:
- **Rung 3 (IATC `:claim` → Lean *statement*) is the wall.** It presupposes BOTH
  (a) the objects being typed / in mathlib — most arXiv CT objects are not — and
  (b) statement-level autoformalization, itself an open problem at middling
  accuracy today.
- **Rung 4 (sorry-filling) is the research frontier** — where Rob himself is stuck
  ("this very complex combinatorial proof").

So the *formalization* part of the ladder leans on two unsolved problems. **Do not
stake the value on climbing it.**

**The reframe: value decouples from the hard steps.** What is already built — the
IATC argument graphs + scopes + explicit warrant-holes, queryable structurally
(`ScopeQuery`/substrate-2) and semantically (embeddings/faiss) — is a
**reasoning-aware structural+semantic index of the mathematical literature**, and
that is valuable with *zero* formalization. Formalisation is upside option, not the
product.

**Two real users (not a menu):**
1. **Now, no formalization — the working mathematician doing structure-first
   search.** This is *Rob's own use case* ("done out of necessity"), at arXiv scale
   instead of one Lean repo: "what arguments depend on this lemma / reuse this
   construction / leave this step unproven, across the literature?" Deliverable
   today from what we have; lowest risk; validates the extraction.
2. **Higher-value, needs only the lower-middle ladder — AI-for-math /
   autoformalization labs.** They are bottlenecked on exactly what our artifact
   provides and they mostly lack: **decomposition of papers into formalizable
   units, a ranked list of formalization targets, and informal↔formal alignment.**
   Most autoformalization works statement-by-statement with no argument/dependency
   structure. The hard rungs (3–4) are *their* problem; we sell the **map and the
   targets**, not the proofs.

**Competitive read.** It's a gold rush (DeepMind, Morph, Harmonic, mathlib-adjacent
efforts), so "buyer" must mean *differentiated*. Our differentiator is the
**informal argument graph with explicit warrant-holes over the full corpus** — not
already-formal data, not isolated statements. That niche appears unoccupied.

**De-risked entry.** Build user (1)'s thing first — structural+semantic argument
search over the extracted corpus — because it needs none of the hard steps, it is
the smallest sellable artifact, and it is the demo that earns a conversation with
user (2). Distributed Formalisation then raises the ceiling for buyer (2) rather
than gating any value. **The minimal sellable product is the map, not the proofs.**

**Feasibility gate — the superpod grounding pass (marker, 2026-06-16).** The
quality worry is partly an artifact of the current representations being all-CPU /
all-classical and **under-grounded** (dp-demo grounded% runs 88% down to **12%**);
a hygiene-map on 12%-grounded input would cry wolf. Joe's read: the 8-GPU superpod
can lift grounding materially, and "the rest of the pipeline would work" once the
representations are *nice* (~a week out, as of 2026-06-16). Crucially: **better
grounding lifts the structural *map* (the in-reach, fundable, non-judging thing)
but does NOT touch the correctness-judgment layer** — grounding binds symbols to
concepts; it doesn't prove steps. So refereeing stays out of reach regardless, and
the moat picture is unchanged. **Do not run the "is the non-judging map useful on a
hard paper" test until post-grounding** — testing the under-grounded artifact would
fail unfairly and teach nothing. Signal to watch when it lands: whether the
**debt-heavy papers (1005.2653, 0711.1761) jump from noise to usable maps** — if
so, grounding was the bottleneck and the map is real; if not, the problem is
deeper. Conclusion-in-progress, not a verdict.

## Relations

- **`M-typed-holes`** (futon3c) — the `(typed-hole, fill)` datatype; DarkTower
  `Fill`/`Comb`/`Discharge` laws (rung 2); projections #2 (sorry→proof) and #6
  (illocutionary / ArSE witness). This excursion is projection #2 fed at scale.
- **`M-distributed-proofreaders`** (futon6) — the loop this re-uses; recognition →
  formalisation.
- **`E-iatc-model`** (futon6) — the IATC inference layer; source of the EDN graphs.
- **IATC pilot** — `close-reading/iatc-clojure/` (13 checked graphs),
  `futon6/scripts/iatc_argcheck.bb` (the checker to lift at rung 2).
- **`E-queries-and-scopes`, `E-question-driven-scope-coverage`** — the scope/query
  substrate and the structure-first method; this session's earlier chain.
- **Rob's Lean + neo4j/pgvector** (relayed 2026-06-15) — the verified-territory
  counterpart: realtime structural+semantic index on Lean build.
- **`substrate-2`** (Arxana, :7071) + **futon3a embeddings** — our neo4j/pgvector
  analog already in hand.
