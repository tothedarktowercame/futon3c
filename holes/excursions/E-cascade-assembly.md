# E-cascade-assembly — what assembly could look like, measured against the real graph

**Opened 2026-08-17 by claude-2 (Analyst, M-apm-demonstration) at Joe's
direction**, after the semantic edges first reached the store (futon3a
`b7c7332`, futon3c `fdd6d4e7`). Joe: *"I'm not sure what the cascade assembly
actually looks like. So, maybe we should generate several possibilities, as well
as some synthetic projections for what it might look like if the store of
patterns grew considerably."*

Design exploration. Nothing here is built, and no option is recommended as the
only one — the point is to make the choices visible against real numbers before
committing.

## 1. The graph as it actually is, today

Measured 2026-08-17 over `futon3/library/math-*` and the live store:

| | |
|---|---:|
| math patterns | **103** |
| semantic edges (`@why` + `@see-also` targets) | **55** |
| patterns carrying ≥1 semantic edge | **20** |
| patterns carrying ≥1 reviewed memory | **31** |
| total memory attachments on math patterns | **230** |
| patterns with BOTH edges and memories | **20** |
| patterns with memories but NO edges (unreachable by traversal) | **11** |
| patterns that are pure connectors (edges, no memories) | **0** |

Three structural facts follow, and they constrain every option below.

**The graph has no interior.** Zero pure connectors: every node that routes also
carries payload. Nothing exists purely to relate other things. That is a
consequence of how the edges were authored — all 55 were written in one sitting
on 2026-08-17, as a side effect of writing 19 patterns, so every edge's source
is one of those 19. This is not an organically grown web and should not be
generalised from too confidently.

**It is shallow, but not trivially so.** `@why` chain depths: 12 patterns at
depth 1, 6 at depth 2, 1 at depth 3. Of 33 distinct edge targets, only **9** are
themselves edged; **24 are dead ends** — mostly `math-informal/*` classical
strategies that carry no memories.

**It is sparse.** 83 of 103 patterns have no semantic edge at all. Any assembly
strategy that relies solely on `@why`/`@see-also` is blind to 80% of the
library.

## 2. Why a cascade is qualitatively different from leaf lookup

The deepest chain in the graph today:

```
math-formalization-CA/riemann-darboux-api          1 memory
  --@why--> math-formalization-CA/measure-integration-api    46 memories
  --@why--> math-strategy/missing-dependency-protocol        48 memories
  --@why--> math-informal/reduce-to-known-result              0 memories
                                                       total 95
```

**Leaf lookup on `riemann-darboux-api` returns 1 memory. The cascade reaches
95.** But the count is the least interesting part. The 94 additional memories
are not *about* Riemann–Darboux — nothing in them says "Darboux". They are about
locating measure-theoretic statements in the library, and about what to do when
a needed theorem is absent. Which is precisely the work a Darboux→Lebesgue
bridge runs into.

That is the qualitative difference, stated plainly:

- **Leaf lookup is similarity retrieval.** It returns memories whose text, tags
  or problem id resemble the query. It answers *"has anyone worked on this?"*
- **A cascade is structural retrieval.** It returns memories attached to
  patterns that stand in an authored relation to the situation. It answers
  *"what governs a situation of this shape?"* — and the answers are drawn from
  problems that are not yours and share no vocabulary with yours.

A similarity search cannot reach the 94, because the thing that connects them to
the query is not a feature of the text. It is a claim someone made — `@why` —
that this specific concern is an instance of that general one. The edge carries
information no embedding of the two documents contains.

**Worked example against a real problem.** `a95A01`'s memories attach to
`riemann-darboux-api`, `measure-integration-api` AND
`missing-dependency-protocol` — three patterns on a single `@why` chain. So the
structure the cascade would walk is the structure that problem's own history
already traced, independently. That is a weak confirmation that the authored
edges track something real rather than one author's taste, and it is the first
such check available.

## 3. Five assembly strategies

### A. Ancestor walk (`@why` closure)

From the seed pattern, follow `@why` to the root, collecting memories at each
hop. Ordered specific → general.

- **Cheap and deterministic.** No ranking, no tuning; the walk terminates in ≤3
  hops today.
- **Reads correctly**: the ordering is an explanation — *here is your case, here
  is the strategy it instantiates, here is the principle behind that*.
- **Weakness**: reaches nothing for the 83 unedged patterns, and terminates in
  `math-informal/*` roots that carry no memories, so the last hop is usually
  empty.

### B. Ancestor + siblings (up, then down one)

Walk `@why` up; at each ancestor, also collect its *other* children — the
patterns that share a governing strategy.

- **This is the analogy move**, and probably where the distinctive value is.
  From `measure-integration-api`, going up to `missing-dependency-protocol` and
  back down reaches `weak-convergence-hilbert` and `riemann-darboux-api`: peer
  work under the same discipline, which no similarity search would surface.
- **Weakness**: fan-out is governed by hub in-degree, which is exactly what
  grows worst at scale (§4).

### C. `@see-also` neighbourhood (peer expansion to radius k)

Breadth-first over `@see-also` only.

- Peer techniques, no authority ordering — appropriate when the question is
  "what else is in this neighbourhood?" rather than "what governs this?"
- **Weakness**: no gradient. `@see-also` makes no authority claim in either
  direction, so radius-2 drifts without any principle telling you when to stop.

### D. Category-scoped expansion (the arXiv coordinate)

Use the primary category (the directory) and `@cross-list` as the relation.

- **The only strategy that covers the unedged 80%.** It needs no authored edge,
  because the coordinate is already there in every pattern's location.
- Cross-listing gives genuine cross-domain reach: `proof-architecture`
  cross-lists `[CA FA]`, so a functional-analysis problem can find a pattern
  filed under strategy.
- **Weakness**: it is a bucket, not a claim. Same-category is much weaker
  evidence of relevance than an authored `@why`.

### E. Weighted composite

Score each reached memory by how it was reached: `@why` ancestor (highest —
authored authority), sibling-under-shared-parent (high), `@see-also` (medium),
same-category (low), and decay by hop distance.

- Degrades gracefully as the graph fills in: category-heavy today, edge-heavy
  later, without changing the interface.
- **Weakness**: introduces weights, which are tuning knobs, which are a
  standing invitation to fit them to whatever demo is in front of you. If this
  is chosen, the weights need to be a stated policy with a reason, not
  constants someone nudged.

## 4. Synthetic projection: what happens as the store grows

Assume the authoring discipline holds — each new pattern declares one `@why`
and ~2 `@see-also`, as today's 19 did.

| | now | ×3 (≈300) | ×10 (≈1000) |
|---|---:|---:|---:|
| patterns | 103 | ~300 | ~1000 |
| semantic edges | 55 | ~900 | ~3000 |
| mean out-degree | 0.53 | ~3 | ~3 |
| `@why` depth | 3 | 4–5 | 5–6 |
| memories | 230 | ~700 | ~2300 |

Depth grows roughly logarithmically because `@why` **converges**: specific
patterns point at strategies, strategies point at a small set of informal
principles. The library gets wide, not deep. Cascade cost therefore stays
bounded — a walk of 5 hops, not 50.

**The real problem at scale is hub saturation, and it is already visible.**
Today two patterns hold 94 of 230 attachments — **41% of all memory on 2% of the
patterns**. `missing-dependency-protocol` alone has 48, and it is the `@why`
target of at least three other patterns. Every cascade that walks upward
converges on it.

At ×10, that hub plausibly carries 400–500 memories. A cascade that reaches it
and returns everything has not retrieved — it has flooded. **Any assembly
design needs a fan-in policy at hubs from the start**: a cap, a relevance
filter, a recency or review-status gate, or a rule that hub memories are
summarised rather than enumerated. Choosing this late means every consumer has
already been built against the flooding behaviour.

Second-order effect worth watching: as the library grows, the 11 patterns with
memories but no edges are the shape of the default. New patterns get written
faster than anyone authors edges for them. If `@why` authoring lags pattern
authoring, option D (category) stops being a fallback and becomes the primary
mechanism by weight of numbers.

## 5. What would settle the choice

- **Does the structural reach actually help?** The 94 non-obvious memories from
  the worked chain are a hypothesis, not a result. The test is whether an agent
  given them solves something it otherwise would not — which is the same
  measurement the transfer checks already make.
- **Hub policy before hub pain.** Decide the fan-in rule now, while
  `missing-dependency-protocol` has 48 memories and the wrong answer is cheap.
- **Is `@see-also` load-bearing or decorative?** It has 36 targets and no
  authority gradient. If no assembly strategy ends up using it, that is a
  finding about the vocabulary, not just about the code.
- **Depth-1 dominance.** 12 of 20 edged patterns are depth 1, so most cascades
  are one hop and barely distinguishable from a join. The interesting behaviour
  lives in the 7 patterns at depth ≥2. That is a thin base to design on, and
  more authored `@why` edges would widen it cheaply.

## 6. Honest caveat

Every number here comes from a graph authored by one agent in one afternoon.
The 3-hop chain is a single instance. `a95A01`'s three-pattern agreement is a
single confirmation. This document is a map of the design space with real
coordinates attached — not evidence that cascades work.

---

# Part 2 — Graph shapes under a Laplacian, measured (2026-08-17)

At Joe's direction, after §1's finding that the graph is a star-forest with no
interior: *"hub saturation is 'better than' just isolated stars… but we could
readily imagine different shapes that might work better in one way or another."*

Grounded in `docs/retrieval-whitepaper-v2.md` §4.5, which already establishes
the relevant caution — V1's spectral admissibility criterion (λ₂ > 0.1) was
**retracted** for anti-correlating with useful structure, because the deployed
graph's largest component was a single hyperedge attaining **λ₂ = 1.0 by
construction**, and because *"the corpus grows as star-forests and is closed
under adding memories."* That is exactly the shape §1 measured, arrived at
independently.

## Method

Real graph: 103 nodes, 55 edges, payload = 230 measured memory attachments.
Candidate shapes built on the **same budget** (103 nodes, 55 edges) so the
comparison is not confounded by size. λ₂ is the second eigenvalue of the
degree-normalised graph Laplacian on the largest component (Jacobi solver, pure
Python — no numpy on this host). `yield@3` is the mean number of memories
reachable within 3 hops from a random seed.

## Result

| shape | λ₂ | comps | maxdeg | yield@3 |
|---|---:|---:|---:|---:|
| **actual (star-forest)** | **0.1024** | 60 | 8 | **65.5** |
| single hub | **1.0000** | 48 | 55 | 125.0 |
| few hubs (5) | **1.0000** | 48 | 11 | 26.8 |
| balanced tree (b=3) | 0.1071 | 48 | 4 | 52.9 |
| path/chain | 0.1734 | 48 | 2 | 14.3 |
| ring + shortcuts | 0.1744 | 49 | 2 | 15.6 |

### 1. v2's inversion reproduces on our own graph, on a different operator

The single-hub shape — the flooding pathology §1 warned about — scores
**λ₂ = 1.0000, the maximum**. `few hubs (5)` also scores 1.0000 while yielding
**4.7× less** (26.8 vs 125.0), so λ₂ does not even separate two shapes with
wildly different retrieval behaviour. Meanwhile `path/chain` has the best
non-degenerate λ₂ (0.1734) and the **worst** yield (14.3).

**λ₂ ranks these shapes roughly inversely to what a cascade wants.** v2 found
this with the Zhou degree-normalised *hypergraph* operator on a commit corpus;
this is the ordinary graph Laplacian on the pattern corpus. The retraction
generalises — it was not an artifact of that operator or that corpus.

Worth noting for its own sake: the real graph sits at **λ₂ = 0.1024**, almost
exactly on V1's retracted 0.1 floor. It would *pass* a criterion that was
withdrawn for being uninformative. Nothing should be read into that except that
the floor is meaningless here.

### 2. Shape sets reachability; payload PLACEMENT sets yield

The table above assigns the real payload multiset to nodes by degree (the
realistic assumption: heavily-used patterns accrete memories). Re-running with
payload assigned **at random**, 5 trials each:

| shape | yield@3 (degree-assigned) | yield@3 (random, mean) | random range |
|---|---:|---:|---:|
| single hub | 125.0 | 64.2 | 28.4 – 109.0 |
| few hubs (5) | 26.8 | 15.8 | 12.7 – 23.6 |
| balanced tree | 52.9 | 16.0 | 6.0 – 27.4 |
| path/chain | 14.3 | 8.6 | 4.8 – 13.6 |
| ring + shortcuts | 15.6 | 8.9 | 4.9 – 13.8 |

The **ranking** is stable — hub-ish shapes reach more, chain-like shapes reach
least — but the **magnitudes roughly halve**, and single hub alone spans
28.4–109.0 across trials. So *where the memories sit* moves yield as much as the
topology does. Arguing about shape in isolation is under-determined; a shape
proposal has to come with a claim about which patterns accrete payload.

### 3. A metric I proposed in §4 does not survive — retracting it

§4 called for a hub fan-in policy and I proposed measuring "flooding" as the
largest single contributor's share of reached payload. Under degree-assigned
payload, `path/chain` scores 0.09 (excellent) and `single hub` 0.11. Under
random assignment the same shapes score **0.52 and 0.33**. The metric flipped by
5× on `path/chain` without the graph changing at all.

**Flooding as I defined it is a property of the payload distribution, not of the
shape.** It should not be used to rank topologies. The underlying concern from
§4 stands — two patterns hold 41% of all attachments — but it needs a measure
that is invariant to how payload was assigned, and I do not have one yet.

## What this changes about §3's options

- **Do not gate on λ₂.** Neither as an admissibility criterion nor as a shape
  score. v2 retracted it once; this reproduces the reason on our data.
- **`few hubs` is the trap.** It is the shape a growing library drifts into
  naturally — a handful of strategy patterns accreting everything — and it
  scores worst-but-one on yield while looking perfect on λ₂. The current
  star-forest is genuinely better than the shape it is drifting toward, which is
  the opposite of the intuition that consolidation helps.
- **The actual graph is a reasonable compromise already** (yield 65.5, second
  only to the degenerate single hub, at maxdeg 8). Joe's read that hub
  saturation beats isolated stars is right, and the current shape sits between
  them rather than at either extreme.
- **Depth is cheap, breadth is not.** Chain-like shapes have good λ₂ and poor
  yield because 3 hops on a degree-2 graph reaches ~4 nodes. If cascades are to
  be shallow (§4 projects depth 5–6 at ×10), the graph needs branching, not
  length.

## Caveats

Same as §6, plus: these shapes are synthetic and regular, whereas a real library
would be irregular; `yield@3` counts memories reached, not memories that helped,
which is the quantity we actually care about and cannot measure without the
transfer checks; and every shape here leaves ~48 nodes isolated because 55 edges
cannot connect 103 nodes — the comparison is between *sparse* shapes, which is
the honest regime today but will not be the regime at ×10.

---

# Part 3 — The naturalistic graph, and what it says about the small one

Joe, after Part 2: *"so far this seems inconclusive… what if we look at the
scale of the whole futon3/library? They don't all have whys and hows, but they
do exist as part of a graph — linked by missions that cite them."*

He was right that Parts 1–2 were inconclusive: they analysed 55 edges authored
by one agent in one afternoon. This part uses a graph nobody authored.

## Method, and the trap avoided

Scanned 271 mission/excursion documents across futon3, futon3b, futon3c and
futon4 for references to any of the **1,302** library pattern ids (103
namespaces).

**One document, `futon3c/holes/excursions/pipeline-semilattice-clusters.md`,
cites 267 patterns** — a catalogue of cluster output, not a mission using
patterns in the course of work. It alone is 37% of all incidences, and under
clique expansion would generate 35,511 of 38,355 pairwise edges — **93% of the
graph from one file**. Excluded, and kept as a bipartite/hypergraph structure
throughout: a document citing *k* patterns is one incidence relation, not
k(k−1)/2 pairwise ones. This is the error `retrieval-whitepaper-v2.md` §4.5
records as having *"already inverted a metric for us once"*.

## Three graphs over the same library

| | authored `@why`/`@see-also` | mission co-citation | problem co-attachment |
|---|---|---|---|
| population | math patterns | engineering patterns | math patterns |
| relation | an authored claim | a doc cites both | a problem attaches memories to both |
| nodes | 103 (20 edged) | **273** | 30 |
| edges / incidences | 55 | **446** (83 docs) | 134 (70 problems) |
| components | **60** | **12** | 7 |
| giant component | 43% | **92%** | 57% |
| mean 2-hop reach | 8.7 *(at 3 hops)* | **19.0** | 3.5 |
| items per document/problem | — | mean 5.4 | **mean 1.9** |

### 1. The three structures are very nearly disjoint

- Of **55** authored semantic edges, only **2** have both endpoints cited by any
  mission, and only **1** has its endpoints co-cited in the same document.
- Of **103** math patterns, only **13** are cited by any mission document.
- **273** patterns appear in the citation graph, **44** in the authored graph,
  and only **8** in both.

They are not competing descriptions of one structure. They describe different
populations doing different work: **missions cite engineering patterns**
(`realtime/`, `aif/`, `futon-theory/`, `agent/`, `peripherals/`), while
**mathematics patterns are used by problems**, and that usage is recorded as
memory attachments rather than as citations.

This is why Parts 1–2 could not conclude. They analysed the authored math graph
in isolation, which is the youngest and thinnest of the three.

### 2. The mature graph is well-connected, and nobody authored it

The mission co-citation graph puts **92% of cited patterns in one component**
with a mean 2-hop reach of **19 peers** — against the authored graph's 43% and
8.7. Its connectivity does not come from high-degree patterns: **64% of cited
patterns are cited exactly once**. It comes from the *documents*, each of which
touches a mean of 5.4 patterns and thereby relates them.

That is what a pattern-usage graph looks like once real work has flowed through
it, and **it accrues for free** — no one wrote those relations down as edges.

### 3. The math domain's bottleneck is not missing `@why` edges

The problem co-attachment graph is the correct analogue for mathematics, and it
is thin: 70 problems, 30 patterns, 134 incidences, giant component 57%, mean
2-hop reach 3.5.

The decisive number is **1.9 patterns per problem** (median 2, max 5), against
**5.4 patterns per document** in the mature corpus. Connectivity in the mature
graph is manufactured by documents that touch several patterns at once. Math
problems touch barely two.

So the cheapest way to make math cascades richer is **not** to author more
`@why` edges. It is to raise patterns-per-problem — an attach-time behaviour, in
the machine's own loop, not an authoring chore. Going from 1.9 to 5.4 would do
more for reach than doubling the authored edge count, and it is the kind of
thing the cycle can be made to do rather than a human.

Note also that only **30 of 103** math patterns have ever been attached to at
all. The library is largely unused, not largely unconnected.

## Reimagining assembly from the big context down

Part 1 offered five strategies over the authored edges. The three-graph
comparison replaces that framing:

**Reach should ride co-incidence; ordering should ride authored edges.**

- **Co-incidence (problem×pattern, mission×pattern) supplies breadth.** It is
  free, empirical, already covers 92% of the mature corpus in one component, and
  needs no authoring discipline to keep pace with the library. It is the only
  structure that will not lag.
- **`@why` supplies direction and explanation.** Co-incidence is undirected and
  makes no claim: it says *these were used together*, never *this governs that*.
  The specific→general ordering that makes a cascade readable as an argument
  exists only in the authored edges.
- They are complementary, and — measured above — currently **almost disjoint**,
  so using either alone forfeits most of the library.

This also retires the framing of Part 1 §3 options A–C as alternatives. They are
orderings over a reach set; the reach set should come from co-incidence.

## What would settle the remaining questions

- **Does patterns-per-problem actually move?** It is a measurable property of
  the cycle and the most leveraged number found here. Track it per frame; it is
  the natural companion to the S-readings.
- **Do the two structures converge as the math corpus matures?** Today 1 of 55
  authored edges is co-cited. If the fraction rises as problems accumulate, the
  authored edges are predicting real usage; if it stays flat, they are a private
  taxonomy and should be justified on other grounds.
- **Is the engineering graph's 92% the right target, or an artifact of documents
  that enumerate?** The catalogue exclusion moved 267 incidences; a second such
  document would move the number again. The giant-component figure should be
  re-derived with an explicit rule for what counts as *use* rather than mention.

## Caveat

The citation graph is built by string-matching pattern ids in prose, so it
counts *mentions*, not uses — a document arguing against a pattern cites it the
same way as one applying it. The co-attachment graph is cleaner (an attachment
is an act, not a mention) but far smaller. Neither has been checked against
whether the reached material actually helped, which remains the quantity that
matters and the reason prerequisite 2 exists.

---

## What Part 3 changes about prerequisite 1 (use-receipts)

This excursion is groundwork for build item 1, not item 1. But Part 3 changed
what item 1 has to record, so the definition is worth pinning before it is
built.

**A use-receipt must record the ROUTE, not just the hit.** If reach rides
co-incidence and ordering rides authored edges (Part 3), then a receipt saying
only *"memory X was surfaced and used"* cannot distinguish:

- the memory a plain leaf lookup would have found anyway;
- the memory reached only via co-attachment with another problem;
- the memory reached only via an authored `@why` hop.

Those are three different claims about the machinery, and only the third
justifies the authoring cost of `@why`. A receipt without the route makes the
cascade unfalsifiable — it will always appear to work, because the leaf hits are
in the same bucket as the structural ones.

Minimum fields, therefore: memory id, the route that surfaced it (`leaf` /
`co-incidence` / `@why-hop`, with hop count), whether it was used, and the frame
it was taken in.

**And patterns-per-problem should be in the same receipt.** Part 3 identified it
as the most leveraged number available — the mature corpus runs at 5.4
patterns per document, the math corpus at 1.9, and connectivity comes from that
number rather than from authored edges. It is currently tracked **nowhere**
(grep across futon3c finds it only in this file). It is cheap to record at
attach time and it is the leading indicator for whether cascades will have
anything to traverse.

**Supply is a separate axis from receipts.** Two routes discussed with Joe on
2026-08-17 — GPU mining over already-solved problems and their transcripts, and
proto-patterns from his existing Math.StackExchange pass — both raise supply
(more patterns, and more attachments per problem). Neither is item 1, and
neither is blocked by it. They are the input side; use-receipts are the
measurement side. Worth keeping distinct so that "we mined more patterns" is
never mistaken for "cascades were shown to help".
