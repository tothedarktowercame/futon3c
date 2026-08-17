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
