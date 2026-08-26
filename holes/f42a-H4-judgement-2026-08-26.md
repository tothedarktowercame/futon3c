# f42a — H4 judgement: do the cascade's additions bear on the obstacle f42 hit?

*claude-19, 2026-08-26. The judging seat did not compute the expansion:
the real run is codex-20's (`915a4aa1`, `77a1bac0`;
`holes/f42a-cascade-run-cap{100,1000}.edn`), re-run once by the judge and
found byte-identical. Per PLAN H4, both answers were worth having; this is
the answer.*

## Inputs

**What the cascade adds to f42's shelf (real run, cap 1000).** 103 memories:
48 by `:why-hop` and 55 by `:co-incidence`. All 48 why-hop additions arrive
through one pattern, `math-strategy/missing-dependency-protocol`, and they
are exactly its 48 reviewed attachments (checked: 48 of 48 ids match). The
co-incidence additions arrive via 15 patterns, led by
`math-strategy/corpus-trust-protocol` (14) and
`math-formalization-FA/weak-convergence-hilbert` (7).

**What f42 actually hit** (`live/student-attempt-1.edn`, `:failure-account`,
six items; the attempt proved a97J07 — the Schwarz-style bound
‖f 0‖ ≤ √(ab) from bounds on the open upper and lower arcs):

1. API name: `Complex.norm_le_of_forall_mem_frontier_norm_le` is actually the
   root-namespace `norm_le_of_forall_mem_frontier_norm_le`. One `#check`.
2. **The mathematical crux**: hypotheses bound ‖f‖ only on the *open* arcs;
   the frontier bound needs the endpoints z = ±1. No Mathlib "bound on a
   dense subset extends to the frontier" was found; fixed by an explicit
   `arc_limit` lemma (normalised curve on the sphere, `le_of_tendsto`).
3. `mul_le_mul` signature friction; two failed applications.
4. False-positive `sorryAx`: the frozen file lacked `open Topology Filter`;
   unknown identifiers elaborated into placeholder sorries. Lesson recorded by
   the student: read the full compile output before hunting axioms.
5. Shelf: of 40 accessible memories only `e-f72e5ece` (Blaschke constancy,
   a01J05) was used — for the max-modulus API pairing
   (`norm_le_of_forall_mem_frontier_norm_le` + `DiffContOnCl`). A guide memory
   describing exactly the arc-limit residual (`e-apm-promotion-f240c45c…`)
   surfaced only *after* the proof was complete.
6. Minor tactic friction (six one-round-trip name errors).

## The 48 why-hop additions against those six

Read one by one (table in the working notes; names are the ids' own):

- **On the crux (item 2): none.** The 48 contain seven mathematical
  statements — Poisson a.e. convergence, two lemniscate facts,
  `zeroCountInClosedBall` homotopy invariance, a logDeriv circle integral,
  integral Minkowski, a circle-submean-to-disk-area lift — and none concerns
  extending a boundary bound from a dense subset of the sphere, continuity to
  the boundary, or the maximum modulus principle with partial boundary data.
  The other 41 are process rules ("inventory assembly dependencies before
  polishing leaves", "stop research after repeated API miss", "scope absence
  claims to the index searched", "leave a consultation trail", …) and
  open-hunger markers for other problems (Schwarz–Pick two fixed points,
  Lebesgue density, rectangular contour residue).
- **On item 4 (false-positive sorryAx): two, weakly.**
  `preflight-file-elaboration-before-sorry-accounting-or-runner-dispatch`
  ("treat successful elaboration of the exact target as a precondition for
  proof-hole measurement") and
  `separate-lexical-sorry-count-from-real-proof-hole-count` describe the
  sorryAx-without-textual-sorry confusion the student walked into. Had either
  been read, item 4 would have cost less than the one round trip it cost. Both
  are regulative, in V2's sense; neither supplies content.
- **On items 1, 3, 6: none** — these are Mathlib-revision name/signature
  frictions, and the hub's entries are not about this API surface.
- **On item 5: the cascade does not reach the memory that would have helped.**
  The arc-limit memory the guide later deposited is a same-problem leaf; no
  pattern route reaches it, and it did not exist at attempt 1.

The 55 co-incidence additions were scanned by pattern rather than one by one:
`corpus-trust-protocol`, `weak-convergence-hilbert`, `series-evaluation-api`,
`derivative-bounds-api`, `inner-product-space-api`, … None of the fifteen
patterns is about boundary behaviour of holomorphic functions on the disk.

## Verdict

**No, for the obstacle that mattered.** On f42 the cascade adds 103 memories
and none bears on the mathematical crux. Two process memories bear weakly on a
one-round-trip apparatus slip. This is the PLAN's second outcome — **volume
without relevance** — and it is the sharper argument against H3 that the PLAN
anticipated: tripling a hash-ordered shelf on which the used memory already
sat at 47 of 48 makes the delivery worse, not better.

## Two things the judgement is not

- It is not an outcome claim. f42 closed on attempt 1; nothing here measures
  what the student would have done with 148 entries.
- It is not a claim about the expander's design. `expand-memory-cascade` did
  what its receipt says: shortest authored route, cheapest route per memory,
  cap reported. The result is a fact about the **store**: `why` edges from
  f42's 23 seed patterns lead, once seed patterns and memory-less targets are
  removed, to a single hub whose 48 attachments are the codex-sorry-loop era's
  process rules. That is V2 §4.6's "graph never populated", seen from the
  consumer's side.

## What follows for the plan

- **The why-hop block is the same 48 memories whoever asks.** f10 (2 seeds)
  got 48 why-hop; f15 (5 seeds) got 48; f42 (48 seeds) got 48
  (D1 technote; D0 artifacts). Any seed set touching `measure-integration-api`
  or `holomorphic-disk-api` — both `@why` the hub — receives the whole hub.
  A cascade whose expansion is constant in the seed is not a cascade over
  this store; it is one extra shelf appended to every shelf.
- **H3 stays last and stays gated**, as Joe ordered. Why-hop-only wiring
  would deliver this fixed block of 48 to every student.
- **H5 is the lever, and it is now specific.** Not "populate the graph" in
  general: (a) attach memories to the patterns they are *about* rather than
  to the protocol under which they were mined — the seven mathematical
  statements in the hub belong to `holomorphic-disk-api`,
  `measure-integration-api`, etc., where a why-hop from a related seed would
  actually narrow; (b) give the hub's process rules a home that is not a
  `math-strategy` pattern reached from every API pattern.
- **H2 is unaffected** and remains the change that helps whatever happens
  to the graph.
- **The whitepaper's "descent from high-level patterns" model** needs the
  hub's shape stated as its counterexample: descent here reaches one node
  with 48 children and no structure among them.
