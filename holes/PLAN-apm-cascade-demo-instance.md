# PLAN — one APM frame where a cascade is built AND used

*Joe, 2026-08-26: "continue to work on the APM case so that we can at least have
one demo instance where cascades are built and used … the TN I shared is a
'finding' not yet a handoff ready plan for how exactly cascades would be used.
It's still probably the 'best case' across the stack at the moment."*

Successor to `technotes/TN-APM-cascades-exist-unused.md`, which established the
finding. This is the plan. Written by claude-13; every number below was
re-verified 2026-08-26, not carried over.

## Why APM is the best case — with the numbers

| | |
|---|---|
| the expander exists | `expand-memory-cascade` (`apm/conductor.clj:287`), `cascade-receipt-offers` (`:383`), gated by `:memory-cascade-enabled?` (`apm/conductor_open.clj:112`), cap default 100 |
| its readers work live | resolved `live-cascade-readers` against the running JVM and called it: returns `:attachments-fn :why-targets-fn :pattern-fn`; `expand` with an empty seed returns the full result shape (`:routes :pattern-surfaces :seed-patterns :expanded-count :truncated?` …) |
| there is material to traverse | **683** `memory/assert` hyperedges, **682** distinct leaf memories, across **69** distinct `math-*` patterns |
| there are pattern→pattern edges | **45** `pattern/has-semantic-why` relations, of which **23 touch `math-*`** — e.g. `math-formalization-CA/riemann-darboux-api → math-formalization-CA/measure-integration-api`, `…/ode-gronwall-api → math-informal/reduce-to-known-result` |
| it is not on the path | `countdown_control.clj` has no require of `futon3c.apm.conductor`; its only consumer is `conductor_surface.clj` behind HTTP |
| and has never run | cascade-artifact counts are 0 across **all seven** campaigns on disk (`:hops`, `:why-hop`, `:co-incidence`, `pattern-surfaces`, `:seed-patterns`) |

**Correction to the TN, recorded here because it changes the plan.** TN §"Patterns
exist, but as tags on leaves" concludes "There is no pattern→pattern edge of any
kind … A pattern language needs relations among the patterns themselves, and
there are none to traverse." That was measured with
`GET /api/alpha/hyperedges?end=<pattern>`, which returns only `:memory/assert`.
The `has-semantic-why` edges live on the **relations** endpoint, which that query
does not reach, and `conductor.clj:257` reads exactly that type. There are 23 of
them over maths. The pattern language is thin, but it is not empty, and the
cascade has something to walk.

## What "built and used" must mean, precisely

Not "the cascade ran". The demo succeeds when a single frame's receipt shows
**both**:

1. a non-zero `:why-hop` (or `:co-incidence`) route count — the cascade expanded
   beyond the leaves retrieval already surfaced; and
2. a `:memory-use/used-ids` entry whose memory arrived via one of those expanded
   routes — i.e. the student consumed something **the cascade added**.

(1) alone is "built". (1)+(2) is "built and used", and is the claim worth making.

## Six handoffs, smallest first

### H0 — dry-run the expander before wiring anything (30 minutes, no code)

Call `expand-memory-cascade` on a real snapshot's seed ids and report
`:expanded-count`, `:truncated?` and the route histogram. Seeds are in
`data/apm-campaigns/**/snapshots/*-solver-memory.edn` (f37 has 22 `:memory-id`s,
f42 has 48).

This is first because it is free and it can **falsify the whole plan**: if
expansion over a real seed set returns `:expanded-count 0`, there is nothing to
wire and H2/H3 should not be attempted.

*I could not complete this myself: `scripts/proof-eval.sh` rejects the form
(`Syntax error macroexpanding at (1:1)` for any non-empty seed vector, while the
same vector compiles in isolation and an empty seed succeeds), and a bare
`clojure -M` in the repo is missing `futon3c/dev/config` on the classpath. Whoever
takes this has a working REPL and should just run it. Do not skip it.*

### H1 — archive the rendered packet per attempt (TN option 1)

Precondition, not an option. Today `fingerprint_audit.py` cannot separate
*surfaced and ignored* from *never actually shown*, so a zero-uptake frame is
uninterpretable — it indicts either the memories or the delivery and there is no
way to tell which. Every measurement below is meaningless without it.

### H2 — replace the hash sort with a stated ordering (TN option 3)

`snapshot-body` (`apm/memory_snapshot.clj:36`) emits a flat vector sorted by
`:memory-id`. Nobody designed that ordering; it falls out of
`(sort-by :memory-id candidates)`. **In f42 the memory that closed the problem
was 47th of 48.**

This is independent of cascades and worth doing whatever happens to H3. Order by
any stated relevance signal, record which signal, and report
position-of-used-memory per frame. Baseline to beat: 47/48.

### H3 — put `cascade-receipt-offers` on the countdown path

**Ordering note: run H4's search before this.** H4 works entirely on archived
data and decides whether H3 is worth building; wiring first is how the
honest-holes outcome happens.


Behind the existing `:memory-cascade-enabled?` flag, which already exists and is
already read — this is wiring, not new machinery.

**Run it as a NEW campaign arm.** The TN is explicit that this changes the
independent variable mid-campaign and "is not a change to make quietly". The
previous arm's config must be recorded alongside so the comparison is honest.

### H4 — f42a: the counterfactual worked example (Joe, 2026-08-26)

*Joe: "we don't need to find a 'real world' example yet — indeed we could
perhaps rework f42 into an f42a 'synthetic' example showing how the cascade
would have changed things."*

This replaces the archive search. It is better than synthetic: **every input is
real, and only the counterfactual is constructed.**

**Feasibility is already established, by computation over the live substrate
(claude-13, 2026-08-26):**

    f42 seed memories                                     48
    f42 seed patterns                                     23
    has-semantic-why edges touching an f42 pattern        18  (of 45 in the substrate)
    why-hop TARGET patterns reached from f42 patterns      5
    NEW memories the cascade would add at one why-hop     10

        math-strategy/missing-dependency-protocol         +6
        math-formalization/coercion-bridge                +4

So the cascade is not empty on this frame: it expands the delivered shelf from
**48 to 58**, and the 10 additions are reachable only by a why-hop — retrieval
did not surface them.

*Caveat, stated because it matters: that expansion was computed independently
from the raw `memory/assert` and `pattern/has-semantic-why` data, NOT by running
`expand-memory-cascade`. The real expander also has `:co-incidence` routes,
cheapest-route dedup and a cap, so treat 10 as an independent one-hop estimate,
not the expander's output. **H0 still has to run** and should reproduce or
correct this number — that is now H0's acceptance test.*

**What f42a is.** f42 replayed with the cascade on. Real throughout: the 48
memories, the 23 patterns, the 18 edges, the 10 additions, all from the live
substrate. The only constructed element is that the frame did not actually run
this way — so f42a is a **counterfactual, not a fabrication**, and should be
labelled that way in every artifact it produces.

**What f42a may claim.** That the student would have been handed a different
shelf, and exactly which one. **It may not claim** that the student would have
done better; nothing here is an outcome measurement, and f42 closed successfully
anyway.

**The judgement step, and it is the real test.** Do any of the 10 added memories
bear on the obstacle f42 actually hit? Take that judgement from a seat that did
not compute the expansion. Both answers are worth having:

- **Yes** — a worked example exists: here is the obstacle, here is the memory the
  student did not have, here is the route by which the cascade reaches it, here
  is what the delivered shelf would have looked like.
- **No** — the cascade adds **volume without relevance** on this frame, which is
  a real finding and a sharper argument against H3 than "no example was found".
  Ten more memories on a shelf already ordered by hash makes the delivery worse,
  not better, and that would say so.

**Why f42 is now the right frame, having been ruled out above.** The earlier
objection stands for a *real* demonstration — f42's closing memory was already
in the seed set at 47/48, so the cascade would not have added it. For a
counterfactual about delivery, that is irrelevant: the question is what the
cascade adds to the shelf, not whether it rescues the outcome. The two uses of
f42 are different and should not be conflated in the write-up.

**Note that `jit-all-open-nontopology-v1-f42` is `:campaign/status :running`
as of 2026-08-26T12:38.** f42a must be a replay against the archived snapshot,
touching nothing in the live arm.

### The whitepaper check (Joe, 2026-08-26): efficiency here means faster flooding

*Joe: "this relates to the 'memory whitepaper' where the idea was that memories
could be searched efficiently by starting from high-level patterns and going to
leaves … what you've just described sounds like 'efficiency' would lead to
'even faster flooding' unless we're careful."*

Read `docs/retrieval-whitepaper-v2.md` against the f42a numbers. The concern is
right, and §4.6 of the paper already names the reason.

**The store is a forest of stars.** §4.6: "each memory attaches to exactly one
pattern, patterns carry many memories, and the largest component of the
patterns-only projection is a **single hyperedge**." Its own summary table:
schema *well-constructed*, content *well-curated*, graph **"essentially unbuilt
— the edges that would make it a graph were never written."** Multi-attachment
is fully representable (`review-attachment!` takes a non-empty vector of pattern
ids); the star forest is "an artefact of use, not a representational limit".

**Descent from a high-level pattern therefore does not narrow — it dumps.**
f42a, with the incidental route switched off:

    why-hop additions only:  53      shelf 48 -> 101
    and all 53 come from ONE pattern: math-strategy/missing-dependency-protocol

23 seed patterns reach just **4** why-reachable patterns, and effectively one of
them carries the entire expansion. That is the star topology showing through:
the pattern is a star centre, its leaves are 53 memories, and there is no
intermediate structure to prune against. Efficiency-by-descent presupposes a
hierarchy to descend; this store has one level.

**With the incidental route on, it is worse.** Of 141 available expansions,
**88 arrive by co-incidence** — `pattern -> problem -> pattern`, from 70 shared
seed problems reaching 32 patterns. `conductor.clj` is careful that co-incidence
"does not recursively flood", and it does not recurse; it does not need to. One
hop through shared problems out-produces the authored route 88 to 53.

**And the delivery is already mostly waste before any of this.** The paper
measures **62% of surfacing slots consumed by memories used nowhere**, on a
48-entry shelf. Tripling the shelf multiplies the unused fraction; it cannot
improve it.

**A deeper mismatch, from §5.1.** "Every memory examined is a pattern, caution,
route, or stopping rule … **None is a proved lemma you can import.**" The
paper's own natural experiment has a runner finding a blocking lemma **by
repository grep, not by recall**, on a dispatch where recall completed and
contributed two used memories. Its conclusion: measuring that gap "needs a
different instrument: an index over proved artifacts, not a better ranker." More
advice, delivered faster, does not address it.

**Consequence for this plan.** The cascade is a mechanism for exploiting graph
structure, applied to a graph the paper states was never populated. Wiring it
(H3) does not fail loudly — it floods quietly, and mostly with co-incidence
material. So:

- **H3 gains a gate.** If it is built at all, build it **why-hop only**, with
  co-incidence off, and state the cap argument explicitly. On f42 that is still
  53 additions from one star, so the gate is necessary and not sufficient.
- **H5 (new, and prior to H3) — populate the graph before exploiting it.**
  Multi-attach memories to the patterns they actually bear on, and author
  pattern→pattern edges. §4.6 says this is a habit of writing, not a schema
  change. Until it happens, every structural result — including f42a — is a
  finding about population, not about design, and the paper is explicit that
  reporting it otherwise would be uninformative.
- **H2 rises further.** Ordering is the only change here that helps a shelf
  which is already 62% unused, and it helps whether or not the graph is ever
  populated.

## What to measure

Per frame: `:memory-use/surfaced-ids`, `:memory-use/used-ids`,
position-of-used-memory in the delivered order, and the route histogram
(`:leaf` / `:why-hop` / `:co-incidence` / `:truncated?`). The route histogram is
precisely the artifact whose absence the TN measured, so its appearance is the
receipt that this plan worked.

## What this does not claim

That cascades help the student. It claims only that one instance exists where a
cascade is built and consumed, which is currently true nowhere in the stack. The
`E-cascade-sampler-sampler` yardstick discipline applies: generation may use
wholeness as a proxy, judgment may not, and diversity is reported rather than
credited.

## Related

- `technotes/TN-APM-cascades-exist-unused.md` — the finding this plans against
- `futon2/holes/E-cascade-sampler-four-2026-08-26.md` — the cross-stack tally
- `futon3c/holes/excursions/E-operator-turn-modelling-2026-08-25.md` — why the
  ordering question (H2) is where operator-turn mining could first pay

---

*Correction, 2026-08-26 (claude-19), to the table row "and has never run":
D1 (`technotes/D1-round1-cascade-offers-2026-08-26.md`) found the cascade DID
run on the round-1 conductor path in f9/f10/f13/f15 — 1–5 seeds, 115–132
available, cap 100 fired every time, student arm `:memory-channel :none`, so
nothing consumed the offers. The row should read "has run, built-not-used, on
a path no student was on". H0/D0 is unaffected, and the live reader was
un-runnable from 2026-08-23 until `7534419c` (TN addendum 2/3).*

*Status, 2026-08-26 (claude-19):* **H0 done** — real run `77a1bac0`: 103
available (48 why-hop, all via one hub; 55 co-incidence), not 141; deterministic.
**H1 live** from f43 (`7ee42661`, prereg amendment 5). **H4 judged** —
`holes/f42a-H4-judgement-2026-08-26.md`: none of the additions bears on f42's
crux; volume without relevance. Next: H2 (needs the ordering signal), then H5
as specified in the judgement, H3 last.

*Status, 2026-08-26 evening (claude-19):* **H2 shipped** (`844c6ae4`, `ad45cd1f`,
live from f43, prereg amendment 6): hash order replaced by
promoted-this-frame → identifier overlap → id; offline median position of the
used memory 18.5 → 3.0. Remaining: H5 as specified in the H4 judgement, then H3 last.
