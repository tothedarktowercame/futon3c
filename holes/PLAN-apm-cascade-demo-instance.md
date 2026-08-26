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

## Five handoffs, smallest first

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

### H4 — the worked example: show the cascade being USED on a named problem

*Joe, 2026-08-26: "otherwise we get the 'wired but no example' failure case we
saw with 'honest holes' in the Cascade Live sheet."*

That failure is on record and worth stating precisely, because H3 alone
reproduces it. O5 honest-holes in `C-cascade-real` is fully wired: a generator
(`scripts/o5_honest_holes.py`), a dry-run artifact, arrows certified
`:consistent? true` and explicitly described as "real, not aspirational". Its
output is **2 holes across 231 core missions**, and no case was ever shown where
a hole changed anyone's decision. The machinery is not in doubt; the
demonstration never happened. H3 delivers exactly that state for cascades —
route labels in a receipt, and nothing anyone can point at.

**H4 is therefore not optional and not last.** Its search should run against the
existing campaign archive BEFORE H3 is wired, because it can be done entirely on
recorded data and it decides whether H3 is worth doing.

**The selection criterion, fixed in advance.** Pick the demonstration frame by
this rule, stated before any outcome is examined, so the case cannot be chosen
to flatter the cascade:

> a **closed** frame in which the student stumbled, where some leaf memory M
> satisfies: (a) M was NOT in that frame's `:memory-use/surfaced-ids`, (b) M is
> attached to a pattern P' reachable by `pattern/has-semantic-why` from a pattern
> P that WAS attached to a surfaced memory, and (c) M's body addresses the
> specific obstacle the student hit.

(a) and (b) are mechanical over the archive and the 23 math `has-semantic-why`
edges. (c) is a judgement and must be made by a seat that is not the one that
selected the frame.

**Not f42.** Its closing memory was already in the seed set, ranked 47th of 48.
The cascade would not have added it — ordering (H2) is what addresses f42, and
using it to demonstrate H3 would be the flattering-case error in its most
tempting form. Note also that `jit-all-open-nontopology-v1-f42` is
`:campaign/status :running` as of 2026-08-26T12:38, so it is not archive
material at all.

**What the deliverable is.** One page, on one named problem, containing:

1. the problem and the obstacle the student actually hit, quoted from the trace;
2. the shelf as delivered — the flat hash-sorted list, with positions;
3. memory M, which was not on it;
4. the route: seed memory → pattern P → `has-semantic-why` → P' → M, with the
   edge ids, so a reader can re-walk it;
5. the counterfactual, stated honestly as a counterfactual: what the student
   would have been handed with the cascade on, and why M addresses the obstacle;
6. what it would have cost — `:expanded-count` at the frame's seed set, against
   the cap.

**A null result is a valid and valuable deliverable.** If no frame across the
seven archived campaigns satisfies the criterion, then the cascade's usefulness
is unevidenced on the whole recorded history, and H3 should not be built. Say
that plainly rather than relaxing the criterion until a case appears — relaxing
it is how "wired but no example" becomes "wired with a bad example", which is
worse.

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
