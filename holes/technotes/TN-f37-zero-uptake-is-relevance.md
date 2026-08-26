# f37 zero uptake is a RELEVANCE failure, not an uptake failure

2026-08-26, claude-12, watching f37 live.

## The number

f37/a96A08, student attempt 1, completed (`:live-job-certified`, 12.7 KB,
12m34s): **0 of 22 accessible memories used.** This is the repeat of f35's
0-of-27 that Joe flagged as needing to be sorted on the spot.

Delivery was not the problem — 22 memories were on the shelf and readable. The
question is whether the student ignored good material (uptake) or was handed
the wrong material (relevance). The shelf settles it.

## What was on the shelf

Of the 22, by name:

- **16 are complex analysis / value distribution** — winding-interval-to-circle
  integral bridges, `logcounting-not-raw-jensen`, divisor-sum-to-root-count
  descents, `enat-nat-multiplicity-cast-bridge`, Blaschke canonical product
  constancy, argument-principle `im-ne-zero` arithmetic, entire injective
  affine routes, boundary critical point exclusion and winding count. This is
  the a92J01–a95J04 cluster.
- **5 are series / summability / measure bridges** — dyadic shell summability,
  `tsum-to-finsum-via-finite-support`, `integrableOn_Ioi_rpow` power tails.
  Tangential at best.
- **1 is on topic** — `affine-reparametrization-to-canonical-sinc-limit`.

a96A08 is an improper sine-integral problem. The student was handed a shelf
overwhelmingly about counting zeros of entire functions, and declined it.

**Declining that shelf is the correct response to that shelf.** Reading 0-of-22
as a memory-system failure would be reading the wrong instrument. So would
"fixing" the Student card to raise uptake — that would only teach it to cite
irrelevant memories, which is worse than ignoring them, because a cited
irrelevant memory pollutes the transfer statistic we are trying to measure.

## Two structural causes, worth separating

1. **The shelf is not selected for the problem.** Seeding offers everything at
   `:attachment-status :reviewed`, unranked and unfiltered. 22 unranked
   memories, 16 from another subfield, is a dump rather than a retrieval. f32
   attempt 1 already showed this — "18 accessible, all off-topic" — so this is
   the second recorded instance, not a one-off.

2. **Consecutive problems are not topically adjacent.** a96A07 is a Jordan
   curve / winding argument; a96A08 is a sine integral. Adjacent problem ids
   are not adjacent mathematics. Tier-A condition 3 asks for cross-problem
   transfer *through the shelf*; that cannot happen when the problem following
   a deposit is in a different subfield, however good the deposit was.

## The sharpest illustration

The single on-topic memory is f37's OWN re-scribed candidate — deposited within
f37, promoted within f37, handed back to f37's own student. Same-problem, not
cross-problem, and therefore not evidence for condition 3 at all.

Meanwhile the four ORIGINAL f37 candidates — `partial-fraction-to-removable-
sinc-representation`, `integrability-by-isBigO-tails`, `aecover-symmetric-
tendsto-evaluation`, `affine-reparametrization-to-canonical-sinc-limit` — were
squarely about this subfield and were destroyed by the persistence defect
earlier the same night (see TN-spec-delta section 9). Even had they survived,
they would have been the right material for exactly one kind of successor
problem, and nothing in the queue guarantees such a problem comes next.

## Open question — Joe's, not an apparatus defect

Making the shelf relevance-ranked, or ordering the queue so topically related
problems are adjacent, **changes what the experiment measures**, so it is not
something to fix unilaterally mid-campaign.

- (a) rank or filter the shelf against the current problem statement;
- (b) order the queue by subfield so a deposit can meet a related successor;
- (c) both.

(a) is the more honest fix: real retrieval is query-driven, and a fixed unranked
shelf tests something no memory system would actually do. See
`TN-retrieval-probe-definition.md`, which is about this question.

Until one of these lands, **tier-A condition 3 is not merely unmet, it is close
to unreachable** — and a null result on memory transfer would be measuring the
seeding policy rather than memory.
