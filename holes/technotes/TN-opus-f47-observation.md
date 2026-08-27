# TN-opus-f47-observation — a memory that was never on a shelf, used anyway

claude-clink-1 (APM frame-watcher), 2026-08-27, with the account of the mechanism
supplied by claude-19. Written at Joe's request as a record, not a proposal: it is
not clear anything needs doing.

Frame f47, problem **a97A01**, campaign `jit-all-open-v2`, student attempt 1.

## What happened

a1 ran under amendment 8's same-problem holdout and used exactly one memory:

    :shelf/holdout          :same-problem
    :shelf/withheld-count   3
    :accessible-memory-ids  67
    used-ids                ["e-23a2940f-5fa6-444e-948c-74e6e201eb31"]
    outcome                 partial

That memory was deposited by `f39-guide`. **f39's problem was also a97A01** — verified
from each frame's own manifest, not inferred — so f47 is a retry of a problem that already
had a frame in the previous campaign, and the memory is same-problem.

## The mechanism (claude-19's account, from the artifacts)

It reached the student through two legitimate channels, both on the receipt:

- **Cascade.** `:receipt/memory-cascade` → `:used-via-cascade` records it with
  `:route :sibling` on pattern `math-formalization-CA/measure-integration-api`, a pattern
  the shelf's seeds sit on. `:expanded-available 168`, `:truncated? true`,
  `:holdout-excluded 0`.
- **Search.** `:receipt/memory-use :surfaced-ids` holds 82 = the 67 accessible + 15 search
  hits. The id is among the 15 and not among the 67. One query is recorded, and its receipt
  on disk lists the id in its results.

Validation was right not to fire. `allowed = shelf ∪ search ∪ cascade − withheld`, and
`withheld` was the three own-frame deposits; `:holdout-excluded 0` is truthful because none
of those three was among the cascade's candidates.

## Why it was never on a shelf — the part worth remembering

f39 was disposed `:partial` with `apparatus-repair-required? true`, so it is not a
**completed** frame, so `campaign-prior-memories` never read its last snapshot. f47's shelf
declares lineage `["jit-all-open-nontopology-v1" "jit-all-open-v2"]` and carries 70
memories with depositors spanning f28–f47; this id is in none of them, and f47's
`:prior-dropped` never mentions it.

The memory is **reviewed, attached to a live pattern, and has never been on any shelf**. It
exists only in the store.

Amendment 8 computes `:shelf/withheld-ids` from the snapshot, so the holdout can only
withhold what was shelved. Cascade and search both reach past the shelf into the store. On
a retry-shaped frame — where the frame's problem equals an earlier frame's problem — a
same-problem memory deposited by an *incomplete* prior frame is therefore reachable and
structurally unwithholdable.

That is a category amendment 8 did not consider. If the holdout is ever to be airtight, its
predicate has to key on the memory's own subject rather than on shelf membership.

## What this is and is not

**Is:** cross-frame, cross-campaign retrieval that worked. A memory deposited in f39 of
campaign v1, which the shelf machinery had discarded as belonging to an incomplete frame,
was found and used by a student in f47 of campaign v2 and contributed to a partial. Joe's
reading, and the right one: the pipeline did the thing it exists to do, and the
same-problem coincidence is circumstantial to that.

**Is not:** tier-A condition 3 evidence. That condition requires a memory mined in one
frame to be fingerprint-used in a later frame **on a different problem**, through the
shelf. This use fails on two counts — same problem, and not through the shelf. Condition 3
stays at n=1 for the programme.

## Corrections to my first report of this incident

Recorded because the wrong version was circulated before the right one:

- I reported that no channel on the receipt accounted for the id. It did — I read
  `:memory-cascade` and `:accessible-memory-ids` and concluded from an empty cascade and 67
  ids. The used list is at `:receipt/memory-cascade :used-via-cascade`, and the search hits
  at `:receipt/memory-use :surfaced-ids`. Both were populated.
- I suggested a surfacing check had failed to fire. It was correct not to.
- I said v2 declares no `:campaign/priors`. It declares both campaigns.
- I framed it as claude-19's provenance defect (`:provenance` being the carrying frame
  rather than the depositor). It is not that: the id was never in a snapshot at all, so no
  provenance was ever stamped on it.

## Second observation: the shelf's utility collapses as it grows

Measured across the five student attempts run under campaign v2, counting each used id
against that attempt's own `:accessible-memory-ids`:

| attempt | shelf | used | from shelf | from elsewhere |
|---|---|---|---|---|
| f46 a1 |  0 | 0 | 0 | 0 |
| f46 a2 |  5 | 4 | 4 | 0 |
| f46 a3 |  7 | 5 | 5 | 0 |
| f47 a1 | 67 | 1 | **0** | 1 |
| f47 a2 | 72 | 4 | **3** | 1 |

A shelf of five or seven — entirely the frame's own deposits — is used at 80% and 71%. A
shelf of seventy is used at effectively zero.

The collapse is sharper than the table shows. f47/a2's three shelf hits were `f47-scribe`
and `f47-guide` on a97A01: the frame's **own** deposits again. Of the roughly 69
prior-frame memories on that shelf, **none were used**. What got used was what this frame
had just written, plus one thing search found.

So on this evidence the shelf is not operating as a retrieval mechanism. It is operating
as a scratchpad for work in progress, with a large inert tail attached.

**Caveat, and it matters.** f47/a1's `0 of 67` is confounded: amendment 8's holdout
withheld exactly the three own-frame deposits, which is the class f46 shows to be heavily
used. So a1 is not evidence that priors are useless — it is evidence that removing
own-deposits removes almost all of the shelf's realised value. a2 is the unconfounded case
and points the same way.

**Why this bears on condition 3.** Joe's framing, and the right one: this is better read as
*the shelf failing to be useful* than as *students routing around the shelf*. The student
searched because the shelf did not supply what it needed, and search delivered — twice in
f47, including one genuinely cross-problem memory (`ams-codex-1` on a97J02, used on
a97A01, contributing to a partial). Condition 3 requires transfer **through the shelf**, so
it reads zero for both. A criterion that measures one channel while the useful traffic
flows through another will keep reporting n=1 while transfer demonstrably occurs.

Nothing here says the memories are bad or that transfer does not happen. It says the shelf,
at its current size and composition, is not where transfer is happening.

## If we come back to this

The open question is whether the holdout should be airtight. Making it so means keying the
predicate on the memory's subject rather than shelf membership, which also brings the
cascade and search channels under it. Nothing here is broken — the machine behaved
correctly at every step and the receipts are honest. What it costs, left alone, is that
attempt-1 on a retry-shaped frame is not a clean cross-problem test, and any tally that
treats it as one will be wrong.
