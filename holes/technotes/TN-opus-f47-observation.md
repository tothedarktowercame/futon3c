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

---

## Addendum, 2026-08-27: the search channel was serving withheld memories

The section above says "the machine behaved correctly at every step." That is wrong, and
the way it is wrong changes what the second observation means.

While measuring f48/a1 the same way, its live record turned out to carry
`:terminal-repair-attempts 1` and

    :terminal-repair/findings [:student-memory-used-without-surfacing
                               :student-memory-used-despite-holdout]

f46/a1 carries exactly the same pair. Both attempts used a memory their own holdout had
withheld; both were discarded and re-dispatched.

### The defect

The dispatch request carries `:shelf/holdout` and `:shelf/withheld-ids`, but
`typed-role-submission/register!` filters that request through `authority-fields`, which
listed neither key. The shelf removed the withheld ids and the cascade removed them;
`role-memory-search/search!` never learned they existed and returned them on request.

`allowed = shelf ∪ search ∪ cascade − withheld` is evaluated when the terminal is
validated, which is after the student has read the memory. Detection was working. Nothing
prevented the read.

Of the three frames that have run with a holdout, search served withheld ids in two:

| frame | shelf | withheld ids served by search | used | result |
|---|---|---|---|---|
| f46 a1 | **empty** | 2 of 2 | both, "materially used" | discarded, re-run |
| f47 a1 | 67 | 0 | — | stands |
| f48 a1 | 76 | 1 of 4 | 1 | discarded, re-run |

Both students disclosed the breach themselves. f48's account: the id "was also in the
withheld holdout list; it reached me through the authorized, receipt-recorded search
channel and I reported that transparently in the failure account." f46's recorded that its
shelf snapshot was empty, so everything it had came through search.

Fixed in `d3cf69df`: the holdout now travels on the job authority, and `search!` drops
withheld ids before returning, recording `:shelf/holdout`, `:holdout/withheld-count` and
`:holdout/excluded-ids` on the receipt so enforcement is auditable rather than a silent
drop. A job with no holdout filters nothing, so attempts 2 and 3 are unaffected.

### Terminal repair overwrites the evidence

`live/student-attempt-1.edn` is a single slot. The repair re-dispatch persists over it, so
the discarded attempt's `used-ids`, its response, and its disclosure are gone. f46/a1's
surviving record reads `used-ids []` — indistinguishable from an attempt that used
nothing. The two keywords in `:terminal-repair/findings` are the only trace left on disk.

Both discarded attempts were recovered from the Agency job ledger while they were still
resident (`:terminal-repair/original-job-id`, both `state: done`) and written to each
frame's `superseded/` directory. `data/*` is gitignored, so those two files are the only
copies once the ledger entries age out.

Real work was thrown away with them. f48/a1 had compiled `apm_a98a03_vol_image_mul`
(`volume ((r * ·) '' s) = ofReal |r| * volume s`), which exists under no name in this
Mathlib; f46/a1 had elaborated four lemmas including the pole-cancelling pairing identity.

### What this does to the second observation

**The f46 a1 row of the shelf-utility table is measuring an erased attempt.** Its `shelf 0,
used 0` describes the clean re-run. The original a1 used two same-problem memories that
search supplied and the holdout had withheld.

**The reading needs correcting.** The note concluded, following Joe, that this is better
read as *the shelf failing to be useful* than as *students routing around the shelf*. On
f47 that still holds — f47 had no breach, and its student searched because a 67-entry
shelf did not supply what it needed. On f46 it does not: the shelf was empty and the
channel that supplied the student was handing over memories the instrument had explicitly
removed. That is not a shelf failing to be useful. That is the holdout leaking, and the
from-shelf-zero pattern is partly an artefact of it.

Which frames are affected is bounded by what survives: three attempt records on disk carry
a holdout at all, because the live slot is reused. Earlier frames cannot be checked.

The open question at the end of the note — whether the holdout should be airtight — is
now answered for the search channel, which is enforced. The cascade already excluded
withheld ids. What remains open is the category f47 identified: a memory that was never on
any shelf cannot be withheld by a predicate that keys on shelf membership, and that is
still true.

## f49 (2026-08-28): the first uncontaminated holdout observation

f49/a1 on a98A04 is the first attempt to run with the depositor-truth gate live
(codex-12, `aec461a4`/`a79ac3c3`, M2), and the first whose numbers can be taken at
face value:

| frame | withheld | served & used | record repaired? |
|---|---|---|---|
| f46 | 2 | 2 of 2, student disclosed | yes — `superseded/` |
| f48 | 4 | 1 of 4, student disclosed | yes — `superseded/` |
| f49 | 3 | **0** | **no `superseded/` directory** |

The last column is the part that matters for reading the earlier rows. Terminal repair
rewrites `live/student-attempt-N.edn` in place, so f46's and f48's surviving records both
show a clean re-run with `:used-ids []`. Their real breaches are visible only in
`superseded/student-attempt-1.repair-1.original-job.json`. A zero in a repaired record is
not a measurement; it is the absence of one. I read f48's `0/76` as an honest empty on the
day and was wrong.

f49 has no `superseded/` directory, `:used-ids []` is the attempt's own first and only
report, and `:shelf/withheld-count 3` is intact. Independently, none of the three withheld
ids appears in the job's prompt, result, or events — they occur only in the dispatch
authority, which is where they are declared.

**So the shelf-utility observation survives, and now rests on something.** 76 accessible
memories, zero used. The student worked from Mathlib primitives, hit the 30-minute stop
mid-bridge-1 of a98A04, and reported the FTC computational kernel it had compiled but not
merged. Nothing was withheld from it that it reached for; nothing on the shelf was reached
for at all.

That is the same shape as f47 — a large shelf yielding nothing — but for the first time it
is not confounded by a leak, and not erased by a repair. One clean point is not a trend.
It is, though, the first point in this table that means what it says.

**Still open**, unchanged from the f47 section above: a memory that was never on any shelf
cannot be withheld by a predicate keyed on shelf membership. M2 keys on depositor truth at
a common pre-serve gate, which closes the leak for memories that HAVE a depositor
recorded; the never-shelved, provenance-absent category is what remains.

## The fingerprint audit does not discriminate (2026-08-28)

f50/a1 on a98A07 looked, at first reading, like the first observation of condition 3.
The student used one memory of 76; the record has no `superseded/` directory; the holdout
withheld 2 and none leaked; the memory was deposited by **f34-guide on a95J03** — a
different problem, sixteen frames and three days earlier — and it was **on the shelf**
(`:accessible-memory-ids` contains it), so it arrived through the shelf rather than
through search or cascade. Every structural condition for cross-problem transfer holds.

`fingerprint_audit.py` returns `"verdict": "fingerprinted"` for it. That verdict does not
survive reading the row:

    tokens-named 30, tokens-hit 4, tokens-novel 1
    novel-hits ["norm_num"]
    in-base-already 3
    paste-longest-run 0, paste-lines-hit 0

Of the 30 tokens the memory names, 4 appear in the student's artifact, and 3 were already
in the base file. The entire witness is `norm_num`. There is no pasted text at all. A
student reaching for `norm_num` is not a student using a memory from f34.

**Across the campaign, 20 rows: 8 `already-in-base`, 4 `unwitnessed`, 8 `fingerprinted`.**
Four of the eight fingerprinted rows rest on a single token with zero paste, and three of
those tokens are ubiquitous:

| row | sole witness | discriminating |
|---|---|---|
| f46/a3 a96J08 | `Mathlib.Analysis.SpecialFunctions.Trigonometric.Cotangent` | yes — a specific import |
| f47/a3 a97A01 | `exact_mod_cast` | no |
| f47/a3 a97A01 | `exact_mod_cast` | no |
| f50/a1 a98A07 | `norm_num` | no |

`exact_mod_cast` alone accounts for four novel-hits across the corpus. The other four
fingerprinted rows carry genuinely specific names — `intervalIntegral.integral_comp_neg`,
`hasSum_geometric_of_norm_lt_one`, `Real.exp_lt_one_iff`, `Complex.norm_exp` — and those
are real witnesses.

So the instrument is not broken; it fails to weight tokens by how much seeing one tells
you. A fully-qualified Mathlib lemma or import appearing in a proof that did not have it
before is evidence. A bare tactic name is not: it is in thousands of proofs, and its
presence is explained without reference to any memory.

**On a specificity rule the corpus has 5 fingerprinted uses, not 8.** These counts feed the
preregistered capability-transfer analysis, so the difference is not cosmetic — it is the
difference between a reported transfer rate and an artefact of counting `norm_num`.

Two consequences for the reading of this note as a whole. First, the earlier sections'
"the shelf is not where transfer happens" survives, and is if anything strengthened: the
uses that looked like shelf transfer include some that are not uses at all. Second, f50/a1
should NOT be recorded as the first instance of condition 3. It remains the first attempt
where every structural precondition held — clean holdout, unrepaired record, cross-problem
memory delivered through the shelf — and that is worth keeping. What is missing is the
artifact-level evidence that the memory changed the proof.

## f50/a2: the shelf supplied the frame itself; the useful memory came around it

a2 carries no holdout — that is a1-only — so it saw everything, 81 accessible after this
frame's own five deposits landed. It used two, and closed bridge 1
(`outcome=partial-bridge-1-closed`), so this is an attempt that made real progress.

    e-apm-promotion-5ee5f9ebc3dacc34ade531388d5f6dce
      author f50-scribe, subject a98A07        ON SHELF
    e-codexpilot-upgrade-diskwise-L1-convergence-to-local-uniform-convergence
      author codex-5,    subject M-codex-sorry-loop    NOT ON SHELF

The shelf's entire contribution to a successful attempt was the frame's own scribe deposit
on the frame's own problem, made minutes earlier. That is self-supply, not transfer.

The memory that carried knowledge from elsewhere — an L1-to-local-uniform convergence
upgrade, deposited by codex-5 against `M-codex-sorry-loop`, a different lab and not part
of the APM problem corpus at all — was **not on the shelf**. It arrived through search or
cascade.

This is the third frame with the shape the note opened with, and the first where the
holdout was sound and the record unrepaired, so it is not an artefact of either defect.
It sharpens the second observation: it is not only that a large shelf yields little. When
something useful does arrive, it arrives **around** the shelf, and what the shelf itself
supplies is the depositing frame's own recent work.

Note also what this does to the arithmetic. a2's "2 of 81 used" is not two units of
transfer: one is self-supply and one bypassed the mechanism under measurement. Counting
`:used-ids` without asking where each memory came from and who deposited it will report
transfer that did not occur — the same failure mode as counting `norm_num` as a
fingerprint, arriving by a different route.

## f51/a1: an empty shelf recorded as a null result

f51 (a98J03) skipped `promote-solver` entirely — the phase is simply absent from
`jit-all-open-v2-f51/live/`, where f50 had it at `:promotion-certified` with 8 approved
supply. The consequence runs through `live_learning_phases.clj:251`:

    (and (= :student-attempt kind) promotion-receipt)
    (assoc :memory-snapshot {... :accessible-memory-ids accessible-ids})

No promotion phase means no receipt, means no snapshot, means no accessible memories. The
attempt ran anyway. Its record:

    :outcome                "not-closed"
    :used-ids               []
    :shelf/withheld-count   0
    :accessible-memory-ids  ABSENT
    :memory-snapshot        ABSENT
    superseded/             does not exist

The monitor line for it is `a1: memory 0/0 used outcome=not-closed`.

**The shelf was not empty.** `campaign-prior-memories` for this campaign returns **504
candidates**, 1 dropped, lineage `["jit-all-open-nontopology-v1" "jit-all-open-v2"]`. Five
hundred and four memories existed and none were offered.

This is the most dangerous shape a defect in this campaign can take, and it is worth
separating from the others in this note. Every other failure recorded here STOPS
something: a leak voids a frame, a halt stops the machine, a weak fingerprint is at least
visible in the audit. This one produces a well-formed record that passes every check and
means nothing. "0 of 0 used" is not distinguishable, in the data, from a student that had
504 memories available and reached for none. Counted naively it is a null observation of
transfer; it is actually an observation of nothing at all.

The record is clean by every structural test this note has used to validate the others: no
`superseded/` directory, an unrepaired first report, an honest outcome. Those tests were
built to catch contamination. They do not catch absence.

**f51's learning arm should be excluded from the transfer analysis.** Its solve is
unaffected and banks normally.

## The four ways this campaign can report transfer that did not happen

Collected, because they are independent and all inflate rather than deflate:

1. **Contaminated holdout** (f46, f48) — the withheld memory was served anyway, and
   terminal repair rewrote the record so the breach survives only in `superseded/`.
2. **Ubiquitous-token fingerprints** — `norm_num`, `exact_mod_cast` counted as witnesses.
   Fixed by weighting on rarity; 5 fingerprinted, not 8.
3. **Self-supply counted as transfer** (f50/a2) — the shelf's contribution was the frame's
   own scribe deposit on its own problem, minutes old.
4. **Empty shelf counted as a null result** (f51/a1) — no snapshot offered, "0 of 0 used".

Three of the four are invisible in a naive read of `:used-ids` and
`:accessible-memory-ids`. The instrument for each is now different: check `superseded/` for
(1), read `novel-hits` for (2), resolve depositor and subject for (3), and require a
snapshot to exist at all for (4).

## f52/a1: the first solve, with zero uptake

f52 (a99J01) is the first frame registered with the phase order repaired after A10, so its
student arm had a real shelf: **84 accessible, 1 withheld** under `:shelf/holdout
:same-problem`. There is no `superseded/` directory; the record is the attempt's first and
only report; no withheld id appears among the used.

    :outcome                "solved"
    accessible 84   withheld 1   used 0   leak none

**The student solved the problem on attempt 1 using none of the 84 memories.**

The claim needed checking, because `outcome` and artifact have disagreed twice today. The
student BRANCH head is `9fa428f7`, the base revision, carrying 3 sorries and `sorryAx`. The
proof is in the worktree, uncommitted:

    /home/joe/code/apm-frames/f52-a99J01-student
      problems/a99J01/lean/Main.lean   0 sorries, 264 lines, " M" (modified, uncommitted)

Elaborated directly: `apm_a99j01` depends on axioms
`[propext, Classical.choice, Quot.sound]`. No `sorryAx`. The solve is real.

Two things follow.

**For the shelf question.** Every clean observation so far has shown the shelf contributing
little, but they were all failed or partial attempts, where "found nothing useful" is weakly
evidenced — perhaps nothing on the shelf could have helped. f52/a1 is a SUCCESS. The
strongest student result the campaign has produced used the shelf least. That is a harder
fact for the shelf than any of the null results before it.

**For the apparatus — and a correction.** I first recorded here that the machine had NOT
archived the student source, and preserved the proof out-of-band as blob
`b42979bbbf150035e39d990f8c0d6675ca1c128d`. That was wrong. `student-attempt-1-source`
exists for f52 and contains
`b42979bbbf150035e39d990f8c0d6675ca1c128d-Main.lean` — byte-identical to what I
"rescued", named by the same content hash. The machine had archived it before I looked.
f54 did the same for a99J05 (`4190ca5c…-Main.lean`).

My check printed nothing and I read absence into it, which is precisely the error this note
spends its length documenting elsewhere. The rescue blobs are harmless duplicates of the
machine's own archives.

So the standing work-preservation gaps are two, not three: f48 and f49 voided with
`:solve/pin-status :skipped`, and f51's solver reported `:sorry-warnings 0` for rounds whose
heads still carried sorries. A solving student attempt IS archived, content-addressed, at
the time it completes.

What remains true is narrower and still worth knowing: the student BRANCH head stays at the
base revision with its sorries, and the proof lives in the worktree and the content-addressed
archive. Reading the branch alone says "not solved"; reading `:outcome` alone says "solved".
Only the archive and the worktree carry the proof.

## f52 complete: three solves, and the one real transfer is within-frame guidance

All three attempts on a99J01 solved. None used any memory that predates the frame.

| attempt | accessible | used | provenance | fingerprint |
|---|---|---|---|---|
| a1 | 84 (1 withheld) | 0 | — | — |
| a2 | 87 | 2 | `f52-scribe`/a99J01, `f52-guide`/a99J01 | both **fingerprinted** |
| a3 | 89 | 2 | `f52-scribe`/a99J01, `f52-guide`/a99J01 | `no-source` (not yet archived) |

No `superseded/` on any of them; a1's holdout withheld 1 and leaked nothing.

**The shelf finding is now as strong as this campaign can make it.** Three independent
solves, fresh sessions reset to base, a shelf of 84-89 memories, and not one memory from
before this frame was used by any of them. The earlier clean observations were failures and
partials, where "nothing on the shelf helped" is weakly evidenced. These are successes.

**But two of the uses are real, and that matters.** a2's are not `norm_num`-grade artefacts:

    f52-scribe  novel-hits ["ae_lt_top"]                                    fingerprinted
    f52-guide   novel-hits ["EReal.coe_ennreal_toReal" "ENNReal.add_lt_top.mpr"
                            "EReal.coe_le_coe_iff" "lintegral_add_left"
                            "lintegral_congr_ae" "Real.norm_eq_abs"
                            "IsFiniteMeasure" "EReal.coe_sub" "abs_of_nonneg"
                            "abs_of_nonpos" "le_total"]                     fingerprinted

Eleven specific Mathlib lemmas from the guide's deposit appearing in a proof that did not
have them. That is a witnessed, unambiguous memory use — the clearest in the corpus.

So the mechanism that demonstrably works is **within-frame guidance**: the guide observes a
failing attempt, deposits specific material, and the next attempt uses it and solves. The
mechanism that does not show up is **the shelf as a library of prior campaign memory**.

This reframes the null results rather than merely adding to them. It is not that memory
does not transfer; it is that the transfer we can witness happens over minutes and within a
frame, between roles working the same problem, and not over days and across problems
through an accumulating shelf. The shelf grows — 76, 81, 83, 84, 87, 89 — and the frames
keep reaching past it to what they just made.

Campaign tally after f52: 28 rows — 8 `fingerprinted`, 3 `weak-fingerprint`,
10 `already-in-base`, 5 `unwitnessed`, 2 `no-source`. Of the 8 fingerprinted, the two above
are the only ones deposited by a role in the SAME frame as the attempt that used them, and
they carry by far the most novel tokens.

## f53 (a99J03): the pattern repeats, and the shelf is mostly restatement

| attempt | accessible | used | provenance | outcome |
|---|---|---|---|---|
| a1 | 94 (2 withheld) | 0 | — | partial |
| a2 | 99 | 1 | `f53-scribe`/a99J03 | (re-dispatched after wall-clock death) |
| a3 | 102 | 3 | `f53-guide`/a99J03, `f53-scribe`/a99J03 x2 | **closed** |

No `superseded/` on any attempt; a1's holdout withheld 2 and leaked nothing. a2's use is
`fingerprinted` on `contDiffOn_succ_iff_deriv_of_isOpen` — a specific Mathlib lemma, not a
ubiquitous tactic. a3's three are `no-source` pending archival.

**Every use is self-supply.** Not one of the 94-102 shelved memories predating the frame
was touched by any attempt. This is the second consecutive frame in which the attempt that
CLOSED the problem did so on its own frame's guide and scribe deposits — f52/a2 with eleven
novel Mathlib tokens from its guide, f53/a3 with a guide deposit and two scribe deposits.

Running tally since the depositor-truth holdout was repaired — ten clean attempt
observations across f49, f50, f52, f53:

**No attempt left a fingerprint from a shelved memory that predates its own frame.**
The shelf grew 76 -> 102 across them. Two f50 attempts did select prior-frame shelf
memories, so the earlier wording ("no attempt has used") was false:

| attempt | memory | provenance | route | fingerprint verdict |
|---|---|---|---|---|
| f50/a1 | `e-63b7c7c1-1906-412c-ae18-b4644762fbea` | f34-guide / a95J03 | shelf | weak (`norm_num` only) |
| f50/a3 | `e-apm-promotion-0af2ca3ab1d3461da7f90822a1c7c028` | f44-scribe / a98A02 | shelf | unwitnessed |

The f44-scribe memory had already appeared in f47/a3, also cross-problem through the
shelf and unwitnessed. These two appearances are one selected memory, not two witnessed
transfers:

| attempt | problem | memory provenance | route | fingerprint verdict |
|---|---|---|---|---|
| f47/a3 | a97A01 | f44-scribe / a98A02 | shelf | unwitnessed |
| f50/a3 | a98A07 | f44-scribe / a98A02 | shelf | unwitnessed |

The corrected claim is therefore about artifact evidence, not selection: across these
ten observations, prior-frame shelf memories were selected, but none left a qualifying
fingerprint.

### Instrument boundary at f53

The guide and analyst seats moved to zai `glm-5.3` at f53 (`12cf8b48`). The
within-frame fingerprint result up through f52 rests on six Claude-guide frames; f53 is
the first glm-guide instance and did preserve the result (a3 closed using a guide deposit
and two scribe deposits). This is one observation, not evidence that the guide-model
change is immaterial. f54 and f55 do not extend the stratum because apparatus gates
prevented their guides from depositing. Report the pre-f53 and f53+ strata separately
until the latter has more than one usable point.

### A second line of evidence: the shelf accumulates restatement

f53's supply line finished at `5 approved this frame (+11 reassigned to existing
patterns)`. Eleven of sixteen deposits matched patterns already in the library; five minted
new ones. The ratio held as the frame ran — `+3 reassigned` early, `+6` at a2, `+11` at
close — so this is the frame's steady behaviour, not an artefact of when it was read.

That is independent of the uptake counts and bears on WHY the shelf goes unused. If most
new material restates what is already held, then growth from 76 to 102 measures deposit
volume, not coverage. A student searching such a shelf meets many phrasings of the same
few things — which is consistent with what the students actually do: search, find the
restatements unhelpful, and work from Mathlib primitives or from what their own frame's
guide just told them.

The campaign is not failing to produce memory. It is producing a great deal of it, mostly
about what it already knew, and the transfer that demonstrably works happens over minutes
inside a frame rather than across days through the accumulation.

## f58/a1: every structural condition met, and still nothing transferred

This is the strongest test the campaign has run, and it is worth recording in full because
it came back negative.

f58 (`aunk04`), student-attempt-1, **solved the problem**, using one memory of 110:

    e-63b7c7c1-1906-412c-ae18-b4644762fbea
      author   f34-guide
      subject  a95J03           — a DIFFERENT problem
      at       2026-08-25       — four days and 24 frames earlier
      on-shelf true             — delivered THROUGH the shelf, not by search or cascade

2 withheld, no leak, no `superseded/`, unrepaired first report. Cross-problem,
cross-frame, shelf-delivered, on a successful attempt. Every precondition for condition 3
holds simultaneously, which has not happened before.

The fingerprint audit, once the archive landed:

    verdict already-in-base
    tokens-named 30   tokens-hit 5   tokens-novel 0
    in-base-already 5   paste-longest-run 0

All five of the memory's tokens that appear in the proof were **already in the base file**
before the student began. Zero novel tokens. No pasted text.

`already-in-base` is a harder negative than `unwitnessed`. It does not say "we could not
find evidence"; it says the memory's entire detectable contribution was present in the file
the student was already editing. Nothing crossed.

Note this is the SAME memory f50/a1 claimed, where the verdict was `weak-fingerprint` on
`norm_num` alone. Two different students, on two different problems, four days apart, have
now both reported using this one f34-guide memory, and neither use left a trace. That is
worth its own attention: a memory that reads as relevant, gets selected, gets reported as
used, and changes nothing is a specific failure of the shelf rather than a null result.

**Sixteen clean observations. Still zero witnessed uses of a shelved memory predating its
own frame.** The strongest candidate the campaign has produced is a solve whose one memory
contributed nothing the base file did not already contain.

Campaign tally at this point: 33 rows — 14 `fingerprinted`, 3 `weak-fingerprint`,
10 `already-in-base`, 5 `unwitnessed`, 1 `no-source`. Every one of the 14 fingerprinted
uses is a within-frame deposit.

## The instrument cannot witness the kind of memory students actually reach for

One memory has now been claimed three times, by three different students, on three
different problems, across five days:

    e-63b7c7c1-1906-412c-ae18-b4644762fbea
      author f34-guide, subject a95J03, deposited 2026-08-25
      name: bank-cheap-sorries-first-root-bound-final-assembly-sphere-nonvanishing-library-theorem

    f50/a1  a98A07   weak-fingerprint   novel-hits ["norm_num"]
    f58/a1  aunk04   already-in-base    0 novel, 5 hits all pre-existing
    f62/a1  b01J01   pending archive

Out of 110 shelved memories, this is the one that keeps getting selected. Earlier in this
note I recorded that as a pathology — "a memory that reads as relevant, gets selected, gets
reported as used, and changes nothing". Reading its body corrects that:

    "Two 30-minute Student attempts on the same file: attempt 1 closed bridge_2 and the
     final theorem and left bridge_1; attempt 2 (fresh session, with the route memories on
     the shelf) opened bridge_1 first, spent the budget..."

It is a **strategic** memory — about how to allocate a fixed attempt budget across a proof
with several holes. Bank the cheap sorries first. It names no Mathlib lemma, no import, no
tactic beyond the incidental.

`fingerprint_audit.py` witnesses a use by finding the memory's tokens in the artifact and
asking whether they were already in the base. That works for content transfer: a lemma
name, an import, a specific rewrite. It cannot work for strategic transfer, because a
student who follows "bank the cheap sorries first" writes a proof whose TOKENS are entirely
its own. The influence is in the ORDER of the work, not its vocabulary.

So the three non-witnesses are not evidence that this memory did nothing. They are evidence
that the instrument cannot see what it does.

**This qualifies the central finding of this note, and the qualification is real.** The
claim "no attempt has used a shelved memory predating its own frame" rests on `:used-ids`
and on fingerprints. `:used-ids` is a student's self-report and the students keep reporting
this one. The fingerprint is the check that overrides the self-report — and it is
structurally blind to exactly the category being reported.

What survives unchanged: for CONTENT transfer, the shelf contributes nothing measurable.
All 14 fingerprinted uses in the corpus are within-frame deposits, and the five
first-attempt solves (a99J01, a99J05, a99J06, b00J02, b01A02) used no shelved memory at
all — those are solves, so a strategic memory that shaped them would have to have done so
without being reported, which the students' own logs do not suggest.

What is now open: whether strategic memories transfer, and by what instrument that could be
established. Token overlap cannot do it. Candidate approaches worth someone's thought —
attempt-ordering compared against the memory's prescribed order; time-to-first-close;
whether a student's stated plan echoes a shelved plan. None of these are cheap, and none
are in place.

Until then the honest statement is narrower than what this note said before: **the shelf
shows no witnessed CONTENT transfer, and the campaign has no instrument that could witness
strategic transfer either way.**

## f62: the guide loop resumes, and the frame is its own control

f62 (b01J01) is the first frame since f53 to get a guide intervention certified
(`guide-intervention-1 :live-job-certified`, review `:promotion-certified`), after six
consecutive frames parked on the mode barrier. It then did something the campaign has been
missing: it ran both kinds of memory use in the same frame, under the same conditions, with
the same instrument.

**a1** — 1 of 110 used, `outcome=open`. The memory was
`e-63b7c7c1...` (f34-guide / a95J03), the strategic "bank cheap sorries first" memory
described in the previous section. Fingerprint verdict: **`unwitnessed`, 0 novel tokens**.

**a2** — 4 of 114 used, **`outcome=solved`**. All four were f62's own within-frame
deposits: two from `f62-guide`, two from `f62-scribe`, all on b01J01. Fingerprint verdicts:

    fingerprinted  6 novel  [CommGroup.equiv_prod_multiplicative_zmod_of_finite
                             Subgroup.index_comap_of_surjective ZMod.castHom_surjective
                             Subgroup.index_ker Pi.evalMonoidHom ZMod.castHom]
    fingerprinted  8 novel  [... plus MonoidHom.range_eq_top, M.index]
    fingerprinted  1 novel  [Subgroup.map]
    already-in-base 0 novel

`CommGroup.equiv_prod_multiplicative_zmod_of_finite` appearing in a proof that did not have
it is not a coincidence of vocabulary.

**Why this frame settles more than the others.** Everything is held constant — same
problem, same student role, same shelf, same audit, hours apart. The difference between a1
and a2 is which KIND of memory was used. The content memories from the frame's own guide
are witnessed, six and eight novel tokens deep, and the attempt closed. The strategic memory
from four days earlier is unwitnessed, as the previous section predicted it must be.

So the two findings in this note are not in tension; they are about different things:

- **Content transfer is real, witnessed, and within-frame.** All 17 fingerprinted uses in
  the campaign are deposits made by a role in the same frame as the attempt that used them.
  The guide-to-student loop works, and f62/a2 is its third demonstration (after f52/a2 and
  f53/a3) and the strongest, because the guide phase had just been repaired.
- **The shelf's older content contributes nothing measurable**, and for STRATEGIC memories
  the instrument cannot tell us either way.

Campaign tally after f62: 38 rows — 17 `fingerprinted`, 3 `weak-fingerprint`,
12 `already-in-base`, 6 `unwitnessed`.

## f65: the fourth guide→student solve, and two apparatus notes (2026-08-31)

**f65/b01J04 closed and is banked.** a1 used 0 of 123 (1 withheld, no leak). a2 used 2 of
126, both within-frame (`f65-guide`, `f65-scribe`). a3 used exactly one memory — an
`f65-guide` deposit on b01J04 — and returned `outcome=success`. `#print axioms apm_b01j04`
gave `[propext, Classical.choice, Quot.sound]` with no `sorryAx`, the statement was
unchanged, and the solve is now on `origin/master` (`1b625d3c`, `9b014793`) at
`sorry_count_total: 0`.

That makes four frames whose SOLVING attempt used a within-frame guide deposit: f52/a2,
f53/a3, f62/a2, f65/a3.

**Banking is not part of the frame cycle.** Nothing under `src/` calls
`bank-sweep/sweep-to-master!`. A frame can certify its close, pin a sorry-free head under
`refs/apm/banked-solves/<frame>/<problem>/<head>`, and stop there: the pin is in the
corpus repo, master is untouched, and the problem still reads as open. b01J04 sat in that
state between its close at 10:41Z and the sweep at 11:00Z, and b01J03 had spent the
previous night the same way. The campaign's idea of what is solved and master's idea of
what is solved drift apart by exactly the solves nobody has swept yet, and the only thing
that closes the gap is a person or agent deciding to run the sweep.

The sweep's own gate held: it banked f65/b01J04 and refused f51/a98J03 as `:not-pinned`.
f51's head is a 1-sorry improvement on master's 2, not a solve, and its frame stopped at
`guide-intervention-1` without ever closing — so the pin gate was refusing the right thing
for the right reason.

**A park disposition the machine cannot execute.** All eight parked frames now carry
supervisor decisions: seven `:partial` and, since last night, f63 `:retry`.
`reconcile-park-decisions` documents its own limit — "the decision's disposition is
recorded but never executed here" — and nothing else reads the field; there is no
validated vocabulary for it at all, only whatever a supervisor writes. So `:retry` reads to
a human as a pending action while no code will ever act on it.

It happens to be satisfied anyway, by a different route. f63's problem is b01J02, which
still has 1 sorry on master, and `derive-queue` defines open as "at least one remaining
Lean sorry" against `origin/master`. The JIT queue rederives from master each time, so
b01J02 stays in the open set and will be offered again on its own. The retry needs no
executor — but that is a property of how the queue is derived, not of the decision record,
and the record should not be read as having caused it.

### Correction: prior-frame reads are not zero, and never were (2026-08-31)

Through f63, f64 and f65 I reported each first attempt as "0 of N used, no leak, clean" and
described the run as attempts that used nothing from a prior frame's shelf. Those individual
counts were right and the streak I built out of them was not. Resolving the depositor of
every memory used in every attempt of the campaign gives 48 uses, of which 9 are
cross-frame:

    f47/a1  <- f39-guide    a97A01              cross-frame, SAME problem
    f47/a2  <- ams-codex-1  a97J02              cross-problem
    f47/a3  <- f44-scribe   a98A02              cross-problem
    f50/a1  <- f34-guide    a95J03              cross-problem
    f50/a2  <- codex-5      M-codex-sorry-loop  cross-problem
    f50/a3  <- f44-scribe   a98A02              cross-problem
    f58/a1  <- f34-guide    a95J03              cross-problem
    f62/a1  <- f34-guide    a95J03              cross-problem
    f66/a1  <- f34-guide    a95J03              cross-problem

The other 39 are within-frame guide and scribe deposits. What is true, and is what the
sections above actually establish, is narrower: every use that the fingerprint audit
WITNESSES — where the proof carries Mathlib tokens the memory introduced — is within-frame.
The cross-frame reads are real reads; they are just unwitnessed by that instrument. I
collapsed "no witnessed transfer" into "no reads", which is the fourth way to overstate
listed at the top of this note, run in reverse.

**One memory is doing all of the recent cross-frame work.**
`e-63b7c7c1-1906-412c-ae18-b4644762fbea`, deposited by `f34-guide` about a95J03, is a
`:kind :reference` route memory: how to sequence a time-boxed Lean session — bank the cheap
closes and commit first, do not open the hardest bridge first. Since f52 it is the ONLY
memory any attempt has read from a prior frame, and it has been selected by the FIRST
attempt of four frames on four unrelated problems:

    f50/a1  a98A07   partial
    f58/a1  aunk04   success
    f62/a1  b01J01   open
    f66/a1  b03J01   proved

Two of the four closed. That is not evidence the memory caused them: f66/a1's proof is 180
lines, sorry-free, and shares zero Mathlib tokens with the memory — no
`Multiset.card_le_card`, no `Polynomial.card_roots'`, no `image_circleMap_Ioc`, nothing.
First-attempt closure is also common here on its own (7 of 21 frames: f52, f54, f55, f58,
f60, f61, f66), so a1 closing is not by itself a signal.

What the pattern does establish is retrieval, not benefit. A single strategic memory about
ordering is being pulled out of a 129-item shelf by unrelated problems, repeatedly, while
everything else on the shelf goes unread. Whether that is the retriever finding the one
genuinely general item, or a ranking artefact that keeps returning the same row, is a
question about the retriever, and it is answerable by looking at what the shelf offers and
in what order — which is a different measurement from the ones in this note.

## Three verified solves the bank never saw (2026-08-31)

Frames whose FIRST attempt closed the problem mostly did not close the frame. f54, f55,
f58, f60 and f61 each ran one student attempt, one guide intervention, and no
`close-frame`: they are the frames parked behind the guide-mode barrier. Promotion to a
banked solve fires at frame close, so each of their proofs stopped where the frame did.

Five sorry-free attempt sources, none of them on master. Sorry-free is not the gate,
though, so I built each one against apm-lean's pinned Mathlib and printed its axioms:

    f60  b00J02  compiles, closes apm_b00j02, axioms include
                 apm_b00j02._native.native_decide.ax_1_2   -> REFUSED, correctly
    f61  b01A02  five native_decide calls                  -> refused on the same ground
    f54  a99J05  [propext, Classical.choice, Quot.sound]    -> passes
    f55  a99J06  [propext, Classical.choice, Quot.sound]    -> passes
    f58  aunk04  [propext, Classical.choice, Quot.sound]    -> passes

f60 is the useful case for calibrating what a student's own report is worth. Its attempt
recorded "closed: apm_b00j02 proved with 0 sorries ... exit 0", and every word of that is
true — the file compiles and the theorem is closed. It closes on `native_decide`, which
trusts the compiler's evaluation rather than the kernel, and the axiom print is the only
thing in the record that says so. The gate refusing it is the gate working.

The other three do pass, on all three counts: clean axioms, and a theorem statement
byte-identical to master's after whitespace normalisation. Their commits already existed —
`refs/apm/student-candidates/<frame>/<problem>/attempt-1/<sha>` — holding exactly the file
I built. So the work was preserved; what never happened was the promotion from preserved
candidate to banked solve.

    a99J05  f5916ac9  (f54/attempt-1)   master has 2 sorries
    a99J06  d3b1ea91  (f55/attempt-1)   master has 1 sorry
    aunk04  7ead4b45  (f58/attempt-1)   master has 3 sorries

I pinned all three under `refs/apm/rescued-solves/<problem>/<sha>` and stopped there. They
are NOT swept. Every problem banked so far carried a pin the machine made through
`verify-and-pin!`; these carry a pin I made from my own build, and substituting my
verification for the machine's gate is a different act from running the sweep the machine
had already authorised. The sweep is Joe's call.

a99J05 also shows the stall is not new: it already had a rescued-solves pin from
2026-08-29 (`f653f3dc`, a different and independently sorry-free proof), and master still
has its two sorries two days later. Rescuing the work is happening. Banking it is not.
`bank-audit/unbanked-solved` reads solves out of the campaign's frame records, so a frame
that never closed contributes nothing for it to find — which is why today's sweep saw only
f65/b01J04 and f51/a98J03, and none of these.
