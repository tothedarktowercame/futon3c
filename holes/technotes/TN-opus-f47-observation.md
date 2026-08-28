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

**For the apparatus.** The machine did not archive the student source. f49 has a
`student-attempt-1-source` directory; f52 does not. A verified proof existed only as a dirty
worktree, and any cleanup would have destroyed it. Preserved out-of-band as blob
`b42979bbbf150035e39d990f8c0d6675ca1c128d`, pinned at
`refs/apm/rescued-blobs/a99J01-f52-student`, without touching the student branch.

That is the third distinct way today that a completed proof was one step from being lost:
f48 and f49 voided with `:solve/pin-status :skipped`, f51's solver reported
`:sorry-warnings 0` for rounds whose heads still carried sorries, and now a solved student
attempt left unarchived. The axiom gate in `verify-and-pin!` protects what reaches master.
Nothing protects what never gets committed.

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

**No attempt has used a shelved memory that predates its own frame.** The shelf grew
76 -> 102 across them.

### A second line of evidence: the shelf accumulates restatement

f53's supply line ended at `2 approved this frame (+6 reassigned to existing patterns)`.
Six of eight deposits matched patterns already in the library; only two minted new ones. It
was `+3 reassigned` earlier in the same frame.

That is independent of the uptake counts and bears on WHY the shelf goes unused. If most
new material restates what is already held, then growth from 76 to 102 measures deposit
volume, not coverage. A student searching such a shelf meets many phrasings of the same
few things — which is consistent with what the students actually do: search, find the
restatements unhelpful, and work from Mathlib primitives or from what their own frame's
guide just told them.

The campaign is not failing to produce memory. It is producing a great deal of it, mostly
about what it already knew, and the transfer that demonstrably works happens over minutes
inside a frame rather than across days through the accumulation.
