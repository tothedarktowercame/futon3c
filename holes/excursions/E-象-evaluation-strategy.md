# E-象-evaluation-strategy: evaluating a War Machine click as a bundle of acts

Opened 2026-10-04 by claude-17 at Joe's direction, under M-象-2000. It starts
from the evaluation strategy claude-17 wrote for codex-10 the same day, for WM
click `wm-click-abb61cd8-2e13-4f76-91e1-1f8ae70522af` (run
`2026-10-04-16b392b4-…`, target M-self-documenting-stack, grounded commit futon7
`2fa9cb49`). That strategy answered one click. This excursion generalises it so
that the same analysis can be run on any click, by any reader, and give the same
mechanical results and comparable judgements.

Joe (2026-10-04): "2m2s selection and token breakdown should be evaluated against
the work actually achieved. If the agents vetted the mission, walked it through
DOCUMENT, and produced a useful Morning Brief, that may be a substantial success
even though it is not as cheap as writing DOCUMENT once by hand." And: develop
"a robust and *repeatable* analysis strategy", using 象's logic and markers.

## The idea

A click does more than produce an artifact. It also states things. It says
which want it chose and why, and that the artifact meets a checklist item. It
says the mission stays open, gives Joe a brief, and attests that two cascade
patterns helped. Each of these is an act in 象's sense (象/言即行): it has an
author, a kind, and content that a later record can confirm or contradict.

So the evaluation reads the click the way 象 reads a turn:
1. Split it into acts.
2. Give each act a mark from the reply-proforma key.
3. Apply the 象 rule that governs that kind of act.

The rule turns into a check with a falsifier and a closed verdict. Repeatability
comes from three fixed things:
- the decomposition into acts;
- the act-kind-to-rule table;
- the verdict vocabulary.

Only the judgement sentences vary between readers, and they are constrained by
the same table.

Nothing here makes Joe's approval a condition of reward. The checks run on the
record. Joe's reading enters as evidence about the click, under the outcome
model of §5, and never as a gate.

## 1. The click as a bundle of acts

Each unit of evaluation is one kind of act in the click. The mark is the one a
reply would carry if the click had been written as a turn to Joe.

| Unit | Act | Mark | Produced by | Example in abb61cd8 |
|---|---|---|---|---|
| U1 Artifact | A report of what was built, with evidence | ㊢ report | Author | `lc1-document-pass.edn`, the §7 pointer, the DOCUMENT checkbox |
| U2 Vetting | The judgement of what stays open, and why | 🈝 defer / 🈲 constrain | Author and reviewer | "mission remains open for a manual browser walkthrough" |
| U3 Decision | The choice of target and want over the alternatives | ㊝ prioritize | Selection | target M-self-documenting-stack, selected-wants `[h54d2f6cb14fa h048dfec887c1]`, R6/R14 bypasses |
| U4 Review | A claim that U1 meets the want | ㊬ verify | Reviewer | "The DOCUMENT item is complete" |
| U5 Brief | A standalone summary for Joe, plus things to try | ㊥ gist, ㊭ propose | Brief writer | brief item `ea1-6283…attempt-001` |
| U6 Cascade | Attestations that named patterns helped | ㊣ approve | Cascade feedback | evidence-to-disposition-once, reproduce-the-recorded-run |

The reusable evidence (retained prompts and replies with shas, the EDN join) is
not a unit of its own. It is how U1, U3 and U4 can be checked later, and its
value is counted under amortisation (§6).

A click that leaves a unit out is not penalised for the omission. Its record says
the unit is absent, as a typed none, so the gap shows up when clicks are compared.

## 2. From act kind to check

The 象 library already governs these act kinds
(`futon3/library/象/*.flexiarg`). Each row turns a pattern's THEN into a
check, and its HOWEVER into the falsifier.

| Act (mark) | Rule | Check | Falsifier |
|---|---|---|---|
| ㊢ report (U1) | 引非读: a reference is graded named / read / witnessed | Each evidence entry is graded. Shas are recomputed against the named file at the commit's time. Grade read needs a one-sentence account of what the file does; grade witnessed needs a test or run trace | A sha mismatch, a missing path, or an entry that is named-only but counted as support |
| ㊢ report (U1) | 每段自验: a phase states which part of the acceptance case now runs | The artifact advances the mission's own acceptance item. This is judged against the checklist text, not the artifact's form | The artifact passes every check of its own format and leaves the acceptance item where it was |
| ㊬ verify (U4) | 答必真且中的: an answer must be true *and* answer the question | The reviewer's verdict is true (the artifact has what it says) and on target (the want reviewed is the want selected) | The review is correct about something other than the selected want, or would read the same for any artifact of that kind |
| ㊝ prioritize (U3) | 视图出于史 + 双时并记: a view is rebuilt from history, and each read says which time it is as of | Replaying the selection inputs as of the click's start gives the same target and want. A singleton bypass holds only if the field had one candidate at that time | Other eligible candidates existed, or the replay picks something else |
| 🈝 defer (U2) | 诺必践 + 欠与被欠: an owed item is a debt with a criterion | Each "remains open" becomes a listed debt: what, who owes it, and the criterion that discharges it. The debt is consistent with the mission text | The mission text says the item is already done, or nobody is named as owing it |
| ㊥/㊭ (U5) | 象不忘: cite the record, don't copy it; voxterm reads only the first paragraph | The gist stands alone and names what changed and what is owed. Each "thing to try" can be done from the brief and cites a record | The gist omits the open debt, or a suggestion can't be acted on without opening the run |
| ㊣ approve (U6) | 释义非授 (an interpretation is not authority) + 引非读 | Each attestation cites the step where the pattern was used | The attestation would appear on any successful click (it is generic) |
| any | 明言假设: assumptions made while implementing are listed | The author's reply lists the gap-filling choices, as DERIVE-2 item 12's `disclosure/choice` would | The diff shows a choice (vocabulary, scope, file location) that the reply never mentions. Item 12 says this is only findable by review, not by query |
| the rubric itself | 裁亦有失: a rule writes its own HOWEVER first | Each new row of this table states how it could reward the wrong thing | A row with no stated failure mode |

**Verdicts.** Every check returns one of three verdicts, borrowed from the P8
fulfilment check (DERIVE-2 item 10):
- `:holds`
- `:fails`
- `:unable-to-determine`, with a reason from a closed set: `:input-missing`, `:input-unpinned`, `:out-of-population`, `:judgement-required`.

No check returns a score on a scale. Scales are where two readers diverge
without being able to say why.

**Populations.** Every number declares its population, as an answer does under
DERIVE-2 item 13: `:question`, `:sources`, `:excluded`, and `:read` (as of when,
on which time axis). This is how a token count for "the click" can be told apart
from a token count for "the author job". The abb61cd8 figures quoted to codex-10
mixed the two, by summing author and reviewer.

## 3. Quantities

Per click:
- **Phase times.** Selection, debugger dwell, author wait, reviewer wait, and
  the remainder up to wall time. The remainder is reported, not dropped: in
  abb61cd8 it is about 96 s of 536 s.
- **Tokens in three categories.** Uncached input, cached input and output, per
  role, each priced at its own rate. Derived from those:
  - input:output per role;
  - reviewer uncached input as a share of the author's (the cost of independence);
  - output tokens per line of artifact.
- **Progress.** Checklist items closed, wants reached against wants selected,
  and mission closure. These are typed counts; none of them is a proportion of
  "done".
- **Evidence completeness.** For each unit, the claims graded witnessed / read /
  named / missing (引非读).

Over time, from git and the evidence store:
- **Rework induced.** Later commits that change or revert this click's files or
  want within N days.
- **Reads.** Later reads of the artifact or retained evidence by a selection, a
  brief, an agent turn, or Joe. These are the observed uses that §6 amortises over.

## 4. Baselines

The click is not compared with "writing DOCUMENT by hand" as though the products
were the same. The comparison is made product by product:
- **Baseline A, measured.** One agent seat gets the same checklist item, starts
  from the parent of the grounded commit (a worktree at `2fa9cb49^`), and has
  no selection and no review. Record its tokens and time, and run the U1 checks
  of §2 on its output. Baseline A prices U1 alone.
- **Baseline B, by hand.** Record it as `:kind :estimate` with whoever
  estimated it. Never mix it with measured numbers.
- **Overhead products.** U2 to U6 are what the click produced beyond baseline A.
  Each is listed with the reader it serves and whether a read has been observed.
  This is E-象-spec-wiring's check applied to evaluation: an output nothing
  consumes is a finding, however durable it is.

The question the comparison answers is whether the overhead products are read
and used. Whether the overhead is small is a different question.

Baseline A must start from the same state as the click. In abb61cd8, §7
DOCUMENT already existed from 2026-05-21. The click added a machine-readable
join, a five-line pointer and a checkbox. A baseline that writes §7 from nothing
would flatter the click.

## 5. Operator semantics

Joe's response to a click, or to its brief item, is data with a known missing-data
structure (E-the-dark-tower-3 Q7; daxiang_live §6–8). It can take five values:

| Outcome | How it is recorded | Effect on the click's evaluation |
|---|---|---|
| Seen and acted on | An operator turn citing the click or brief, or a commit building on it | Read observed (§6); ㊣ confirms the reward; 🈸/㊭ uptake recorded |
| Seen, no response | The brief item was opened, with no later turn citing it | Nothing changes; recorded as such |
| Unseen | The brief item was never opened | Nothing changes; U5's effect is `:unable-to-determine :input-missing` |
| Accepted, not recorded | Later work presupposes it with no citation (turn 391/392 is the type case) | Found only by reading; recorded when found |
| Complaint or revert | A ㊩ about the click, or a revert of its commit | Withdraws the reward provisionally |

Rules:
1. **Grounded reward needs no operator signal.** It is computed from the §2
   checks on U1, U3 and U4.
2. **Silence is missing data, never approval.** Under the self-masking case the
   record cannot recover what silence means, so it is never imputed.
3. **A complaint acts like an inferred withdrawal (DERIVE-2 item 6).**
   - 象's reading of the ㊩ is an interpretation and ends nothing by itself
     (释义非授).
   - A separate step writes a provisional withdrawal of the reward, provided the
     complaint resolves to exactly one click.
   - The word `undo` reverses it.

   Joe is never asked to approve the reward, and a word from him is enough to
   take it back.
4. **"Seen" needs its own record.** At present the record cannot tell "unseen"
   from "seen, no response". Until the brief surface records opens, U5 is
   evaluated on content only, and the evaluation says so.

The brief's commitments are Elephant 2000 speech acts. "DOCUMENT done;
walkthrough owed" is a report plus a promise. The promise is a debt under
诺必践. Across a campaign, U5 is scored by whether its debts are discharged or
released, which needs no question to Joe.

## 6. Per click and per campaign

**Per click:** the §2 checks for U1 to U6, the population-declared quantities of
§3, the list of overhead products with their readers, and the anomalies.

**Per campaign:**
- closures per uncached token;
- the rework rate;
- observed reads of each click's artifacts and evidence;
- the distribution of operator outcomes (§5);
- the rate at which brief debts are discharged;
- whether U6 attestations predict anything: an attested pattern should appear in
  later grounded clicks more often than its base rate, or the attestation is noise.

**Amortisation.** The cost of a reusable output is spread only over reads that
have been observed. Until a read is observed, the click carries the whole
overhead. An estimate of future use is not a read.

**Repeatability is itself measured.**
- The mechanical checks must give identical results on a rerun with pinned inputs.
- The judgement checks are run by a second reader on a sample of clicks. Each
  disagreement is recorded with the row of §2 it falls under. A row where
  readers often disagree needs a tighter falsifier; the fix is not to drop it.

## 7. The evaluation packet

This packet is fixed. A click is evaluated by filling it in.

**Inputs, all pinned:**
- the run record and the brief item, by path and sha-256;
- the grounded commit and its parent;
- every file the artifact cites as evidence, at the commit's time;
- the retained author and reviewer prompts and replies, by sha;
- the mission file at the parent commit and at the grounded commit;
- an as-of time for every evidence-store read.

**Mechanical steps** (a script, eventually; by hand the first time):
1. Recompute the artifact's evidence shas, and grade each entry named / read / witnessed.
2. Reconcile dispositions: follow-ons in the mission text against entries in the
   artifact, plus a check that every named mission exists.
3. Diff the mission before and after, and measure what the artifact adds beyond prose that already existed.
4. Reconcile phase times, reporting the remainder.
5. Price tokens by category and role, each figure declaring its population.
6. Reconcile selected-wants with reached-wants, and the want the brief says it covered.
7. Recount the field for each bypassed rule (R6, R14, …) at selection time.

**Judgement steps.** One reader, who is neither the author nor the reviewer,
writes one sentence per §2 row. Each sentence carries the row's mark and a
citation. For example: `㊬ (U4 on target) holds: reviewer reply cites the five
:shipped ids and the empty :unclassified (reply sha bed8…)`.

**Output:** one EDN record, filed next to the run record:

```clojure
{:eval/schema :xiang/click-eval-v1
 :eval/click "wm-click-…" :eval/run "…" :eval/as-of #inst "…"
 :eval/inputs [{:role :run-record :path … :sha256 …} …]
 :eval/units {:u1 {:checks [{:row :yin-fei-du :verdict :holds|:fails|:unable-to-determine
                             :reason … :cites […]}]
                   :absent? false}
              … :u6 {…}}
 :eval/quantities {:phases-ms {… :remainder …}
                   :tokens {:author {:uncached … :cached … :output …} :reviewer {…}}
                   :population {:question … :sources … :excluded … :read …}}
 :eval/baseline {:kind :measured|:estimate|:none :u1-cost …}
 :eval/overhead-products [{:unit :u5 :reader :joe :read-observed? false}]
 :eval/operator {:outcome :unseen|:seen-no-response|:acted|:accepted-unrecorded|:complaint
                 :basis …}
 :eval/debts [{:what … :owed-by … :criterion …}]
 :eval/anomalies […]
 :eval/reader {:id … :distinct-from-author? true :distinct-from-reviewer? true}}
```

The record is itself an act. Its stamp names the reader as executor and signer,
with the evaluation's dispatch edge as authority. A later reader can withdraw
and replace it, but cannot edit it.

## 8. The first instance: abb61cd8

These anomalies were found while writing the codex-10 strategy. The first run of
the packet has to test them; none is assumed resolved.

1. **Want correspondence.** `selected-wants` holds two tokens,
   `:hole/h54d2f6cb14fa` and `:hole/h048dfec887c1`. In the record,
   `reached-wants` is empty 46 times and holds each token 22 times. Which token
   is the DOCUMENT checkbox? Were both reached? Does the brief's "want-coverage:
   DOCUMENT checkbox" agree with the final reached set?
2. **The mission contradicts itself.** The commit checks the DOCUMENT box inside
   a §7 headed "Closure summary — live walk-through completed" (2026-05-21). The
   walkthrough box above it stays unchecked. Either the box is stale, and the
   machine missed a possible closure, or the heading over-claims, and DOCUMENT
   now certifies a contradiction. This decides U2.
3. **DOCUMENT already existed.** This bears on baseline A (§4) and on U1's
   每段自验 check.
4. **The evidence shas are the author's.** Recompute them.
5. **Singleton bypasses.** R6 and R14 were both bypassed as singletons. A small
   field is a known WM failure, so the field size is a check, not a given.
6. **Unaccounted wall time**, about 96 s.

## Steps

1. Run the packet on abb61cd8 by hand. One Kimi or Zai job: the inputs are
   listed in §7, and the output is the EDN record. claude-17 reviews the record
   against §2. Not started.
2. Run the same packet on a second click by a different reader. Compare the
   mechanical steps for identity and the judgement steps row by row. This step
   is the first test of repeatability. Not started.
3. Script the seven mechanical steps. Only after step 2, so the script encodes a
   procedure that two readers have already applied. Not started.
4. Record brief opens, so that §5 can tell unseen from seen. That is a change to
   the brief surface, owned by M-象-2000's INTERACT work, not by this
   excursion. Not started.
5. Feed `:eval/debts` into the P9 obligations reader, so that "walkthrough owed"
   is listed where the other debts are. Not started.

## HOWEVER (裁亦有失)

This strategy could reward the wrong thing in three ways:
- **Rewarding thoroughness.** A click that produces many overhead products with
  readers named, but never read, will look well on a per-click reading. The
  amortisation rule (§6) is the guard, and it works only at campaign level.
- **Rewarding acts that are easy to check.** The §2 table rewards acts that
  leave checkable records, and a click that does useful work silently will
  score badly. The typed-none for absent units records that the work happened
  without punishing the silence. It does not reward the work.
- **Becoming a process to satisfy.** If the evaluation record becomes something
  a click must have, it turns into the gate CLAUDE.md warns against (a warrant
  is labour-saving, not a gate). It stays an observation about the click.
