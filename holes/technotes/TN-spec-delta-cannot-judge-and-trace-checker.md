# Spec delta — `cannot-judge` is not a verdict, and the trace checker joins the cycle

Author: Claude (Fable 5, `claude-12`), 2026-08-25, at Joe's direction after
the F32–F35 bank review. This is the specification the two implementation
packets work from; it is architecture, not implementation, and it is
deliberately short enough to disagree with.

Sources verified for this note: `mathlib4-apm-validation`
`feature/apm-cycle-model-v3` at `6f364b3cc1` (all six `DarkTower/APM*.lean`),
futon3c `master` at `90f5eb15`, frame records f32–f35.

## 1. The problem, stated once

On f33 (2026-08-25) three guide deposits and eight scribe candidates were
returned `:cannot-judge`; the frame certified `:result "closed"` with a
well-formed receipt. On f35, six frames later, the same defect voided three
more guide deposits — the reviewer writing its own verdict as *"Approval
cannot be persisted because the typed JSON boundary keywordizes
new-pattern-rationales keys while validate-pattern-accounting looks up string
pattern IDs"* — while judging all three coherent and actionable. The running
Student is three memories short and nothing stopped.

Two defects, in different layers:

- **The proximate one is a Clojure bug.** `role_memory_search/validate-pattern-accounting`
  looks up `new-pattern-rationales` by string key; the typed JSON boundary
  keywordizes those keys. Every coined pattern id is therefore unaccounted and
  every such deposit fails, unconditionally, since it was written. No
  specification prevents a lookup bug and none should be expected to.
- **The one that matters is a modelling error.** `be9978b5` added
  `:cannot-judge` to a flat Clojure set of review verdicts so that an
  unreviewable pass would stop blocking frames. It succeeded. A review pass
  that resolved nothing now certifies, and the only record that anything was
  wrong is prose inside the verdicts themselves.

**Why Lean did not catch it.** It has no verdict type: `cannot-judge`,
`approve` and `reject` do not occur in any `DarkTower/APM*.lean` file. The
widening needed no Lean counterpart because Lean never modelled what a verdict
is. Qualification passed correctly and vacuously.

## 2. What the verification apparatus currently does

Worth stating precisely, because the second packet only makes sense against it.

**Wired and real.** `generated_contract/validate` compares the Lean-emitted
`apm-cycle-contract-v4.json` against `required-*` maps in Clojure and fails on
inequality; `countdown_control.clj:45` reads that pin at frame launch. A wrong
*value* — a changed timeout, a missing policy key — is caught. That is a
genuine Lean→Clojure gate and it works.

**Not wired.**

1. `campaign_trace/from-durable-state`, the projection from a live frame to the
   checker's input, has exactly one caller: its own unit test. Nothing in
   `src/` or `scripts/` invokes `APMCampaignTraceChecker`. **The campaign trace
   checker has never been run against a real frame.**
2. `validGuideSnapshotTransition` (`APMCycleMachine.lean:355`) requires
   `candidatePatternsNonempty = true` — exactly what f33's and f35's voided
   reviews violate (`:pattern-ids []`). Its only reference in the repository is
   a theorem proving that a literal the model invents (`unreviewedGuideSnapshot`,
   hardcoded `"f27-guide"`) is refused.
3. `promotion-review-enums-normalized`, `promotion-rejections-explicit`,
   `promotion-approved-candidates-accounted` and `candidate-pattern-binding-required`
   reach Clojure only as values checked equal to constants Clojure hardcodes.
   No code branches on any of them. They are asserted, not enforced.

So the apparatus verifies constants and does not observe behaviour. This is the
retrieval-whitepaper §7a class — *written but not wired up* — located in the
validator itself.

## 3. The model contradicts itself about snapshot binding — read this before wiring

`APMCycleContractEmitter.lean:187` emits
`next-student-binds-latest-reviewed-snapshot: true`. `APMCampaignTraceChecker.lean:164`
requires

```lean
trace.studentBindings.all fun binding => binding.snapshotDigest == trace.solverSnapshotDigest
```

These are different properties and the running design realises the emitter's,
not the checker's: f33 bound two distinct snapshots across its attempts
(solver, then a guide-2 union), f34 bound three (solver, guide-1 union of 25,
guide-2 union of 27). Under the checker as written, **every frame with an
effective guide intervention is invalid.**

Consequence for packet 2: wiring the checker unchanged makes every recent frame
go red, and the tempting response — weaken the checker until frames pass — is
the wrong one and would convert this note's whole argument into its opposite.
The checker is what is stale. The correct predicate is a *chain*: attempt 1
binds the solver snapshot; each later attempt binds either the same digest or
the union snapshot published by the most recent completed review; and every
digest is content-addressed and non-empty. Fix the checker first, then wire it,
then confirm f32–f34 pass and a fabricated out-of-chain binding fails.

## 4. The delta — `cannot-judge` is not a verdict

The single modelling decision:

> A review pass yields, per candidate, either a **judgement** about the
> candidate (`approve` / `reassign` / `reject`) or a report that the
> **apparatus** could not produce one (`cannot-judge`). These are different
> kinds of thing and the type must say so. `cannot-judge` carries no
> information about the candidate; it is the reviewer reporting a defect in
> the machine it was asked to use.

From which:

- **A pass is `resolved` when every candidate carries a judgement.**
- **An unresolved pass does not advance the frame.** Its successor is a
  non-advancing hold — the `:awaiting-apparatus-repair` state
  `TN-fable-F32-model` §3.3 already prescribes for repair exhaustion; the
  same state, reached a second way. The frame holds at its last valid receipt.
- **The hold is resumable, keyed on the contract blob having changed**: once
  the apparatus is repaired, re-validate the *persisted* review without
  redispatch (f33 and f35's candidates were judged coherent — they need no new
  reviewer turn), or re-dispatch under the new contract version as an
  append-only successor. A frame is never voided by the driver (Joe,
  2026-08-25).

**What this must NOT do.** Zero approvals is a legitimate outcome and stays
one: a pass in which every candidate is rejected on the merits is *resolved*
and advances. Both scribe cards and `promotion-proctor-v3` rule 8 state that
zero approvals is a finding to report, not an error — this delta must leave
that exactly as it is. Only the absence of a judgement blocks. A pass that is
part judgements and part `cannot-judge` is unresolved: the judged candidates
keep their verdicts and are not re-reviewed on resume.

Had this held on 2026-08-25, f33 would have stopped rather than certified, and
f35 would be holding now instead of running three memories short.

## 5. Packets

Two, sequential, with a review between. Both are Lean-first: the emitter owns
receipt schemas and phase I/O since `hole-generated-receipt-schemas-v1` closed,
so a verdict vocabulary added only on the Clojure side would reproduce the
`:surfaced-ids` defect of F32 exactly.

**Packet 1 — model the verdict (Lean only, safe during a running frame).**
An inductive verdict type with `cannotJudge` distinguished from the three
judgements; `resolved` over a pass; the non-advancing successor for an
unresolved pass, with its resume conditions; theorems that an unresolved pass
does not advance and that an all-`reject` pass does. Emitter gains the verdict
enumeration and a `promotion-pass-resolution-required` policy key.
Acceptance: `lake build` clean; a fixture pass containing one `cannotJudge`
does not advance; an all-`reject` fixture does.

**Packet 2 — wire the checker (Lean + Clojure).** Fix `memoryValid`'s binding
predicate per §3; call `from-durable-state` and the checker at close-frame for
every frame; a frame whose trace fails the checker does not certify. Acceptance:
f32, f33, f34 replay green from their persisted ledgers; a fabricated
out-of-chain binding and a fabricated unresolved pass each fail; no redispatch
anywhere in the test.

**Then, small and separate:** the string/keyword fix in
`validate-pattern-accounting`, plus re-validation of f33's and f35's persisted
reviews under the corrected validator without redispatch.

## 6. Sequencing against the running campaign

- **Safe during F35**: everything in `mathlib4-apm-validation` (separate repo,
  touches no running process); Clojure built and tested in its own worktree
  JVM.
- **Waits for F35 to close**: regenerating `apm-cycle-contract-v4.json` —
  `countdown_control.clj:45` reads that path at frame launch, so moving the pin
  mid-campaign changes what the next frame validates against. Likewise any
  namespace reload into the shared JVM, which is reload-from-master only
  (futon3c CLAUDE.md, one-JVM policy).

---

## 7. Correction (Joe, 2026-08-25): unreviewable material should be impossible, not held

Joe: *"The 'unreviewable material' is something that the Lean system should
have modelled as a total violation and made impossible."*

He is right, and §4 above stopped one step short. §4 says `cannot-judge` is
not a judgement and that a pass containing one must not advance — detection
plus a hold. But detection was the second-best countermeasure available. §2.2
of the whitepaper orders them: *make the invalid state unrepresentable* first,
*instruments that make absence loud* second. Packet 1 built the instrument.

**The evidence that this is a category error, not a verdict.** Every cause of
`cannot-judge`, specified and observed, is an apparatus failure:

| source | cause |
|---|---|
| `promotion-proctor-v3` | no base problem blob or Solver final head (`:reviewer-inputs-missing`) |
| `promotion-proctor-v3` | `evidence-ids` name a job trace that cannot be fetched (`:trace-unavailable`) |
| observed, f32–f35 | "persisted evidence unavailable; the candidate memory entry and current memory/assert edge cannot be fetched" |
| observed, f32–f35 | "the dispatched candidate ID has no evidence entry or memory/assert edge" |
| observed, f33 | "typed pattern-accounting is unsatisfiable … JSON keywordizes rationale keys" |
| observed, f35 | "the job search did not discover the exact pattern ID" |

Not one instance is a reviewer that could see a candidate and could not form a
judgement about it. All 26 are the machine failing to supply what it asked
someone to judge.

**So the obligation moves from the verdict to the dispatch.** A review dispatch
must carry, per candidate, evidence that:

- the candidate is persisted and fetchable at the id being dispatched;
- its parent pattern resolves to a fetchable entity;
- the reviewer's own inputs resolve — base problem blob, Solver final head, and
  any job trace named in `evidence-ids`.

Absent those, **there is no dispatch to send**: the review request is not
constructible, and the frame holds at the deposit step with a named apparatus
defect rather than paying for a reviewer turn that can only fail. Reaching
`awaitingApparatusRepair` remains possible as a backstop for a failure mode
nobody anticipated; it stops being the ordinary path.

**A second defect visible in the same evidence.** Several reasons read "the
dispatched candidate ID has no evidence entry" — the machine dispatched
candidates for review that had never been persisted at all. The deposit half
failed silently and the review half was asked to judge nothing, both emitting
well-formed receipts. The precondition closes this too, and it is the more
alarming of the two because it means a scribe's whole pass could evaporate
between writing and review with nothing in the receipt to say so.

**Sequencing.** Fix 1 (coined-pattern ingestion, in flight) removes one CAUSE.
This removes the POSSIBILITY, and subsumes fix 1 as one of the things that
satisfies a precondition. They touch the same code path, so they land in that
order rather than concurrently. Lean-first: the precondition belongs in the
model before the driver, or it reproduces the F32 shape — an obligation
enforced in Clojure with no counterpart in the specification.

## 8. Re-entry after repair needs a NEW job id — observed, f37, 2026-08-25 23:45

§4 said a hold resumes either by re-validating the persisted review or by
"re-dispatch under the new contract version as an **append-only successor**".
That phrase was written as a design preference. f37 has now shown it is a hard
requirement, and shown what happens without it.

Sequence: f37's four scribe candidates were never persisted (§7's second
defect, live). `67e381aa` fixed the deposit path so candidates persist before
review, with controller-derived `:memory-id` and `:content-digest` and the
agent's claim retained as `:reported-content-digest` — the F32 `:surfaced-ids`
lesson applied to deposits. The recovery then reset f37's promotion to
`:stage :deposit` to re-run it through the corrected path, which is right.

It then re-announced the SAME job id:

```clojure
:error/code :promotion-stage-dispatch-failed
:announced {:ok false, :err "announced-job-conflict",
            :http/status 409, :state "done",
            :job-id "apm-role-69dbf1ae47aba52250afd82e074f34ceed80cc354a5e32f45796a9dcf0b6a502"}
```

Agency already holds that id as `done` and refuses it. So a frame repaired
correctly cannot re-enter its own phase, and the regulator cycles
`:running`/`:failed` against a 409 with no progress and no new failure record.

**The rule.** A phase re-entered after an apparatus repair is a SUCCESSOR, not
a replay: it mints a fresh job id and links the prior one as its predecessor.
Reusing the id is not merely rejected, it is wrong in principle — the prior
job really did run and really did terminate, and its receipt should stay
readable as the record of the attempt that failed. Overwriting it would erase
the evidence that a repair was needed.

This also gives the resume path in §4 its concrete shape, and it is why "never
replay a persisted terminal that failed deterministically without changing
something" (Joe, F32) has a second half: when you HAVE changed something, the
re-run is still a new dispatch, not the old one retried.

## 9. The general rule: an agent-minted identifier is a claim, not a fact

Four boundaries failed the same way on the night of 2026-08-25, in sequence,
each discovered only after the previous was fixed:

| # | boundary | what was minted | what existed |
|---|---|---|---|
| 1 | scribe deposits candidates | `e-codexpilot-scribe-f37-<slug>` ×4 | nothing — no evidence, no `memory/assert` write |
| 2 | promotion re-enters after repair | reuse of `apm-role-69dbf1ae…` | the job, already `done` — 409 on every retry |
| 3 | reviewer returns a verdict | `:verdict :reassign`, reasoned | attachment still `:proposed`; verdict unwritten |
| 4 | reviewer cites its review evidence | `e-review-f37-affine-sinc-limit-successor` | nothing — 404 |

These were treated as four defects and fixed as four defects (`67e381aa`,
`92b30026`, `07e0433d`, `1c260df0`, `13b3bb59`). They are one defect with four
instances, and fixing them singly means discovering instance five in production.

**The rule.** *Wherever an agent mints an identifier for something it claims to
have written, that identifier is a CLAIM about an artifact, and the artifact
must be read back before the identifier is usable.* The controller derives the
id from the persisted content (as `67e381aa` now does for candidates); the
agent's own name is retained beside it as a claim — `:reported-content-digest`
— so a divergence is visible rather than reconciled away.

Applying this per-site is the mistake. The sites are enumerable: candidate
deposit, review evidence, review verdict, snapshot publication, guide deposit,
terminal submission. Each is an agent→controller boundary carrying an id.

**Relation to `ValidReviewDispatch` (`b5d6d08edc`, §7).** That type says a
review dispatch may not exist unless its candidates and reviewer inputs are
fetchable. Boundary 1 is exactly what it forbids, and the driver now enforces
it — `67e381aa` "verifies both are fetchable before constructing reviewer
dispatch". Boundaries 3 and 4 are the same obligation on the review's OWN
outputs, which the model does not yet cover: a review is not complete until
its verdict and its evidence are readable. The model should be extended to say
so, rather than the driver growing a fifth bespoke check.

**Confirmed by prediction, same night.** The list above named the untouched
sites as *candidate deposit, review evidence, review verdict, snapshot
publication, guide deposit, terminal submission*. Within the hour f37 failed at
the next one on that list. codex-10's root cause, verbatim: "snapshot
publication used pre-persistence agent review claims instead of the
controller-normalized reviews that were actually written to the substrate. This
made the valid reassignment attachment appear inconsistent." Six commits to
clear it (`133f0273`, `95aa4b26`, `e955cdc2`, `7b008ad0`, `ebd9eed9`,
`a624d9e6`), after which scribe-reduce certified 1 reassign / 3 reject / 0
cannot-judge and the failure count held at 53 across 40+ ticks.

A rule that predicts where the next instance will appear is worth more than the
five repairs that preceded it. Two sites on that list remain unaudited: guide
deposit and terminal submission.

**A correction on how this looked from outside.** While those six commits
landed, the failure count climbed 48 -> 53 and the error code cycled between
four classes. I read that as the wall repeating and set a stopping rule. It was
not repeating: each new class was a deeper layer becoming reachable once the
one above it was fixed. The distinction matters operationally -- *a changing
error class means the obstruction is moving; only a REPEATING one means it is
not* -- and on this evidence I would have stopped the work one commit short of
the fix.

**Why it kept being missed.** Every one of these emitted a well-formed receipt.
The scribe's four candidates had content-digests, pattern-ids, lane and leaf
summaries; the reviewer's verdict had a substantive reason naming real Mathlib
APIs. Nothing in any receipt was malformed. The only signal was the artifact's
absence, and only a check that goes and looks can see that.

### 9a. Amendment: the rule binds at mint time, not retroactively

codex-10 pushed back on §9 while implementing it, and was right. The rule as
first written — "the controller derives the id from the persisted content" —
is correct for a NEW output, and wrong for an id that an immutable completed
record already references.

f37's review evidence id `e-review-f37-affine-sinc-limit-successor` was minted
by the reviewer and cited inside a review that had already completed. Replacing
it with a controller-derived id would have made the id factual at the cost of
breaking the completed review's own reference to it — repairing the artifact by
falsifying the record that points at it.

What was done instead: persist the reviewer-supplied id EXACTLY, read the entry
back, and only then treat it as usable. The claim was made true rather than
replaced.

So the rule has two cases:

- **Minting a new identifier** — the controller derives it from persisted
  content; the agent's proposed name is retained beside it as a claim
  (`:reported-content-digest`), as candidate persistence does since `67e381aa`.
- **An identifier already referenced by an immutable record** — do not
  re-derive. Persist exactly what was referenced and read it back. If the
  content cannot be persisted under that id, the honest outcome is a failed
  review, not a substituted id.

`bccf8bbc` also gave the review boundary a single ordered contract, which is
the shape the other boundaries should copy: persist review evidence → read back
the exact entry → apply the returned verdict to the attachment → read back the
attachment projection → only then permit snapshot publication. Note the verdict
applied is the one the reviewer RETURNED, not a re-derived value.

**Still open**, and deliberately not absorbed at 00:40: snapshot publication,
guide deposit, and terminal submission are the remaining agent→controller
boundaries carrying identifiers. codex-10 flagged them as needing a systematic
audit rather than a fourth bespoke repair, which is the right call — they are
Joe's to schedule, not something to swallow inside a review fix.

**Independence, resolved.** The `:depositor "f37-promotion-proctor"` field in
f37's review record was mislabeled auxiliary output from the agent and was not
used as deposit authority. The authoritative depositor is `f37-scribe`; the
reviewer is `f37-promotion-proctor`. Reviewer independence held. Worth recording
because the mislabel is itself a small instance of the same family: a field
asserting something about provenance that was not the provenance the machine
actually used.

## 10. The shelf offers identifiers, not knowledge — f37, 2026-08-26

Zero uptake has been the campaign's most persistent symptom: f35 used 0 of 27
across three attempts, f37 used 0 of 22 and then 0 of 24. It was read as a
relevance problem (the shelf is full of other problems' guide notes) and that
reading is not wrong, but it is not the binding constraint.

**What the Student actually receives.** The whole memory input to f37
attempt 2, from its dispatch prompt:

```clojure
:memory-snapshot {:snapshot-digest "4e147e0b…"
                  :accessible-memory-ids ["e-01c38dee-…" "e-07a2f8d1-…" … 24 …]}
```

Twenty-four opaque UUIDs. No names, no hooks, no bodies. The memories' own
names -- `removable-singularity-normalize-to-continuous-ae-representative`,
`symmetric-truncation-limit-to-lebesgue-integral-over-the-line` -- do not
appear in the prompt at all.

**Why that matters here specifically.** Those two were deposited under
`claude-guide-v2.3` by a Guide that had read this Student's own failure
account; they were reviewed, `:attachment-status :reviewed`, verified present
on the shelf, and aimed at the exact residual the Student was stuck on (the
reviewer's reasons name `MeasureTheory.aecover_Icc`, `Real.sinc_of_ne_zero`).
Supply, delivery and relevance were all satisfied simultaneously for the first
time in the campaign. Uptake was still zero.

**Attribution.** `zai-student-v2.md` line 25 says plainly: "`:accessible-memory-ids`
lists every memory you may read. Fetch each of them **before you start** and
again **when you hit a wall**." The Student recorded `:queries []` on both
attempts. The instruction exists; it was not followed; the apparatus recorded
the non-compliance correctly. This is an agent behaviour failure, not a machine
defect.

But the design makes non-compliance the path of least resistance: complying
means 24 blind fetches before writing a line of Lean, with nothing to triage
on. The hook -- written expressly to be recognisable from the obstacle's
vocabulary -- is the one field that would make triage possible, and it is the
field withheld.

**Corroboration from the one success.** The campaign's only cross-problem
transfer (f33 attempt 1) came through OPEN SEARCH, not the shelf -- and search
returns content. The same card's line 30 ("search with the vocabulary of your
obstacle") is what produced it. When the channel shows the Student what it is
offering, it gets used; when it shows a list of hashes, it does not.

**Two candidate repairs, different in kind.** (a) Put each accessible memory's
hook in the prompt beside its id -- cheap, and the difference between a memory
channel and a list of hashes. (b) Refuse an attempt that records zero queries
against a non-empty shelf -- enforcement, the same shape as an unresolved
review not advancing a frame. Both change what the experiment measures, so
both are Joe's call. Recorded here rather than acted on.

**Consequence for every uptake number banked so far.** They were measured
against a channel that never showed the Student the content it was counting as
"offered". `:accessible-memory-ids` has meant *ids listed in a prompt*, not
*memories the Student could see*.

## 11. The witness standard is silent on procedural memories — f39, 2026-08-26

f39 attempt 2 produced the campaign's first CROSS-PROBLEM use through the
shelf: `e-63b7c7c1`, `:evidence/subject {:ref/type :problem, :ref/id "a95J03"}`
— mined from f34's problem, used on a97A01. The Student made 3 queries and
selected it from 27 accessible memories. That provenance is substrate fact, not
a claim, and it is the shape tier-A condition 3 asks for.

The fingerprint check returns nothing. None of the memory's named identifiers
(`Multiset.card_le_card`, `Polynomial.card_roots'`,
`circleIntegral_logDeriv_eq_divisor_sum`, `MeromorphicOn.divisor`) appears in
the artifact.

**That is the correct result, and it is why the check cannot adjudicate this
use.** The memory is PROCEDURAL: "bank cheap sorries first … attempt 2 opened
bridge_1 first, spent the budget confirming API spellings, and ended with ZERO
compiled lines — all three sorries open. Order for a time-boxed session: (1)
bridge_2 …". Its content is the ORDER OF WORK under a budget. Its named
identifiers belong to a95J03's proof and have no business in a97A01's; their
absence is what correctness looks like, not what failure looks like.

`retrieval-whitepaper-v3` §3.1 measures whether the committed artifact carries
identifiers the memory named, differenced against the base file. That works for
API-transfer memories and is SILENT BY CONSTRUCTION on advice about sequencing,
budget discipline, or which obstacle to attack first. A procedural memory that
worked perfectly and one that was never read produce the same fingerprint: none.

**Consequence for §3.1a's numbers.** The 35-event audit reported 0 unwitnessed
claims. That reads as "every USE claim left a trace" and should read as "every
USE claim of the KIND this instrument can see left a trace". The audit's
population was API-naming memories; procedural ones were not distinguished, and
any that were present were scored by a test that cannot detect them.

**What would witness a procedural memory.** Not a lexical trace. Its claim is
counterfactual — the Student did X before Y, and would have done otherwise. The
honest candidates are (a) the ORDER of closure across attempts, which the
receipts already record (f39: base 4 sorries → 3, 19 lines added, one bridge
closed), compared against the order the memory prescribes; or (b) treating
procedural memories as a separate class whose uptake is reported but whose
load-bearing-ness is not claimed without the ablation. Do NOT extend the
fingerprint regex to match prose — that would manufacture the appearance of a
witness where none exists, which is the failure this whole campaign documents.

**Status.** Tier-A condition 3 is NOT recorded as met on this evidence. The
transfer is cross-problem and real; whether the memory did any work is
unadjudicated, and the standard as written cannot adjudicate it.

## 12. `:used-ids` records consultation, not load-bearing use — f39 attempt 3

f39 attempt 3 claimed three uses of 29 accessible, including BOTH guide
memories written for its own stated residual and delivered through the
corrected review path (`e-23a2940f`, `e-6556333d`, both `:attachment-status
:reviewed`). It made 2 queries, certified at 16 KB, and closed one sorry
(base 4 -> 3, 201 -> 223 lines).

**The fingerprint check returns zero.** None of the identifiers those memories
name -- `Finset.Ico`, `Finset.sum_Ico_eq_sum_range`, `Finset.sum_nbij`,
`Finset.range`, `Ico_disjoint_Ico`, `Ico_subset_Ico` -- appears in the artifact.
They are absent from the base blob too, so this is not base contamination.
Unlike §11's procedural memory, these DO name APIs, so §3.1's standard applies
and it comes back empty. This is the campaign's first genuinely unwitnessed USE
claim; the f28-f34 audit found 0 of 35.

**But the student is working the same obstacle.** Its proof contains `Ico` ten
times and `cell` twenty-three times -- the coarse/fine cell decomposition the
memories address. It engaged the territory and did not adopt the prescribed
`Finset` machinery.

**What this says the field means.** `zai-student-v2` asks the Student to report
memory-use. If it reads that as *which memories I consulted* rather than *which
load-bear in what I wrote*, then `:used-ids` is a CONSULTATION COUNT. Every
uptake number in this campaign should be read that way, with the fingerprint as
the separate and stricter question of whether anything transferred.

The two diverge cleanly here, which is the point of having both:

| | claimed | fingerprinted | closed |
|---|---|---|---|
| f37 attempt 3 | 5 | yes -- 4 APIs novel vs base | 0 sorries |
| f39 attempt 3 | 3 | no -- 0 of 6 named APIs | 3 sorries |

f37 is load-bearing use. f39 is consultation. Reporting them under one heading
would have made the campaign's uptake look twice as good as it is.

**Consequence.** Do not "fix" the divergence by tightening the Student card
until `:used-ids` means load-bearing -- an agent cannot reliably know which of
its reads bore weight, and asking it to assert that manufactures exactly the
claim §9 warns about. Report consultation and fingerprint separately, and let
the gap between them be a measurement rather than a defect.

## 13. Two findings from f40, the first healthy frame

f40/a97J05 ran the intended loop with nothing broken in it: scribe mined the
solver, deposits persisted under controller ids, the reviewer read full
evidence and APPROVED on merit, the memories reached the Student, the Student
used them, and attempt 2 closed the problem at 0 sorries in 171 lines against a
186-line base. Four used memories, all four fingerprinted with APIs absent from
base blob `539525533e98`. Five approvals across the frame (4 scribe, 1 guide) —
the scribe seats had been 0-for-31.

**Read the 0-for-31 correctly.** The cards were revised six hours before f40
and did not change again. What changed is that deposits survived to be read and
reviewers read real evidence. Every prior zero was destroyed before
persistence, refused on a hook-only projection, or rejected for fields no card
documented. None was a judgement about the deposits. The seats may have been
producing approvable work throughout; the machine could not show it.

### 13.1 A legitimate `cannot-judge` exists after all

§7 catalogued every documented cause of `cannot-judge` and found all of them to
be apparatus failures, which is why it argued the verdict is a category error.
codex-10, asked directly, said it saw no legitimate candidate-level meaning
once dispatch validity holds. f40's guide-intervention-2-review produced a
counterexample:

> "…**pattern fit cannot be certified under the frozen one-search budget**: the
> reviewed-corpus search returned no canonical support for the proposed
> `math-strategy/missing-dependency-protocol` attachment, and assigning any
> discovered singularity pattern would be incoherent."

The reviewer judged the CONTENT completely — it named `HasSum.congr_fun`,
explained the `NEW = OLD` versus `OLD = NEW` direction, and called the memory
findable. It declined only the taxonomy placement, having searched once under a
bounded budget and found nothing that fits, and refused to force an incoherent
attachment.

That is not the machine failing to supply evidence. It is a reviewer declining
to fabricate a category — the same discipline §9 asks for everywhere else.
**Before `ReviewVerdict.cannotJudge` is retired, this case needs somewhere to
go**, or the reviewer's only options become fabricating an attachment or
rejecting good content on a technicality. The honest shape is probably a
distinct outcome for *content judged, attachment uncertifiable under budget* —
which is a statement about the corpus and the search budget, not about the
candidate.

### 13.2 A cosmetic sink can fail a whole tick

f40 also failed with `:live-supervisor-projection-failed` ->
`:problem-projection-buffer-sink-failed`, whose finding is:

    emacsclient: can't connect to /run/user/1000/emacs/server: Connection refused

The projection PUBLISHED correctly — `:publication {:ok true, :path
".../f40/problem-buffer.md"}`. Only the mirror into an Emacs buffer failed,
because no Emacs server was running at 07:00. A display convenience that cannot
reach a dead socket takes down a regulator tick.

Same structural shape as the night's other blocks: a step with no successor
state stops the thing it was decorating. The durable record is written and
correct. A sink that is not the record should not be able to fail the frame.

## 14. The pattern corpus is narrower than what the scribes learn — f40

§13.1 recorded one `cannot-judge` that was not an apparatus failure. f40
produced two more in `scribe-reduce`, with the same shape, which makes three in
one frame and turns an anomaly into a structural finding.

All three say a version of:

> "pattern fit cannot be certified under the frozen one-search budget: the
> corpus search returned no canonical support for the proposed
> `tactic-algebra-interference` attachment, and the discovered singularity
> patterns do not coherently describe a scoped-notation parse failure."

In every case the reviewer judged the CONTENT completely. One of the refused
memories is *"add `open scoped NNReal ENNReal` before binders using ℝ≥0 or
ℝ≥0∞"* — concrete, actionable, and explicitly checked as "not already in file".
What could not be certified was where it belongs in the taxonomy.

**The cause is not the reviewer and not the apparatus.** These memories are
about LEAN MECHANICS — notation scoping, parser errors, tactic interference,
algebraic packaging — and the canonical pattern corpus is MATHEMATICAL
(`math-formalization/*`, `math-informal/*`, singularity patterns). A bounded
search cannot find a home for a scoped-notation parse failure because no such
home exists. The reviewer is refusing to attach an incoherent parent rather
than forcing a fit, which is exactly the discipline §9 asks for.

**Consequences.**

1. `ReviewVerdict.cannotJudge` cannot simply be retired (§7 proposed it, §13.1
   qualified it). Three instances in one frame are the taxonomy speaking, not
   the machine failing. Retiring the constructor would force reviewers to
   fabricate attachments or reject good content on a technicality — both worse
   than an honest "cannot certify fit".
2. The honest repair is on the CORPUS side, not the verdict side: either the
   pattern library gains a place for Lean-mechanics knowledge, or such memories
   are admitted with an explicit "no canonical parent" status rather than
   blocked. Note `codex-scribe-v2` already asks for exactly this kind of
   knowledge — "the API that actually fits", parse errors masquerading as
   tactic failures — so the cards are commissioning work the taxonomy cannot
   file.
3. The one-search budget is doing real work here and should be named as a
   parameter. "Cannot certify under a bounded search" is a different claim from
   "no such pattern exists", and the receipts currently conflate them.

**Scale of the loss.** f40 approved 5 of 9 reviewed candidates; 3 of the 4
non-approvals are this class. Roughly a third of the frame's minable knowledge
is being held up by a taxonomy gap rather than by any judgement about quality.

## 15. Two watcher defects where the failure state is indistinguishable from the quiet state — f40/f41, 2026-08-26

Both found while watching f41 start, both in the watching apparatus rather
than the machine, and both of the same shape: **the instrument's broken state
produces exactly the output its healthy quiet state produces.** Neither could
be caught by looking at the instrument's output, which is the only thing
anyone looks at.

### 15.1 The frame watcher restarted itself on a signal it generated

`387ba76e` taught the babysitter to detach the frame watcher when the queue's
`:active` goes nil during the mint gap. It did detach — and then the
supervisor restarted it on the frame that had just certified, because
terminating the watcher makes its reader thread emit an EOF, and EOF was the
crash signal. The teardown was indistinguishable from the failure.

Worse, the restart is self-sustaining: `start_watch()` calls `stop_watch()`,
which emits another EOF, which the queue may deliver after `start_watch()`'s
drain, which triggers the next restart. The f40→f41 boundary shows the loop
in the log — one detach at close-frame, a re-attach to the certified f40,
then four attach/exit pairs on f41 that were not f41 crashing at all. It
terminates only by luck, when an EOF happens to land before a drain.

Fix (`b1b581a0`): the EOF sentinel carries the process that emitted it.
`stop_watch()` already clears `current_proc`, so a sentinel whose process is
not the installed watcher is stale by construction. This is exact where the
drain and the frame-id guard were both timing-dependent.

Consequence while it was live: the watchdog alerted continuously on f40 after
f40 was correctly complete —
`active-phase-state-invalid+coordinator-heartbeat-stale+terminal-job-collection-stale`,
every one of which is the truthful description of a *finished* frame. Alerts
on correct work are how an alert channel stops being read.

### 15.2 The event emitter kept tailing a dead log

`apm-frame-events.py` pinned `APM_BABYSIT_LOG` to one path. Every babysitter
relaunch — three tonight — produced a new bg id and a new log file, leaving
the emitter reading a file nothing writes to any more. It did not error, did
not exit, and reported nothing. **Nothing is also what it reports when the
queue is quiet.** I restarted the babysitter twice tonight without restarting
the emitter and had no way to tell from the output.

Fix (`057f2009`): default `auto`, re-resolved every poll to the newest
`/tmp/futon3c-bg/*.log` that identifies itself as babysitter output. Existence
is deliberately not the trigger — a relaunched babysitter leaves the old file
on disk, so "the path still exists" is no evidence it is still live.

### 15.3 The general rule, and where else to look

§9's rule is about minted identifiers: *an id an agent gives you is a claim
until the artifact is read back.* This is the watcher-side analogue:

> **A watcher must be able to distinguish "nothing is happening" from "I am no
> longer able to see anything."** Where it cannot, its silence is unfalsifiable
> and its alerts eventually get ignored.

Both fixes work by attaching an identity to the thing being trusted — which
process emitted this EOF, which file is the live one — rather than inferring
liveness from a condition that holds in both states.

Sites not yet audited against this rule: the regulator's own heartbeat
staleness check (does it distinguish a stalled coordinator from an unreadable
one?), and `apm-frame-pulse.py`'s `sorries ?` field, which prints the same `?`
when the worktree has no Main.lean as when the count genuinely cannot be
parsed.

## 16. The machine halted a 141-problem campaign on a correct rejection — f41, 2026-08-26

At 08:21:01Z, with f41 already solved (a97J06 closed at 0 sorries in 3
rounds), the regulator went `:failed` with
`:error/code :promotion-review-projection-invalid` and stayed down.

The f41 promotion proctor had **rejected** a candidate because its pattern
attachment was incoherent:

> its attachment to `math-informal/failure-mode-characterization` is
> incoherent because that pattern explicitly concerns a theorem's sharp
> failure regime rather than a proof method failing on one problem

— naming `math-formalization-CA/measure-integration-api` as the pattern it
judged correct. That is `promotion-proctor-v3.md` rule 5 carried out to the
letter ("an incoherent attachment is never `:approve`"). The reviewer did
its job well, and the machine stopped the line on it.

### The guard governs a value it does not use

`memory_lifecycle.clj:429`:

```clojure
(when-not (or (= :reassign verdict)
              (exact-patterns? edge-patterns pattern-ids))
  (throw (ex-info "review pattern set does not match attachment" ...)))
```

Four lines above, `attachment-status` is `:proposed` for `:reject` and the
edge's patterns are rewritten only for `:reassign`. On the reject path the
review's `:pattern-ids` are stored on the review record and never touch the
edge. The invariant is load-bearing only for `:approve`, where it means
"these patterns are right"; for `:reject` it demands the reviewer echo back
the attachment it has just called incoherent.

### The graceful path already exists and is bypassed

`apply-existing-attachment-review!` (`memory_lifecycle.clj:246`) runs the
same comparison and returns `{:ok false :finding {:failure
:promotion-patterns-review-mismatch …}}`; its `:else` branch is what calls
`review-attachment!`. But `promotion_review_store.clj:196` calls the raw
`review-attachment!` directly, so the mismatch arrives as an exception,
becomes `:promotion-review-projection-invalid`, and takes the regulator down.

So the same condition has a designed non-fatal response and a fatal one, and
the promotion pipeline is wired to the fatal one. This is the third shape in
this note's collection, after §9 (a minted id is a claim) and §15 (a watcher
must know when it has gone blind):

> **Where a defect has a natural blast radius, the machine must not exceed
> it.** An invalid review is a defect in one candidate. Its correct
> consequence is that the candidate is not promoted — it stays `:proposed`,
> which is already the safe state — not that 141 problems stop.

The safety property Joe asked for (unreviewable material must be impossible)
is preserved by leaving the candidate `:proposed` and recording the finding.
It does not require halting the campaign, and halting it is what makes the
machine "hardly able to run".

Dispatched to codex-10 as `invoke-1787732828037-1423-93b3dd78` with the fix
scoped to: `exact-patterns?` required only for `:approve`; the promotion
review store routed through the finding-returning path; a per-candidate
invalid review must not set `:regulator/status :failed`. Recovery is the
verification — f41 must get past promote-solver on the proctor's existing
verdicts, which are not to be re-authored.
