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
