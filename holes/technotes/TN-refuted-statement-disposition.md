# TN-refuted-statement-disposition — a false registered target is a terminal mathematical result

Codex-2, 2026-08-26, design response to f45/a96J08.

Status: design for review before implementation. f45 should remain halted until
the new transition exists. This note does not treat the corrected target already
on `apm-lean` master as authority to rewrite f45's registered problem.

## Decision

Add `refuted` as a third `ProblemOutcome`. Its only valid frame result is
`frameVoid`:

| problem outcome | frame closed | frame partial | frame void |
|---|---:|---:|---:|
| solved | yes | yes | no |
| unsolved | no | yes | yes |
| refuted | no | no | yes |

The asymmetry is intentional.

- `unsolved / framePartial` means more work on the same registered statement may
  succeed, so the retained Solver head may be retried.
- `unsolved / frameVoid` means the frame cannot continue because its apparatus
  or baseline is unusable. It says nothing about the truth of the problem.
- `refuted / frameVoid` means checked evidence establishes the negation of the
  registered statement. Retrying that statement is forbidden.
- `solved / framePartial` remains bankable because the mathematical outcome and
  the experimental completion are separate facts.

`refuted` is preferable to Clojure's currently latent `:invalid`. “Invalid” does
not say whether the statement, apparatus, receipt, or registration is invalid.
In fact `queued_frame_adapter.clj` currently assigns `:invalid` to *every* void,
including `:apparatus-invalidated`. That contradicts the model's stated
orthogonality: broken apparatus does not establish a false proposition. Replace
that wire value; do not retain it as an alias in the new schema.

## A third constructor is necessary but not sufficient

`validOutcome .refuted .frameVoid` only validates a pair of labels. It must not
allow a caller to turn a Solver error, a failed proof attempt, or an informal
opinion into a refutation.

Introduce a content-addressed `RefutationCertificate` at the terminal boundary.
The Lean model should require one exactly when the outcome is `refuted`; the
Clojure validator should enforce the corresponding data shape before issuing a
statement-refuted void certificate. The certificate should bind:

- frame id and problem id;
- the registered repository, revision, path, blob, and target digest;
- one or more elaborated refutation theorem names;
- the checked artifact revision/blob containing those theorems;
- the Lean command result and axiom audit for each theorem;
- the reviewer/study receipt id and a digest of the recorded study;
- the source ledger version and digest.

The axiom policy belongs to the qualification authority already used for proof
artifacts. The transition must not invent a second, weaker notion of “checked.”
For f45, the two opposite-direction instances and the non-discriminating half
instance belong in the study; the certificate names their checked artifacts.
The type need not encode “two instances” globally. That is evidence specific to
this sign-error diagnosis, not a universal condition for refuting a theorem.

In Lean, make terminal evidence explicit rather than adding an unconnected
predicate beside `TerminalOutcome`. One suitable shape is:

```lean
structure RefutationEvidence where
  registeredTargetDigest : String
  checkedArtifactDigest : String
  theoremNames : List String
  axiomAuditDigest : String
  studyDigest : String

inductive VoidClassification
  | knownFailingBaseline
  | apparatusInvalidated
  | statementRefuted

structure TerminalOutcome where
  problem : ProblemOutcome
  frame : FrameResult
  learning : LearningOutcome
  voidClassification : Option VoidClassification
  refutation : Option RefutationEvidence
```

`validTerminalOutcome` then requires all refutation fields and a nonempty theorem
list exactly for `.refuted`, pairs that outcome only with
`.statementRefuted`, and requires `refutation = none` for `.solved` and
`.unsolved`. A void classification is present exactly for `frameVoid`; the two
non-refutation classifications pair only with `unsolved`. The executable trace
cannot prove theorem truth from strings; it can prove that the trace is bound to
the qualification result that did. The qualification checker remains the
authority for elaboration and axioms.

Add and prove these consequences:

- a refuted outcome is valid only with `frameVoid`;
- a refuted outcome is neither bankable, successor-eligible, nor retry-eligible;
- an apparatus-invalidated void cannot carry a refuted problem outcome;
- a statement-refuted void cannot carry an unsolved problem outcome;
- deleting or changing any refutation-evidence digest invalidates the terminal
  outcome.

## The trace checker needs a second terminal shape

`APMCampaignTraceChecker.accepts` currently demands the complete eleven-edge
phase sequence, all eleven receipt ids, memory and review completion, analyst
completion, and `closed = true`. Therefore it cannot accept an early void even
after `ProblemOutcome.refuted` is added.

Bump the trace schema and represent terminal disposition explicitly. Do not
overload `closed` to mean both closed and void. Suggested fields are:

```text
terminalDisposition: "closed" | "void"
problemOutcome:      "solved" | "unsolved" | "refuted"
frameResult:         "closed" | "partial" | "void"
refutation:          null | { certificateId, targetDigest,
                              qualificationDigest, studyDigest }
```

The checker should accept one of two paths:

1. **ordinary close** — the current full canonical sequence and its existing
   memory, review, analyst, and terminal checks;
2. **early void** — a nonempty canonical prefix ending at the receipt that
   detected the terminal condition, followed by a content-addressed void event.
   Ledger and receipt continuity, dispatch evidence for every executed phase,
   campaign isolation, and terminal digest binding still apply. Unexecuted
   phases must not have fabricated receipts.

For `statement-refuted`, the early-void branch additionally requires the
refutation certificate and qualification binding above. For other void
classifications, refutation evidence must be absent and the outcome is
`unsolved`. The trace checker should decode wire strings to `ProblemOutcome` and
`FrameResult` before applying `validTerminalOutcome`, rather than maintain a
second hand-written truth table in strings.

The contract emitter should publish at least:

```text
problem-outcomes = [solved, unsolved, refuted]
refuted-requires-frame-void = true
refuted-requires-qualified-evidence = true
refuted-retry-same-problem = false
statement-refuted-void-outcome = refuted
non-refutation-void-outcome = unsolved
early-void-canonical-prefix = true
corrected-problem-requires-new-registration = true
```

`generated_contract.clj` must require these exact values and reject unknown
terminal-policy keys as it does now. This keeps the generated Lean contract,
Clojure consumer, and trace checker from drifting independently.

## Clojure transition

There are two different durable operations and they should remain separate:

1. dispose of f45 on the evidence already obtained;
2. register a corrected problem and mint a new frame.

They must be ordered, but correction is not a precondition for recording a
valid refutation. Some false statements will have no known repair.

### 1. Refutation disposition

Add a typed controller command, for example `dispose-refuted-frame!`, at the
queue orchestration boundary. It receives the halted-frame receipt and the
qualified refutation/study receipt. It must:

1. verify that the active frame and registered problem pins match the evidence;
2. call `frame_void/prepare` with `:statement-refuted` and the refutation
   evidence;
3. append the obligation claim and `:frame/stopped` event with compare-and-append;
4. derive and validate a terminal receipt with
   `:problem/outcome :refuted`, `:frame/result :void`, and the certificate ids;
5. retire the frame and append its terminal record to queue state;
6. leave the queue with no active frame and with the original problem consumed,
   never retryable.

`frame_void/prepare` should make classification-specific evidence mandatory.
`:statement-refuted` requires a qualified refutation and study digest. The two
existing classifications forbid refutation evidence. Remove the default
classification: an omitted reason must fail closed, since defaulting a new
mathematical result to `:known-failing-baseline` destroys its meaning.

Wire this from the typed halted-frame disposition path, not directly from an
arbitrary `:error/code`. `:solver-defect-review-required` is how f45 stopped,
but an error code is not proof of falsehood. The review/qualification receipt is
the authority that selects `:statement-refuted`.

### 2. Corrected problem registration

A corrected target is a new problem registration, even when it uses the same
source path. It needs a new problem id and must bind the new revision/blob and a
new target digest. Its metadata should include:

```clojure
{:problem/supersedes <old-problem-id>
 :correction/basis-refutation-certificate <certificate-id>
 :correction/study <study-digest>}
```

Do not mutate the pinned queue plan in place and do not decrement `:next-index`.
The current plan id hashes its problem vector; changing the vector underneath an
existing state would either fail the plan/state check or require bypassing it.
Add an append-only queue amendment (or mint a successor queue plan) whose digest
binds the predecessor plan, added problem registration, authorizing disposition,
and resulting plan. Qualification of the new registration occurs before a new
frame is minted. Only then may f46 be created.

This gives the durable order:

```text
qualified refutation
  -> statement-refuted void certificate
  -> f45 terminal receipt (refuted / void)
  -> study recorded and bound
  -> corrected problem registration (new id and pins)
  -> queue amendment
  -> qualification
  -> f46 minted
```

If correction registration or qualification fails, f45 remains correctly void
and the queue remains without f46. Replaying any completed prefix is idempotent
by certificate/event/plan digest.

## Clojure ownership by namespace

- `campaign_machine.clj`: validate the stopped certificate shape and retain its
  terminal disposition in the projection; do not infer mathematical outcome
  from `:stopped` alone.
- `frame_void.clj`: classification-specific certificate validation, no default
  classification, qualified-refutation fields for `:statement-refuted`.
- `queued_frame_adapter.clj`: derive `:refuted` only from a valid
  statement-refuted certificate; derive `:unsolved` for other voids. Remove the
  blanket `:invalid` mapping.
- `queued_frame_terminal.clj`: exact outcome/result table, conditional evidence
  requirements, and explicit non-retryability for refuted outcomes.
- `problem_queue_supervisor.clj`: persist refuted completion without advancing
  into a same-problem retry; accept only a separately authorized append-only
  queue amendment for a correction.
- `frame_specification.clj`: validate supersession and correction provenance on
  a corrected registration while continuing to pin repository/revision/path/blob.
- `frame_cycle_contract.clj` and `generated_contract.clj`: consume the emitted
  refutation and early-void policy.
- `campaign_trace.clj`: emit schema-v2 terminal disposition and qualified
  evidence, including a canonical executed-phase prefix rather than invented
  receipts for skipped phases.

## Tests and migration

Lean model and qualification tests:

- all four invalid outcome/result pairs are rejected and the five table entries
  above are accepted;
- refuted without evidence, with empty theorem names, or with an empty digest is
  rejected;
- refuted is not bankable, successor-eligible, or retry-eligible;
- an early statement-refuted trace with a canonical prefix is accepted;
- mutations to classification, target digest, qualification digest, study
  digest, prefix order, or terminal ledger digest are rejected;
- an apparatus-invalidated void with `unsolved` is accepted and the same trace
  labelled `refuted` is rejected.

Clojure tests:

- `frame_void_test`: evidence matrix per classification and missing explicit
  classification;
- `queued_frame_adapter_test`: statement-refuted maps to `:refuted`; apparatus
  and baseline voids map to `:unsolved`;
- `queued_frame_terminal_test`: the full outcome/result table and evidence
  mutations;
- `problem_queue_supervisor_test`: refuted consumes the old queue item, never
  retries it, and a correction appears only through a content-addressed
  amendment/new plan;
- `campaign_trace_test` and `generated_contract_test`: schema-v2 early void and
  mutation rejection;
- an integration test replays the f45 shape through void, terminal derivation,
  queue retirement, corrected registration, qualification, and f46 minting.

Schema v1 artifacts remain readable only through an explicit v1 decoder. Do not
translate `:invalid` heuristically: old apparatus voids used that value, so it
cannot safely mean `refuted`. Preserve the original artifact and require a
reviewed migration record if an old void is reclassified.

## Implementation boundaries discovered during design

The requested Lean files are split between checkouts: `APMCycleMachine.lean`
and `APMCycleContractEmitter.lean` are in `/home/joe/code/mathlib4`, while
`APMCampaignTraceChecker.lean` and `APMQualification.lean` are currently in the
`mathlib4-apm-validation` worktree. Before implementation, consolidate the
branch as required by workspace policy so the four explicit Lake targets are
built from one coherent checkout.

Also, `/home/joe/code/mathlib4/.lake/packages` is currently a real directory,
not the required symlink to the canonical package authority. Under the workspace
rules no Lake command may be run there until the canonical workspace lifecycle
repairs it. No Lake command was run while preparing this design, and
`.lake/build/lib/Mathlib.olean` remains absent.

## Recommendation for f45/f46

After implementation and qualification, use the checked f45 refutations and
study to issue a `:statement-refuted` certificate and record f45 as
`refuted / void`. Register the corrected positive-sign target from `apm-lean`
master as a new problem whose provenance names that certificate, then mint f46.
Do not edit f45, relabel it unsolved, or treat its Solver head as retry state.
