# TN-refuted-statement-disposition — minimal path past a false registered target

Codex-2, 2026-08-26. Revised to Joe's scope: the objective is to solve the
corrected problem, not to build a refutation archive.

## Decision

Add `refuted` as a third `ProblemOutcome`. Its only valid frame result is
`frameVoid`:

| problem outcome | frame closed | frame partial | frame void |
|---|---:|---:|---:|
| solved | yes | yes | no |
| unsolved | no | yes | yes |
| refuted | no | no | yes |

This expresses the one distinction the machine currently lacks:

- `unsolved / framePartial`: further work on the same registered statement may
  succeed, so its retained Solver head may be retried;
- `unsolved / frameVoid`: the frame cannot continue because its apparatus or
  baseline is unusable; this says nothing about the proposition;
- `refuted / frameVoid`: the registered statement must not receive more Solver
  work;
- `solved / framePartial`: the proof remains bankable even when the experimental
  trailer is incomplete.

`refuted` is a control outcome, not a new mathematical deliverable. The existing
diagnosis in `problems/a96J08/problem.md` is sufficient. Do not introduce a
refutation certificate type, study ledger, theorem-name list, axiom-audit digest,
supersession link, or correction-basis link.

The controller's reviewed disposition selects `:statement-refuted`. A Solver
error alone must not select it automatically, but the cycle machine need only
validate the typed disposition and ordinary content-addressed void event. It is
not responsible for formalizing the diagnostic.

## Void means f45 never happened

Voiding f45 removes it from campaign frame history. It does not append a failed
terminal frame, leave a retryable Solver head, or create an old problem id to
supersede. `a96J08` remains the queue slot's logical problem id. The corrected
positive-sign formulation already on `apm-lean` master is re-minted as f46 under
that same id.

`refuted / frameVoid` is the disposition used while applying the void
transition. The resulting campaign projection contains no f45. The append-only
event log may retain the fact that a void transition occurred, because replay
needs it, but frame enumeration and experimental results must behave as though
f45 never happened.

## Lean changes

In `APMCycleMachine.lean`:

1. add `ProblemOutcome.refuted`;
2. add only `validOutcome .refuted .frameVoid => True`;
3. prove a refuted outcome is neither bankable, successor-eligible, nor
   retry-eligible;
4. retain `unsolved / frameVoid` for apparatus and baseline voids.

No additional evidence field belongs in `TerminalOutcome`.

In `APMCycleContractEmitter.lean`, publish the minimum policy needed by the
Clojure consumer:

```text
problem-outcomes = [solved, unsolved, refuted]
refuted-requires-frame-void = true
refuted-retry-same-problem = false
statement-refuted-void-outcome = refuted
non-refutation-void-outcome = unsolved
early-void-canonical-prefix = true
voided-slot-reuses-logical-problem-id = true
corrected-pins-require-plan-revision = true
```

`APMQualification.lean` should contain non-vacuity and mutation witnesses for
the new table row and its non-retryability.

## Early-void trace

`APMCampaignTraceChecker.accepts` currently requires all eleven canonical phase
edges and receipts, memory/review/analyst completion, and `closed = true`. A
mid-cycle void therefore cannot produce an accepted trace.

Add a second accepted terminal shape:

1. **ordinary completion** retains the current full-cycle checks;
2. **early void** contains the canonical phase prefix actually executed plus a
   content-addressed void event.

The early-void branch still requires ledger continuity, receipt continuity for
executed phases, valid dispatch evidence for executed jobs, campaign isolation,
the typed problem/frame outcome pair, and terminal digest binding. It must not
invent receipts, memory observations, review passes, or analyst wakes for phases
that did not execute.

Represent disposition explicitly, for example:

```text
terminalDisposition: "closed" | "void"
problemOutcome:      "solved" | "unsolved" | "refuted"
frameResult:         "closed" | "partial" | "void"
voidClassification:  null | "known-failing-baseline" |
                            "apparatus-invalidated" |
                            "statement-refuted"
```

The checker should decode these strings to the Lean types and use the model's
truth table. Classification adds the two necessary consistency checks:
`statement-refuted` pairs with `refuted`, while the other void classifications
pair with `unsolved`. No refutation-evidence payload is required.

## Clojure transition

### Dispose the active frame

Wire the reviewed controller disposition to `frame_void/prepare` and
`frame_void/void!`. `frame_void/prepare` already constructs the obligation and
void event but currently has no source caller.

For `:statement-refuted`, terminal derivation produces
`:problem/outcome :refuted` and `:frame/result :void`. For
`:known-failing-baseline` and `:apparatus-invalidated`, it produces
`:problem/outcome :unsolved` and `:frame/result :void`.

Remove Clojure's blanket `:invalid` outcome. It currently labels every void as
invalid, including apparatus failures, and has no corresponding Lean value.

Applying the void must:

1. validate the active frame/problem identity and ledger version/digest;
2. append the existing void obligation and stopped event atomically;
3. validate the `refuted / void` disposition;
4. clear f45 without adding it to `:completed`;
5. restore the queue cursor to the `a96J08` slot;
6. block minting until that slot's revised pins have been installed and
   qualified.

The review decision is the authority for the classification. Do not infer
refutation directly from `:solver-defect-review-required` or another error code.

### Recast the same queue slot

The queue plan id hashes its problem vector, so the corrected blob cannot be
substituted inside the existing plan. A bare cursor decrement would simply mint
the refuted pins again. Appending another `a96J08` item would incorrectly leave
two logical occurrences of a problem whose first frame was void.

Add a narrow plan-revision transition that:

- is available only immediately after a void restored the current slot;
- replaces that slot's repository/revision/path/blob pins while preserving
  `:problem/id "a96J08"` and its index;
- produces a new content-addressed plan id and matching queue state;
- qualifies the replacement pins before minting;
- mints f46 through the ordinary `prepare-next` path.

The plan revision needs no refutation or correction metadata. Its authority is
the reviewed void disposition plus the replacement problem entry supplied by
Ground Control.

The complete path is:

```text
reviewed :statement-refuted disposition
  -> early-void event
  -> f45 removed from campaign projection
  -> a96J08 slot restored
  -> plan revised with corrected a96J08 pins
  -> corrected slot qualified
  -> f46 minted with problem id a96J08
```

If plan revision or qualification fails, the queue remains blocked at the
restored slot and no f46 is minted. Replaying each persisted step is idempotent
through the existing content digests and compare-and-append boundaries.

## Clojure ownership

- `campaign_machine.clj`: accept and project the void event without retaining a
  campaign frame result.
- `frame_void.clj`: keep the three explicit classifications and expose the
  existing preparation through the reviewed disposition path.
- `queued_frame_adapter.clj`: map statement-refuted void to `:refuted` and the
  other void classifications to `:unsolved`; remove `:invalid`.
- `queued_frame_terminal.clj`: enforce the outcome/result table and ensure
  refuted is never retryable.
- `problem_queue_supervisor.clj`: clear the void frame, restore its slot, and
  accept the narrow content-addressed plan revision before reminting.
- `frame_specification.clj`: validate the replacement pins under the unchanged
  problem id.
- `frame_cycle_contract.clj` and `generated_contract.clj`: consume the emitted
  minimal terminal policy.
- `campaign_trace.clj`: emit the early-void prefix and classification without
  synthetic later-phase data.

## Tests

Lean:

- accept the five valid outcome/result pairs and reject the other four;
- prove refuted is not bankable, successor-eligible, or retry-eligible;
- accept a canonical early statement-refuted void trace;
- reject wrong prefix order, digest discontinuity, `refuted / partial`, and an
  apparatus void labelled refuted.

Clojure:

- `frame_void_test`: all three classifications reach the ordinary void event;
- `queued_frame_adapter_test`: statement refutation maps to `:refuted`, other
  voids to `:unsolved`;
- `queued_frame_terminal_test`: enforce the complete table;
- `problem_queue_supervisor_test`: f45 disappears, the old plan cannot remint,
  the revised plan preserves slot/index/problem id, and f46 can be prepared;
- `campaign_trace_test` and `generated_contract_test`: accept the early-void
  shape and reject policy mutations;
- integration: disposition through f46 mint, with `a96J08` unchanged and f45
  absent from campaign frame enumeration.

## Implementation prerequisite

The mixed-source dependency was removed on 2026-08-26. Commit `be9a6efe02` merges
the validation branch into `/home/joe/code/mathlib4`; that canonical checkout
now owns the cycle machine, contract emitter, campaign trace checker,
qualification model, and trace-fixture script together. Builds must use those
files from that one checkout, never a mixture of worktree paths.

Also, `/home/joe/code/mathlib4/.lake/packages` is a real directory rather than
the required symlink to the canonical package authority. Workspace policy
forbids running Lake there until the canonical lifecycle repairs it. No Lake
command was run during this design revision, and
`.lake/build/lib/Mathlib.olean` remains absent.
