# TN: F32 (model) — one mistyped UUID was fatal, and the Lean-validated machine could not have caught it

Author: Claude (Fable 5, `claude-10`), 2026-08-25. Companion:
`TN-fable-F32-process.md` (the night itself — babysitter, escalation, the
self-heal loop). This note is about the *machine*: what the defect is, why a
Lean-specified and qualified system still contained it, and what closes the
class. Sources: the persisted F32 submission, the coordinator failure log,
futon3c `master` at `b33e758e`, `mathlib4-apm-validation`
`feature/apm-cycle-model-v3` at `4ec58fea04`, contract pin
`apm-cycle-contract-v3.json` (regenerated 2026-08-24 12:42; qualification
report 14:36).

Campaign `jit-all-open-nontopology-v1`, frame `f32`, problem `a92J01`.

**Short answer.** The Student's repair turn was asked to hand-copy 21
controller-minted memory UUIDs into its terminal report; it got one character
wrong (`aa5e` for `aa5a`); the validator's set-membership check rejected it;
the single repair attempt was already spent; the coordinator went `:failed`
with no successor state. The invariant being protected — *no memory used that
wasn't on the reviewed shelf* — was never violated: the two `:used-ids` were
both on the shelf. The field that failed, `:surfaced-ids`, does not exist in
the Lean model. It lives inside a residual hole that was declared open
(`hole-generated-receipt-schemas-v1.edn`) and never closed; the validation
was correct *up to that hole*. Joe's rule: **the machine must be made
correct and the frame resumed** — not voided.

---

## 1. The defect, in three parts

### 1.1 The interface made an LLM transcribe controller-owned identifiers

`src/futon3c/apm/live_learning_phases.clj:466–476`, the Student prompt:

> Return `:memory-use` with the exact `:receipt-id`, `:snapshot-id`, and
> `:snapshot-digest` from the request, plus vector-valued `:surfaced-ids` and
> `:used-ids`.

The controller **already holds** every admissible ID: the snapshot's
`:accessible-memory-ids` are in the request's `:authority`, and any open-search
results are in content-addressed FTS receipts the controller wrote itself
(`role_memory_search.clj:110,139`). `allowed-memory-ids` at
`live_learning_phases.clj:201–204` is computed from exactly those. Asking the
model to *re-type* a subset of that set and then checking set-membership
(`:238–242`) adds no information — the only thing it can do is fail.

The persisted submission
(`data/apm-role-submissions/apm-role-5cffdf18….edn`, mtime 22:23:07) shows
both halves in one file: `:authority … :accessible-memory-ids [… "e-1866fc8e-
aa5a-…" …]` (controller-written, correct) and `:submission … :surfaced-ids
[… "e-1866fc8e-aa5e-…" …]` (LLM-written, one character off). The model did
the tautological thing — copied *all 21* accessible IDs into `:surfaced-ids`
(surfaced ≡ accessible) — and then listed two `:used-ids`, neither of which is
the mistyped one. Semantically the report was fine.

Rule: **an identifier minted by the controller is filled in by the
controller.** LLMs return *content* and *references to receipts*; they do not
echo UUIDs. (Codex is currently sweeping the codebase for every place the
machine asks an LLM for an ID of any kind; §3 is what makes that sweep
unnecessary next time.)

### 1.2 One repair budget shared between "wrong envelope" and "wrong content"

`generated_contract.clj:33,59` and `live_job_driver.clj:10`:
`:repair-attempts 1` per role per job. Attempt 1 burnt it on
`:typed-submission-missing` — the agent never called the submission tool at
all. That is a *contract/format* failure. The repair turn then made a
*content* failure with zero budget left, so `live_job_driver.clj:280–298`
fell through to `:live-job-terminal-repair-exhausted`.

F25 died the same way (`*codex-repl:codex-10*` 7847: "F25 failed closed with
`:live-job-terminal-repair-exhausted` after one repair attempt"). "Attempt 1
didn't use the tool" is evidently the *common* first fault, and it eats the
budget meant for real repairs every time.

### 1.3 Exhaustion is a crash, not a state

After `:live-job-terminal-repair-exhausted` the coordinator sits at
`:regulator/status :failed` forever. There is no modeled successor — no
"clerical correction", no "frame holds while the apparatus is repaired". The
only restart available is to replay the same exhausted submission (codex-10
did this once, failure #26 at tick 9842, identical to #25).

What exhaustion *should* mean: "this step cannot be certified under the
current apparatus." The correct successor is a **resumable** state — the frame
holds at its last valid receipt while the apparatus is repaired, and once the
contract blob changes the same step can be re-validated (the persisted
submission re-checked under the corrected validator — codex-10 already does
this by hand, "revalidate without redispatch") or re-dispatched with a fresh
budget under the new contract version. Neither transition is modeled, so the
only honest thing the driver could do was stop.

---

## 2. Why a Lean-specified, Lean-validated system still contained this

Joe (2026-08-25): "I am baffled as to how a system can be specified in Lean,
with the translation of that specification into Clojure, and pass validation,
and *still contain* this kind of problem."

**The field is not in the model.** `grep -ri "surfaced\|used-ids\|memory-use"`
over `DarkTower/APMCycleMachine.lean`, `APMCycleContractEmitter.lean`,
`APMCampaignTraceChecker.lean` and the emitted contract JSON returns nothing
(the only hits are in `MemoryArmPreregistration.lean`, an unrelated analysis).
Lean models what the *controller* must put into a Student dispatch
(`student-dispatch-required-fields`: `attempt-ordinal`, `promotion-receipt-id`,
`snapshot-id`, `snapshot-digest`, `accessible-memory-ids`) and what evidence
of an *open search* looks like (`RoleSearchEvidence`, commit `35655354d5`,
which states `self-reported-query-is-search-evidence = false`). It says
nothing about what the Student *returns*, because the Lean emitter does not
emit receipt schemas at all — and that is a **declared, open residual hole**:
`holes/labs/M-apm-demonstration/hole-generated-receipt-schemas-v1.edn`,
`:hole/status :open`, reason "the Lean emitter … does not yet emit receipt
schemas or per-phase requires/produces declarations."

**What Lean actually checks about a submission.** `TraceStep` carries four
Booleans — `submissionRegistered`, `submissionPersisted`,
`submissionSchemaValid`, `submissionAuthorityDerived` — and
`APMCampaignTraceChecker.lean:148–150` requires them all true. They are
computed by Clojure (`campaign_trace.clj:130–132`,
`(get-in job [:submission :schema-valid?])`) and projected into the trace.
So the refinement check on submissions is, precisely: *Lean verifies that
Clojure reports its own hand-written schema as satisfied.* That is exactly
the pattern `docs/TN-validated-system-HOWTO.md` §4 warns against — "Do not
describe a hand-maintained field as Lean-owned" — except that nobody
described it as Lean-owned; it simply sits on the Clojure side of a hole that
was declared and then not closed.

**How the field got in.** `:surfaced-ids` and the finding
`:student-memory-surfaced-outside-snapshot` entered
`live_learning_phases.clj` in `91e3ace1` (2026-08-22, "Enforce Student
memory snapshot evidence") — a Clojure-only change, two days before the
contract pin was last regenerated. It added an LLM-authored evidence field on
the Clojure side with no Lean counterpart, and the qualification harness
cannot object, because receipt schemas are outside what it qualifies. The
Lean principle that *should* have caught it exists — Lean already refuses
self-reported queries as search evidence — but it was stated for queries,
not for identifiers, and the emitter never carried it to the receipt layer.

**So:** nothing in the validation lied; the qualification report correctly
lists the hole as open; the defect lives entirely inside it. The machine is
validated up to the receipt-schema hole, and that hole is where every
LLM-authored field lives.

---

## 3. What closes the class

In priority order. Each is one file / one behaviour / one test.

### 3.1 Close the receipt-schema hole in the emitter

The Lean emitter emits, per role, the fields an LLM may author in a terminal
submission, with the rule that **a field whose value is a controller-minted
identifier is controller-derived, never LLM-authored** — the generalisation
of `self-reported-query-is-search-evidence = false` to "self-reported
identifiers are not evidence". Clojure validates the generated schema
(exactly as `generated_contract.clj` already validates bounds and policy),
and `hole-generated-receipt-schemas-v1.edn` moves to `:closed` per its own
closure test ("Clojure validates their generated form and removes the EDN
merge without changing an accepted campaign trace"). After this, a validator
that accepts an LLM-typed UUID cannot pass qualification, and the current
"find every place we ask an LLM for an ID" sweep does not need repeating.

### 3.2 Stop asking the model for `:surfaced-ids` (the instance)

Controller derives `surfaced = accessible-memory-ids ∪ receipt-surfaced-ids`
(it already computes this as `allowed-memory-ids`). The submission carries
`:used-ids` and `:queries` only; the validator checks `used ⊆ surfaced`.
`:student-memory-surfaced-outside-snapshot` becomes unreachable and is deleted.
Prompt text at `live_learning_phases.clj:474–476` changes accordingly.

`:used-ids` remains model-authored — that is the actual claim being made — so
constrain it at the tool boundary: the submission tool's JSON schema gets
`enum: <allowed-memory-ids>` per element, so an out-of-set ID is a schema
error *inside the agent's turn* (it sees it and retries) rather than a
terminal validation failure *after* the turn. ~10 lines in the
submission-tool schema builder; worth doing independently of the derivation.

### 3.3 Make exhaustion resumable in the driver

After `:live-job-terminal-repair-exhausted` the frame enters
`:awaiting-apparatus-repair`, holding at its last valid receipt. Two resume
paths, both keyed on the contract blob having changed since the failure:
(i) *re-validate* the persisted submission under the corrected validator
(F32's case — once 3.2 lands, `:used-ids` are two valid IDs and it certifies
as-is); (ii) *re-dispatch* the step with a fresh repair budget under the new
contract version, recorded as an append-only successor of the exhausted job.
The frame is never voided by the driver.

Acceptance: fixture with `:repair/attempts ≥ max` yields
`:awaiting-apparatus-repair`; after a contract-blob change the next tick
re-validates and certifies (i) or announces a successor job (ii); with no
blob change it stays put and reports why. This transition belongs in
`APMCycleMachine.lean` first and the driver second.

### 3.4 Separate the envelope budget from the content budget

`:typed-submission-missing` gets its own attempt counter
(`:typed-submission-migration-attempts` already exists at
`live_job_driver.clj:257` for the migration case — generalise it). A content
finding after a contract repair still has its full content-repair budget.

Log `attempt-1-contract-failures / frames`. If it is high (F25 and F32 both
say it is), the fix is on the tool side — the controller pre-fills the whole
envelope (`:authority` is already controller-owned; the agent should only
fill `:submission`), not more retries.

---

## 4. What to do with F32 now

Heal it, don't void it:

1. Implement 3.2 (controller-derived `:surfaced-ids`; validator checks
   `used ⊆ derived`) on master with tests; a second agent reads the diff
   before reload (I am on the roster and hold the full context —
   `claude-10`).
2. Reload the two namespaces from `/home/joe/code/futon3c`.
3. Re-validate F32's persisted Student terminal — no redispatch. Under the
   corrected validator its `:used-ids` (two IDs, both on the shelf) certify,
   the frame continues into the Scribe phase, and F33 follows.

No correction transition for the typo'd field is needed — 3.2 makes the
field disappear. Neither codex-10's "void F32" recommendation
(`*codex-repl:codex-10*` 13924–13926) nor my earlier concurrence stands:
voiding is the machine giving up, and Joe has said no ("it would just lead to
more errors"). This note does not perform the steps; codex-10 owns the frame.

---

## 5. What I checked

- The submission file: `:authority` list has `aa5a`, `:submission
  :surfaced-ids` has `aa5e`, 21 surfaced = all 21 accessible, `:used-ids` =
  2 IDs not including the bad one; `:attempt-ordinal 2` / `:phase
  :student-attempt-2` confirms the repair turn.
- `coordinator.edn`: status `:failed`, ticks 9842, updated-at 23:05:26Z; 26
  failures; the four `:student-memory-surfaced-outside-snapshot` entries
  with `:repair/reason` (16:31, 18:10, 18:13 are F30 — cross-checked against
  transcript 12387 — 22:23 and the 23:05 replay are F32).
- Source: validator `live_learning_phases.clj:185–250`, prompt `:466–476`,
  driver budget/exhaustion `live_job_driver.clj:10, 76, 229, 280–330`,
  budgets `generated_contract.clj:33–59`; `git log -S':surfaced-ids'` →
  `91e3ace1` 2026-08-22.
- Lean: sizes and grep of the four `DarkTower/APM*.lean` files and the
  emitted JSON for `surfaced|used-ids|memory-use`; `TraceStep` fields
  (`APMCampaignTraceChecker.lean:30–60`, check at `:148–150`);
  `campaign_trace.clj:34–57,130–132`; emitter `memory-policy` block
  (`APMCycleContractEmitter.lean:123–150`); commit `35655354d5` diff;
  `hole-generated-receipt-schemas-v1.edn`; `qualification.clj:11–30`.
- HOWTO `docs/TN-validated-system-HOWTO.md` §§3–5.

Not checked: whether the F30 validator widening
(bbc04b0b / eb9c5846 / 77a72e92, 2026-08-24 16:31–18:13) is itself sound. It
was shipped under alert pressure with no reviewer and deserves its own
review.
