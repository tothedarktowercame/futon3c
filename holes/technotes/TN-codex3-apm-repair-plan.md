# TN-codex3-apm-repair-plan — repair the boundary around the cycle

Codex-3, 2026-08-27. Plan only. This is based on
`TN-opus-f48-critical-findings.md`, `TN-opus-f47-observation.md`, and inspection
of the current launch, regulator, job-driver, typed-authority, search, and
durable-coordinator paths. The linked Claude artifact was not available as
machine-readable content from this session; the 25-code/86-occurrence totals
and the interleaving conclusions below therefore use the technote's account,
not an independent recount of that artifact.

## Recommendation

Do not add another theorem about the current `shelf`, and do not begin by
turning all 25 live error codes into Lean propositions. First make four
boundaries explicit and executable:

1. one controller-owned access decision in front of every way a role can
   obtain a memory;
2. one launch preflight that exercises the installed apparatus, not merely its
   source representation;
3. one append-only attempt/event history from which mutable projections are
   rebuilt;
4. one durable coordinator state machine whose status includes scheduled,
   enabled, stopping, and in-flight facts.

Then extend the Lean model over the same declared carriers and transitions.
The model did what it was asked to do. The failure was that neither the set of
memory carriers nor the launch/runtime wrapper was part of its subject. The
repair must make that subject mechanically enumerable; otherwise a future
channel can again exist outside both proof and enforcement.

The ordering matters. Access prevention and durable evidence stop the two
most damaging failure modes quickly. Preflight then prevents known apparatus
faults from spending a frame. Operational truth comes next because it changes
the coordinator lifecycle and shutdown semantics. Formal expansion follows
the executable inventories, so Lean proves the architecture that actually
exists rather than another hand-selected subset.

## 1. Establish the inventory and the failure taxonomy

**Cost: cheap. Build first, in parallel with the narrow access gate design.**

Create a generated machine inventory with three finite registries:

- **effectful entry points:** frame mint, role dispatch, memory read, terminal
  collection, repair/retry, advance, stop, recover, and resume;
- **memory-serving carriers:** shelf snapshot, search store, cascade/pattern
  expansion, inherited repair results, prompt embedding, and any future
  retrieval or cache path;
- **durable records and projections:** coordinator registry/state, frame
  ledger, attempt history, role-submission authority, search receipts,
  certificates, job ledger references, and human-facing status projections.

Every registered entry names its precondition/enforcement function, authority
fields consumed, durable evidence emitted, and terminal error vocabulary. CI
must fail when a memory-serving implementation or an effectful frame entry
exists without a registry entry. Prefer registration at construction (a
channel cannot be instantiated without an access-gate capability) over a
source-text grep; static scans are useful only as a second check for
unregistered call sites.

Reclassify the 86 recorded failures by **boundary and disposition**, retaining
the original 25 codes:

- domain rejection: a requested cycle transition was invalid;
- apparatus precondition failure: launch/configuration/source/runtime contract
  was invalid before work;
- dependency/transport failure: an external operation did not complete;
- implementation exception: a `Throwable` crossed an owned boundary;
- persistence/publication failure: an intended durable transition did not
  commit or did not become visible;
- recovery/reconciliation failure: durable and runtime facts disagreed;
- evidence-integrity failure: history was absent, mutable, or unverifiable.

For every occurrence record stage, action attempted, side effects already
committed, retryability, operator action, and whether a student saw apparatus
output. This is the right treatment for the unmodelled majority initially:
make it an operational assurance corpus with explicit fault injection and
recovery obligations. A theorem is appropriate later only where a code
corresponds to a deterministic state transition. An escaped exception is not
made safe by proving a proposition named after it.

Deliverables and gates:

- all 25 codes classified, with the 86 occurrences reconciling to the source
  data;
- no `:unknown` bucket allowed at campaign start;
- each class has an owner, retry policy, evidence requirement, and campaign
  admission consequence;
- new error codes cannot land without classification and a test at the owning
  boundary.

## 2. Replace channel-local holdout logic with one access decision

**Cost: medium. Build first.**

Define the policy over the universe of candidate memories, not over the shelf:

`may-serve?(job-authority, memory-metadata, channel, purpose) -> decision`

For `:same-problem`, the decision compares the current problem with depositor
truth attached to the memory itself. Shelf membership may explain how a
candidate was found, but it cannot define whether it is withheld. A memory
from an incomplete prior frame, as in f47, must be decidable in exactly the
same way as a shelved memory. Missing or unverifiable depositor/problem
metadata fails closed for a holdout attempt.

The gate returns both allowed candidates and signed/content-addressed decision
evidence: policy/version, authority digest, candidate ids, excluded ids and
reasons, and channel. Search, cascade, shelf materialisation, inherited repair
results, and prompt assembly must call it before bytes or substantive memory
content cross the role boundary. Terminal validation independently recomputes
or verifies the decision from receipts; it remains detection of gate or
evidence corruption, not the primary enforcement point.

Enforcement cannot always precede the physical action it describes. Outcomes,
student claims about use, compiler results, and external service responses are
known only afterward. For those, constrain the **next irreversible action**:
validate a terminal before certification, promotion, advance, or publication.
For reads, launches, dispatches, workspace mutation, and frame minting, there
is no such excuse: validate before performing the action.

Authority propagation should be schema-derived. The defect was possible
because `authority-fields` was an independent allow-list. Generate request,
persisted authority, channel capability, receipt, and validator schemas from
one contract, and add round-trip tests asserting that every field read by an
enforcement function survives each hop unchanged. A field allow-list can
remain an implementation detail only if completeness is generated and tested,
not manually remembered.

Tests required before campaign use:

- one withheld and one allowed memory injected through **every registered
  carrier**, including a store-only memory from an incomplete frame;
- mixed-channel and duplicate-id tests proving union/order/truncation/cache
  behaviour cannot reintroduce an excluded id;
- missing and forged provenance, missing policy, stale authority, repair
  inheritance, and newly registered channel tests;
- a final prompt-boundary test proving withheld content, not merely its id, is
  absent from what the role receives;
- property tests comparing gate decisions across all carriers for the same
  authority and memory metadata.

This critiques proposed instrument check 1: hand-testing each channel is a
valuable smoke test, but the durable assurance is a mandatory common gate plus
a registry completeness test. A list of three currently known channels will
age exactly as `authority-fields` did.

## 3. Make attempt and transition evidence append-only

**Cost: medium. Build immediately after, or alongside, the access gate.**

Treat `live/student-attempt-N.edn` as a projection, never as the authority.
Before any repair, retry, migration, resume, or redispatch, append an immutable
event that contains or content-addresses:

- the request authority and ticket;
- terminal job and typed submission;
- collection evidence and all channel receipts;
- validation result and findings;
- disclosure/failure account;
- disposition (`accepted`, `superseded`, `voided`, `apparatus-invalidated`,
  etc.), actor, reason, and successor attempt/job id.

Persist the predecessor event before announcing the successor. The successor
references it; the mutable live file is atomically rebuilt from the event log.
If archival fails, redispatch must not occur. This ordering prevents a repair
from destroying the evidence it repairs and also avoids a successor existing
without a durable predecessor disposition.

`b642640a` is a correct local repair but not evidence that the class is closed.
It retains data inside the generic live-job state. The audit must include
`live_solver_rounds`, typed-contract migration, transport retries, supervisor
recovery, claim recovery, frame voiding, manual repair/resume, job-ledger
expiry, and every write using an attempt-number path. Search for the semantic
operations, not only `terminal-repair`: overwrite and compaction can destroy
evidence without using that name.

Required tests:

- fault injection at every boundary between archive, projection, announce,
  register, activate, accept, and projection update;
- replay/rebuild yields the same live state and certificate references;
- no event can be mutated or silently orphaned by retention/compaction;
- repeated repair and typed migration preserve the full predecessor chain;
- a scan verifies every terminal job id is either live or durably disposed.

Historical `data/*` should not be the only authority if it is gitignored and
subject to ledger expiry. Choose a backed-up durable store with explicit
retention and integrity verification. Git is not required, but expiry must be
a proved/exported lifecycle transition rather than garbage collection that
silently removes the last copy.

## 4. Build launch preflight as an admission transaction

**Cost: medium for the first probes; expensive to make hermetic across all
dependencies. Build the narrow version before restarting a campaign.**

Preflight runs once against the exact installed runtime/configuration that will
mint and drive the frame. It produces a durable, content-addressed admission
certificate bound to source revisions, loaded namespace digests, generated
contract digest, runtime identity, registry digest, and probe version. Frame
mint and first dispatch require a fresh matching certificate. A source change,
reload, registry change, or contract change invalidates it.

The first probe set should be:

1. the carrier-completeness and withheld-memory canary from section 2;
2. source/runtime identity for the APM namespaces actually invoked;
3. generated-contract round trip: Lean emitter output parses as the Clojure
   validator's input and both agree on a pinned positive and negative corpus;
4. a lifecycle canary: start a disposable coordinator, observe one in-flight
   tick, request drain-stop, prove quiescence, and prove recovery does not
   restart a disabled entry;
5. launch-path dry run through audit, frame mint intent, initial durable state,
   and first-tick validation without publishing a real frame.

The technote's proposed checks 2 and 3 are directionally right but underspecified.
“Namespaces match committed source” must account for a dirty canonical
checkout and deliberate live reload: compare the loaded definitions/resources
to the exact declared runtime revision/digests, not merely repository HEAD.
Contract agreement needs semantic positive/negative fixtures, not textual
schema equality. Proposed check 4 is not merely an instrument check; truthful
running status requires the lifecycle redesign in section 5. The preflight
should test that design, not substitute for it.

Preflight failure is an apparatus failure and must prevent mint/dispatch. Store
its full evidence outside any frame attempt; do not create a student-shaped
failure for a frame that should never have existed.

## 5. Give the coordinator one durable lifecycle and truthful status

**Cost: expensive and migration-sensitive. Design after sections 1–3; implement
before unattended campaign recovery resumes.**

Replace the ambiguous combination of registry enablement, scheduler table, and
`:regulator/status :running` with a durable lifecycle such as:

`disabled -> starting -> idle -> ticking -> draining -> disabled`

with terminal `failed`/`complete` dispositions and an epoch/lease for each
run. A tick durably claims `(coordinator-id, epoch, tick-id)` before effects,
heartbeats or records its start, and durably completes/fails it afterward.
Single-flight is enforced by that claim, not inferred from one JVM's runner
map. Recovery reconciles leases and explicit durable intent; it never treats
“registered” or stale `running` as permission to start.

Expose one status operation that derives and labels:

- desired state (enabled/disabled/draining);
- scheduler state and epoch;
- current tick id, start time, heartbeat/lease expiry, and last committed
  effect boundary;
- active role jobs;
- durable regulator state and last failure;
- reconciliation verdict (`consistent`, `stale-lease`, `runner-missing`,
  `unexpected-runner`, etc.).

Do not collapse these facts to one boolean. “Are you running?” should return a
summary (`quiescent`, `scheduled-idle`, `tick-in-flight`, `draining`, `failed`,
or `inconsistent`) plus the facts that justify it.

Unify or rename stop APIs. The ordinary operator stop must durably disable
first, prevent new claims, cancel future scheduling, wait for or explicitly
time out the current tick, and return a quiescence witness. A non-blocking
variant may return `draining` plus the tick id, but must not say `stopped`.
Process-local scheduler cancellation should be private or named as such. A
hard abort is a separate operation with explicit partial-effect evidence and
reconciliation requirements.

Test with deterministic barriers inside every tick stage: stop before claim,
after claim, after frame-mint persistence, after dispatch, and before tick
completion; recover concurrently; simulate JVM death and stale leases. Assert
no post-quiescence effect, no duplicate tick effect, no disabled recovery, and
truthful status at each barrier.

## 6. Move launch and exception behaviour into an executable wrapper model

**Cost: medium for Clojure transition tests; expensive for Lean refinement.
Begin after the lifecycle vocabulary is fixed.**

The 13 `live-supervisor-launch-audit-failed` occurrences and 13
`live-regulator-tick-threw` occurrences deserve first-class transition tests
now:

- launch audit has a typed result and runs before mint/dispatch;
- each launch step has a commit point and compensating/reconciliation rule;
- every owned callback boundary catches and classifies exceptions with stage,
  input digest, committed-effects summary, and causality;
- an exception transitions durably to `failed` exactly once and cannot be
  auto-rearmed without an evidenced repair/resume;
- persistence failure while recording failure is separately surfaced and
  recoverable, rather than replacing the original cause.

Avoid catching `Throwable` as a complete recovery policy. It is acceptable as
the outer containment boundary, but fatal VM errors/interruption/cancellation
need explicit treatment, and the result must retain the causal chain. The
current `live-regulator-tick-threw` class is too coarse for assurance even
though it prevents an exception from escaping the scheduled task.

Use state-machine/model-based tests with generated fault schedules across
launch and tick. This provides more value for these 26 observations than 26
shallow unit tests keyed to error strings.

## 7. Expand the Lean model only from declared carriers and transitions

**Cost: expensive. Start the interface design early; prove it after executable
registries and lifecycle semantics stabilise.**

Change the holdout statement from a shelf theorem to an access theorem:

- finite `Carrier` type (or generated carrier identifiers);
- `Candidate carrier memory`/retrievability relation;
- depositor-truth predicate independent of shelf membership;
- `Served job carrier memory` only through an authorised gate;
- theorem: same-problem holdout implies no served memory has the held subject,
  for every registered carrier;
- coverage obligation: every live serving adapter refines one `Carrier`, and
  the generated live carrier set equals the model carrier set.

Apply the same audit pattern to every existing theorem: list each theorem's
carrier (shelf entries, attempt records, ledger transitions, certificates,
etc.), list the corresponding live producers/consumers, and flag a theorem
whose noun has multiple live carriers but only one modelled instance. This is
how to find scope gaps before a breach. The result should be generated as a
coverage matrix in CI, not maintained as prose.

Then model the coordinator wrapper at the granularity of committed effects:
admission, mint intent/commit, tick claim, dispatch intent/commit, terminal
observation, advance, drain, and recovery. Prove safety properties such as no
effect without admission, at most one active tick per epoch, disabled implies
no new tick claim, quiescence witness implies no in-flight tick, and every
committed attempt has a durable disposition. Do not try to prove that networks,
filesystems, or callbacks do not fail; prove that any failure leaves a
classifiable durable state from which recovery is defined.

The generator must reject drift among Lean declarations, generated Clojure
contracts, registries, and adapters. A theorem count is not a coverage metric;
the useful measures are declared-carrier coverage, transition coverage, and
refinement-test coverage.

## 8. Attribute apparatus breaches without weakening outcome rules

**Cost: cheap once append-only events and admission certificates exist.**

The machine should distinguish:

- `student-fault`: the apparatus enforced the policy and the submitted action
  violated a role-owned rule;
- `apparatus-caused`: the apparatus served/embedded forbidden content or
  violated its own authority before the student could act;
- `mixed`: an apparatus breach occurred and a later independent student action
  also violated a rule;
- `indeterminate`: required evidence is missing or contradictory.

Classification is evidence-derived from access decisions, prompt boundary,
authority, disclosure, and timestamps; it is not inferred from whether the
student admitted the breach. A student's disclosure is preserved as evidence
but neither creates nor cures apparatus causation.

Joe's decision to void f46/f48 need not change. The consequence is instead:

- apparatus-caused and indeterminate attempts are never training/evaluation
  evidence about student compliance or capability;
- their cost and failure count accrue to apparatus reliability;
- retry does not consume a student attempt budget or produce adverse
  student-quality evidence, unless policy explicitly says otherwise;
- original work remains durably inspectable even when the outcome is void;
- disclosure is measured separately and cannot make acceptance less likely
  than silence under otherwise identical apparatus facts.

This removes the incentive to conceal while preserving the disposition that a
contaminated attempt does not count.

## Campaign restart gates and sequence

### Gate A — before any new student frame (cheap/medium)

1. Reconcile and classify the 86-failure corpus.
2. Land the registered carrier inventory and common pre-read policy gate.
3. Pass all-carrier canaries, including store-only same-problem memory.
4. Land append-before-repair history and audit every overwrite/retry path.
5. Run preflight probes 1–3 and the launch dry run; persist an admission
   certificate.

Manual operation is acceptable only if status is reported conservatively and
durable stop is used. It is not acceptable to describe an empty runner table
as quiescence.

### Gate B — before unattended recovery/restart (expensive)

6. Land the durable tick claim, drain-stop, quiescence witness, and reconciled
   status API.
7. Pass stop/recover/JVM-death fault schedules and preflight lifecycle canary.
8. Require evidenced repair/resume for failed epochs.

### Gate C — before claiming formal assurance

9. Generate the carrier/transition coverage matrix.
10. Generalise the holdout theorem over served memories and all carriers.
11. Add wrapper safety theorems and adapter refinement tests.
12. Report coverage by live carrier and effectful transition, alongside theorem
    count.

## What not to do

- Do not add `:shelf/withheld-ids` independently to each future channel. That
  repeats the omission mechanism.
- Do not rely on terminal validation to protect confidentiality or experimental
  integrity. It detects a read after the read occurred.
- Do not call a larger source grep a carrier proof. Require construction
  through registered capabilities and use scans only to catch bypasses.
- Do not treat a mutable attempt file, a transient Agency ledger, or a
  gitignored recovery copy as the sole historical record.
- Do not make `stop!` “work” by clearing only one of registry, runner, or state.
  Lifecycle transitions must update coherent durable intent and reconcile the
  runtime.
- Do not retry `tick-threw` or launch-audit failure blindly. Retryability is a
  property of the stage and committed effects, not the outer error code.
- Do not mirror every operational failure in Lean. Model the deterministic
  transition and containment obligations; test real dependency failures with
  fault injection.
- Do not weaken the holdout to shelf membership because f47's cross-channel
  transfer was useful. If attempt 1 is intended as a same-problem holdout, make
  it true everywhere; measure useful transfer with a separate, channel-neutral
  metric.
- Do not preserve the existing tier-A condition-3 metric as the sole account of
  transfer. It asks specifically about shelf traffic while observed useful
  traffic also uses search and cascade. Keep it if shelf efficacy is the
  question; add a channel-neutral transfer measure if learning transfer is the
  question.

## Where the diagnosis needs qualification

The critical technote is right that no modelled theorem was violated and that
scope, timing, evidence durability, and operational truth are the missing
classes. Three qualifications matter:

1. The leak is broader than “the search channel lacked withheld ids.” The
   policy's domain was shelf-derived, so even perfect propagation could not
   exclude f47's store-only same-problem memory. The permanent repair is
   depositor-truth at a common serve boundary.
2. “Every invariant is checked when a terminal is collected” is too broad.
   Launch audit and some provider/authority checks already occur earlier. The
   actionable distinction is whether each invariant is checked before its
   first irreversible or information-revealing effect. The inventory should
   measure that directly.
3. The four proposed preflight checks are necessary probes, not the assurance
   architecture. Checks 1–3 must be bound to a runtime admission certificate;
   check 4 depends on a durable lifecycle. Running them as an unbound script
   would detect today's defects while permitting tomorrow's drift between the
   probe and the frame.

The shelf-utility observation should likewise be retained with its corrected
denominator. It shows that the shelf is not currently where most demonstrated
transfer occurs; it does not show that memory retrieval is ineffective. Holdout
assurance and transfer measurement should therefore be separated: one is a
universal access restriction, the other an empirical channel-neutral outcome
measure.

## Completion criteria

This repair is complete when a fresh channel cannot serve a memory without
declaring itself and passing the common policy gate; a frame cannot mint
without a runtime-bound admission certificate; every attempted and superseded
terminal remains reconstructible; stop returns either an evidenced in-flight
state or a quiescence witness; recovery cannot contradict durable disablement;
all 25 failure codes have tested containment/recovery semantics; and the Lean
coverage report ranges over the same carriers and committed transitions as the
running machine.
