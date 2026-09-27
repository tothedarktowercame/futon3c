# M-inbox-zero-claim-lifecycle

**Status:** DERIVE pending; MAP complete (2026-09-27)
**Owner:** codex-5 (initial author); claude-8 is the requesting discovery owner/reviewer.
**Repos:** futon3c, futon3/inbox-zero-lib; operational state in storage/inbox-zero.
**Lifecycle:** [Futonic Mission Lifecycle](../../../futon4/holes/mission-lifecycle.md)

## 1. IDENTIFY

### Motivation and operator anchor

Joe, 2026-09-27: “Inbox zero needs some further sorting out,” followed by
“2-3 engineering days sounds heavy, can we make a misison about this so we
can keep track of progress?” This mission records that work and makes the
remaining decisions and evidence visible. The estimate is provisional, not
an agreed implementation size or a requirement to build a new subsystem.

Inbox-zero can commit one seat's unfinished edits under another seat's name.
An active claim on a shared path survives its original edit and is later used
as authority for unrelated working-tree content. Discovery found three such
commits: futon3c `58c3dd7c`, `685d211d`, and `e347e724` took claude-8's shell
edits during claude-10's promotion. The Git author was Joseph Corneli; the
incorrect seat attribution appears in promotion messages and derived links.

### Principle and prior work

The authority to promote an edit must be supported by evidence about that
edit. A historical association with a filename is insufficient. This is
already stated in [inbox-zero/promote-at-turn-end](../../../futon3/library/inbox-zero/promote-at-turn-end.flexiarg):
promote what the ending turn touched, exempt another seat's live work, and
record holds. The existing dirty-set projection also recognizes that a clean
observation invalidates an earlier claim for later dirt; promotion does not
apply that rule.

### Scope

In scope: claim creation and ending, eligibility shared by promotion and
attribution views, execution-time protection against changed content,
observable refusal/release outcomes, and conservative handling of existing
claims. Include the minimum watcher diagnosis needed to establish that the
already-published release was ingested. Preserve automatic promotion where
its authorship and completion evidence are adequate.

Deferred: a general inbox-zero redesign, full instrumentation of every shell
editor, bulk claim expiry without a reviewed policy, rewriting old commits,
and retrospective correction of the entire attribution history. Unsupported
edit paths must have an explicit outcome in this mission even if instrumentation
is deferred. The six audited commits remain evidence, not commits to rewrite.

### Completion criteria

- [ ] **C1 — Cross-seat safety:** In a real temporary Git repo, A has a claim,
  B edits the file and leaves it dirty, and A's turn ends. No commit is made
  for A; B's bytes and HEAD remain unchanged; a typed reason is recorded.
- [ ] **C2 — Execution race:** Repeat with B editing during a promotion gate
  or after planning. The bytes committed, if any, must be exactly those for
  which the design established authority. Cover deletions and file modes.
- [ ] **C3 — Lifecycle:** A completed/consumed edit cannot authorize later
  dirt. Repeated turn-end delivery and delayed witness ingestion cannot
  resurrect that authority. Existing unqualified claims fail safely.
- [ ] **C4 — Useful positive case:** A's own eligible completed edit still
  promotes behind the existing gates, with correct attribution and push policy.
- [ ] **C5 — Coherent views:** Promotion, dirty-set projection, and new
  session-commit links agree on applicable claim authority; a derived link
  is not presented as independent proof of authorship.
- [x] **C6 — Operational closure:** Read back the released successor for
  `claim:8c7eb3879d22…` through the current-claim projection. Document any
  remaining intake delay and its effect on safety. Publication alone is not closure.
- [ ] **C7 — Review and discovery:** Relevant checks pass, independent review
  is recorded, and the operating instructions explain holds, release, and
  unsupported editing surfaces. Link them from the existing inbox-zero docs.

- [ ] **C8 — Relevant, honest notifications (Joe, 2026-09-27):** A dirty
  file whose mtime merely overlaps an unrelated agent's activity must not be
  presented to that agent as its work or assigned for commit/delete cleanup.
  Uncertain ownership must stay visible through a documented triage route,
  with evidence type and uncertainty explicit. Cover simultaneous agents,
  synthetic operator windows, session rollover and notification deduplication;
  do not hide the repository backlog or treat ambiguous overlap as authorship.

### Relationships, evidence and responsibility

The existing inbox-zero implementation is the dependency being repaired;
[M-autoclock-in](M-autoclock-in.md) supplies mission attribution context but
is not evidence that a particular edit belongs to a seat. The mission enables
safe shared-checkout turn-end promotion.

Primary evidence: discovery commit **f21f838f** and its
[report](../technotes/TN-inbox-zero-claims-d-2026-09-27.md),
[652 current stale claims](../technotes/inbox-zero-claims-d-current-active-2026-09-27.tsv),
and [1,480 historical active records older than 12h](../technotes/inbox-zero-claims-d-history-active-2026-09-27.tsv).
These counts are dated snapshots, not current live measurements. The report
contains source sites, exact claim/seat identities, session evidence and limits.

Joe requested the mission after reviewing the discovery summary. codex-5 owns
this initial tracking document; implementation packets should name their author
and independent reviewer when dispatched. No new dispatch is implied here.

**IDENTIFY exit: Met.** Joe confirmed the gap warrants a tracked mission;
this scope carries forward the discovery and the required cross-seat bad case.
HEAD is omitted because the gap and incident evidence are already concrete.

## 2. MAP

| Ready (no new code needed) | Missing or unresolved |
|---|---|
| Immutable claim records and active/released/superseded schema states | Automatic, evidence-backed end of promotion authority |
| Validated `watcher/write-witness!` and single-writer intake | Confirmed ingestion of the specific release; explain slow intake |
| Current-claim projection by worktree/path/seat | One eligibility rule consumed consistently by all relevant views |
| Dirty-set clean-after-claim exclusion | Protection when no clean observation occurred between two edits |
| Empty-index, status-class, gate and staged-path checks | Evidence that the staged content belongs to the ending turn |
| Successful Edit/Write/MultiEdit witnesses | Explicit treatment of shell edits and mixed preexisting dirt |
| Real-Git executor tests and turn-promotion test seams | Regression reproducing the exact A-claim/B-edit failure |

### Survey questions and answers

- **Q1: How are claims created and ended?** Answered: successful recognized
  edit tools and confirmed attribution mint immutable active claims. A later
  released/superseded record for the tuple ends current authority. Discovery
  found no automatic release producer. Source sites are in the linked report.
- **Q2: What does promotion actually select?** Answered: latest dirty paths
  with exactly one current active claim naming the ending seat. It does not
  bind that selection to the current edit's bytes or turn.
- **Q3: Are attribution links independent evidence?** Answered: no; they use
  active path-claim intersection. All six audited commits eventually had links
  to claude-10, including the three independently established claude-8 edits.
- **Q4: Has the narrow release taken effect?** Answered: not in the persisted
  projection at the 18:38:06.782Z read-back; subsequently confirmed released
  at 18:54:27.002Z (checkpoint below). Published at
  2026-09-27T18:16:17.037Z via `write-witness!`; still absent from the snapshot's
  current claims at 18:19:43.150Z. The watcher was advancing slowly, not proven
  stopped. Re-read current state before proposing recovery.
- **Q5: What is the smallest sufficient change?** Survey answered: no candidate
  has established sufficiency. The corrected inventory and counterexamples
  below identify available IDs, tool input, hash semantics, missing consumption
  evidence and test seams. Selecting the minimum sufficient design is DERIVE
  work; neither the full edit chain nor hash-only design is adopted.
- **Q6: How do unsupported/mixed edits and delayed intake behave?** Answered
  by source tracing and the reviewed counterexamples below: they can retain
  sole-claim eligibility, include another editor's dirt, or reuse unconsumed
  authority. Execution checks paths/status rather than content. Future refusal
  semantics and their real-Git demonstrations belong to DERIVE/VERIFY.

Surprises: the existing clean-after-claim rule documents the same defect from
August 26 but is not used by promotion. Shell edits can leave no new claim,
so an old sole claimant wins rather than producing an ambiguity. Two commit
messages list two planned paths even though their actual diffs change only one.

**MAP exit: Met.** Judged by codex-5 after review of kimi-9's corrected survey
(18:38Z checkpoint below). Each survey question has a concrete factual answer
or a demonstrated absence of evidence; ready/missing inventory is complete.
This does not mark C6 complete or accept a sufficient implementation design.

## 3. DERIVE

**DERIVE exit: Not started.** The discovery's content-bound, turn-scoped edit
chain is a candidate, not an approved design. Compare it with smaller changes
against C1–C5 before committing to it. Expiry alone does not cover B editing
before A's first turn-end; a post-edit hash alone may include B's prior dirt.

- [ ] Specify entity identities, authority lifetime, consumption and relations.
- [ ] Define checkable invariants for planning, staging, commit and replay.
- [ ] Draw producer → witness intake → projection → executor → receipt/release
  wiring, including field ownership and cross-process timing.
- [ ] Write IF/HOWEVER/THEN/BECAUSE for each non-obvious choice; record PSRs
  when selecting patterns.
- [ ] Define operator-visible held/released/pending outcomes.
- [ ] Fill the fidelity contract: preserve index isolation, compile/lint gates,
  explicit path staging and push policy; adapt claim eligibility and links;
  name tripwire tests for each preserved/adapted capability.
- [ ] Re-estimate and split implementation into independently reviewable packets.

## 4. ARGUE

**ARGUE exit: Not started.** Survey applicable patterns, record trade-offs,
explain why the chosen evidence excludes cross-seat edits, and provide a
3–5 sentence plain-language argument. No war-room ruling has yet been
identified; check applicability and cite one or explicitly record none.

## 5. VERIFY

**VERIFY exit: Not started.** At entry, fill the specification bill of materials
for attribution claims, records, process, feedback/read-back and decisions.
Name the actual available verifier for each aspect or a priced hole with a
revisit trigger. A wiring diagram is required: multiple parties write/read
fields, and records cross repo/process boundaries. Check it against source.

- [ ] Spike C1/C2 with real Git and actual planner/executor dependencies.
- [ ] Verify lifecycle ordering under delayed intake and replay.
- [ ] Pre-check every completion criterion and fidelity tripwire.
- [ ] Record findings and return to DERIVE if an invariant cannot be met.

## 6. INSTANTIATE

**INSTANTIATE exit: Not started.** Implement only after the design and its
verification are recorded. No promotion behavior has changed in this mission.

- [ ] Land scoped packets with independent review and explicit-path commits.
- [ ] Run clj-kondo, futon4/dev/check-parens.el, and relevant warranted or
  single-namespace tests; tag real-Git slow tests appropriately.
- [ ] Demonstrate C1–C7, record receipts and reproduce commands.
- [ ] Record PURs, checkpoint SHAs, and explicit follow-on work.

Shared JVMs remain single instances on canonical master checkouts. No worktree
code may be live-loaded. Restart is Joe's call. Witness publication must not
be replaced by an external writer editing the snapshot to evade intake delay.

## 7. DOCUMENT

**DOCUMENT exit: Not started.** Update inbox-zero operating documentation and
appropriate navigable docbook entries, link them from existing surfaces, and
verify discoverability. Document the final authority rules, refusal reasons,
release read-back, supported editing surfaces, and any remaining tickets.

## Progress and next packets

| Packet | Status | Deliverable / evidence |
|---|---|---|
| D — Discovery | Done | f21f838f; source trace, six-commit audit, complete dated inventories |
| R — One stale claim | Done | Released successor confirmed by real current-claims projection on the 18:54Z snapshot; C6 met |
| M — Finish survey | Done | Reviewed Q4–Q6 answers; two proposals unproven; reduced estimate withdrawn |
| V — Design and regression spike | Next | Resolve evidence/consumption/index requirements in DERIVE; ARGUE/VERIFY and fidelity tests follow |
| I — Implement and review | Pending | Scoped commits, independent review, C1–C7 evidence |
| N — Notification attribution/routing | Next after S1 review | Joe's live claude-1 example; C8; separate sweeper source and tests |
| O — Operating documentation | Pending | Navigable instructions and final checkpoint |

### Checkpoint 2026-09-27 — mission created

Created at Joe's request from the completed discovery packet. The original
2–3 engineering day estimate includes design, real-Git race tests and review;
it will be revised after packet M, not treated as a fixed commitment. The next
useful action is to verify the release and finish the minimum-change survey.
This checkpoint creates tracking only; it neither declares live closure nor
starts implementation or dispatches an agent.

### Checkpoint 2026-09-27 18:31Z — MAP helper dispatched

At Joe's explicit request, codex-5 dispatched **kimi-9** through Agency for a
read-only Q5/Q6 survey. Job `invoke-1790533836865-25557-ed31bba7` was accepted
and observed running. The requisition names this mission. Deliverable: source
sites, ready/missing inventory, unsupported/mixed-edit and concurrency cases,
minimum sufficient next spike, and revised effort estimate. No implementation,
shared-state mutation or further delegation is authorized in that packet.
codex-5 will review and consolidate its result here; MAP remains open.

Q4 recheck by codex-5: a streamed snapshot scan at **18:30:54.708Z** still
finds 1,483 claim records and the original active `claim:8c7eb3879d22…`;
the release successor is not yet ingested. Read-only watcher status reports
cycle 557, previous cycle finished 18:24:36.087Z, current cycle started
18:24:41.091Z, progress 18:27:27.694Z, inbox-zero phase, no last error.
This establishes continued slow progress, not closure or a stopped watcher.
No restart, reload, extra cycle, or competing snapshot writer was used.

### Checkpoint 2026-09-27 18:35Z — first MAP survey reviewed

kimi-9 completed job `invoke-1790533836865-25557-ed31bba7`. codex-5 reviewed
its source inventory and challenged its proposed sufficiency argument. The
Agency job retains the full response; this checkpoint records the reviewed
findings rather than adopting all helper conclusions.

Additional ready infrastructure:

- `dev/futon3c/dev.clj:1100-1140` retains tool input until a non-error result,
  correlating by agent/session/tool-use ID. Claim minting discards the edit's
  content. `witness.clj:139-146` binds a claim to that tool call and path.
- `futon3/inbox-zero-lib/src/futon3/inbox_zero/watcher.clj:184-194,224-235`
  computes raw-byte SHA-256 for observations and records HEAD. **Index hash is
  always nil here**, so the field's presence is not reusable index evidence.
- `state.clj:68-120` permits additional claim keys. That makes an optional
  extension syntactically possible; it does not establish backward-compatible
  semantics or authorize treating old records as content-qualified.
- `futon3/test/futon3/inbox_zero/promote_exec_test.clj` has a real temporary
  Git repository harness; planner purity and turn-promotion injection seams
  allow focused regression tests without a shared live checkout.
- Current claim/promotion calls have no durable turn identity propagated into
  them. **This is not proof that the system lacks one:** Agency has invoke
  job IDs. Reuse and propagation through the CLI/tool-result boundary remain
  to be surveyed.

The helper proposed post-result claim hashes, a historical clean baseline,
one eligibility predicate, staged-object checks and release-on-commit, with
an estimate of 1–1.5 days. **Neither sufficiency nor that revised estimate is
accepted yet.** Concrete gaps requiring a revised answer:

1. Clean X is observed; B changes it to X+B; A edits another part; the claimed
   post-result hash is X+B+A. The proposed clean-baseline and hash-equality
   conditions both hold while including B's work. Reading the hash after
   the tool result also permits B to edit between the result and that read.
2. Content equality does not establish unconsumed authority. After A commits
   H, B can reintroduce H on a different HEAD before release/clean intake.
   The design must state baseline/consumption evidence rather than declare
   byte-identical attribution irrelevant to the agreed criteria.
3. `git ls-files -s` returns Git object IDs and modes, not raw-byte SHA-256.
   Filters, symlinks, modes and explicit deletions need defined semantics;
   a nil hash alone is not a deletion witness. A staged-content check must
   also address shared-index mutation before the subsequent commit.

Follow-up job **invoke-1790534092997-25562-b34e879d** asks kimi-9 to correct
the inventory, test these arguments against source, and estimate a bounded
next spike separately from full implementation. It remains read-only. Neither
a full edit chain nor a hash-only repair is established necessary/sufficient.
MAP remains open; no production behavior was changed.

Q4 read-back at **18:35:14.241Z** still finds the original active claim and
1,483 claim records, without the published release successor. Watcher status
still reports running, cycle 557 in inbox-zero, last completed cycle at
18:24:36.087Z and no last error. Keep the distinction between a durable
published release and current persisted authority explicit.

### Checkpoint 2026-09-27 18:38Z — corrected survey accepted; MAP closed

kimi-9 completed follow-up `invoke-1790534092997-25562-b34e879d`, accepted
all four review counterexamples, and withdrew hash-only sufficiency and the
1–1.5 day estimate. codex-5 checked the source excerpts and consolidated the
survey. Previous checkpoints retain their historical judgments; the current
phase verdict above records this review's outcome.

Corrected ready/missing inventory, supplementing the initial table:

| Ready, with evidence | Missing / DERIVE obligation |
|---|---|
| Agency inbox job IDs are persisted (`agency/inbox.clj:25-68`); Claude CLI `invoke-once` has a local `invoke-trace-id` (`dev.clj:3594-3615`), with a corresponding warm path | Neither ID is passed through the current witness hooks or promotion calls; decide what identity is required and how to propagate it |
| Tool inputs survive until correlated successful results (`dev.clj:1100-1140`) | No transaction evidence establishes that a pre-state belongs to A and excludes intervening B edits |
| Observation raw-byte hashes and HEAD, with optional claim extension possible | Define canonical Git object/mode/deletion evidence and conversions; index/hash is nil |
| Real-Git executor harness and injectable planner/turn pipeline | Verify content through commit despite shared-index writers; an empty index at entry is insufficient |
| Immutable release intake and current-claim projection | Immediate or otherwise safe consumption semantics despite delayed release ingestion |
| Existing dirty-set clean-after filter | Consistent authority semantics for promotion, dirty sets, and newly derived commit links |

The helper's specific receipt tuple and “atomic pre/post witness” are **design
candidates**, not surveyed existing facilities or proven minimum requirements.
The existing hooks observe a tool stream; they do not own a transaction around
filesystem mutation. Adding two hash reads to them cannot by itself establish
atomicity or exclusion of another writer. Likewise, a local invoke trace ID is
available, but its presence does not prove it is a stable replay identity.
A new Agency plumbing signature may be one option; this survey does not prove
it is necessary or that no existing context path could carry the ID.

DERIVE must settle these obligations:

1. **Edit authority:** establish provenance for exactly the authorized delta
   and baseline, or refuse when that cannot be established. Address mixed
   preexisting dirt and edits between tool execution and witness capture.
2. **Consumption:** prevent a consumed claim authorizing new work, including
   identical content on a changed HEAD before release intake. Specify receipt,
   crash and replay behavior; do not equate byte equality with authority.
3. **Git semantics and commit integrity:** distinguish raw-byte SHA-256 from
   Git object IDs; account for clean filters, CRLF, symlinks, explicit deletion
   and mode. Establish that the checked content is the content committed,
   including index writes between staging/checking/commit. Git's per-command
   locking does not establish one transaction across those separate commands.
4. **Identity and views:** select only the identity information required by
   the above invariants, and make the relevant projections agree.

No blanket claim of “no content is captured anywhere” is accepted: file
observations do capture hashes and tool inputs exist transiently. The specific
absence is durable evidence binding the claimed edit transaction to content.

Latest Q4 evidence: the streamed state scan at **18:38:06.782Z** still has
1,483 claims, all historical records active, and the target current claim is
unchanged. Snapshot mtime is 18:24:32.478Z. The published successor remains
pending; C6 is unchecked. The next operational action is read-back and, if
still pending, read-only diagnosis of the owning watcher's intake. No direct
snapshot edit or second writer is an acceptable recovery.

**Next packet:** DERIVE the smallest enforceable authority/consumption/commit
contract, then use a real-Git VERIFY spike for the four reviewed counterexamples,
object/mode/filter cases and a positive promotion. Preserve the original C1–C7.
The helper estimates 0.5–1 engineering day for a bounded survey/spike and a
provisional 1.5–3 days for full work, but neither is a delivery commitment.
Time-box the next packet by its evidence deliverables and re-estimate after
it; no code or runtime change is authorized merely by this estimate.

Validation: reviewed source and Agency results, streamed live snapshot,
checked mission phase markers and relative links. No implementation, extra
helper dispatch, reload, restart, watcher tick or state mutation in this review.

### Checkpoint 2026-09-27 18:43Z — continuation authorized; D1 dispatched

Joe: “I'm happy for you to continue through the mission using parks and asking
kimi-9 for the packets work.” codex-5 continues as mission owner/reviewer;
kimi-9 authors bounded packets through Agency. Phase exits remain reviewed
judgments, not automatic consequences of helper completion.

Dispatched D1 DERIVE as job **invoke-1790534585660-25569-f17111ce**.
Expected artifact: `holes/labs/M-inbox-zero-claim-lifecycle/D1-derive.md`, plus
checker-grounded wiring if appropriate. Scope: enforceable edit authority,
consumption/replay, Git object and shared-index semantics, shared eligibility,
operating interfaces, fidelity matrix, invariants, source changes and test
mapping. Helper may commit only those design artifacts; no production code or
runtime mutation. If existing hooks cannot enforce an invariant, name the
required structural change and unresolved issue rather than bypassing it.

Next continuation: review D1 against C1–C7 and the four counterexamples;
return a bounded revision if necessary, otherwise record DERIVE and dispatch
ARGUE/VERIFY work in lifecycle order. Recheck release ingestion as part of
operational follow-through. Continue implementation/review/documentation only
when preceding obligations are met, retaining the no-restart and canonical
shared-checkout constraints. Parks on actual job IDs carry this continuation
through the Emacs REPL; no internal Codex collaborators are used.

### Checkpoint 2026-09-27 18:55Z — D1 returned for revision; C6 complete

D1 author commit **b9e6fb65**, artifact
[design packet](../labs/M-inbox-zero-claim-lifecycle/D1-derive.md), reviewed
by codex-5. **DERIVE remains not started as an accepted specification**;
D1 is a proposed design with blocking findings, not a completed phase.

Two reviewer experiments used a real temporary Git repository, bounded by
`timeout 25`, removed automatically on exit. No shared checkout was altered:

- **CE2:** A commits H; no receipt is published; B commits X on a new HEAD,
  then leaves H dirty. Authorized and worktree OID were
  `a9edc74f3848050ab04b488787d715349bb9b215`, HEAD's path OID was
  `62d8fe9f6db631bd3a19140699101c9e281c9f9d`. D1's already-landed predicate
  is false, so it does not prevent reuse after a commit/receipt crash gap.
- **Same-path index resync:** B stages `B staged bytes`; D1's
  `git reset -q HEAD -- f` replaces the staged value with `X`. The worktree
  survives but B's staged work does not. An empty index observed earlier does
  not prevent this race. Private commit staging does not justify this resync.

Additional source/design findings returned to the helper:

- A consumed/already-landed claim is ineligible for a new promotion but must
  remain usable to explain its own commit. D1's I8 “eligible at derivation
  time” prevents positive post-commit links; separate prospective permission
  from historical receipt attribution while sharing authority semantics.
- Buffered tool-use observation is not proof a pre-read completes before
  tool execution. Endpoint equality does not prove every interleaving is
  detected. Define trusted raw bytes, authorized modes, filters and real tool
  semantics; a filter-normalized baseline may hide different raw prebytes.
- Checking a hash then rereading for `hash-object -w` can write different
  content. Freeze captured bytes, conversion context, old HEAD/tree/parent;
  inventory hooks/signing/config semantics lost by replacing `git commit`.
- The lifecycle's named executable wiring checker exists at
  `src/futon3c/diagramprover/wiring.clj` (including ingest, read/write checks
  and source conformance). Surveying argument-map checkers did not justify
  D1's claim that no grounded wiring format exists.

Dispatched bounded revision **D2**, job
**invoke-1790535289457-25601-41cedf69**, to kimi-9. It may revise design
artifacts and run disposable real-Git experiments; no production/state changes.
Return a corrected implementable contract or a precise structural blocker and
smallest evidence-producing spike. No weaker crash fallback, unreviewed phase
exit, or acceptance-criterion relaxation is authorized. Owner parks on this
actual job and reviews the continuation before advancing.

**C6 evidence:** streamed snapshot at **2026-09-27T18:54:27.002Z** contains
1,484 claims, 655 current tuples, 654 active tuples. The target tuple now
projects `:released` via
`claim:release-inbox-zero-claims-d-8c7eb3879d22-20260927`, referencing the
original `claim:8c7eb3879d22ea52d75ef793b7316dc1d08cc70eb9ae5cee2f5ac8bb0307dc84`.
A bounded bb call to the real `projection/current-claims` over the streamed
claim records asserted released current state and preservation of the original
active historical record. Publication was 18:16:17.037Z; the successor was
absent at 18:38Z and present by 18:54Z. These are observation bounds, not the
exact ingestion timestamp. During the delay, promotion still saw stale active
authority; the general repair must account for this lag. No restart, reload,
forced watcher cycle or competing snapshot writer was needed for closure.

### Checkpoint 2026-09-27 19:05Z — D2 review; S0 evidence spike dispatched

D2 **8fedc4cc** provides a substantive revision and
[grounded wiring declarations](../labs/M-inbox-zero-claim-lifecycle/wiring-d2.edn).
[Current candidate](../labs/M-inbox-zero-claim-lifecycle/D2-derive.md)
is **not accepted** as an implementable contract. DERIVE remains pending;
ARGUE/VERIFY phase exits have not been claimed. C6 remains complete.

Improvements retained for the next design: distinguish prospective promotion
permission from historical receipt attribution; target releases at claim IDs;
stop resetting foreign staged content; survey actual repository hooks; use the
existing wiring checker and report unimplemented conformance findings. These
are useful directions, not proof of the revised end-to-end contract.

Owner ran two further real-Git checks in a disposable repository under
`timeout 25`, with automatic cleanup and no shared checkout mutation:

1. **Normal positive path fails:** baseline/shared index X, authorized
   worktree H, no foreign writer. D2's private-index commit + ref CAS gives
   HEAD=H, index=X, worktree=H, `git status --porcelain` = `MM f`. The cached
   diff reverses H to X. D2 §9's claim that this finishes clean is false.
   The unchanged index entry is not another seat's dirt; it is the executor's
   incomplete index/ref transition. Leaving it can revert the promotion in a
   later commit. C4 must cover ordinary index state as well as HEAD contents.
2. **Consumption is not monotone under a ref rewrite:** after A's trailer
   commit H, omit receipt publication and reset HEAD to mint HEAD using
   `update-ref`. H remains in the worktree, but `mint-head..HEAD` has no
   reachable trailer. D2's trailer scan cannot establish prior consumption.
   Safe crash/retry behavior needs a durable protocol independent of this
   reachability assumption, or a justified structural ownership constraint.

Other remaining obligations: pre-commit hooks may mutate the private index,
so checked-before-hook is not checked-before-commit. Two filter invocations
can produce different object IDs even with immutable raw input; compare the
actual persisted object. CLI-owned hooks order one CLI's tool, not arbitrary
other writers; observed mode differences do not prove the tool authorized a
chmod. Filter-normalized baselines and path-alias races need explicit treatment.
These are requirements to verify, not permission to weaken the criteria.

Dispatched **S0**, job **invoke-1790535890716-25607-a6653719**, to kimi-9:
a bounded executable real-Git evidence packet feeding DERIVE, not another
prose-only sufficiency argument. Authorized artifacts: disposable-repo script,
report, optional D2 review banner. No production/runtime/state changes.

S0 must reproduce the known defects, then test a minimal transaction candidate
for clean ordinary success, foreign same/other-path staged-state preservation,
Git-compatible locking and durable prepare/commit/recovery crash cutpoints.
It also tests mutating/refusing hooks and a deterministic stateful clean filter.
Capture prerequisites remain explicit; no live CLI configuration change or
billable model invocation is part of this packet. A structural blocker is an
acceptable evidence result; routing around one is not.

Owner will review S0 and update the contract before advancing DERIVE. This
spike is targeted risk reduction inside an unresolved design, not a claim that
the mission has skipped ARGUE or completed VERIFY. Helper returns actual
commands, assertions, transcripts and go/no-go judgments; owner independently
checks the decisive cases. Continuation uses a park on the real S0 job ID.

### Checkpoint 2026-09-27 19:16Z — S0 independently rerun; S1 dispatched

S0 **727518e6** artifacts:
[report](../labs/M-inbox-zero-claim-lifecycle/S0-report.md),
[harness](../labs/M-inbox-zero-claim-lifecycle/s0-git-transaction.py),
[transcript](../labs/M-inbox-zero-claim-lifecycle/s0-transcript.txt).
codex-5 read the harness and independently ran it under `timeout 60`:
**6/6 defect reproductions, 26/26 candidate assertions**, exit 0. These
counts validate the stated scenarios, not the whole promotion contract.

S0 establishes useful facts: native Git honors index.lock; a byte-CAS refresh
can preserve foreign staged entries in tested interleavings; prepared intent
can exclude retries independently of Git history; hook mutation requires a
post-hook tree check; clean filters can return different OIDs on the same raw
input. The final authority/journal protocol is not yet accepted.

Two additional owner tests imported the actual S0 functions into a disposable
real Git repo (`timeout 25`, `cleanup()` in `finally`):

1. Baseline X; worktree H; `candidate_commit(..., crash_at="after-cas")`;
   native `git commit -qm 'ordinary concurrent commit during index gap'`.
   The native commit succeeds with HEAD:f = X, reverting the promotion.
   Thus the unlocked ref-CAS → index-refresh interval remains unsafe.
   Index reconciliation is not cosmetic; later native commits can consume
   the stale index. An interoperable lock or another enforced ownership
   mechanism must cover the dangerous interval, including crash recovery.
2. Calling S0 `recover()` after that descendant commit writes its outcome
   using the new HEAD, not the actual promotion commit. In the owner run,
   actual promotion = `4c703468c35dc55e27e0eea7b9314f6249a43003`, incorrectly
   recorded descendant = `e6d9e8d01accd423876ba692a03361b5e3c04f39`.
   Recovery must find and verify the exact claim commit, parent, tree and
   path objects; a boolean trailer match plus current HEAD is insufficient.
   Old receipts must not use current HEAD:path as historical evidence.

Source-review limits: S0 journal rename overwrites and does not fsync; it is
an experiment, not a durable immutable single-winner acquisition protocol.
Crash tests are controlled returns, not process-kill or power-loss tests.
Its filter test proves stage == returned OID, not stage == authorized OID.
I1/I8 are not demonstrated by an actual planner/link predicate test and cannot
be graded “go” on the strength of the trailer experiment. Capture still needs
raw-baseline and alias/mode evidence; normalized `git diff` is not raw trust.

Dispatched **S1**, job **invoke-1790536534131-25612-606cc6e9**, to kimi-9.
Scope is deliberately transaction-only: close/refuse native-writer access
through the ref/index interval, make lock ownership and crash recovery explicit,
pin immutable receipt identities, demonstrate single-winner claim preparation,
and refuse persisted-object mismatch. Use actual native Git interleavings,
retain ordinary clean success and foreign same/other-path preservation, label
fault models honestly, and surface any structural blocker. Do not remove an
unexplained native lock or reinterpret an authorization mismatch as new authority.

No implementation or production/state/config changes authorized in S1; only
separate lab script/report/transcript with explicit-path commits. Existing S0
remains historical evidence. Owner parks and reviews S1 before settling DERIVE;
capture is a later bounded packet rather than being mixed into this one.
C1–C7 unchanged, C6 complete, DERIVE still pending.

### Checkpoint 2026-09-27 19:22Z — Joe's live irrelevant-followup example

Joe reports a followup to **claude-1** naming apparently irrelevant work:

> inbox-zero: futon3c-d is carrying 10 dirty file(s) (3 untracked); 5 of them
> were written during your turns. Commit or delete what is yours and leave
> what is not. Newest first: holes/labs/M-inbox-zero-claim-lifecycle/s0-git-transaction.py
> (also inside codex-4, kimi-9, 象's turn), scripts/test_xiang2000_p0_origins.py
> (also inside codex-4, kimi-9, 象's turn), holes/labs/M-象-2000/p0-expected.edn
> (also inside codex-4, kimi-9, 象's turn), scripts/xiang2000_p0.py
> (also inside codex-4, kimi-9, 象's turn), emacs/session-mode.el
> (also inside codex-4, kimi-9, 象's turn). Full list:
> git -C /home/joe/code/futon3c status --porcelain

codex-5 traced the exact wording to **a separate notification path**, not
`promotion/plan-promotion`:

- `src/futon3c/inbox_zero/sweeper.clj:65-103` reads current dirty entries and
  filesystem mtime. An untracked directory uses the newest descendant time.
- `sweeper.clj:155-179` collects job activity windows; missing finish times
  extend to now. Every roster agent reported invoking also gets a synthetic
  window from now minus 30 minutes to now. The window records do not carry
  exact session identity, repo or mission relevance.
- `sweeper.clj:190-223` assigns an entry to every currently reachable agent
  whose window contains its mtime, adding `:shared-with`. This is temporal
  overlap, not evidence of who wrote the file.
- `sweeper.clj:225-256` ranks by overlap counts (up to three recipients by
  default), samples five newest matching paths, then emits Joe's exact
  “written during your turns; commit or delete” wording.
- `sweeper.clj:393-395` resolves those agent IDs to their **current** sessions.
  An older window can therefore be directed at a newer session of the seat.
- `test/futon3c/inbox_zero/sweeper_test.clj:145-184` explicitly tests time
  attribution and duplication to overlapping agents. These tests preserve
  the current behavior; changing only the prose would leave the relevance
  problem in routing intact.

The dirty-notice lane itself does not commit/delete/mint claims
(`sweeper.clj:15-17`), but it asks recipients to act on a list selected without
authorship evidence. It shares the mission's attribution concern while using
a different mechanism. S0's helper authorship is established by packet commit
727518e6; this example does not establish ownership of the other listed files.
Historical window reconstruction has not been performed, so the source trace
explains the mechanism without asserting the exact window that matched claude-1.

**Scope addition at Joe's steering:** C8 above, and packet N for notification
attribution/routing. Preserve C1–C7 unchanged. A fix must keep unattributed
backlog visible while avoiding cleanup assignments to unrelated concurrent
seats; observed overlap may remain labeled diagnostic evidence, never authorship.
Do not merely add “possibly” to the same noisy assignment or silence the lane.

Also survey wording consumers when changing the format:
`scripts/xiang2000_p6o3.py:28` and `scripts/test_xiang2000_p6o3.py:8` recognize
the exact present notice pattern. Their semantics must stay coherent with any
new structured classification/wording, rather than silently losing detection.

S1 job `invoke-1790536534131-25612-606cc6e9` was checked and is still running.
Its existing park remains the continuation; no duplicate park/dispatch was
created. Review that result, then prioritize a bounded N survey/design packet
with kimi-9. Owner will connect the notification contract to the authority
contract without forcing notification repair to wait for every commit-mechanics
implementation detail. No runtime/source behavior changed in this checkpoint.

### Checkpoint 2026-09-27 19:26Z — S1 review; notification packet prioritized

S1 **4a65b781** artifacts:
[report](../labs/M-inbox-zero-claim-lifecycle/S1-report.md),
[harness](../labs/M-inbox-zero-claim-lifecycle/s1-git-transaction.py),
[transcript](../labs/M-inbox-zero-claim-lifecycle/s1-transcript.txt).
Owner read the implementation and independently reran under `timeout 60`:
reported 23 candidate assertions and two new defect reproductions pass; the
five historical S0 defect assertions also rerun. No production mutation.

Progress: native index.lock spans the ordinary ref-CAS/index-update interval;
receipt lookup now verifies a claim commit's parent and tree rather than just
naming current HEAD; journal acquisition uses O_EXCL and file/directory fsync;
persisted-object mismatch refuses instead of changing authorization. These
improve the tested paths but do not establish the full transaction contract.

**Blocking recovery counterexample (owner, real Git):** create baseline X,
authorized H; `s1_commit(..., crash_at="in-critical-after-cas")`; emulate dead
owner exactly as S1's own test does. Wrap `lock_release_ours` to run native
`git commit` immediately after the first successful unlink during recovery.
`recover_s1` removes the orphan lock before reacquiring it. Native commit in
that gap exits 0 and makes HEAD:f = X (stale reversal); recovery reports
`:recovered-committed`. Disposable repo removed in `finally`, `timeout 25`.
This is an ordinary cooperating Git writer, not an adversary copying lock
contents. The unsafe interval remains in recovery even though normal execution
now excludes it. No residual-risk acceptance is granted.

Other review limits: `lock_release_ours` rechecks only the transaction field,
not the claimed byte-for-byte identity. Its check then unlink is not atomic
against another recovery actor. Recovery must not clear another transaction's
ours-formatted lock simply because its PID appears dead. Typed journal
completion must distinguish a verified commit receipt from pending/refused
index reconciliation; a deferred refresh is not full completion. These remain
explicit transaction requirements, alongside capture and full predicate tests.
Power-loss behavior has not been tested and is not claimed.

**Next priority is Joe's notification example, C8.** Dispatched **N1**, job
**invoke-1790537123218-25620-7babadc4**, to kimi-9. Deliver one bounded,
source-backed notification routing/wording design and implementation packet,
not production edits. It must inspect the actual consumed operator/backlog
surface, dedupe, exact-session relevance, sole/ambiguous temporal overlap,
synthetic windows, and mixed known/unknown paths. Temporal overlap must not
assign commit/delete work; uncertainty must remain visible, not vanish when
some other path has a recipient. Survey exact-text classification consumers
before changing message format. Do not fabricate ownership or a repo-owner map.

N1 is reviewed independently of unfinished transaction mechanics, so C8 can
progress without waiting for the full promotion redesign. No S2 dispatch yet;
the reproducible recovery blocker above is retained for that future packet.
Owner parks on N1, reviews the concrete proposal, and then authorizes a narrow
implementation if its routing evidence and visibility contract are adequate.
C1–C7 remain unchanged, C8 added by Joe's steering, C6 complete, full DERIVE
not yet accepted. No JVM reload/restart, state writes or notification sends
were performed in this review.

### Checkpoint 2026-09-27 19:30Z — N1 source/consumer review; N2 dispatched

N1 **3db44558**, [proposal](../labs/M-inbox-zero-claim-lifecycle/N1-notifications.md),
reviewed by codex-5. The mixed-repo backlog omission and temporal-overlap
misrouting are confirmed. Proposed routing direction is useful; implementation
is **not yet authorized** because evidence and consumer claims remain unsound:

- An exact-session tool witness or confirmed attribution record is historical;
  neither current record schema binds the current dirty bytes. N1 E1/E2 must
  not reintroduce the stale-claim defect as “authorship-grade” notification input.
- `futon3/inbox-zero-lib/src/futon3/inbox_zero/escalation.clj` routes supplied
  items, using a seat already named on the item/plan or a literal tier-2
  `street-sweeper` fallback. A historical routing decision does not establish
  a current, exact-session triage responsibility grant. Read-only lookup of
  `/api/alpha/agents/street-sweeper` returned **Agent not found** during review.
- N1 both finds no programmatic backlog reader and calls the same file a
  consumed surface. Writing uncertain work there while almost eliminating
  notices is not demonstrated visibility. Need an existing discoverable
  consumer integration, or a precise routing/surface decision from Joe.
- The cited `agency/inbox.clj` persisted payload does not contain session-id;
  session availability must be established from actual invoke records, not
  asserted from those lines. Missing identity cannot map to the current seat.

Owner also inspected the existing inbox-zero board consumer and batch dispatch.
They consume watcher state and enforce their own dispatch/commit constraints;
that does not establish that they read `operator-backlog.edn` or display it to
an operator. Do not route uncertain dirt through their commit path as a shortcut.
The standing escalation policy says volume alone is not operator judgement;
a visible repo-pressure display is different from an unsolicited judgement task.

Dispatched **N2**, job **invoke-1790537360720-25622-6f75c30f**, to kimi-9:
resolve these source facts and identify an actual operator-facing surface
(Emacs/HUD/mission UI or another demonstrated consumer) with a small testable
producer-to-display integration. No fabricated authorship, standing role or
recipient. If a preference/authority choice is indispensable, return the one
missing decision and two grounded options for owner presentation. Retain old
notice-classification compatibility, mixed-repo completeness, dedupe and exact
session discipline in the proposed implementation contract.

N2 is document/source survey only, <=160-line deliverable, explicit-path commit;
no source/runtime/state changes or outgoing notices. Owner parks on N2. The
transaction recovery blocker remains recorded for S2; C6 remains complete.

### Checkpoint 2026-09-27 — N2 reviewed; N3 local implementation authorized

Owner reviewed N2 **cf87f30d** against the producer, projection and consumer.
Choose **A: producer-side integration**, keeping mana JSON, WM projection and
markdown consistent. N2 corrects historical-claim authority and invented triage
routing. It identifies real existing consumers; it does not yet establish live
replacement visibility. Owner GET `/api/alpha/war-machine` returned **503**,
`running? false`, no cached days. C8 is not complete and runtime cutover is
blocked pending a demonstrated available replacement surface.

Source corrections: `scan-metabolic-balance` reconstructs per-repo maps and
would discard added fields; `summarize-working-tree-hygiene` reconstructs again,
filters positive pressure, and takes eight queues. Merely merging uncertainty
into mana is insufficient. The implementation must propagate it through actual
projection/rendering, retain low-pressure uncertain dirt, expose bounded-list
remainder and accessible full detail, and distinguish missing/stale input from
zero. Joins must preserve canonical worktree identity, including the difference
between `futon3c-d` and display labels. A backlog pathname alone does not prove
a usable drilldown. No serve-time overlay with divergent producer semantics.

**N3 dispatched to kimi-9**, actual job
**invoke-1790537808721-25626-98b07480**: bounded local implementation and tests,
explicit-path commits across the necessary repos. Structurally replace unsafe
personal attribution with authorship-unknown repo pressure; do not add a flag
that re-enables the known-unsound route. Preserve unrelated sweeper lanes.
Temporal overlaps and historical citations remain diagnostic only. Cover Joe's
example, sole overlap, historical claims, mixed uncertainty, repeat/rollover,
identity aliases, missing/stale/malformed data, low pressure and queue overflow.
Require a real producer-to-projection-to-consumer fixture and normal lint,
parenthesis and targeted test gates. Existing historical p6o3 classification
must remain compatible. Owner will independently review commits and evidence.

This authorization is **local code only**. No live state/snapshot writes,
notifications, scheduler start, reload or restart. The packet must document the
concrete paired deployment prerequisites; it must not switch off running notices
while the replacement consumer is unavailable. No claim of C8 completion or
full DERIVE exit is authorized. S1 recovery unlock/relock failure remains open
for later S2, capture remains unproven, C6 release remains complete. Owner parks
on the actual N3 job; deadline wake requires job inspection, not redispatch.

### Checkpoint 2026-09-27 — N3 reviewed; N4 corrective implementation

Reviewed N3 source commits futon3c 4a20507d, futon0 8399bee, futon2
8427db315 and report a514e47a. Unsafe personal-assignment machinery is
structurally removed in local source; runtime remains unchanged. **Not accepted
for deployment/C8 completion.** Owner found concrete visibility failures:

- Backlog and feed share a row containing only five newest filenames. The
  advertised full detail cannot enumerate the remainder; plain pathname text
  does not establish usable UI navigation.
- Producer merge iterates only mana manifest roots. Owner executed actual
  merge function with empty manifest and an uncertainty root carrying ten
  dirty paths: result `[]`. Worktree identity also disappears at WM scan.
- Owner executed actual loader on `{:repos [{:root "/tmp" :dirty-count -7}]}`
  in a disposable EDN file: returned `:available`, nil timestamp/age and
  `:stale? false`. Missing required freshness/count validation is confirmed.
- Projection defaults missing uncertainty to numeric zero. Actual table does
  not render missing/malformed or stale uncertainty status. Overflow detail
  points to a backlog that does not contain all ordinary pressure queues.
- Diagnostic-window failures can suppress reporting; publication failures
  need explicit incomplete status. Tests must cover these boundaries.

Owner probes ran with timeout and removed disposable data. First loader probe
failed before invocation because script initialization resolved NO_SOURCE_PATH;
second loaded only the real loader/merge definitions and produced the results
above. No test suite rerun was needed to establish these new counterexamples.
N3's disclosed accidental live mana regeneration violated packet scope; the
correction must make tests safely importable and sandbox all outputs. Skipped
.bb files do not count as checked parentheses.

Dispatched corrective implementation **N4** to kimi-9, actual job
**invoke-1790538875534-25633-c905b834**. Require complete navigable detail,
canonical-root union without invented measurements, validated freshness and
explicit unknown state, real producer-through-render tests and failure cases.
The first dispatch attempt was locally rejected for requisition syntax, so
there was no duplicate accepted job. Deployment report must establish available
consumer BEFORE removing the live notice lane, not reload first into a 503.
No deployment, state writes or notification sends authorized by this packet.

Joe's prompt steering is recorded: agent/design-pattern prompt such as
`$象/诺必践*>` may consume verified current-work dirt in future; `?` may
represent unresolved checkout dirt. Baseline prompt must not imply clean, and
this reporting feed cannot authorize `*`. No prompt implementation or message
to claude-7 dispatched. C6 complete; transaction recovery S2 and capture remain
open; full DERIVE pending. Owner parks on N4 for independent review.

### Checkpoint 2026-09-27 — prompt-line collaboration authorized

Joe explicitly authorized collaboration with claude-17 via
[SEAM-prompt-line](../labs/M-象-2000/SEAM-prompt-line.md) (516377e3).
Owner read the seam and dispatched bounded coordination job
**invoke-1790539003417-25635-c9c4175e** to claude-17. Renderer/pattern remain
M-象-2000's; inbox-zero owns its future provider. Requested concrete registry
API/context, exact seat/session and canonical worktree scope, observation vs
render time, evidence references, composition and diagnostic omission behavior.

Current claims/overlap cannot authorize `*`. Initial `?` must rely on a fresh
scoped dirty observation, not stale data or absence from the thresholded pressure
feed. Absence never means clean. Proposed `?` precedence for mixed verified-own
and unresolved-other dirt, retaining both in the spelled-out facts/basis; this
is a coordination proposal pending reply. No provider/renderer implementation
or deployment dispatched yet. N4 remains active under its existing job, not
replaced or duplicated. Both actual jobs will be inspected on resume.
