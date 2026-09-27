# M-inbox-zero-claim-lifecycle

**Status:** MAP (2026-09-27)
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
- [ ] **C6 — Operational closure:** Read back the released successor for
  `claim:8c7eb3879d22…` through the current-claim projection. Document any
  remaining intake delay and its effect on safety. Publication alone is not closure.
- [ ] **C7 — Review and discovery:** Relevant checks pass, independent review
  is recorded, and the operating instructions explain holds, release, and
  unsupported editing surfaces. Link them from the existing inbox-zero docs.

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
- **Q4: Has the narrow release taken effect?** Open. Published at
  2026-09-27T18:16:17.037Z via `write-witness!`; still absent from the snapshot's
  current claims at 18:19:43.150Z. The watcher was advancing slowly, not proven
  stopped. Re-read current state before proposing recovery.
- **Q5: What is the smallest sufficient change?** Open. Inventory existing
  turn IDs, tool-result evidence, Git object identities, consumption records,
  and test seams before choosing new record types or instrumentation.
- **Q6: How do unsupported/mixed edits and delayed intake behave?** Open.
  Enumerate observable refusal cases and concurrency orderings using real
  dependencies. Do not infer ownership from filenames, age, or a recent claim.

Surprises: the existing clean-after-claim rule documents the same defect from
August 26 but is not used by promotion. Shell edits can leave no new claim,
so an old sole claimant wins rather than producing an ambiguity. Two commit
messages list two planned paths even though their actual diffs change only one.

**MAP exit: Not met.** Q4–Q6 remain open; discovery is substantial but does
not yet establish the minimum implementable repair.

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
| R — One stale claim | Published; read-back pending | Released successor `claim:release-inbox-zero-claims-d-8c7eb3879d22-20260927`; C6 remains open |
| M — Finish survey | Next | Q4–Q6, minimum-change options, revised estimate |
| V — Design and regression spike | Pending | DERIVE/ARGUE/VERIFY, C1/C2 reproduction and fidelity tests |
| I — Implement and review | Pending | Scoped commits, independent review, C1–C7 evidence |
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
