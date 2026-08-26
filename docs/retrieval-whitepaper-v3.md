# Silence, Witness, and Demand: an Agent Memory System Audited by Its Own Twins

**V3 — DRAFT. Phase 1 sections 2026-08-11; §§2.1 (items 14–20), 3.1a,
3.1b, 5a added 2026-08-25 from the f28–f35 frame campaign; §4a (the delivery
layer) added 2026-08-26 from the cascade investigation. Batch-dependent
sections are skeletal and marked.** Successor to V1 ("Warrant-Disciplined Agent Memory:
Architecture, Instrumentation, and a Pre-Repair Baseline") and V2 ("Catching
Our Own Instruments: Construct Validity, Self-Applied"). Programme:
`holes/excursions/E-memory-whitepaper-v3-programme.md`. Evidence citations
are commits (futon3c / apm-lean), evidence-store ids (substrate
migration-store-21), and frame records under `data/experiment-frames/`.

## Abstract (draft)

V1 described a warrant-disciplined memory architecture and measured its
pre-repair baseline. V2 turned the instruments on themselves and found the
instruments wanting. V3 reports what happened when the system was finally
made to run — and run against controlled twins. Three results. First, the
strongest form yet of the series' silence thesis: the entire deployed
retrieval pipeline ran dead for an extended period while every receipt it
emitted was well-formed, and the same signature — absence presenting as
normal operation — recurred at every layer we instrumented, including
twice inside instruments built specifically to prevent it. Second, a
witness standard that survives adversarial checking: a memory USE counts
only when the committed artifact carries the memory's fingerprint, and
under that standard plus paired blind dispatch, self-reported memory
benefit repeatedly failed corroboration — including one case contradicted
by its twin's wall clock. Third, an inversion of where memory pays: the
passive retrieval channel contributed nothing detectable at current
quality (preregistered, falsifier fired), while the demand side —
runners' failed searches converted into votes, votes into voter-specified
artifacts, artifacts into callbacks — proved a theorem within ninety
minutes of being built. The paper's constructive contribution is the
resulting pipeline (votes → build → callback), its measurement discipline
(frames with enforced slots; twin adjudication; blind relevance scoring),
and a characterization of the retrieval defect written by its own users.
A fourth strand, added later and reported as such: a campaign carrying
memories from a strong solver to a weaker student on the same problem,
in which the witness standard was applied mechanically to every use claim
on record — no claim unwitnessed, the verbatim-paste failure mode ending
exactly where the write-side norm changed while the pasted memories
themselves stayed on the shelf, and the single cross-problem transfer in
the corpus arriving through lexical search rather than through the
pipeline built to deliver it, having been counted as zero by three
separate instruments.

## 1. What this draft is

### 1.1 Relationship to V1 and V2

V1's architecture stands unmodified: evidence store, reviewed attachments,
warrant fields, the recall ladder. V2's method — treat our own instruments
as the object of study — is inherited and extended from *instruments* to
*operations*: where V2 audited codings and denominators, V3 audits a live
campaign day and two preregistered batches, with an operator hierarchy
(frontier model supervising, mid-tier operating, third-party measuring)
that is itself part of the reported system.

### 1.2 The thesis

A memory system for proving agents fails silently by default, at every
layer, including in its own countermeasures; the workable responses are
(i) enforcement by construction, (ii) instruments that make absence loud,
and (iii) a standing culture of auditing the instrument — and once those
hold, the measurable value of memory concentrates on the demand side
(knowing what is missing, precisely, with the asker's vocabulary attached)
rather than the supply side (pushing stored content at runners).

## 2. The silence chapter

### 2.1 The catalogue

Each instance below presented as normal operation and was discovered only
by an independent probe. Dates 2026-08-10/11; refs in parentheses.

1. **The dead pipeline.** Zone's dispatch CLI resolved the substrate to a
   port serving either nothing (connection refused, typed into
   `store-unavailable`) or a fresh 16-row shadow store, while the real
   137,904-row index sat unqueried one port over. Every recall for the
   Zone campaign era returned `recall-empty` with well-formed receipts.
   Found by tracing one "query-semantics bug" to its store (futon3c
   `01d42841`, `5a36e16c`; E-ground-control-pass-to-zone §2.2). Root
   causes: an env default naming a nonexistent directory; a hardcoded
   fallback to a port retired weeks earlier.
2. **The unbootable master.** A 623-line file required by HEAD was never
   committed; every clean checkout exited status=2 at boot. Latent because
   the one machine that ran it held the file untracked (`26308814`).
3. **The null experiment.** A 395-job A/B ran to completion with every
   outcome column null — 1,337 rows recording that work was dispatched,
   never what came of it. Thrown out (report §2.6 of the handover).
4. **Display without persistence.** The v1.3 `memory-use/kind` field
   rendered in dry-run output while the contract function silently dropped
   it from stored receipts; found only because a frame's own receipt came
   back kindless (`dc52b161`).
5. **The unfalsifiable success report.** A seat re-registration returned
   "ok" — verifying only that the seat existed, not that the new
   configuration took; the stale configuration served three review passes
   (fixed `9c443e24`; compounded by a pouch-compatibility check that
   ignored permission mode, `7b2bf54a`).
6. **The operator's twin.** A JVM restart silently re-pointed a dispatch
   identity at a fresh session; completion callbacks then spawned a
   headless second incarnation of ground control which ran (correct,
   methodical, uncoordinated) experiments under the supervisor's name.
   Detected only because a notification instrument built two hours
   earlier surfaced its jobs (journal `invoke-trace` + session forensics).
   Recurred 2026-08-11 in a subtler form: with both registry names bound
   to the SAME session, their separate turn queues still spawned two
   concurrent processes resuming one session — parallel incarnations
   that each did real supervisor work (verdict, merges) without
   recognizing the other's, each mis-attributing the other's commits.
   Closed structurally: the alias name now forwards its turns into the
   primary's serialized queue. Same session ≠ same identity; the queue,
   not the session id, is where singularity lives.
7. **Sentinels inside the countermeasure.** The frames system — built so
   that unfilled slots are structurally impossible — accepted the literal
   string "MISSING" and the keyword `:pending-pair` as filled slots.
   Found not by the author but by a fresh operator's induction audit
   (batch-1-report errata; `8d205959`).
8. **Fictitious isolation.** Frame records minted random UUIDs into their
   `:session` slot while the runner seats carried one session across their
   entire job queue — fresh-session-per-run, a preregistration condition
   of both batches, never held, and the record asserted it did. Worse
   than an empty slot: it reads as evidence. Survived two batches and two
   supervisor reviews; found by the operator joining frame records
   against live job session-ids mid-batch (batch-2 amendment 3; batch-1
   erratum D4; `66a52638`). The decisive experimental stratum was
   salvaged by re-running it on supervisor-verified session resets with
   the actual session id recorded at close.
9. **The gate that indicted the innocent.** The attribution-compliance
   gate's regex tolerated a backtick before the memory id but not before
   the keyword; one seat wraps whole attribution lines in backticks, so
   its complete, correct attributions failed invisibly and the gate
   deposited compliance-failure rows naming the seat — false evidence
   with an agent attached. Sub-finding: the gate's once-per-run
   idempotency (designed to prevent double-counting) froze the false
   verdicts in its state store — one of TWO live blockers (stale
   bytecode cache was the other; the operator established both by
   experiment), so the parser fix appeared inert on the normal path
   until the frozen adjudications were cleared with an audit trail. Operator-found, mid-batch, with two self-refuted
   hypotheses en route to the one-character root cause (`8a5ad3c9`;
   retractions e-retraction-gate-*).
10. **The transport that acknowledged into the void.** Replies written
    inside auto-bellback turns are routed to no recipient — a deliberate
    loop-safety rule ("a bellback never bellbacks") that doubles as an
    unmarked dead-letter office. The transport accepted these messages
    exactly as it would for a live recipient: no error, no bounce, no
    missing field. The batch operator, unaware, filed escalations there
    for a full session — including a decisive-pair halt that sat unread
    three hours — and misread the resulting pattern as bell latency
    ("several of my findings reached you only because you happened to
    bell me on an adjacent topic"). Found not by any instrument but by
    the supervisor noticing that content known to exist had never
    arrived. Fixed by norm plus net: an explicit-bell rule in the
    operator runbook, and a watcher that scans void-routed replies for
    escalation markers and re-sends them as real bells (`312fca36`).
    The net's first live firing caught the very escalation that
    motivated its construction; a deliberate void-path test by the
    operator then verified the post-launch path within one poll cycle
    (the operator having first — correctly — refuted a working-path
    test as evidence).
11. **The ban that never existed.** The dispatch packet's acceptance
    gate turned on "axiom-clean" — a term the packet uses (headline and
    acceptance clause) but never defines; its only operationalization is
    the sorryAx test. Three runners produced three sincere readings: two
    strict (refuse `native_decide`; one discarded a working proof to
    comply), one narrow (sorryAx-only — from the one runner that audited
    explicitly, recorded `#print axioms` verbatim, and judged the
    entries permissible under the only test given). Both strict runners
    cited a packet prohibition of `native_decide` that does not exist —
    zero occurrences at HEAD — and the supervisor filed a "norms carried
    by session experience, not packet text" ruling on top of the
    citation without checking the text it presupposed. The gate looked
    enforced because most runners happened to over-comply; the
    enforcement was coincidence wearing the costume of a rule. Operator-
    caught (interview cross-check against the template; batch-2
    amendment 6, which retracts amendment 5's finding). Distinctive
    twist: here the well-formed-but-unmoored records were the agents'
    own sincere self-reports — and one supervisor ruling — not a
    machine's.
12. **The isolation that scoped only writes.** The frame contract
    confined each experimental arm's WRITES ("work only in {checkout};
    commit on the frame branch only") and was silent about reads; all
    ~44 frame worktrees sat world-readable in one directory, sharing one
    git object store. A verified-fresh-session runner read a prior arm's
    frame off disk — its interview cites "control-frame notes" whose
    text greps to exactly one branch and zero in base — and calibrated
    its behavior from them; the next scheduled re-run's own answer (a
    committed proof transport) was equally on disk waiting. Every twin-
    independence claim in two batches rested on isolation that was
    requested, never enforced; no runner violated any instruction.
    Self-reported by the runner, operator-verified, supervisor-
    reproduced (batch-2 amendment 7); fixed by quarantine-before-
    dispatch (bundle + worktree removal + ref deletion, closing the
    disk channel and the shared-object-store channel) and, for the next
    batch, read-scope in the contract and worktree removal at close.
13. **The evidence base with no past.** The frame corpus itself — 44
    records across three batches carrying every receipt id, axiom
    block, session id, obstruction, and twin reference the reports
    rest on — was never under version control: a gitignore pattern
    excluded the data directory wholesale (and a second, later pattern
    silently deadened the file's own existing negation lines). No
    history, no backup, no audit trail; silently editable; the
    validation gate reads the same unversioned files and passes
    regardless. Discovered only when the operator, executing a
    supervisor ruling that said "version control IS their append-only
    history" and citing earlier repairs as recoverable-from-git
    precedent, ran `git add` and was refused — the ruling's premise
    had never been checked, and the cited precedent repairs had
    themselves been unrecoverable all along. The deepest specimen in
    the catalogue: not a field, a receipt, or a gate, but the ground
    the records stand on, looking exactly as authoritative absent as
    present. Fixed by un-ignoring and committing the corpus
    retroactively (history begins the day of discovery; pre-history is
    attested only by reports and transcripts) and queueing close-time
    auto-commit for the next batch.

**Second tranche, 2026-08-24/25.** The instances above are two days in
August. The campaign continued, and the frames that followed (f28–f35,
§5a) produced six more — the interest of which is that four of them sit
inside instruments built *after* the catalogue above was written, by
people who had read it.

14. **The identifier the machine asked a model to copy.** The Student's
    terminal submission required it to hand-transcribe 21
    controller-minted memory UUIDs; on f32 it got one character wrong
    (`aa5e` for `aa5a`), the set-membership check rejected the report,
    the frame's single repair budget was already spent on a different
    failure class, and the coordinator went `:failed` with no successor
    state. The invariant being protected was never violated — both
    claimed uses were on the reviewed shelf. The field that failed does
    not exist in the Lean model: it lives inside a residual hole that was
    declared open and never closed, so the validation was correct *up to
    that hole*, and the hole is where every LLM-authored field lived. The
    system was specified in Lean, translated to Clojure, passed
    qualification, and still contained this (`TN-fable-F32-model`; fixed
    `114ea24e`, hole closed).
15. **Eight hours of a siren talking to nobody.** The same failure was
    correctly diagnosed within forty minutes and then sat untouched
    overnight: the on-duty agent treated repairing the apparatus as
    needing an authorization it could have obtained from any idle agent
    on the roster, while the watchdog belled it 48 times from an identity
    (`claude-cli`) that is not registered — so all 48 replies were
    delivered nowhere, and the watchdog has no code path that reads a
    reply. Two incident keys fired for one condition; the 48th bell was
    identical to the first. No message was addressed to the operator;
    there is no channel for one (`TN-fable-F32-process`).
16. **The budget that was written down nowhere.** Operator and reviewer
    spent an evening reasoning about a "30-minute Student attempt".
    Searching the entire lab tree for it returns zero hits: the role card
    said 120 minutes, the harness enforced a flat 60 across every seat,
    and 30 existed only in the room. A three-way mismatch, discovered by
    asking why an attempt was still running at 42 minutes
    (`TN-sonnet-F33-finding`; capped `d21c3dcf`).
17. **`cannot-judge` began to certify.** A one-line change admitted
    `:cannot-judge` to the set of valid review verdicts, so that a
    reviewer unable to fetch its candidates no longer blocked a frame
    (`be9978b5`). Frame f33 then ran three review passes in which **11 of
    11 candidates** were `cannot-judge` — persisted evidence 404, and a
    validator defect described below — and closed with a well-formed
    certified receipt. The frame's own close audit recorded it as
    findings F1/F2/F4; nothing in the receipt shape did. A review that
    reviewed nothing is now a review that passed.
18. **The isolation invariant that quietly stopped being true.** "No
    memory used that was not on the reviewed shelf" is the property the
    snapshot machinery exists to enforce. Open corpus search was wired on
    2026-08-24; from the next frame on, Students reached memories that
    were never on any shelf — f33 attempt-1 used a 2026-07-31 memory from
    a different problem entirely, f33 attempt-2 used an unpromoted guide
    memory. Both are recorded, in the frame's own findings, as the
    channel *working*. Neither the receipt nor the preregistration
    marked that the invariant had been retired, and the shelf machinery
    continued to report as though it were the only door.
19. **"The watcher ingests this file."** Both new scribe seats, unable to
    introduce a coined pattern through the deposit path (item 20), wrote
    their patterns into markdown side files opening with that sentence.
    No code in any of the three repositories reads them; none of the
    seven coined pattern ids exists in the substrate or the library. The
    deposits were then attached to canonical patterns that do not fit,
    and rejected at review for exactly that — a documented workaround
    for a defect, which produced well-formed evidence of a different
    failure.
20. **A string where a keyword was.** Underneath 17 and 19: the
    pattern-accounting validator requires a rationale for each newly
    coined pattern id, looks that rationale up by *string* key, and
    receives a map whose keys JSON-parsing has turned into *keywords*.
    Every coined id is therefore unaccounted and every such deposit
    fails, unconditionally, since it was written. Diagnosed by one of the
    affected agents in its own lane report, which is the only place it is
    recorded.

### 2.2 The argument

The instances share one mechanism: **a record whose well-formedness is
independent of the reality it reports.** A receipt can be schema-perfect
about a query that never reached a store; a success string can verify
existence rather than effect; a sentinel can satisfy a presence check.
Vigilance does not fix this — the campaign's operators (human and model)
read these records repeatedly without registering absence, exactly as
V2's unenforced-docstring fields were read by two reviewers across eleven
passes. What fixed each instance was one of three moves:

- **Enforcement by construction**: make the invalid state unrepresentable
  (frames' closure slots; the sentinel refusal; permission-mode joining
  pouch compatibility). The staging bank's ValidatedTrace lesson (H6),
  applied at operational altitude.
- **Instruments that make absence loud**: the operator bellback watcher
  (which caught instance 6 on its first day); batch validation exiting
  nonzero; the sweeper's idempotent re-sweeps.
- **Audit the instrument**: the induction protocol that produced instance
  7's discovery is now standing practice — a fresh reader, a checklist
  whose first verb is *verify*, and explicit permission to fail the
  author.

The prior series' claim (I3, staging bank): norms must be authored before
recurrence becomes visible. V3 adds the operational corollary: **authored
norms decay into sentinels unless some instrument or fresh reader is
charged with disbelieving them.**

The second tranche sharpens this rather than merely lengthening it. Items
14, 17, 19 and 20 are defects *in the countermeasures*: a Lean-specified
cycle whose validation was sound up to a declared hole that happened to
contain every model-authored field; a review verdict added to keep frames
moving that turned an unreviewable pass into a passing one; a workaround
for a validator defect that manufactured well-formed evidence of a
different failure. Item 18 is the sharpest, because nothing broke: a new
capability was wired correctly, and an invariant the surrounding machinery
still asserts simply stopped being true, with the frames recording each
violation as a success. The mechanism is unchanged — well-formedness
independent of the reality reported — but the second tranche says
something the first could not, since the first was written by people
discovering the problem and the second by people who had already named
it. **Knowing the failure mode does not confer immunity to it; the only
things that helped, again, were construction and a fresh reader.** [n: 20
instances over two episodes, four days apart in a six-week campaign; a
catalogue with a mechanism, not a rate.]

## 3. The witness standard, and what survived it

### 3.1 The fingerprinted chain (existence, n=1)

a94A09, 2026-08-10: dispatch-time recall surfaced two memories; the
runner's Memory-usage section reported one USED, one IGNORED with reason;
the committed Lean (`apm-lean 22c5b80c`) contains
`apm_a94a09_exists_scaled_fixed_point`, whose statement is precisely the
USED memory's prescription
("package all elementary hypotheses in the theorem's intended quantifier
shape") instantiated. Offered receipt `e-fab2e3d9…`; outcome
`e-memory-outcome-sweeper-6e8a041a…`. This is the series' first
end-to-end witnessed chain on a clean index, and it fixes the standard:
**a USE claim counts when the artifact carries the memory's fingerprint;
prose attribution alone is design signal, never outcome data.**

### 3.1a The standard applied to a whole campaign (n=35 events)

The a94A09 chain was hand-checked. §5a's campaign made it cheap to apply
the standard mechanically to every USE claim on record: for each
(attempt, memory), extract the Lean identifiers the memory names, look for
them in the attempt's archived closing artifact, and separately measure
the longest run of memory-body lines appearing verbatim there
(`analysis/fingerprint_audit.py`, reading
`fingerprint-2026-08-25.json`).

**Both measured against the base file the Student was handed.** This is
the step that decides whether the number means anything: memory, base file
and certified head discuss the same mathematics in the same vocabulary, so
an identifier already present in the base is not evidence that a memory
put it there. Raw matching reports 29 of 35 events fingerprinted;
differencing against the base — resolved from each attempt receipt's own
`:base-revision` and `:problem-path` — reports 23. The six events that
move are the difference between a measurement and a well-formed number.

Over 35 use events (19 attempts, 13 with a USE claim, frames f28–f34):

| | events |
|---|---|
| fingerprinted (identifier novel to the artifact) | 23 (18 distinct memories) |
| paste (8–25 consecutive body lines verbatim) | 6 (6 distinct memories) |
| matched only identifiers already in the base | 6 |
| unwitnessed | 0 |

Three results. First, **no USE claim was unwitnessed** — every
`:used-ids` entry left some trace in the artifact. Set against §3.2, where
prose attribution repeatedly failed corroboration, the difference is the
channel: a self-report made inside a machine that binds it to a
controller-derived surfaced set and archives the artifact behaves
differently from a self-report made in an interview.

Second, **the six paste events are confined to two frames**, and they are
the same six proof-text blocks an independent review had flagged as still
being seeded campaign-wide. The audit reaches that set from artifacts
rather than from bodies. All six remained on every subsequent shelf and
were never used again after the scribe role card was split and given
explicit proof-text limits (≤3 tactic blocks, ≤4 KB, whole-declaration
copies rejected). Availability held constant while the behaviour changed
— the closest thing to a controlled before/after the corpus offers on this
failure mode, and evidence that the write-side norm did what it was
written to do.

Third, the audit's *negative* rows are its validation. Six events report
that the memory's identifiers were present but all were already in the
base — and they fall in exactly the attempts independently known to have
produced nothing: one whose budget went entirely on signature
verification with no Lean written, one that died on the mistyped
identifier of §2.1 item 14. The instrument was not told either fact.

The limits are worth stating with the result. A fingerprint is a
*necessary* condition: it rules out the unwitnessed claim; it does not
establish the counterfactual, which only the ablation arm can. A hit can
even be a refutation — one counted identifier appears in the artifact
because the Student engaged with a memory and *corrected* it. And these
are events, not memories.

### 3.1b The first cross-problem chain (existence, n=1)

a94A07, 2026-08-25, frame f33 attempt 1. The Student abandoned the base
file's own declared route, searched the corpus twice, and on the second
query — phrased in the vocabulary of the route it had just chosen, not of
the problem — surfaced a memory deposited three weeks earlier by a
different agent working a different problem
(`e-codexpilot-force-a-sublinear-entire-function-constant-by-Cauchy-derivative-estimates`,
witness commit apm-lean `662b9ec` = a94J08). The closing artifact carries
that memory's construction and two of the three APIs it names; the third
it does without. The problem closed at 0 sorries under standard axioms,
independently recompiled.

Three things make this the most informative single event in the corpus.
It is the **first USE of a memory mined from another problem** — every
other use to date, 25 of 26 distinct memories, was on the problem the
memory came from, which is a cache rather than a store. It **bypassed the
entire promotion pipeline**: no scribe, no proctor, no shelf; the memory
was reached by lexical search on obstacle vocabulary, which is precisely
the retrieval model §4.2's users described and the anchor mechanism did
not implement. And the Student's own account states the mechanism —
its first query, in the problem's vocabulary as the file's comment framed
it, returned one irrelevant hit; the second, in the vocabulary of its
obstacle, returned this.

It was also recorded as zero, three times over: the frame's close audit
counted the attempt as memory-free, the store's reuse metric reported no
cross-problem use, and the campaign summary carried that number forward.
Each instrument was well-formed. The origin rule could not resolve
memories whose subject is a mission rather than a problem — which is
exactly the naming hygiene the scribe cards demand — so the memories with
the best hygiene were the ones that could never be counted as reuse. The
rule now falls back to the commit a memory cites as its witness; the
campaign's cross-problem reuse count is 1, not 0. §2.2's mechanism, this
time in the measurement layer rather than the machine.

### 3.2 What the standard did to attribution (batch-1 preview)

Under paired blind dispatch (batch-1, prereg + report in
`holes/labs/M-zai-learning-loop/`), both attributed USEs in ten paired
problems adjudicated as marginal: one self-reported speed benefit was
contradicted by the memoryless twin closing the same problem faster
(6.6 vs 4.3 min; `e-use-adjudication-a01A05` + timing addendum); the
other was behavior the packet already mandated
(`e-use-adjudication-t00A02`). The plausibility rubric V2 relied on
(38% "load-bearing") would have passed both. Twins are cheap; judgement
is expensive and wrong. [Full batch results: §5.]

## 4. Retrieval, characterized by its users

### 4.1 The defect

Anchor-term selection ranked candidates by rarity in the *problem*
corpus, which selects artifact vocabulary ("belonging", "compiled") and
— the decisive instance — **inverted relevance**: a memory describing
problem a01A12's exact conformal map surfaced for the wrong problem
(a01A01, correctly ignored as noise) and failed to surface for its own,
whose runner then re-derived the content and confirmed on interview that
the memory would have accelerated the pass
(`e-retrieval-miss-a01A12-slit-wedge` + victim statement).

### 4.2 The users' requirements

Exit interviews across twelve dispatches yield a consistent search model:
runners find reusable work by **engine names** (grep for the Mathlib
declarations they are about to use — which located a
parameter-for-parameter reusable sibling) and by **structural
similarity** (a control runner, unprompted: "dispatch-time retrieval of
the structurally similar b94J03 lemma would have closed the finite
branch immediately") — never by concept vocabulary, which is what the
anchor mechanism searched. Requirements written by the consumers.

### 4.3 The repair, as an experiment

Per the bank's A2b rule (a repair that does not move outcomes is worse
than none), the fix shipped as a switchable contrast, not a default:
anchors drawn from the pre-cap term pool, filtered to a memory-corpus
document-frequency band, ranked [problem-IDF, memory-df] (`6521fd3a`).
Offline acceptance: the a01A12 miss reverses (its memory surfaces, anchor
"slit") with the negative control clean and the default path
byte-identical. Three plausible ranking rules failed before this one —
IDF-tie-by-order, and max-df-in-band, each producing a different wrong
anchor — recorded because the failures are the argument for
preregistration. **Status: under live test as batch-2 (prereg
`batch-2-prereg.md`; falsifier on surfacing relevance, outcomes
predicted null; blind relevance scoring by a third-party model lane).**

## 4a. The delivery layer [DRAFT — claude-19, 2026-08-26]

§4 characterised retrieval by its users. This section is about the step after
retrieval and before use: what a student is *handed*, in what order, and
through what structure. V2 §4.6 established that the store's graph was never
populated — one pattern per memory, a forest of stars — and left open what
that costs. §5a's students are the first consumers whose shelf is archived
per attempt, so the cost can now be read off the record. Every number below
is from a real artifact (`holes/f42a-cascade-run-cap{100,1000}.edn`,
`holes/technotes/D1-round1-cascade-offers-2026-08-26.md`,
`holes/labs/M-apm-demonstration/analysis/shelf-order-2026-08-26.json`); the
finding they refine is `holes/technotes/TN-APM-cascades-exist-unused.md`.

### 4a.1 What the student is handed

Through f42 the shelf was `:snapshot/memories`: a flat vector of reviewed leaf
memories **sorted by `:memory-id`** — hash order, carrying no relevance
signal, because `(sort-by :memory-id candidates)` was never a design decision.
On f42 the memory that closed the problem sat at position 47 of 48. Across the
12 archived frames with snapshots, the used memory's median position under
that order was **18.5** on shelves of 20–50 (56 positioned use events). The
packet text itself was not archived until f43 (§2.2's mechanism at the
independent variable: the snapshot stood in for the prompt, so "surfaced and
ignored" and "never shown" were indistinguishable).

Two facts about the shelf entries matter for everything downstream. First,
**586 of 1,099 snapshot entries on disk carry no text** — older promotions are
recorded by content digest only — so any ordering or scoring that reads the
snapshot alone scores them zero. Second, an entry's `:provenance :problem-id`
is the problem of the *frame that promoted it*, not the problem it was mined
from.

### 4a.2 Ordering: measured offline, then shipped

Two keys were proposed and scored against the archive before touching the
pipeline: *promoted-this-frame first* (§5a: 25 of 26 distinct used memories
were used on the problem they came from), then *distinct Lean identifiers
shared with the base problem file*, then id.

| ordering | median position | mean | top-10 of 56 |
|---|---:|---:|---:|
| delivered (hash) | 18.5 | 17.0 | 19 |
| promoted-this-frame first | 2.5 | 4.6 | 52 |
| identifier overlap | 3.0 | 5.3 | 48 |
| combined | 3.0 | 3.5 | 54 |

The first key carries most of the gain and is, in §5a's own vocabulary, a
cache effect. The second is what moves the five cross-problem uses (median
15 → 9) and is the only key that rescues f42's closing memory (47 → 7 of 48,
sharing `closedBall`, `DiffContOnCl`, `norm_le_of_forall_mem_frontier_norm_le`
with the base file). The audit had to be corrected once, for the textless
entries, before it said that. The combined ordering is live from f43 as
`:snapshot/version 2`, with the signal, per-memory scores and a
`:base-text-present?` flag recorded on the snapshot (prereg amendment 6).

### 4a.3 The cascade: built, run, delivered to nobody

`conductor.clj` carries a memory cascade in exactly the shape V2 modelled —
`:leaf` at zero hops, then `:why-hop` along authored `pattern/has-semantic-why`
edges, then `:co-incidence` through shared problems, cheapest route per
memory, a cap and a `:truncated?` flag. Its history is a silence instance in
its own right:

- it is not on the path that feeds §5a's students (`countdown_control.clj`
  does not reach it; no campaign directory contains a route label);
- it **did** run on the round-1 conductor path in four frames (f9, f10, f13,
  f15), where 1–5 seed memories expanded to 115–132 available offers, the cap
  of 100 fired every time, and the offers were computed *after* the solver's
  attempt and persisted under `data/problem-state/` — read by no one, since
  every student arm on that path had `:memory-channel :none`;
- from 2026-08-23 it could not run at all: its reader asked the substrate
  for a 5,000-row window three days after the substrate began refusing
  anything over 1,000, and the failure was a thrown exception no path
  reached (fixed `7534419c`; a full window is now refused, not truncated).

### 4a.4 What the cascade adds, on real inputs

Run for the first time with live readers over f42's 48-memory shelf (cap
1,000, deterministic on re-run): **103 additions — 48 by why-hop, 55 by
co-incidence.** All 48 why-hop additions arrive through one pattern,
`math-strategy/missing-dependency-protocol`, and are exactly its 48 reviewed
attachments. The same 48 arrived on f10 (2 seeds) and f15 (5 seeds). **The
why-hop expansion is constant in the seed**: any shelf touching an API
pattern that declares `@why missing-dependency-protocol` receives the whole
hub. Over this store the cascade is one fixed shelf appended to every shelf.

Judged against what f42 actually hit — the student's own failure account
names the crux as extending open-arc bounds to the sphere's endpoints — none
of the 103 additions bears on it; two process memories bear weakly on a
one-round-trip `sorryAx` false positive (`holes/f42a-H4-judgement-2026-08-26.md`).
That is the outcome the plan named in advance as *volume without relevance*,
and it is a fact about the store, not the expander: the seed patterns' `@why`
edges lead, once seeds and memory-less targets are removed, to a single node
with 48 children and no structure among them. Descent from a high-level
pattern does not narrow here; it dumps. V2 §4.6 predicted the shape; this is
the shape's cost, measured from the consumer's side.

### 4a.5 What follows

The order of repairs falls out of the numbers. Ordering the shelf helps
whatever happens to the graph and shipped first. Populating the graph —
repointing the four `@why` declarations that make the protocol every API
pattern's parent, and attaching the hub's seven mathematical statements to
the API regions they are about — is the precondition for any descent that
narrows (`holes/PLAN-H5-populate-the-graph.md`). Wiring the cascade onto the
student path is last, gated on a re-run showing an expansion that varies
with the seed, and would ship why-hop-only with co-incidence off. None of
this is an outcome claim: the claim ladder of §5a is unchanged, and "a
cascade built *and used*" is true nowhere in the stack as of this draft.

[n: one frame judged (f42), four round-1 frames reconstructed, 12 frames
scored for ordering. The ordering result is offline over archived attempts,
not a controlled comparison; f43 onward measures it live.]

## 5. The ladder results [SKELETAL — batch-2+ pending]

Batch-1 (B1: channel on/off): falsifier fired; sorry deltas identical
per arm (−6/−6); noise floor low and canyon-shaped (twins converged to
identical proofs and once to an identical unprompted corpus repair);
divergence only where the library offers competing engines. Nine labeled
twin-diffs; one semantically-same/syntactically-different proof pair.
[Tables from frame records; batch-2 results; cumulative P7.]

## 5a. Transfer between unequal agents: the frame campaign [DRAFT — f29–f34 closed, f35 in flight]

§5's ladder varied the memory channel between two runs of the same model.
The campaign that followed it varies the *model*: a strong solver closes
a problem, the memories mined from that close are the only channel to a
weaker student re-proving the same problem cold. This is a different
claim from anything in §5 and is preregistered separately
(`prereg-capability-transfer-v1`, written 2026-08-24 before its
confirmation units existed).

**The design.** One frame = one problem. A Codex solver closes it and its
trace is mined by a scribe into candidate memories; an independent
promotion proctor reviews them (reviewer ≠ depositor); approved memories
form a shelf. A Zai student then attempts the same problem three times,
each a fresh cold session with no memory of the last, holding the base
file and the shelf — and, from f32, a live corpus search tool as well
(the boundary noted at the end of this section; before it the shelf was
the whole channel). Between attempts a guide reads the student's
failure account and may deposit further memories. Every phase emits a
receipt into an append-only ledger; a frame missing a required slot is
`:incomplete`, never scored — §5's frames discipline at a larger
granularity.

**The claim ladder, and where it actually stands.** The preregistration
separates, following Sen, what the student can *do* from what it *could*
do:

| level | claim | status |
|---|---|---|
| 1 | Codex closes APM problems | established, uninteresting |
| 2a | memories carried through the medium are USED by a weaker student | **established**: 20 fingerprinted events over 15 distinct memories in the confirmation set (23 / 18 including the excluded pilot frame f28 — §3.1a) |
| 2b | the memories are LOAD-BEARING — the same student without them fails | **not run**: the ablation arm does not exist |
| 3 | a stocked store lets the student close problems beyond its independent reach | not attempted |
| 3b | the same medium lifts a *human* student | separate phase, not in this registration |

The gap between 2a and 2b is the whole of what this section cannot yet
say. Every result below is about usage, not benefit. The ablation is
cheap to describe and has not been run: it requires student rounds with
memory withheld on problems the memory arm closed, and its power band is
narrow — too easy and both arms close, too hard and both fail.

**What the frames show so far.** The registration's confirmation set is
F29 onward — F1–F28 are excluded because the causal path from solver
memory to student did not exist, an exclusion documented before the
registration was written and turning on the *instrument*, not on the
results. Through f34 that set is five reported frames (f29, f30, f32,
f33, f34; f31 void, no student attempts), twelve student attempts
carrying a USE claim, 32 use events: 20 fingerprinted over 15 distinct
memories, 6 pastes, 6 matching only base-file identifiers, none
unwitnessed. All six pastes fall in f29–f30, before the scribe cards
were split. One use is cross-problem (§3.1b). §3.1a's slightly larger
counts (35 events, 13 attempts, 23 fingerprinted) include f28, an
excluded pilot frame, because the audit was run over everything with an
archived artifact; the difference is f28 attempt 3 and its three
fingerprinted events. Under the registration's
total-reporting rule the null and degraded frames are reported with their
named defect rather than dropped, and the unit of evidence is the frame,
not the series — pooling across a continuously repaired instrument is
what the design forbids.

**The supply/demand asymmetry, in the new design.** Of 45 candidates
reviewed across f32–f35, 8 were approved. All 8 came from the guide,
which deposited 14; the two scribe seats deposited 31 and had none
approved — 11 rejected on the merits, 20 returned `cannot-judge` because
the reviewer could not fetch or could not bind them.

Much of that is apparatus rather than judgement, and one instance shows
how much. The single validator defect of §2.1 item 20 has now voided
three guide deposits on f33 and three more on f35 — the latter while
this section was being written, in a frame still running, with the
reviewer recording its own verdict as: *"Approval cannot be persisted
because the typed JSON boundary keywordizes new-pattern-rationales keys
while validate-pattern-accounting looks up string pattern IDs."* It had
judged all three coherent and actionable against named residuals. A
one-token type mismatch is deciding what a student is allowed to know,
and the only place that is written down is inside the verdicts it
produces.

The scribe seats' remaining failures include a design contradiction
worth stating in its own right: the student-side scribe is
chartered to mine *process* corrections — the stumbles a cold agent
repeats — while the proctor reviews every candidate for fit against an
open mathematical residual, which a process rule by construction does not
have. The seat cannot yield until one card gives.

What did yield is the guide: it reads what the student says it lacked, in
the student's own words, and writes one memory against one residual.
That is §6's demand-side result reappearing inside a supply-side
pipeline. Two qualifications keep it honest. These are same-problem
memories — a cache, in the scribe card's own vocabulary — and 25 of 26
distinct used memories were used only on the problem they were mined
from. And on one frame the guide's memories were written *after* an
attempt had already closed the problem without them, the following
attempt then being recorded as a closure with memory; a second frame
shows the same shape with a longer post-intervention attempt than the
successful one preceding it. Whether an intervention between attempts
buys anything is an open question the campaign is now instrumented to
answer and has not yet answered.

**Instrument-version boundaries.** Three changes cut across the frames
and no comparison may span one silently: open corpus search wired
2026-08-24 (so from f32 the student has both a shelf and a query tool,
unrandomized — access mode is collinear with frame ordinal); controller-
derived memory accounting 2026-08-25 (pre-f35 surfacing records are model
transcriptions, after they are controller facts); and the student attempt
cap cut from an enforced 60 minutes to 30 at f35. All three are recorded
as amendments to the preregistration rather than edits to it.

## 6. Demand-side memory: votes, builds, callbacks [DRAFT-READY DATA]

The pipeline: every failed search, hunger memory, and Tier-A gap is a
vote keyed by concept; at threshold a build lane produces the artifact —
spec preferably verbatim from a voter; blocked problems park on
`concept:<slug>` deps and re-run when ground control verifies the
artifact. First revolution, 2026-08-10, ~90 minutes: three votes for
Schwarz–Pick rigidity → `ConstructionTargets.SchwarzPick` (16 axiom-clean
declarations, `10eac91b`, target theorem stated by its voter) → callback
→ one-pass closure of a theorem that had survived three prior closer
hops (`a266157d`, merged `087924c`). The ledger after one day: 14
concepts, three past threshold, every at-threshold entry carrying
voter-written specs, stepping-stone decompositions, demand-query tags,
and in one case a named upstream backport source. [Terms-vs-fit and the
glue census: §7.]

## 7. Terms and fit [SKELETAL — census v2 pending]

6,114 unnamed `have`-steps vs 2,139 named lemmas (~3:1); signature
clustering finds statement shapes, text embeddings find route twins (a
step hand-derived in the evening's work matched its two prior
occurrences at 0.77 cosine), proof-term shapes remain the open rung.
Every fingerprinted or attributed USE to date is fit-shaped (regulative),
none term-shaped: the division-of-labor hypothesis — libraries hold
terms, memory's durable niche is glue — is preregistered as P2's kind
distribution. [v2 extraction with justifications; the model-of-runner
rung may join here.]

## 7a. The same loop over software, and the operator's corrections as the corpus [NOTE — Joe, 2026-08-15]

The system's subject so far has been mathematics: lemmas, fits, need
vocabulary. Nothing in the loop requires that. A single day's work
building this experiment's own apparatus produced the same shapes, over
code rather than proofs, and one shape that the mathematics corpus does
not readily supply.

**What the software session produced, in the mathematics vocabulary.**
The repeated defect was *written but not wired up*, and it recurred at
four distinct levels: a value moved from a comment into EDN and still
read by nothing (the attempt caps; moved `44a3f1e4`, found decorative
`9d9e7a07`, mission I.57); a predicate declared and never called (the
engine's own `:required-outputs` advance gate, present in two docstrings
and read by nothing — I.30; wired `0907fc6c`, `e6caa48f`); a docstring
claiming atomicity the implementation lacked (proof's `save-state!`,
"atomic write and version bump" — I.50; replaced by the append-only
store, `13130808`); and — the deepest — three tools declared in a
**phase the machine could not enter**, because the engine clears the
cycle the moment an advance *returns* the last phase (`44d8fe6d`).
That last one had generated an entire second pipeline around the gap it
left. Each instance is a *fit* in exactly the regulative sense §7
describes: not a term to reuse, but a shape of glue that failed the same
way twice.

**The mineable unit was the operator's correction, not the agent's.**
The agent's self-corrections were already written down — that is what the
mission record is for, and a system that mines only those learns from a
party with an interest in the result. The corrections that moved the work
came from outside it, and several were of recognisable *type*:

- *contradiction in terms* — "three cold attempts in the same tree" is
  either cold or shared; the spec had used a word the role card defined
  otherwise;
- *category collision* — an experiment's validator living in a
  cellular-automata repo whose pins attest to a different result;
- *premise challenge* — "if it is a mechanical check I cannot see how it
  would be self-certification";
- *scope honesty* — "are we talking about 5 total dispatches now, or
  more?".

**Two of those four are mechanically checkable.** A contradiction in
terms is a spec using a term against the definition its own source of
intent gives; a category collision is content whose declared purpose does
not match its container's. Both are comparisons between an artifact and a
document that already exists. The other two are not obviously reducible,
and pretending otherwise would be the supply-side error §1.2 warns about.

**Why this belongs in a memory paper rather than a methodology one.**
The corrections that could be automated are exactly the ones with a
*written referent* — a role card, a README. That is the demand-side
result again, in a second domain: the value was not in storing what the
operator said, but in knowing which of his objections had a document
behind them. **Preregistered as a note, not a result: n=1, one operator,
one day, and the agent doing the mining is the party being corrected.**

**Postscript (2026-08-15, same day).** The class recurred while this
note was being written. An independent pre-run audit of the finished
peripheral (`TN-problem-peripheral-RC-fable-review.md`) found the
round-1 registration's own resource pins — `:reg/environment-revision`,
`:reg/harness-revision` — written into the frozen EDN and read by
nothing in the engine or the validator: a cycle provisioned at the wrong
revision would have validated cleanly. Wiring began the same afternoon
(`29638fde`, harness revision measured from the repository at
cycle-begin). A note about "written but not wired up" was drafted while
a fresh instance sat unwired in the registration it describes — which is
the §2.2 mechanism again: the pins were well-formed, so nothing about
their presence disclosed that nothing read them. The catch came from the
standing countermeasure §2.2 names last — a fresh reader charged with
disbelieving the document.

## 8. Asserted on our own authority [TO MAINTAIN]

- The twenty-instance catalogue is complete for its two periods as far as
  we know — by construction we cannot know it is.
- Relevance scoring blindness (batch-2) is procedural, not cryptographic.
- All twin comparisons carry a two-seat confound; seats traded
  fastest-arm honors across batch-1 pairs, but no formal seat calibration
  exists yet.
- The operator hierarchy's economics (frontier/mid/third-party split) is
  reported from one day's practice.
- §3.1a's fingerprint is a necessary condition on a USE claim, not a
  causal one: it establishes that the artifact carries what the memory
  named, never that the student could not have got there alone. §5a's
  level-2b is the claim that would need the ablation, and the ablation
  has not been run.
- The identifier extraction behind §3.1a is a regex over memory bodies,
  calibrated against one hand-checked case (§3.1b) and validated by its
  negative rows. It cannot distinguish a memory's identifier being *used*
  from its being *corrected*; at least one counted event is the latter.
- §5a's paste before/after holds the availability of the six pasted
  memories constant, not the problems, the students, or the rest of the
  pipeline. It is a controlled comparison in one factor, not a trial.
- Every count in §5a is over frames run on a continuously repaired
  instrument. That is a stated condition of the design rather than a
  defect in it, and it is why the unit of evidence is the frame with its
  revision attached, not the pooled series.

## Appendix A. Artifact index [TO MAINTAIN]

Frames + twin-diffs: `data/experiment-frames/batch-*`. Receipts and vote
ledger: substrate, session "vote-and-callback-pipeline", tags
:concept-vote :glue-census :use-adjudication :retrieval-miss. Runbook:
`E-batch-operator-runbook.md`. Day synthesis: `E-2026-08-10-learnings.md`.
Priors: `E-memory-priors-survey.md`.

**§4a (delivery layer).** Real cascade runs `holes/f42a-cascade-run-cap100.edn`,
`-cap1000.edn` (`scripts/apm-cascade-dry-run.sh`); counterfactual
`holes/f42a-cascade-example.edn`; judgement `holes/f42a-H4-judgement-2026-08-26.md`;
round-1 reconstruction `holes/technotes/D1-round1-cascade-offers-2026-08-26.md`;
ordering audit `holes/labs/M-apm-demonstration/analysis/shelf_order_audit.py`,
`shelf-order-2026-08-26.json`, `NOTE-shelf-order-audit-2026-08-26.md`; finding
and addenda `holes/technotes/TN-APM-cascades-exist-unused.md`; plans
`holes/PLAN-apm-cascade-demo-instance.md`, `holes/PLAN-H5-populate-the-graph.md`.
Packet archive from f43: `live/<phase>-packet.txt` (prereg amendment 5);
ordered shelf from f43 (amendment 6).

**§§2.1 items 14–20, 3.1a, 3.1b, 5a (frame campaign).** Frame records,
per-phase receipts and archived attempt sources:
`data/apm-campaigns/jit-all-open-nontopology-v1/*-f{28..35}/`.
Preregistration and its amendments:
`holes/labs/M-apm-demonstration/prereg-capability-transfer-v1.edn`.
Fingerprint instrument and reading:
`holes/labs/M-apm-demonstration/analysis/fingerprint_audit.py`,
`fingerprint-2026-08-25.json`, write-up
`NOTE-fingerprint-audit-2026-08-25.md`. Store shape and origin/reuse rule:
`analysis/memory_shape.py`, `memory-shape-2026-08-25.json`. Silence
instances: `holes/technotes/TN-fable-F32-model.md` (14),
`TN-fable-F32-process.md` (15), `TN-sonnet-F33-finding.md` (16),
`TN-fable-F32-F35-bank-review.md` (17–20, and the review these sections
came from). Role cards under `holes/labs/M-apm-demonstration/role-cards/`:
`codex-scribe-v1`, `zai-scribe-v1`, `promotion-proctor-v3`,
`zai-student-v2`. Prior review of the pasted population:
`holes/excursions/E-early-memories-review.md`.
