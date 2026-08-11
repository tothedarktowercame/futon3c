# Silence, Witness, and Demand: an Agent Memory System Audited by Its Own Twins

**V3 — DRAFT, Phase 1 sections (2026-08-11). Batch-dependent sections are
skeletal and marked.** Successor to V1 ("Warrant-Disciplined Agent Memory:
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
charged with disbelieving them.** [n: 7 instances, one day; this is a
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

## 5. The ladder results [SKELETAL — batch-2+ pending]

Batch-1 (B1: channel on/off): falsifier fired; sorry deltas identical
per arm (−6/−6); noise floor low and canyon-shaped (twins converged to
identical proofs and once to an identical unprompted corpus repair);
divergence only where the library offers competing engines. Nine labeled
twin-diffs; one semantically-same/syntactically-different proof pair.
[Tables from frame records; batch-2 results; cumulative P7.]

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

## 8. Asserted on our own authority [TO MAINTAIN]

- The seven-instance catalogue is complete for the period as far as we
  know — by construction we cannot know it is.
- Relevance scoring blindness (batch-2) is procedural, not cryptographic.
- All twin comparisons carry a two-seat confound; seats traded
  fastest-arm honors across batch-1 pairs, but no formal seat calibration
  exists yet.
- The operator hierarchy's economics (frontier/mid/third-party split) is
  reported from one day's practice.

## Appendix A. Artifact index [TO MAINTAIN]

Frames + twin-diffs: `data/experiment-frames/batch-*`. Receipts and vote
ledger: substrate, session "vote-and-callback-pipeline", tags
:concept-vote :glue-census :use-adjudication :retrieval-miss. Runbook:
`E-batch-operator-runbook.md`. Day synthesis: `E-2026-08-10-learnings.md`.
Priors: `E-memory-priors-survey.md`.
