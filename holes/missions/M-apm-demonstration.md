# M-apm-demonstration

**Status:** HEAD complete; IDENTIFY draft pending operator acceptance (2026-08-14)
**Gate:** operator-acceptance — HEAD must be recognised as faithful to the
mission's live shape before IDENTIFY hardens it. Per the lifecycle, the
operator clears this by editing the file.
**Gate:** operator-decision — IDENTIFY's completion criteria (§1.5) name
thresholds that are proposals, not settled numbers.

**Note on phase discipline (Joe, 2026-08-14).** An earlier draft of IDENTIFY
carried design conclusions from `capability-proof-apm-v2-plan.md` — "N7 is the
keystone", the measurement vector, `L(i)`'s form, a dispatch order — as though
they were settled. They are not: they were written before any survey of what
the loop already emits, which is MAP's job. They now live in the **N-register
(§1.4)** as open directions, each with the question MAP must answer. The
lifecycle is explicit that MAP "produces facts, not decisions"; importing
DERIVE-level design into IDENTIFY is the failure HEAD's exit criterion warns
about — pretending the unknowns have already been settled.

Supersedes `E-apm-halftime-pre-go-live.md`, **closed 2026-08-14**: its 24
items are discharged, corrected in place, or explicitly parked. What remains
is not repair but construction, so it is a mission rather than an excursion.

`E-apm-halftime-pre-go-live-EF.md` remains live **as an empirical source**,
not as the plan. It is the informal description of the system; this mission is
the formal one.

---

## HEAD

*Per `futon4/holes/mission-lifecycle.md`: HEAD preserves the operator's voice
and carries tensions forward before IDENTIFY hardens them. It is not design.*

### Operator-voice anchor

Joe, 2026-08-14, in his own words across the session that produced this file:

> "Our actual plan to develop a meaningful *training* signal using the
> multiagent system, to look at how it improves things per problem, and to
> demonstrate improved capability on BPM or Arxiv proofs isn't actually in the
> document as far as I know — or if it is, it isn't backed by a
> description/demonstration of a viable system — because we only have prepared
> the precursor to that viable system now."

> "The previous capability-proof just amounts to 'if Codex is good at math it
> can solve these problems' which isn't very interesting."

> "Since the current state of evidence seems to be 'pilot not measurement', I
> want each round of the agentic loop to lead to measurables that can serve as
> something like a loss function as we move from problem to problem."

> "This is a much better state of affairs for experimental work b/c we can now
> pin hashes to experiment stages."

### The guiding light, reversed

Joe offered this as the mission's orienting principle, to be read **inverted**:

> "It was on display in the bottom of a locked filing cabinet stuck in a
> disused lavatory with a sign on the door saying 'Beware of the Leopard.'"

Inverted, it is an acceptance criterion: **an artifact counts as available only
when the person or agent who needs it can find it, by the vocabulary they
actually use, at the moment they need it.** Technically-present is not
available.

This is not decoration. Every failure repaired on 2026-08-14 was this failure,
and so is the retrieval defect the V3 whitepaper diagnoses (§4.1): a01A12's
memory was in the store, indexed by a vocabulary its own runner would never
search by, and the runner re-derived the content instead.

### What's already felt to be true

- **The relay works.** Boundary artifacts transport work between agents; the
  repository itself (boundary comments, neighbouring solved problems) has been
  the memory channel that demonstrably functions.
- **Framing dominates architecture.** One sentence of task framing moved memory
  consultation from 0 to 21 calls, same agent and same store (E9/E10,
  preregistered).
- **The demand loop has fired once, end to end.** Votes → build → callback →
  one-pass closure of a theorem that had survived three prior closer hops
  (V3 §6; `10eac91b` → `a266157d`, merged `087924c`). That is learning at the
  ability level, witnessed, n=1.
- **Failure is legible when instrumented.** The day's substrate work produced
  named errors and measured rates rather than mysteries.

### Anti-glibness discipline

What would make this mission superficial:

- **Reporting a solve rate as if it were a capability claim.** The corpus
  percentage is an odometer, and its current gauge is known to read low.
- **Letting a falling `L` stand in for learning.** Without pre-declared
  difficulty stratification, a falling loss is indistinguishable from an easier
  tail of the corpus.
- **Measuring across a retrieval-regime change.** The anchor-selection repair
  (`6521fd3a`) is under live test; a slope spanning it confounds "the process
  learns" with "we fixed the index."
- **Counting a round that never elicited consultation.** If the treatment arm
  does not look, the round measures nothing while appearing valid.
- **Upgrading a warrant on the existence of an artifact** rather than on a
  demonstration that its consumer can reach it.

The discipline that prevents it: the warrant machinery the capability proof
already has — *a warrant upgrades only by certificate, never by narrative* —
plus the reversed guiding light as a new certificate class.

### Working-economy position

**What this underwrites:** the claim that the futon stack constitutes a
capability, not a collection of agents that sometimes succeed. The
demonstration target is improved capability on **BPM or arXiv proofs** — held
out from APM, which makes it formally a transport claim.

**What underwrites it:** as of 2026-08-14, and not before —

- three repos at inbox zero and even with origin, so an experiment stage pins
  to a hash (futon3c `33a148c2`, futon6 `01d7802`, futon1b `3e1b0d2`);
- the 154-file evidence corpus reachable and sha-verified (154/154) with
  `backup_evidence.sh` as the sync basis;
- a substrate whose census is a measurement, not archaeology (`pattern/library`
  1288 rows == 1288 distinct names; `pattern/clause` 9668 == 9668; zero
  non-qualified ids or relation endpoints);
- a written identity convention, making conformance a one-line audit.

### Clarity-gap / carried-forward tensions

Named here so IDENTIFY does not pretend they are settled:

1. **The odometer is miscalibrated.** v1 says ~27% closed; `status.json` gives
   186/475 = 39.2%, and E5 showed that file *understates* closure. No
   percentage should be quoted until a comment-aware sorry detector exists.
2. **`L(i)` needs a difficulty model.** Stratification or matching, declared in
   advance. Not yet designed.
3. **N6's transport derivation is an unpaid obligation.** v1 states it
   correctly as a selection diagram and never discharges it.
4. **The retrieval regime is mid-repair.** batch-2 is under live test; the
   regime boundary is a stratum boundary for every measurement.
5. **The scribe role is staffed but not performed.** `ams-scribe-1` was
   repurposed as a bridge-lane seat; the hunger audit has never run at campaign
   scale. Two of the most informative measurables are empty for that reason.
6. **The sync is manual.** `backup_evidence.sh` is invoked by hand; until the
   rsync pattern exists, the corpus's reachability rests on someone
   remembering — the same failure shape, relocated from git to attention.
7. **The one effect-size datum is one edge wide.** RAW-CTL corrected:
   enriched 3/25 = 12.0%, raw 4/26 = 15.4% over 51 inference edges. **Not a
   directional prior** — a sizing lesson.

### Provenance

Generated 2026-08-14 from a single operator session (surface `emacs-repl`,
seat `claude-2`), following the closure of `E-apm-halftime-pre-go-live` and
the repair of the futon1b substrate. Direct antecedents:
`capability-proof-apm-v2-plan.md` (written earlier the same day),
`E-apm-halftime-pre-go-live-EF.md`, `docs/retrieval-whitepaper-v3.md`,
`holes/labs/M-diagramprover/capability-proof-apm.tex`. Operator quotations are
verbatim from the session, not paraphrase.

---

## 1. IDENTIFY *(draft — gated)*

### 1.1 Motivation

The discrepancy: the capability proof certifies only its **enabling** nodes.
Sorting v1's own warrant table by certification makes the gap exact —

| certified (scaffolding) | uncertified (the thesis) |
|---|---|
| N1 extra resources fill Mathlib holes — n=3 | **N5 retrieval serves the need — weak** |
| N2 work transports between agents — n=4 | **N6 transports to held-out — designed** |
| N3 the store records learning — n=4 | **N7 outcomes mechanically scoreable — designed** |
| N4 agents consult when instructed — n=1 (ctrl) | **N8 learns at ability level — designed** |
| N9 the pipeline runs — n=2 | |

Everything certified amounts to "a relay exists, runs, and hands work between
agents." That is the reading Joe calls uninteresting, and the table proves it
rather than merely permitting it.

**Correction carried from `capability-proof-apm-v2-plan.md` §8.1:** N8 is
*mis-graded*. V3 §6 witnesses a complete demand→build→closure revolution
(`10eac91b` → `a266157d`), so N8 is `inductive-n=1`, not `designed`. Neither
document cited the other — itself a Leopard instance, a witness that existed
where the document needing it could not see it.

### 1.2 Theoretical anchoring

Retained from v1 and not in question: the **constructive (BHK) reading** of the
capability claim; **graded warrants with typed refusal**; **transportability
via selection diagrams** for N6.

Candidate, *not* adopted: a **loss-function framing** in which each round emits
measurables and the claim becomes a slope rather than a property; and the
**reversed guiding light** as a warrant condition (reachability by the
consumer's own vocabulary). Both are the operator's, both are plausible, and
neither has been tested against what the system already emits. See §1.4.

### 1.3 Scope in / out

**In.** Determining, for each uncertified node, whether the work is already
done, partly done, or greenfield — and only then designing. A demonstration on
held-out BPM or arXiv proofs. Revising `capability-proof-apm.tex` to v2.

**Out (explicitly deferred).** Rewriting v1's warrant machinery — it is the
contribution and survives intact. Substrate-2 reingest. The `#uuid` gate.
B/C/D packet residue.

### 1.4 OPEN IN IDENTIFY — the N-register

Joe, 2026-08-14: *"your plan according to N-numbers from previous rounds could
serve as a checklist for IDENTIFY — for example, 'N7 is the keystone' may be
true but IDENTIFY would help us see what work (if any) has already been done,
or whether this is a greenfield design issue … as they currently comprise a
speculative design."*

**Everything in `capability-proof-apm-v2-plan.md` §2 is speculative design.**
It was written before any survey of what the loop already emits. It is retained
as a *source of hypotheses*, not as a plan. Each node below is therefore left
**open**, with the question MAP must answer stated as: *already done / partly
done / greenfield?*

| node | v1 grade | what is FACT | what is SPECULATION | MAP must determine |
|---|---|---|---|---|
| **N5** retrieval serves the need | weak | The defect is diagnosed (V3 §4.1, inverted relevance on a01A12). Users search by **engine names** and **structural similarity**, never concept vocabulary (V3 §4.2, twelve exit interviews). A repair shipped as a switchable contrast (`6521fd3a`), under live test as batch-2. | That the hunger audit is "the instrument", and that running it is "a measurement, not a build". | What does the hunger audit actually record today? Has it ever run? What would campaign scale cost? Is batch-2 concluded? |
| **N6** transports to held-out | designed | v1 states it as a transport claim with selection variable `S`; the causal engine exists (backdoor/front-door/transportability). | That the derivation is discharge-able at all, and that BPM is the right held-out set. | Has any transport derivation been attempted? Does the engine refuse it? Which of BPM / arXiv has the better-formed selection diagram? |
| **N7** outcomes scoreable | designed→mechanical | v1 names "delta-form endpoint, executed-witness-only scoring" but never defines the endpoint. 57 `receipt.json` files exist with a rich schema. Ledgers exist (`bridge-pilot-jobs.jsonl`, `escalation-queue.jsonl`, `axiom-audit.jsonl`). | **That N7 is "the keystone"**; the six-row measurement vector; `L(i) = cost + residual + rework`. None of this has been checked against what the loop emits. | Which vector quantities are **already emitted**? Which need new bookkeeping? Is there a scoring implementation already, in the driver or the gate? |
| **N8** learns at ability level | designed | **Mis-graded.** V3 §6 records a complete demand→build→closure revolution (`10eac91b` → `a266157d`, merged `087924c`) closing a theorem that had survived three prior closer hops. The vote ledger exists: 14 concepts, three past threshold. | That N8 should be restated as `dL/d(problems seen) < 0`; that difficulty stratification is the right control. | Is the vote ledger machine-readable as a series? Is there a difficulty signal already in the corpus (`closer_hop`? sorries at formalization? family?) |

**Consequence for ordering.** The v2 plan asserted an order (N7 first, then N8
and N5, N6 last). That order rests on the keystone claim, which is itself
speculative. **No order is adopted here.** MAP's findings determine it.

**Consequence for the three preconditions.** Headroom, arm independence and
elicitation-verified are *observed invalidators*, not design — they are facts
about assays that failed, so they stay. What is speculative is promoting them
to formal pass/fail conditions; that is a DERIVE decision.

### 1.5 Completion criteria *(testable; thresholds are proposals — see Gate)*

Stated so as not to presume the register's outcome:

1. **Every N-register row resolved** to already-done / partly-done /
   greenfield, with evidence — this is MAP's exit condition and this mission's
   first real deliverable.
2. **N7 carries a mechanical warrant**, by whatever endpoint DERIVE settles on.
3. **N5 graded against the users' contract** (engine-name and
   structural-similarity queries), not a generic relevance notion.
4. **N8 stated in a checkable form** and evaluated within a fixed retrieval
   regime named by hash.
5. **N6 discharged or refused**, with the refusal recorded if so.
6. **A held-out demonstration** on BPM or arXiv satisfying the three
   preconditions.
7. **The capability proof revised** to rank nodes by load-bearing status and to
   drop the miscalibrated odometer.

### 1.5 Relationship to other missions

- **Supersedes:** `E-apm-halftime-pre-go-live` (closed 2026-08-14).
- **Depends on:** `E-apm-halftime-pre-go-live-EF` as empirical source;
  `docs/retrieval-whitepaper-v3.md` for the retrieval regime; the batch-2
  live test (`6521fd3a`) for regime stability.
- **Enables:** `capability-proof-apm.tex` v2.

### 1.6 Source material

`holes/labs/M-diagramprover/capability-proof-apm.tex` (v1, 311 lines) ·
`holes/labs/M-diagramprover/capability-proof-apm-v2-plan.md` ·
`holes/excursions/E-apm-halftime-pre-go-live-EF.md` ·
`docs/retrieval-whitepaper-v3.md` (§4 retrieval, §6 demand loop, §7 terms) ·
`futon6/README-apm-lean-ground-control.md` §1 (the regime) ·
`data/evidence/` (154 files, sha-verified) ·
`futon6/holes/TN-raw-ctl-reanalysis.md` (the one effect-size datum) ·
`scripts/backup_evidence.sh`, `scripts/batch2r_pair.sh`, `scripts/rawctl2.py`.

### 1.7 Owner and dependencies

Owner: `ams-claude-2` (Zone), operator Joe.
Repos: **futon3c** (mission, loop, evidence), **futon6** (ground control,
RAW-CTL, playbooks), **futon1b** (substrate), **apm-lean** (corpus).
External: Codex lanes for implementation under the handoff protocol;
`oxf-claude-3` holds Dionysus.

---

## MAP *(not started)*

**Exit condition:** every row of the N-register (§1.4) resolved to
*already-done / partly-done / greenfield*, with evidence. Until then no
ordering, no keystone, and no endpoint definition is adopted.

Survey questions to answer with concrete findings, not speculation:

- **Q1** Which measurement-vector quantities are *already* emitted by the loop,
  and which need new bookkeeping? (Anything needing new bookkeeping will not
  survive contact with a campaign.)
- **Q2** What does the hunger audit currently record, and what would running it
  at campaign scale cost?
- **Q3** Is there a difficulty signal already in the corpus usable for
  stratification (closer_hop? sorry count at formalization? problem family?).
- **Q4** What is the true corpus closure under a comment-aware sorry detector?
- **Q5** Which held-out set — BPM or arXiv — has the better-formed selection
  diagram, i.e. where is `S` most separable from the pipeline edge?
- **Q6** What is the batch-2 regime boundary in commit terms, and how many
  problems fall each side of it?
