# E-apm-halftime-pre-go-live-EF — a working lab for the second half

> **STATUS CHANGE 2026-08-14 (Joe).** The parent excursion
> `E-apm-halftime-pre-go-live.md` is **closed**, and this packet is **no longer
> "the" plan**. It remains live as an **empirical source** — the informal
> description of what the system should be. The formal plan is now
> **`holes/missions/M-apm-demonstration.md`**, which follows the mission
> lifecycle (`futon4/holes/mission-lifecycle.md`).
>
> Read this for the loop's shape, the role table and the measurement vector;
> read the mission for the gap statement, scope and completion criteria.

**Written 2026-08-14 by claude-2 at Joe's request:** *"I'd like to batch the E
and F segments together because they are both relevant to creating a working
lab for the second half of the APM project."*

Sections **E** (ownership and hygiene) and **F** (the experiment itself) of the
locked list. These decompose E1–E5 and F1–F5; they are **not** new items.
B/C/D are **held** until A is done, per Joe, for coherence.

Every figure re-verified against the live tree on 2026-08-14. Where it differs
from the locked list, the corrected value is marked.

## The organising idea

A lab is working when three things are true, and each maps to a track:

1. **You can trust what the evidence says** — the data already on disk is
   analysed, not merely collected. (F3, F4)
2. **You can trust what a result means** — the protocol can actually produce a
   pass, and the gates that judge it are known-good. (F2, F1, F5)
3. **You can trust the workspace** — one source of truth per artefact, no
   forked twins, no unowned piles. (E1–E5)

**Track 1 is the one Joe has been asking for and not getting.** Joe,
2026-08-13: *"I haven't seen an interim report that helps me understand what
actually works and what doesn't."* F3 and F4 answer exactly that and are
already paid for — the data exists. **Do not starve them behind janitorial
work.** Track 3 is cheap and runs in parallel.

---

# Track 1 — analyse the evidence we already have

## F3 — the glue-census has never been analysed (6,114 steps)

**Verified:** `futon3c/data/glue-census/` holds `have-corpus.jsonl`
(**6,114 lines**), `clusters.tsv` (3,805 lines), `have-corpus-minilm.npz` —
9.1 MB total, already embedded.

**This is the closest thing on record to the actual research question.** It
holds a positive result: a hand-derived `closure_ball` rewrite was found
verbatim at two sites in `a95A05` by plain-English query at **0.77 cosine**.

⚠ **Wording correction (Joe, 2026-08-14).** The locked list calls this *"the
one positive result on record"* and claude-2 repeated that phrasing. The
defensible claim is narrower: **it is the one positive result found in the
current Zone tree.** Joe: *"I really find it hard to believe that there is only
one positive result on record, I suspect that there are notes that may have
been lost."* That is a records problem, not a finding about the experiment, and
treating the two as the same would understate the project's results in a
halftime report. See the Dionysus sweep below.
The research line is *Scribe-extracted rewrite rules mined from Zai
self-corrections* — not patterns — so this corpus, not the pattern library, is
where "does it work?" gets answered.

**Goal.** Turn 6,114 collected steps into a statement about whether mined
rewrites are findable and useful.

**Deliverable — a written analysis, not a pipeline:**
- How many of the 6,114 steps are genuine self-corrections vs noise.
- Cluster structure: does `clusters.tsv` group semantically, or by surface form?
- **Retrieval check:** for a sample of hand-derived rewrites, are they findable
  by plain-English query, and at what cosine? The `closure_ball`/0.77 result is
  **n=1** — the deliverable is whether it generalises.
- An honest verdict, including "the corpus does not support a conclusion" if
  that is what it shows. **A negative result here is a real result** and is
  more useful than a hedge.

**Do not** build new mining infrastructure. Analyse what exists.

## F4 — no interim report on memory use

**Verified:** 57 `receipt.json` files under `apm-lean/problems/*/`, with a rich
schema (`proof/problem-id`, `proof/cycle-id`, `frame/id`,
`frame/upstream-boundaries`, `inputs`, `state`, `frame/artifacts`).

The locked list records the supporting counts: 610 rows, 242 surfacing
dispatches, 86 with a use, 54 distinct memories used, 94 problems. **"To what
effect" needs a join that has never been done.**

**Goal.** Answer Joe's question verbatim: *"can we write up a report on what
memories have been used, how frequently, and to what effect?"*

**Deliverable.**
- **Used how often:** distribution over the 54 distinct memories — is use
  concentrated in a handful, or spread?
- **To what effect:** the join. Did dispatches that surfaced a memory close
  their problem more often than those that did not? State the comparison and
  its n.
- **State the confound plainly.** This is observational, not randomised:
  memories surface on problems that are *not a random sample*. If the
  comparison cannot support a causal claim, say so — do not dress a
  correlation as an effect.
- If the receipts cannot answer "to what effect", say **what field would have
  to be recorded** for the second half to answer it. That is the most valuable
  possible output of this packet.

---

# Track 2 — make a result mean something

## F2 — no pre-registration exists for the commissioning test

**Write and freeze it before Run 0.** It must state, in advance: the
hypothesis, the arms, the primary outcome measure, the n, the stopping rule,
and what result would count as a **failure**.

**Acceptance.** Committed and unedited thereafter. A pre-registration amended
after seeing data is not one. This packet is **prose, not code**; it needs a
ruling from Joe, not an implementation.

**Why first in this track:** F1 cannot be repaired without it — the previous
assay was invalid partly because there was no prior statement of what would
count.

## F1 — zero assays have ever passed; the one that ran was invalid

Two defects, both fatal, both known: **no headroom**, and **the arms shared a
session** (so the arms were not independent).

**Goal.** A design in which a pass is *possible* and a shared-session
contamination is *impossible*.

**Deliverable — design first, run second:**
- Headroom: show the measure can move. If the ceiling is at or below the
  control, the assay cannot pass regardless of the intervention.
- Arm independence: separate sessions, demonstrated, not asserted.
- A dry run on the canned CT problem (`futon3c/data/ct-commissioning/`,
  which already has `problem-ct-primary.md` and `problem-analysis-negative.md`)
  **before** any real run.

**Blocked on F2.** Do not run an assay before the pre-registration is frozen.

## F5 — the `:attribution-incomplete` gate has unknown reliability

**This gate falsely indicted a seat over a backtick in a regex.** It is used to
judge whether work is honestly attributed; a gate that misfires on punctuation
cannot be trusted to judge the second half.

**Goal.** Characterise it: false-positive and false-negative rate against a
labelled set of known-good and known-bad attributions.

**Acceptance.** A number, not an opinion. Include the backtick case as a
regression test. If it cannot be made reliable, **say so and recommend
retiring it** — an unreliable honesty gate is worse than none, because its
verdicts get quoted.

---

# Track 3 — hygiene (cheap, parallel, no dependencies)

## E1 — 86 uncommitted files in `futon3/library/` (verified: 86)

No owner. **Do not `git add -A`** — this pile nearly swallowed an unrelated
agent's work once already. Triage into: mine / someone else's / generated, and
commit only what is genuinely owned, in separate commits with honest messages.

## E2 — 203 uncommitted entries in `futon3c` (corrected: 203, list said 198)

Includes a **deleted `.clj-kondo/config.edn`** — still deleted, which matters
because clj-kondo is a gate on every Clojure handoff and its config is missing
from the tree. Restore or consciously retire it.

## E4 — `patterns-index.tsv` twin (verified: 1,359 vs 1,355)

`data/notions/patterns-index.tsv` (1,359 rows) vs
`storage/futon3/resources/sigils/patterns-index.tsv` (1,355).
**Restoring the symlink would silently drop every pattern authored since
2026-08-10.** Reconcile explicitly, never by symlink. Note this file is also
where **B5**'s 26 colliding keys live — coordinate if B is unheld.

## E5 — status-derived corpus percentages are understated (verified)

`apm-lean/problems/a94A09/status.json` reports `sorry_count_total: 1`, but
commit `a266157` is *"a94A09: close uniqueness via Schwarz-Pick rigidity"*.
So the status file says open for a problem with a closing commit.

**Any corpus completion percentage derived from `status.json` is wrong**, and
percentages are exactly what a halftime report quotes. The audit needs a
**comment-aware** sorry detector (a `sorry` inside a comment is not a sorry).
**Deliverable: the corrected corpus percentage**, plus the count of problems
whose status disagrees with their commits.

## E3 — ground-control README fork — ⚠ PREMISE CHANGED, verify before working

The locked list records 1,170 lines in `futon6/` vs 1,147 in
`apm-evidence/docs/`. **Only one copy now exists**
(`futon6/README-apm-lean-ground-control.md`, 1,170 lines); the
`apm-evidence/docs/` copy is not present. Either it was already reconciled or
it moved. **Confirm before doing anything** — this may already be closed.

---

## Gates for every packet in this file

Prose deliverables (F2, F3, F4, F5): a committed document; state n for every
number; distinguish measured from inferred.
Code: `clj-kondo` 0/0, `futon4/dev/check-parens.el`, `git diff --check`,
relevant tests.
All: **bell `claude-2` back with a summary + commit shas.**

**Do not restart the futon1b substrate** without checking
`du -sb migration-store-21/log` first (D3) — a quiet store boots in ~28 s, a
backlogged one took 379 s and once failed entirely.

---

## The Dionysus sweep (Joe, 2026-08-14) — prerequisite for F3 and F4

Joe: *"I really find it hard to believe that there is only one positive result
on record, I suspect that there are notes that may have been lost."*

Zone is not the whole record. Before F3 or F4 concludes anything about what the
project has achieved, **sweep Dionysus for APM material that never reached
Zone**. A negative interim report written off an incomplete corpus would be
worse than no report — it would be wrong in the discouraging direction.

**Therefore F3 and F4 are gated on this sweep**, not merely informed by it.
Dispatched to `oxf-codex-1` (federated peer `172.236.108.82`) on 2026-08-14.

---

# REFRAME (Joe, 2026-08-14): EF as an agentic coding loop with a loss function

Joe: *"we've talked about various roles (prover, scribe, etc.) … it isn't
formalised. Since the current state of evidence seems to be 'pilot not
measurement', I want each round of the agentic loop to lead to measurables that
can serve as something like a loss function as we move from problem to
problem."*

**"Pilot not measurement" is the correct diagnosis** and it reorders this whole
packet. F3/F4 analyse what a pilot happened to leave behind. The reframe asks
the loop to *emit measurements by construction*, so the second half does not
depend on archaeology.

## The loop already exists — it is just not instrumented

From `futon6/README-apm-lean-ground-control.md` §1 ("The regime"), which is the
closest thing to a written spec:

| # | role | does | already emits |
|---|---|---|---|
| 1 | **Formalizer** (Codex) | APM problem → Lean statement | the statement |
| 2 | **Reviewer** (Claude) | statement-fidelity gate | a verdict |
| 3 | **Freeze** | `frozen_hash` + `frozen_declarations` | a hash |
| 4 | **Prover** | closes, or reports **Tier A** (missing Mathlib lemma) / **Tier B** (bridge lemma) / **defective statement** | a typed outcome |
| 5 | **Scribe** | draft + promotion, memory entries with retrievable tags, **hunger audit** | memories |

Two known holes in the regime, both already documented and neither closed:
- **the freeze does not cover `def` bodies** — "an open gap Joe knows about and
  has not yet ruled on". A contract that does not bind the thing under change
  cannot bound rework.
- **the scribe was never run on the recent campaign** — `scribe.md` unmodified
  since 2026-08-04 while the campaign ran on the 8th–9th; `ams-scribe-1` was
  repurposed as a bridge-lane seat. So role 5 has been *staffed but not
  performed*, which is why the memory record is thin.

## The measurement vector — one per round, per role

Each quantity must come from an artifact the loop **already produces**.
Anything requiring new bookkeeping will not survive contact with a real
campaign.

| role | measurable | direction |
|---|---|---|
| Formalizer | statement defects found at review | ↓ |
| Reviewer | **escape rate** — defects the prover hits that review missed | ↓ |
| Freeze | **contract leaks** — post-freeze changes to `def` bodies | ↓ (currently unmeasured *and* unbounded) |
| Prover | outcome ∈ {closed, TierA, TierB, defective}; attempts to close; residual `sorry` count | ↓ |
| Scribe | memories promoted; **hunger audit — queries returning empty** | ↓ |
| Scribe (join) | promoted memories later *surfaced and used* on a later problem | ↑ |

**The hunger audit is the instrument that already exists and is closest to a
gradient.** It measures retrieval failure directly, per round, from the
scribe's own template. It has simply never been run at campaign scale.

## The loss function

Per problem *i*:

```
L(i) = cost_to_close(i) + residual(i) + rework(i)

  cost_to_close : prover attempts (optionally wall-clock / tokens)
  residual      : sorries remaining + open Tier A/B items
  rework        : re-freezes + defective statements caught late + review escapes
```

The research question is then **not** "does retrieval work?" but a slope:

> does `L` fall as accumulated memory grows — `dL/d(problems seen) < 0`
> — *conditioned on memory being available*?

That is exactly "something like a loss function as we move from problem to
problem", and it is answerable from the vector above without any new
infrastructure.

**The confound that must be stated, not finessed:** `L(i)` is only comparable
across problems if problems are comparable in difficulty. A falling `L` is
otherwise indistinguishable from an easier tail of the corpus. So the design
needs stratification or a paired/matched comparison **declared in advance**.

## What this does to F1–F5

The reframe promotes two items from hygiene to prerequisite:

- **F2 (pre-registration) is now load-bearing, not paperwork.** A loss function
  chosen after seeing the data is a curve fit. The difficulty-stratification
  above must be fixed in advance or the slope means nothing.
- **F5 (gate reliability) bounds the measurement.** `escape rate` and `defects
  found at review` are *gate outputs*. A gate that misfires on a backtick puts
  noise directly into the loss function. Characterise it before trusting any
  round.
- **F1** becomes: does one round of the loop produce a complete, valid
  measurement vector? That is a far cheaper pass/fail than the original assay,
  and it can be dry-run on the canned CT problem.
- **F3/F4 are re-scoped as *baseline*, not verdict.** They establish `L` for
  problems already run, so the second half has a zero point to improve on.
  This also dissolves the tension in F3: the corpus does not have to answer
  "does it work" — it only has to yield a baseline.

## Ordering under the reframe

1. **F2** — pre-register the loss function and the stratification. Blocks all.
2. **F5** — characterise the gates that feed it.
3. **F1** — dry-run one round on the CT problem; acceptance is *a complete
   measurement vector*, not a proof.
4. **F3/F4** — compute the baseline `L` over the existing corpus.
5. Then, and only then, run rounds and look at the slope.

**Also needed, and currently missing:** the scribe pass must actually run, or
role 5 emits nothing and the two most informative measurables (hunger audit,
memory-use join) stay empty — which is precisely how the current record became
thin enough to look like "one positive result".
