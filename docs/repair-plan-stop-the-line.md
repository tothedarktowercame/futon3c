# Repair plan — stop the line

**STATUS: DRAFT, AWAITING JOE'S VETTING. Nothing here is authorised and
nothing in it has been executed. Committed unvetted deliberately, because
"plans haven't been committed, vetted, thought through, checked, or
adhered to" is the complaint this document answers — so it exists first,
gets marked up second, and is adhered to third.**

Joe, 2026-08-13: *"we resume the case loop only when we know we are
building a pipeline that makes sense … no running any pipelines or plans
until we have a validated working system behind it. The only order of
business now is to repair and prove that the repairs work."*

Fable's plan (2026-08-13) is good and is **overruled on sequencing only**:
its Q3 measurement and Q1 case-loop resumption are downstream of this
document. Its **loopback probe** is adopted here as R2, and its
**pre-registration + instrument freeze** as R3.

---

## 0. The one structural fact that decides the shape

**The memory channel is proven. The pattern channel has never worked once.**

- Memory channel: a94A09, 2026-08-10 — recall surfaced two memories at
  dispatch, the runner USED one, the committed Lean instantiates the
  memory's prescription, both receipt halves written. A complete chain,
  witnessed, n=1 but real.
- Pattern channel: no pattern has ever been retrieved by a runner. Not
  once, in any slice. Today's sort did **not** change this — verified
  after the move: the math pool is 45 patterns and all four mined content
  patterns remain **invisible** to it, because `math-formalization/` and
  `math-strategy/` are outside the `math-informal*` glob.

**Therefore: do not repair a second retrieval stack. Put patterns onto the
channel that already works.** Every alternative means building and
validating a parallel apparatus with zero live consumers — which is the
gold-plating Fable correctly named.

## 1. The repair table

Every row has a **PROOF**: a test that can fail. A repair without a
failing-mode is not a repair, it is a claim.

### R0 — leaks and divergences (independent, no ordering between them)

| # | Defect | Repair | PROOF |
|---|---|---|---|
| R0.1 | Staging dirs are wired into the retrieval pool: `load_all_patterns` defaults `extra_library_dirs=DEFAULT_STAGING_DIRS`, so unassayed candidates are already retrievable. Quarantine is not quarantining. | Remove staging from the default pool; make inclusion explicit and opt-in. | A test asserting a known staged, unassayed candidate (`replace-enumeration-with-structural-counting`) is ABSENT from the default pool. Test must be shown to FAIL against the current code before the fix. |
| R0.2 | `patterns-index.tsv` exists as two divergent real files (repo 1360 rows, `storage/` 1355) after a symlink was replaced by a regular file. | Declare one canonical, archive the other with a tombstone. Do **not** restore the symlink. | A preflight check that (a) names the canonical path, (b) fails if a second divergent regular copy exists anywhere under `/home/joe/code`. Run it; it must currently fail. |
| R0.3 | 24 index rows silently dropped: 26 duplicate qualified names, 16 disagreeing on title/hotwords (`f3/p0` is both "Portal Query Layer" and "MUSN Coordination Substrate"). | Key on something row-unique; emit collisions as a report. | Row count in == row count out, or an explicit collision report accounting for every difference. `1358 in / 1334 out / 24 unexplained` must become `0 unexplained`. |
| R0.4 | `README-apm-lean-ground-control.md` is forked: 1170 lines in `futon6/`, 1147 in `apm-evidence/docs/`, contents differ. Fable calls it the most valuable inherited document. | Establish which is current; tombstone the other. | `diff` is empty, or a note states which supersedes which and why. |

### R1 — the architectural repair: patterns onto the proven channel

| # | Repair | PROOF |
|---|---|---|
| R1.1 | Ingest patterns into XTDB as first-class entities: uuid-keyed, `:qualified` / `:family` / `:domain` / `:grade` / `:status` as attributes. | Count reconciliation (every source row accounted for as ingested or reported-as-collision); round-trip query returns a known pattern by id and by domain. |
| R1.2 | Retrieve patterns through the **existing** recall ladder (`dispatch_with_recall`: normalize → stopword-filter → IDF rank), not a second scorer. | The four mined content patterns are retrievable by `:domain :mathematics`; the process pattern is NOT. Both assertions in one test. |
| R1.3 | `:domain` becomes the scoping predicate; the directory glob stops being load-bearing. | Filing a pattern in the "wrong" directory no longer changes what a math run retrieves. Demonstrate by moving one pattern and showing retrieval is unchanged. |

**Explicitly NOT in R1:** repairing the Tier-0 hotword scorer, its
stopword/IDF gap, or the formal-identifier-vs-runner-English vocabulary
gap. Those belong to an instrument with no live consumers. If R1 lands,
they may never need doing.

### R2 — the proof that the system works (Fable's loopback probe)

| # | Repair | PROOF |
|---|---|---|
| R2.1 | Deposit a **canary**: a pattern/memory tailored to a specific known problem, deliberately worded in that problem's own vocabulary. | Dispatch that problem and require the canary to surface at dispatch-time recall. **Canary fails → the system is not repaired, and nothing downstream runs.** |
| R2.2 | Then one real pattern end-to-end: author → review → assay → deposit → surface → use. | The a94A09 standard applied to a pattern: surfaced at dispatch, USED by the runner, corroborated by the artifact, both receipt halves written. |

**R2.1 is the gate for everything.** No assay, no tide test, no slice 4,
no case loop until a canary surfaces. This is the countermeasure to the
failure this whole week was: a falsifier firing on plumbing.

### R3 — governance (Joe's rulings needed, not mine to adopt)

| # | Proposal | Source |
|---|---|---|
| R3.1 | Pre-registration with a stated falsifier before any dispatch called an experiment. | Fable; the batch era's discipline, which worked |
| R3.2 | **Instrument freeze** between pre-registration and readout — apparatus work lives in the gaps between measurements, never concurrent with one. | Fable. Binds the captain hardest; this week violated it continuously. |
| R3.3 | Admission standard for `library/pattern-mining/` — the math teachability assay is inapplicable to process patterns. Three candidates in that family's README; none adopted. | claude-2, open |
| R3.4 | An instrument is Phase 0 **only if the next measurement's DV flows through it**. | Fable; adopted here as the pruning rule |

## 2. What is NOT being done

Named so it cannot drift back in: no slice 4, no tide test, no assays, no
case loop, no retrospective mining fleet, no Tier-0 scorer repair, no
hotword tuning, no new agent lanes. The line is stopped.

## 3. Sequencing

R0 is independent and small. R1 is the real work and needs a dispatch.
R2 gates everything downstream. R3 needs Joe.

The honest estimate is that R0 is hours, R1 is a day or two of codex work
with a review, R2 is one dispatch. Nothing resumes until R2.1 passes.

## 4. What I want vetted before I touch anything

1. Is R1's premise right — patterns onto the proven memory channel rather
   than repairing Tier-0? It is the biggest call in here and it is mine.
2. Is R0.2's "do not restore the symlink" the right resolution, or should
   `storage/` win and the repo copy be rebuilt from it?
3. R3.1–R3.4 are yours to rule on.
4. Anything in §2 you want moved back in.
