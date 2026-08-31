# TN: F66–F70 review — have the repairs taken, and is the memory useful to a solving agent?

Author: Claude (Fable 5, claude-2), 2026-08-31 ~23:30Z, at Joe's request ("look at
the most recent five or so frames, through F70, which is now in process; another
commentary on whether the repairs have taken place, and whether it is developed to a
point where the memory system can seem to be useful for an agent solving the
problems"). Scope: f66–f70 in `jit-all-open-v2`, read against
`TN-fable-F36-F57-review.md` (§7 recommendations and the 08-30 addendum's three gaps)
and `TN-opus-f47-observation.md`, whose f58–f68 sections I checked rather than
re-told. f58–f65 appear where a count needs them. §7 lists what I ran.

## Short answer

1. **Five frames, four problems solved, three banked.** f66/b03J01 closed and
   banked; f67/b90A01 `:partial` (students never closed it; the solver did, in 14
   rounds) and banked; f68/b90A03 parked at scribe-reduce with all three student
   attempts sorry-free and the solve stranded off master; f69/b93A01 closed and
   banked; f70/b93J01 at `:solve`, round 25 of 50, one sorry left, still running as
   I write (23:29Z). §1.
2. **The repairs from the previous note landed, with one exception that matters:
   the close-frame fingerprint audit runs on every close and writes an empty
   file.** `frame-fingerprint-audit/audit!` hands the *frame* directory to a script
   that expects a *campaign* directory, so f62, f64, f65, f67 and f69 each carry
   `analysis/fingerprint-audit.json` with `rows: []`, `use-events: 0`, status
   `:ok true`. The unit test asserts exactly the wrong call shape. Nobody noticed
   because the offline campaign-level run still works and is what the Opus note
   quotes. §3.
3. **Memory use in f58–f69, by the artifact standard: 32 use events, 13
   fingerprinted, all thirteen within-frame.** Five cross-problem reads, zero
   fingerprinted (three are the f34 route memory; two are content memories from
   f64-guide and f62-guide, both `already-in-base`). The campaign's picture has not
   changed shape since the last note: what transfers with a token trail is the
   solver's route, mined by the scribe minutes earlier, into a fresh student on the
   same problem. §4.
4. **Is that useful to a solving agent? Within the frame, yes, and f69 is the
   cleanest instance so far.** Three fresh students on b93A01 from the same base:
   a1 (four same-problem deposits withheld) left 2 sorries; a2 (the four deposits
   released) proved it, 339 lines, carrying `Equiv.Perm.lcm_cycleType`,
   `Multiset.lcm_cons`, `Subgroup.subgroupOf_eq_bot` and five other tokens the base
   did not have; a3 (deposits accessible, chose one cross-frame memory instead)
   left 2 sorries. Across f62–f69, in three of seven frames a deposit-arm attempt
   closed where the holdout attempt did not, and in none the reverse. That is the
   prereg's own "suggestive, not clean" contrast (amendment 8's size confound is
   unresolved), and it is a within-frame result, which amendment 3 says is not the
   campaign's question. Both halves of that sentence are true and the note should
   not let either swallow the other. §4.
5. **Two apparatus facts that bear on "useful".** The agent that actually solves
   the open problems — the solver — has no memory channel at all; every solve in
   this window was done blind, and the students are replicating the solver's work
   with and without its notes. And the memory-cascade arm returned HTTP 503
   `:expensive-read-busy` in 15 of the 25 student attempts f58–f69 (all three
   attempts of f67 and f69), after 14–235 s of waiting each time; in the ten
   attempts where it worked, `used-via-cascade` is `[]`. The cascade has
   contributed nothing to any use event in the window. §5.

Recommendations, small and ordered, in §6.

## 1. What the five frames did

| frame | problem | opened (Z) | solver | frame result | student attempts (outcome · used/accessible · sorries) | wall-clock |
|---|---|---|---|---|---|---|
| f66 | b03J01 | 08-31 10:55 | 1 round, success | closed, solved, **banked** (`c6fc4972`) | **proved 1/129 (0)** · **solved 1/130 (0)** · **closed 2/133 (0)** | 2h02 (close via `countdown-recovery`, 12:57) |
| f67 | b90A01 | 08-31 12:58 | 14 rounds (4 lost to a provider outage), `:solver-remediation-required`, then closed | partial, solved, **banked** (`0b1b0383`) | partial 0/134 (3) · partial 2/140 (3) · partial 5/142 (3) | 4h29 |
| f68 | b90A03 | 08-31 17:37 | 1 round, solved | **parked** at scribe-reduce, `:review-set-mismatch`, disposition `:partial` (codex-17, 19:02) | **success 0/143 (0)** · **success 1/146 (0)** · **closed 1/148 (0)** | — (a1's proof pinned by Opus at `refs/apm/rescued-solves/b90A03/b3751400`, not swept) |
| f69 | b93A01 | 08-31 18:58 | 1 round, progress → verify | closed, solved, **banked** (`9fe22909`) | partial 0/143 (2) · **proved 4/148 (0)** · partial 1/150 (2) | 1h45 |
| f70 | b93J01 | 08-31 20:44 | round 25 of 50 at 23:29Z, 1 sorry (support-6 commutator case of a minimal-support argument in Aₙ, n ≥ 6) | at `:solve` | — | 2h45 and counting |

Bold = archived source has 0 `sorry`. All attempts in all frames start from the
same base (`:base-revision 9fa428f7`, the pinned apm-lean master), not from the
previous attempt's file; the solver's work reaches a student only as scribe
memories. That is what makes the a1/a2 comparison in §4 a comparison.

For continuity with the last note: f58 (aunk04) and f60/f61 (b00J02, b01A02) ran
one attempt each and parked at the guide-mode barrier; f59 voided; f62 (b01J01),
f64 (b01J03), f65 (b01J04) closed with the problem solved; f63 (b01J02) parked at
promote-solver with disposition `:retry`. Banked as of tonight: 14 problems, open
queue 109 (`51b4b724`). Stranded sorry-free student proofs with a rescued pin and
no sweep: a99J05, a99J06, aunk04, b90A03 — all from *parked* frames, as the Opus
note's 08-31 correction says.

## 2. The repairs: recommendation by recommendation

Against `TN-fable-F36-F57-review` §7 and the addendum's gaps (a)–(c).

| # | recommendation | state on 2026-08-31 |
|---|---|---|
| 1 | f42/a1 in prereg with the two-count rule | done (amendment 10) |
| 2 | correct the Opus f53 tally | done; the note has since corrected *itself* again (08-31, "prior-frame reads are not zero, and never were"), which is the right direction |
| 3 | fingerprint audit at frame close | **wired, ineffective** — every per-frame artifact is empty; §3 |
| 4 | process-memory verdict class | done and reaching the verdict: `not-adjudicable-by-token` fires on f67/a3's regulative row; `:memory-use/kind` is read off the reviewed `memory/assert` edge (`21389e00`), so addendum gap (b) is closed. Rows from f62 on carry `substitutive`/`regulative`; f58's and the f34 memory's stay `unknown`, as they should |
| 5 | f53 guide-model boundary | done (amendment 10, `seat-cast.edn`) |
| 6 | re-validate f33/f35 under `2d9e08a8` | done as a record (8/8), unpublished |
| 7 | population A | the six proof-text ids gated out (`:proof-text-candidate-publishing-forbidden`); population A itself still unrun |
| 8 | non-UUID close count | subsumed: close receipts now carry a controller-derived `:receipt/memory-use-audit` per id with attempt ordinals — I checked f66, f67, f69 against the attempts' `:used-ids` and they match exactly |
| 9 | verify `:claimed-defect` parks the frame | the mechanism has existed since `a5975fe2` (08-21): a round reporting `:claimed-defect` with a residual moves the solve to `:solver-defect-review-required` and stops; `codex-solver-v5.md` tells the solver when to use it. No solver in f58–f70 emitted it, so it remains unexercised in a live frame. Closed as "exists", open as "shown to work" |
| (a) | proctor v4 / guide card line | done: `promotion-proctor-v4.md`, `claude-guide-v2.4.md`, condition C-12 |
| (c) | conditions-registry entry | done: C-11 (from f60), C-12 (from f61); C-13/C-14 added tonight for the exhausted-repair termination |

Defects that surfaced *inside* this window and what happened to them (all from
the Opus note; I checked the code and the commit for each):

- **A finding the machine detects and cannot ask anyone to fix.** f67's regulator
  died on an `:attachment-projection` finding with no repair instruction;
  `69109f33` renders that one shape. An hour later f68 parked on a sibling —
  `:review-set-mismatch` (the proctor persisted an empty review vector) — which
  had no instruction either. Tonight `live_learning_phases.clj` carries
  instructions for `:reviewer-missing`, `:review-set-mismatch`,
  `:review-attribution-mismatch`, `:review-verdict-invalid`,
  `:review-reasoning-missing` (`15eaf71d`, 19:02), and `0a0a87e3` terminates an
  exhausted hold instead of waiting forever (C-13/14). f68 itself is still parked;
  the fix arrived seven minutes after it parked.
- **A Mathlib cache refresh stopped the campaign three times in fourteen
  minutes**, because `countdown-manifest/validate` runs a full Lean build and
  cannot tell "does not qualify" from "could not build". `bdf494aa` keeps the
  `ex-data` so the next such stop names its requirement; the coupling itself is
  unchanged.
- **The Lean successor-record gate can no longer fail on the review-repair
  path.** `55ce42a4` computes a `collectionEvidenceId` from `{job-id, findings}`
  when no collection was recorded, which is the only thing
  `predecessorRecordComplete` (`APMCampaignTraceChecker.lean:137`) exists to
  check. The Opus note calls this "a gate that cannot fail is worse than an
  absent one"; nothing since has touched it (`git log -S promotion-repair-terminal`
  shows only the one commit). Noting it here so it does not read as settled.
- The defect register (`TN-apm-defect-register.md`) has not been updated since
  08-28 (A11). Everything above lives only in the Opus note's running sections.

## 3. The close-frame audit writes an empty file every time

`frame-fingerprint-audit/audit!` is called from `record-close-observations!`
with `:state-directory` = the frame's directory
(`…/jit-all-open-v2/jit-all-open-v2-f69`). It takes the directory's *file name*
as the campaign id and its *parent* as `APM_CAMPAIGNS`, then runs

    APM_CAMPAIGNS=…/jit-all-open-v2 fingerprint_audit.py --campaign jit-all-open-v2-f69 --json

The script's `attempts(campaign_dir)` iterates `os.listdir(campaign_dir)` looking
for frame directories that contain `live/`. Inside a frame directory the entries
are `live`, `analysis`, `terminal`, … — none has a `live/` child — so it finds
zero attempts and exits 0 with a well-formed, empty report. `valid-audit?`
accepts it, the status file says `:ok true, :audit/use-events 0, :audit/rows 0`,
and the close proceeds. I reproduced it by running the command by hand: `rows 0
use-events 0`. Every wired artifact on disk is in this state:

| frame | `analysis/` | rows | why |
|---|---|---|---|
| f62, f64, f65, f67, f69 | present, `:ok true` | **0** | the path bug |
| f66 | absent | — | closed by `countdown-recovery` (`complete-claimed!`), which does not call `record-close-observations!`; f66 also has no `analyst/` wake for the same reason |
| f68 | absent | — | parked, never closed |

The test (`frame_fingerprint_audit_test.clj:44`) pins the call as
`[[<parent> "campaign-1"]]`, i.e. it asserts the bug. The fix is one of: pass the
campaign root's parent and the campaign id, plus a `--frame` filter to the script;
or point the script at the frame directory directly. Either way the test should
assert on rows > 0 for a fixture with a `live/student-attempt-1.edn`.

Consequence for this note: every per-use number below comes from my own
campaign-level run (`fingerprint_audit.py --campaign jit-all-open-v2 --json`,
71 s, exit 0, saved as `/tmp/fp-v2-0831.json`), exactly as it did on 08-29. The
"automatic at close" property recommended in §7.3 does not yet exist in effect.

## 4. Memory use, f58–f69, and what it says about usefulness

### 4.1 By the artifact standard

32 use events (Student `:used-ids`, non-void frames f58–f69), resolved on :7073
for depositor and subject, verdicts from today's audit run:

| | fingerprinted | already-in-base | unwitnessed | not-adjudicable | total |
|---|---|---|---|---|---|
| within-frame (own guide/scribe) | 13 | 9 | 4 | 1 | 27 |
| cross-problem | **0** | 3 | 2 | 0 | 5 |
| all | 13 | 12 | 6 | 1 | 32 |

Paste: 0. The five cross-problem rows in full:

| frame/att | memory | depositor · subject | kind | verdict | what the student said |
|---|---|---|---|---|---|
| f58/a1 | `e-63b7c7c1` | f34-guide · a95J03 | reference (route) | already-in-base | — |
| f62/a1 | `e-63b7c7c1` | f34-guide · a95J03 | reference | unwitnessed | — |
| f66/a1 | `e-63b7c7c1` | f34-guide · a95J03 | reference | unwitnessed | "did NOT contain mathematics for this problem; only its generic method advice … was used, and that ordering was followed" |
| f67/a3 | `e-…c2523f52` "count-prime-order-elements-by-sylow-sigma-partition" | f64-guide · b01J03 | memory (content) | already-in-base | listed among five used; not singled out |
| f69/a3 | `e-…b5c6ea1c` "subgroup-pointwise-action-lives-on-mulaut-under-pointwise-locale" | f62-guide · b01J01 | memory (content) | already-in-base | "This spelling came from the surfaced conjugation-spelling memory (MulAut.conjNormal route)" — the compiled spelling is in a scratch file; `conjNormal` does not appear in any committed f69 source |

So the Opus note's 08-31 widening holds and gains a third point: two *content*
memories have now crossed problems through the shelf (f64→f67, f62→f69), both on
topically matched Sylow material, and both left nothing in the committed artifact
that the base lacked. The f69/a3 row is the useful one for calibrating
self-report against artifact: the student credits the memory with a spelling it
demonstrably compiled, and the audit is right that the proof it committed does
not contain it — the attempt failed before the spelling was needed in the file.
Both statements are true; "used" and "left a trace in the deliverable" are
different facts, and the record keeps both.

### 4.2 The within-frame contrast, f62–f69

Amendment 8 makes attempt 1 a same-problem holdout (the scribe has already mined
the solver's trace before a1 runs, so it is a real holdout). Attempts 2 and 3 get
those deposits. All three start from the same base file. The seven frames since
the guide loop was repaired:

| frame | withheld at a1 | a1 (holdout) | a2 | a3 | paired reading |
|---|---|---|---|---|---|
| f62 | 2 | open, 2 sorries (1 cross) | **solved** (4 own; 6+8+1 novel tokens) | partial, 3 sorries (4 own) | deposit arm closed, holdout did not |
| f64 | 2 | partial, 1 sorry | partial, 1 sorry (1 own, in-base) | incomplete, 1 sorry | neither |
| f65 | 1 | not-closed, 1 sorry | not-closed, 1 sorry (2 own) | **success** (1 own; 12 novel tokens) | deposit arm closed, holdout did not |
| f66 | 1 | **proved** (1 cross) | **solved** (1 own; 2 novel) | **closed** (2 own; 3+2 novel) | both — deposit not necessary |
| f67 | 3 | partial, 3 sorries | partial, 3 sorries (2 own) | partial, 3 sorries (4 own + 1 cross) | neither — hardest problem in the campaign |
| f68 | 1 | **success** | **success** (1 own; 5 novel) | **closed** (1 own; 5 novel) | both — deposit not necessary |
| f69 | 4 | partial, 2 sorries | **proved** (4 own; 5+3 novel) | partial, 2 sorries (1 cross) | deposit arm closed, holdout did not |

Three frames favour the deposit arm, none the reverse, two are at ceiling, two at
floor. Attempt-level: holdout attempts closed 2 of 7; deposit-arm attempts closed
7 of 14. Three caveats, each verifiable above:

1. **Session variance is large.** f62/a3 had the same four memories as f62/a2 and
   left three sorries; f69/a3 had the four deposits on its shelf, reached for a
   cross-frame memory instead, and failed. Memory presence is not memory use, and
   use is not closure.
2. **The prereg's own confound stands** (amendment 8, `:confound/unresolved`):
   withholding shrinks the shelf as well as changing it. No size-matched control
   has been run.
3. **Amendment 3 rules that within-frame attempts are not the campaign's
   contrast.** This table answers "does the solver's route, packaged as memory,
   help a fresh student on the same problem?" — the whitepaper's within-frame
   guidance loop — not tier-A condition 3.

### 4.3 f69 in detail, because it is the case to cite

b93A01 (a group of order pq embeds in S_{p+q}, minimal degree). Four
`f69-scribe` memories were deposited 19:24–19:25Z from the solver's one-round
trace and approved by the proctor (`promote-solver.edn`, 4 approve). a1 (19:33Z)
had all four withheld, ran two obstacle-vocabulary searches, closed the
nonabelian branch by the normal-core argument and left the abelian branch and
the degree bound: 2 sorries. a2 (19:52Z) had them, ran one search, and proved it:
339 lines, `[propext, Classical.choice, Quot.sound]`, statement unchanged. The
audit finds two of the four fingerprinted — `Equiv.Perm.lcm_cycleType`,
`Equiv.Perm.sum_cycleType`, `Multiset.mem_erase_of_ne`, `Multiset.lcm_cons`,
`Multiset.dvd_lcm` (5 novel) and `Subgroup.subgroupOf_eq_top.mp`,
`Subgroup.subgroupOf_eq_bot`, `MonoidHom.ker_eq_bot_iff` (3 novel); I confirmed
four of these by grep against the base. The other two are unwitnessed.

The student's own account also records the memory getting something wrong:
"`Multiset.le_sum` … (named in memory e-…0bee2494) … unknown constant in this
Mathlib version; grep over Mathlib found no `Multiset.le_sum`. Replaced with a
cons_erase-based helper." A memory that names a lemma that does not exist in
the pinned Mathlib costs a probe and a workaround; the student still closed. That
is a small, concrete example of what "useful" looks like at present: the route
and most of the API, with the student paying for the errors.

### 4.4 What the solving agents actually get

- **The solver gets nothing.** `solve.edn` for f66–f70 carries no memory field
  and `codex-solver-v5.md` does not mention memory. b03J01, b90A01, b90A03,
  b93A01 were solved by an agent with no shelf; b93J01 is being attempted the
  same way (25 rounds so far). If the question is "does memory help an agent
  solve open problems", the campaign is not currently asking it of the agent that
  solves them.
- **The student gets ~150 ids, of which ~24 are readable through its own
  `memory_read` tool**; the `e-apm-promotion-*` ids reach it through the
  controller-owned search. f67/a1 reported "the APM-store ids are not readable
  through my local memory_read tool; none was fetched" and f67/a3 "fetched all 24
  non-promotion memories … every one of them is complex analysis or measure
  theory". Every fingerprinted use in the window arrived by search hit on
  obstacle vocabulary, not by browsing the shelf. The shelf-as-list is not the
  delivery mechanism; the search is. (One attempt's report; consistent with the
  others' accounts of how they found what they used.)
- **The one memory that crosses problems by browsing is the f34 route memory**,
  still (f58, f62, f66 in this window). Every student that reads it says the
  same thing — it has no mathematics for their problem, they followed its
  ordering. Amendment 10's `not-adjudicable-by-token` class is now what the audit
  would say about it if its kind were recorded; its kind is `:reference` from
  before the typed regime, so it reads `unknown` and the verdict stays
  `unwitnessed`. A one-line backfill of `:memory-use/kind :regulative` on that
  edge would move three rows to the honest class.

## 5. The cascade arm has been down for most of the window

`:receipt/memory-cascade` on the 25 attempts f58–f69:

| | count | note |
|---|---|---|
| `:error` HTTP 503 `:expensive-read-busy` on `/api/alpha/hyperedges` | **15** | f60/a1; f64/a2; f65/a1,a2; f66/a1,a3; f67/a1,a2,a3; f68/a1,a3; f69/a1,a2,a3 — after 14–235 s of `:expansion-ms` |
| ok, `offers` 0 or 100 (cap), `truncated? true`, `expanded-available` 178–181 | 10 | `used-via-cascade` = `[]` in all ten |

So condition C-6's sibling-route cascade has delivered zero used memories in the
window, and in the attempts where it failed each student's dispatch spent about
two minutes on a read that returned 503 before the student started. The
`expensive-read-busy` refusal is futon1b protecting itself (the bounded-read
commits of 08-30 — `19e1760e`, `1b82248c`, `ac262917` — are the same theme from
the other side); it is doing its job. What is missing is that a failed cascade is
recorded on the attempt and nowhere else: no conditions entry says "cascade
inoperative from f65", the audit's `delivery-route` column reads `shelf` for
everything, and the f67/a3 student was the one to notice ("the problem-specific
shelf appears to be missing entirely"). If the cascade is part of the
experimental design it needs a per-frame status line; if it is not, C-6 should be
closed and the two minutes given back.

## 6. Recommendations (ordered; one file / one behaviour each)

1. **Fix `frame-fingerprint-audit/audit!`'s path handling** so the wired audit
   produces rows, and change the test to assert on a non-empty fixture. Until
   then re-run the campaign-level script by hand after each close (I did today).
2. **Make `countdown-recovery` closes call `record-close-observations!`**, or
   record explicitly that a recovery close skips the audit and the analyst wake.
   f66 is missing both.
3. **Record cascade status per attempt in the audit and per frame in
   `conditions.edn`** (`:cascade :failed-503` / `:ok`), and decide whether C-6 is
   still an arm. Fifteen 503s across seven frames is a boundary, not noise.
4. **Backfill `:memory-use/kind :regulative` on `e-63b7c7c1`** so its three
   window rows and the four earlier ones read `not-adjudicable-by-token`.
5. **Sweep or refuse the four rescued solves** (a99J05, a99J06, aunk04, b90A03).
   Opus pinned them and stopped, correctly; the decision is Joe's, and until it
   is made the bank count under-reports the machine's solves by four.
6. **Give the solver a shelf on one frame** — even a read-only snapshot of the
   scribe's prior deposits on the same topic — before claiming anything about
   memory and solving. Today the only agent solving open problems does so without
   the memory system. That is a design choice; it should be stated in the prereg
   as one.
7. **Add a size-matched holdout control** (amendment 8's unresolved confound) on
   the next frame that reaches a2: withhold N random cross-problem memories
   instead of the N same-problem ones. Three favourable frames of seven is enough
   to be worth the one extra attempt.
8. **Update the defect register** from the Opus note's 08-29–08-31 sections, or
   retire it and say the Opus note is the register.
9. **Revisit `55ce42a4`.** A digest over fields already in the observation
   satisfies `predecessorRecordComplete` without a collection ever existing.
   Either record a real collection on the review-repair path or make the Lean
   predicate say what it now checks.

## 7. What I checked

- Ledgers (`ledger.edn`, all events) and `terminal/frame-terminal.edn` for
  f58–f70; `live/preflight.edn`, `solve.edn` (round counts, outcomes, last
  residual), `student-attempt-{1,2,3}.edn` (outcome, holdout, withheld ids,
  accessible/surfaced/used ids, queries, `:receipt/memory-cascade`,
  `:receipt/failure-account` in full for f66–f69), archived
  `student-attempt-N-source/*.lean` (line and `sorry` counts),
  `promote-solver`/`scribe-reduce`/`guide-intervention-N-review` verdict counts,
  `close-frame.edn` `:receipt/memory-use-audit` (matched against `:used-ids` for
  f66, f67, f69). Scripts: `/tmp/frame-extract2.clj`, `/tmp/used-ids3.clj`,
  `/tmp/attempt-detail.clj`, `/tmp/base-cascade.clj`, `/tmp/cascade-use.clj` (bb).
- Every `:used-ids` entry f58–f69 resolved on :7073 for author/subject/at;
  `e-63b7c7c1` and `e-…b5c6ea1c` bodies read.
- `fingerprint_audit.py --campaign jit-all-open-v2 --json` (71 s, exit 0; 64
  rows campaign-wide, 32 in f58–f69); f69/a2's novel tokens spot-checked by grep
  against the base at `9fa428f7`; the wired invocation reproduced by hand with
  `APM_CAMPAIGNS` set as `audit!` sets it (0 rows).
- `frame_fingerprint_audit.clj`, `countdown_control.clj` 1395–1410 and
  1725–1760, `frame_fingerprint_audit_test.clj` assertions,
  `fingerprint_audit.py` `attempts()`; `live_solver_rounds.clj` `:claimed-defect`
  handling and `git log -S`; `live_learning_phases.clj` repair-instruction table;
  `codex-solver-v5.md`, `zai-student-v2.md`; role-card directory listing.
- `conditions.edn` C-10–C-14; prereg amendments 8, 10, 11;
  `frame-park-decisions.edn` f68 entry; apm-lean `refs/apm/banked-solves` and
  `refs/apm/rescued-solves`; futon3c log 08-30 12:45 → 08-31 23:20 (subjects).
- Read in full: `TN-fable-F36-F57-review` (with addendum),
  `TN-typed-memory-supply-experiment`, `TN-apm-defect-register`, and the
  f58–f68 sections of `TN-opus-f47-observation`.

Not checked: Lean recompilation of any attempt (I relied on archived source and
receipt `:axioms`/`:lean` fields, and on Opus's builds for the rescued solves);
live transcripts; Agency job records; whether f70 closes.
