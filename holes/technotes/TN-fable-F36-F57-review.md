# TN: F36–F57 review — twenty-two frames since the F32–F35 bank review

Author: Claude (Fable 5, claude-16), 2026-08-29 ~22:40Z, at Joe's request
("review recent apm frames since the last fable review"). Scope: every frame
minted after `TN-fable-F32-F35-bank-review.md` (2026-08-25 17:46Z): f36–f44
in `jit-all-open-nontopology-v1`, f45–f57 in `jit-all-open-v2`. Read against
that note's nine recommendations, `prereg-capability-transfer-v1.edn`
(amendments 1–9), and the notes written in the window — `TN-opus-F41-analysis`,
`TN-opus-f47-observation` (f46–f53), `TN-opus-f48-critical-findings`,
`TN-apm-defect-register`, `TN-same-problem-holdout`,
`TN-refuted-statement-disposition`. Where those notes already cover a frame I
checked their claims rather than re-telling them. §8 lists what I ran.

## Short answer

1. **Twenty-two frames minted, ten certified with the problem solved.** Six
   closed (f37, f40, f42, f44, f52, f53), four partial with the solver at
   0 sorries (f41, f46, f47, f50), three voided as apparatus-invalidated
   (f48, f49, f51), one voided as statement-refuted (f45 → re-minted as f46
   on the corrected a96J08), five disposed `:partial` by park decision
   (f36, f38, f43 at `:solve`; f54, f55 at guide-intervention-1), one parked
   and re-runnable (f39, scribe-reduce), two in flight (f56, f57). §1.
2. **Transfer, measured on the artifacts:** 80 recorded memory uses in the
   window, 40 fingerprinted, 69 same-problem and 11 cross-problem. Of the 11
   cross-problem uses **one is fingerprinted — f42/a1** — and nobody has
   written it up. It is the closest thing to a tier-A condition-3 instance in
   the data, and §3 says exactly how far it goes: the memory supplied the
   side-condition spellings for a lemma the base file had already named.
3. **The within-frame result is older and broader than the f47 note says.**
   Guide/scribe deposits used by a later attempt in the same frame are
   fingerprinted with specific Mathlib names in f37, f40, f42, f44, f46, f52
   and f53 — from the first frame in this window, not from f52. All but f53
   ran under a Claude guide; f53 is the only glm-guide frame with the
   result, and f54/f55's guides died at an apparatus gate. §3, §5.
4. **Two corrections to `TN-opus-f47-observation`.** Its f53 section's
   running tally ("no attempt has used a shelved memory that predates its
   own frame" across f49–f53) is contradicted by two f50 rows in its own
   earlier sections; and the one prior-frame memory the shelf has delivered
   *twice* across problems (f44-scribe's rewrite rule, in f47/a3 and f50/a3)
   is missing from it. §4.
5. **Of the nine recommendations, five landed, two half, two did not.** The
   fingerprint check is still an offline script nobody re-ran for f36+ until
   today; `E-early-memories-review` population A is still unrun and its six
   proof-text memories are still being offered. §6.

Recommendations, small and ordered, in §7.

## 1. What the twenty-two frames did

| frame | problem | opened (Z) | frame result | Student attempts (outcome · used/accessible) | wall-clock |
|---|---|---|---|---|---|
| f36 | a96A07 | 08-25 20:19 | park → `:partial`, `:library-infrastructure-absent` (50 rounds, 1 sorry at line 3168) | — | — |
| f37 | a96A08 | 08-25 22:59 | closed, solved | partial 0/22 · rejected-evidence 0/24 · **success 5/26** | 3h05 |
| f38 | a96J08 | 08-26 02:04 | park → `:partial`, `:problem-statement-false` (sign error; solver refuted it in round 1, then ran 49 more) | — | — |
| f39 | a97A01 | 08-26 03:02 | park at scribe-reduce, `:apparatus-fault-already-repaired`; 5 unresolved `cannot-judge` | partial 0/27 · partial 1/27 · partial 3/29 | — |
| f40 | a97J05 | 08-26 05:03 | closed, solved | partial 4/31 · **success 4/33** · solved 7/34 | 2h40 |
| f41 | a97J06 | 08-26 07:43 | partial, solved | partial 4/39 · partial 5/41 · not-closed 4/42 (scratch-file blindness, `TN-opus-F41`) | 2h55 |
| f42 | a97J07 | 08-26 12:12 | closed, solved | **proved 1/48** · rejected-evidence 0/48 · proved 2/50 | 3h09 |
| f43 | a97J08 | 08-26 15:21 | park → `:partial`, `:library-infrastructure-absent` | — | — |
| f44 | a98A02 | 08-26 17:28 | closed, solved | **success 3/53** · closed 2/55 · closed 3/56 | 2h08 |
| f45 | a96J08 | — | void, `refuted` (statement false; corrected `9fa428f7` 08-26 21:36) | — | — |
| f46 | a96J08 | 08-26 21:55 | partial, solved | not-closed 0/0 (repaired; original leaked 2 withheld) · not-closed 4/5 · partial 5/7 | 10h31 |
| f47 | a97A01 | 08-27 08:26 | partial, solved | partial 1/67 · partial 4/72 · partial 5/73 | 3h46 |
| f48 | a98A03 | 08-27 12:13 | void, apparatus (holdout leak on search) | open 0/76 (repaired) | 1h25 |
| f49 | a98A04 | 08-27 13:38 | void, apparatus (dead on first tick; one clean attempt later) | not-closed 0/76 | — |
| f50 | a98A07 | 08-28 10:00 | partial, solved | partial 1/76 · partial-bridge-1 2/81 · partial 2/83 | 5h26 |
| f51 | a98J03 | 08-28 15:57 | void, apparatus (phase order lacked promote-solver; shelf 0/0) | not-closed 0/0 | 1h46 |
| f52 | a99J01 | 08-28 17:44 | closed, solved | **solved 0/84** · solved 2/87 · solved 2/89 | 2h35 |
| f53 | a99J03 | 08-28 20:20 | closed, solved | partial 0/94 · solved 1/99 (re-dispatched after wall-clock death) · closed 3/102 | 21h57 |
| f54 | a99J05 | 08-29 18:17 | park at guide-1 → `:partial`, guide terminal schema mismatch | **success 0/110** | — |
| f55 | a99J06 | 08-29 19:11 | park at guide-1 → `:partial`, guide payload normalization mismatch | **solved 0/110** | — |
| f56 | a99J08 | 08-29 20:28 | at guide-1 | not-closed 0/110 (2 sorries) | … |
| f57 | a99J10 | 08-29 22:04 | at promote-solver | — | … |

Bold = the attempt's archived source has 0 `sorry`. f54 and f55 carry
`:axioms ["propext" "Classical.choice" "Quot.sound"]` and `lake env lean …
exits 0` in the receipt; I did not recompile them (§8).

Two things the table shows that the per-frame notes do not:

- **The solver closes most of what it is given.** `gpt-5.6-sol` reached
  `:problem/outcome :solved` in 10 of the 14 non-voided frames that got a
  solver verdict; of the other four, f36 and f43 were disposed as library
  gaps, f38 as a false statement, and f39's solver did not close a97A01
  (retried as f47, where it did). The open-problem corpus was repinned to
  118 after banking three solves (`f4f38c6d`).
- **Student attempt-1 closes with an empty hand are now the mode in v2.**
  f52, f54, f55: three consecutive attempt-1 solves, shelves of 84–110,
  `:used-ids []`. Before f52 the only attempt-1 closes were f42 (1 used,
  fingerprinted) and f44 (3 own-frame scribe memories, fingerprinted). Since
  the a99* problems are the tail of the queue, a problem-difficulty
  explanation is as available as a shelf-utility one; nothing in the
  records separates them.

## 2. Memory use in the window, by the artifact standard

I re-ran `analysis/fingerprint_audit.py` (with the 08-28 rarity rule
`3bae5b51`) on both campaigns; outputs saved as
`analysis/fingerprint-2026-08-29-{jit-all-open-nontopology-v1,jit-all-open-v2}.json`.
The 08-25 JSON covered f28–f34; nothing had been run for f36+ as a set.

Use events f36–f57 (Student `:used-ids`, joined to the depositor and subject
on :7073):

| | fingerprinted | already-in-base | unwitnessed | weak | total |
|---|---|---|---|---|---|
| same-problem (memory subject = frame problem) | 39 | 19 | 8 | 3 | 69 |
| cross-problem | **1** | 4 | 4 | 2 | 11 |
| **all** | 40 | 23 | 12 | 5 | 80 |

Paste: 0 events in the window (tier-A condition 4 holds).

By depositor: 68 of the 80 uses are the frame's own guide/scribe deposits,
made minutes to hours earlier; 9 are prior-frame shelf memories; 3 are
external (codex-2 07-26, ams-codex-1 08-04, codex-5 07-30 — the last two
reached the Student via the cascade or search, not the shelf).

The 11 cross-problem uses, in full:

| frame/att | problem | memory | depositor · subject | route | verdict |
|---|---|---|---|---|---|
| f39/a2 | a97A01 | `e-63b7c7c1` "bank-cheap-sorries-first…" | f34-guide · a95J03 | shelf | already-in-base |
| f39/a3 | a97A01 | `e-1ac936fb` "symmetric-interval-law-to-dyadic-differentiation" | codex-2 · M-zai-learning-loop | search | unwitnessed |
| f40/a3 | a97J05 | `e-e9f9c621` "verified-api-card-memlp-power-law…" | f32-guide · a92J01 | shelf | weak (`NNReal`, `p_zero`) |
| f41/a1 | a97J06 | `e-73ac922d` | f37-guide · a96A08 | shelf | already-in-base (nothing committed) |
| f41/a3 | a97J06 | `e-3411c0c2` "entire-injective-affine-route…" | f33-guide · a94A07 | shelf | already-in-base (0 of 26 novel) |
| **f42/a1** | **a97J07** | **`e-f72e5ece` "blaschke-canonical-product-constancy-not-polynomial"** | **f29-guide · a01J05** | **shelf + search** | **fingerprinted** |
| f47/a2 | a97A01 | `e-a97j02-measure-finite-union-closed-grid-cells` | ams-codex-1 · a97J02 | cascade | unwitnessed |
| f47/a3 | a97A01 | `e-apm-promotion-0af2ca3a…` "refuted-blocked-route-assemble-truncation-limit-from-corpus-pieces" | f44-scribe · a98A02 | shelf | unwitnessed |
| f50/a1 | a98A07 | `e-63b7c7c1` (again) | f34-guide · a95J03 | shelf | weak (`norm_num`) |
| f50/a2 | a98A07 | `e-codexpilot-upgrade-diskwise-L1-convergence…` | codex-5 · M-codex-sorry-loop | cascade | already-in-base |
| f50/a3 | a98A07 | `e-apm-promotion-0af2ca3a…` (again) | f44-scribe · a98A02 | shelf | unwitnessed |

Two prior-frame memories were reached for on two different problems each,
both through the shelf: f34-guide's "bank the cheap sorries first" (a process
memory) and f44-scribe's "the blocked route is refutable; assemble from
corpus pieces" (a rewrite rule). Neither left a token in any artifact. The
memories that cross problems are the ones that say *what to do*; the ones
that leave fingerprints (API cards, lemma names) stay on their own problem.
That is consistent with the whitepaper §4.2 reading — users find work by
structural similarity — and it is also exactly the class the fingerprint
standard cannot adjudicate (`TN-spec-delta` §11). Worth stating in the
prereg rather than letting these two rows sit as "unwitnessed" as if that
settled them.

## 3. f42 attempt 1 — the one fingerprinted cross-problem use

**What happened.** a97J07 asks ‖f 0‖ ≤ √(ab) for f analytic on the disc with
‖f‖ ≤ a on the open upper arc and ≤ b on the open lower arc. The Student
(zai, `zai-student-v2`, 48 accessible) formed g(z) = f(z)·f(−z), extended
the open-arc bounds to the equator points by an explicit normalised curve
and `le_of_tendsto`, and closed with `norm_le_of_forall_mem_frontier_norm_le`
on the ball: `lake env lean` exit 0, 0 sorries, standard axioms, 166 lines
against a 33-line base. It ran one search — "maximum modulus principle
frontier bound … norm_le_of_forall_mem_frontier_norm_le" — and reported one
memory used: `e-f72e5ece`, f29-guide, 2026-08-24, subject a01J05, about why
a Blaschke-product constancy step fails and how to repair it. The memory was
on the shelf (`:accessible-memory-ids` contains it) and also in the search
hits.

**Fingerprint.** 29 tokens named, 7 present in the artifact, 5 novel to the
base: `isBounded_ball` (4/508 problem files on origin/master), `frontier_ball`
(4/508), `mem_ball_self` (11/508), `one_ne_zero` (21/508 — just under the 5%
rule), `one_pos` (2/508). I recomputed those frequencies by hand; the rule is
working as designed (`norm_num` 214/508, `exact_mod_cast` 66/508 are
correctly non-witnessing).

**What transferred, in the Student's own words:** "The only materially used
memory was e-f72e5ece (Blaschke constancy), which supplied the exact
max-modulus API pairing (`norm_le_of_forall_mem_frontier_norm_le` +
`DiffContOnCl` from analytic+continuous) — the same pairing its Step C used."
And: "none of the 40 snapshot memories addressed this residual's core."

**How far it goes.** Three caveats, each verifiable in the record:

1. The base file's boundary comment already named
   `Complex.norm_le_of_forall_mem_frontier_norm_le` (base count 1; the
   Student's failure account says its first step was correcting the
   namespace). The route was given; the memory did not select it.
2. The memory's headline content — canonical factors,
   `meromorphicTrailingCoeffAt`, the two-directional constancy argument — is
   absent from the artifact (0 hits). What was taken is the *application
   recipe* for one lemma: bounded ball, frontier of ball, `DiffContOnCl.mk_ball`.
3. The novel witnessing tokens are the side-conditions any proof of a97J07
   via that lemma would need. They are rare in the corpus because the corpus
   is mostly not max-modulus problems, not because they are distinctive of
   this memory.

So: it meets the letter of whitepaper §3.1 (named identifiers, novel to the
base, in a committed, compiling artifact), it crossed a problem boundary
(a01J05 → a97J07) and a campaign wave (f29 → f42), and it came through the
shelf. It does not meet the sense of condition 3 in which the memory changes
what the Student does. It is the same shape as f33/a94A07 in the F32–F35
note — the memory confirmed and supplied API for a route the Student was
already on — with the difference that this time it *was* on the shelf.
Condition 3's count, read strictly, is still 1 (f33, via search); read by
the §3.1 letter, it is 2, and this is the first through-the-shelf instance.
The prereg should say which reading it uses; §7 item 1.

Also in f42: a controller-owned search run *after* the proof was complete
surfaced `e-apm-promotion-f240c45c…` (f42-scribe) describing the arc-limit
residual the Student had just solved by hand. The Student declined to claim
it. That is the witness discipline working, and it is also the clearest
instance in the window of the shelf holding the right thing at the wrong
time — the scribe wrote it from the solver's trace while the Student was
running.

## 4. Checks on the standing notes

**`TN-opus-f47-observation`, f53 section.** "Running tally since the
depositor-truth holdout was repaired — ten clean attempt observations across
f49, f50, f52, f53: no attempt has used a shelved memory that predates its
own frame." Two f50 rows contradict it, both on the shelf
(`:accessible-memory-ids` contains the id): f50/a1 used `e-63b7c7c1`
(f34-guide, a95J03) — which the same note's own f50/a1 section discusses at
length as "on the shelf" — and f50/a3 used `e-apm-promotion-0af2ca3a…`
(f44-scribe, a98A02), which the note does not mention at all. The corrected
sentence is: no attempt in f49–f53 *left a fingerprint from* a shelved
memory predating its frame (verdicts weak and unwitnessed). That is the
claim the evidence supports and it is nearly as strong.

**Same note, f47/a3.** Not covered there; it is the first appearance of the
f44-scribe memory crossing problems (a98A02 → a97A01) through the shelf,
unwitnessed. With f50/a3 that makes it the only prior-frame shelf memory
reached for twice in the window.

**`TN-opus-F41-analysis`, condition 3.** Confirmed: f41's committed artifact
(a3) carries 0 of the f33-guide memory's identifiers; all its uses are
already-in-base or unwitnessed under today's tool. The note's "f40's four
approved memories were used zero times in f41" also holds — none of the
four f40-scribe ids appears in any f41 `:used-ids`.

**Defect register A10 / f51.** Confirmed from the record: no `promote-solver`
in `live/`, `:accessible-memory-ids` absent, `used 0/0`. Excluding f51's
learning arm is right.

**f45/f46 and the refuted statement.** The correction to a96J08
(`9fa428f7`, 08-26 21:36Z) precedes f46's mint (21:55Z), and f46's
`:base-revision` is `9fa428f7`; the f38 park's refutation (ω = 1/4 witness)
concerns the old sign. Nothing in f46 depends on the false statement.

## 5. Instrument-version boundaries inside the window

The prereg's per-frame rule requires these to be recorded; amendments 5–9
cover some. The full list as I can reconstruct it from `conditions.edn`,
the commit log and the notes:

| from | change | where |
|---|---|---|
| f43 | packets archived verbatim (`live/*-packet.txt`); snapshot ordering v2 | v1 `conditions.edn` C-1, C-2 |
| f45/f46 | memory-cascade arm, sibling route, cap 100; campaign lineage v1→v2 | C-6, `memory-cascade-arm.edn` |
| f46 | same-problem holdout on attempt 1 (amendment 8) | `TN-same-problem-holdout` |
| after f48 | holdout enforced on the search channel (`d3cf69df`); superseded terminals retained (`b642640a`) | `TN-opus-f48` |
| f49 | depositor-truth gate at the pre-serve point (`aec461a4`/`a79ac3c3`) | `TN-opus-f47` §f49 |
| f50 | seat models pinned in `seat-cast.edn` (`832aae55`): guide `claude-opus-5`, student `glm-5.3`, solver/scribe `gpt-5.6-sol` | this note |
| f52 | phase order restored after A10 (`a689fba1`); student dispatch refuses a nil promotion receipt | defect register |
| **f53** | **guide and analyst seats moved to zai `glm-5.3` (`12cf8b48`)** | this note |
| retro | amendment 9: every zai seat had been glm-5.3 all along (glm-5.2 silently substituted) | prereg |
| offline | fingerprint rarity rule (`3bae5b51`) | `TN-opus-f47` |

The f53 boundary is the one no note has flagged. The within-frame transfer
result (§2, item 3 of the short answer) rests on seven frames, six under a
Claude guide and one — f53 — under glm-5.3. f53's a3 did close on its guide's
deposit with four specific witnessing tokens
(`contDiffOn_succ_iff_deriv_of_isOpen`, `contDiffOn_infty`, …), so the
result did not vanish at the model change; but n=1, and f54/f55 — the next
two chances — lost their guides to apparatus gates before any deposit
landed. If the campaign continues under the glm guide, the stratum should be
reported separately until it has more than one point.

## 6. The nine recommendations from 08-25

Traced through the log by a subagent; I spot-checked `2d9e08a8` and
`e2acba41` against their diffs.

| # | recommendation | status |
|---|---|---|
| 1 | fix keywordized pattern-accounting; re-validate f33 guide-1 / f35 promote-solver | fix landed `2d9e08a8` (18:04, minutes after the note); **re-validation never run** — f35 guide-2 ran at 18:08 on a JVM without it |
| 2 | real path for coined patterns | done: `coined_pattern.clj` + `apm-ingest-coined-pattern-files.sh` (`185ab50e`), published as `:proposed` before review; the "watcher ingests this file" sentence replaced (`45322ad3`); ten scribe pattern files committed (`04c3e0a7`) |
| 3 | fingerprint at close-frame; retro-apply | retro-applied offline (`02684230`, f28–f34); **not wired** — all ten close-frames in the window carry a prose `:used` integer, no per-id record; f36+ unaudited until today's run |
| 4 | fix `memory_shape.py` origin + close-frame non-UUID count | first done (`e2acba41`: reuse reads 1 not 0); **second not done** |
| 5 | Zai-scribe review test before f36 | resolved on the scribe side instead: `zai-scribe-v2` sends protocol to the Student card, not the shelf; proctor card untouched; the whitepaper still lists the contradiction as open (§ around line 731) |
| 6 | run population A | **not done**; `E-early-memories-review.md` untouched since 08-24; three of the six ids offered again as cascade candidates on f42 (`f42a-cascade-example.edn`) |
| 7 | repair or retire f31 | retired in writing (prereg frame record, "REPAIR DEFERRED, deliberately (Joe)"); `prior-dropped` still lists it on every frame through f57 (7 entries: f31 + six f35 ids) |
| 8 | prereg hygiene | boundaries recorded (amendment 1); `:TBD/operator` deliberately left (amendment 3); "used, not helped" stated in whitepaper §5a |
| 9 | whitepaper structure | done in `e2acba41`: §5a, §2.1 items 14–20, §3.1b (f33) |

## 7. Recommendations (ordered; each one file / one behaviour)

1. **Prereg: record f42/a1** as a frame-record entry under condition 3 with
   the three caveats from §3, and state which reading of condition 3 the
   score uses (route-selecting vs §3.1-letter). Under the letter it is the
   first through-the-shelf instance; under the stricter reading the count is
   unchanged.
2. **Correct the f53 tally sentence in `TN-opus-f47-observation`** to the
   fingerprint form (§4). Add the f47/a3 and f50/a3 rows.
3. **Run `fingerprint_audit.py` at frame close, automatically**, and commit
   its JSON per campaign. Today's outputs are in `analysis/`; the check has
   been offline for four days and 80 use events. If wiring into close-frame
   is still too much, a cron on the campaign directory is enough.
4. **Add a `process-memory` verdict class to the audit** (or a per-memory
   `:kind` gate) so a rewrite rule / process memory used cross-problem is
   reported as "not adjudicable by token" rather than "unwitnessed". Two of
   the eleven cross-problem rows are this.
5. **Record the f53 guide-model boundary** as a prereg instrument-version
   entry and report the within-frame transfer result by guide stratum.
6. **Re-validate f33 guide-1 and f35 promote-solver under `2d9e08a8`** —
   still eight candidates that passed the content test and were voided by a
   bug fixed the same day.
7. **Run population A.** The six proof-text memories are still on every v2
   shelf and still offered by the cascade.
8. **Close-frame `:used` for non-UUID ids** (rec. 4b) — still prose.
9. **Verify that a `:solver/outcome :claimed-defect` now parks the frame.**
   `TN-refuted-statement-disposition` adds the `refuted` outcome; whether
   the round-1 refutation in f38 would today stop the solver instead of
   spending 49 more rounds is not stated anywhere I found.

## 8. What I checked

- Frame records for f36–f57: `ledger.edn` (problem, open/last timestamps,
  transitions), `terminal/frame-terminal.edn`, `live/student-attempt-{1,2,3}.edn`
  (disposition, outcome, accessible / surfaced / used ids, queries, holdout
  fields, terminal-repair findings), `live/{promote-solver,scribe-reduce,
  guide-intervention-{1,2}-review}.edn` verdict counts and `prior-dropped`,
  `live/close-frame.edn` keys, `superseded/` presence, archived
  `student-attempt-N-source/*.lean` (line and `sorry` counts). Extraction
  scripts: `/tmp/frame-extract.clj`, `/tmp/used-ids2.clj` (bb).
- Every `:used-ids` entry (80) resolved on :7073
  (`GET /api/alpha/evidence/<id>`, EDN) for author, subject, timestamp;
  joined to the fingerprint rows.
- `fingerprint_audit.py --campaign … --json` on both campaigns (exit 0,
  0 unfetchable / no-source / no-base); token document frequencies for 14
  tokens recomputed with `git grep -l` over `origin/master`
  `problems/*/lean/Main.lean` (508 files, 277 sorry-free).
- f42/a1: base file at `f7de6887` vs archived source; grep for the memory's
  named constructions; the Student's failure account and shelf note from
  the receipt payload.
- apm-lean history of `problems/a96J08` (correction `9fa428f7` vs f46 mint).
- `frame-park-decisions.edn` records for f36, f38, f39, f43, f54, f55
  (`d11cf85b`, `51ffd80a`); `seat-cast.edn` and its two commits
  (`832aae55`, `12cf8b48`); v1/v2 `conditions.edn`; prereg tier-A block and
  amendments 1–9 headers; `apm-qualification-v1.edn` header.
- Read in full: `TN-opus-f47-observation`, `TN-opus-f48-critical-findings`,
  `TN-apm-defect-register`, `TN-same-problem-holdout`; `TN-opus-F41-analysis`
  and `TN-refuted-statement-disposition` in part.
- Subagent trace of the nine recommendations (48 tool calls); I re-read the
  `2d9e08a8` diff and `e2acba41` stat myself.

Not checked: Lean recompilation of any attempt (I relied on archived
source + receipt `:axioms`/`:compile` fields); live transcripts; the
qualification gate; the Agency job records (aged out — `model` is not
recoverable from them, so per-frame seat models before the `seat-cast.edn`
pin are inferred from `frame_seats.clj` history, not observed); the ~390
commits in the window beyond those named here.

## Addendum, 2026-08-30 ~12:50Z — Codex's follow-through, checked before f60's first learning phase

Joe asked Codex (codex-17) to apply the §7 recommendations and restarted the
campaign; f60 (b00J02) was minted 12:11Z and is at `:solve` as I write. The
window is 21 commits, `8aa26d7c..5c3faacf`. In between: f58 (aunk04) ran to
guide-1 and was the intended stop boundary — its a1 solved with one memory,
`e-63b7c7c1` (f34-guide, a95J03), on the shelf, verdict `already-in-base`,
which Opus recorded as the strongest negative so far; f59 (b00J01) was minted
past that boundary, voided `:apparatus-invalidated`, and frozen as a fixture
(`f59-post-f58-boundary-fixture.edn`, explicitly not experimental evidence).

### Recommendation by recommendation

| § 7 | what landed | coherent with the intent? |
|---|---|---|
| 1 f42/a1 in prereg | amendment 10: frame record with the three caveats verbatim, and a two-count rule (artifact-letter count includes it; route-changing count does not) | yes |
| 2 Opus tally | f53 sentence rewritten to the fingerprint form; f47/a3 and f50/a3 rows added; new "Instrument boundary at f53" section | yes |
| 3 fingerprint at close | `frame-fingerprint-audit/audit!` runs from `countdown-control/record-close-observations!` after the close is durable, publishes `<campaign>/analysis/fingerprint-audit.json` + a status EDN atomically; analyzer failure is recorded, never retracts the close. The frame-close receipt now carries a controller-derived `:receipt/memory-use-audit` (per memory id, attempt ordinals) that the close-frame role must echo; contract v4 regenerated | yes — and it also discharges rec. 8 (the audit is id-agnostic; test covers an opaque id) |
| 4 process-memory verdict | `not-adjudicable-by-token` added to the script, gated on a `:memory-use/kind` recorded on the attempt receipt; never inferred from prose | **half — see below** |
| 5 f53 guide boundary | amendment 10 `:casting-boundary` + Opus section | yes |
| 6 f33/f35 re-validation | `revalidation-f33-f35-pattern-accounting-20260830.edn`: 8/8 retrospectively approved by re-reading the proctors' preserved reasons and replaying `2d9e08a8`; receipts and substrate untouched | yes, as an evidential record; they stay unpublished |
| 7 population A | the six proof-text ids were already rejected on 08-25 (`codex-10`, `:proof-text-not-memory`); verified `candidate-visible?` false; I confirmed none of the six is in f53's or f58's accessible list; new gate `:proof-text-candidate-publishing-forbidden`. The note says plainly this closes the slice, not population A | yes for the slice |
| 8 non-UUID close count | subsumed by 3 | yes |
| 9 `:claimed-defect` parks the frame | nothing in the window touches it | **still open** |

### What I verified

clj-kondo on every changed `src/`/`test/` file: 0 errors, 0 warnings.
`clojure -M:test` on the seven changed/new namespaces: 125 tests, 551
assertions, 0 failures. `python3 -m unittest …/test_fingerprint_audit.py`:
11 OK. Lean emitter parity: `cmp <(lake env lean --run
DarkTower/APMCycleContractEmitter.lean) generated/apm-cycle-contract-v4.json`
→ identical (apm-lean `9d2a1db5`). Serving JVM (:6768) resolves the new
vars — `live-learning-phases/memory-use-audit`,
`countdown-control/record-close-observations!`,
`promotion-pipeline/typed-memory-use-candidate?`,
`memory-snapshot/stratify-candidates` — so f60 runs the code on disk (A11
not repeated). Re-ran the audit on v2: 33 use events after excluding the four
void frames (f48, f49, f51, f59), f58/a1 `already-in-base`,
`transfer-stratum` and `delivery-route` columns present.

### Two things f60 is exposed to

**(a) Two new hard requirements on model output, neither in a pinned role
card.** The promotion proctor must now put `:memory-use/kind` (`:substitutive`
| `:regulative`) on every `:approve`/`:reassign` of a candidate minted under
`:admission/schema :typed-memory-use-v1` — which is every candidate from
now on. The instruction lives only in the dispatch-prompt suffix
(`live_promotion.clj:313-368`); `promotion-proctor-v3.md` does not mention
it. Omission fails `validate-certified-promotion-pass` with
`:review-memory-use-kind-invalid`, and that pass is held
`:awaiting-apparatus-repair` — a park, not a halt, and not data loss. The
close-frame role must echo the controller's `:memory-use-audit` exactly; the
`--init` template renders the null and the request in the packet carries the
value, but the repair text for `:close-evidence-invalid` still says only
"trace id and result" (the specific keyword rides in `:finding/details`).
This is the shape that parked f39 (scribe asked for `:hook`/`:body` its card
never named) and f54 (guide asked for `:content-digest`). Cards pin by blob,
so do not edit mid-f60; a `promotion-proctor-v4` and one guide-card line
before f61 is the cheap fix.

**(b) The kind classification does not reach the verdict.** Three places now
speak about memory kind and none joins to the APM Student path:

- the proctor's `:memory-use/kind` is stored on the review edge
  (`promotion_review_store.clj`) and projected by `peripheral/memory_recall`;
- the audit script reads `:memory-use/kind(s)` from the *attempt receipt*
  (`recorded_use_kinds`), and only `dispatch_with_recall` writes that field —
  APM Student receipts carry none (f58's has none);
- `memory-snapshot/stratify-candidates` reads `:memory/kind` in a third
  vocabulary, `#{:substitutive-content :regulative/process}`, which nothing
  writes; its default is `:observed-only`, so no behaviour changes, but
  `:kind-counts` will read `{:unknown N}` indefinitely.

Net effect on campaign data: every row is `memory-use-kind: unknown` (33/33
in today's rerun), `not-adjudicable-by-token` cannot fire, and the two
regulative rows in §2 still read `unwitnessed`. Not wrong — the script
refuses to guess, which is the right default — but the field the proctor is
now required to produce is the one the audit never looks at. One join closes
it: `memory_metadata` reading `:memory-use/kind` off the evidence/review
edge, or the controller copying it into the Student receipt's accessible
list at dispatch.

**(c) No conditions-registry entry.** v2 `conditions.edn` still ends at
C-10, so f60's manifest does not record that the close-frame audit, the
typed-admission gate and the audit-echo contract are live from f60.
Amendment 10 records analysis and the f53 boundary; the instrument change
itself needs a C-11 with `:since-frame "f60"`.

Overall: the changes do what §7 asked, with the tests and parity to show it.
The gaps are one unfinished join (b), one operational hazard that will show
up as a park rather than a bad measurement (a), and one missing registry
line (c).
