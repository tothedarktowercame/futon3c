# TN: F32–F35 bank review — what these frames bank for the v3 memory whitepaper and the Codex→Zai transfer experiment

Author: Claude (Fable 5), 2026-08-25 ~18:00Z, at Joe's request ("make sure
we're actually banking things relevant to the v3 memory whitepaper and the
broader transfer-memories-to-Zai experiment"). Campaign
`jit-all-open-nontopology-v1`, frames f32 (a92J01), f33 (a94A07), f34
(a95J03) certified; f35 (a95J04) at `student-attempt-1`. Read against
`docs/retrieval-whitepaper-v3.md` (draft 2026-08-11),
`E-memory-whitepaper-v3-programme.md`, `prereg-capability-transfer-v1.edn`,
`PROPOSAL-memory-access-experiment-20260824.md`, and the role cards
`codex-scribe-v1`, `zai-scribe-v1`, `promotion-proctor-v3`. §7 lists what I
actually ran.

## Short answer

1. **The silence chapter is being fed well.** f32–f35 add at least six
   instances of the whitepaper's §2.2 mechanism (a record whose
   well-formedness is independent of the reality it reports), several of
   them inside instruments built after the 08-11 catalogue. Listed in §3.
2. **One genuine positive result is banked, and the frame's own instruments
   read it as zero.** f33 `student-attempt-1` closed a94A07 (exit 0, 0
   sorries, standard axioms, independently recompiled) using one memory from
   a *different* problem (a94J08, codex-5, 2026-07-31), found by open
   search, not on the shelf; the artifact carries two of the memory's three
   named APIs and its construction. The close-frame audit records this
   attempt as `:used 0` / "memory-free"; `memory_shape.py` files the memory
   under `used-with-unknown-origin`; the campaign reuse metric still reads
   **0 used on a problem ≠ mined-from**. §2.
3. **For the transfer experiment, nothing in f32–f35 can yet say memories
   *help*.** The ablation arm (`prereg` level-2b) does not exist; every
   "used" is a Student self-report checked only for set-membership, below
   the whitepaper's own §3.1 fingerprint standard; and 25 of the 26 distinct
   used memories were used only on the problem they were mined from. What
   is banked is level-2a ("used"), mostly of Guide memories written for that
   problem after the Guide read the Student's failure account. §4.
4. **The two new scribes have produced 31 candidates and 0 approvals**, for
   three apparatus reasons and one design contradiction; all 8 approved
   memories in f32–f34 came from the Guide. The scribes' pattern-library
   `.md` files are ingested by nothing (0 of 7 coined patterns exist in the
   substrate or `futon3/library`). §5.

Recommendations, ordered and small, in §6.

## 1. What the four frames produced

| frame | problem | Student attempts (outcome; `:used-ids`) | scribe candidates → verdicts | Guide candidates → verdicts | wall-clock |
|---|---|---|---|---|---|
| f32 | a92J01 | 1: partial, 0 used ("all 18 shelf memories concern a01J05/a01A12/a01J06"); 2: 2 used (guide); 3: **0 sorries**, 3 used (guide) | promote-solver: 2 reviewed (both pre-existing `codexpilot` memories) → 2 reject; scribe-reduce: 4 → 4 cannot-judge (never persisted) | 3 → 3 approve | 21:04 → 08:38 (11h34, incl. the 8 h overnight stall, `TN-fable-F32-process`) |
| f33 | a94A07 | 1: **0 sorries**, 1 used (`e-codexpilot-…Cauchy-derivative-estimates`, a94J08, via open search); 2: 0 sorries, 1 used (guide, unpromoted, via search); 3: 0 sorries, 2 used (guide) | promote-solver: 4 → 4 cannot-judge (404); scribe-reduce: 7 → 7 cannot-judge (404) | guide-1: 3 → 3 cannot-judge (keywordized pattern-accounting); guide-2: 1 → approve | 08:38 → 12:42 (4h04) |
| f34 | a95J03 | 1: partial, 0 used (self-corrected a "directional priming" claim after the controller flagged it); 2: failure, 2 used (guide), no Lean written; 3: no typed receipt, partial | promote-solver: 5 → 5 reject (3 pattern-attachment-mismatch, 2 already-in-file); scribe-reduce: 4 → 4 reject (no mathematical residual / not actionable) | 4 → 4 approve | 12:42 → 16:04 (3h22) |
| f35 | a95J04 | 1: running (27 accessible; 30-min cap now enforced, `turn-timeout-ms 1800000`) | promote-solver: 5 → 5 cannot-judge (`:pattern-ids []`; reasons read as approvals) | — | 16:28 → … |

Every frame's `promote-solver` also carries
`:receipt/prior-dropped [{:frame-id "f31" :finding :prior-snapshot-unreadable}]`
— f31's memories have been absent from every shelf for four frames. Listed
loudly (the countermeasure working), never repaired.

## 2. The positive result, and how three instruments missed it

**What happened.** f33 `student-attempt-1` (job `apm-role-…`, receipt
certified) abandoned the base file's BLOCKED route (essential-singularity
classification of `f(1/w)`), searched the corpus twice, and on the second
query — phrased in the vocabulary of its *new* route ("Liouville, bounded
entire, injective, constant contradiction") — surfaced
`e-codexpilot-force-a-sublinear-entire-function-constant-by-Cauchy-derivative-estimates`.
Substrate record: author `codex-5`, `2026-07-31T03:16Z`, subject
`M-codex-sorry-loop`, witness commit apm-lean `662b9ec` = "prove a94J08
entire lower bound impossible". Not on f33's shelf (22 accessible ids, all
UUID-form, all from a01J05/a01J06/a01A12/a92J01); reached only through the
open-search channel wired 2026-08-24 (`0ed27146`…`77a72e92`).

**Fingerprint check** (the whitepaper §3.1 standard, applied to
`live/student-attempt-1-source/4b4a3c24…-Main.lean`, 366 lines):

- `Complex.analyticAt_of_differentiable_on_punctured_nhds_of_continuousAt` —
  memory `:key-api[0]` — present twice (lines 122, 241), each filling a
  removable singularity of a quotient, which is the memory's construction
  ("removable quotients can manufacture the needed … entire function");
- `is_const_of_deriv_eq_zero` — `:key-api[2]` — present once;
- `Complex.norm_deriv_le_of_forall_mem_sphere_norm_le` — `:key-api[1]` —
  **absent**; the Student bounded the difference quotient by linear growth
  plus `IsCompact.exists_isMaxOn` and finished with Liouville (line 319)
  rather than the Cauchy estimate.

So: the artifact carries the memory's construction and two of its three
named APIs; the Student's own account names steps 3–4 as where it was used.
This meets the standard the whitepaper set with a94A09 — with the same
honest caveat as a94A09: the Student was already on the route when it
searched (the query was written in that route's vocabulary), so the memory
*confirmed and supplied API for* a route, it did not *select* it. n=1,
existence, not a rate.

**How it was read as zero, three ways:**

1. `close-frame.edn` (Guide audit): `:memory-use-by-attempt {:attempt-1
   {:outcome "proved", :used 0}}` and, in the result justification, "the
   first memory-free, the last two with memory". The typed receipt says
   `:memory-use {:used-ids ["e-codexpilot-…"]}`. The audit evidently counted
   only UUID-form / shelf ids.
2. `memory_shape.py` (re-run today, `scratchpad/memory-shape-2026-08-25.json`):
   `used-on-problem-!=-mined-from: {}`, and the memory sits in
   `used-with-unknown-origin` because its `:evidence/subject` is a mission
   ref, not a problem ref. Origin is recoverable (witness commit → a94J08).
3. The campaign-level reading Joe and the review notes carry ("reuse,
   measured properly, is zero", `E-early-memories-review` §finding 2) is
   therefore stale by one — and the one is the only instance so far that
   crossed a problem boundary.

Also worth banking: this use bypassed the entire promotion pipeline (no
scribe, no proctor, no shelf) — a pre-campaign memory reached a Student by
lexical search on obstacle vocabulary. That is §4.2 of the whitepaper ("users
find reusable work by engine names and structural similarity, never by
concept vocabulary") happening again, in the Student's own words: "vocabulary
of the obstacle as stated by the old comment was the wrong vocabulary".

## 3. Silence-catalogue candidates from f32–f35 (for whitepaper §2.1)

Numbered continuing from the draft's 13.

14. **The identifier the model was asked to echo** (F32,
    `TN-fable-F32-model`). Validator rejected a one-character UUID typo in an
    LLM-transcribed field that the controller already held; the field
    (`:surfaced-ids`) had no Lean counterpart because it lived inside a
    declared-open residual hole. Fixed `114ea24e`; hole closed per
    `TN-controller-owned-identifier-replay-audit`.
15. **The budget that existed nowhere** (F33, `TN-sonnet-F33-finding`). The
    "30-minute student attempt" everyone was reasoning about appeared in no
    document and no code; the card said 120, the harness enforced 60. Fixed
    `d21c3dcf` (f35 is the first frame under it).
16. **`cannot-judge` certifies.** `be9978b5` admitted `:cannot-judge` to
    `review-verdicts` so a review that cannot fetch its candidates no longer
    blocks the frame. f33 then ran three reviews with **11 of 11**
    candidates `cannot-judge` (404 on persisted evidence; keywordized
    pattern-accounting) and certified `:result "closed"` with a well-formed
    receipt. The frame's own close audit (findings F1, F2, F4) recorded it;
    the receipt shape did not. Distinctive twist: two of the f33 guide-1
    memories were `cannot-judge` in the review yet `:reviewed` on the
    substrate edge, and the Student used one of them in attempt 2 via search
    (close-frame F3) — the shelf invariant "no memory used that wasn't on the
    reviewed shelf" has been false since open search landed, and nothing in
    the receipt says so.
17. **The audit that miscounted the only interesting number** (§2 above):
    the f33 close-frame's `:used 0` for the one cross-problem use.
18. **"The watcher ingests this file."** Both scribe pattern-library files
    (`pattern-library-zai-scribe-f34-a95J03.md`,
    `pattern-library-codex-scribe-f35-a95J04.md`) open with this sentence.
    No code in futon3c, futon1b, or futon3 reads them
    (`futon3/scripts/pattern_pull.clj` goes substrate → filesystem, not
    the reverse); 0 of the 7 coined pattern ids exist in the substrate
    (`entities?type=pattern/library`) or in `futon3/library`. The f34
    scribe-reduce disposition admits the consequence: "candidates reuse
    discovered canonical ids because new-id rationales cannot pass the
    pattern-accounting gate" — i.e. the deposits were attached to patterns
    that do not fit, and the proctor rejected them for exactly that.
19. **The reuse metric that reads 0 at n=1** (§2, instrument 2): an
    origin-resolution rule that only recognises problem-ref subjects.

Countermeasures that *worked* this round, worth saying in §2.2: the
`prior-dropped` listing (f31, loud for four frames); the mechanical guard;
the controller's `used ⊆ surfaced` check, which on f34 attempt-1 made the
Student retract a "used" claim it had made on "snapshot access plus
directional priming" — the witness standard biting at the tool boundary.

## 4. Mapping onto the whitepaper's claims and the prereg

**§1.2 / §6 (supply vs demand).** The 0/31 scribe vs 8/11 Guide split is the
draft's demand-side thesis reappearing in the new design: the Guide reads the
Student's failure account (what is missing, in the asker's vocabulary) and
writes one memory against one residual, and those are the memories that get
approved and used. Two qualifications the paper must carry: (a) these are
same-problem memories — 25 of 26 distinct used memories were used only on the
problem they were mined from (today's reading); by `codex-scribe-v1`'s own
sentence, "a store that only helps on the problem it was mined from is a
cache"; (b) on f33 the Guide memories were written *after* attempt 1 had
already closed the problem without them (attempts 2 and 3 then "closed with
memory") — the guide-after-success question in the 08-24 PROPOSAL now has a
second instance (f30 was the first).

**§3 (witness standard).** The frames run below the draft's own standard.
`:used-ids` is a self-report validated for membership in the
controller-derived surfaced set (correct, post-`114ea24e`), but nothing
checks the artifact. The close-frame Guide already recompiles every attempt
and has the source blobs; adding "grep each used memory's `:key-api` /
named lemmas in the closing source, record hits per id" is a small change
and is what would let §3 report an n instead of two existence proofs.

**§4 (retrieval characterized by users).** f32 attempt-1 and f33 attempt-1
both report the shelf as entirely off-topic (18/18 and 21/21 memories from
other problems) and both then searched; f33's search worked on the second,
obstacle-vocabulary query. The Students' `:queries` (now controller-derived
from FTS receipts) are the demand-side corpus the programme's "post-batch
interviews" wanted; they are being banked automatically.

**§5 / P1–P7 and `prereg-capability-transfer-v1`.** The draft's §5 and the
P-series belong to the batch ladder (closed 08-11). f28+ is a different
design with its own prereg, and the draft has no section for it. As of
today the prereg's level-2b is `:not-yet-run` (no ablation arm), its
`:TBD/operator` fields are unfilled, and the level-2a "first observed at
F29" that grounds it was the proof-text paste. f32–f34 give better
level-2a instances (named-lemma memories, used, problem closed), still
same-problem. Two instrument-version boundaries the prereg's own
per-frame rule requires recording: open search wired 08-24 (so every attempt
since f31 runs the PROPOSAL's "both" condition, unrandomized — the ordinal
confound Joe flagged is live in the data) and the 30-minute cap at f35.
The six f29/f30 proof-text memories are still on every shelf (all six ids
are in f35's `:accessible-memory-ids`); `E-early-memories-review`'s "run
population A before F32's promote-solver" did not happen.

## 5. The scribes

**Codex scribe (`:promote-solver`).** f33: 4/4 `cannot-judge`, persisted
evidence 404 — the deposits never landed (close-frame F1: "solver snapshot
carries zero problem content"). f34: 5 rejects — 3
`pattern-attachment-mismatch` (dispatched pattern set ≠ persisted edge), 2
`already-in-file`. f35: 5/5 `cannot-judge` with `:pattern-ids []`, but every
reason string is a positive residual-fit + generality answer ("actionable,
findable, coherent with pointwise-hassum-to-taylor-…") — the content passed
the proctor's test and the attachment could not be resolved because the
parent pattern does not exist anywhere (§3 item 18). f35's content is
exactly the shape the card asks for; the block is mechanical.

**Zai scribe (`:scribe-reduce`).** f32: 4/4 never persisted. f33: 7/7 404.
f34: 4 rejects, all "no mathematical residual / not actionable": the
deposits are process rules (route-map-before-reconstruction,
memory-consumption-discipline, submit-in-turn, plus one stale absence
claim). That is what `zai-scribe-v1` asks for (arc/trajectory rewrite rules
and process memories) and what `promotion-proctor-v3` rule 3 (residual fit)
rejects; rule 6 (the six-field arc-rule schema, witnessed) is in the card and
was not the test applied. Until one card yields, every Zai-scribe frame is a
guaranteed 0. My read: process rules are for the *next Student's conduct*,
not for a `sorry`; they belong in the Student role card or a process shelf
reviewed by rule 6, not on the math shelf reviewed by rule 3.

**One bug, three victims.** "JSON keywordizes rationale keys but the
validator performs string lookup" voided f33 guide-1 (3 memories), forced
the Zai scribe to attach to misfit canonical ids (f34), and is why coined
patterns cannot be introduced through the deposit path at all — which is
what the un-ingested `.md` side files were an attempt to route around.

## 6. Recommendations (ordered; each is one file / one behaviour)

1. **Fix the keywordized pattern-accounting lookup**, then re-validate f33
   guide-1 and f35 promote-solver under the corrected validator without
   redispatch (the F32-model §3.3 path). Expected: 3 + 5 approvals from
   candidates that already passed the content test.
2. **Give coined patterns a real path**: either a script that turns a
   `pattern-library-*.md` into `futon3/library` flexiargs + substrate
   entities before the review runs, or delete the "watcher ingests this
   file" line and have the scribe report `ran-empty: no parent pattern`.
3. **Fingerprint at close-frame**: for each `:used-ids` entry, grep the
   attempt's closing source for the memory's `:key-api` / named lemmas;
   record hits. Retro-apply to f29–f34 by hand once (the blobs are
   archived). This is the whitepaper §3 n.
4. **Fix the two counters**: `memory_shape.py` origin resolution for
   mission-subject memories (witness commit → problem); close-frame's
   `:used` count for non-UUID ids. Re-read: reuse = 1, not 0.
5. **Decide the Zai-scribe review test** (rule 6, or a process shelf)
   before f36's `scribe-reduce`.
6. **Run `E-early-memories-review` population A** — the six proof-text
   memories are on f35's shelf now.
7. **Repair f31's snapshot** or retire it explicitly; four frames of
   `prior-dropped` is a loud signal nobody has answered.
8. **Prereg hygiene**: record open-search (08-24) and the 30-min cap (f35)
   as instrument-version boundaries; fill the `:TBD/operator` fields; state
   in the whitepaper that f28+ measures *used*, not *helped*, until the
   ablation arm exists.
9. **Whitepaper structure**: a new section for the f28+ transfer design
   (prereg-capability-transfer-v1), separate from §5's batch ladder; §2.1
   gains items 14–19; §3 gains f33 attempt-1 as the second fingerprinted
   chain and the first cross-problem one.

## 7. What I checked

- Read in full: `docs/retrieval-whitepaper-v3.md`,
  `E-memory-whitepaper-v3-programme.md`,
  `PROPOSAL-memory-access-experiment-20260824.md`,
  `prereg-capability-transfer-v1.edn`, `E-early-memories-review.md`,
  `TN-fable-F32-model.md`, `TN-fable-F32-process.md`,
  `TN-sonnet-F33-finding.md`, `TN-controller-owned-identifier-replay-audit.md`,
  role cards `codex-scribe-v1`, `zai-scribe-v1`, `scribe-v3`,
  `zai-student-v2`, `promotion-proctor-v3`, both scribe pattern-library files.
- Frame records f32–f35: `live/promote-solver.edn`, `live/scribe-reduce.edn`,
  `live/guide-intervention-{1,2}-review.edn`, `live/close-frame.edn`,
  `live/student-attempt-{1,2,3}.edn` (accessible / surfaced / used ids,
  queries, failure accounts), `projection/latest.edn`; ledger timestamps for
  wall-clock.
- Artifact: `f33/live/student-attempt-1-source/4b4a3c24…-Main.lean` grepped
  for the memory's three `:key-api` names and `sorry`.
- Substrate (`:7073`): `GET /api/alpha/evidence/e-codexpilot-force-…`
  (author, date, subject, key-api, witness commit);
  `GET /api/alpha/entities?type=pattern/library&limit=5000` grepped for the
  seven coined pattern ids (0 hits). apm-lean `git show --stat 662b9ec` →
  `problems/a94J08`.
- Re-ran `analysis/memory_shape.py --json` (reading written to the session
  scratchpad, not the lab dir): 960 memories, 628 edges, 26 distinct used,
  0 cross-problem by its rule, 1 unknown-origin.
- Git: `be9978b5` diff (one-line `review-verdicts` change);
  `role_memory_search.clj` history (five commits 2026-08-24); `d21c3dcf`
  stat; `ce6a2677`, `5f92badf`, `114ea24e` stats.
- Grepped futon3c `src/` + `scripts/`, futon1b `src/` + `scripts/`, futon3
  `scripts/` + `dev/` + `src/` for `pattern-library` readers.

Not checked: the f35 Student's live transcript beyond the substrate's
turn-round entries (rounds 113–114, "stopping per the reserve"); whether the
f30 validator widening (`bbc04b0b`/`eb9c5846`/`77a72e92`) is sound — still
unreviewed per `TN-fable-F32-model` §5.
