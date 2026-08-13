# E-compression-conductor-runbook — running the compression pass

**For the conductor seat (candidate: claude-4 if Opus, or a codex
agent — Joe + Fable assign). Written 2026-08-12 by claude-1 (Fable)
after conducting the case-1 pilot by hand; supervisor: claude-1. Read
with M-case-studies §compression pass + §worse-is-better lane +
§retrospective mining, and futon3/README-pattern-mining.md (the method
this instantiates — especially "moves not topics", the honest-HOWEVER
test, add-only-gaps, and §5's distribution-inheritance warning).**

## The loop (per closed case, or per mining slice)

**Stage 1 — detection (zai-1, the student).**
- One job per transcript, INDEPENDENT (no job sees another's marks).
- Rubric: `apm-evidence/case-1-corpus/rubric-stage1.txt` — surprisal
  marks, 3-6 per transcript, each with QUOTE / WHY-SURPRISING / MOVE
  (verb phrase, move not topic) / GENERALIZES-TO (situation-class,
  problem-free).
- Dispatch `--from compression-pilot` (bellbacks route to no agent);
  collect results PASSIVELY from the jobs ledger. NEVER send a bell
  addressed to claude-1 from inside this workflow (parallel-incarnation
  hazard; see memory + instance 6).
- Pilot calibration: 9 transcripts → 41 marks, 0 nothing-surprised.

**Stage 2 — synthesis + authoring (conductor drafts, review gate).**
1. Tally MOVE phrases; cluster SEMANTICALLY (literal duplicates never
   happen). A cluster is a candidate when marked from ≥3 transcripts
   or ≥2 transcripts + corroborated by the runner's own debrief
   (cross-validation is the strongest signal the pilot found).
2. DEDUPE against the whole flexiarg library (all families, not just
   math-*): grep keywords + read the nearest neighbors. Add only gaps.
3. Author survivors as flexiargs (template: the case-1 five in
   `futon3c/data/pattern-staging/case-1/`). Requirements: verb-phrase
   name (never the object manipulated — hotword trap), honest HOWEVER
   (cannot state what goes wrong = slogan = reject), @provenance line
   with mark counts, grade recorded (principle/technique/snippet).
4. **Review gate FIRST.** claude-4 (reviewer seat since the 2026-08-12
   captain handover; conductor ≠ reviewer) reviews every candidate's
   formulation. Verdicts: APPROVE / REVISE (with exact text) / REJECT
   (with the criterion failed). Rejected candidates are renamed
   `*.flexiarg.REJECTED` with a TOMBSTONE file; they are never globbed
   into a registration pass.
5. **Registration comes AFTER review AND after the stage-3 assay** —
   both, not either. Only then do hotwords go into futon3's
   patterns-index.tsv.

   *Amended 2026-08-12 by claude-2 (captain), defect B2 from the gate's
   first firing.* The original order registered at step 4 and reviewed at
   step 5, so all five case-1 candidates were live in the library before
   anyone read them — and one of them (`search-the-namespace-not-the-
   qualified-name`) was subsequently rejected and had to be retracted from
   a live index. Registering before the gate makes the gate advisory.

   CAUTION on mirroring: mirror every authored file into
   `futon3c/data/pattern-staging/<case>/` (versioned in futon3c) — this
   remains right, but **the reason previously given was wrong.** Zone's
   futon3 IS a git repo (HEAD `a4579f2`, remote
   `github.com:tothedarktowercame/futon3`) and `library/` IS tracked
   (1150 files). What was true is narrower: newly authored patterns were
   left UNCOMMITTED, so they had no history — the fix is to commit them,
   not to rely on the mirror. Verified 2026-08-12; the "futon3 is not
   under version control" claim in earlier docs is retired.

   Second caution, same discovery: `resources/sigils/patterns-index.tsv`
   was a SYMLINK into `/home/joe/code/storage/futon3/...`, and the case-1
   registration silently replaced it with a regular file. All readers
   resolve the path relative to the futon3 repo root, so they do see the
   new rows — but the `storage/` copy is now an orphaned 1355-row stale
   twin, and a well-meaning "restore the symlink" would silently drop
   every pattern authored since 2026-08-10. Do not restore it without
   reconciling both copies.

**Stage 3 — teachability assay (zai-1, the student again).**
- Pick a SOLVED goal the candidate should help with (ground truth
  known). Text-only design: "produce a proof plan naming specific
  APIs", control vs treatment (same packet + candidate flexiarg text).
- Score against ground truth: does the treatment plan reach the known
  route (names the load-bearing APIs / the transport / the technique)
  where control does not? Conductor scores; Fable spot-checks.
- HARD RULE: no store deposit without an assay pass. Failed candidates
  stay in pattern-staging with the assay result recorded — they are
  research notes, not memories. (The store's disease was unvalidated
  supply; the assay is the immune system.)

## Hard rules

- Independence in stage 1 is absolute — convergence is the signal and
  it only means something if the reads were blind.
- Every count reported = what you CHECKED (jobs ledger ids, grep
  output), not what you remember.
- Empty results are facts about your query first (namespace-grep
  lesson — now itself a pattern: search-the-namespace-not-the-
  qualified-name).
- Traffic discipline: ONE report per case/slice to the supervisor,
  plus true blockers only.
- Escalate: any candidate contradicting an existing library pattern
  (that's a library finding, not a new pattern); any assay design
  where ground truth is ambiguous; anything touching the store's
  deposit side.

## Mining-slice variant (retrospective lane)

Same loop over historical evidence slices (apm-evidence manifest),
with stage-1 vote threshold raised: pattern counts when independently
surfaced from ≥N DISTINCT PROBLEMS (not transcripts). Before a slice:
run the recogniser over the slice's prose and collect the no-candidate
passages — that miss-list is the worklist (README-pattern-mining §6),
and it must be measured on prose, not extracted steps (§5 bias).

## Assay design lessons (case-1 pilot, 2026-08-12 — learned by failing)

- **Headroom first:** assay on the step that STALLED in the transcripts
  (where the candidate would have been load-bearing), never on a step
  the runners handled — a goal the student solves unaided measures
  nothing. Pilot instance: the bridge lemma was route-findable by
  zai-1 cold; the right target was the instance-diamond assembly.
- **Arms are sessions, not prompts:** one seat carries one session
  across its queue (D4). Control-then-treatment on the same seat lets
  the treatment see the control ("unchanged from the first assay").
  Verified session reset between arms, or distinct seats.
- **Score non-application too:** a treatment that correctly declines
  to use an inapplicable memory, naming where it WOULD apply, is
  positive evidence about deposit scoping — record it, separately from
  the teachability verdict.

## Slice-1 lesson: the novelty bias (2026-08-12)

Surprisal selects for OUT-OF-DISTRIBUTION content, not for the content
you care about. Slice 1 ran on an 81%-math corpus and returned
coordination patterns (verdicts, gates, evidence rows) — our homegrown
agency machinery is more novel to the student than Mathlib proofs, so
the surprise gauge locked onto the exotic minority stratum. (The
distribution-inheritance warning, §5 of the mining README, in a new
guise: the instrument inherits the reader's training distribution.)
COUNTERMEASURES, both mandatory for math mining: (1) rubric v2
(apm-evidence/case-1-corpus/rubric-stage1-math-v2.txt) pins the
measurement DOMAIN — in-scope math/formalization process, out-of-scope
coordination, COORDINATION-ONLY as a valid response; (2) packets are
filtered to the math stratum via prose/index-strata.tsv (stratum
column: math | coord). The coord stratum is separately minable for
AGENCY-family patterns — slice 1 demonstrated that by independently
re-deriving two silence-catalogue countermeasures from the logs
(probe-the-claimed-property-not-the-acceptance-proxy,
separate-evidence-history-from-verdict-state) — route those through
claude-2 review for the agency family, never math.

## Quality gates addendum (2026-08-12, quality/throughput review)

- **Quote verification (mandatory, stage 2 step 0):** before clustering,
  mechanically verify each mark's QUOTE appears verbatim (or
  near-verbatim, whitespace-tolerant) in its packet's source chunks.
  Marks with unverifiable quotes are FABRICATIONS: excluded from
  clustering, counted in the report, and a fabrication rate >5% fails
  the slice. Nothing in the pipeline checked this before.
- **Inter-slice convergence:** from slice 3 on, report which clusters
  re-surface from DIFFERENT problems than any prior slice — the
  strongest promotion signal; track in a running table at
  data/pattern-staging/cluster-ledger.tsv.
- **Throughput scaling is gated on quality:** more zai worker seats /
  a second conductor only after two consecutive slices clear the
  claude-2 review gate with <20% candidate rejection.

## Stage 4 — the tide test: discoverability against elicited demand
## (Joe, 2026-08-12; first run after slice-3 review)

The assay proves teach-when-given; nothing above proves FIND-when-
needed, and retrieval is where both batch nulls lived. A library that
teaches but never surfaces is a sandcastle. So, periodically (every
~3 slices):

1. **Panel**: sample K=5-8 UNSOLVED problems — the actual future
   consumers, not the solved past.
2. **Elicit demand**: one cheap job per problem (codex or zai, no
   solving): "sketch your route in 5 lines; then list the memories,
   artifacts, or techniques you would WISH for, phrased as the search
   queries you would actually run." Wishes in the runner's natural
   vocabulary — that vocabulary IS the test.
3. **Match through the REAL retrieval paths** (not by hand): each
   wish-query goes through (a) the store recall ladder
   (dispatch_with_recall offline replay) and (b) the pattern index
   Tier-0 hotword retriever. Score each wish: DIRECT HIT (a
   deposited/staged pattern surfaces) / NEAR-MISS (the pattern exists
   but the query vocabulary cannot reach it — a hotword/anchor defect,
   repairable) / TRUE GAP (nothing exists).
4. **Actions**: near-misses → hotword/registration repairs (cheap,
   high-value — discoverability is mostly vocabulary); true gaps →
   the DEMAND-DRIVEN priority list for the next slices (mine where
   the tide already wants something); headline metric = DEMAND-MATCH
   RATE, tracked per tide test. Rising match rate = the library is
   real; flat = sandcastles.

The full re-prove loop (does a live solve fingerprint-USE a mined
pattern?) comes free when case-solving resumes — the case loop with
recall IS that test, and the longitudinal metrics capture it. The
tide test is its cheap leading indicator in the meantime.

## Scaling policy + per-slice retrievability check (Joe, 2026-08-12)

**Scaling decision rule:** slice 2 clears claude-2 review clean → scale
immediately (parallel zai worker seats + second conductor). Upgrades
needed → apply them, run ONE more slice, then scale. Never run long
while unconsumed quality signals sit in the queue — reviews and assays
are consumed BEFORE the next scale step, not banked.

**Per-slice retrievability sanity check (stage 2 step 6, mandatory):**
for each authored candidate, form 2 natural queries from the
PROVENANCE MARKS' vocabulary (the wishes/phrasings of the runners and
readers — never the pattern's own name or hotwords), and verify the
candidate surfaces in the Tier-0 retriever top-5. A candidate that its
own provenance vocabulary cannot find is NOT FINDABLE — repair
hotwords before it counts. This is the cheap every-slice version of
the tide test: memories must be reachable without convoluted
reasoning, or transfer to live use cannot be trusted.

**Unit-of-work note (Joe):** mining is the case where BATCHES genuinely
beat one-at-a-time — patterns are cross-instance objects, so the unit
of work must match the unit of signal. Solving stays per-case (closure
is a per-problem object); mining stays batched (a pattern cannot even
be OBSERVED in a single instance). Same campaign, two correct shapes.

## Slice-2 root cause CORRECTION + context-rotation protocol (2026-08-12)

The conductor's packet-size diagnosis was WRONG (and so was the
supervisor's first session theory). Root cause: the zai invoke-fn
holds the full conversation in an unbounded closure atom — after ~110
jobs the accumulated context exceeded the model window and EVERY new
prompt failed 1261, even 6KB probes. Fixed in src (07e944d7): session
rotation now truncates the conversation to the system message.

CONSEQUENCES:
- **Slices 1-2 independence caveat:** all reads shared one accumulating
  context — later reads had earlier packets AND marks in view. Slice-1
  clusters carry an anchoring caveat; convergence there is weaker
  evidence than the ledger suggests. Slice 3+ is the clean run.
- **Rotation protocol (conductor self-serve, no Drawbridge):** every 20
  reads, POST /api/alpha/agents/restore with {"agent-id":"zai-1",
  "type":"zai","session-id":"zai-<fresh-uuid>"} — verified to truncate
  the context (24KB probe OK immediately after). Rotate BETWEEN
  problems, never mid-problem.
- **Packet budget:** ≤2 chunks (~18KB) per read is comfortably inside
  the fresh-context window (24KB probe passed); the budget constraint
  is cumulative, not per-packet.
- Debugging pearl: Drawbridge surfaces runtime asserts as "Syntax error
  macroexpanding" — wrap the call in try/catch and read .getMessage to
  get the real error ("ZAI/ZAIF requires a durable evidence store").

## Job-cap rule (2026-08-12, slice-3 overrun): one stage, one job

Slice 3 died of Agency job-cap OVERRUN between stage 1 and synthesis —
silently, as cap kills always are. The 93 persisted marks survived
because persist-as-you-go is mandatory; the synthesis did not. RULE:
a slice is TWO dispatches — a stage-1 job (reads, persist each mark as
it lands) and a separate stage-2 job (verify/cluster/author from disk).
Never let synthesis share a job with a hundred reads. Recovery from
any cap kill = re-dispatch the stage that died, from the persisted
artifacts of the stage that finished.

## Two-generations lesson (2026-08-12, slice-3 reconciliation)

Slice 3 produced TWO contradicting honest reports: the original job
(recovered from its overrun scare, redid reads as a "final clean run"
into marks-final/, 312 marks, commit b953b42d) and a supervisor-
dispatched recovery job scoped to marks/ — the graveyard of the
CANCELLED preliminary attempts — which correctly found nothing
admissible there and rolled back the authored candidate (d1a607a3,
reverted). Neither agent erred; the supervisor's evidence-base scoping
did. RULES: (1) before dispatching recovery for a "dead" job,
re-verify its terminal state — overrun jobs can recover, and a
recovery racing its recoveree makes two writers; (2) a recovery packet
must scope evidence by CONTENT (the attested final artifacts named in
the surviving report), never by the supervisor's memory of directory
layout; (3) when generations of an artifact exist, the conductor names
the operative one in its report and stale generations get a
TOMBSTONE file saying which run superseded them.

## Slice-3 gate ruling (2026-08-12): FAIL at 5.77% vs 5.0% — by the letter

Re-verification (0816a2c2): of 128 excluded quotes, 107 were the
VERIFIER's false negatives (naive matching; unicode/wrapping), 2
sibling drift, 1 true cross-problem bleed (reassigned; no cluster lost
threshold), 18 true fabrications → 5.77%. The threshold is 5.0%, set
before calibration data existed; it stays where it was set, because
moving a gate after reading the number is the disease this whole
apparatus treats. Slice 3 therefore FAILS the fabrication gate
marginally. Per the scaling decision rule: upgrades + ONE more slice,
then scale on its pass. UPGRADES (mandatory from slice 4):
- Rubric v3: quotes must be COPY-PASTED EXACTLY; when exact quoting is
  not possible, write PARAPHRASE: instead of QUOTE: — an honest lane
  for compression removes the incentive to decorate a paraphrase with
  quotation marks. PARAPHRASE marks count for clustering but are
  flagged, and their rate is reported.
- The tolerant matcher (NFKC/case/whitespace/markdown/punctuation +
  ordered ellipsis fragments) is the STANDARD verifier from now on;
  the naive matcher is retired — its 84% false-negative rate would
  poison every future gate reading.

## Known needed cleanups (Joe, 2026-08-12)

Janitorial tasks. **Anyone can pick one up at any time, no dispatch and no
permission needed.** They are small, unambiguous, and independent of the
slice loop. Tick an item off in place (strike it and date it) rather than
deleting it, so the list stays an honest record of what was left lying
around. Items that need a DECISION rather than labour are marked
**[decision: Joe]** and should not be "tidied" by whoever finds them.

1. **TWO real copies of `patterns-index.tsv`, not three.** [decision: Joe]
   The live one is `futon3/resources/sigils/patterns-index.tsv` (1359 rows
   — every reader resolves this path from the repo root). It *was* a
   symlink into `/home/joe/code/storage/futon3/resources/sigils/
   patterns-index.tsv` until the case-1 registration silently replaced it
   with a regular file; that storage copy is now an orphaned stale twin at
   1355 rows. **DO NOT "restore the symlink" as a tidy-up** — it would
   silently drop every pattern authored since 2026-08-10.
   *Corrected 2026-08-13 (codex-4, verified by claude-2):*
   `/home/joe/code/data/notions/patterns-index.tsv` is **a symlink to the
   live file**, not a third copy — it is a working compatibility link and
   should be retained. The original wording here asserted three copies
   without checking the file types.
   PROPOSAL ON THE TABLE (codex-4, awaiting Joe): make the futon3 path
   canonical; keep the data/notions symlink; archive and retire the stale
   storage copy after explicit reconciliation; require writers to resolve
   the canonical real path before atomic replacement; add a preflight
   check for row count/hash plus forbidden divergent regular copies.

2. **86 uncommitted entries in `futon3/library/`.** Sitting in the working
   tree with no history; one `git checkout` away from gone (instance 13's
   lesson, still live). Not mine and not the mining lane's — whoever
   authored them should commit or discard.
   *Corrected 2026-08-13 (codex-4, verified by claude-2):* the original
   wording named `baldwin/`, `agency/`, `aif/`, `career-coherence/`,
   `forward-model/` — which are among the SMALLEST contributors. The
   actual spread is 14 directories: futon-theory 20, baldwin 10,
   structure 8, repository-transition 8, mmca 8, war-room 7,
   forward-model 7, ukrns 6, memory 5, math-strategy 3, then p4ng,
   career-coherence, aif, agency at 1 each. The original list was
   generalised from the first 20 lines of an alphabetically sorted
   `git status` — the same extraction failure as trusting an anchored
   grep: assert you have seen the whole list before summarising it.
   PROPOSAL ON THE TABLE (codex-4, awaiting Joe): treat discovered library
   dirt as owned work, never anonymous cleanup — record owner/session and
   age immediately, move changes into an owner-specific worktree or a
   provenance-labelled WIP commit, and require the owner or a designated
   steward to choose merge vs explicit discard after review; prevent
   recurrence with clean-tree lane preflights and a scheduled dirty-tree
   report.

3. **`futon3` git identity was unset** and is now `Joseph Corneli
   <jcorneli@brookes.ac.uk>` (repo-local, set 2026-08-12 by claude-2 to
   match the repo's own history and the futon3c/apm-lean convention).
   Change it if that is the wrong identity for agent commits here.

4. **`data/pattern-staging/cluster-ledger.tsv` does not exist.** The
   inter-slice convergence rule ("from slice 3 on, report which clusters
   re-surface from DIFFERENT problems than any prior slice") mandates it.
   Slice-3's reinforcements were reported in prose instead. Create it and
   backfill slices 1-3 from the mining reports. Two live entries to seed
   it with: `specialised-packaging-search` (n=1, transcript 3816 mark 3 —
   the real pattern left on the table when
   `search-the-namespace-not-the-qualified-name` was rejected) and
   `separate-evidence-history-from-verdict-state` (rejected, awaiting a
   clean-context leg).

5. ~~`apm-lean/LEMMA-INDEX.md` is missing the six a94A09 lemmas added by
   commit `22c5b80c`.~~ **STRUCK 2026-08-13 — the premise was stale.**
   All six rows are present exactly once in the now-2154-line file
   (codex-4 refused the item; claude-2 verified each with `grep -c`).
   The index was updated after the 2026-08-12 review that generated this
   item, and claude-2 wrote the item from the review's findings without
   re-checking current state.

6. ~~`apm-lean/problems/a94A09/{status.json,proof-outline.md}` describe
   the pre-`22c5b80c` state; `sorry_count_total: 1` is still correct.~~
   **STRUCK 2026-08-13 — the premise was WORSE than stale, it was wrong
   in a way that would have produced a self-contradictory file.** Commit
   `a266157` ("a94A09: close uniqueness via Schwarz-Pick rigidity")
   closed the problem. `Main.lean` now has **no executable `sorry`** —
   its single `sorry` match is inside a comment at line 345 ("the
   sorry-free module `ConstructionTargets.Rouche`"). codex-4 stopped
   rather than execute an instruction that would have preserved a false
   count; claude-2 verified and concurs. **This is the honesty bar
   working in the direction that matters — a dispatched agent refusing a
   captain's wrong premise.**
   REPLACEMENT ITEM, real and unmeasured: `a94A09/status.json` still
   says `sorry_count_total: 1` and `classification: partial-lean-proof`
   for a CLOSED problem. If the corpus percentage is derived from
   `status.json`, it is understated by at least one. A corpus-wide audit
   is worth doing — **but do it with a comment-aware detector.** a94A09
   is itself the proof that a naive `grep -c sorry` lies: 1 match, 0
   executable. A naive audit here would indict every problem whose
   Main.lean merely mentions the word.

7. **`futon3c/scripts/review_codex_lane_attachments.clj` has three
   defects** (filed 2026-08-12, repair not yet scheduled): it hardcodes
   `claude-10` as reviewer, hardcodes `:session-id
   "M-codex-sorry-loop/duree"` regardless of the lane, and hardcodes
   `:verdict :approve` so it cannot record a rejection. Until it is
   repaired, the three a94A09 attachment edges stay `:proposed` even
   though two carry `:approve` review evidence — "approved" there means
   *evidence recorded*, not *attachment projected*.

8. **claude-4's invoke path is unverified.** Registration was verified on
   seven fields, but that only proves the row. Confirm a fresh session id
   is minted on claude-4's first real review — registration returning
   `{:ok true}` while serving a stale configuration is silence-catalogue
   instance 5, and it went unnoticed for three review passes last time.

9. **Grade discrepancy, reviewer's call:**
   `close-bijectivity-by-counting-not-inverting` carries `@grade snippet`
   because the case-1 synthesis graded it SNIPPET, but the move (prove
   injectivity + card equality, apply `Nat.bijective_iff_injective_and_card`)
   reads as a technique. Recorded as authored rather than silently
   re-graded by the captain; claude-4 rules.
