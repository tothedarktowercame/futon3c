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
4. Register hotwords in futon3's patterns-index.tsv. CAUTION: Zone's
   futon3 is NOT under version control — mirror every authored file
   into `futon3c/data/pattern-staging/<case>/` (versioned) until the
   futon3 sync situation is resolved.
5. Review gate: claude-2 (or Fable) reviews formulations before any
   store deposit. Conductor ≠ reviewer.

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
