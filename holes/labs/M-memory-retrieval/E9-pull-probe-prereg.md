# E9 — pull-only retrieval probe (preregistered, 2026-08-03)

**Question (Joe):** select a problem likely to elicit memories, do NOT
spoonfeed them, and see whether zai retrieves them mid-session.
Single hand dispatch, operator-commissioned, direct (not via cohort
apparatus). This is a PROBE: retrieval-level scoring only; it is not a
cohort row and carries no arm-assignment claims.

## Arm

Pull-only: plain bell (no dispatch_with_recall ⇒ no push injection —
verified: the cron path's plain bells carried no recall augmentation)
+ an invitation paragraph naming the REAL tools (`memory_search`,
`pattern_memory`; note `library_search` does not exist — the
V3-arm-design line listing it inherits the known invented name and
needs a one-word fix).

## Selection rule (mechanical; both versions recorded)

- v1: first problem in `candidate_problem_ids()` order whose informal
  text shares ≥2 slug-derived terms (len>3) with a dense endpoint
  (density ≥16). OVER-MATCHED on generic terms: selected a96J02 via
  {complex, only}. Recorded, discarded, rule revised BEFORE dispatch.
- v2 (used): terms len ≥6, extended math-generic stoplist, first-match
  ≥2. **SELECTED: a96J04** — endpoint
  math-formalization/tactic-algebra-interference (density 20), overlap
  {absolute, measure}. Alternates seen and recorded: a96J05 (4 terms),
  a96J07 (10), a96J08 (9), a97J01 (8). First-match discipline kept
  over max-overlap precisely because the data had been seen; **a96J07
  is the designated second probe** if one is wanted.

## Expected known-item memories (named BEFORE dispatch, from the
attachment export; never in the packet)

1. `e-codexpilot-specify-invariant-measure-when-lintegral-translation-stalls`
2. `e-codexpilot-state-the-absolute-integral-normalization-explicitly`

Both: tactic-algebra-interference endpoint, reviewed, current.

## Leakage check (mechanical, pre-dispatch)

Packet text ∩ expected-memory slug terms (len ≥6) must be ∅. Result
recorded below at dispatch.

## Predictions (written before the run)

- P1 (the binding risk): the tools are NOT called at all — the S3
  precedent (runner hunted a lemma by name with memory_search unused).
  The invitation paragraph is the treatment; P1 tests
  elicitation-by-invitation.
- P2: IF called with obstacle vocabulary, expected memories surface at
  readable pre-cutoff ranks — E8's oracle-arm result (vocabulary is
  the lever; pull queries are composed at maximum information) makes
  hit the expected outcome CONDITIONAL on the call.
- P3: the offered/pull receipts are readable either way (SEQ-0.5
  fields: mode, degraded?, reason; codex-5's pull-side recording) —
  and an empty-under-load pull would be v9's first organic receipt.

## Scoring

Retrieval-level now: tool-called? (job transcript), known-item
presence + rank (pre-cutoff), ranking-mode fields, used-ids.
Outcome-level: NOT scored here (endpoint capture 1a in flight);
anything outcome-shaped is observational context only.

## Dispatch record (filled at dispatch)

- Quota gate: OPEN at 2026-08-03T13:54:59Z (unit3 82 available).
- Dispatched via agency_send --from claude-10 (attributable, parkable,
  bell-back routable — not http-caller).

- Leakage check v1: FAILED on 'invariant' (base packet's process-rule
  boilerplate, non-math sense); packet reworded ("standing rules")
  rather than waived — the check stays binary. Recheck: NONE, clean.
- Dispatched: job invoke-1785765468643-936-8155fcf8 to zai-1, park
  park-129aa0f7 (deadline 50 min; zai ~30-min cap is SOFT, overrun is
  not stuckness).

## RESULTS (read from receipts 2026-08-03 ~14:20Z, pre-bellback)

Job completed (~16 min). Proof outcome (context only, not scored):
honest partial — 1 sorry at a genuinely-missing Mathlib dependency
(open ⊆ ℝ as countable disjoint-interval union), boundary documented.

- **P1: CONFIRMED — the memory tools were NEVER called.** Transcript
  evidence (store, zai-1 turn-round entries, rounds 1–55): 24 tool
  calls captured = run_shell 11, edit_file 5, write_file 4, read_file
  3, generic code search 1; memory_search 0, pattern_memory 0. All
  memory_search strings in the store window are coordination echoes
  of our own planning messages. No pull-side surfacing receipts
  exist for the job (consistent: nothing was called).
- **P2: UNTESTED** — conditional on a call that never happened. The
  two named memories were never given the chance to surface.
- **P3: receipts readable, trivially** — and NOT a v9 event: this is
  never-called, not called-and-starved.
- **The sharpening irony, recorded:** the runner ended blocked on a
  MISSING DEPENDENCY — the canonical situation the memory system
  exists for, and the densest endpoint in the corpus
  (missing-dependency-protocol, 40) — and never once consulted the
  store it was explicitly invited to use.

**Reading:** replicates S3's observation under preregistration. The
binding constraint on the pull channel is TOOL-CALLING PROPENSITY,
not retrieval quality. A passive invitation paragraph is insufficient
elicitation. Design consequence for axis 1: pull arms need a process
GATE, not an invitation — e.g. "before declaring a blocker you MUST
run one memory_search on the obstacle terms and cite its result in
the boundary note" — which is the talk-vs-walk structural lesson
(instructions don't change behavior; gates do) applied to the
runner's own memory use. Designated second probe (a96J07) should test
the gate variant against this invitation baseline.

- Bellback (post-hoc, verdicts unchanged): zai's final summary matches
  the receipts read — exit 0, 1 sorry, classification Partial, commit
  `1d01493`. The summary itself contains no mention of memory tools,
  consistent with never-called. Two proven helper lemmas banked
  (monotone_image_interval, monotone_image_Icc).

## COUNTERFACTUAL ANALYSIS (post-hoc, computed 2026-08-03 ~14:45Z)

Joe's question: had it used the memory, better chance? Answered by
running the tools ourselves rather than speculating. **The pull
channel fails at THREE stacked layers, each independently
sufficient:**

1. **Propensity (P1, confirmed):** no memory tool was ever called.
2. **Affordance:** the invited tools cannot express the need —
   memory_search takes METADATA FILTERS ONLY (no query string;
   verified in zai_api.clj), pattern_memory takes tags only. The one
   vocabulary-capable tool is psr_search (query + top_k, "search the
   futon pattern library... bounded hooks for reviewed attached
   memories") — present in the tool set but named in NO invitation,
   and the arm-design's phantom "library_search" is almost certainly
   a garbling of it.
3. **Index reach:** counterfactual psr_search runs (same backend
   resolution as the harness): the blocker's actual vocabulary
   ("decompose open subset ... countable disjoint intervals") returns
   five org-process patterns, zero math. "missing dependency
   protocol" — the DENSEST endpoint's exact name — returns
   stop-the-line/logging/coordination patterns, zero math (its name
   tokens are org-generic; lexically camouflaged). Generic math
   vocabulary ("lintegral measure integration Mathlib lemma") DOES
   reach the math-formalization family — so coverage exists, but
   neither blocker-specific nor protocol-name queries resolve to the
   memories that matter.

**Verdict on the counterfactual: NO — with the tools as they stand,
even perfect propensity would have hit layers 2-3.** The
missing-dependency-protocol memories (the ones on point for the
actual blocker class) were unreachable via any plausible query. This
extends v9's candidate-generation locus to the pull channel: pull
fails at generation too, one layer per mechanism, all three
co-present in one 16-minute session.

Secondary check: the runner's "not available as a packaged lemma in
Mathlib" boundary claim was grep-probed (not exhaustively): no direct
packaged form found; claim plausible though a countable-basis route
around it was not investigated. Left open.

## EXIT INTERVIEW (testimony, clearly marked as such; all checkable
claims verified against the record)

Interview job invoke-1785769354670-942-b98c938a. Testimony aligned
with the record on EVERY checkable point (zero calls; memory_search's
metadata-only shape accurately described; the single generic-search
call matches its "one targeted grep" account; psr_search awareness
consistent with the system prompt). No confabulation detected —
high-quality testimony.

**Classification: H-propensity primary, H-framing secondary.**

- PROPENSITY (its own words): the invitation "read as informational
  context, not as an instruction to actively query"; the blocker was
  framed as "a genuine Mathlib gap to document, not as something a
  prior session might have already partially addressed"; "I should
  have had that thought. I didn't." The missing move is a FRAME SHIFT
  (world-fact -> repo-history-fact), which an invitation does not
  force and a gate would.
- FRAMING (the discovery): the runner KNEW psr_search and correctly
  understood its self-description — "consulted before adopting a
  library pattern" — and therefore rationally did not reach for it at
  an OBSTACLE ("didn't feel like a reusable pattern... one-off proof
  attempt"). The right tool exists, was known, and its own
  description miscues the pull use case.
- Its ranked options at the blocker (prove from scratch / avoid /
  one Mathlib grep / commit honestly) are exactly right EXCEPT for
  the absent memory step, which it named itself unprompted.

**Q5 verification — even its own hypothetical queries fail:** its two
verbatim psr_search phrasings return org-process patterns (plus
tangential math-informal hits); its imagined memory_search tags
["open-set" "interval-decomposition" "Lebesgue" "null-set"] return 0
items — no such tag vocabulary exists in the store.

## THE COMPLETE ANATOMY (one problem-spend + one interview)

Four distinguishable mechanisms, each evidenced, each with a cheap fix:
1. Propensity — invitation != trigger → process GATE (probe 2, a96J07).
2. psr_search framing — self-description excludes the obstacle case →
   one-line description change ("also when blocked: search for prior
   obstacles").
3. memory_search affordance — no vocabulary parameter → add a query
   param or route content queries to psr_search.
4. Index/tag reach — blocker vocabulary and even the runner's natural
   tags resolve to nothing; densest endpoint lexically camouflaged →
   math hotwords/tags on endpoints and memories (metadata work, not
   code).
