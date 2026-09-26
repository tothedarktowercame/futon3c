# Cohort 1 — first recall-live training cohort (preregistration)

Registered 2026-07-26, claude-4 (ground control). Casting per Joe: runner
zai-5 (fresh seat; APM cron quota-gated off at 54% usage, no contention).
Charter: holes/missions/M-zai-learning-loop.md.

- **Population (fixed, deterministic — first five unattempted APM problems in
  id order):** a92J02, a93A01, a93A03, a93J02, a93J05. One session each,
  sequential. Session shape: formalize (create lean/Main.lean per a95-series
  conventions) + prove; mechanical witness = lake exit/sorry count/commit.
- **Recall LIVE:** every session dispatched via scripts/dispatch_with_recall.clj
  (offered-receipts recorded; recall timeout 30s bounded; recall-empty
  degrades gracefully and is a typed row condition, not a failure).
- **Scribe pass per session:** codex-2 three-lane mining after each session;
  drafts to operator review; promotion via the proven path. Training
  problems are mineable (no quarantine).
- **Sub-arm (labeled, not counted with the fresh five): one revisit session
  on a96A04** — the Young-blocker problem — testing whether the recalled
  missing-dependency-protocol memory changes behavior at the site it was
  mined from. Confounded by prior repo work; reported as its own row.
- **Counting rule:** batch-0 discipline verbatim. No retries; every outcome
  in the denominator; pacing note (edit early, commit honest partial ~20m)
  in every packet from session 1.
- **Observables:** all four levels. Leaf level live for the first time:
  surfaced-ids vs used-ids in receipts + arc evidence of effect. Zero
  leaf-hits remains publishable.

## Amendment 1 (2026-07-26, before session-1 outcome known; Joe's direction)

Session 1 surfaced recall-empty for a92-terrain: the current recall is
lexically gated (description-less patterns, boilerplate query terms, no
semantic lane). Rather than repeat memoryless behaviour five times, the
memory system is being FIXED IN PARALLEL, targeted to come online mid-cohort:

- **Each row now records :recall-system** — v0-lexical (as-dispatched today)
  or v1-enriched (pattern descriptions + problem-file query terms +
  embedding proposal lane if feasible). Sessions 1–2 expected v0; later
  sessions v1 as fixes land (actual recorded, not assumed).
- The cohort's comparison is now explicitly WITHIN-cohort: broken-by-design
  vs working recall on comparable fresh problems, plus the a96A04 revisit
  arm under v1.
- Fix lanes: codex-6 (pattern descriptions from ground-control vocabulary +
  query-term extraction from problem.md/proof-outline.md), codex-5
  (embedding proposal lane feasibility via futon3a miniLM shared-corpus
  infra; timeboxed — feasibility verdict acceptable if build is large).

## RESULTS (cohort complete, 2026-07-26 evening)

| # | problem | outcome | commit | recall-system | surfaced/used |
|---|---------|---------|--------|---------------|---------------|
| 1 | a92J02 conv-in-measure subseq | **SOLVED** | 8b58d46 | v0-lexical | 0/0 |
| 2 | a93A01 UC iff diff-quotient | **SOLVED** | 035e4d9 | v0-lexical | 0/0 |
| 3 | a93A03 Kadec-Klee + liminf | PARTIAL (1 sorry) | a72683a | v1-enriched | 0/0 |
| 4 | a93J02 alternating harmonic | PARTIAL (1 sorry) | e5158e7 | v1-enriched | 3/0 (reasoned non-use, named) |
| 5 | a93J05 doubly periodic entire | **SOLVED** | 32f09c8 | v1-enriched | 0/0 |
| R | a96A04 heat kernel (REVISIT, labeled) | IMPROVED 4→3 sorries | f614856 | v1-enriched | **2/2 — FIRST LEAF-LEVEL USE EVENT** |

**Fresh arm: 3 solved, 2 honest partials, 0 failures, all formalizations
verified faithful.** All receipts in store (rows 1–3 retro-written during
receipt-path repair; rows 4–R written in-script).

### Observables (four levels)

1. **Leaf: ONE WITNESSED USE EVENT** (revisit arm). Memories mined from
   a95J08/a96A04 cron sessions, recalled into the a96A04 revisit, cited by
   the runner as redirecting effort away from re-probing the absent Young
   inequality toward the provable Gaussian normalization — which it proved
   (4→3 sorries, f614856). Offered receipt e-... (dispatch), outcome
   receipt e-0436fa62 (use-half). The memory→behavior→outcome chain is
   closed and witnessed. Caveat honestly: revisit row is confounded by
   prior repo work; the *use citation* and behavior change are the
   witnessed part, the sorry-reduction attribution is supporting evidence.
2. **Pattern**: session 4 surfaced strategy-level memories in series
   terrain (right neighborhood, wrong level) — partial pattern-level hit;
   the scribe's coverage note formalizes the gap.
3. **Field**: n=6 too small for trends; typed observations recorded
   (2 partials both blocked on API side-conditions/absence, not on
   mathematics — consistent with batch-0's finding).
4. **Structure-formation**: stale-blocked-labels rule reached n=2
   (updated in place, not duplicated); the scribe DECLINED a false n=3
   merge (liminf ≠ missing-theorem — taxonomic differentiation); first
   challenge draft (corpus correction, machine-witnessed).

### Coverage map (scribe note, e6e554a/06ccb5e)

Complex analysis: zero memories. Series: strategy-level only — the
surfacing-miss diagnosis (generic terms outranked discriminative nouns:
series/alternating/harmonic/tsum dropped) is the v1.1 tuning item.
Measure theory/functional analysis: growing.

### Infrastructure fixed mid-cohort (all committed)

- Receipt path: three defects (JSON→EDN endpoint = the lost-writes class;
  missing x-penholder; agency-base instead of substrate-base targeting) —
  66fd3e7, 365a8ea. In-script writes verified from session 4 on.
- Pattern descriptions + problem-file query terms (lane A, 7776cfc);
  reviewed with my own gate re-runs.
- Semantic lane: feasible-later verdict chartered as S5.

### Outputs for operator review

18 scribe drafts (4 strategy, 9 lemma-location, 4 process, 1 challenge)
in memory-drafts-cohort1.edn — including the HasSum conditional-convergence
trap, the corpus-correction challenge (a93A01 informal solution), and the
reasoned-non-use process rule. Promotion path proven; awaiting operator
review cadence.
