# E-memory-priors-survey — what we actually know before preregistering

**Drafted 2026-08-10 by ams-claude-1 at Joe's direction**: before the batch-1
prereg, survey the evidence that memories — when created and when found —
help proving outcomes, and state priors honestly. Joe's assessment going in:
the evidence is thin even at ~50% corpus completion. The survey's job is to
make the thinness precise. Store counts re-run 2026-08-10 against
migration-store-21; all other figures cite their source documents.

## 1. The witnessed-use census (re-counted today)

Across the ENTIRE store: **98** memory-use receipts (29 offered-half, 42
outcome-half, 27 legacy untyped). Of these, **11 have non-empty used-ids**,
naming **15 distinct memories** — over a campaign of 490+ problems and
thousands of dispatches. Exactly **one** use is artifact-corroborated
(a94A09, 2026-08-10: the memory's prescription visibly instantiated as
`apm_a94a09_exists_scaled_fixed_point` in commit `22c5b80c`).

Floor caveat, both directions: `used-ids` had no code writer until the
sweeper (bank §A1; populated ~16% of outcome halves when hand-authored), so
true use exceeds 11 — but the A1+ signal says the *dropped* uses skewed
load-bearing, so the bias direction is known and the count is still a floor,
not an estimate.

## 2. Evidence FOR, by strength tier

- **Tier 1 — artifact-corroborated (n=1).** a94A09: recall surfaced 2, the
  runner USED the rouche-packaging memory, the committed Lean carries its
  fingerprint, existence closed in the same 5-minute dispatch. Also the
  clean IGNORED with correct reason — the channel discriminates.
- **Tier 2 — attributed, uncorroborated (n=10 receipts).** Includes
  cohort-2's "saved a search on a problem already solved" (weak, and known);
  the 2026-08-01 cluster (7 receipts) from the cohort runs. P2's
  adjudication rated 17/49 coded uses load-bearing (38%) — but that rubric
  is exactly what H1 says is unvalidated; it remains judgement.
- **Tier 3 — mechanism/indirect.** The measured cost of NOT having the
  channel: the polynomial zero-count bridge independently derived **three
  times** (a92J05, a97A08, a94A09 — claude-2 review); twelve lemniscate
  lemmas derived three times; twins 76 minutes apart. Re-derivation is real,
  recurrent, and priced in dispatches. Separately, the trapped-lemma audit
  (91% unimportable) shows the channel has been structurally throttled all
  campaign: memories were largely re-derivation *instructions*, so the weak
  observed effect UNDERSTATES a repaired channel's potential. That argument
  cuts both ways — it is also an admission that most past "memory help"
  could not have worked.

## 3. Evidence AGAINST / bounding

- **Most dispatches had no memory input at all**: recall-empty 64%
  (structural, P1), 80/90 recorded queries return no candidate under the
  sweep operator (P3) — while the campaign still closed ~200 problems (46%
  of attempted, HUD 2026-08-09). Closure demonstrably does not require the
  channel; whatever the effect is, it is marginal on top of capable closers.
- **62% of surfacing slots went to memories never used anywhere; 52 of 77
  memories were never used once** (V2). Surfacing ≠ helping.
- **Two lexical failure-mechanisms falsified** (bank §A2/A2b) — our model of
  why retrieval fails has been wrong twice; and the anchor-vocabulary defect
  (t91A05's "compiled") shows surfacing can be pure noise.
- **The 8–9 Aug A/B produced zero usable outcome rows** (1337 nulls). No
  evidence either way; thrown out.
- **Zone-specific**: every Zone dispatch before 2026-08-10 ran effectively
  memoryless regardless of intent (the silent substrate failure), so the
  campaign's Zone-era outcomes are all "control arm" in retrospect.

## 4. One genuinely surprising datum

Under the v1.3 kind mapping, the single fingerprinted USE is a
**regulative** memory (`:feedback`-kind — it shaped how the runner packaged
hypotheses), not a substitutive lemma-pointer. Our best-evidenced help is
guidance-shaped, while the entire promotion/importability programme targets
substitutive content whose reuse we have never fingerprinted even once.
The prereg should not assume the effect lives where we have been building.

## 5. Proposed priors (for Joe to adjust before any prereg)

1. **P(recall surfaces something | APM dispatch, current store)** ≈ one
   third (the non-empty rate), of which relevance is uncertain; direct
   a94A09-grade hits expected mainly where scribe passes have covered the
   terrain — currently one problem plus the codexpilot legacy.
2. **Effect given a relevant memory surfaces**: moderate on route shape and
   token cost (anchored on Tier 1 + re-derivation prices); **small-to-nil on
   binary closure** — capable closers re-derive; the channel buys time, not
   possibility. Batch-1 should therefore expect signal in route divergence
   and cost, NOT closure rate, at n=10.
3. **Regulative vs substitutive**: no prior worth stating beyond §4's
   surprise; v1.3 kinds exist precisely so this becomes data.
4. **Creation-side (scribe) effect**: n=0 — no scribe-authored memory has
   yet been surfaced to a later dispatch. Batch 1 may generate the first
   such event; treat any occurrence as reportable.
5. **The falsifier stands**: if arm differences in the recall-hit stratum do
   not exceed the recall-empty stratum, the channel does nothing detectable
   at this n, and V3 reports that.

## 6. What this survey cannot say

No dispatch in the campaign's history has run with a memory *withheld*. The
counterfactual is unmeasured; the ablation design stays banked (programme
§Phase 2 history). The head-to-head bounds the channel effect from the
naturalistic side only.

## Sources

Store census 2026-08-10 (this doc §1) · a94A09 chain: job
`invoke-1786369654355-3517-0994cab1`, commit `22c5b80c`, receipts
`e-fab2e3d9…`/`e-memory-outcome-sweeper-6e8a041a…`, claude-2 review pass 4 ·
`E-memory-v3-staging.md` §§A1/A2/H · `retrieval-whitepaper-v2.md` §§4-5 ·
`README-apm-lean-ground-control.md` §4d/4g/4m · HUD figures 2026-08-09 ·
`E-ground-control-pass-to-zone.md` §2.
