# E-memory-whitepaper-v3-programme — finishing the M-memory-retrieval V3 whitepaper

**Drafted 2026-08-10 by ams-claude-1 (ground control) at Joe's request.**
Successor to `E-memory-whitepaper-v2-programme.md`. Inputs:
`E-memory-v3-staging.md` (the bank, 2026-08-01), `retrieval-whitepaper-v2.md`
(§"Known residuals, all deferred to V3"), the M-memory-retrieval mission WS6
charter, and the 2026-08-10 Zone campaign (E-ground-control-pass-to-zone.md +
today's first witnessed end-to-end recall chain).

**The V3 thesis, provisionally:** V1 described the architecture and its
pre-repair baseline; V2 turned the instruments on themselves (construct
validity, self-applied). V3 is the *counterfactual* paper: the ablation that
makes "load-bearing" a measurement rather than a judgement — plus the
2026-08-10 finding that the entire deployed pipeline can be silently dead
while every receipt reads normally, which is the strongest form yet of the
silence result the whole series has been converging on.

---

## What changed since the bank was written (2026-08-01 → 2026-08-10)

The staging doc predates the Zone campaign. Six deltas bear directly on V3:

1. **The pipeline was silently dead on Zone, and is now proven alive.** The
   substrate the dispatch CLI reached either did not exist (`ams-store`,
   500s typed into `recall-empty`) or was an empty shadow. Four independent
   silent failures in one morning (lane never wired to recall; store pointed
   at a nonexistent dir; `job_tree.clj` uncommitted with master unbootable;
   A/B outcome columns all null). claude-7's handover names this as a V3
   finding and it is: **the receipts were well-formed the whole time.**
2. **First witnessed end-to-end chain on a clean index (n=1).** a94A09,
   2026-08-10: dispatch-time recall surfaced 2 memories; the runner USED
   `package-every-rouche-homotopy-slice-for-the-argument-principle` and the
   committed Lean (`apm-lean` `22c5b80c`) visibly instantiates the memory's
   prescription (`apm_a94a09_exists_scaled_fixed_point` is the packaged
   slice); IGNORED the other with a stated reason; both receipt halves
   written (offered `e-fab2e3d9…`, outcome
   `e-memory-outcome-sweeper-6e8a041a…`). Existence proof, not a rate.
3. **A1's "no code writer" is partially repaired in the wild.**
   `memory_outcome_sweeper.py` (mechanical-witness/v1) parses runner
   Memory-usage sections from the invoke ledger and writes outcome halves
   idempotently. The A1 backfill (130 job results on dev-laptop; Zone's 169+)
   is now a *run*, not a build. Stratify by provenance and extraction method
   as the bank already requires.
4. **New lexical-stage evidence the bank must absorb.** The anchor term is
   chosen by highest IDF over the *problem* corpus, which selects artifact
   vocabulary: "belonging" (zeroed a conjunctive candidate set) and
   "compiled" (t91A05 surfaces a generic memory through a vacuous anchor).
   This does not contradict A2b's "evidence points away from the lexical
   stage" — it is a different sub-stage (anchor selection, DF-source
   mismatch) and it is *measured*. The candidate repair (rank anchors by
   memory-corpus DF via `?df=`) is designed as an experiment under A2b's
   rule: a repair that does not move the outcome would be worse than none.
5. **The scribe restarted 2026-08-10** after six days repurposed, under an
   explicit importability discipline (memories must cite importable names;
   trapped lemmas flagged for promotion, never presented as reusable). First
   pass (a94A09) in flight as this is written — its drafts include the
   hunger-audit open-hunger memory the protocol requires. This is §I's
   write-side story getting its authored norm.
6. **The site calculus for the ablation inverted.** H4 said the federation
   is one site (dev-laptop); lucy can't run Lean; chicago is dark. But the
   campaign moved: **Zone has apm-lean, the Lean toolchain, the Mathlib
   cache, the clean index, and comparatively fresh codex lanes.** The
   isolated-user design (H3e) should be stood up on Zone.

## Phases

### Phase 0 — instruments before dispatches (the unrecoverables)

Every dispatch that runs without these loses data forever (bank §B). Small
edits, but they touch the live path → Interface 1: new `:recall-system`
version tag (v1.3), landed at a lane boundary, never mid-wave.

- **0a — adopt + run the A1 backfill.** Bless `memory_outcome_sweeper.py` as
  A1's writer (review its extraction against the a94A09 receipt as the
  known-good case). Sweep dev-laptop's 130 + Zone's job ledgers. Deliverable:
  use-rate with provenance strata — the number C3 pays for, measured at last.
- **0b — receipt fields:** B5 (persist which ladder rung fired), B2
  (dispatch-time candidate set: ids + scores + `index-as-of`), B4
  (`:memory-use/kind` substitutive|regulative). One edit to
  `dispatch_with_recall.clj` + receipt schema.
- **0c — B1 reachability index.** Mechanical join of `LEMMA-INDEX.md`, the
  import graph, and problem files — the trapped-lemma audit already computed
  most of it. Converts V2 §5.1's five instances into a rate; doubles as the
  promotion pipeline's (Fable Phase 0/1) measurement instrument.

### Phase 1 — the Zone results, written while fresh (parallel with 0)

- **1a — the silence chapter.** The four-failure morning + V2's three
  unenforced-docstring fields + I3's norms-are-prior claim, written as one
  argument: *the failure mode of this system class is silence, and the
  countermeasure is authored norms enforced by types/instruments, not
  vigilance.* This is V3's methodological core and it is already fully
  evidenced.
- **1b — the witnessed chain (n=1) as an existence result**, with the code
  fingerprint as the witness standard: a USED attribution corroborated by
  the artifact, not by prose. Sets the DV convention Phase 2 needs.
- **1c — anchor-vocabulary findings** (delta 4) with the U-curve and A2b
  restated; preregister the anchor-DF experiment (do not ship the rule).

### Phase 2 — the naturalistic head-to-head — the headline

**REVISED 2026-08-10 (Joe): the ablation is set aside in favour of a
naturalistic paired head-to-head.** Rationale: the E2 facility absorbed
eleven review passes and still carried a do-not-run verdict — the apparatus
was where the fluff lived — while the naturalistic design needs almost no
new apparatus: `dispatch_with_recall.clj` already implements the treatment
contrast as `--memory-channel :push` vs `:none`, and the seats were
provisioned for exactly this (`f56780aa`: ams-codex-2 memory arm,
ams-codex-1 control). This is the 8–9 Aug A/B redone with the instrument
that killed it (null outcome columns) now fixed and proven live. The H
material stays banked; the ValidatedTrace lesson (H6) survives as the
enforcement standard for THIS design's preregistration.

- **2a — design + prereg (light but real).** Same problem, same packet, to
  both seats; own git branch per run from the same base revision, fresh
  session per run (the shared-worktree finding H3c applies to head-to-heads
  too; branches are the cheap fix on open problems, where there is no
  answer to leak). Problem panel by fixed rule: next N open sorries,
  class-stratified (≤1 per prelim class in flight), topology-blocked
  excluded. Predicted direction and falsifier stated before the first
  dispatch.
- **2b — the built-in placebo.** Stratify pairs by dispatch-time recall
  status. Recall-empty pairs are control-vs-control in effect: arm
  differences there estimate the seat/noise confound directly, replacing
  the separate noise-floor pilot. **Falsifier: if arm differences in the
  recall-hit stratum do not exceed the recall-empty stratum, the channel
  does nothing and V3 says so.**
- **2c — DV: behavioural, plural, artifact-corroborated.** Closure /
  sorry-delta, route divergence, memory citation with the a94A09
  fingerprint standard (attribution corroborated by committed code, never
  prose alone), control re-derivation of store-held facts. Paired coding on
  artifacts; honest n= markers.
- **2d — replication arm: `abl-codex-1` on Dionysus** (on Zone's roster,
  invoke-ready) for a subset of pairs — another box is the strongest cheap
  independence check. **Probe first**: whether it has apm-lean + toolchain
  + Mathlib cache; do not count on it until verified.
- **What this trades away, stated in the paper:** the channel-level claim
  ("the memory channel changes behaviour") replaces the memory-level one
  ("memory M was load-bearing"); the P2 rubric stays a judgement. The
  trade buys ecological validity and near-zero apparatus, and treatment
  runs are real campaign progress — the cost is partly recovered.

Budget: ~10–16 paired problems ≈ 20–32 dispatches over a few days, well
under the ablation's estimate. Lane hygiene still applies: fresh sessions
per run; enumerate seat exposure before sizing.

### Phase 3 — repairs V3 reports as repaired (post-measurement only)

- A3: route the codex lane through `review-attachment!` (unblocks C3).
- A2b's attachment-layer test (projection stage), and the anchor-DF
  experiment from 1c — experiments first, rules only if they move outcomes.
- Scribe cadence: one pass per completed dispatch wave, importability
  discipline binding; report the LusinN witness + the restart as I's
  narrative arc. C1 (git-mined corpus) only if instrument validation needs
  it; it restores nothing (bank §D).

### Phase 4 — assembly

Claims-enumeration first (WS6 charter), then: the silence chapter (1a); the
counterfactual result (2d) with the noise floor beside it; use-rate measured
(0a) with strata; reachability as a rate (0c); write-side identity (I + the
scribe restart); conative/norms material (G, I3) as the conceptual frame;
updated correspondence tables (live Ψ still scalar — say so); honest deltas
and the asserted-on-own-authority list, V2-style. The §7-risk sections still
need a reader who was not in the room (V2's own flag; unresolved). Send-to-Rob
gate remains Joe's.

## Sequencing and rough scale

Phase 0 ≈ days (0a immediately; 0b at the next lane boundary; 0c ~a day).
Phase 1 is writing, parallel. Phase 2 is the long pole: 2a-2b setup, then
2c-2d ≈ a week given lane budget. Phase 3 parallel after Phase 0. Phase 4 a
few days. **Realistic: 2–3 weeks to a V3 draft with the ablation in it.**

## Decisions needed (Joe)

1. ~~Isolated user account on Zone~~ **superseded by the 2026-08-10
   naturalistic revision** — branch isolation + optional abl-codex-1
   replication replaces the account setup.
2. **Panel size and dispatch budget** for the head-to-head (~10–16 paired
   problems ≈ 20–32 dispatches proposed).
3. **Interface-1 successor**: the mission names claude-4 (live loop) and
   claude-6 (retrieval growth) as owners; both sessions are gone. Propose:
   both interfaces collapse into ground control (ams-claude-1) with Joe as
   the policy gate, recorded in the mission doc.
4. **Version tag** v1.3 for the 0b receipt fields, landing at a lane
   boundary.
5. **Whether the V3 title continues the arc** (V1 architecture → V2
   instruments → V3 counterfactual/silence) — affects 1a's framing.

## Cross-references

`E-memory-v3-staging.md` (the bank: A/B/C/E/G/H/I) ·
`retrieval-whitepaper-v2.md` §residuals · `M-memory-retrieval.md` WS6 ·
`E-ground-control-pass-to-zone.md` (the four silent failures; §4 gaps) ·
apm-lean `22c5b80c` + receipts `e-fab2e3d9…` / `e-memory-outcome-sweeper-…`
(the witnessed chain) · `memory_outcome_sweeper.py` (A1's writer) ·
`a94A09-scribe/` (the scribe restart pass).
