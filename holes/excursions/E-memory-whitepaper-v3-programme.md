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

**REFRAMED 2026-08-10 (Joe): design research measured by residual
capability.** At ≈50% proved, closing APM is not in doubt; the question is
what capability remains at the end. The null V3 must beat is "the campaign
was the capability to spend OpenAI tokens." So:

- **Waves, not a frozen RCT.** A few open problems per class (a/m/t/b) per
  wave; system changes (packet, recall, promotion affordances) are
  PERMITTED between waves and are the point — "can we get this to go
  through in one pass, robustly, across classes" is the target. Discipline:
  per-wave prereg + `:recall-system` version tag per wave, never mid-wave.
- **Primary battery — residue per closure, trended across waves:**
  (1) one-pass closure rate per class; (2) deposit: lemmas promoted to
  importable modules, memories citing importable names, index regenerated
  at close; (3) reuse: LIB-citation rate (baseline 6%, README §4g
  adoption) and memory USE under the fingerprint standard; (4) duplication
  manufactured; (5) tokens per closure across waves — falling =
  accumulating capability, flat = the token-spending null, reported
  either way.
- **Paired arms as a duplicate generator (Joe's observation).** Two arms
  on the same problem produce near-exact twin lemmas by construction —
  ground truth the write-side programme (bank §I) never had. Branch
  protocol: `exp/<pid>-mem` and `exp/<pid>-ctl`, no auto-merge; ground
  control adjudicates the merge; twin sets diffed into a labeled
  near-duplicate dataset (the calibration set for §I2's statement-identity
  instrument); the unmerged branch is kept as the variant store per the
  2026-08-09 "duplicates are more to choose from" ruling.
- **The channel contrast nests inside the residue frame:** the
  preregistered prediction is that the memory arm's accumulation SLOPE
  differs (steeper reuse, falling cost), not merely its single-pass score.

**REFINED AGAIN 2026-08-10 (Joe): a LADDER of A/B pairings, batches of 10,
loss-minimizing.** Memory-on vs memory-off is only the BASIC pairing — the
programme is a sequence of increasingly good A/B contrasts *starting* there.

- **Batch structure — "mini workshops."** Each batch ≈ 10 dispatches on
  real open problems (class-stratified). Batch k+1's contrast is chosen
  from batch k's findings; the running-best configuration is always one
  arm, so every batch does real campaign work under the best-known system
  while resolving one design uncertainty. Candidate ladder after B1
  (:push vs :none): :push vs :push+pull; anchor-term DF-source variants
  (memory-corpus vs problem-corpus); packet search-receipt variants;
  fresh-scribed vs stale store; push-don't-pull (top-k library lemmas in
  the packet). Pick by expected loss reduction, prereg one contrast per
  batch.
- **Loss-minimization is the objective, not vindication.** The design goal
  is explicitly NOT "demonstrate memory beats no-memory." Loss = wasted
  dispatches + running a known-worse config + unrecoverable data (missing
  instruments). Rules: every dispatch is real work; both arms' artifacts
  are adjudicated for merge so no run is discarded; contrasts are sized to
  move the next DESIGN decision, not to clear a significance bar — V3
  reports the trajectory and the residue, not a hypothesis test.
- **Post-batch interviews ("focus groups").** After each batch, a debrief
  bell to a sample of runner seats while session context is warm: what did
  you need and not have; did the surfaced memories matter, honestly; what
  did you search for and miss; what would have made this one-pass. This
  generalises the hunger audit from queries to stated needs; output feeds
  (a) the next pairing choice, (b) the scribe's demand-side tags, (c)
  V3's qualitative section. **Separation rule: interviews are design
  signal only — USED claims still require the artifact fingerprint;
  self-report never counts as outcome measurement.**

Budget: batches of 10, number of batches open-ended by design (the ladder
stops when a batch's contrast resolves nothing — a stopping rule, not a
schedule). Lane hygiene still applies: fresh sessions per run; enumerate
seat exposure before sizing. abl-codex-1 replication: probe OVERRAN
2026-08-10 (job invoke-…f6cfb9ba) — liveness unconfirmed; retry with a
longer budget before counting on it.

#### Frames — run containerization on Zone (Joe, 2026-08-10)

Both arms stay on Zone (32 cores / 249 GB, ~181 GB available with GLM 4.5
Air down; `.lake` cache 7.2 GB). Containerization follows Joe's **frames**
notion (Futon6): in the Minsky reading, a run is a typed slot-structure
with required slots enforced by construction — the shape already realized
by the γ frame in `futon6/holes/anatomy-of-a-wm-flight.md` ("no frame, no
pair, no commit"); in the pool reading, a frame is one bounded round —
rack, play, score, clear. Both converge on the same artifact. This is the
E2/ValidatedTrace lesson landing at the right altitude: ~50 lines of frame
construction/validation in bb, not a proof facility.

**Setup slots (filled before dispatch, frame invalid without them):**
`:frame/id` · `:problem` · `:base-revision` · `:checkout` (git worktree of
apm-lean at base-revision + `cp -al` of `.lake` — hardlinks make the 7.2 GB
cache per-frame free; verify in frame 0 that lake writes fresh files rather
than mutating in place) · `:branch` (`exp/<pid>-<arm>`) · `:seat` +
`:session` (fresh) · `:memory-channel` + `:recall-system` version ·
`:resources` (systemd transient scope in `futon-agents.slice`, e.g.
`MemoryMax=16G CPUQuota=400%` — 4-6 concurrent frames fit comfortably) ·
`:budget` (tokens/wall-clock).

**Closure slots (a frame without them is `:incomplete`, never scored):**
`:commit-or-obstruction` · `:receipts` (offered AND outcome ids) ·
`:lake-result` (exit + output hash) · `:axioms` (verbatim) · `:interview`
(debrief transcript ref, or an explicit skip reason) · `:twin-diff` (once
the pair completes). The A/B's null-outcome-columns death of 8–9 Aug
becomes STRUCTURALLY impossible: an unscoreable frame is visible as
`:incomplete`, not silently counted.

Frame records land as EDN beside the repl-traces convention
(`data/experiment-frames/<batch>/<frame-id>.edn`) plus an evidence row per
frame. The batch (frame-system) = 10 frames + prereg + adjudicated merges
+ twin-lemma diff + retro.

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

---

## The second-half research programme (Joe, 2026-08-10: "one hop isn't a programme")

**The design the corpus hands us.** The first ~50% of APM ran with the memory
channel dead (Zone: silently, all of it) or structurally throttled (91%
trapped, no scribe, no outcome writer). It is an *accidental control cohort*,
baselined by `E-memory-priors-survey.md`. The remaining ~291 problems
(~27 days at the measured 10.69 clean/day) run fully instrumented: live
channel, v1.3 kinds, frames, scribe cadence, reviewer gate, the A/B ladder.
V3's spine is therefore a **longitudinal before/after study the size of the
whole campaign**, with each prereg hop instantiating one wave of this
standing programme — not the programme itself.

### Memory kinds under study (taxonomy to be refined by interview data)

| kind | examples | core prediction over the half |
|---|---|---|
| **substitutive-content** | lemma-locations, importable pointers, routes | fingerprinted use rises from **n=0** only after promotion makes pointers importable; tracks importable fraction, not memory count |
| **regulative/practice** | packaging prescriptions, search-order discipline | continues to dominate fingerprinted uses early (our only Tier-1 case is regulative); stated so it can LOSE to substitutive once supply exists |
| **open-hunger** | the Schwarz–Pick/Möbius marker (first tracked instance) | converts to grounded memories; time-to-grounding is measurable and falls as scribe cadence holds |
| **negative/boundary** | failed-route records (three failed Jensen attempts) | repeat-failed-route incidents fall where a boundary memory existed and surfaced |
| **consolidation** | the five cross-problem pairs; cyclic-number theorem | one promoted general lemma closes gaps in ≥2 problems; class-local duplication falls |
| **identity/canonical-location** | "this fact lives HERE" (write-side) | twin-lemma manufacture per closure falls after the identity norm + paired-arm dataset calibrate detection |

### Longitudinal predictions (each with falsifier + instrument)

- **P1 (design research).** One-pass closure rate rises across waves while
  tokens-per-closure falls. *Falsifier:* flat curves = the token-spending
  null, reported as such. *Instrument:* frames.bb batch records.
- **P2 (witnessed use).** Fingerprinted use events grow from 1 to a
  preregistered N (proposed: ≥12 over the half), with kind distribution
  shifting substitutive-ward after promotion Phase 1. *Falsifier:*
  regulative stays ≥80% of fingerprinted uses ⇒ §4's surprise is the
  finding and the promotion investment thesis is wrong. *Instrument:* v1.3
  receipts + sweeper + fingerprint audits at wave close.
- **P3 (recall coverage).** Recall-empty falls from 64% toward a floor as
  scribe coverage accumulates — floor stated in advance (t91A05-class
  problems where empty is honest). *Falsifier:* empty-rate insensitive to
  scribe coverage ⇒ the attachment-layer conjecture (A2b) dominates and
  lexical/scribe work is misdirected. *Instrument:* per-dispatch
  recall-status census, re-run monthly.
- **P4 (library uptake).** LIB-citation rises from the 6% adoption baseline
  after index automation + push-don't-pull land mid-programme. *Falsifier:*
  no rise ⇒ affordance was not the bottleneck (the propensity story wins).
  *Instrument:* import scans per wave (the 0c reachability index).
- **P5 (write-side).** Re-derivation incidents per 100 closures fall after
  the identity norm; the paired-arm twin dataset calibrates the detector.
  *Falsifier:* duplication rises with retrieval precision (claude-9's
  standing prediction, §I) — which would be a MAJOR positive result for the
  write-side thesis, stated now so it cannot be claimed post-hoc.
- **P6 (creation loop).** First scribe-authored → later-surfaced → USED
  chain occurs (currently n=0), then recurs at a reportable rate.
  *Instrument:* provenance join between scribe output ids and receipt
  used-ids.
- **P7 (the channel).** Cumulative across the ladder: hit-stratum arm
  differences exceed empty-stratum differences. *Falsifier:* they do not,
  at cumulative n across all waves ⇒ the channel does nothing detectable
  and V3 leads with that.

### Phase map over the half

Waves 1–2: baseline contrast (:push/:none), instrument shakedown, first
interview corpus. Then **promotion Phase 1** (the banked single-lemma
experiment — now doubly motivated by P2's kind question) grows substitutive
supply; waves 3+ measure uptake. Mid-programme: index automation (the
cheapest lever, still unautomated) and the push-don't-pull rung. Monthly:
re-run the priors-survey census — those tables ARE the paper's longitudinal
results section. Scribe pass per completed wave; interviews feed the
taxonomy; APPROVALS through the reviewer seat.

**Standing rule:** every batch prereg cites this section and instantiates
exactly one wave of it. The programme is the object; preregs are its
turns.

---

## Generic proof plans: the vote-and-callback pipeline (Joe, 2026-08-10)

**The mechanism.** Every demand signal — a runner's failed search (interview
Q3), a hunger memory, a Tier-A prove-or-find gap, a re-derivation — is a
**vote** for a concept. Votes are recorded as typed evidence rows keyed by a
canonical concept slug. At threshold (≥2 distinct problems), the concept
goes to a build lane (ConstructionTargets packet, spec preferably taken
verbatim from the voters — closers state exactly what they need). Each
blocked problem meanwhile parks its rerun on `concept:<slug>` — **the park
IS the callback**: when the artifact lands (compiled, importable, indexed,
memory written), ground control completes the dep and every voter's rerun
wakes as a frame with the artifact named in its packet.

A blocked problem thus becomes a **generic proof plan**: an honest partial
+ precisely stated obligations + concept deps wired to callbacks. This is
not an A/B arm; it is the pipeline the A/B ladder measures. It is also the
residual-capability thesis made operational: the votes ledger, the build
lane, and the callback registry are what remain when the corpus is done.

**Choreography rule (learned from frame 0's double-harvest):** one delivery
channel of record per job — park OR auto-bellback, never both; park
payloads open with a has-this-been-harvested check. Concept deps are
completed manually by ground control after verifying the artifact, never
automatically by job completion.

**First revolution (live, 2026-08-10):** `schwarz-pick-rigidity` — votes:
a94A09 frame-0 run + interview ×2 (searches verbatim), the a09 open-hunger
memory, and the README §4m topology cluster's adjacent asks. Spec: the
runner's own `eq_id_of_two_fixed_points` statement. Build dispatched;
a94A09-uniqueness rerun parked on `concept:schwarz-pick-rigidity`.
Second candidate at threshold: the thrice-derived polynomial zero-count
bridge (a92J05, a97A08, a94A09) — claude-2's review finding.

**Candidate rung (Joe, 2026-08-11): model-of-runner contrast.** The frame
machinery is seat-agnostic, so arms can differ by MODEL rather than by
memory configuration: ZAI workers vs Codex workers on paired problems, with
Codex pushed to the reviewer role for those batches (roles rotate; the
operator/reviewer/scribe separation holds regardless of which model fills
each). Enables model comparison on the same residue battery — and the
budget distribution (three independent quotas) makes it nearly free to
schedule. Queue behind the retrieval rungs.
