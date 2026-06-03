# Campaign: C-substrate-completion — A Real Ground Metric for substrate-2

**Date:** 2026-05-31
**Status:** CHARTER + CONSTITUTION + ESCROW (2026-05-31) + STANDARD-ARGUE (§4) + **STANDARD-VERIFY RATIFIED (Joe, 2026-06-01, PASS-on-design)**. Keystone `M-substrate-metric` is **DELIVERED (v1)** with named residue; O1 closed; E1 curvature query delivered; E2 continuity cut passed O4(b)/O4(c). **Escrow E1 + E2 `:contract-released`** — consumers build to the verified spec. Campaign remains open until at least one paired requirement is consumed live. Working mode: **swarm**.
**Charter source:** `futon3/holes/war-bulletin-10.md` WR-21.
**Lifecycle:** `futon4/holes/campaign-lifecycle.md` (the first Campaign instance under it).
**Owner:** Joe; coordination ownership transferable.
**Authors:** claude-1 (2026-05-31, emacs-repl, paired with Joe).

> This is the **first Campaign**. It is *chartered*, not *running* — and certainly
> not *validated*. The claim is only that it passes the shape-first + grounding
> discipline (`campaign-coherence/shared-standard-has-no-single-owner`): a live
> joint goal a single Mission demonstrably could not own.

---

## 1. CHARTER

### Joint goal / gap

**substrate-2 has no real metric.** It carries a scalar field `T: nodes → [0,1]`
plus discrete `ΔT` along edges — a *function painted on a graph*, not a manifold.
None of the objects rich AIF and differentiable methods need (distance, tangent
space, curvature, natural gradient) exist.

The gap is **cross-mission by construction**: three Missions each need a
*geometric* object from substrate-2, and each needs the **same** underlying metric —
but each sees only its own slice, so none can establish that one metric serves all
of them. Stated as the obligation no single Mission can discharge:

> *Deliver a ground metric on substrate-2 whose derived objects (curvature,
> continuity, information geometry) are simultaneously adequate for the
> tension-proposer (M-aif2), gradient-taking (M-differentiable-code), and the
> manifold reconstruction (E-codebase-manifold) — and verify that adequacy before
> any of them builds to it.*

### The shared standard (what the Campaign delivers)

A **ground metric on substrate-2**: a distance `d` over substrate-2 nodes, built
from a blend of **dependency + git co-change + semantic distance**, exposing a
verified *contract* of derived objects:

- **Curvature** — Ollivier–Ricci `κ(x,y) = 1 − W₁(μ_x, μ_y)/d(x,y)` (discrete, via
  the Wasserstein / earth-mover metric; negative on bridges/bottlenecks = "ameliorate
  here"). Consumed by M-aif2 as *principled tension*.
- **Continuity** — a continuous embedding (spectral / diffusion; graph-Laplacian →
  Laplace–Beltrami) giving a smooth structure for gradients. Consumed by
  M-differentiable-code.
- **Information geometry (latent)** — the Fisher–Rao metric on the WM belief
  simplex (R1 per-entity posteriors), already present and unused; the standard names
  how it relates to the external metric.

The **standard is the contract**, not the implementation: what `d` must satisfy and
what queries it must expose (`distance`, `curvature-at`, `embedding`), such that all
three consumers are served.

### Joint completion criterion

The Campaign dissolves when **both** hold:
1. **STANDARD-VERIFY passes** — the metric's contract is verified *fit for all
   consumers* (aif2's curvature-as-tension requirement **and**
   M-differentiable-code's continuity requirement are each verifiable against the
   contract), as a design, before either builds to it.
2. **≥1 paired requirement is released-and-consumed** — e.g. M-aif2's
   tension-proposer reads curvature from the live metric (escrow entry `:satisfied`).

(Note: *not* "all members done." The criterion is cross-mission by design.)

### Membership

| Role | Mission | State | What it contributes / needs |
|---|---|---|---|
| **Keystone** — owner **codex-3** | `M-substrate-metric` (`futon3c/holes/missions/`) | **OPENED 2026-06-01**; HEAD/IDENTIFY done, O1 schema in MAP | Defines + builds the ground metric; owns its own internal lifecycle/VERIFY |
| **Paired (consumer, E1)** — owner **claude-3** | `M-aif2` (`futon2/holes/M-aif2.md`) | E1 consumer; **M-aif2 mission CLOSED on own criteria (2026-06-02)**; slice-1 live-installed reading the delivered curvature; **extensions possible depending on the Campaign outcome** | Needs curvature as the tension-proposer's signal (replaces the signed-grad-vs-`(1−T)` the Stage-B spike exposed) |
| **Paired (consumer, E2)** — owner **claude-6** | `M-differentiable-code` (`futon5/holes/missions/`) | early pilot; O1 closure set declared | Needs the continuous embedding for gradients over the code graph |
| **Paired (consumer, E3)** — owner **claude-6** · **LATE JOINER (2026-06-02)** | `M-categorical-code` (`futon5/holes/missions/`) | IDENTIFY + MAP done 2026-06-02; **ESCROW E3 `:contract-released`** (metric-side; (c) dynamics self-sourced from git/XTDB — no O5) | Generalizes E2 via a functor `F: Evolution → Code` (porting futon5's evolution machinery to code); consumes the metric for code-health **diagnostics** (coupling, structural entropy) + as the Genotype it evolves |
| **Demonstrator** | `E-codebase-manifold` (M-aif2 §6) | follow-up, unopened | The evolving-manifold animation = W₂ mass-flow over the metric; validates the metric is meaningful |

### Provenance

- Chartered from **war-bulletin-10 WR-21** (2026-05-31).
- Crossing point surfaced this session: Joe's frank pushback that the "thin scalar
  field" blocks AIF-as-he-wants and M-differentiable-code; the aif2 VERIFY Stage-B
  spike's tension-polarity finding made it concrete.
- Mathematics pointers: Rob (information manifolds; the Wasserstein / earth-mover
  metric → Ollivier–Ricci curvature as the discrete↔continuous bridge).

### Grounding — why a Campaign, not a Mission

Per `campaign-coherence/shared-standard-has-no-single-owner`: the metric's
**cross-mission adequacy** has no single-Mission owner — each consumer sees only its
own requirement. The **evidence** that a Mission could not own it is already on the
record: WR-21 documents the thin scalar field having **forked into three thin
shadows** — aif2's ΔT tie-breaker, M-differentiable-code's would-be continuity,
M-live-geometric-stack's T — none a real metric. That fork is the failure mode
(`HOWEVER (b)`) the Campaign exists to prevent.

### The dependencies this Campaign will escrow (preview — to formalise in ESCROW)

| from-mission | on | requirement | status |
|---|---|---|---|
| M-aif2 | the metric | curvature as the tension-proposer's signal | `:held` |
| M-differentiable-code | the metric | continuous embedding for code-graph gradients | `:held` |

Both `:held` until STANDARD-VERIFY (`→ :contract-released`) and DELIVER (`→ :satisfied`).

**CHARTER exit criterion:** ✅ **MET — Joe ratified 2026-05-31** (joint goal is real,
genuinely cross-mission, membership right). → CONSTITUTION next (survey the three
dependent missions + governance + ready/missing).

---

## 2. CONSTITUTION  *(≈ MAP — survey the dependents + set governance; 2026-05-31)*

### Dependent-mission survey (read, not guessed)

| Member | Needs from the metric | Contributes | State (verified 2026-05-31) |
|---|---|---|---|
| **M-aif2** | **Curvature as the tension-proposer's signal** — Ollivier–Ricci over substrate-2, replacing the signed-grad-vs-`(1−T)` the Stage-B spike exposed. Negative curvature (bottlenecks) = where to propose. | The consumer with a *verified* spec already (its `aif2-exotype` + invariants tell the metric exactly what "tension" must support). | IDENTIFY→VERIFY complete; INSTANTIATE held on metric. |
| **M-differentiable-code** | **(a) Node-granularity / uniformity** (its *#1 upstream blocker* — a gradient over nodes spanning 4 orders of magnitude is ill-conditioned); **(b) a continuous embedding** of the code graph (the band a gradient ranges over). Constraints: *not* a black-box GNN (R-GCN found wanting — measurement instrument, not learned objective); **canonical trees only** (exclude worktrees/origin/`.state`). | Turns the metric's `∇` from heuristic into a *literal autodiff gradient*; identifies real continuous-embedding assets (`futon3/resources/embeddings/*`, `futon6/src/futon6/graph_embed.py` typed-hypergraph embedding). | MAP opened 2026-05-31 (the "notes landing" Joe flagged). |
| **E-codebase-manifold** | The metric + a time axis (git history) to animate as W₂ mass-flow. | The *demonstrator* — proves the metric is meaningful (a wrong metric makes an ugly/meaningless animation). | Follow-up stub (M-aif2 §6), unopened. |

### CONSTITUTION finding — node-granularity is a *shared prerequisite*, not just M-differentiable-code's

M-differentiable-code's #1 blocker (uniform, well-scaled nodes) is **also the metric's own precondition**: you cannot define a distance `d` without first defining *what the nodes are*. So node-granularity is **cross-cutting** — it belongs in the metric's contract (the keystone must resolve it), and it is the point where M-differentiable-code's blocker and the metric converge. This is exactly the cross-mission coupling a Campaign exists to surface: resolving node-granularity once, in the keystone, unblocks two members at the same time. (Joe's standing note — refactor/enforce a size-limit, converging toward Rob's ≤800-line chunking — is a candidate path; flag it for the keystone's MAP.)

### Governance (roles & decision rights)

- **Coordination owner:** the agent in the chair paired with Joe (claude-1 this session). Single supervised thread.
- **STANDARD-VERIFY ratifier:** **Joe.** The cross-mission-adequacy sign-off *is* the Campaign's reason to exist; operator-ratified, consistent with the consent-gate discipline.
- **Dissolution authority:** Joe.
- **Autonomous form (deferred, per Joe "we don't need that yet"):** the "Autonomous" in *Temporary Autonomous Institution* would mean **one agent per member mission, moving them in parallel**. Activation trigger = the **post-STANDARD-VERIFY fan-out** (when the verified contract lets paired missions resume *concurrently*) — that is the natural moment parallel agents help. Migration follows supervised→autonomous (`[[project_consent_gate]]`). Named here so it isn't silently assumed; not activated.

### Coordination shape

**Convergent** (confirmed): the keystone (`M-substrate-metric`) must land the verified contract first; the two paired missions are held until then, then fan out in parallel — which is exactly where the deferred per-mission agents would come in.

### Ready vs. missing (Campaign scope)

| Ready (reuse) | Missing (the Campaign's work) |
|---|---|
| M-aif2's verified spec = consumer #1's exact requirement | **`M-substrate-metric` not yet opened** — the keystone (promote it) |
| Continuous-embedding assets (`futon3/resources/embeddings/*`, `futon6 graph_embed.py`) | The ground metric `d` (dependency + co-change + semantic) + **node-granularity resolution** (the shared prerequisite) |
| substrate-2 typed hypergraph + the thin `T` field (the v0 to upgrade) | The **verified contract** (STANDARD-ARGUE/VERIFY) — curvature for aif2 + continuity for differentiable-code |
| Math bridges identified (Ollivier–Ricci/Wasserstein; Fisher–Rao latent; spectral embedding) | The formalised **escrow ledger** (ESCROW) + held-slice marks in each paired mission |
| `logic-model-before-code` for STANDARD-VERIFY | The cross-mission-adequacy ratification routine (governance, first use) |

**CONSTITUTION exit criterion:** every dependent mission's requirement-on-the-standard is recorded (✓ above) and governance is named (✓). → ESCROW next.

---

## 3. ESCROW  *(register the held dependencies; 2026-05-31)*

The authoritative escrow ledger. Each entry is `:held` until STANDARD-VERIFY freezes the contract (`→ :contract-released`, build-to-spec) and RUN/DELIVER ships the metric (`→ :satisfied`, consume-live).

| # | from-mission | on | requirement | status |
|---|---|---|---|---|
| **E1** | M-aif2 | M-substrate-metric (the metric contract) | curvature (Ollivier–Ricci) as the tension-proposer's signal — replaces signed-grad-vs-`(1−T)` | `:contract-released` (SV 2026-06-01) |
| **E2** | M-differentiable-code | M-substrate-metric (the metric contract) | continuous code-graph embedding (the band a gradient ranges over) **+ node-granularity resolution** | `:contract-released` (SV 2026-06-01) |
| **E3** | M-categorical-code (**LATE JOINER 2026-06-02**) | M-substrate-metric (the metric contract) | code-health diagnostics + Genotype: **(a)** Genotype = O1 node-identity ✓; **(b)** coupling ≈ O2 curvature + structural-entropy ✓; **(c)** structural-dynamics / damage-spread **self-sourced from the git/XTDB temporal substrate — NOT a metric obligation (no O5 needed)** | `:contract-released` (metric-side, at join 2026-06-02) |

**Release triggers:** `:held → :contract-released` on Campaign **STANDARD-VERIFY** (contract fixed; consumers may build to the verified spec); `:contract-released → :satisfied` on **RUN/DELIVER** (live metric consumed).

**E3 late-joiner note (2026-06-02) — no O5 needed:** E3 joined *after* STANDARD-VERIFY. Its metric requirements (a)+(b) are subsumed by the already-ratified **O1/O2** contract, so it enters `:contract-released` (metric-side) at join — no re-verification. Its distinctive requirement (c) — structural-dynamics / damage-spread — **does NOT extend the metric contract (no O5)**: per the M-categorical-code MAP (Q4), the dynamics already lives in the **git/XTDB temporal substrate** (change-propagation across commits), so it is self-sourced by M-categorical-code's own loop. This keeps the keystone metric static + simple. `:contract-released → :satisfied` on live consumption (code-health diagnostics computed on the delivered metric + the structure evolved over it).

**Held slices marked in the consuming missions (done 2026-05-31):**
- `M-aif2` header — INSTANTIATE slice 1 (the tension-proposer) marked `:held`; "do not build against the thin scalar field" noted.
- `M-differentiable-code` header — continuity requirement + node-granularity marked `:held`; "don't solve node-granularity unilaterally — keystone work now" noted.

**Shared-prerequisite note (from CONSTITUTION):** E2's node-granularity half is also a precondition of the metric itself — resolving it in the keystone discharges part of E2 *and* unblocks the metric. The keystone's MAP should treat node-granularity as its first sub-deliverable.

**Health telemetry:** 3 entries — E1, E2 `:contract-released` (SV 2026-06-01); **E3 `:contract-released`** (metric-side, late join 2026-06-02, no O5 required). RUN/DELIVER remains open until live metric consumption / proof-point gates are satisfied.

**ESCROW exit criterion:** every cross-mission dependency entered the ledger with a `:held` status, a named two-step release path, and a marker in the consuming mission (✓ all three). STANDARD-VERIFY has since ratified the contract and advanced both entries to `:contract-released`; RUN/DELIVER advances them to `:satisfied`.

---

## 4. STANDARD-ARGUE  *(the metric contract; authored 2026-06-01, Joe-ratified)*

The contract `M-substrate-metric` is built *against*. Converged via a claude-3 ⇄ claude-6
micro-whistle requirements salvo (2026-06-01); working copy + full provenance in
`C-substrate-completion.STANDARD-ARGUE.draft.md` (now superseded by this section). **Four
obligations**, each a STANDARD-VERIFY check *as a design, before anyone builds*:

### O1 · R-shared-nodes (keystone-owned identity — resolves the granularity tension)

The keystone fixes **one multi-resolution, typed node-identity set** over substrate-2,
**closed under {E1's relation-types, E2's relation-types}**. VERIFY checks against it:
**(id)** every declared grain/type present · **(E1)** every node carries a type `κ` can
dispatch on · **(E2)** every node is conditioning-normalized so `∂s/∂A` is well-scaled.
This dissolves the keystone-once-vs-falls-out-of-DERIVE tension by splitting node-**IDENTITY**
(shared, keystone-owned, resolved once) from node-**PROPERTY** (per-consumer clause):
`A[n,r,target]` *selects* a typed sub-lattice, it does not *create* identity — valid iff the
identity is multi-resolution and closed under the declared relations; otherwise a unilateral
subdivision = re-authoring identity = **escalate to Joe**. *(Schema landed: M-substrate-metric
§2 — node-type union + `feeds-μ?`/`feeds-A?` relation-use table.)*

### O2 · Two cuts of one object (certified independently)

One object (ground metric `d` / node embedding); two cuts VERIFY certifies separately —
**one does not imply the other** — over three axes: **order** (E1 zeroth/read-curvature · E2
first/`∂s/∂A`) × **latency** (E1 live/scan-cadence · E2 offline/batch-OK) × **clause** (E1
owned curvature sign/polarity · E2 conditioning). Discrete Ollivier–Ricci read and smooth
Laplace–Beltrami gradient are different regimes off the same `d`.

### O3 · R-E1 — curvature-as-tension *(owner claude-3)*

**(a) Owned polarity.** Sign/threshold owned by the **metric, not the consumer**; the
tension-proposer *reads* direction, never re-derives it. **Resolved form (M-substrate-metric
IDENTIFY, 2026-06-01):** *propose-here = geometric strain (curvature) ∧ unresolvedness*, where
`:resolution-state` is a **metric-owned node feature** (per-grain provider/rollup, or explicit
unknown/non-actionable). Curvature alone never fires — a complete node may be geometrically
interesting yet non-actionable. This retires M-aif2's Stage-B `signed-grad-vs-(1−T)` ambiguity.
**(b) Liveness.** Curvature answerable against current substrate-2 at WM-scan cadence, or under
a defined staleness/caching bound (Ollivier–Ricci is Wasserstein-per-edge = expensive).
**Adequacy:** (1) a complete node (e.g. `futonzero-capability`, the −9.8 mis-flag) must **not**
fire; a bottleneck must. (2) read within scan budget or declared staleness bound.

### O4 · R-E2 — continuity-as-differentiable-band *(owner claude-6)*

**(a)** Continuity query returns coords a gradient flows through; `∂s/∂A` finite; embedding is
a fixed observation the metric supplies (consumer optimizes only `A`). **(b)** Conditioning
owned by the metric (`∇` not dominated by any single node). **(c) Non-degeneracy:** the supplied
embedding must *discriminate* (text-BGE-grade spread), **not** the R-GCN arm that collapsed to
cosine ≈ 1.0 (`futon6/technote-arxiv-mining.md:14-30`). **O1-join:** the embedding must exist at
the grain E2 selects (symbol/boundary, not only namespace). **Adequacy:** conditioning check on
the 115k-ns-vs-1-line-`defn` extremes; cosine-spread floor on sampled pairs. Latency: offline OK.

**Joint-completion mapping (§1):** STANDARD-VERIFY passes when O1–O4 are each verified *as a
design*, fit for both consumers, before either builds; the Campaign dissolves toward
`:satisfied` once ≥1 paired requirement is released-and-consumed.

**STANDARD-VERIFY result:** PASS-on-design ratified by Joe on 2026-06-01, relayed by claude-3. O1-O4 are accepted as cross-mission adequate as a design; E1 and E2 escrow entries are `:contract-released`. Keystone v1 RUN/DELIVER proof points R1, R2, and R4 have since landed; remaining ratchet rungs are follow-on / consumer-facing.

---

## 5. RUN/DELIVER — the proof-point ratchet  *(authored 2026-06-01, claude-2)*

These are **not gates** (STANDARD-VERIFY is the gate, on the *design*). These are
post-hoc **proof points** for DELIVER: "show me it's working on something real."
A **ratchet** in the futon5 sense — each rung produces a durable artifact, and
rung N+1 cannot be honestly claimed without rung N's artifact in hand. Ordered so
the **cheap on-box rungs come first** and every heavy rung sits behind codex-3's
§2.9 memory gate (off-box/superpod or hard-sliced). Maps each rung to the
STANDARD-ARGUE obligation (O1–O4) it exercises.

| Rung | Proof point — "it works on THIS" | Pass criterion | Exercises | Posture |
|---|---|---|---|---|
| **R0** | structural curvature is meaningful *at all* | hop Ollivier–Ricci marks bridges (−κ) vs clusters (+κ) | O2 (read cut) | ✅ **DONE** — `futon6/resources/differentiable-math/ricci-tag-curvature.json` (math-tag proxy graph) |
| **R1** | curvature meaningful on **substrate-2's own** `feeds-μ?` graph (not the math proxy) | human recognizes top −κ edges as real cross-area bridges | O1 (shared nodes) + O2 | ✅ **DONE** — `holes/missions/M-substrate-metric.R1-report.md` |
| **R2** | curvature **+ polarity** fires "propose-here" correctly (the aif2 payload) | complete-but-central node (`futonzero-capability`, the −9.8 mis-flag) does **not** fire; open `:sorry` at a bridge **does** | O3 (curvature-as-tension; the Stage-B fix demonstrated) | ✅ **DONE** — `holes/missions/M-substrate-metric.R2-curvature-full-report.md` |
| **R3** | **convergence — is it actually ONE metric?** *(the load-bearing rung)* | hop-distance vs BGE-distance substantially agree on the bridge ranking; if they diverge, that IS the finding (two metrics wearing one name) | O2 (the "two cuts of one object" claim, tested not asserted) | **needs embeddings → off-box / hard-sliced (§2.9)** |
| **R3.5** | the **blend the charter named** | adding git **co-change** to dependency+semantic measurably improves the bridge ranking, or is shown redundant (real answer either way) | charter §"shared standard" (`d` = dependency + co-change + semantic) | cheap-ish (git-log mining), on-box |
| **R4** | the differentiable band is **sane** (claude-6's micro-test, in its right place) | `∂s/∂A` finite + well-conditioned across the symbol-size spread | O4 (a,b,c) | ✅ **DONE** — O4(b) PASS; `:conditioning-scale` audit/optional in v1 |
| **R5** | one gradient step proposes **one real structural edit**, optionally with E1 curvature shaping the loss | the edit-proposal is legible + defensible vs the wiring contract, OR its disagreement with the drawn prior is itself the signal (combining-methods-as-diagnostic); if curvature enters the loss, O3 polarity and `feeds-mu?` / `feeds-A?` separation remain intact | O4 → M-differentiable-code payload + O2 convergence pressure | off-box |
| **R6** | the demonstrator: the metric is **meaningful**, not just well-typed | `E-codebase-manifold` W₂-mass-flow animation over git history shows recognizable structural evolution (a wrong metric → meaningless animation) | whole-metric falsifier | off-box |

**Why a ratchet, not a checklist:** R0→R2 are cheap, on-box, and produce the
artifacts R3+ consume — progress is immediate and visible while the box is shared.
**R3 is the hinge** (does "one metric" survive contact with two distances?); every
heavy rung sits behind it *and* behind §2.9. You cannot hand-wave R3 without R1's
substrate-graph curvature file; cannot do R5 without R4's conditioned gradient.

**Status:** R0, R1, R2, and R4 are done. R3/R3.5 remain O2 convergence / blend
follow-ons; R5 remains the E2 edit-proposal payload and is the right home for
claude-2's loss-with-E1-curvature unification idea; R6 remains the
`E-codebase-manifold` demonstrator. **Ownership:** R1–R3 + R3.5 land naturally on
the keystone (codex-3) + E1 (claude-3); R4–R5 on E2 (claude-6); R6 on the
demonstrator. Keystone `M-substrate-metric` is DELIVERED(v1) with named residue;
the Campaign remains open until live consumer consumption advances escrow to
`:satisfied`.

## Next phases
- **STANDARD-VERIFY**: ratify O1–O4 as cross-mission-adequate *as a design* (Joe; logic-model-
  before-code at Campaign tier). On pass: E1/E2 escrow `:held → :contract-released`.
- **RUN/DELIVER**: M-substrate-metric ships the verified metric (climb the §5 ratchet); escrow `→ :satisfied`.
- **DISSOLUTION**: dissolve; the missions are the residue; this doc is the closure record.

---

## 6. ROUND-UP (2026-06-02, claude-3) — what the descriptive turn unlocked

Two sibling excursions — `futon5a/holes/excursions/E-half-mil-audit` (claude-3, *horizontal*) and `E-the-dark-tower` (claude-6, *vertical*) — plus a pre-registered investigation thread (`futon5a/holes/tech-notes/TN-misfit-to-self-description`) have considerably re-grounded this Campaign. Round-up, additive — **changes no obligation, escrow state, or the dissolution criterion.**

**(a) The re-anchor: consumer value is DESCRIPTIVE, not predictive.** Convergence note (claude-3 + claude-6, signed): every *predictive* projection off the metric reduced to a classical baseline (symbol→keyword, code-edges→common-neighbours, categorical-diagnostics→churn; M-categorical-code VERIFY: churn AP 0.81 > coupling 0.70). The metric's consumer value is **describing structure** (manifold morphology, intentional provenance, where intent bends) — gated by a co-occurrence baseline from the start. This sharpens, not weakens, the charter: the consumers (E1/E2/E3) re-aim from forecasting to description.

**(b) Horizontal — the demonstrator is grounded (R6).** `E-half-mil-audit` surveyed *what is* across the 500k-LOC stack via git: the manifold **grows at its edges** (`corr(node-age, centroid-distance)=+0.48`) and **accretes into a few interior basins** (16 attractor hubs in a 108-file fat tail = 5% of files / 29% of LOC). This is `E-codebase-manifold` (R6 demonstrator) made concrete — the morphology a "wrong metric" would fail to reproduce.

**(c) Vertical — the metric IS the first rungs of one tower.** `E-the-dark-tower`: state → change → change-of-change is *one* tower (differential ≅ I-Ching iching/iiching ≅ higher-categorical). substrate-2's `(T,∇,Δ)` and **this Campaign's contract are the tower's first three rungs**: O1 identity = level 0 (state), O2/E2 continuity-gradient = level 1, O2/E1 Ollivier–Ricci curvature = level 2. The Campaign delivered the bottom of the tower without naming it; the read is *climb, don't flatten* (every predictive flattening lost to baselines).

**(d) The misfit thread (pre-registered generator→evaluator) cleared the baseline wall — descriptively.** New laws, each baseline-gated: **level↔span** (corrections/curvature are spatially tight, refactors global), **centrality↔conservatism** (central namespaces edited often but gently), **pheromone↔basin** (the files most mission-referenced ARE the git-accreted basins — the self-description and the accretion are the same hubs). And a positive curvature result that does **not** reduce to the dependency graph (below).

**(e) Concrete substrate unlock — a new E1/O3 ground structure, born without turn→code.** The mission corpus self-describes the build densely (140/172 missions name files; 1182 `mission→file` edges). Ollivier–Ricci on the file-co-mission graph yields interpretable cross-concern **intent-bridges** — and **all 25 top bridges are absent from the `fdep` dependency graph** (some within futon3c's own domain): mission-intent structure the dependency graph cannot see. This landed as a ratified substrate relation:

- **`:mission/mentions-file` / `:mentions/stated`** — implemented by codex-3 (**commit `325315f`**, `codex/m-substrate-metric-runtime`), promoting the pre-existing `code/v05/file→mission` projection (no new parser). **O1-safe edge-only** (mission + file are existing nodes); `feeds-mu? true`, `feeds-A? false`; `:mentions/realized` reserved for a deferred git-co-temporal complement (the Pareto-20%).
- **E1-side `μ_x` spec (claude-3):** the relation enters the curvature **measure** as a file→file co-mention projection with **Newman weighting** `w(f,g)=Σ 1/(|files(m)|−1)` (down-weights broad missions/hub files), mixed by `β` (default 0.5; **`β=0` recovers pre-mentions curvature exactly** — a reversible extension whose ablation is the built-in baseline test); `d_E1=hop` denominator unchanged, so κ reads *"is intent-coupling tighter than dependency-distance?"*.
- **Status:** emitter landed; **ingest materialization in progress** (Joe-authorized); then codex-3 wires `μ_x`, claude-3 verifies curvature on materialized edges (β-ablation). Handoff + full spec: `futon3c/holes/CODEX-HANDOFF-mission-file-substrate-edge.md`; thread + rungs: `TN-misfit-to-self-description.md`.

**Implications for the ratchet (not yet formal rungs):** (i) a candidate **new proof-point** — intent-curvature on `:mission/mentions-file`, baseline-gated by the β-ablation (a descriptive complement to R0–R2's `feeds-μ?` curvature); (ii) **R5/R6 re-aim** onto descriptive value (manifold + intent-provenance) rather than predictive edit-forecasting. **Unchanged:** E1/E2/E3 escrow remain `:contract-released`; dissolution still awaits ≥1 paired requirement consumed **live** (the `μ_x` consumption of materialized `mentions-file` edges is a concrete path toward E1 `:satisfied`, not yet reached).

---

## 7. The substrate itself — what's ingested, the mission→code correspondence, the open JAX question (2026-06-02 round-up)

We have discussed many pieces (Malli signatures, embedding actual code, the few fat files, git-historical prediction) but **do not yet have an end-to-end plan** from code-ingest → JAX → useful output. This rounds up the substrate's actual state and the open question, in light of the stack work and claude-6's research footing (`E-the-dark-tower-2`).

### 7.1 What substrate-2 actually ingests now (code side)
- **O1 identities:** `:file / :namespace / :symbol / :boundary`.
- **Code-embedding cache:** `code-emb.npy` (7534×1024 BGE) = **7111 symbols + 423 namespaces** (living-Clojure, origin-excluded).
- **`feeds-A` graph:** `relations.json` — 7084 ownership + 852 file-dependency edges.
- **The metric:** E1 Ollivier–Ricci curvature (hop) + the **mention-curvature** channel (`feeds-μ`).
- **NEW — `:mission/mentions-file`:** mission→file edges from the mission-corpus self-description (`feeds-μ`).

### 7.2 The mission→code correspondence now available
`mission→file` (landed, **stated/explicit**) ∘ `file→namespace→symbol` (the code ingest) = **mission→actual-code**, traceable at namespace/symbol grain — "which code a mission touches" is now answerable from the substrate. *Honesty:* `mission→file` is the *stated* mention map; `file→symbol` is the code ingest; **`turn→code` remains git-only**, and **`turn→mission` is conjectural** (no clock-in — `E-the-dark-tower-2` §7) and mission cross-refs aren't yet ingested as queryable structure.

### 7.3 The open question — does the code-side ingest give JAX anything useful?
Honest session evidence:
- **Predictive:** symbol-name embeddings → keyword; code-edges → common-neighbours; the differentiable apparatus reduced to baselines. The *current* representation (symbol names + BGE + edges) gives JAX little **predictive** traction.
- **Descriptive:** curvature (intent-bridges, mention-curvature) yields real non-baseline structure. JAX-as-survey-instrument works; **JAX-as-forecaster doesn't** — on this representation.

So *today*: the code ingest gives JAX **descriptive** traction, not predictive. The open question is whether a **richer representation** flips that.

### 7.4 Candidate richer representations (the menu — none tested end-to-end into a beats-baseline loss)
- **Malli signatures** — typed structural shape, richer than name-dominated symbols; the natural retry where the *symbol-name* arm was the expected null.
- **Embedding actual code** (bodies/scopes), not just symbol names.
- **The fat-files focus** — only 108 files (29% of LOC) are large; the 95% small files are already at grain, so JAX effort concentrates on the attractor basins, the rest is ~uniform.
- **Git-historical patterns** — the temporal/evolutionary **ground truth** (predict real edge-additions; node-arrival growth; the PhD-Ch.6 method; the deferred `:mentions/realized` git-co-temporal complement). The one strand with a genuine prediction target ("what actually happened").

### 7.5 Theoretical footing (claude-6, `E-the-dark-tower-2` §8–9) — what "differentiable" can even mean here
The deep-research survey **relocates** the differentiable ambition (and corrects v1's over-identification):
- **Substrate → Poly (polynomial functors):** evolving/reconfigurable **coalgebras = "grows at the edges"**; the recommended substrate footing (over the BV/Caus causal-typing layer).
- **First-order differentiation (gradient / free-energy) *composes* with the substrate** via Smithe's categorical active inference on Poly — landed, first order (the WM/VFE side).
- **The higher-order curvature tower (tangent categories) is rigorous but its bridge to Poly is an OPEN PROBLEM** — a place futon could *contribute*, not consume.

Consequence for JAX-on-substrate-2: a **first-order** differentiable loss has a categorical home; the **2nd-order / curvature** ambition is research-open. This *matches the empirics*: first-order edit-proposals ran; curvature is descriptive; predictive-via-gradient never beat baselines.

### 7.6 Honest conclusion — no end-to-end plan yet; deep-research-gated
We have a populated substrate (identities + embedding + edges + `mission→file` + curvature), a clear **descriptive win**, a clear **predictive null** on the current representation, a **menu of richer representations** untested end-to-end, and a **footing** (Poly + first-order-diff landed, curvature↔Poly open). We do **not** have a code-ingest → JAX → beats-baseline solution. The honest read: a real *predictive* solution likely needs **(a)** a richer code representation (Malli / code-bodies / the git-temporal target) **and (b)** progress on the higher-order-tangent↔Poly gap (or a deliberate stay-at-first-order). Until then the substrate's delivered value is **descriptive** (manifold / provenance / curvature), and the predictive ambition is a **deep-research front** (the dark-tower duology + the differential-strand reframe), not yet a buildable plan. Recorded to consolidate the substrate's state and make the open question explicit.
