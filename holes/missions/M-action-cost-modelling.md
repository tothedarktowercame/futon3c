# Mission: Action Cost/Benefit Modelling for the WM

**Type:** Mission (M-prefix; lifecycle HEAD → IDENTIFY → MAP → DERIVE → ARGUE → VERIFY complete; retitled from E- to M- by Joe 2026-05-26 once DERIVE surfaced that this is full-fledged-mission scope, not a side excursion).
**Status:** HEAD / IDENTIFY / MAP / DERIVE / ARGUE / VERIFY all drafted and operator-ratified through 2026-05-27. VERIFY's 10 carried-forward tensions: 4 done (T1, T5, T8, T10), 4 held for downstream INSTANTIATE (T2, T4, T6, T9 — all blocked on `E-substrate-2-sorry-typing.md`'s own INSTANTIATE which is a future cycle), 2 future-Joe-triggered (T3, T7). Mission ready for INSTANTIATE when Joe ratifies.
**Date:** 2026-05-26
**Author + end-to-end owner:** claude-1 (inhabiting `:war-machine-pilot`, emacs-repl with Joe).
**Replaces:** `futon3c/holes/missions/HEAD-wm-action-cost-prioritisation.md` (same content under §HEAD below).
**Renamed-from:** `E-action-cost-modelling.md` (excursion) → `M-action-cost-modelling.md` (mission), 2026-05-26 after §3.8 surfaced the aliveness-synthesis scope.

---

## HEAD (preserved from the operator-shape intake, 2026-05-26)

**Trigger:** Joe's emacs-repl finding 2026-05-26, after the live tied-bucket display fix (commit `713c74d` futon3c) surfaced the WM's actual indifference among 5 sibling sorries: *"until we have some strategy for doing cost (and benefit) aware prioritisation, it's somewhat premature to greenlight new work. I think we are at the stage of writing a HEAD for the Excursion."*

### The question

The WM's R5 augmented EFE math is **correct** — every term in `G-total = G-risk + G-ambiguity − 0.4·G-info + 1.2·G-survival` matches the design. But the **action-representation** is too coarse to differentiate between cheap and expensive actions of the same family. Today's evidence: 5 `address-sorry` actions tied at G=-4.4228 because the forward-model treats every sorry as "+1 sorry-count-norm reduction," regardless of whether the sorry is closeable in 5 minutes (hand-edit reclassification) or several months (entity-tag classifier + likelihood predictors gated on M-INC step (b)).

Question to answer **before** designing any cost/benefit term:

> **Do we already have, somewhere in the futon stack, the conceptual machinery for cost/benefit-aware action prioritisation? And if yes — does that machinery's geometry fit naturally into the WM's EFE math, or would adding it require something the geometric stack doesn't yet support?**

### Prior art consulted (HEAD-stage)

- **`~/vsat.wiki/analysis-demo/`** — Bayesian scenario comparison in a value-chain domain. Parametric-assumption pattern + explicit "compare under varying parameters" idiom. *What to lift*: parametric-comparison pattern. *What NOT to lift*: the specific value-network model.
- **`futon0/holes/missions/M-futonzero-capability.md`** — Sen's capability approach. The right question for action prioritisation isn't *"which sorry reduces sorry-count-norm by 1?"* — every sorry does — but *"which sorry's closure unlocks new capability?"* *What to lift*: capability-delta as the benefit dimension. *What NOT to lift*: full FutonZero agent-monitoring machinery.
- **ARGUE phase of every futonic mission** — Every mission already articulates "given what we know, why is this build necessary?" — this IS a cost/benefit signal. *What to lift*: existing ARGUE statements as already-authored signals; wiring not authoring. *What NOT to lift*: ARGUE-formalism for short-form registry sorries.
- **`futon3/holes/missions/M-live-geometric-stack.md`** — substrate-2 with ~360k hyperedges; "tension is conserved"; T-delta per action would be the natural cost/benefit metric. *What to lift*: T-delta as the geometric cost/benefit signal. *What this requires*: a query path from the WM's forward-model into the substrate-2 hypergraph.

### Design space (sketch — not commitments)

| Shape | Where the cost/benefit number comes from | Maturity | Cost to build |
|---|---|---|---|
| **(a) Per-action `:intrinsic-cost`** | Hand-curated cost annotation per action class | Lowest — analogous to today's `:intrinsic-value 0.1` field | Cheap; arbitrary; doesn't generalise |
| **(b) ARGUE-derived weight** | Parse existing mission/sorry ARGUE statements; project into a benefit-vector | Medium — pattern-mining over substrate Joe already wrote | Moderate; depends on ARGUE-uniformity per artefact |
| **(c) T-delta from substrate-2** | Query M-live-geometric-stack for action's predicted T-redistribution | Highest — geometrically principled; capability-aware | Highest if substrate doesn't yet expose action-projection queries |

### §HEAD's Step 1 outcome — hair-split test result (live experiment, 2026-05-26)

The 5 sorries currently tied at G=-4.4228 in the WM's ranked-actions:

| Sorry | Estimated cost | Benefit unlocked | Notes |
|---|---|---|---|
| `r3a-likelihood-coupling-density` | **HIGH** — needs edge-entity classification (new substrate; multi-cycle work, analogous to today's support-coverage cycle) | R3 coverage +1 channel (toward 7/14) | Rationale explicitly says "if edge-entities exist; currently doesn't" — prerequisite-gated |
| `r3a-likelihood-ticks-firing-ratio` | **HIGH** — needs tick-entity-typing (new substrate; multi-cycle) | R3 coverage +1 channel | Same shape as coupling-density |
| `r3d-per-entity-attribution` | **VERY HIGH** — blocked on M-INC step (b) prerequisite (real per-entity event streams; months-scale upstream) | R3d → "per-entity Bayesian attribution honest" — major correctness upgrade for R3 wiring | Largest cost; largest benefit; longest prerequisite chain |
| `stub-lifts-pending-aif-edn` | **MEDIUM** — 32 stories × hand-author `.aif.edn` files OR ship a generator | Drains the story-decomposition queue; substrate completion at registration→decomposition layer | Substrate-completion benefit, not R-criteria advancement |
| `wm-ui-hud-mode-rationale-hardcode` | **LOW** — bounded refactor (~1hr); two well-defined v2 paths (dynamic EDN read OR vocab→cljs codegen) | Removes small known drift surface between vocab + UI | Smallest cost; smallest benefit; cleanest "anyone can do this" task |

**Spread**: at least 2 orders of magnitude in cost (1hr → months); 4 distinct benefit categories.

**Verdict**: hair-split branch **FAILS**. These 5 sorries are not equally-cost or equally-benefit. The WM's coarse forward-model is honestly reporting its indifference, but the indifference is an artifact of the model not encoding the obvious asymmetry the operator can read in 5 minutes by skimming rationales.

### HEAD exit criteria (operator-verified)

- [x] Operator-voice anchor present (Joe's "premature to greenlight new work" quote)
- [x] What's already felt to be true: WM math is correct; the gap is in action-representation, not the math
- [x] Anti-glibness discipline: "the math is broken" framing rejected; the math is doing what it says it does
- [x] Working-economy position: WM cannot be fit for pilot inhabitation if it presents minute-scale and months-scale work as equivalent
- [x] Carried-forward tensions named (the YES/NO branch on M-live-geometric-stack)
- [x] Provenance recorded (HEAD authored claude-1 2026-05-26 after `713c74d`)

---

## 1. IDENTIFY (drafted 2026-05-26 per operator-directed transition)

### Motivation — the gap

We are not 100% confident in two adjacent claims:

- **(a)** that the WM's current AIF scores produce a *meaningful* prioritisation signal for the operator/pilot, beyond their formal correctness;
- **(b)** that we know how those AIF scores *relate* to the differential-geometry concepts from `M-live-geometric-stack.md` (T-distribution, tangent bundle, conservation of tension under pattern-application).

This pair of unresolved claims is a **meaningful** gap, not a stylistic one, for one concrete reason: **the WM currently presents minute-scale and months-scale work at the same "next move" level**. Today's live test case made this visible — the 1-hour HUD-tooltip refactor and the months-scale `r3d-per-entity-attribution` blocked on M-INC step (b) both surface with `G=-4.4228`. As Joe put it: *"months-scale work almost certainly needs a preliminary 'work breakdown' step (indeed, possibly needing a Mission structure of its own, or in some cases a novel and even bigger structure like a Campaign!)"*

The gap therefore decomposes into three nested questions:

1. **Cost-signal gap**: How should the WM annotate each candidate action with a scale (minute / hour / session / multi-session / mission / campaign)?
2. **Benefit-signal gap**: How should the WM annotate each candidate action with a capability-delta (what does closing this unlock)?
3. **Work-breakdown gap**: When the WM's recommended action is multi-session or larger, what mechanism converts the action into a tractable sequence of pilot cycles? Is a Mission enough, or does the futon stack need a new structural level — a **Campaign** — for the very largest items?

### Theoretical anchoring

- **AIF EFE (R5 augmented)** — formally correct; coarse forward-model is the problem locus.
- **Sen's capability approach** (per `M-futonzero-capability.md`) — capability-delta as the benefit dimension. Every sorry's closure unlocks some capability; the WM's model doesn't represent which.
- **Differential geometry / T-distribution** (per `M-live-geometric-stack.md`) — T-delta per action would be the natural geometric cost/benefit signal IF substrate-2 exposes the action-projection queries; the HEAD's §Decision tree turns on whether it does.
- **ARGUE-phase rationale** (per `futon3b/AGENTS.md` and every futonic mission) — *every mission already articulates its own cost/benefit through its ARGUE statement*. Those statements are unread by the WM today; reading them is wiring, not new authoring.
- **Bayesian scenario comparison** (~/vsat.wiki/analysis-demo/) — the parametric-comparison idiom transfers to per-action cost/benefit annotation regardless of the substrate.
- **Tentative novel structural element — Campaign** — Joe-introduced 2026-05-26 as the candidate name for "an organising structure above Mission, for months-scale efforts comprising multiple Missions." Not yet a defined entity in the futon stack; IDENTIFY treats this as a candidate invariant whose existence is itself part of the gap.
- **Aliveness synthesis** (added after DERIVE §3.8 / [[project_aliveness_synthesis]]) — Christopher Alexander's *The Nature of Order* (Book 1: *The Phenomenon of Life*, the 15 fundamental properties); Nikos Salingaros' geometric mathematisation (scale-hierarchy, topological coherence of wholeness, *A Theory of Architecture*).  Under this framing, **Mana** = positive aliveness-quantity (Alexander); **Anamnesis** = the inverse (the currently-implemented "mana"-snapshot file-tension pressure; Greek ἀνάμνησις, "recollection of pending tension").  AIF EFE, T-distribution, EOI strength, anamnesis, and mana are all **projections of one underlying aliveness-quantity** — currently treated as parallel readouts by the futon stack.  The mission's reconciliation problem (geometry ↔ AIF) dissolves under this framing because the readouts are already the same quantity in different surfaces; the work is recognising the unification and making them consume one substrate.  This anchoring re-shapes the work from "extend AIF with a cost term" into "type the aliveness-projection at the sorry-scale (and beyond) so the WM consumes geometric ground-truth rather than parallel-reality readouts."

### Scope in

- Designing and shipping the **work-breakdown layer** between WM action recommendations and actual pilot cycles
- Annotating candidate actions with a `:scale` field (proposed values: `:minute :hour :session :multi-session :mission :campaign`) — open question whether to source from operator-curated EDN, from ARGUE-mined text, or from substrate-2 T-delta
- Defining what a **Campaign** is (or proving it isn't needed because Missions suffice for everything months-scale) — shape-first IDENTIFY may apply here
- The 5 currently-tied sorries are the **live test case** for the work-breakdown layer: at minimum, the `:minute`-scale one (`wm-ui-hud-mode-rationale-hardcode`) should rank above the `:campaign`-scale one (`r3d-per-entity-attribution` gated on M-INC step (b)) after the layer ships

### Scope out

- Re-deriving the WM EFE math (it's correct)
- Reweighting existing channel preferences (a different excursion's territory — see E-wm-metric-redesign for that shape)
- Implementing a Campaign UI / runtime / lifecycle manager (premature; the structural definition must exist before management can)
- Replacing operator judgement on cost/benefit (the work-breakdown layer aids judgement, doesn't replace it)
- Automated work-breakdown ("decompose this multi-month thing into pilot cycles for me") — that's its own deep problem; this excursion only asks the WM to *recognise that decomposition is needed*

### Completion criteria

- [ ] **Cost-signal lands**: Each candidate action in the WM's ranked-actions carries a `:scale` (or equivalently expressive) annotation, sourceable from at least one of shape (a)/(b)/(c)
- [ ] **Work-breakdown gate fires**: When the WM's top recommendation is `:multi-session` or larger, the live next-move-live tile renders a "needs work breakdown" CTA instead of "→ address-sorry X"
- [ ] **Mission-or-Campaign decision is operator-visible**: The CTA either points to "this is a Mission" (existing futonic structure) or "this requires a Campaign" (new structure if we confirm it's needed; or the absence is operator-explicit)
- [ ] **The live test case discriminates**: After the layer ships, the 5 currently-tied sorries no longer present as equivalent. At minimum, `:minute`/`:hour` ranks above `:campaign` regardless of EFE score
- [ ] **M-live-geometric-stack decision recorded**: Either substrate-2 exposes per-action T-delta queries (shape (c) lands), OR a finding is filed on M-live-geometric-stack identifying the action-projection-query gap, with the 5-tied-sorries 2-orders-of-magnitude-cost-spread as the test case
- [ ] **Campaign structure resolved**: Either ships as a new futon-stack structural element with documentation in `mission-lifecycle.md` (or its sibling), OR is explicitly rejected as "Missions suffice" with rationale

### Relationship to other missions

- **Consumed by** `M-war-machine-pilot.md` — the pilot's prioritisation problem is exactly what this excursion solves
- **Depends on read pass into** `M-live-geometric-stack.md` — the shape-(c) decision hinges on its current capability
- **Sibling of** `E-wm-metric-redesign.md` — that excursion is about metric *shape* (saturation, log-scale); this excursion is about metric *coverage* (cost dimension not represented at all). They live in adjacent corners of the same problem
- **Potentially births** a new mission or campaign on Campaign-as-structural-element if shape-first IDENTIFY surfaces it as a non-trivial concept
- **Test-case-of** `[[project_tickle_as_operator_model]]` — Tickle's focus-direct mode would specifically benefit from `:scale` annotations (don't poke the pilot to "do" a multi-session thing; instead point at the work-breakdown CTA)

### Source material

- §HEAD of this file (preserved above)
- Today's commit chain: `30bbc89` (futon2 C2), `c71dc42` (futon2 C3+C4), `713c74d` (futon3c tied-bucket display)
- `~/vsat.wiki/analysis-demo/` — parametric assumption pattern
- `futon0/holes/missions/M-futonzero-capability.md` — Sen capability framing
- `futon3/holes/missions/M-live-geometric-stack.md` — geometric T-delta framing
- Every futonic mission's §ARGUE — already-authored cost/benefit signals
- `mission-lifecycle.md` — where any Campaign structural element would dock
- `futon3c/holes/missions/E-wm-metric-redesign.md` — sibling excursion
- `[[project_war_machine_as_tm_on_manifold]]` — WM-as-TM-on-manifold framing
- `[[project_piano_roll_vs_live_stack]]` — live-stack distinction; cost/benefit prioritisation requires LIVE manifold queries

### Owner and dependencies

- **Owner**: claude-1 / Joe paired (operator-paced); this excursion explicitly NOT autorunner-eligible at IDENTIFY (judgement-heavy)
- **Repos involved**: futon3c (WM logic + this doc), futon2 (judge), futon3 (M-live-geometric-stack), possibly substrate-2 (futon1a)
- **Hard dependencies**: read pass on `M-live-geometric-stack.md`'s current INSTANTIATE/DOCUMENT state to settle the shape-(b)-vs-(c) decision
- **Soft dependencies**: Tickle (`[[project_tickle_as_operator_model]]`) integration — the `:scale` annotation is what Tickle's focus-direct mode would consume

### Carried-forward tensions (from HEAD; live for MAP)

1. Is `M-live-geometric-stack`'s substrate-2 ALREADY queryable for per-action T-delta? (HEAD §Decision tree unresolved; MAP phase to confirm by reading the mission's current state)
2. Does the **Campaign** structure deserve to exist as a futon-stack element, or is it just "a Mission that needs sub-Missions"? (Shape-first IDENTIFY question; MAP enumerates at least 2-3 plausible Campaign instances or records `:special-case true`)
3. How does the work-breakdown layer interact with `[[project_claude_autorunner]]`? — months-scale work CANNOT use autorunner; the `:scale` annotation may double as autorunner-gate (`autorunner-off-if-scale-above :session`)
4. The pilot's existing `:hop-trigger` (E-pilot-hop-trigger-wiring) hops into peripherals that EXIST. No peripheral exists for "work breakdown" — does one need to? Or is work-breakdown best handled as a substrate edit (author a new mission file) without a dedicated peripheral?
5. The hair-split outcome (HEAD §Step 1) showed an operator can produce a useful `:scale` annotation in 5 minutes by skimming rationales. That micro-instance IS shape (b) run by hand. **Open question**: how much of the work-breakdown layer is just automating the operator's 5-minute skim?

### IDENTIFY exit criterion

A human (Joe) reads §IDENTIFY and agrees:
- The gap is real (not a stylistic complaint about the WM)
- The scope is right (work-breakdown layer + Campaign-structure question; NOT EFE reweighting)
- The completion criteria are testable (the 5-tied-sorries live test case is the canonical discriminator)
- The carried-forward tensions are honest (not pretending shape-(c) is settled when the M-live-geometric-stack consultation hasn't happened)

---

## 2. MAP (drafted 2026-05-26 with the 5 tied sorries as working set)

### Operator framing (Joe, emacs-repl 2026-05-26, verbatim)

> *"Let's move it forward, taking the '5 tied options' as our working set — how do we prioritise them? Do they need further breakdown? Are we getting real signals from AIF or from the geometry? Are we duplicating work (geometry + AIF saying different things that aren't reconciled)? Basically the gap is real until such time as I can click on 'G=-4.418' and get a meaningful trace of the computation — though of course if the computation was meaningful we'd be unlikely to get a five-way tie."*

**Elevated finding (Joe's last sentence) — the tie IS the evidence**:

If the WM's EFE were a meaningful prioritisation signal, an exact 5-way tie across heterogeneous actions would be vanishingly unlikely. The fact that the tie exists is itself the falsification: the model is too coarse to distinguish actions whose cost spans 2 orders of magnitude. This recasts the entire excursion: we're not adding cost-awareness because the math is wrong — we're adding cost-awareness because the tie is a *symptom of model-coverage incompleteness* that MAP can now name precisely.

### Inventory: signal sources for action prioritisation

| Source | Status | Where it lives | Cost to consult |
|---|---|---|---|
| **AIF EFE** (R5 augmented) | ACTIVE; coarse; visible in WM | `futon2.report.war-machine/judge`; `futon2.aif.policy` | O(1) (already on screen) |
| **Geometric T-delta** (substrate-2) | NOT WIRED into WM; substrate is operationally live (~360k hyperedges) per `M-live-geometric-stack.md` INSTANTIATE phase | `futon1a` substrate; `M-live-geometric-stack.md` describes the manifold; action-projection queries are an open question (HEAD §Decision tree) | UNKNOWN — depends on query-path existence |
| **Operator skim** | ACTIVE; high-signal; low-bandwidth | Joe (or pilot agent reading rationales by hand) | ~5 min / 5 actions per skim |
| **ARGUE-phase rationale** | EXISTS PER-MISSION; unread by WM | Every `M-*.md` file's §ARGUE | O(N) over missions; pattern-mining feasible |

Concrete finding from this inventory: **the operator's 5-minute skim IS the prioritisation signal currently in use.** Shape (a)/(b)/(c) from the §HEAD design-space sketch are all attempts to automate or geometrise what the operator already does live. This makes Q5 from §IDENTIFY's carried-forward tensions sharper: **how much of the work-breakdown layer is just automating the operator's 5-minute skim?** Probably most of it.

### Ready / missing table

| Capability | Ready | Missing |
|---|---|---|
| WM produces ranked-actions with G-decomposition (G-risk, G-amb, G-info, G-survival per action) | ✓ (`futon2.report.war-machine/judge` line ~3105 emits all 4 terms; verified live this session) | — |
| Live tile renders tied-bucket when WM has no preference | ✓ (commit `713c74d`, this session) | — |
| Click on G-total → trace of computation | ✗ | UI affordance + per-term provenance |
| `:scale` annotation on actions | ✗ | source-of-truth decision + emission path |
| substrate-2 action-projection queries (geometric T-delta per action) | ✗ | M-live-geometric-stack consultation pending |
| ARGUE-mined benefit weights per mission/sorry | ✗ | pattern-mining pass over `M-*.md` and `sorrys.edn` |
| Operator-curated scale-by-id EDN | ✗ | smallest-viable substrate-edit (per-id `:scale` field) |
| Work-breakdown CTA when scale > `:session` | ✗ | cljs tile addition |

The ready column says: **the data infrastructure for the prioritisation trace mostly exists** (G-decomposition is already emitted; the live tile already renders structured data). The missing column says: **the affordances and the cost/benefit dimensions are absent**.

### Q1: How do we prioritise the 5 tied options?

By the operator skim already conducted in §HEAD's Step 1, with one additional dimension (leverage = the system's downstream payoff per unit of work):

| Rank | Sorry | Scale | Cost | Benefit | Leverage |
|---|---|---|---|---|---|
| 1 | `wm-ui-hud-mode-rationale-hardcode` | `:hour` | LOW | LOW | Decent — small drift surface closed; canonical "low-hanging fruit" |
| 2 | `r3a-likelihood-coupling-density` | `:multi-session` | HIGH | MEDIUM | High — same shape as today's support-coverage; pattern proven; +1 R3 channel |
| 3 | `r3a-likelihood-ticks-firing-ratio` | `:multi-session` | HIGH | MEDIUM | High — same as coupling-density (tick-entity-typing analogous to edge-entity classification) |
| 4 | `stub-lifts-pending-aif-edn` | `:multi-session` | MEDIUM | MEDIUM | Moderate — substrate completion; not R-criteria leverage |
| 5 | `r3d-per-entity-attribution` | `:campaign` | VERY HIGH | VERY HIGH | UNCERTAIN — blocked on M-INC step (b); leverage is "huge once unblocked," but unblocking is itself a Campaign-scale piece of work |

**Operator-judgement prioritisation**: this is decided by reading rationales, not by the WM. The WM's tie at G=-4.4228 dissolves once `:scale` is in scope: `:hour` < `:multi-session` < `:campaign`, regardless of EFE.

### Q2: Do they need further breakdown?

| Sorry | Breakdown shape |
|---|---|
| `wm-ui-hud-mode-rationale-hardcode` | **NO** — single cycle, 2 well-specified v2 paths, choose one |
| `r3a-likelihood-coupling-density` | **YES** — Excursion-shaped, same 4-cycle subplan as E-support-coverage (design tag schema, implement edge-entity classifier, implement likelihood, verify live shift) |
| `r3a-likelihood-ticks-firing-ratio` | **YES** — Excursion-shaped, parallel to coupling-density |
| `stub-lifts-pending-aif-edn` | **YES** — Excursion-shaped, but with 32 internal cycles (one per story) — or a single generator cycle that handles all 32 mechanically |
| `r3d-per-entity-attribution` | **YES — Mission or Campaign** — blocked on M-INC step (b). The breakdown has at least 3 sub-units: (i) M-INC step (b) itself (real per-entity event streams), (ii) downstream R3d rewiring to consume per-entity events, (iii) verification against R3 multi-channel sign-aggregation. Three Missions clustered → Campaign-candidate |

**Shape-first IDENTIFY check on Campaign**: with `r3d-per-entity-attribution` as one candidate Campaign instance, we'd need at least 2-3 plausible siblings to call this a real shape. Other candidates from a quick scan:
- `M-the-futon-stack` itself (multi-quarter; multiple sub-missions already authored as M-the-futon-stack-Q6-r12-design-choices etc.)
- The hypothetical "self-representing-stack at full coverage" (substrate-2 → substrate-3 evolution)

So Campaign-as-shape has at least 3 plausible instances. Per `mission-lifecycle.md` §"shape-first IDENTIFY", this clears the threshold for adopting the namespace; it's not `:special-case true`. Campaign DOES deserve to exist as a structural element. The remaining definitional work belongs in MAP/DERIVE.

### Q3: Are we getting real signals from AIF or from geometry?

| Signal | Status | What it currently tells us about the 5 tied sorries |
|---|---|---|
| **AIF EFE** | Active; correct math; coarse | "All 5 produce predicted-observation = `sorry-count-norm` decreases by 1. G-totals all = -4.4228. WM has no preference." |
| **Geometric T-delta** (substrate-2) | NOT WIRED | Hypothetical: each sorry has a SPECIFIC T-redistribution profile across the hypergraph (UI sorry → small T-delta on UI subgraph; r3d-per-entity → large T-delta across entire belief domain). Would discriminate the 5 cleanly. |
| **Operator skim** | Active; what produced the prioritisation in Q1 | The 5-minute read of rationales DOES produce a meaningful ranking — but it's not the WM's signal. |

**Conclusion on Q3**: the WM is getting REAL signals from AIF (the EFE values are honest); it's NOT getting signals from geometry (the substrate-2 path doesn't exist yet); the operator is providing what the WM cannot. The "real signal vs noise" distinction: AIF is signal at its level of resolution, but the resolution is too coarse for the operator's question.

### Q4: Are we duplicating work? (Geometry + AIF saying different things that aren't reconciled?)

**Operator correction 2026-05-26 (Joe, emacs-repl)**: *"we can't afford to push Reconciliation of AIF + geometry to another mission — if we don't deal with it now, the geometry remains dead from the point of view of the War Machine, which is just silly."* This collapses what was a "future-tense risk" into a **DERIVE-phase mandatory output**.

The reason the correction stands: any shape-(a)/(b) cost annotation that ships WITHOUT a geometric-reconciliation path becomes a parallel reality alongside AIF — manual cost numbers AND predictive-coding G-totals, both consulted, both unreconciled, both unwired to the geometric ground-truth that substrate-2 represents. The "duplication threat" is then no longer hypothetical: it's the design itself, by inaction.

Current state restated:
- Both AIF belief and substrate-2 hypergraph encode "entity status" (`:open`, `:strengthened`, `:foreclosed`, etc.). They read from related but distinct stores.
- The WM's belief is bootstrapped from `stack-annotations.edn :sections` — a substrate-1 era artefact, not substrate-2.
- substrate-2's `~360k hyperedges` carry rich `:status` + `:role` + `:ref` + edge information; the WM consumes none of it directly.
- This isn't "currently no duplication" — it's "currently no consultation at all"; the geometry is information-rich but the WM is information-blind to it.

**Reconciliation candidates (DERIVE must pick one + sunset others):**

| Candidate | What it means | Reconciliation property | Cost |
|---|---|---|---|
| **R-1: Composition into EFE** | Add `G-T-delta` term to G-total: `G = G-risk + G-amb − 0.4·G-info + 1.2·G-survival + γ·G-T-delta` | T-delta is *another EFE channel*; AIF formalism extends without breaking | Needs principled γ weight; existing EFE callers see a new term |
| **R-2: Lexicographic tiebreaker** | Rank by (G-total, T-delta); when G-totals tie within ε, T-delta orders the bucket | Smallest change; AIF unchanged; T-delta supplements | Requires per-action T-delta queries; doesn't reconcile DEEP disagreement (only ties) |
| **R-3: Dimensional separation in display** | WM ranks by G alone, but the live tile shows BOTH G-total AND T-delta per action; operator picks when they disagree | No reconciliation in math; reconciliation is operator-judgement | Operator burden stays; doesn't close the gap |
| **R-4: Substrate-of-belief (reconciliation by construction)** | WM belief IS a projection of substrate-2 geometry; no separate signal exists; predictive-coding updates apply *to a belief whose ground-truth is substrate-2* | No possible disagreement — same source | Most invasive; touches bootstrap-from-stack-annotations + every belief-update path |

R-4 is the most elegant — it makes the duplication threat impossible by construction. R-1 is the most formally correct given the AIF framing. R-2 is the smallest change that yields a visible discrimination on the live tile today. R-3 leaves the gap open.

**Operator-judgement question for DERIVE**: which of R-1..R-4 fits the futon stack's invariants on substrate authoritativeness? My read: **R-4 is the target; R-2 is the v0 staging post that produces visible discrimination immediately and demonstrates the substrate-2 query path exists.** If substrate-2 currently lacks the action-projection-query API, R-2 cannot ship either — which makes the M-live-geometric-stack consultation a HARD blocker on DERIVE, not a soft one.

**Conclusion on Q4 (revised)**: There IS duplication-by-omission today — the WM and the geometry are silently disagreeing simply by not consulting each other; that disagreement just doesn't show up because the geometry isn't visible to the WM. The DERIVE phase MUST output a chosen reconciliation candidate from R-1..R-4 plus the wiring sketch.

### Q5: The "click on G=-4.418 → meaningful trace" affordance

Joe's operational criterion for gap closure. Concretely:

```
[Click on "G=-4.423" in next-move-live tile]
  ↓
[Trace panel opens, showing for selected action:]
  G-total = G-risk + G-ambiguity − 0.4·G-info + 1.2·G-survival
          = 0.259  + 0.015       − 5.594    + 0.899
          = -4.423

  G-risk      = 0.259    [drives: sorry-count-norm gap = 0.50, scaled]
  G-ambiguity = 0.015    [drives: variance over predicted observation]
  G-info      = 13.985   [drives: info gain from action's projection]
  G-survival  = 0.749    [drives: hinge-loss for predicted out-of-pref channels]

  Predicted observation (post-action):
    sorry-count-norm    : 0.797 → 0.793  (Δ = -0.004)
    annotation-health   : unchanged
    mission-health      : unchanged
    [all other channels unchanged]

  Tie diagnostic:
    4 other actions produce IDENTICAL predicted-observation.
    → This action's G-total is not action-specific; it's family-specific.
    → Pick by other means (scale, operator-skim, geometric T-delta).
```

This is the ready surface (all the data exists per the Ready/Missing table); the missing piece is the cljs panel + the click handler. It's an `:hour`-scale piece of work — would itself fit `wm-ui-hud-mode-rationale-hardcode` shape (small bounded UI refactor).

**Crucially**: the trace panel makes the tie *diagnostic-rich rather than diagnostic-poor*. Today the tie looks like "WM is broken" (Joe's first reaction). With the trace, the tie looks like "WM is being honest — these 5 actions are equivalent at this resolution; here's why; here's what additional resolution would discriminate them."

### Surprises documented

1. **The Ready column is fuller than expected.** The G-decomposition data is already emitted per action; the live tile already renders structured data; the substrate-2 hypergraph is operationally live. The "missing" pieces are mostly UI affordances + small wiring, NOT new substrate.

2. **The Campaign-as-shape passes the shape-first IDENTIFY test** with 3 plausible instances on first scan. This was a guess in IDENTIFY's theoretical-anchoring; MAP confirms it.

3. **The operator-skim is the de-facto active signal.** The WM has been honest about its indifference all along; what makes it look broken is the *display* of indifference as if it were preference. The tied-bucket display fix (commit `713c74d`) closed the worst of that; the trace affordance would close the rest.

4. **Reconciliation policy is NOT a future-tense problem** (corrected by operator 2026-05-26). The current "no consultation" state IS the silent-disagreement state; deferring reconciliation to a later mission would crystallise the geometry as dead-from-WM-POV — a non-starter. DERIVE must output a chosen reconciliation candidate (R-1..R-4 per §Q4); R-4 (substrate-of-belief by construction) is the elegant target; R-2 (lexicographic tiebreaker on G-ties) is the v0 staging post that proves the query path exists.

5. **Joe's "tie IS the evidence" framing reframes the whole excursion.** This is no longer "add cost-awareness to make EFE smarter"; it's "the tie is a coverage-incompleteness signal — close the coverage gap, the tie self-resolves." The work is additive (new dimensions) not substitutive (replace EFE).

### MAP exit criterion

A human (Joe) reads §MAP and agrees:
- The signal-source inventory is honest (AIF + geometry + operator-skim + ARGUE; today only AIF + operator-skim are active)
- The 5-tied-sorries prioritisation in Q1 is the right operator-skim output (or proposes amendments)
- Campaign-as-shape passes shape-first IDENTIFY with the 3 plausible instances named
- The "click on G-total → trace" affordance is a concrete enough next-step shape that DERIVE can specify its API
- The reconciliation candidate set (R-1..R-4 per §Q4) is exhaustive enough that DERIVE can pick from it without needing to invent a fifth shape

### Next phase

DERIVE has four outputs (all of them, in this excursion — no deferrals):

1. **Reconciliation candidate selected**: pick from R-1..R-4. R-4 is the target; R-2 is the v0 staging post. Specify which; specify the migration from v0 to target if they differ.
2. **M-live-geometric-stack consultation**: hard blocker on DERIVE — substrate-2's action-projection-query API must be confirmed-exists OR a finding-with-spec filed on `M-live-geometric-stack.md` to add it. Either outcome unblocks shape-(c) wiring.
3. **Trace affordance design** (smallest viable shape from MAP §Q5): the click-on-G-total UI + the per-term provenance + the tie-diagnostic block. Implementation-ready.
4. **Campaign-as-structural-element spec**: proposed addition to `mission-lifecycle.md` defining what a Campaign is, with the 3 instances enumerated in §Q2 as the worked examples.

The `:scale` annotation source decision (operator-curated EDN vs ARGUE-mined vs T-delta-derived) collapses naturally out of (1) and (2): under R-4 (substrate-of-belief), scale is a derived projection of substrate-2's structural depth; under R-2 (tiebreaker), scale could be the easy first proxy. No separate decision needed.

---

## 3. DERIVE (drafted 2026-05-26 after M-live-geometric-stack read pass)

### §3.1 M-live-geometric-stack consultation result (was the hard blocker)

**Mission status quoted from `M-live-geometric-stack.md`**: *"COMPLETE (2026-04-28). All seven phases delivered."*

**Initial (incorrect) finding (2026-05-26)**: I grepped `futon1a/src/` for `compute-T / T-field / grad-T / delta-T` and got zero results, concluding "Phase 2 geometric-layer functions are not callable in code; mission status incorrect; potential discipline-break."

**Correction (Joe escalated the framing; I re-grepped to verify)**: Phase 2 IS delivered as `futon3/scripts/geometric_layer_phase2.clj` — a babashka script that computes `T, ∇T, ΔT, drift` by querying the futon1a HTTP API on :7071. My grep was incomplete (missed `scripts/`). The discipline is intact; my reading was wrong. Lesson saved as [[feedback_grep_src_and_scripts]].

**Honest finding after correction**: M-live-geometric-stack delivered exactly the scope it claimed — a code-structure geometric layer. The script computes:
- `T(var) = 1 if no incident :coverage edge else 0`
- `T(test) = 0`
- `∇T(e) = T(target) − T(source)` over `:calls` / `:coverage` / `:vocabulary-use` edges
- `ΔT(v)`, `drift`, components

**The actual gap** (sharpened by the consultation): **the entity-domain is CODE STRUCTURE** (`code/v05/{var,test,namespace,term}`). It does NOT include `:sorry` or `:mission` as first-class entity types. Sorries — the very things the WM is recommending actions about — are NOT typed in the geometric substrate. As Joe put it: *"that's kind of hilarious, given that the WM is recommending five actions to do with sorries."*

The gap is therefore not "missing geometric functions" but **"missing entity-type for the things we're trying to rank."** The geometric formalism is sound; its domain doesn't yet cover sorries.

### §3.2 The reconciliation path (operator-redirected 2026-05-26)

The earlier R-2-proxy proposal (use hyperedge fan-out as a crude structural weight) was authored before Joe's reframing landed: *"a sorry is either a manifold end (better) or a pole at infinity (workable as an approximation, and arguably the same as the former anyway)... we presumably need sorry as a first class thing in order to even start to look at the geometry around our five tied options."*

This is the **right framing** and supersedes R-2-proxy as the v0 design. The substrate-2-aware reconciliation isn't "let's use whatever signals we can scrape from current substrate-2 to break ties on sorries" — it's "**type sorries as first-class entities in substrate-2 with manifold-end T-semantics, then the existing geometric formalism applies**."

#### Selected v0 path: **sorry-as-manifold-end in substrate-2** (joint output of two parallel tracks)

Two parallel tracks share a substrate-2 extension; both depend on **M-INC step (b)** (the typed-event-vocabulary in code, currently HEAD-as-escrow under codex-7) landing:

| Track | Purpose | Owner candidate | Status |
|---|---|---|---|
| **Geometry-track** (sibling-excursion to be authored; tentative name `E-substrate-2-sorry-typing.md`) | Add `:sorry` entity-type to substrate-2 vocabulary; define `T(sorry) = 1 if :open else 0` (manifold-end semantics, isomorphic to `T(var) = 1 if no incident :coverage edge else 0`); define edge classes (`:addresses`, `:resolves`, `:raises`, `:bites`, `:related-mission`); add an in-JVM query path so the WM can consume T-delta-per-sorry | claude-1 or codex | Pending M-INC step (b) |
| **Mining-track** (`M-a-sorry-enterprise`, advance from current state) | Mine sorries from agent-interaction transcripts; emit as M-INC-typed events into substrate-2's new `:sorry` entity-type | Mining-shaped agent (codex-2 batch?) | Pending M-INC step (b) + draft of vocabulary |

**The geometry isn't dead-from-WM-POV under this plan** because once `:sorry` is typed in substrate-2 with manifold-end T-semantics, T-delta-per-action (where action.target is a sorry-entity) becomes a query against substrate-2 that the WM can consume. The original R-4 (substrate-of-belief by construction) lands as the LONG-RUN shape; the v0 staging post is "manifold-end-T over typed sorries via the existing Phase 2 bb script extended to enumerate `:sorry` entities."

**Reconciliation property by construction**: belief and geometry both consult the same `:sorry` entity in substrate-2; no possible disagreement. The duplication-by-omission threat (§Q4) closes structurally.

**Why this beats the original R-2-proxy**: R-2-proxy would have been substrate-2-WEAR but not substrate-2-INFORMED. Hyperedge fan-out per sorry isn't a principled cost/benefit signal — it's a count. The manifold-end T-semantics IS principled (matches the M-live-geometric-stack §formalism's logic), and the same geometric formalism that ranks code-structure entities ranks sorry-entities once they're typed.

**Tie-discrimination property**: under sorry-as-manifold-end, the 5 tied options would all have `T=1` (each is `:open`) but `∇T` and `ΔT` differ per sorry depending on incident edges (`:addresses` from missions, `:raises` from events, `:related-mission` density). The current 5-way G-total tie becomes a 5-way distinct (G, T-delta) tuple. Tie self-resolves.

**Re-classification of `:sorry/r3d-per-entity-attribution`**: previously typed `:campaign` because blocked on M-INC step (b). Under this plan, M-INC step (b) is the SHARED predecessor for both tracks AND for r3d-per-entity-attribution. Once it lands, r3d-per-entity-attribution becomes `:mission`-scale (not Campaign-scale), since its blocker is the same single substrate-vocabulary commit.

### §3.3 Finding-with-spec for follow-on missions (re-targeted 2026-05-26)

Original draft proposed filing a "geometric-functions absent" finding on `M-live-geometric-stack.md`. **That finding was withdrawn** after the consultation correction (Phase 2 IS delivered in `scripts/`). The real follow-on is a finding-with-spec pointing at TWO missions:

**Finding for `M-live-geometric-stack.md` §"Open questions / follow-ons"** (request, not stop-the-line):

> The geometric formalism (`T`, `∇T`, `ΔT`, `drift`) is delivered as `futon3/scripts/geometric_layer_phase2.clj` and operates over `code/v05/{var,test,namespace,term}` entities. **The futonic stack's prioritisation problem requires extending this entity domain to include `:sorry` (and likely `:mission`) as first-class types with manifold-end T-semantics.** This is not a mission-failure of M-live-geometric-stack; it's a downstream substrate-vocabulary extension that consumes the existing formalism. Authoring tracker: `futon3c/holes/missions/E-action-cost-modelling.md` (the geometry-track per §3.2 above).

**Finding for `M-interest-network-coupling.md` (M-INC) §"Successor missions"**:

> M-INC step (b) (typed-event-vocabulary commit, currently HEAD-as-escrow under codex-7) is now identified as the hard predecessor for: (i) the geometry-track sorry-typing in substrate-2, (ii) `M-a-sorry-enterprise` mining-track resuming in earnest, (iii) `:sorry/r3d-per-entity-attribution` becoming addressable (re-typed from `:campaign`-scale to `:mission`-scale). Suggestion: codex-7's piece #2 → step (b) sequence is now load-bearing for the WM's prioritisation problem; visible elevation of priority warranted.

### §3.4 Trace affordance design (UI specification)

Already sketched in §MAP Q5; concrete shape for implementation:

**Data side (already ready per Ready/Missing table)**:
- `judgement.ranked-actions[i]` carries `G-risk`, `G-ambiguity`, `G-info`, `G-survival`, `G-total` (✓ verified live this session)
- `judgement.priorities` carries per-channel observed values + preferred-range (✓ exposed in live tile)
- Predicted-observation post-action: NOT currently exposed per-action (✗); needs new emission in `judge` — small addition (~10 lines)
- Structural-weight (from R-2-proxy): NEW; needs `derive-structural-weight` fn in `futon3c.aif.stack-generator` or sibling

**UI side**:
- Live tile's `G=-4.423` becomes a `<details>` element (no separate click handler needed; html-native disclosure)
- Open state renders the trace block:

```
G-total = G-risk + G-amb − 0.4·G-info + 1.2·G-survival   = -4.423
           0.259  + 0.015   − 5.594     + 0.899

G-risk = 0.259  ← driven by: sorry-count-norm at 0.80, gap = 0.50
G-amb  = 0.015  ← driven by: low variance over predicted observation
G-info = 13.985 ← info gain from action's projected channel change
G-surv = 0.749  ← hinge-loss for predicted-out-of-preferred channels

Predicted observation post-action:
  sorry-count-norm  : 0.797 → 0.793  (Δ = −0.004)
  [other channels unchanged]

Tie diagnostic:
  4 other actions produce IDENTICAL G-decomposition.
  → G alone cannot discriminate; lexicographic tiebreaker uses structural-weight.

Structural weight (substrate-2-derived):
  fan-out:           7 hyperedges reference this entity
  edge-class-div:    3 distinct edge classes
  activation-hits:   2 pattern-activations in window
  → weight: 12 (medium); ranks 3 of 5 in tied bucket.
```

**Implementation budget**: ~`:hour` scale. cljs `<details>` element + per-action trace data passed through from backend. The structural-weight computation lands alongside per §3.2.

### §3.5 Campaign-as-structural-element spec (grounded re-authoring; VERIFY T10)

The grounding question here is the one asked by `structure/what-problem-is-this-actually-solving`: **what problem is Campaign-shape actually solving?** The answer is not "we want a bigger container than Mission." The answer is: **some pieces of work have one joint outcome that depends on several Missions, and that joint outcome is easy to lose if each Mission is allowed to optimize only for its own local success.** This is the pattern's proxy-problem substitution failure mode in structural form.

**IF** a candidate body of work requires multiple Missions whose outputs must combine to reach one shared outcome, and no single Mission can honestly own that coordination without becoming misshapen,

**HOWEVER** treating that work as "just one large Mission" breaks Mission-scale discipline, while treating it as "just several sibling Missions" leaves the joint outcome unowned. In the first failure mode, one Mission absorbs too many phases, repos, or verification burdens and stops being tractable. In the second, each constituent Mission can look successful on its own terms while the real outcome that required the cluster in the first place slips between them.

**THEN** name a **Campaign** as the coordinating structural element above Mission. A Campaign carries:
- **A name** — the shared outcome being pursued (for example `Campaign-R3-honesty`)
- **Constituent Missions** — the specific Missions whose outputs must combine
- **Coordination shape** — sequential, parallel, or convergent
- **Joint completion criterion** — a testable condition that no single constituent Mission can satisfy alone

The Campaign itself gets a lightweight `HEAD → IDENTIFY → MAP` layer whose job is to keep the joint outcome explicit, keep the constituent Missions in view, and name how their outputs combine. The constituent Missions continue to follow the standard lifecycle independently. Campaign `COMPLETE` fires only when the joint completion criterion is met.

**BECAUSE** that is the smallest structural element that solves the actual problem. Without Campaign-shape, multi-Mission work is mis-shaped either as a Mission that grew too big or as a cluster with no organizing frame. Campaign is justified only when it prevents the real joint problem from being replaced by a set of locally-successful proxies.

**Worked-example verification**:

- `Campaign-R3-honesty` **passes** the Campaign-vs-Mission test.
  IF: honest per-entity attribution requires M-INC step `(b)`, downstream R3d rewiring, and multi-channel verification.
  HOWEVER: none of those can honestly absorb the others. Making M-INC own the rewiring and verification distorts M-INC; making the rewiring Mission own its vocabulary predecessor distorts the rewiring Mission; verification cannot be reduced to either one.
  THEN: the right shape is a sequential/convergent Campaign whose joint completion criterion is "per-entity attribution is honest end-to-end across vocabulary, consumer, and verification."
  BECAUSE: the shared outcome is not "step (b) landed" or "rewiring landed" but "R3 honesty exists in operation."

- `Campaign-the-futon-stack` **passes**, retroactively.
  IF: the `M-the-futon-stack-Q1..Q7` cluster exists to answer one stack-scale organizing question through several Missions.
  HOWEVER: any one of those Missions can locally succeed while the stack-level question remains unresolved, and no single Mission can honestly own all seven without collapsing into an umbrella programme document.
  THEN: the cluster is better understood as a Campaign with several constituent Missions.
  BECAUSE: the problem being solved is stack-scale coordination of a family of questions, not the independent success of each question-shaped Mission.

- `Campaign-substrate-completion` **does not yet pass** as a validated Campaign instance; it remains a candidate.
  IF: "substrate-1 → substrate-2 → substrate-3" names a coherent multi-stage arc.
  HOWEVER: at present this is still an umbrella story more than a worked coordinating frame; the constituent Missions and joint completion criterion are not yet named tightly enough.
  THEN: record it as a candidate instance, not as proof that Campaign-shape is real by itself.
  BECAUSE: otherwise we would be doing exactly what the grounding pattern warns against — naming a larger artifact without being clear about the problem it solves.

**Result of the 3-instance check**: Campaign-shape remains justified. Two of the three instances (`Campaign-R3-honesty`, `Campaign-the-futon-stack`) pass the Campaign-vs-Mission test cleanly, which is enough to show this is not a one-off `:special-case true` invention. The third (`Campaign-substrate-completion`) stays in candidate status until its constituent Missions and joint completion criterion are named more concretely.

### §3.6 DERIVE outputs summary (the 4 mandatory deliverables — revised after corrections)

| Deliverable | Output | Location |
|---|---|---|
| (1) Reconciliation candidate | **Sorry-as-manifold-end in substrate-2**, jointly produced by a geometry-track sibling-excursion + M-a-sorry-enterprise mining-track, both downstream of M-INC step (b) | §3.2 above (revised) |
| (2) M-live-geometric-stack consultation | Phase 2 IS delivered (in `scripts/`); the real follow-on is a vocabulary-extension finding, NOT a "missing functions" stop-the-line | §3.1 + §3.3 above (corrected) |
| (3) Trace affordance design | `<details>`-element-based; data mostly ready; ~`:hour` implementation | §3.4 above |
| (4) Campaign-as-structural-element spec | Definition + 3 worked-example instances + proposed lifecycle | §3.5 above |
| **Bonus** | `r3d-per-entity-attribution` re-typed `:campaign` → `:mission` (since its blocker is the same M-INC step (b) shared with the geometry/mining tracks) | §3.2 closing paragraph |

### §3.7 Carried-forward tensions for ARGUE (revised)

1. **Geometry-track ownership**: should the substrate-2 sorry-typing land as a new excursion `E-substrate-2-sorry-typing.md` (within E-action-cost-modelling's umbrella) or as a section of M-INC step (b) itself (since the vocabulary commit is what they share)? Argue both housings before VERIFY.
2. **`T(sorry)` formalism**: is `1 if :open else 0` the right manifold-end definition, or should the 7-status vocabulary produce a finer-grained T (e.g. `T(:open)=1, T(:refined)=0.7, T(:strengthened)=0.3, T(:addressed)=0`)? The latter gives smoother gradients but introduces operator-tuning surface.
3. **Campaign-as-element discoverability**: does it belong in `mission-lifecycle.md` or in a sibling `campaign-lifecycle.md`? Structural placement matters.
4. **Manifold-end-vs-pole-at-infinity**: Joe noted these are "arguably the same anyway." Are they? Topologically a manifold-end is structure-running-out (boundary); a pole at infinity is divergence (asymptotic). For a sorry, both readings produce `T=1` semantics — but they imply different ∇T patterns at the boundary. Worth surfacing before the formalism freezes.
5. **Mining-track-vs-vocabulary sequence**: M-a-sorry-enterprise's mining work can START before M-INC step (b) lands (interview-design, embedding work) but can't EMIT records into substrate-2 until the vocabulary lands. How much pre-vocabulary design is safe?

### DERIVE exit criterion (revised)

A human (Joe) reads §3 (post-corrections, post-aliveness-synthesis) and agrees:
- The M-live-geometric-stack consultation correction is honest — Phase 2 IS in `scripts/`; my initial reading was wrong; lesson [[feedback_grep_src_and_scripts]] saved
- The selected v0 path (sorry-as-manifold-end in substrate-2; jointly produced by geometry-track + mining-track; both downstream of M-INC step (b)) is the right shape
- The trace affordance design remains implementation-ready and is independent of the substrate-vocabulary work
- The Campaign spec is concrete enough to land as a proposed addition to `mission-lifecycle.md`
- The 5 carried-forward tensions are honest things for ARGUE to defend or refute
- The re-classification of `r3d-per-entity-attribution` from `:campaign` to `:mission` (consequent on M-INC step (b) being the shared predecessor) is a fair simplification of the cohort
- The §3.8 aliveness synthesis (Alexander/Salingaros/mana/AIF/T/EOI as projections of one underlying quantity) is the right load-bearing reframing that motivates the M- (mission) retitle, not just a thematic note

---

### §3.8 Aliveness synthesis (Alexander/Salingaros/mana/AIF/T/EOI)

**Operator framing 2026-05-26 (Joe, emacs-repl, verbatim)**:

> *"M-interest-network-coupling is — in fact — extremely interesting, because I am doing a lot of high-value HITs with my EOIs.  And, effectively, AIF is a proxy for Interest in the EOI sense.  This is a problem (much like our supposed geometry gap) if we don't have an actual way of understanding Interest.  Now, we have talked about 'mana' — mana is supposed to measure 'aliveness' in something like the Christopher Alexander sense, which, if you're familiar with Salingaros's interpretation, is a geometric quantity.  So, I think a few things might come together nicely if we keep pushing."*

**The unification claim**: the futon stack has multiple readouts of one underlying quantity, currently treated as parallel realities.  Joe's clarifying rename (2026-05-26) distinguishes **Mana** (positive aliveness, Alexander/Salingaros sense) from **Anamnesis** (the currently-implemented `mana-snapshot.bb` measure — the inverse: un-discharged tension, what the system can't forget, Greek ἀνάμνησις "recollection"):

| Readout | Where it lives | Sign |
|---|---|---|
| EOI corpus strength | `futon5a/holes/missions/M-expressions-of-interest.md` | + (where Joe puts value) |
| AIF Expected Free Energy | `futon2.report.war-machine/judge` | inverse (lower G = preferred) |
| **Anamnesis** (formerly mis-named "mana" in code) | `futon0/scripts/mana-snapshot.bb` (substrate-rename to `anamnesis-snapshot.bb` is a follow-on) | inverse (high = un-discharged tension calling for attention) |
| T-distribution | `futon3/scripts/geometric_layer_phase2.clj` (M-live-geometric-stack) | + (high = un-knit boundary) |
| **Mana** / Alexander "aliveness" / wholeness | Pattern Language; The Nature of Order | + (emergent from productive tension resolution) |
| Salingaros geometric formalisation | Scale-hierarchy + topological coherence; mathematizes Alexander | + (geometric structure of wholeness) |

**Key finding — Mana / Anamnesis distinction (Joe, 2026-05-26)**:

The current `mana-snapshot.bb` measure is more accurately called **Anamnesis**: what the system hasn't forgotten yet, the un-discharged tension that calls for attention.  **Mana** in the Alexander/Salingaros sense is the positive aliveness-quantity that emerges when anamnesis discharges productively.  Their relation: *mana flows when anamnesis discharges; high anamnesis = pending mana awaiting release.*  Two signs of the same projection of the same quantity.

`mana-snapshot.bb` computes pressure = `f(N-count, D-age-days, B-bytes)` — accumulated uncommitted-work signal at the working-tree level, with tiers (silent / advisory / high / stop-the-line).  The futon stack already implements this; what was missing was the explicit naming + the unification with the broader aliveness-geometry.

**Substrate rename as a sibling follow-on**: rename `mana-snapshot.bb` → `anamnesis-snapshot.bb`; update consumers (`futon2.report.war-machine` metabolic-balance channel; the `wm-operator-clear.edn` sentinel; `futon3c/holes/missions/E-wm-staleness-meta-stop.md` references).  Not in scope for M-action-cost-modelling's first cycle — flagged here for operator-decision and tracked as a downstream task once the naming-distinction is operator-ratified.

**Sorry under the synthesis** (four readings, one phenomenon):

| Reading | Description |
|---|---|
| Alexander | A sorry is an aliveness-deficit — a region of the manifold where wholeness hasn't formed |
| **Anamnesis** | A sorry is a high-anamnesis region — the system can't forget the un-knit work; it remembers via the sorry-registry |
| Geometry-track (this mission) | A sorry is a manifold-end with `T = 1 if :open else 0` |
| EOI / interest sense | A sorry is a low-attention region (no current EOI directed at it) |

The 7-status M-INC vocabulary (`:spawned :refined :strengthened :addressed :falsified :foreclosed :reopened`) IS the **anamnesis → mana lifecycle**: as a sorry moves toward `:strengthened` / `:addressed`, anamnesis discharges and mana forms.  `:falsified` / `:foreclosed` are productive non-formation closures (the question itself dissolves; both anamnesis and mana drop to zero).

The 7-status M-INC vocabulary (`:spawned :refined :strengthened :addressed :falsified :foreclosed :reopened`) IS the aliveness-formation lifecycle — entities move along it as wholeness forms (or, with `:falsified` / `:foreclosed`, as productive non-formation closes the question).

**Why this expands the mission scope from E- to M-**:

The original framing of E-action-cost-modelling was "the WM doesn't represent action cost; bolt cost-awareness on." The synthesis reveals the work is much bigger: **the WM, mana, EOI corpus, and M-live-geometric-stack are all reading projections of one aliveness-quantity, and the futonic system's prioritisation problem cannot be solved by patching any single one — it must recognise the unification and make them consume one substrate.**

That's a Mission-scale piece of work. Hence the retitle to M-action-cost-modelling.md.

**Reconciliation strengthens**: in the original R-4 framing, "substrate-of-belief by construction" meant WM belief is a projection of substrate-2 geometry. Under the synthesis, R-4 becomes: **belief, mana, AIF EFE, T, EOI strength are all projections of aliveness over the typed substrate**; AIF's predictions ARE projections of aliveness-flow; mana's pressure IS the local-tension reading at the working-tree scale. No reconciliation in the "make A and B agree" sense — they're already the same thing, just named differently in different surfaces.

**The geometry-track + mining-track from §3.2 still hold** as the immediate implementation path, but their motivation deepens: typing `:sorry` as a first-class entity in substrate-2 is now framed as "naming the aliveness-projection at the sorry-scale" rather than just "letting the WM rank sorries." Same code, deeper rationale.

**Implications for downstream missions / excursions**:

- **M-a-sorry-enterprise (mining)**: now framed as "mining aliveness-deficits from agent interactions" — same work, sharper telos
- **`:next-move-live` trace affordance (§3.4)**: when it ships, the G-decomposition + structural-weight panel should optionally surface aliveness-readings per action (mana on the affected paths, T-delta on the entity, EOI proximity if any). Not required for v0; nice-to-have once the substrate matures
- **Tickle (`[[project_tickle_as_operator_model]]`)**: the focus-direct mode would naturally consume aliveness-projections — "point the pilot at the highest-aliveness-deficit region they can address right now" is the operator-modelling shape this synthesis suggests
- **The 5-tied sorries test case**: each has a distinct aliveness-deficit profile (mana, T, EOI-proximity, etc.); once `:sorry` is typed, the tie self-resolves not just on count-of-edges (R-2-proxy) but on the multi-projection aliveness signature

**Theoretical anchoring additions for IDENTIFY (to fold back on next operator-VERIFY)**:

- Christopher Alexander, *The Nature of Order* (esp. Book 1: *The Phenomenon of Life*) — the 15 fundamental properties, life as emergent geometric quality
- Nikos Salingaros — mathematization of Alexander; topological scale-hierarchy of wholeness; relevant work: *A Theory of Architecture*, papers on adaptive complexity
- Existing futon-side theoretical anchoring (AIF, capability-approach, ARGUE-derived weight) all become **sub-projections of the aliveness-geometry**

**Carried-forward tension (new, for ARGUE)**:

6. **Is the aliveness-synthesis genuinely load-bearing for the design, or is it a useful framing that doesn't change the actual code/data shapes?** This is the productive defend-or-refute question for ARGUE. My read: it changes the substrate-2 vocabulary work from "type sorries" to "type aliveness-projections (sorries are one)"; it changes mining from "extract sorries" to "extract aliveness-deficits"; it changes the trace affordance from "show G-decomposition" to "show multi-projection aliveness signature." The synthesis IS load-bearing; the work just becomes bigger and more honest.

7. **Operator-judgement on scope**: M- (mission) retitle implies multi-week+ scope, multiple sub-excursions, possibly a place in the futonic-mission cadence rather than a quick excursion. Honest about that — the synthesis really does pull more in.

**Addendum 2026-05-27 — Niche construction as the operative mechanism**:

VERIFY-stage empirical work (§5.1 T2/T6 closures) showed that the synthesis isn't load-bearing at the single-axis T-projection layer (T-binary collapses to is-open; binary T cannot break ties whose generator IS open-sorry status) but IS load-bearing at the **ΔT composition layer** — the Laplacian over a typed-edge manifold delivers the discriminating gradient the synthesis predicted. The missing piece of the §3.8 articulation as originally drafted was the **operative mechanism** by which mana flows / anamnesis discharges / projections converge.

That mechanism is **niche construction**. In AIF terms (Lewontin 1983; Odling-Smee/Laland/Feldman 2003; Friston 2010, 2017): an inference agent cannot construct its own niche; the agents-operating-its-peripherals do. The substrate's typed-edge graph IS the WM's niche; agents (operators paired with claudes) author new edges and vertex families that enrich the niche; the WM on the next tick sees a richer manifold and ranks more accurately. Anamnesis (un-discharged tension) drives the construction work; the construction discharges anamnesis into mana (positive aliveness); mana-flow seeds the next disclosure.

Pattern landed 2026-05-27 at `~/code/futon3/library/aif/niche-construction.flexiarg`: false-floor readings (e.g. ΔT=0 on action classes whose edge family isn't yet typed) are the niche-construction signals, not bugs. Today's T9 retroactive case study showed three of three non-sorry actions (anchor-reframe, file-edit, excursion-step) reading ΔT=0 false-floor under the current sorry-only edge inventory — which is exactly what tells the agents-operating-the-WM which edge families to construct next.

The synthesis is therefore four terms, one substance, one loop:

| Phase | Substance state | What surfaces |
|---|---|---|
| 1 | anamnesis discloses | un-discharged tension becomes legible in the substrate (high-ΔT region surfaces; false-floor names a missing edge family) |
| 2 | anamnesis discharges | felt salience selects the candidate; action taken |
| 3 | niche constructed | typed-edge trace lands in substrate-2 — the substrate is enriched for the next agent's read |
| 4 | mana flows | pattern crystallises; renewed aliveness seeds the next disclosure |

This is the same aliveness substance through four moments. The four-fold has independent alignment with M-pilot-appearance's REPL / four-foundations / R-criteria donor table (see M-pilot-appearance §7.7 for the cross-mission breadcrumb).

---

## 4. ARGUE (drafted 2026-05-26 after pattern cross-reference)

### Pattern cross-reference

The pattern scan does **not** overturn DERIVE's selected v0 path, but it does sharpen where each part belongs:

| Pattern | Where it applies | What it forces / clarifies |
|---|---|---|
| `aif/expected-free-energy-scorecard` | §MAP Q5 and §3.4 trace affordance | Keep `G` decomposed and operator-visible. The right response to a five-way tie is not "hide the math and add a magic weight" but "show the named terms and then show what they fail to discriminate." This pattern directly supports the `<details>` trace block and the refusal to collapse everything into one opaque scalar. |
| `aif/term-to-channel-traceability` | §3.4 trace affordance, especially "click on `G=-4.418` → meaningful trace" | Every displayed `G-risk / G-ambiguity / G-info / G-survival` term should declare the observation channels and intermediate values that produced it. This turns the trace from explanatory prose into auditable provenance and makes the tie diagnosable rather than rhetorical. |
| `aif/hierarchical-budget-aware-action-selection` | IDENTIFY's `:scale` field, work-breakdown gate, Campaign threshold | The mission's core move is budget-shaping, even though the budget here is time / coordination / decomposition capacity rather than setup-budget. The pattern says hard feasibility belongs in the selector/gate while soft pressure can remain in scored terms. That supports the design choice to render "needs work breakdown" when scale exceeds a pilot cycle, rather than pretending scalar `G` alone should absorb months-vs-minutes asymmetry. |
| `t3/capability-not-functioning` | IDENTIFY benefit-signal framing; DERIVE's rejection of "sorry-count goes down by 1" as sufficient benefit | Closing a sorry is not the point; unlocking a capability is. This pattern is the exact theoretical support for replacing count-of-closures with capability-delta and for distinguishing "small refactor" from "shared predecessor that unlocks a mission cohort." |
| `structure/mana-allostasis` | §3.8 aliveness synthesis and the handling of open problems | The pattern treats unresolved tension as the precursor to useful resolution, not as a mere bug count. This supports the argument that some open items carry more stored pressure than others, and that resolving them should be understood as releasing that pressure in a structured way. |
| `war-machine/inhabitation-threshold` | The live recommendation surface as the operator's actual action-entry point | A surface that recommends campaign-scale work as if it were an ordinary next move is not yet fit for ordinary use even if its underlying math is "formally correct." The work-breakdown CTA is therefore not decoration; it is required if the surface is supposed to guide real work. |
| `war-machine/operational-not-decorative` | Trace affordance and Campaign classification | `:scale`, `Campaign`, and the trace only count if they change behaviour on the live surface. This pattern rejects a merely documentary Campaign idea or a descriptive trace panel that cannot be checked against emitted terms. It reinforces the requirement that these structures alter ranking/gating, not just prose. |
| `structure/what-problem-is-this-actually-solving` | The MAP→DERIVE reframing from "add cost term" to "restore meaningful next-move discrimination" | This pattern explains why the mission had to retarget. The problem was never "the scorer lacks one more term"; it was "the recommender cannot honestly distinguish tractable work from months-scale work at the point of recommendation." DERIVE's shift to a clearer representation of the work plus a scale gate is the non-proxy formulation of the real problem. |

**Catch-up effect from pattern scan**:
- The trace affordance must remain **term-anchored** and **channel-anchored**; otherwise it violates both AIF traceability patterns.
- The work-breakdown layer belongs partly in **selection/gating**, not only in score shaping; otherwise months-scale actions still surface as misleading next moves.
- The aliveness synthesis is not merely poetic garnish. `structure/mana-allostasis` gives a plain reading of it: unresolved work stores pressure; different actions release different amounts of that pressure.

### Theoretical coherence

The DERIVE design remains coherent with IDENTIFY's original anchoring, but the center of gravity has sharpened.

- **AIF coherence**: the mission still refuses the glib move "change the EFE math." `G` remains a named, decomposed scorecard. The correction is to the action domain and the surface contract around recommendation, not to the formal existence of `G`.
- **Capability coherence**: Sen's capability framing survives intact and gets stronger. The benefit signal is not "one more closed sorry"; it is "what capability becomes live if this closes?" That is exactly why `r3d-per-entity-attribution` and a HUD refactor cannot be treated as equivalent, even if both decrement the same coarse count.
- **Geometric coherence**: the consultation with `M-live-geometric-stack` did not reveal a missing formula; it revealed that the system is not yet representing the relevant work items in the same data model as the geometry. In plain terms, the geometric model says: treat open work as a shape with nearby consequences, and ask how much surrounding structure would change if this item were addressed. That is why this mission must represent sorries and missions in the same graph of relationships as the code entities, rather than scoring them in a disconnected side table.
- **ARGUE-rationale coherence**: IDENTIFY's claim that existing mission `ARGUE` sections already contain cost/benefit signal remains true, but ARGUE now shows why that is only one reading of a larger picture. The mission is not abandoning ARGUE-derived benefit; it is placing it beside the other signals rather than letting each live alone.
- **Aliveness synthesis**: this is a deepening, not a theory swap. In plain terms, the aliveness model says: open work holds stored tension, and some kinds of resolution make the whole system feel more coherent and more usable than others. The scoring model, the geometric model, the interest signal, and the open/closed status of a sorry are all trying to measure that in different ways. That changes the naming and the target data work, so it is load-bearing; but it does not invalidate the earlier AIF / capability / geometry anchors. It gathers them.

### Trade-off summary

- We give up the cheap local fix of a hand-authored `:intrinsic-cost` as the primary answer. It is fast, but it would encode judgement in a parallel table and leave the deeper mismatch unresolved.
- We give up the idea that one scalar should do all the work. The design instead accepts a two-layer surface: decomposed `G` plus a scale/work-breakdown gate, with the geometric and aliveness readings supplying the principled tie-break as part of this mission's actual deliverable.
- We accept a larger structural scope by introducing **Campaign** as a real candidate element. That increases conceptual surface area, but it is more honest than pretending a multi-mission coordination problem is just a very large next step.
- We defer full automation of decomposition and the richer "why this matters" readout in the UI. The mission still commits to recognising when decomposition is needed, making the current recommendation trace meaningful, and grounding that recommendation in the shared representation rather than in a parallel heuristic.
- We choose a shared representation of the work over proxy counts. That costs more now, but it is part of the present requirement, not a later cleanup, because without it the recommendation logic is still split across disconnected views.

### Generalization notes

This design generalizes beyond the five tied sorries wherever three conditions hold:

- the system is selecting among heterogeneous actions that differ sharply in scale or coordination cost;
- the current scoring surface is formally correct but too coarse to distinguish them;
- the things being acted on can be represented in one shared model with meaningful lifecycle/status semantics.

Under those conditions, the same pattern applies: keep the score decomposed, expose a trace, add a work-breakdown or feasibility gate at the selection layer, and represent the action targets in the same shared model so the different signals are not maintained in parallel worlds.

The design should transfer to:
- recommendations beyond sorries, once missions and related work items are represented in the same shared model.
- Tickle's focus-direct mode, where the gate is "what can the operator productively address in this cycle?"
- Other futonic prioritisation surfaces that currently mix immediate and campaign-scale items without a decomposition boundary.

What would need to change elsewhere:
- If another domain has no meaningful lifecycle semantics, the geometric reading will need a different way of marking "unfinished here" than `T(sorry) = 1 if :open else 0`.

### Plain-language argument

The recommendation view is giving bad next-step advice not because its math is broken, but because it treats a one-hour task and a months-long effort as the same kind of action. The right fix is to keep the existing score visible, show how it was computed, add a separate check for whether the recommended action is actually small enough to do now, and represent the relevant work in one shared model rather than in disconnected scoring schemes. The geometric model here is simple: some open items sit near a lot of other consequences, so addressing them reshapes more of the surrounding work than others. The aliveness model is also simple: some open items carry more stored tension than others, so resolving them makes the whole working situation feel more coherent and usable. This mission should deliver that shared representation now, because without it the recommendation logic is still guessing across separate views.

### ARGUE exit criterion (held open by operator 2026-05-26)

Joe's framing (emacs-repl 2026-05-26): *"there are still some risks in ARGUE and we could keep it 'open pending verification'. The mission has quite a few complex moving parts that need to be aligned; the argument needs to keep it simple, but risks being an over-simplification. I like the idea of carrying forward some tensions to VERIFY and asking, 'OK, so what are we actually going to do about them?'"*

The four ARGUE exit criteria above are partially-met (the strategic argument holds; the pattern scan is real; aliveness synthesis is accepted as load-bearing), but ARGUE is kept **open pending VERIFY** because the carried-forward tensions from §3.7 + §3.8 + the implicit ones in codex-5's draft need explicit operational hooks. VERIFY (§5 below) picks each tension up and answers "what are we actually going to do about it?"

This is a deliberate stylistic choice — the alternative would have been a much longer ARGUE that chews through each tension before exiting. Joe's preference: keep ARGUE strategic and short; load VERIFY with the operational specificity. The risk of over-simplification stays named (rather than hidden by a false-resolution).

---

## 5. VERIFY (drafted 2026-05-26; operational hooks for ARGUE's carried-forward tensions)

VERIFY in this mission is not "did we build it?" — the building is INSTANTIATE's job. VERIFY is **"for each unresolved tension, what is the operator-checkable test or sibling-action that resolves it?"** The tensions are inherited from §3.7, §3.8, and codex-5's ARGUE; each gets a concrete next-action.

**Dispatch discipline** (Joe, emacs-repl 2026-05-26): per [[feedback_car_of_sequence_dispatch]] — take the **car** of T1..T10, do it, observe, then the next.  The table below is NOT a flat ratify-all-at-once list; each row carries explicit dispatch metadata.  Operator interjection-points live at each car-boundary.

### §5.0 Dispatch state (audit 2026-05-27 evening; post-empirical-session)

**Closed in original VERIFY pass (pre-2026-05-27)**:
- **T1 DONE** — housing decision (a); E-substrate-2-sorry-typing.md authored FULLY by codex-5 through HEAD → IDENTIFY → MAP → DERIVE → ARGUE → VERIFY → INSTANTIATE (rapid review-code cycle with claude-1, 2026-05-27). INSTANTIATE delivered: watcher-integration code (`futon3c/src/futon3c/watcher/file_ingest.clj` + `multi.clj`) + bootstrap script (`futon3c/scripts/ingest_sorrys_to_futon1a.clj` with explicit T-A2 decommission-rule); 8 tests / 22 assertions / 0 failures; T-A1 round-trip fixture passes lossless across 3 cases.
- **T5 DONE** — mining-track unblocked; finding filed at `futon5a/holes/missions/M-a-sorry-enterprise.md` (tail) naming the 3 load-bearing artefacts an agent would consume.
- **T8 DONE** — both findings filed; M-INC finding includes ownership-transition + step-(b)-landed updates.
- **T10 DONE** — codex-5 completed the grounded re-authoring in place at §3.5, closing the pre-commit Campaign-spec gate. Option A taken; see T10 row for outcome details.

**Closed by 2026-05-27 empirical session** (operator-paired emacs-repl cycle with claude-1):
- **T2 DONE** — empirical test on the live 7-way tie at G = −4.2558 (which the original 5-way tie at G = −4.422 had become after E-support-coverage and r3a reclassification cycles). After E-substrate-2-sorry-typing INSTANTIATE landed (live watcher integration: 16 `code/v05/sorry` hyperedges + 22 `code/v05/related-mission` edges in substrate-2), binary T(sorry) reads 1 for all 7 tied sorries. Confirmed: binary T is structurally 1-1 with action-existence — the tie *consists of* the open sorries, so binary T cannot break it. The proper discriminator is **ΔT over the mission-phase manifold**, made computable by the convergence of E-substrate-2-sorry-typing (today) + M-weird-modernism Task 8 (2026-05-23 single-parser consolidation that ingested `:mission/phase` into mission-doc vertices). The 7-way tie split cleanly into 4 distinct ΔT-levels (2.2 / 1.0 / 0.7 / 0.2; plus a substantive 0.0 dual for two sorries sharing one HEAD-phase target). Finer-grained T over a richer status vocabulary remains an open option but no longer the necessary next step at this layer.
- **T4 DONE** — manifold-end reading confirmed. Original test phrasing: *"if ∇T is well-defined and finite at sorry-entities, manifold-end reading holds; if it diverges, pole-at-infinity is operative."* Observed finite ∇T values across all 22 incident `code/v05/related-mission` edges. Pole-at-infinity reading rejected; manifold-end stands.
- **T6 DONE** — synthesis is NOT load-bearing at the single-axis T-projection layer (binary T collapses to is-open); IS load-bearing at the **ΔT composition layer** (the Laplacian over a typed-edge manifold delivers the discriminating gradient). Load-bearing-as-composition, not load-bearing-as-direct-projection. Operative mechanism named: **niche construction** (see §3.8 addendum + `~/code/futon3/library/aif/niche-construction.flexiarg`). The synthesis is one substance through four moments: anamnesis discloses → anamnesis discharges → niche constructed → mana flows. Four-fold has independent alignment with M-pilot-appearance §7.7's REPL/four-foundations/R-criteria donor table.

**Addressed with structural finding; INSTANTIATE work explicitly named** (today):
- **T9 ADDRESSED** — retroactive case study on 3 non-sorry action classes from `pilot-inhabitations.edn`: cg-686fdc10 (anchor-reframe), cg-58271911 (file-edit/spec-implementation), e-support-coverage-c2-c4 (excursion-step). All three targeted M-war-machine-pilot's neighborhood — operator's action selection was correct by intuition and by cross-ref evidence. The ΔT *shape* generalises (T on vertices + ∇T on typed edges + ΔT as Laplacian) but yields ΔT = 0 false-floor for non-sorry actions under the current sorry-only edge inventory. The substrate-2 typed-edge inventory must extend before the WM head can rank non-sorry actions by ΔT: (a) feed `code/v05/mission-cross-ref` (already in substrate from M-weird-modernism Task 8) into the Laplacian; (b) derive `code/v05/file→mission` from `:mission/code-paths`; (c) ingest E-prefix excursion docs as a typed vertex family (currently not ingested at all). Per `aif/niche-construction.flexiarg`, the false-floor reading IS the niche-construction signal — these are sibling-actions filed for INSTANTIATE, not VERIFY blockers.

**Future-Joe-triggered**:
- T3 (Campaign doc-split on 3rd instance)
- T7 (2-week cadence checkpoint 2026-06-09)

**Score (refresh 2026-05-27 evening)**: 7 DONE at VERIFY (T1, T2, T4, T5, T6, T8, T10) / 1 ADDRESSED with structural finding + sibling-actions filed (T9) / 2 future-Joe-triggered (T3, T7). Every tension is either closed or has explicit sibling-actions named for INSTANTIATE.

**Mission VERIFY status**: **COMPLETE.** Per §5.3 exit criterion (*"complete enough that ARGUE can exit when VERIFY's operational hooks have been executed (or the corresponding sibling-actions filed)"*) — every hook has either executed (T1, T2, T4, T5, T6, T8, T10) or named explicit sibling-actions for INSTANTIATE (T9), or is future-Joe-triggered with a named fire-condition (T3, T7). No tension is hand-waved.

**INSTANTIATE work explicitly named** (visible without doing it; ratification by Joe):

1. **T9-completion**: extend ΔT formalism to include `code/v05/mission-cross-ref` (already in substrate, just not fed into Laplacian); derive `code/v05/file→mission` from `:mission/code-paths`; ingest E-prefix excursion docs as a typed vertex family. Run a non-sorry action through the extended pipeline and confirm finite ΔT.
2. **§3.4 trace affordance**: implement the click-on-action-show-meaningful-trace UI specified in the trace affordance design.
3. **WM head live wiring**: use ΔT(target-mission-neighborhood) as the actual tie-breaker in the live ranked-actions surface (right now ΔT was computed offline in this VERIFY session; INSTANTIATE makes it live).
4. **§3.8 niche-construction completion**: the niche-construction-as-operative-mechanism reading is added as a §3.8 addendum today; INSTANTIATE consolidates it into the synthesis proper.

INSTANTIATE awaits operator ratification of VERIFY exit.

### §5.1 Tension-by-tension operational hooks

| # | Tension (from §3.7/§3.8/ARGUE) | Operational hook ("what are we going to do about it?") | Owner / venue |
|---|---|---|---|
| T1 | **Geometry-track housing**: new excursion `E-substrate-2-sorry-typing.md` vs section of M-INC step (b)? | **DONE 2026-05-27**. (a) chosen; codex-5 authored `E-substrate-2-sorry-typing.md` fully through HEAD → IDENTIFY → MAP → DERIVE → ARGUE → VERIFY in a rapid review-code cycle with claude-1 (review per phase) on 2026-05-27. Excursion stops at VERIFY exit-criterion; INSTANTIATE belongs to the excursion's own future cycle. M-INC step (b) (claude-3, 2026-05-26) is the theoretical-anchoring input to the excursion's DERIVE design choices. | Codex-5 (excursion DONE; awaits its own VERIFY-of-VERIFY); claude-3 (M-INC step (b) DONE 2026-05-26) |
| T2 | **T(sorry) formalism**: binary (`1 if :open else 0`) vs finer-grained over 7-status vocabulary? | **DONE 2026-05-27.** Empirical session: live-substrate ΔT computation on the current 7-way tie at G=−4.2558. Binary T(sorry) reads 1 for all 7 tied — confirmed structurally 1-1 with action-existence; cannot break a tie whose generator IS open-status. ΔT over mission-phase manifold (made computable today by E-substrate-2-sorry-typing ingest + M-weird-modernism Task 8's enriched mission-doc props) split the tie into 4 distinct levels. Binary T is the right v0; finer-grained vocabulary is a separately-prioritised question, not the necessary next step. | claude-1 + Joe (emacs-repl 2026-05-27) |
| T3 | **Campaign-as-element discoverability**: `mission-lifecycle.md` vs sibling `campaign-lifecycle.md`? | **Author the spec inline in `mission-lifecycle.md`** as the v0 home (one file is easier to navigate); if Campaign spawns 3+ concrete instances over the next 2 months, promote to sibling doc. Trigger: when authoring the third actual Campaign, the inline spec hits readability friction → time to split. Until then: one doc. | Future-Joe; trigger named |
| T4 | **Manifold-end vs pole-at-infinity topological distinction** | **DONE 2026-05-27.** Observed finite, well-defined ∇T across all 22 incident `code/v05/related-mission` edges in the live substrate (∇T values ranged smoothly between 0.0 and 1.0 corresponding to phase-difference between sorry and target mission). Pole-at-infinity reading rejected; **manifold-end reading confirmed.** | claude-1 + Joe (emacs-repl 2026-05-27 empirical session) |
| T5 | **Mining-track-vs-vocabulary sequence**: how much pre-vocabulary design is safe? | **DONE 2026-05-26**. M-INC step (b) shipped (`interest-event-vocabulary.flexiarg` via claude-3); the M-a-sorry-enterprise mining-track is fully unblocked. Finding filed at `futon5a/holes/missions/M-a-sorry-enterprise.md` (tail) naming predecessor satisfied + the three load-bearing artefacts a mining-agent should consume. The action-request is now M-a-sorry-enterprise's own dispatch decision; no further blocker from this mission's side. | M-a-sorry-enterprise owner picks up when ready |
| T6 | **Aliveness synthesis load-bearing or framing-only?** | **DONE 2026-05-27 (with sharper finding than the original binary question allowed for).** The synthesis is **NOT** load-bearing at the single-axis T-projection layer — binary T collapses to is-open, which is action-existence's own predicate. The synthesis **IS** load-bearing at the **ΔT composition layer** — the Laplacian over a typed-edge manifold delivers the discriminating gradient the synthesis predicted (and the empirical 7-way-tie-break confirms it). Operative mechanism: **niche construction** (`~/code/futon3/library/aif/niche-construction.flexiarg` landed today; §3.8 addendum integrates it into the synthesis). Mana / anamnesis / niche-construction form one substance moving through four moments (disclose → discharge → construct → flow). | claude-1 + Joe (emacs-repl 2026-05-27) |
| T7 | **M-scope honest about multi-week+ work** | **Visible cadence checkpoint at 2 weeks.** At 2026-06-09 (or first operator-paired session after), revisit mission status. If less than 50% of completion criteria are addressed, re-shape: either Mission stays but timeline slips honestly, or sub-scope spawns as a Campaign with this M as its first constituent. Don't let the mission ambiguously balloon. | Joe + claude-1 cadence |
| T8 | **§3.3 finding filings on M-live-geometric-stack and M-INC** — argued or just enacted? | **DONE 2026-05-26.** Both filed as direct edits at the tails of the respective mission docs.  M-live-geometric-stack note acknowledges the consultation correction (`scripts/` location) + names the substrate-2 vocabulary extension as the downstream consumer.  M-INC note elevates step (b) priority + names the codex-7 routing gap discovered when whistling for the T1 ETA + the provisional T1 (a) resolution.  Next car promotes: T1-output (author E-substrate-2-sorry-typing.md). | claude-1 enacted; sibling-mission owners may VERIFY-of-finding asynchronously |
| T9 | **Generalization beyond sorries argued by codex but not grounded** | **ADDRESSED with structural finding 2026-05-27** (retroactive case study against `pilot-inhabitations.edn` rather than running new actions). Three closed non-sorry inhabitation cycles examined: cg-686fdc10 (anchor-reframe wm-ui-anchor:0011), cg-58271911 (file-edit on stack_generator.clj), e-support-coverage-c2-c4 (excursion-step on E-support-coverage). All three targeted M-war-machine-pilot's neighborhood — operator action selection correct by intuition + cross-ref evidence. ΔT *shape* (T-on-vertices + ∇T-on-typed-edges + ΔT-as-Laplacian) generalises, but reads ΔT=0 false-floor under the sorry-only edge inventory. Per `aif/niche-construction.flexiarg`, false-floors ARE the niche-construction signals: they name the typed-edge families the substrate must construct next. **Sibling-actions named for INSTANTIATE**: (a) feed `code/v05/mission-cross-ref` (already in substrate) into the Laplacian; (b) derive `code/v05/file→mission` from `:mission/code-paths`; (c) ingest E-prefix excursion docs as a typed vertex family. The generalisation claim from §3.6 stands with the modifier *"same pipeline shape, but typed-edge inventory must extend beyond `:related-mission`."* | claude-1 + Joe (emacs-repl 2026-05-27); INSTANTIATE work explicitly named |
| T10 | **Campaign-shape justification through `structure/what-problem-is-this-actually-solving`** | **DONE 2026-05-27.** Codex-5 took **Option A**: in-place replacement of §3.5. The re-authored section now states the actual problem Campaign-shape solves, uses explicit IF/HOWEVER/THEN/BECAUSE structure, and re-tests the 3 worked-example instances individually. Outcome: `Campaign-R3-honesty` and `Campaign-the-futon-stack` pass cleanly; `Campaign-substrate-completion` remains candidate-only until its joint completion criterion is named more tightly. VERIFY T10's pre-commit gate is therefore closed. | codex-5 |

### §5.2 What VERIFY does NOT do

- **Doesn't pretend the tensions are resolved.** They aren't; each just gets a concrete next-action that, when executed, resolves it.
- **Doesn't commit to executing all of these in this mission.** Some (T1, T8) are sibling-actions; some (T2, T4, T6, T9) are tests-during-INSTANTIATE; some (T3, T7) are future-Joe-triggers.
- **Doesn't re-litigate ARGUE.** Codex's strategic argument stands; VERIFY adds the operational specifity that ARGUE deferred.
- **Doesn't pre-commit Campaign as real without evidence.** T10 has now supplied the pattern-grounded argument and the 3-instance check; Campaign remains justified by that evidence, not by decree.
- **Doesn't dispatch all 10 hooks at once.** Per [[feedback_car_of_sequence_dispatch]], §5.0 names the car (T1) and the next-up (T8); the rest wait until they unblock.  Operator interjection-points live at each car-boundary.

### §5.3 VERIFY exit criterion

A human (Joe) reads §5 and agrees:
- Each of T1..T10 has an operational hook that is **testable or enactable**, not just "we'll think about it"
- The owner/venue column is honest (not "TBD" for things that need a real owner)
- The hooks are **proportionate** to the tension's severity — not over-engineered checks for minor things, not hand-waving for load-bearing ones
- The list is **complete enough** that ARGUE can exit when VERIFY's operational hooks have been executed (or the corresponding sibling-actions filed)

When Joe ratifies §5, ARGUE → CLOSED, VERIFY → IN_PROGRESS, and the mission moves forward with the named hooks as the work list.

---

## 6. INSTANTIATE (pending)

Pending VERIFY.

---

## 7. DOCUMENT (pending)

Pending INSTANTIATE.

---

## Appendix A. Cross-references

- HEAD provenance: `HEAD-wm-action-cost-prioritisation.md` (to be deleted; content preserved above)
- Today's commits: `30bbc89` `c71dc42` `713c74d`
- Lifecycle convention: `futon4/holes/mission-lifecycle.md`
- Sibling-cohort: `E-wm-live-recommendation.md` (made the surface live), `E-wm-metric-redesign.md` (shape of metrics), `E-support-coverage.md` (the cycle that surfaced the tied-bucket symptom)

## Appendix B. Provenance

- HEAD authored: claude-1, 2026-05-26 after commit `713c74d`
- HEAD → IDENTIFY transition directive (Joe, emacs-repl 2026-05-26): *"let's see if we can step from HEAD to IDENTIFY following the standard mission-lifecycle.md steps — in brief terms the 'gap' is that we're not 100% confident in the AIF scores, nor in thinking through how they relate to the differential geometry concepts from M-live-geometric-stack.md — the reason this is a meaningful gap is that, as you said, the WM seems to surface minute-scale and months-scale work at the same level 'next step' — whereas months-scale work almost certainly needs a preliminary 'work breakdown' step (indeed, possibly needing a Mission structure of its own, or in some cases a novel and even bigger structure like a Campaign!)"*
- IDENTIFY drafted: claude-1, 2026-05-26 (same session)
- MAP drafted: claude-1, 2026-05-26 (same session)
- DERIVE drafted and corrected after `M-live-geometric-stack` read pass: claude-1, 2026-05-26 (same session)
- ARGUE drafted after pattern cross-reference into `futon3/library/`: codex-5, 2026-05-26
