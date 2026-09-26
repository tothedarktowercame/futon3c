# E-substrate-2-timetravel — substrate-2 points code at HEAD, so it can't time-travel code

**Date:** 2026-06-23
**Status:** IDENTIFY / charter (owned end-to-end by claude-2; E-prefix excursion).
**Provenance:** surfaced in [[M-G-over-cascades]] (futon2) VERIFY — the retrospective-∂κ spike's Phase-0 de-risk.
We wanted to read Ollivier-Ricci curvature `κ` over the code-graph *as of past commits* (to measure
tension-discharge over time), reached for XTDB `db-as-of`, and found the code structure is frozen at HEAD.
**Parents/links:** M-G-over-cascades (the consumer), M-live-geometric-stack (futon3 — the geometric layer this
fixes), the rung-3 Ollivier-Ricci demo (`futon5a/holes/tech-notes/misfit_rung3_curvature_demo.py`),
[[reference_substrate2_code_graph_is_head_snapshot]] (the memory), [[project_substrate_2]].

---

## 1. The finding (verified)

substrate-2 (futon1a / XTDB, bitemporal, served on :7071) does **not** version code *contents* over time.

- The `code/v05/edits` edge (commit → var) is documented as **"v0 HEAD-snapshot"**
  (`futon3c/src/futon3c/watcher/commit_ingest.clj:10`). The namespaces / vars / calls / contains structure is
  the **current** code re-posted on each ingest (`re-posting updates the same record`, line 23), **not** the
  structure as it was at each commit.
- What *is* genuinely temporal: the **commit spine** — `code/v05/commit` vertices carry `{timestamp}` props and
  `code/v05/precedes` chains commit_t → commit_{t+1} — and the **evidence** layer (`xtdb_backend.clj` sets XTDB
  valid-time to `:evidence/at`).
- Therefore `db-as-of <old-time>` over the code-graph returns the **HEAD** structure (or nothing pre-ingestion),
  never the historical graph. Confirmed by reading the ingestion model; the HTTP read path
  `/api/alpha/hyperedges` additionally returns "unknown endpoint" at present (query via Drawbridge instead).

So substrate-2's bitemporality covers **commits + evidence**, but the code model is a **HEAD mirror**.

## 2. Why "pointers to HEAD" is the wrong mental model

The advertised mental model (M-live-geometric-stack): *"the futon stack is one living object whose geometric
structure can be read in O(query) time"* — a **detailed, queryable, bitemporal model of the stack's code
contents**, from which `T, ∇T, ΔT, drift` are derived over the typed edges. Operator's framing of the defect
(Joe, 2026-06-23): *"a temporal database … pointing to HEAD, while we're trying to model what's actually in the
code … doesn't really add up."*

The point of a bitemporal store over code is to answer **"what did the code look like as of commit C / time
T?"** A store that re-posts HEAD for code structure cannot answer that — the temporality is real for *who
committed when* but vacuous for *what the code was*. So:

- **Wrong model:** "substrate-2 is a live, time-travelable model of the stack's code." (False for code contents.)
- **Actual model today:** "substrate-2 is a time-travelable model of **commits + evidence**, with a **HEAD
  mirror** of code structure hung off the latest ingest."

This is the precise successor of M-live-geometric-stack's **Gap A** (single edge-class → ∇T ≡ 0 in places) and
**Gap B** (geometric quantities ad-hoc, not derived): the geometric layer is only as alive as the substrate
underneath it, and an un-versioned code substrate makes the *temporal* geometry (`∂T/∂t`, drift, discharge)
unreadable for the code domain.

## 3. What a proper substrate-2 would enable — better Ollivier-Ricci κ modelling

If `code/v05/edits` (and the var/call/contains structure) were **versioned per-commit / by valid-time** —
i.e. `db-as-of(C)` returns the code-graph *as it was at commit C* — then:

- **`κ(C)` is a native query.** Ollivier-Ricci curvature over the code-graph at any commit C: reuse the rung-3
  OR core (`misfit_rung3_curvature_demo.py` / `pattern_curvature.py`), but sourced from `db-as-of(C)` instead of
  a git rebuild. Negative κ = high-tension cross-concern bridges, *as they existed then*.
- **`∂κ/∂t` (tension-discharge) becomes a first-class bitemporal query** — exactly the measurement
  M-G-over-cascades' VERIFY needs and currently can't get. `G(π) = expected tension-discharge across scales`
  stops being a git-pilot approximation and becomes a substrate read.
- **The War Machine reads tension live.** "Go to high-κ regions, do work, discharge tension" (the WM loop that
  `G` scores) becomes a standing query over substrate-2, not an offline script — closing the `[open]`
  Poly↔curvature seam (`futon5a/.../TN-joe-dt-explainer.md`) for the code domain.
- **The Axis-2 stack-usefulness ground truth is native.** A cascade's stack-wide value (did it discharge stack
  tension at its target region?) is read directly from `κ_after − κ_before`, rather than inferred from a git
  co-edit proxy.

## 4. Sketch of the fix (for a future implementation mission, not done here)

- **Version the code projection by valid-time.** On each commit ingest, write the var/call/contains edges with
  XTDB **valid-time = the commit's timestamp** (same mechanism `xtdb_backend.clj` already uses for evidence),
  so the structure at commit C is recoverable via `db-as-of`. Today they overwrite the same record (HEAD).
- **Two honest cost/scope questions to settle in that mission:** (i) storage/throughput of per-commit code
  structure across all repos (the watcher already touches every commit — the spine exists; this adds the
  structure payload); (ii) whether to version the *full* var-graph or a coarser file/namespace projection first
  (a file-level versioned graph already un-gates κ time-travel and is much cheaper — likely the v1).
- **Re-ingest history** (or accept that κ time-travel only works going-forward from the fix).

## 5. Scope

- **In:** the diagnosis (§1–2, verified), the case for the fix (§3), and a fix sketch (§4) to charter a future
  implementation mission.
- **Out:** implementing per-commit versioning; live OR-wiring; the M-G-over-cascades discharge measurement
  itself (which proceeds *now* via the **git co-edit pilot** — JVM-free, every commit a valid-time — precisely
  *because* substrate-2 can't yet serve it; this excursion is the record of why the pilot is a pilot).

## 5b. Developing the intuition: complexity, curvature, and aliveness (2026-06-23)

The VERIFY measurement (M-G-over-cascades Phase D) found alive missions' regions get *more* tense (κ more
negative), not less — and we first read that as a null. Joe's reframe (and "if AIF is true, this has to work →
we're mis-reading AIF"): we mapped `G` to **tension-discharge**, but AIF minimizes **expected free energy =
accuracy-error + complexity**. κ ("tension") tracks the **complexity** term, which morphogenesis *raises*. So new
structure increasing local tension is the **signature of growth**, not its absence. Aliveness is Salingaros
`L = T·H`: **high tension made harmonious**, not low tension. "Mess" is its opposite — high `T`, low `H`. Our κ
measure sees `T` but is blind to `H`, which is why it can't separate alive from mess.

**Two regimes, one physics (the soap bubble vs the daisy — Joe's examples).**
- **Soap film / minimal surface = passive curvature *minimization*.** It relaxes to least area: mean curvature
  → 0, Gaussian curvature ≤ 0 everywhere (minimal surfaces are saddles). Pure local minimization, *no new
  structure, no growth*. Free energy is **instantaneous and geometric** (surface energy ∝ area).
- **Daisy / living / inferential system = active curvature *organization* (morphogenesis).** Far from
  equilibrium; it *builds* structure — the Fibonacci seed-packing is high organized curvature, not a flat disc.
  Free energy is **expected-over-a-horizon, via a generative model**: minimizing it means *building the model*
  (structure that explains/enables), so curvature gets **organized into modules**, not zeroed.
- **Same mathematics at root (AIF is physics — variational free energy), different regime:** instantaneous-
  geometric (soap) vs expected-with-a-model (daisy). That is the careful distinction to hold. **We were applying
  the soap-bubble model (flatten the tension) to a daisy (organize the tension).**

**The curvature dictionary (intuition, to be pinned down on examples — not yet a theorem).** Ollivier-Ricci κ is
a discrete Ricci curvature: **κ < 0 = bridge / saddle / geodesics diverge** (a bottleneck, the *frontier*, the
`T` side); **κ > 0 = cluster / sphere / geodesics converge** (an integrated module, the `H` side). Note the
suggestive fact: a *purely minimizing* surface is all-saddle (K ≤ 0) — pure minimization tends toward
**all-bridge**. So "minimize tension" does **not** give harmony; harmony is **positive-curvature clustering**,
which a living system *builds*. `L = T·H` ≈ organized expressiveness: managed bridges (`T`) joining integrated
modules (`H`).

**So "discharge" must mean ORGANIZE (T→H), not FLATTEN.** Aliveness = converting tense frontier (new bridges,
κ<0) into organized modules (κ>0) **over time**, *while new frontier keeps growing*. Mess = tension accumulates
**un-organized** (bridges persist, nothing clusters). This is why Joe's *longer-horizon* instinct is likely
right: the conversion `T→H` is a process; one short window after a mission shows only the new frontier (κ down),
not the later integration (κ up at the parts that harmonize).

**Anamnesis / mana, grounded (Joe's examples).** Anamnesis = **un-discharged tension = work-not-yet-done**; it
isn't "wrong," it *takes work*. Joe had to ``find ./ -name "E-*.md" -mtime -2`` to re-situate himself — the trace
exists but isn't **organized** into a visible accreting structure, so re-situating costs work (anamnesis). An
ever-growing accreting view (the `pipeline-pattern-cascade.html` mockup) would have **organized** that tension
into form — then he could just *look* (mana: the work is already discharged into structure). At the largest
scale: building an agent took enormous work (anamnesis), but now any 25-year-old idea becomes a working prototype
nearly for free — **joy** in the Spinoza sense (conatus, increase in power-to-act = mana). **anamnesis → mana is
exactly `T → H`:** organizing un-discharged tension into harmonious capability, *not* flattening it.

**Implication for the measure (and method).** The corrected `G` is **expected `Δ(T·H)`** (harmonious growth), and
the test is **harmony `H` of new structure** — does a mission's new structure *cluster/integrate* (κ trends
positive) vs leave a *bridge-mess* (κ stays negative) — read statically at `T_after`, and/or its **rate of
organization over a longer horizon** (the tense frontier integrating into modules). Phase D's "alive raises `T`"
is *confirmation* of frontier growth; we must add the `H` reading. **Method (Joe): work at the level of
*examples* until the real pattern emerges** — the soap-bubble (minimal-surface) vs daisy (morphogenesis) contrast
is the first worked example; the exact `L ↔ κ` relationship (how complexity relates to differential-geometry
curvature for a *living* system, not a minimizing one) is the thing to develop next, carefully, before trusting
any single metric.

## 5c. Deep-research findings: AIF ∩ morphogenesis (2026-06-23)

*Inline focused pass (the background deep-research workflow kept getting torn down by session cycling; this is a
search-level synthesis, not the full adversarially-verified harness — confidence "strong directional," deep-fetch
to harden specific claims if needed). It strongly **validates** the §5b reframe.*

1. **Morphogenesis IS free-energy minimization toward organized structure (not flattening). [SOLID]** Friston,
   Levin & Pezzulo, *"Knowing one's place: a free-energy approach to pattern regulation"* (J. R. Soc. Interface
   2015): cells are minimal active-inference agents minimizing free energy to reach a **target morphology**;
   self-organization = gradient descent on a FE landscape whose **minima are the target form** (an attractor/goal).
   So a living system minimizes FE by *reaching organized structure*, not by relaxing to uniformity. Directly
   supports "aliveness = organized structure."
2. **Adding structure (complexity) is FE-rational when it buys accuracy. [SOLID]** Variational FE = **complexity +
   accuracy**; structure-learning = *model expansion* (accommodate new patterns) balanced by *Bayesian model
   reduction* (prevent over-fitting); the criterion is the trade — expand when accuracy-gain > complexity-cost,
   merge/reduce when complexity-saving > accuracy-loss (Smith et al., active-inference structure/concept
   learning). This is exactly our "morphogenesis raises complexity *now* to lower *expected* FE."
3. **The κ dictionary is the established Ollivier-Ricci reading. [SOLID]** Sia et al., *Ollivier-Ricci
   curvature-based community detection* (Sci. Rep. 2019): **κ<0 = bridges between communities (bottlenecks);
   κ>0 = within-community, well-connected = forms a module.** Distribution is **bimodal** (intra-community
   positive, inter-community negative). So our `T` = negative-κ frontier/bridges, `H` = positive-κ modules is not
   a metaphor — it's the standard interpretation. (Nuance: *Ricci flow* on networks expands negative / shrinks
   positive to *separate* communities — a tool, distinct from our "structure integrates over time" claim.)
4. **Living pattern formation = far-from-equilibrium dissipative structure (the daisy), distinct from
   equilibrium minimization (the soap bubble). [SOLID]** Turing reaction-diffusion + Prigogine dissipative
   structures: patterns emerge *far from equilibrium*, **positive feedback amplifies** (destabilize uniformity →
   frontier/pattern onset) and **negative feedback confines/stabilizes** (organize) — order sustained by
   dissipation, not relaxation to a minimum. Confirms the soap-bubble vs daisy distinction is real and the
   amplify-then-confine structure mirrors our `T`-grows-then-`H`-organizes process.
5. **FEP is grounded in dissipative-structure thermodynamics, but the Alexander/Salingaros ↔ AIF bridge is
   UNBUILT. [SPECULATIVE / our contribution]** The FEP explicitly rests on Prigogine (open systems self-organize
   via energy dissipation; a Markov-blanket system minimizes variational FE to hold its non-equilibrium steady
   state). But the searches found **no published work linking Alexander's "living structure"/wholeness (or
   Salingaros's geometric formalisation) to the FEP/active inference.** So `L = T·H ↔ AIF` is a *novel* synthesis
   we'd be making, not derivative — opportunity, but flag it as ours, unverified by prior art.

**What it means for `G(π)`.** The AIF-correct objective is **net expected-FE reduction = accuracy-gain −
complexity-cost**; `G(π) = expected Δ(T·H)` is a reasonable *proxy* (T·H rises exactly when added complexity
integrates into modules), and the literature gives the sharper read: reward complexity (T, negative-κ frontier)
**only when it converts to integration** (H, positive-κ modules) — i.e. structure that lowers future surprise.
**Borrowable formalisms:** (a) Ollivier-Ricci + the bimodal community reading for the `T`/`H` geometry; (b) the
complexity/accuracy decomposition + Bayesian model expansion/reduction for the "when is new structure justified"
criterion; (c) "Knowing one's place" as the morphogenesis-as-FE grounding. **Sources:**
[Knowing one's place (RSIF 2015)](https://royalsocietypublishing.org/rsif/article/12/105/20141383),
[AIF, morphogenesis & computational psychiatry (2022)](https://pmc.ncbi.nlm.nih.gov/articles/PMC9731232/),
[AIF structure/concept learning (bioRxiv)](https://www.biorxiv.org/content/10.1101/633677v4.full),
[Ollivier-Ricci community detection (Sci Rep 2019)](https://www.nature.com/articles/s41598-019-46079-x),
[Non-equilibrium thermodynamics & the FEP in biology](https://link.springer.com/article/10.1007/s10539-021-09818-x),
[Turing morphogenesis commentary (Phil Trans R Soc B)](https://royalsocietypublishing.org/doi/10.1098/rstb.2014.0218),
[Dissipative structures in biological systems](https://pmc.ncbi.nlm.nih.gov/articles/PMC6000149/),
[Alexander living structure (Jiang 2019)](https://arxiv.org/pdf/1909.11757).

## 6. Exit criterion

Joe ratifies the diagnosis + the fix direction (file-level versioned code projection first), and the future
implementation mission is named. Until then, M-G-over-cascades uses the git co-edit pilot and treats native
substrate-2 κ time-travel as the post-fix upgrade.
