# Deep Research — AIF ∩ Morphogenesis

**Question:** Active inference / free-energy principle and the formation of living structure, across five threads (FEP morphogenesis; complexity-vs-accuracy of structure-building; minimal-surfaces vs living curvature-organization + network Ricci curvature; Alexander/Salingaros "life" L=T·H ↔ AIF/dissipative structures; the temporal frontier→module angle and its rate/measure).

**Provenance:** salvaged from the deep-research workflow run `wf_f29d5d92-a11` (2026-06-24), which was killed at ~604s mid-Verify before it could synthesize. **23 sources fetched, 109 claims extracted (22/23 sources primary peer-reviewed/preprint).** Adversarial verification completed on only ~10 of the top-25 claims before the kill — every claim that *did* finish came back confirmed (mostly 3–0), but most claims below are **extracted-from-primary-source, not yet adversarially cross-checked.** Treat confidence accordingly; this is the salvage, not the gated output. See `E-deep-research-hardening.md` for why the run died.

---

## Executive summary

There is a mature, internally consistent body of work casting **morphogenesis as variational Bayesian inference**: a tissue is a population of cells that share one generative model of the target form, and development is gradient descent on a variational free-energy landscape whose minima are the target morphology (Friston–Levin lineage, 2015→2020). The engine that lets such structure *grow* is the **exact accuracy-minus-complexity decomposition** of (expected) free energy: model expansion is accepted only when added complexity is paid back by improved evidence/expected free energy — Bayesian Occam's razor, with niche construction as its in-the-world limb. The differential-geometry thread has two clean poles: **passive curvature-minimization** (minimal surfaces / soap films, vanishing mean curvature) versus **active curvature-organization** (discrete Ricci-curvature and Ricci-*flow* on networks, where negative-curvature edges are frontier/bottlenecks and positive-curvature edges are modules), and a genuine information-geometry bridge in between (a Markov-blanketed system carries a Fisher-information geometry on its belief space). The Alexander/Salingaros **L = T·H** "life" measure is a real, geometry-derived scalar with an explicit thermodynamic *analogy* (entropy = 10−H), and there is modern machinery (self-evidencing dissipative systems) that formally unites Prigogine dissipative structures with free-energy minimization — but **no source closes the loop between L=T·H and AIF formally; that link remains analogical/open.** The temporal angle is the best-supported surprise: **network Ricci-flow and dynamical Ollivier-Ricci curvature give an explicit time/scale parameter and a candidate order parameter (the "curvature gap") for frontier→module consolidation, with measured rates** (≈100 flow-iterations on Karate, ≈20 on a 792-node FB ego net; sharp ~100%→0% detectability phase transitions).

---

## Thread 1 — FEP / active-inference accounts of morphogenesis

**Core claim (well-supported, partially verified).** Friston, Levin, Sengupta & Pezzulo cast morphogenesis as variational free-energy minimization: each cell minimizes free energy to *infer its location* in the body plan, which is equivalent to maximizing Bayesian model evidence [knowing-one's-place, J.R.Soc.Interface 2015, rsif.2014.1383]. Every undifferentiated cell shares **one** generative model of the ensemble; the global free-energy minimum is reached exactly when each cell occupies a unique location and has correctly inferred its place — which the authors argue *guarantees* pattern formation and supplies a natural stopping condition for growth/remodelling [ibid.].

- Self-assembly is an *emergent* property of cells sharing a common genetic+epigenetic generative model; "any process of self-organization [can be converted] into a gradient descent on a free energy landscape" [Morphogenesis as Bayesian Inference, Kuchling-Friston-Georgiev-Levin, Phys. Life Rev. 33:88–108, 2020].
- **Action is intrinsic, not passive sensing:** cells migrate over concentration gradients and release/express signals to *fulfil their own predictions* and close prediction-error gaps [rsif.2014.1383] — morphogenesis is active inference.
- **Genetics → model parameters; epigenetics → model inversion.** Genetic codes parametrize the generative model (what a cell should express/sense where); signalling-dependent transcription is the inversion of that model [ibid.].
- **Attractor = target morphology.** Free-energy basins/minima are goal states; "all cells, not only neurons, are free-energy minimising agents, whose prior expectation is to reach a target morphology" — directly mapping Levin's bioelectric anatomical setpoints onto FEP priors [Phys. Life Rev. 2020]. ✓ *adversarially confirmed 3–0.*
- **Pattern regeneration falls out for free:** perturbed populations (split embryos) dynamically reconfigure and re-converge to the same target morphology [rsif.2014.1383].
- **Markov blanket** (Cartesian product of a cell's sensory × active states) is the formal device securing conditional independence of internal/external states, letting a cell be treated as an inferential boundary [Phys. Life Rev. 2020].
- **The bioelectric half (Levin):** voltage gradients specify anatomical pattern partly independently of genetic sequence (tested by manipulating resting membrane potentials in planaria/tadpoles); bioelectric networks are "cognitive glue" / distributed information-processing [drmichaellevin.org publications; Animal Cognition 2023]. Institutional link is concrete: Kuchling, Levin (Allen Discovery Center, Tufts) + Friston (UCL) co-author the variational-morphogenesis program [PubMed 32682906].

**Foundation underneath it:** Bayesian mechanics gives sufficient+necessary conditions for a system's internal states to *infer* its external states; the FEP is a biology-specific tightening of those constraints [arXiv:2410.11735]. At non-equilibrium steady state the flow of internal states is a **gradient flow ascending Bayesian model evidence** [Parr–Da Costa–Friston, PMC6939234].

---

## Thread 2 — complexity-vs-accuracy: how building structure pays off

**The exact decomposition (central, repeatedly attested).** Variational free energy = **complexity − accuracy**, where complexity is the KL divergence of the (approximate) posterior from the prior and accuracy is the expected log-likelihood; minimizing free energy = maximizing model evidence = "minimising the complexity of accurate explanations" = Ockham's razor [Supervised Structure Learning, Friston et al., arXiv:2311.10300; De Novo Learning, Entropy 27(9):992, 2025; Whence the EFE, Neural Computation 33(2), 2021].

**The gate for when adding structure is worth it.** Structure learning has two complementary moves:
- **Bottom-up model expansion** — grow the model (new latent state / path / parameters) *only when* a new observation cannot be explained by existing latent causes [Smith et al., bioRxiv 633677; Entropy 2025].
- **Top-down Bayesian model reduction** — post-hoc pruning/merging to recover complexity [arXiv:2311.10300].

The explicit acceptance criterion: an augmented model is accepted over its parent **only if it both raises marginal likelihood (ΔF) and improves expected free energy / mutual information (ΔG)** [arXiv:2311.10300]. Absent preference constraints, negative expected free energy *reduces to the mutual information* between latent states and outcomes, so expansion is favored precisely when it raises that mutual information [ibid.]. This is the formal sense in which "raise complexity now → lower expected free energy later" is rational.

**Expected free energy as the forward-looking ledger.** EFE decomposes into extrinsic (goal/value) + intrinsic (epistemic, expected information gain) terms; because information gain enters negatively, minimizing EFE *mandates* maximizing information gain — exploration is intrinsic, not bolted on [Whence the EFE]. Equivalently EFE = risk + ambiguity (expected complexity becomes risk; expected inaccuracy becomes ambiguity) [arXiv:2311.10300]. **Caveat the source itself stresses:** the epistemic term does *not* arise merely from projecting VFE into the future — the "Free Energy of the Future" actually *penalizes* exploration; the EFE's epistemic drive has to be added in (EFE = FEF − expected information gain), which Millidge et al. flag as a genuine foundational wrinkle [Whence the EFE].

**Niche construction = structure-building in the world, same ledger.** Agent and eco-niche share one Markov blanket and so mutually infer each other; free energy is jointly minimized at **generalized synchrony** [Constant et al., rsif 2017.0685]. The free-energy minimum is not a point of maximal adaptation to a static environment but an **attracting manifold in the joint agent-environment state space** [Bruineberg et al., PMC6117456]. Natural selection itself is castable as Bayesian model selection over genetically-encoded priors [Constant et al.].

---

## Thread 3 — minimal surfaces (passive) vs living curvature-organization (active); information geometry

**The contrast pole (passive).** Discrete minimal surfaces = simplicial complexes minimizing area / vanishing mean curvature (soap films), constructed via discrete Weierstrass/circle-pattern representations [Lam & Yasumoto, arXiv:2510.23757, *search-surfaced, not fetched*]. This is curvature-*minimization*: the structure relaxes to the lowest-area state.

**The active pole — network Ricci curvature as a module/bottleneck discriminator (strongly attested across 4 independent sources):**
- **Ollivier-Ricci** via Wasserstein optimal transport over random walks: κ<0 edges are heavily-trafficked **bottlenecks/bridges between communities** (the frontier); κ>0 edges are well-connected and "naturally form a community" (modules) [Sia/Ni et al., Sci. Rep. s41598-019-46079-x and -46380-9].
- **Forman-Ricci** (combinatorial, scalable Bochner–Weitzenböck analog): high-curvature edges concentrate in dense hubs/communities, explaining the curvature↔scale-freeness correlation [Weber–Saucan–Jost, arXiv:1607.08654].
- A 2025 survey unifies the zoo (Ollivier, Forman, Bakry-Émery, sectional) and confirms the **shared qualitative feature: all distinguish bottleneck regions (negative) from clustered regions (positive)**; sectional curvature places a graph on a spectrum from tree-like (negative, frontier) to fully-connected (positive, module) [arXiv:2510.22599]. Curvature is *actionable*: curvature-informed rewiring fixes GNN over-smoothing/over-squashing.

**The information-geometry bridge (the genuinely deep link).** Any weakly-mixing random dynamical system with a Markov blanket is **necessarily equipped with an information geometry** [Parr–Da Costa–Friston, PMC6939234]. Concretely it carries a **dual Fisher-information geometry** — two metric tensors built from the Hessian of KL divergence: g_λ on densities over internal states, and g_μ treating internal states as *parameterising densities over external states* (the internal manifold is a belief space about the world) [ibid.]. Optimal active-inference behaviour is then **paths of least action on a statistical manifold whose metric is the Fisher information and whose Lagrangian is variational free energy** [De Novo Learning, Entropy 2025; Brain-like Variational Inference / natural-gradient, arXiv:2410.19315]. *This is the Fisher-curvature counterpart to network Ricci curvature — curvature of belief-space rather than of a graph; no fetched source fuses the two metrics into one object, so the "Fisher ↔ Ricci" unification is an open synthesis, not a cited result.*

---

## Thread 4 — Alexander/Salingaros "life" (L=T·H) ↔ AIF / dissipative structures

**The L=T·H measure is real and geometry-derived (central, primary).** Salingaros: a building's life **L = T·H**, where **T** (architectural "temperature") = degree of detail, **curvature**, and color, and **H** (harmony) = visual coherence / internal symmetry [Salingaros, "Life and Complexity in Architecture From a Thermodynamic Analogy," 1997, architexturez az-cf-172617]. Note curvature is a *first-class* contributor to T — curvature as life-giving, not relaxed-away.

**The thermodynamic analogy is explicit but stated as analogy.** Salingaros defines **architectural entropy = (10 − H)** and perceived complexity **C = T·(10 − H)**; lowering entropy (rearranging toward harmonious, least-entropy interactions) is claimed to "[conform] to the process that gives rise to natural forms" [ibid.]. A reference page explicitly flags how literal-vs-metaphorical this mapping is [eoht.info architectural thermodynamics, *search-surfaced*].

**Alexander's "wholeness" has a network formalization.** Bin Jiang models wholeness via complex networks + a scaling hierarchy (head/tail breaks) over Alexander's 15 fundamental properties; "structural beauty emerges from local actions" — a bottom-up/self-organizing reading [arXiv:1602.08939; 1909.11757]. **But Jiang gives no closed-form L=T·H and no thermodynamic linkage** — so the network formalization and the thermodynamic measure are two separate partial formalizations.

**The AIF/dissipative-structure pole exists and is rigorous.** A 2025 architecture explicitly unifies **Prigogine's dissipative structures + Friston's free-energy minimization + Hopfield attractors**, framing a living/cognitive system as a dissipative structure maintaining its non-equilibrium steady state by minimizing variational free energy via structural self-organization [arXiv:2510.17916]; it even drives itself to self-organized criticality (spectral radius ρ≈1, edge of chaos). Bayesian mechanics ties belief-updating to **entropy production** (heat dissipated along a path = surprise of the time-reversed trajectory) [PMC6939234; arXiv:2410.11735], which is the Prigogine anchor.

**Honest verdict on the formal link:** the two poles (geometric L=T·H "life"; AIF/dissipative-structure self-evidencing) each exist and both use a thermodynamic vocabulary, but **no source in the corpus formally maps L=T·H onto a free-energy / entropy-production functional.** arXiv:2510.17916 supplies the machinery onto which such a map *would* be built but does not cite Alexander. **This is the biggest genuine gap — the link is currently analogical and unbuilt.**

---

## Thread 5 — the temporal angle: frontier (κ<0) → modules (κ>0), and its rate/measure

**This is the best-supported "is there a rate?" result in the corpus.**

- **Discrete Ricci *flow* on networks** deforms edge weights over flow-time: positive-curvature (intra-module) edges shrink, very-negative-curvature (inter-module bottleneck) edges stretch, until thresholding surgically separates communities — an *active* frontier→module organization over time [Ni-Lin-Luo-Gao, Sci. Rep. s41598-019-46380-9]. **Measured rates:** ≈100 flow-iterations to a separable curvature-equalized state on Zachary's Karate club; ≈20 iterations on a 792-node / 14,025-edge Facebook ego network. Near-perfect recovery (Adjusted Rand Index) when modules exist, with a **sharp ~100%→0% detectability phase transition** acting as a network order parameter.
- **Dynamical Ollivier-Ricci curvature** defines κ_ij(τ) = 1 − W₁(p_i(τ),p_j(τ))/w_ij over a *continuous* diffusion-time τ. As τ grows the community structure *unfolds multiscale*: small τ = every node its own community (fragmented frontier), large τ = one community; **at intermediate scales edges carry mixed ±curvature separating modules from bottlenecks** [Gosztolai & Arnaudon, Nat. Commun. s41467-021-24884-1]. The candidate **order parameter / rate measure is the "curvature gap"** Δκ(τ) = (mean within-community curvature − mean between-community curvature)/σ, quantifying how strongly modular structure has formed at each timescale; gaps in the curvature distribution mark bottlenecks at characteristic scales.
- **Forman-Ricci flow** gives a discrete per-step rate: γ̃(e) − γ(e) = −Ric_F(γ(e))·γ(e), unit clock steps; positive-curvature regions shrink/reach steady state *faster*, negative-curvature regions expand and take *longer* — a curvature-dependent timescale for module consolidation vs frontier expansion [arXiv:1607.08654].
- **Convergent evidence from learning systems:** during ANN training, the correlation of *flexibility* with accuracy **peaks before** the correlation of *modularity* with accuracy — dynamic metrics are precursors of emergent modular representations [Sci. Adv. adm8430, 2024, *search-surfaced*]; and a local "topological reinforcement" rule (stabilize within-group, prune between-group links) drives unstructured networks to modular organization [Damicelli et al. 2019, *search-surfaced*]. On the FEP side, TFM-guided growth/pruning self-organizes a network to the edge of chaos [arXiv:2510.17916], and de novo structure learning **grows-then-reduces** a model until it discovers a pullback attractor [Entropy 2025].

**Synthesis for thread 5:** there *is* a rate/measure, and it is curvature-based. The frontier→module story is literally a curvature-sign sorting that proceeds along a (diffusion- or flow-) time axis, with the **curvature gap** (continuous) or **iteration-count-to-phase-transition** (discrete flow) as the order parameter. The open move is to run the *same* clock on a free-energy landscape — i.e. show that AIF structure-learning's grow-then-reduce trajectory has a curvature gap that evolves like the network ones.

---

## Cross-thread bridges (the load-bearing connective tissue)

1. **Markov blanket = the shared hinge.** It makes a cell an inferential agent (T1), induces a Fisher information geometry (T3), and carries an entropy-production / dissipative-structure reading (T4). One object, three threads [PMC6939234; Phys. Life Rev. 2020].
2. **One free-energy landscape, two readings of curvature.** Morphogenesis = gradient descent to attractor basins (T1); network self-organization = Ricci-flow to curvature-equalized modules (T3/T5). Both are descent-to-attractor on a curved space; nobody in the corpus has *identified* the morphogenetic free-energy Hessian with a network Ricci tensor — that is the natural next formal target.
3. **"Build structure now, pay later" is the same ledger in three places:** model expansion (T2), niche construction (T2), and module formation (T5) all trade present complexity for future expected-free-energy / uncertainty reduction.

## Caveats
- **Verification incomplete.** ~10 of the top-25 ranked claims finished 3-vote adversarial checking before the run was killed; the rest (including most of threads 3–5) are extracted-from-primary-source but not adversarially cross-checked. Where a single source carries a thread (e.g. the curvature-gap rate measure rests on one Nat. Commun. paper), treat as medium confidence.
- **Search-surfaced ≠ fetched.** The minimal-surfaces pole (T3), the Prigogine grounding, and the two ANN-modularity papers (T5) come from search snippets, not full fetches — their specific claims are unverified.
- **The headline gap is real:** no source formally links L=T·H to a free-energy functional. Reported as the clearest open problem, not papered over.

## Open questions
1. Can the morphogenetic free-energy Hessian be identified with (or bounded by) a discrete Ricci curvature on the cell-signalling graph — making "morphogenesis = Ricci flow on a belief network" a theorem rather than an analogy?
2. Does an AIF structure-learning run exhibit a measurable **curvature gap Δκ(τ)** over its grow-then-reduce trajectory, with the same phase-transition signature as network Ricci flow?
3. Is there a principled map from Salingaros's (10−H) "architectural entropy" to a variational complexity term — i.e. is L=T·H recoverable as accuracy×(−complexity) on some generative model of perception?
4. The Millidge "Whence the EFE" wrinkle: if the epistemic drive isn't native to projecting VFE forward, what is the right forward-looking objective for *morphogenetic* exploration specifically?

---

## Grounding audit — is M-wm-policies Track 3 (and the Car-3 act-gate) *actually* AIF? (claude-2, 2026-06-24)

*Joe's check: not "compliant with R1–R13" but "a real implementation of the model — grounded in this research
where possible, with omissions named." Mapping each thing we built against the report. Verdict per component:
**GROUNDED ✓** (a cited AIF result backs it), **ANALOGICAL ~** (right shape, but the quantity is a hand-proxy or
a non-AIF analogy), **OMISSION ✗** (the AIF version is unbuilt). Confidence inherits the report's own caveats.*

1. **`:acquire-patterns` = model expansion triggered on the abstain/flat field — GROUNDED ✓ (strongest).**
   Thread 2: bottom-up structure learning grows the model "*only when* a new observation cannot be explained by
   existing latent causes" [arXiv:2311.10300; Entropy 2025]. The abstain regime = the existing action-set cannot
   confidently handle the situation = exactly that trigger. Niche-construction-on-stall **is** AIF structure
   learning, not a metaphor. (T2 is "central, repeatedly attested"; partially adversarially checked.)

2. **The ADDED, separate epistemic term — GROUNDED ✓ in FORM; ANALOGICAL ~ in MAGNITUDE.** The Millidge "Whence
   the EFE" wrinkle (line 41) is decisive *for us*: the epistemic drive does **not** fall out of projecting VFE
   forward (that *penalizes* exploration) — it must be **added** (EFE = FEF − expected info gain). We add it as a
   distinct `λ_e·epistemic` term → structurally correct. **But** our value `0.9·flatness·low-confidence` is a
   hand-crafted proxy, **not** computed expected-information-gain / mutual-information (T2: epistemic value = MI
   between latent states and outcomes). **Omission ✗:** approximate real EIG — the reduction in uncertainty over
   *which patterns/policies actually work* — rather than a flatness heuristic.

3. **The conjunctive act-gate ("act only where both grains agree") — GROUNDED ✓ in FORM.** Thread 2 gives the
   exact acceptance criterion: an expanded model is accepted over its parent **only if it raises marginal
   likelihood (ΔF) AND improves expected free energy / MI (ΔG)** [arXiv:2311.10300]. Our "both grains must concur"
   is a heuristic of that conjunction — the *form* is research-correct (need both, not either).

4. **grain-3 rollout `G(π)` = the ΔG leg — GROUNDED ✓.** It *is* expected free energy (the forward-looking
   ledger, T2). This leg of the gate is genuinely AIF.

5. **grain-2 cascade `C` = Salingaros wholeness as the other gate leg — ANALOGICAL ~ (the report's headline gap).**
   Thread 4's honest verdict: **no source formally maps L=T·H onto a free-energy / entropy-production
   functional** — it is "the biggest genuine gap … analogical and unbuilt" (line 70, 96). So using `C` as the
   ΔF/model-evidence leg is an **analogy**, not AIF. `C`'s `T` (expressiveness)≈accuracy and `(10−H)`≈complexity
   *rhyme* with accuracy−complexity, which is why it tempts — but the bridge is uncited. **Omission ✗:** the AIF
   ΔF leg = the **marginal-likelihood gain (accuracy − complexity)** of the expanded model; `C` is a placeholder
   for it, to be either labelled-analogical or replaced.

6. **Ollivier-Ricci tension metric (M-G-over-cascades DERIVE) — GROUNDED ✓ (strongly).** Thread 3/5, **4
   independent sources**: κ<0 = frontier/bridge, κ>0 = module; the **curvature gap Δκ(τ)** is a real order
   parameter with measured rates. The metric choice was *not* an implementation detail and the research backs it.

7. **"policies across scales" / extensible operator-registry (aif2) — GROUNDED ✓.** T2 niche construction =
   generalized synchrony; the FE minimum is an **attracting manifold** in joint agent-environment space, not a
   point [PMC6117456] — consistent with `G(π)` being a path, not a field.

**Verdict.** Track 3's **structure is genuinely AIF** — model-expansion-on-inadequacy (1) + an added epistemic
drive (2) + a conjunctive accept-gate (3) + an EFE rollout (4), with an attested curvature metric (6). That is
*not* R1–R13 box-ticking. **But two quantitative legs are analogical proxies, not computed variational
quantities:** the epistemic *magnitude* (2) and the Salingaros-`C` gate leg (5) — and (5) is precisely the link
the report names as unbuilt across the whole literature.

**Implication for "closing blockers" (Joe's point, confirmed).** The Car-2/3 coverage fixes (ψ-stability,
move-set coverage, signal magnitude) are technical — **but two of them touch analogical machinery, so closing
them would harden a proxy, not the model.** Before treating them as the real implementation:
- **Re-scope the Car-3 act-gate explicitly as `ΔF ∧ ΔG`** with grain-3 = ΔG (real EFE, keep) and grain-2 either
  (a) **labelled an explicit analogical heuristic** (honest interim) or (b) **upgraded to a real ΔF** =
  marginal-likelihood gain of the acquired cascade-as-model-fragment.
- **Upgrade the epistemic term toward EIG** (expected reduction of uncertainty over which patterns work), not
  flatness.
These are **model decisions, not technical fixes** — which is exactly why Joe paused before closing blockers.

**UPDATE 2026-06-24 — both omissions closed (M-wm-policies Track 3).** (2) the epistemic term is now real
**expected information gain** = normalized Shannon entropy of the action-posterior (`policy.clj/posterior-entropy`,
injected via `:epistemic-override`), replacing the flatness proxy. (5) grain-2 is now a real marginal-likelihood
**F = accuracy − λ·complexity** with **complexity = Σ −log(co-application base-rate inclusion prior)** — a direct
build of this report's **open-Q3** (map `(10−H)` "architectural entropy" → a variational complexity term);
implemented in `cascade_construct.py`, gating `gap-lane` on F≤0 (the AIF accept criterion). λ=0.25 set from the
data knee; a categorical-prior scale-bug was caught and fixed via the sweep. So the Car-3 gate `ΔF ∧ ΔG` is now
AIF on both legs. The one piece of open-Q3 NOT built: this grounds `(10−H)→complexity` operationally for the
cascade scorer, but does not yet *prove* L=T·H = accuracy×(−complexity) as a theorem — the formal identity
remains the open research target, **now chartered as `E-prove-salingaros-cascade-scorer.md`** (settle it:
prove the identity, show monotone-equivalence for the gate, or a clean negative — the live data already hints
`F` and `C=T·H` don't co-rank, so a clean positive identity is unlikely).

**Confidence.** GROUNDED items lean on the confirmed/strongly-attested claims (morphogenesis=FEP + target=prior
is 3–0 adversarially confirmed; Ollivier-Ricci frontier/module is 4-source; the structure-learning gate is
central but only partially cross-checked). The ANALOGICAL verdict on (5) rests on the report's *own* flagged gap.
So the audit is as solid as the report — and the report is honest about where it isn't.

---
*Salvaged 2026-06-24 from workflow `wf_f29d5d92-a11` artifacts. 23 fetched sources / 109 extracted claims in `salvage.json`. Full source list with dates in the workflow record.*
