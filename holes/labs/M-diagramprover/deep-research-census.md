# Deep-research census — literature + reference implementations for the causal diagram engine

**Run:** 2026-08-02, claude session with web access. Executes
`deep-research-spec.md` (written same day).
**Reads against:** WS-A as landed — `52cb3fae` (graph + rule), `d07e4442`
(lazy convex matching), `07115ef8` (DPO rewriting relation);
`futon3c/src/futon3c/diagramprover/{graph,matcher,rewrite,rule}.clj`
(598 lines) plus four test namespaces, working tree clean.

**Timing caveat, stated up front.** The spec says "run after WS-A lands
*and is reviewed*." WS-A has landed (all four modules + tests committed);
I have not seen a completed owner review of it in this session. The
census below therefore reads against the *committed kernel's shape and
stated design constraints*, not against a review verdict. Nothing in the
deltas depends on review outcome; if the review changes the kernel API,
re-check delta D1 in particular.

## Reading levels (honest, per the spec's rule)

- **[source]** — I read the actual code/API surface in a local clone.
- **[read]** — I read the linked page/documentation body.
- **[abstract]** — abstract, landing page, or search-result snippet only;
  the paper body was NOT read this pass.

Anything marked [abstract] is a lead, not a verified claim about the
paper's contents beyond its abstract. Local clones inspected:
`/home/joe/code/diagramprover-refs/{chyp,pyzx,dagitty,y0,causaleffect}`.

---

## RQ1 — Diagram rewriting engines beyond chyp

**Verdict.** Nothing found beats the chyp port for our purposes, and the
port choice is retrospectively vindicated: chyp is the *named successor*
to Quantomatic, which is explicitly unmaintained. But the census turns up
one thing that does change the plan — the rewriting regime we ported
(SDRT-II: plain SMC, convex DPO) is provably **the wrong regime for
WS-B's Markov-category rule set**, and the right one has been published
with its own DPO characterisation (Milosavljevic–Piedeleu–Zanasi, LMCS
2025). DisCoPy is the only other implementation with a `markov` module,
and is worth keeping as a differential-testing oracle, not as a
substrate.

### 1. Rewriting modulo commutative (co)monoid structure — the theory gap
- **What it is.** Milosavljevic, Piedeleu, Zanasi, *Rewriting String
  Diagrams with Commutative Monoid Structure*, LMCS 21(1), 31 Jan 2025.
  arXiv:2204.04274. [abstract, plus abstract fetched in full]
- **Why it matters to us.** This is the single highest-value finding in
  the whole census. The prior state of the art forced a choice: rewrite
  modulo **full Frobenius** (SDRT-I — too strong; it identifies copy with
  merge) or modulo **plain SMC** with a **convexity** side condition
  (SDRT-II — what chyp implements, what we ported). A Markov category is
  an SMC with a **commutative comonoid** (copy/discard) on every object —
  strictly between the two. This paper shows rewriting modulo commutative
  (co)monoid equations is soundly and completely interpretable as DPO
  rewriting of hypergraphs, with a soundness condition *other than*
  convexity. That is exactly WS-B's regime.
- **Consequence for the landed kernel.** Our `matcher.clj` enforces
  convexity, which is the SDRT-II condition. If WS-B introduces copy and
  discard as ordinary generators with explicit rules, the engine will
  work but will not rewrite *modulo* comonoid structure — every
  copy-associativity/commutativity shuffle becomes an explicit rewrite
  step, and normal forms are no longer unique up to the structure we
  actually want to quotient by. This is a correctness-of-abstraction
  issue, not a performance one.
- **What to mine.** The DPO formulation and its soundness condition
  (§ of the paper giving the hypergraph interpretation and the
  replacement for convexity); the completeness proof, to know exactly
  what is and isn't quotiented.
- **Effort:** [read] first — a decision-grade read before any WS-B code.
  Then likely a bounded extension of `matcher.clj`/`rewrite.clj`, not a
  rewrite.
- **License:** paper, CC-BY (LMCS). No implementation known.

### 2. chyp — the incumbent, correctly chosen
- **What it is.** Aleks Kissinger, *Chyp* (Cospans of HYPergraphs), an
  interactive theorem prover for string diagrams. Apache-2.0, 129 stars,
  274 commits; our clone is at `3d28483` (2024-10-15). [source]
- **Why it matters.** Confirmed by upstream docs: chyp implements SDRT-II
  and states "fancier types of rewriting (e.g. rewriting modulo Frobenius
  structure) is planned for the future" — i.e. the restriction we
  inherited (`NotImplementedError` on non-(1,1) boundary vertices in
  `explode_vertex`) is chyp's own frontier, not a shortcut we took.
  Quantomatic's own page names chyp and ZX Live as the two projects
  carrying its goals forward; Quantomatic itself is "no longer being
  actively maintained."
- **What to mine (still unported).** `term.py` (term↔graph, deferred in
  WS-A as non-load-bearing); `proofstate.py` + `checker.py` if WS-D ever
  wants a proof-assistant surface over the engine rather than a library.
- **Effort:** [port], already largely done.
- **License:** Apache-2.0 — compatible.

### 3. DisCoPy — the only other implementation with Markov categories
- **What it is.** BSD-3-Clause, 431 stars, 2043 commits, actively
  maintained through 2026. Two diagram data structures: layer-lists for
  planar monoidal categories, and **cospans of hypergraphs** for
  symmetric monoidal categories — the same representation as chyp. Has
  `discopy.hypergraph` and, critically, **`discopy.markov`** (a `Functor`
  that preserves copies). Papers: arXiv:2311.10608 (Hierarchy of
  Graphical Languages, v1.0), arXiv:2205.05190 (quantum). [read: repo +
  docs index; abstract: papers]
- **Why it matters.** It is the closest thing to an existing
  implementation of the copy/discard structure WS-B needs, and its
  hypergraph representation is close enough to ours that
  diagram-equality and normal-form results are directly comparable.
- **What to mine.** (a) `discopy.markov` — how copy/discard are
  represented and what `Functor` preserves; (b) the diagram-equality
  decision procedure for the SMC/hypergraph fragment, as a *differential
  test oracle* against our `find-iso`; (c) its `Hypergraph` boundary
  handling, for the non-(1,1) case chyp punts on.
- **Honest limit.** DisCoPy is a *computation* library (functors into
  tensors, quantum backends); it is not a rewriting engine in chyp's
  sense — its rewriting is diagram-surgery-level, not DPO-with-matching.
  It does not do do-calculus or identification.
- **Effort:** [oracle-only] recommended. Porting would mean adopting a
  second representation; FFI (Python) is possible but buys little we
  don't already have.
- **License:** BSD-3-Clause — compatible.

### 4. Catlab.jl + AlgebraicRewriting.jl — more general, wrong ergonomics
- **What it is.** AlgebraicJulia. MIT, 41 stars, 250 commits, actively
  developed, **API explicitly unstable**. Implements DPO, SPO, SqPO (and
  per its docs PBPO) generically over **ACSets/C-Sets** in any adhesive
  category; the theory is Brown et al., *Computational
  category-theoretic rewriting* (JLAMP 2023) and *Double pushout
  rewriting of C-sets*. [read: repo README; abstract: papers]
- **Why it matters.** This is the most *general* rewriting engine in the
  census — C-Set rewriting subsumes hypergraph rewriting. If we ever want
  rewriting over a richer schema than typed open hypergraphs (e.g.
  diagrams carrying evidence annotations as a schema, not as payload
  keys), this is the published route.
- **Why it does not displace the chyp port.** (a) It is Julia — a third
  runtime in a stack whose I-0 invariant is "one JVM"; (b) generic C-Set
  DPO gives you *pushout complements*, not the **convex matching** that
  makes SMC string-diagram rewriting sound — the SDRT soundness condition
  is not in the box, we'd have to add it anyway; (c) unstable API.
- **What to mine.** The pushout-complement existence/uniqueness handling
  — the trickiest part of `rewrite.clj` — as a cross-check on our
  implementation's edge cases.
- **Effort:** [read] for the pushout-complement treatment; [FFI]
  rejected.
- **License:** MIT — compatible.

### 5. Quantomatic — dead, do not mine
- Unmaintained by its own project page (dream.inf.ed.ac.uk /
  github.com/zxcalc/quantomatic). Named successors: chyp, ZX Live.
  Mentioned in the mission's WS-A porting-material list; **that mention
  should be dropped** — its value is entirely historical. [read]

### 6. PyZX / ZX Live / VyZX — ZX-specific, not general rewriting
- PyZX (Apache-2.0, clone at `0f54edb`, 2026-08-01 — actively released,
  0.10.5) is a ZX-calculus rewriting library: its rules are *fixed*
  (spider fusion, local complementation, pivoting), not a pluggable rule
  set over a general SMC. [source: clone] Useful only as a benchmark for
  "how fast does a real rewriting engine run on big diagrams."
- VyZX (TOPLAS 2025, doi 10.1145/3807780) formally verifies ZX in Rocq;
  *String Diagrams for Monoidal Categories, in Rocq* (arXiv:2602.19806)
  is a 2026 companion strand. [abstract] Relevant only if we later want
  the engine's soundness *proved* rather than tested — a real option
  given the mission's Lean connection, but off the one-month path.
- *Graph Rewriting Language as a Platform for Quantum Diagrammatic
  Calculi* (arXiv:2511.15581, Springer 2025) [abstract] — a GRL-based
  platform; unassessed, low priority.

### 7. homotopy.io — different problem
- FSCD 2024, arXiv:2402.13179; Rust/WASM, `homotopy-rs`, live at
  beta.homotopy.io. [abstract] Proof assistant for **finitely-presented
  semistrict globular n-categories** — the higher-dimensional
  (2-morphisms and up) regime. Its subsystems (collapse, contraction,
  expansion, typechecking) are about *n*-dimensional diagram normalisation,
  not 1-dimensional rewriting-with-rules. **Note for the dark-tower
  line:** this is the one tool in the census that natively lives at
  E-the-dark-tower's rung ≥2. It is not useful for WS-A/B/C, and is
  precisely the tool to reach for if the tower's "are levels ≥3 real"
  question ever becomes a computation rather than a framing.

---

## RQ2 — Categorical causal inference

**Verdict.** The theory is in much better shape than the spec assumed and
has moved since April: Lorenz–Tull now have a **2026 successor**
(*Causal and Compositional Abstraction*, arXiv:2602.16612) that
generalises their 2023 framework to abstraction between causal models.
The implementation situation is the opposite: **I found no
implementation of JKZ surgery, no implementation of Lorenz–Tull, and no
implementation of the Fritz–Klingler d-separation criterion.** This whole
line is theorem-only, which means WS-B is genuinely greenfield — good for
novelty, bad for having anything to differentially test against except
the DAG-level oracles of RQ3.

### 1. Lorenz & Tull, *Causal models in string diagrams* (arXiv:2304.07638)
- 105 pp, Apr 2023, no journal version found. [abstract]
- **The core object we should adopt: "network diagrams."** A specific
  class of string diagrams in 1-to-1 correspondence with DAGs. This is
  the precise bridge from the interchange JSON (which is DAG-shaped) into
  the engine's representation — better than inventing our own encoding.
- Also: conditioning via a **normalisation box** (so conditioning is a
  diagram operation, not a semantic detour); **counterfactuals** defined
  in-setup; **identifiability of causal effects and counterfactuals
  treated fully diagrammatically**.
- **Why it matters.** WS-B's acceptance bar is Q1 identification, Q2
  surgery+mediation, Q3 d-separation. This paper claims all three live
  diagrammatically in a cd-category. If that holds up on a real read, it
  is the WS-B rule set's specification document.
- **What to mine.** The network-diagram definition (encoding contract);
  the normalisation box (Q1/Q2 conditioning); the identifiability
  chapter (whether their diagrammatic identifiability is *decidable* as
  stated, or a characterisation only — this is the crux and cannot be
  settled from the abstract).
- **Effort:** [read] — this is the single most important read in the
  census after RQ1's #1. Budget a real read of the identifiability
  sections, not a skim.

### 2. Lorenz & Tull, *Causal and Compositional Abstraction* (arXiv:2602.16612)
- Feb 2026. [abstract] Abstractions between low- and high-level models as
  **natural transformations**; unifies constructive causal abstraction,
  τ-consistency, interchange-intervention abstraction, distributed causal
  abstraction. Distinguishes **downward** abstractions (queries high→low)
  from **upward** ones (concrete `do`-interventions low→high), and
  introduces **component-level** abstraction.
- **Why it matters to us specifically.** Every one of our three
  application DAGs is an *abstraction* of a mechanism we can also observe
  finely: the memory-system spec's 20 variables abstract a retrieval
  pipeline; the Lean-pipeline spec abstracts per-module runner legs; the
  transpiler model abstracts individual repair failures. "Is my coarse
  DAG a sound abstraction of the fine mechanism?" is currently an
  unasked question in the mission, and this paper says it has an answer
  shape. Upward abstraction (mapping a concrete `do(add lemma)` to the
  high-level intervention node) is exactly WS-C's move.
- **Effort:** [read] after #1. Candidate delta — see deltas D4.

### 3. Fritz & Klingler, *The d-separation criterion in Categorical Probability* (JMLR 24(46), 2023; arXiv:2207.05740)
- [abstract] Categorical causal models, categorical d-separation, abstract
  d-separation criterion. Key structural facts from the abstract:
  categorical d-separation is a **topological-connectedness** criterion;
  results apply to measure-theoretic probability *and beyond* — including
  **deterministic and possibilistic** networks; d-separation implies
  conditional independence for any generalized causal model, and fully
  characterises causal compatibility for **"pure bloom" causal models** in
  Markov categories with conditionals.
- **Why it matters, twice over.** (a) It is WS-B's Q3 machinery. (b) The
  "applies to deterministic networks" clause is the theoretical licence
  for RQ6's problem — our Lean pipeline and transpiler are *deterministic*
  systems, where classical d-separation-based discovery is pathological
  (RQ6 #1). A criterion that is stated to hold for deterministic
  networks is worth more to us than to the average causal-inference user.
- **What to mine.** The connectedness formulation (implementable as a
  graph/hypergraph reachability query on our representation — plausibly
  cheaper than a port of dagitty's `dconnected`); the exact scope of
  "pure bloom," since that is the fine print on when the criterion is a
  characterisation rather than only sound.
- **Effort:** [read] then [port] — likely a small, high-value port.

### 4. Jacobs, Kissinger & Zanasi, *Causal inference by string diagram surgery* (FoSSaCS 2019; MSCS 2021; arXiv:1811.08338)
- [abstract] Interventions as an **endofunctor** performing surgery;
  clean syntax(string diagrams)/semantics(stochastic matrices) split.
- **Why it matters.** It is the mission's chosen surgery formalism and
  Kissinger is the grant interlocutor. Its "intervention = endofunctor"
  framing is a design constraint worth honouring literally in WS-B: `do`
  should be a *functor on diagrams*, i.e. a rule-set-driven transformation
  of the engine's own representation, not an ad hoc graph mutation.
- **Implementation:** none found. Search returned only papers,
  talks, and the awesome-ACT lists.
- **Effort:** [read] (short paper, high leverage) then [port].

### 5. Related 2025–2026 strands, ranked lower, flagged not assessed
- *Partial Markov Categories* (arXiv:2502.03477) [abstract] — partiality
  / conditioning; likely relevant to the normalisation-box story.
- *Causal Abstractions, Categorically Unified* (arXiv:2510.05033)
  [abstract] — parallel/competing unification to Lorenz–Tull 2026;
  read only if D4 is adopted, to see which framing wins.
- *Combs, Causality and Contractions in Atomic Markov Categories*
  (arXiv:2404.02017) [abstract] — **the bridge object between this
  mission and E-the-dark-tower-2.** "Combs" here are the Caus[−] comb
  hierarchy; this paper places them inside Markov categories. If the
  dark-tower line ever wants to connect its Caus[−]/BV axis to this
  mission's Markov-category axis, this is the paper that claims to do it.
  Not on the one-month path; flagged deliberately.
- *Topos Causal Models* (arXiv:2508.08295), *Infinitesimal Causality*
  (Mahadevan, arXiv:2606.24621) [abstract, both] — noted for completeness;
  no assessment made, no implementations found.
- *Markov Categories, Causal Theories, and the Do-calculus* (Studies in
  Logic 2021) [abstract] — Fong-lineage; the do-calculus-in-Markov-cats
  statement closest to what WS-B must implement.

### The honest gap in RQ2
No categorical-causal-inference result in this census has a runnable
implementation. Everything WS-B does at the *categorical* level will be
first-of-kind and can only be validated by agreeing with **DAG-level**
oracles (RQ3) on the fragment where both are defined. That is a real
validation strategy, but note what it cannot catch: a bug in the
categorical layer that happens to project correctly to DAG-level answers.

---

## RQ3 — Do-calculus / identification engines

**Verdict.** This RQ has the strongest prior art of all six and needs the
least new thinking: **y0 is the oracle**, by a wide margin, and it is
already cloned. The right oracle *set* is y0 (breadth of identification)
+ dagitty (d-separation, adjustment sets, testable implications) +
causaleffect (independent R implementation of ID/IDC for
cross-checking) — three implementations, two languages, so an agreement
is meaningful. DoWhy contributes one thing the others don't: a
statistical **graph-falsification** test.

### 1. y0 — the identification oracle [source]
- Hoyt et al., *Causal identification with Y₀*, arXiv:2508.03167, 5 Aug
  2025; MIT licence; clone at `cc6644d` (2026-04-13), version 0.2.12-dev.
- **Verified from the clone, not the paper:** `src/y0/algorithm/`
  contains `identify/`, `tian_id.py`, `transport.py`,
  `counterfactual_transport/`, `do_calculus.py`,
  `conditional_independencies.py`, `separation/`, `verma.py`,
  `simplify_latent.py`, `estimation/`, `ioscm/`, `taheri_design.py`, and
  **`falsification.py`** (`Falsifications`, `get_graph_falsifications`,
  `get_falsifications`). The paper's own claim — "the most complete suite
  of identification algorithms of any causal inference package" (ID, IDC,
  ID*, IDC*, surrogate outcomes/TRSO, tian-ID, transport, counterfactual
  transport) — is consistent with the tree.
- **What to mine.** (a) `algorithm/identify/` — the reference behaviour
  our engine's Q1 answer must match; (b) `dsl.py` — their estimand DSL is
  the obvious target format for what our engine emits, so results are
  comparable without a translation layer of our own invention;
  (c) `falsification.py` — the testable-implications API the spec asked
  after; (d) `verma.py` — Verma constraints are testable implications
  *beyond* conditional independences, which neither dagitty's
  `localTests` nor DoWhy's LMC test covers.
- **Effort:** [oracle-only] — call it as a subprocess/Python bridge for
  differential testing. Do not port.
- **License:** MIT (BSD-style, Battelle/Hoyt) — compatible.

### 2. dagitty — the d-separation and testable-implications oracle [source]
- Clone at `7a65777` (2024-12-06). **GPL-2** — read carefully: fine as an
  external oracle process, not fine to vendor into futon3c.
- **Verified from the clone** (62 exported R functions), the ones that
  matter to us: `dseparated`, `dconnected`, `paths`, `adjustmentSets`,
  `isAdjustmentSet`, `backDoorGraph`, `impliedConditionalIndependencies`,
  `instrumentalVariables`, `vanishingTetrads`, `localTests`, `ciTest`,
  `equivalenceClass`/`equivalentDAGs`, `toMAG`, `canonicalize`,
  `markovBlanket`, `ancestorGraph`, `moralize`.
- **Why it matters.** `impliedConditionalIndependencies` + `localTests` is
  the cleanest falsification API in the census: implications from the
  graph, then tested against data, with `plotLocalTestResults`. It is the
  right oracle for WS-B's **Q3** (filter-equivalence d-separation on both
  corpus topologies) — Q3 is a pure d-separation question and dagitty is
  the reference implementation of d-separation.
- **Effort:** [oracle-only]. The core is also available as JS
  (`jslib/`), which is the escape hatch if driving R is annoying — same
  algorithms, no GPL-vendoring temptation resolved by it though (still
  GPL).

### 3. causaleffect (R) — the independent-implementation cross-check [source]
- Clone at `ff6059f` (2025-09-19). Tikka & Karvanen. Implements ID, IDC,
  surrogate outcomes (TRSO), transport. Companion `cfid` implements
  ID*/IDC*.
- **Why it matters.** It is an *independent* implementation of the same
  Shpitser–Pearl algorithms as y0. Two independent implementations
  agreeing is the difference between "we match a library" and "we match
  the algorithm." Cheap to add once y0 is wired.
- **Effort:** [oracle-only].

### 4. dosearch (R) — the one with a genuinely different capability
- santikka/dosearch; Tikka, Hyttinen, Karvanen; arXiv:1902.01073.
  [abstract] Search-based do-calculus identification from **arbitrary**
  observational and experimental distributions, with **selection bias,
  transportability, missing data, and arbitrary combinations** thereof.
- **Why it matters to WS-C specifically.** WS-C's Lean-pipeline spec is
  explicitly a **missing-data / measurement-error** model (stale-olean
  and wrong-namespace failures as measurement-error nodes). Missing-data
  identification is exactly dosearch's differentiator, and it is not
  clearly covered by y0's tree. If WS-C's first identification question
  involves the sensor-missingness nodes, dosearch — not y0 — is the
  oracle for it.
- **Effort:** [oracle-only]. Flagged as a delta (D3).

### 5. DoWhy / PyWhy — refutation, not identification
- `dowhy.gcm.falsify.falsify_graph` [read: docs]: falsifies a DAG against
  observational data by comparing LMC (local Markov condition) violations
  against a baseline of node-permuted random graphs; a second test (tPa)
  checks whether the graph is falsifiable at all. Paper: Eulig et al.,
  *Toward Falsifying Causal Graphs Using a Permutation-Based Test*,
  arXiv:2305.09565, AAAI 2025; code at `eeulig/dag-falsification`.
- **Why it matters.** Every other tool here tells you what a DAG
  *implies*. This one tells you, with a calibrated baseline, whether the
  DAG is *contradicted by data* — and specifically guards against the
  failure mode where you cheer because your graph has few violations when
  a random graph would have as few. For a hand-authored DAG of our own
  system (the memory spec, the Lean-pipeline spec), that guard is the
  difference between a causal spec and a diagram of our prejudices.
- **DoWhy's ID implementation is not competitive** with y0's; use DoWhy
  for falsification only.
- **Effort:** [oracle-only]. Flagged as a delta (D2).

### 6. Ananke, pgmpy — noted, not recommended as oracles
Both implement the basic ID algorithm; the y0 paper's framing (ID only,
limited for multi-outcome/conditional/multi-intervention queries) makes
them redundant given y0 + causaleffect. [abstract] No independent
assessment made this pass.

### Recommended oracle set (concrete)
| question | primary oracle | cross-check |
|---|---|---|
| Q1 cohort identification | y0 `identify/` | causaleffect (R) |
| Q2 surgery + mediation | y0 (`transport.py`, `do_calculus.py`) | — none; genuinely thin |
| Q3 d-separation, both topologies | dagitty `dseparated` | y0 `separation/` |
| testable implications of a spec | dagitty `impliedConditionalIndependencies` + y0 `verma.py` | — |
| "is the spec contradicted by data" | DoWhy `gcm.falsify_graph` | — |
| WS-C missing-data identification | dosearch | y0 (partial) |

Note the thin cell: **mediation under surgery has no good oracle.** That
is where a divergence would be hardest to adjudicate, and it should be
priced into Q2's acceptance bar.

---

## RQ4 — Causal models of programs / deterministic systems

**Verdict.** Prior art is real but **thinner than the other RQs and
almost entirely non-categorical**; nothing found does what the transpiler
use case wants (iteratively construct a causal model of a compiler from
repair failures) — the closest work constructs causal models of programs
from *mutation-based interventions*, which is a different and much
better-instrumented setting. Say plainly: for "iterate a causal model of
a compiler from failed fixes," there is **no strong prior art**; there is
one strong adjacent method (CPDA) worth taking wholesale, and a 2026
agent-trace strand that is closer in spirit than in rigour. This is a
place where the mission can contribute rather than consume — but it
should not claim it is standing on a literature.

### 1. Causal Program Dependence Analysis (CPDA) — the strongest adjacent prior art
- Lee, Binkley, Feldt, Gold, Yoo. arXiv:2104.09107; journal version
  *Journal of Systems and Software*, 2024 (S016764232400131X). [abstract]
- **What it does.** Builds a causal model *of a program* by
  **intervening on program execution** — mutating a program element,
  observing which other elements' values change, over a test suite (the
  "Δ-execution model"), then discovering causal structure from those
  observations. Requires only light-weight parsing, no heavy static
  analysis. Output: dependence *with strength*, unlike classical
  (binary) program dependence.
- **Why it matters to us.** This is the method, transposed: our
  interventions are `do(add Mathlib extension)`, `do(withhold lemma)`,
  `do(reorder queue)` — mutations of the pipeline rather than of the
  program text — and our "test suite" is the module queue. CPDA
  legitimises the whole WS-C design and gives it a citable ancestor: a
  causal model of a deterministic software artifact, built by
  intervention rather than by observation. It also answers RQ6's problem
  by sidestepping it (see RQ6 #1).
- **What to mine.** The Δ-execution sampling design (how many mutations,
  where, how to avoid the model being an artifact of the mutation
  operator); the structure-discovery step (which algorithm, and how they
  handle determinism).
- **Effort:** [read] — method transfer, no code to port (and the code, if
  any, is Java-tooling-shaped).

### 2. chirho — modified Halpern–Pearl actual causality, implemented
- Basis Research; `basisresearch.github.io/chirho/actual_causality.html`.
  [abstract/docs snippet] A causal-probabilistic-programming library
  (Pyro lineage) with an implementation of **actual causality under the
  modified HP definition**.
- **Why it matters.** The transpiler use case's real question is often
  actual causation, not average effect: "*which* of these six changes
  actually caused this repair to fail?" HP actual causality is the
  formalism for that, and this is the only *runnable* HP implementation
  the census found.
- **Honest limit.** HP actual causality is famously definition-unstable
  (original / updated / modified HP all differ on the standard examples)
  and is NP-hard to check in general. Treat as a probe, not a component.
- **Effort:** [oracle-only] at most; more likely [read].
- Adjacent: *Reasoning About Actual Causality in Answer Set Programming*
  (KR 2025) [abstract] — an ASP encoding of HP; the right shape if we
  ever want actual-causality checking *inside* a logic-programming
  substrate rather than a Python one.

### 3. Delta debugging / spectrum-based fault localization — mature but not causal
- Zeller lineage (delta debugging, ddmin) is the canonical
  *interventional* debugging method and predates the causal framing
  entirely: it minimises a failing input by systematic ablation. Baah et
  al., *Causal inference for statistical fault localization* (ISSTA 2010,
  10.1145/1831708.1831717) is the paper that put SBFL on a causal
  footing (confounding-adjusted suspiciousness). [abstract, both]
- **Why it matters, and its limit.** Delta debugging *is* `do()` applied
  to inputs, and our sorry-loop's ablations are structurally the same
  move. But this literature localises a fault within a *fixed* causal
  structure (the program's dependence graph); it does not *learn or
  revise* a causal model across repair attempts. The gap the mission
  wants to fill is precisely the one this literature leaves open.
- **Effort:** [read] for framing/citation; nothing to port.

### 4. 2026 agent-trace strand — closest in spirit, weakest in rigour
- *From Noisy Traces to Root Causes: Structural Trajectory Analysis and
  Causal Extraction for Agent Optimization* (STRACE, arXiv:2607.07702)
  [abstract] — mines failure patterns across agent trajectories, does
  "causal localization" to strip non-causal steps and identify a
  root-cause module.
- *Causal fault localisation in dataflow systems* (arXiv:2304.11987),
  *Causality-Driven Neural Network Repair* (arXiv:2504.17946), CauSE 2025
  workshop (Causal Methods in Software Engineering) [abstract, all].
- **Why flagged.** STRACE is the nearest published thing to "read the
  diagram of failures across a corpus of agent runs" — which is this
  mission's founding sentence. It is also, on the abstract, a
  heuristic pipeline rather than an identification-checked causal
  analysis. Useful as a **positioning contrast**: our claim can be
  "same problem, but with identification checked before estimation."
- **Effort:** [read] one of them (STRACE) for positioning; do not build on.

### 5. Program slicing as causal-cone extraction — no dedicated work found
Searched; found the CPDA framing (slicing as the binary special case of
CPDA's weighted dependence) but no paper that treats a program slice as
an intervention-defined causal cone in the Pearl sense. If this is wanted
it is an unclaimed small result, not a literature.

---

## RQ5 — Proofs as diagrams

**Verdict.** Weak prior art *for what we actually want*. There is a deep,
mature literature on proofs-as-graphical-objects (proof nets, deep
inference) and a healthy tooling literature on Lean proof-tree
visualisation (Paperproof, LeanTree) — but these are two disjoint
communities, and **nothing found extracts composable typed string
diagrams from Lean proofs.** WS-D is greenfield. The one genuinely
exciting adjacent result is Bonchi et al.'s neo-Peircean relations, which
gives a *complete diagrammatic axiomatisation of first-order logic* — the
only thing in the census that could make "a proof is a diagram" a
theorem rather than an analogy.

### 1. Bonchi, Di Giorgio, Haydon, Sobocinski, *Diagrammatic Algebra of First Order Logic* (LICS 2024; arXiv:2401.07055)
- [abstract] The calculus of **neo-Peircean relations**: a string
  diagrammatic extension of the calculus of binary relations, with the
  **same expressivity as first-order logic** and a **complete
  axiomatisation**, obtained by combining cartesian and linear
  bicategories.
- **Why it matters.** If proofs are to be diagrams *in our engine*, the
  question is which diagrammatic calculus has the expressivity and the
  equational theory. This one is FOL-complete and — crucially — it is by
  the same community (Bonchi, Sobocinski) whose hypergraph-DPO machinery
  our kernel already implements. Its axioms are a candidate rule set for
  the engine, in the same slot as WS-B's causal rules.
- **Honest limit.** FOL ≠ dependent type theory. Lean goal states are not
  FOL formulas, and the gap is not cosmetic. This is a lead about the
  *right shape*, not a drop-in.
- **Effort:** [read]. Not on the one-month path.
- Adjacent: *An Analytic Propositional Proof System on Graphs*
  (arXiv:2012.01102), *Proof Diagrams for Multiplicative Linear Logic*
  (arXiv:1702.00268), *Modelling MLL via Deep Inference*
  (arXiv:2404.01026) [abstract, all]. Deep inference's "eliminating
  bureaucracy" motive is the same motive as chyp's "handles the category
  theory bureaucracy automatically" — worth one paragraph of framing in
  WS-D, no more.

### 2. Paperproof — the Lean-side extraction path that already exists
- `Paper-Proof/paperproof`, VSCode extension, Lean 4 only. [abstract/read
  of project blog] Renders **Gentzen-style proof trees**: goals are
  nodes, each tactic refines / bifurcates / closes a goal.
- **The load-bearing detail:** it works off Lean's **`InfoTree`**, using
  `TacticInfo` nodes to capture the change in user goals.
- **Why it matters.** WS-D's Phase-1 need is "(goal-before, tactic,
  goal-after) triples" — which the mission's April MAP said needs
  LeanDojo/Pantograph (still uninstalled, still Phase 3a). **`InfoTree`
  is a much shorter path**: it is in the Lean 4 compiler, needs no
  Pantograph, no Python, and no superpod. Paperproof is the working
  demonstration that the triples are extractable this way.
- **What to mine.** Their `InfoTree` → proof-tree extraction (the
  Lean-side code, not the TS renderer); their treatment of `have` chains
  and multi-goal bifurcation, which is exactly where a *tree* becomes a
  *DAG* and hence a diagram.
- **Effort:** [read] then [port] — a small Lean-side extractor.
  Flagged as a delta (D5).
- **Note the representational gap.** Paperproof produces a **tree**.
  Our diagrams are DAGs with typed ports and open boundaries. A tactic
  proof's real structure — shared hypotheses, `have`s used twice — is a
  DAG; the tree view loses exactly the sharing that makes the diagram
  interesting. Don't inherit the tree.

### 3. LeanTree (arXiv:2507.14722) — factorized goal states
- *LeanTree: Accelerating White-Box Proof Search with Factorized States
  in Lean 4*, Jul 2025. [abstract only — title and one-line description]
- **Why flagged.** "Factorized states" is, on its face, the same move as
  typed ports: decompose a goal state into independently-manipulable
  components so that search doesn't re-derive shared structure. If that
  reading survives contact with the paper, it is the closest existing
  work to WS-D's `addresses-valid?` invariant, and it is Lean-4-native.
- **Effort:** [read] — cheap, potentially high-value. Second-most
  valuable read in RQ5 after #1.

### 4. What does not exist
No work found that extracts **composable typed diagrams** from Lean
proofs, no "category of goal states" implementation, no proof-mining of
tactic corpora into a monoidal-categorical representation. The mission's
Layer-1.5 claim ("a proved Lean proof is a morphism in a category whose
objects are goal states") remains, as of this census, **unimplemented by
anyone**. That is a genuine opening; it is also a warning that nobody has
found it worth doing, and WS-D should be able to say what it buys that a
proof tree doesn't.

---

## RQ6 — Causal discovery under full observability

**Verdict.** The discovery literature's central machinery is
**actively harmful** in our setting, and this is well documented rather
than a suspicion: deterministic relations generate conditional
independences beyond the Markov condition, so the true DAG is unfaithful
to its own distribution and PC-family algorithms provably delete real
edges. The good news is that the correct response is already published
and is exactly what our systems permit: **intervene instead of testing
for independence.** For a fully-observable, deterministic, freely
intervenable system, constraint-based discovery is the wrong tool and
intervention-based discovery is the right one.

### 1. The determinism pathology — establish it, then design around it
- Well-attested (search-level consensus, standard result): with `Y=f(X)`
  in `X→Y→Z`, `Y ⊥ Z | X` holds because `X` determines `Y`; PC removes
  edges on independences created by determinism, not by separation, and
  is **unsound** in the presence of deterministic relations. [abstract]
- **Why this matters for the mission's rhetoric as well as its code.**
  Our Lean pipeline is deterministic given inputs (that is its point).
  Any statement of the form "we ran discovery on the pipeline traces and
  learned the graph" would be unsound by construction. WS-C's design —
  *author* the DAG, then check identification — is the correct response,
  and it should be stated as a principled choice, not a convenience.
- **Effort:** [read] one canonical treatment for a citable statement.

### 2. Mazaheri, Zhang & Uhler, *Relaxing Faithfulness with Intervention-Only Causal Discovery* (arXiv:2607.11816, UAI 2026)
- [abstract, fetched] Standard faithfulness is too restrictive; a milder
  **intervention-immediacy faithfulness**, which *allows cancellations*,
  suffices for identification using **hard interventions**. Central
  claim: prioritise interventions over conditional-independence testing.
  **No code found.**
- **Why it matters.** This is the theorem that licenses our whole
  approach: we can do hard interventions on our own pipelines at will
  (`do(withhold lemma)`, `do(reorder queue)`, `do(withhold M)` in the
  memory system's E2), so we can buy identification without paying
  faithfulness — the assumption our determinism would otherwise break.
  It converts "we can't do discovery here" into "we should do
  intervention-only discovery here."
- **Effort:** [read] — high value for WS-C's design justification.
  Flagged as a delta (D6).

### 3. Structure learning from execution traces — no direct hit
Searched for structure learning over program/execution traces; the
returned literature is either **invariant mining** (CLN2INV
arXiv:1909.11542, G-CLN — learning *loop invariants* from traces, not
causal structure) or the 2026 agent-trace work (STRACE, RQ4 #4), or
business-process causal monitoring (MDPI Entropy 26(10):867). None is
"learn a causal DAG from execution traces of a deterministic system."
Combined with RQ4's finding: CPDA is the closest thing that exists, and
it works by intervention, not by observing traces — consistent with #2.

### 4. Model elicitation loops with a human/agent in the loop — thin, and mostly LLM-flavoured
Found *CausalAgent* (arXiv:2602.11527) [abstract], a conversational
multi-agent system for end-to-end causal inference, and MCP-mediated
"causal structure learning agent as algorithm scheduler" descriptions.
These are tool-orchestration papers, not elicitation-methodology papers.
**The iterative human-in-the-loop causal model construction the
transpiler use case describes has no methodological literature that this
census found.** State that plainly rather than dressing the CausalAgent
line up as prior art.

---

## Things that would change the plan — summary

Collected here; stated as proposed deltas in `deep-research-deltas.md`.

1. **The rewriting regime is wrong for WS-B** (RQ1 #1). Convex DPO
   modulo plain SMC ≠ rewriting modulo commutative comonoid (copy/discard).
   Published fix exists (LMCS 2025). — **highest-priority finding.**
2. **The oracle set should gain DoWhy's `falsify_graph`** (RQ3 #5) — the
   only tool that tests whether our authored DAG is *contradicted*.
3. **WS-C's missing-data identification wants dosearch, not y0** (RQ3 #4).
4. **Causal abstraction is an unasked question** with a 2026 answer shape
   (RQ2 #2) — every one of our three DAGs is an abstraction of something
   finer.
5. **WS-D's trace extraction can use Lean's `InfoTree` today** (RQ5 #2),
   dropping the LeanDojo/Pantograph dependency from the critical path.
6. **Intervention-only discovery is the licensed method** for our
   deterministic systems (RQ6 #2), and determinism makes CI-test-based
   discovery unsound (RQ6 #1) — WS-C should say so.

## Things that explicitly do NOT change the plan

- **No better kernel to port.** AlgebraicRewriting.jl is more general but
  costs a runtime and lacks convexity; DisCoPy is a computation library,
  not a rewriting engine; homotopy.io solves a different problem;
  Quantomatic is dead. The chyp port stands.
- **No existing system does WS-B.** JKZ surgery, Lorenz–Tull, and
  Fritz–Klingler are all theorem-only. Nobody has built this.
- **No theorem found that blocks a workstream.** The nearest thing is
  RQ6 #1 (determinism breaks faithfulness), which blocks a method we were
  not planning to use and reinforces the method we were.
- **Drop Quantomatic from WS-A's porting-material list** — cosmetic, but
  it is currently listed as live source material and it is not.

---

## Sources

Ordered by RQ. Reading level in brackets.

**RQ1** — [arXiv:2204.04274 / LMCS 21(1) 2025, Milosavljevic–Piedeleu–Zanasi](https://arxiv.org/abs/2204.04274) [abstract] · [chyp](https://github.com/akissinger/chyp) [source/read] · [SDRT I, JACM 2022 / arXiv:2012.01847](https://arxiv.org/abs/2012.01847) [abstract] · [SDRT II, MSCS 2022 / arXiv:2104.14686](https://arxiv.org/pdf/2104.14686) [abstract] · [DisCoPy](https://github.com/discopy/discopy) [read] · [DisCoPy, arXiv:2311.10608](https://arxiv.org/abs/2311.10608) [abstract] · [discopy.markov API](https://docs.discopy.org/en/main/_api/discopy.markov.Functor.html) [abstract] · [AlgebraicRewriting.jl](https://github.com/AlgebraicJulia/AlgebraicRewriting.jl) [read] · [Computational category-theoretic rewriting, JLAMP](https://www.sciencedirect.com/science/article/abs/pii/S2352220823000421) [abstract] · [Quantomatic](https://github.com/Quantomatic/quantomatic) [read] · [homotopy.io, arXiv:2402.13179 / FSCD 2024](https://arxiv.org/abs/2402.13179) [abstract] · [PyZX](https://github.com/zxcalc/pyzx) [source] · [VyZX, TOPLAS 2025](https://doi.org/10.1145/3807780) [abstract] · [String Diagrams for Monoidal Categories in Rocq, arXiv:2602.19806](https://arxiv.org/pdf/2602.19806) [abstract] · [GRL platform, arXiv:2511.15581](https://arxiv.org/html/2511.15581) [abstract]

**RQ2** — [Lorenz–Tull, Causal models in string diagrams, arXiv:2304.07638](https://arxiv.org/abs/2304.07638) [abstract] · [Lorenz–Tull, Causal and Compositional Abstraction, arXiv:2602.16612](https://arxiv.org/abs/2602.16612) [abstract] · [Fritz–Klingler, JMLR 24(46) 2023 / arXiv:2207.05740](https://www.jmlr.org/papers/volume24/22-0916/22-0916.pdf) [abstract] · [Jacobs–Kissinger–Zanasi, arXiv:1811.08338](https://arxiv.org/abs/1811.08338) [abstract] · [Partial Markov Categories, arXiv:2502.03477](https://arxiv.org/pdf/2502.03477) [abstract] · [Causal Abstractions Categorically Unified, arXiv:2510.05033](https://arxiv.org/pdf/2510.05033) [abstract] · [Combs, Causality and Contractions in Atomic Markov Categories, arXiv:2404.02017](https://arxiv.org/pdf/2404.02017) [abstract] · [Topos Causal Models, arXiv:2508.08295](https://arxiv.org/pdf/2508.08295) [abstract] · [Infinitesimal Causality, arXiv:2606.24621](https://arxiv.org/pdf/2606.24621) [abstract] · [Markov Categories, Causal Theories and the Do-calculus](https://studiesinlogic.sysu.edu.cn/sites/default/files/2022-10/1674%C2%AD3202(2021)%C2%AD06%C2%AD0001%C2%AD24.pdf) [abstract]

**RQ3** — [y0, arXiv:2508.03167](https://arxiv.org/abs/2508.03167) [abstract] + [y0 repo](https://github.com/y0-causal-inference/y0) [source] · [dagitty](https://github.com/jtextor/dagitty) [source] · [causaleffect](https://github.com/santikka/causaleffect) [source] · [dosearch](https://github.com/santikka/dosearch) [abstract] + [arXiv:1902.01073](https://arxiv.org/abs/1902.01073) [abstract] · [DoWhy graph refutation docs](https://www.pywhy.org/dowhy/main/user_guide/modeling_causal_relations/refuting_causal_graph/refute_causal_structure.html) [read] · [Eulig et al., arXiv:2305.09565, AAAI 2025](https://arxiv.org/abs/2305.09565) [abstract] + [dag-falsification code](https://github.com/eeulig/dag-falsification) [abstract] · [DoWhy-GCM, arXiv:2206.06821](https://arxiv.org/pdf/2206.06821) [abstract]

**RQ4** — [CPDA, arXiv:2104.09107](https://arxiv.org/abs/2104.09107) + [JSS 2024](https://www.sciencedirect.com/science/article/pii/S016764232400131X) [abstract] · [chirho actual causality docs](https://basisresearch.github.io/chirho/actual_causality.html) [abstract] · [Halpern, modified HP definition](https://www.cs.cornell.edu/home/halpern/papers/modified-HPdef.pdf) [abstract] · [Actual Causality in ASP, KR 2025](https://proceedings.kr.org/2025/59/kr2025-0059-ozcan-et-al.pdf) [abstract] · [Baah et al., ISSTA 2010](https://dl.acm.org/doi/10.1145/1831708.1831717) [abstract] · [STRACE, arXiv:2607.07702](https://arxiv.org/abs/2607.07702) [abstract] · [Causal fault localisation in dataflow systems, arXiv:2304.11987](https://arxiv.org/pdf/2304.11987) [abstract] · [CauSE 2025 workshop](https://causality-software-engineering.github.io/cause-workshop-2025/) [abstract]

**RQ5** — [Bonchi et al., Diagrammatic Algebra of First Order Logic, arXiv:2401.07055 / LICS 2024](https://arxiv.org/abs/2401.07055) [abstract] · [Paperproof](https://github.com/Paper-Proof/paperproof) + [proof-tree writeup](https://antonkov.github.io/posts/How-to-build-a-proof-tree/) [abstract] · [LeanTree, arXiv:2507.14722](https://arxiv.org/pdf/2507.14722) [abstract] · [An Analytic Propositional Proof System on Graphs, arXiv:2012.01102](https://arxiv.org/pdf/2012.01102) [abstract] · [Proof Diagrams for MLL, arXiv:1702.00268](https://arxiv.org/pdf/1702.00268) [abstract] · [Modelling MLL via Deep Inference, arXiv:2404.01026](https://arxiv.org/pdf/2404.01026) [abstract]

**RQ6** — [Mazaheri–Zhang–Uhler, arXiv:2607.11816, UAI 2026](https://arxiv.org/abs/2607.11816) [abstract, fetched] · [Geometry of the faithfulness assumption](https://www.researchgate.net/publication/228102657_Geometry_of_the_faithfulness_assumption_in_causal_inference) [abstract] · [CLN2INV, arXiv:1909.11542](https://arxiv.org/pdf/1909.11542) [abstract] · [Causal Learning: Monitoring Business Processes, Entropy 26(10):867](https://www.mdpi.com/1099-4300/26/10/867) [abstract] · [CausalAgent, arXiv:2602.11527](https://arxiv.org/pdf/2602.11527) [abstract]
