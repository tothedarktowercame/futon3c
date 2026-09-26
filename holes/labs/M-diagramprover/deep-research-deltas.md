# Deep-research deltas — proposed changes to M-diagramprover §Programme of Work

**Date:** 2026-08-02. **Source:** `deep-research-census.md` (same
directory, same run). **Target:**
`futon3c/holes/missions/M-diagramprover.md` §Programme of Work
(2026-08-02), §Workstreams and §Acceptance for the month.

Per the mission's revision contract these are **proposed deltas, not
applied edits**. Six substantive, one cosmetic. D1 is the only one that
changes work already committed; D5 is the only one that *removes* a
dependency. Nothing here changes the one-month acceptance bar's four
numbered items, though D1 and D2 change what clearing item 2 means.

---

## D1 — WS-B needs rewriting modulo commutative comonoid structure, which WS-A does not implement

**Status: adopt or explicitly reject before WS-B writes a rule.** This is
the one finding that touches landed code.

**Finding.** (census RQ1 #1) Milosavljevic, Piedeleu & Zanasi,
*Rewriting String Diagrams with Commutative Monoid Structure*, LMCS
21(1), 2025 (arXiv:2204.04274), gives a sound and complete DPO-of-
hypergraphs interpretation of string-diagram rewriting **modulo
commutative (co)monoid equations**, with a soundness condition that is
*not* convexity. The prior state of the art offered only the two ends:
modulo full Frobenius (SDRT-I) or modulo plain SMC with convex DPO
(SDRT-II).

**Why it bites.** WS-A ported chyp, which implements SDRT-II: our
`matcher.clj` enforces **convexity**, the plain-SMC condition. WS-B's
substrate is **Markov categories** = SMC + a commutative comonoid
(copy/discard) on every object. That is strictly between the two regimes
the kernel knows about. If WS-B introduces copy and discard as ordinary
generators plus explicit associativity/commutativity/counit rules, the
engine will *run*, but it will not rewrite **modulo** the comonoid
structure: every copy-shuffle becomes an explicit rewrite step, the
rule-application relation blows up combinatorially with equivalent-up-to-
copy-structure results, and "normal form" no longer means what the
categorical semantics says it means. That is an abstraction bug, and it
would surface as WS-B disagreeing with the oracles in ways that look like
identification bugs.

**Proposed delta.** Amend **WS-B** to open with a bounded
theory-then-decide step, before any causal rule is written:

> **WS-B.0 (new, ~2 days).** Read arXiv:2204.04274 to decision grade.
> Determine which of three routes WS-B takes: (a) extend
> `matcher.clj`/`rewrite.clj` with the comonoid-modulo soundness
> condition alongside the existing convexity check (rule sets declare
> which regime they need); (b) keep convex-SMC rewriting and carry
> copy/discard as explicit generators + rules, *documenting* the
> resulting quotient gap and the cases where it is harmless; (c) defer,
> with a written statement of what WS-B's receipts do and do not mean
> under route (b). Record the choice and its reasoning in the mission.

Also amend **WS-A's** description, which currently says "matching + DPO
rewriting modulo associativity/commutativity of the monoidal structure,"
to name the regime precisely as **SDRT-II convex DPO (plain SMC)** — the
current phrasing reads as though the comonoid case is already covered.

**Cost.** One decision-grade read (~half a day) + between zero and ~2
days of kernel work depending on route. **Do not** let this become a
kernel rewrite; route (b) with an honest written gap is an acceptable
one-month answer.

---

## D2 — Add graph falsification to the acceptance bar for WS-B and WS-C

**Finding.** (census RQ3 #5) `dowhy.gcm.falsify.falsify_graph` (Eulig et
al., arXiv:2305.09565, AAAI 2025) tests whether an authored DAG is
*contradicted by data*, comparing local-Markov-condition violations
against a baseline of node-permuted random graphs, plus a second test for
whether the graph is falsifiable at all. Every other oracle in the census
tells you what a DAG **implies**; this is the only one that tells you
whether the DAG is **wrong**.

**Why it bites.** Both application specs — `memory-causal-graph-spec.json`
(20 variables) and WS-C's forthcoming `lean-proof-pipeline-causal-spec.json`
— are **hand-authored by us, about our own systems**. That is the exact
condition under which a causal spec risks being a diagram of our
prejudices rather than of the mechanism. Cheering because our graph has
few CI violations, when a randomly permuted graph would have as few, is
the specific failure the permutation baseline exists to catch. This is
the same discipline as the mission's own `closure-verified?` invariant
(no self-reported closures) applied one level up: no self-reported
causal specs.

**Proposed delta.** Extend **§Acceptance for the month** item 2 and the
WS-C deliverable:

> Each authored causal spec (memory-system DAG, Lean-pipeline DAG) is run
> through a falsification pass — dagitty `impliedConditionalIndependencies`
> + `localTests` for the implications, y0 `verma.py` for constraints
> beyond CI, DoWhy `gcm.falsify_graph` for the permutation-baselined
> verdict — and the result is reported **with the spec**, including the
> case where the spec survives only because the data are too thin to
> falsify anything.

**Cost.** Small; all three are oracle-only calls on data we already have
or are collecting. **Caveat to state when reporting:** for the
Lean-pipeline spec the "data" are deterministic pipeline runs, where the
LMC-violation statistic behaves differently than in the i.i.d.
statistical setting these tests assume. Report the number, and report
that caveat with it.

---

## D3 — WS-C's missing-data identification wants dosearch, not y0

**Finding.** (census RQ3 #4) `dosearch` (Tikka, Hyttinen, Karvanen;
arXiv:1902.01073) does search-based do-calculus identification from
arbitrary observational and experimental distributions with **selection
bias, transportability, missing data, and arbitrary combinations** — the
missing-data case being its differentiator. y0's tree covers ID/IDC/ID*/
IDC*/tian-ID/transport/counterfactual-transport, but missing-data
identification is not clearly among them.

**Why it bites.** WS-C is *defined* as a missing-data model: its
distinguishing design move is that stale-olean and wrong-namespace
failures are **measurement-error / missingness nodes** (the
alarm-shaped-verification-failures finding, formalised). The mission
names y0 and dagitty as the oracle set. If WS-C's first identification
question involves those sensor nodes — and by construction it will — then
the mission's named oracles cannot adjudicate it and dosearch can.

**Proposed delta.** In **WS-C**, name dosearch as the oracle for any
identification question that routes through the missingness/measurement
layer; add it to `diagramprover-refs/` alongside the existing five
clones. In WS-B, leave the oracle set as-is (y0 primary, dagitty for
d-separation, causaleffect as the independent cross-check on ID/IDC).

**Cost.** One clone, one R bridge (dagitty already forces an R path, so
this is incremental). **Also worth recording in the mission:** the census
found **no good oracle for mediation-under-surgery** — WS-B's Q2. Q2's
acceptance should price that in rather than implying all three receipts
are equally checkable.

---

## D4 — Causal abstraction is an unasked question with a 2026 answer shape

**Finding.** (census RQ2 #2) Lorenz & Tull, *Causal and Compositional
Abstraction*, arXiv:2602.16612 (Feb 2026) — the direct successor to the
2023 *Causal models in string diagrams* the mission already builds on.
Abstractions between low- and high-level models as **natural
transformations**; unifies constructive causal abstraction, τ-consistency,
interchange-intervention abstraction, distributed causal abstraction;
distinguishes **downward** abstraction (queries high→low) from **upward**
(concrete `do`-interventions low→high); introduces **component-level**
abstraction.

**Why it bites.** Every one of the mission's three application DAGs is an
abstraction of a mechanism we can also observe more finely — the memory
spec's 20 variables over a retrieval pipeline, the Lean-pipeline spec
over per-module runner legs, the transpiler model over individual repair
failures. The mission currently has no notion of whether a coarse spec is
a *sound* abstraction of the fine mechanism, and no vocabulary for the
move WS-C makes constantly: taking a concrete `do(add this specific
Mathlib lemma)` and reading it as the high-level `do(add extension)`
node. That is literally this paper's **upward abstraction**.

**Proposed delta.** Add to **§Explicitly parked / superseded** a new
*named held* item rather than a workstream — this is post-prototype:

> **Causal abstraction (held, not parked).** Lorenz–Tull 2026
> (arXiv:2602.16612) supplies the formalism for "is our coarse DAG a
> sound abstraction of the fine mechanism?" and for the
> concrete-`do` → abstract-`do` lift WS-C performs informally. Held as a
> post-prototype question; the obligation is *named*, not discharged.
> Entry condition: WS-B's receipts land and WS-C's spec has been used for
> at least one real intervention.

Deliberately a held obligation, not scope: adding it to the one-month
programme would be scope creep on a month that already has four
workstreams.

---

## D5 — WS-D can extract tactic traces from Lean's `InfoTree` today; drop LeanDojo from its critical path

**Finding.** (census RQ5 #2) Paperproof builds Lean 4 proof trees from
the compiler's own **`InfoTree`**, reading `TacticInfo` nodes to capture
goal changes at each tactic. No Pantograph, no Python, no superpod.

**Why it bites.** WS-D needs `(goal-before, tactic, goal-after)` triples.
The April MAP made that dependent on LeanDojo-v2 + Pantograph (Phase 3a,
still uninstalled, listed in §Explicitly parked as "non-blocking; WS-D
uses manual/existing trace extraction until then"). Manual extraction was
costed at ~30 min/file in the April MAP. `InfoTree` is a third path that
neither the April analysis nor the August programme considers, and it is
the shortest one: in-compiler, Lean-native, already demonstrated to work
by a shipping VSCode extension.

**Proposed delta.** Amend **WS-D**:

> Trace extraction is via Lean 4's `InfoTree`/`TacticInfo` (the Paperproof
> approach), not manual transcription and not LeanDojo. LeanDojo/
> Pantograph stays parked. **Represent the extracted structure as a DAG,
> not a tree** — Paperproof renders a Gentzen tree, which discards
> exactly the sharing (hypotheses and `have`s used more than once) that
> makes a proof a *diagram* rather than a derivation.

Also worth a cheap read alongside it: **LeanTree** (arXiv:2507.14722),
"factorized states in Lean 4" — on its abstract, the same move as typed
ports, and Lean-4-native. If that reading survives the paper, it is the
closest existing work to WS-D's `addresses-valid?` invariant.

**Cost.** A small Lean-side extractor (Paperproof's Lean code is the
model). Saves the ~30 min/file manual cost and removes a parked
dependency from a workstream's stated fallback.

---

## D6 — State intervention-only discovery as WS-C's licensed method, and say why CI-based discovery is unsound here

**Finding.** (census RQ6 #1, #2) Deterministic relations generate
conditional independences beyond the Markov condition, so a deterministic
system's true DAG is **unfaithful to its own distribution** and
PC-family algorithms provably delete real edges. Mazaheri, Zhang & Uhler
(arXiv:2607.11816, UAI 2026) show a milder **intervention-immediacy
faithfulness**, which permits cancellations, suffices for identification
using **hard interventions**, and argue interventions should be
prioritised over CI testing. Separately, CPDA (arXiv:2104.09107, JSS
2024) is the strongest existing precedent for building a causal model of
a *program* by intervening on execution rather than observing traces.

**Why it bites.** The Lean pipeline is deterministic given inputs — that
is its point. Any claim of the form "we ran discovery on pipeline traces
and learned the graph" would be **unsound by construction**, and it is
the kind of claim that is easy to drift into once trace data are
abundant. Conversely, we can perform hard interventions on our own
pipelines at will — `do(add extension)`, `do(withhold lemma)`,
`do(reorder queue)`, and the memory system's `do(withhold M)` (E2). So
the mission is in the *favourable* case of the 2026 result, not the
unfavourable one.

**Proposed delta.** Add to **WS-C** a short methodological statement:

> WS-C authors the DAG and checks identification; it does **not** run
> constraint-based structure discovery on pipeline traces. Rationale:
> the pipeline is deterministic, determinism creates conditional
> independences beyond the Markov condition, and PC-family algorithms are
> unsound under it. Model revision proceeds by **hard intervention**
> (`do(add extension)`, `do(withhold lemma)`, `do(reorder queue)`), for
> which intervention-immediacy faithfulness suffices (Mazaheri–Zhang–
> Uhler, UAI 2026); the interventional-model-building precedent is CPDA
> (Lee et al., JSS 2024).

This is documentation, not code — but it converts "we authored the DAG
because discovery was inconvenient" into "we authored the DAG because
discovery is provably the wrong tool here," which is the difference
between a shortcut and a method. It also gives the transpiler use case
its citable ancestor.

---

## D7 (cosmetic) — drop Quantomatic from WS-A's porting-material list

WS-A lists "Porting material: chyp (closest, Python), quantomatic, PyZX."
Quantomatic is **unmaintained by its own project page**, which names chyp
and ZX Live as the projects carrying its goals forward. Its value is
historical only. PyZX's rules are fixed (ZX-specific), so it is a
performance benchmark rather than porting material. Suggested phrasing:
"Porting material: chyp (Apache-2.0, ported). Quantomatic is
unmaintained — historical reference only. PyZX is a performance
reference, not a rule-set source."

---

## Not proposed, deliberately

- **No change of kernel.** AlgebraicRewriting.jl (MIT, Julia, generic
  C-Set DPO) is more general but adds a runtime, has an explicitly
  unstable API, and does not supply the SMC soundness condition we'd have
  to add anyway. DisCoPy is a computation library, not a rewriting
  engine. homotopy.io solves the globular *n*-category problem, not ours.
  **The chyp port stands.**
- **No change to WS-B's novelty claim.** The census found **no
  implementation** of JKZ surgery, Lorenz–Tull, or Fritz–Klingler
  d-separation. WS-B is greenfield, as assumed. The corollary is a
  validation limit worth stating in the mission when the receipts land:
  agreement with DAG-level oracles cannot catch a categorical-layer bug
  that happens to project correctly.
- **No new workstream for proofs-as-diagrams theory.** Bonchi et al.'s
  neo-Peircean relations (LICS 2024) give a *complete* diagrammatic
  axiomatisation of first-order logic and are by the same community whose
  hypergraph-DPO machinery our kernel implements — genuinely the right
  shape for WS-D's rule set. But FOL ≠ dependent type theory, and closing
  that gap is not a one-month task. Recorded in the census as a lead.
- **No adoption of the 2026 agent-trace strand** (STRACE,
  arXiv:2607.07702). Closest published work to this mission's founding
  sentence, and on its abstract a heuristic pipeline without
  identification checking. Useful as positioning contrast — "same
  problem, identification checked before estimation" — not as a
  dependency.
