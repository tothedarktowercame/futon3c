# Warrant-Disciplined Agent Memory: Architecture, Instrumentation, and a Pre-Repair Baseline

**Version 1 — FROZEN 2026-07-31.** Written 2026-07-30 under
M-memory-retrieval WS6. Intended audience: technical readers outside the
futon stack (first: Rob). Sending gate: Joe.

> **Scope note added on freezing.** The observation-channel figures in
> §5.5 (14 offered halves, 14% outcome completion, 71% empty) are correct
> for the corpus this draft could see: the `author=ground-control` slice
> of pattern-outcome receipts as of 2026-07-28. They are **not**
> representative of the system. An overnight run completed 2026-07-31
> raised the corpus to 129 offered halves at **89% outcome completion**
> and 20 metric-bearing rows. Read §5.5 as a pre-repair baseline of a
> slice, and see `holes/excursions/E-memory-whitepaper-v2-plan.md` §1 for
> the measured before/after. Two further findings post-date this
> document and bear on it: recall loss is now diagnosed as structural
> (per-subject queries at 9–16 s against a 30 s total budget) and
> non-randomly biased toward analysis-heavy problems; and bitemporal
> `system-as-of` is silently ignored by the store, which blocks the
> frozen benchmark of §4.2 for existing dispatches. V1 is retained
> unedited apart from this note.

*The chronological evidence ledger from which this draft was written —
including every correction, retraction, and increment in the order it
occurred — is preserved verbatim at `docs/retrieval-evidence-ledger.md`.
This document is organised by argument; that one is organised by
discovery date. Where the two disagree, the ledger records how we got
here and this document records what we currently believe.*

*Provenance note: the mechanism described here has been developed in
public repositories with dated commits (descriptions and working code,
2026-07-22 onward). This document consolidates that disclosure
deliberately, serving as defensive publication. It is not legal advice;
a US filing within the inventor grace period remains a separate
decision, and Appendix A is written to keep that option cheap.*

---

## Abstract

An autonomous agent that stores what it learns does not thereby remember
it: a memory that surfaces only when the agent happens to issue the right
query is storage, not memory. We describe a deployed memory system in
which retrieval runs from a task's own text through independently
reviewed edges to reusable patterns and their attached memories; in which
every dispatch and every witnessed outcome writes a receipt; and in which
those receipts — never self-report — update the retrieval coefficients,
under an exploration-mass floor that guards against the loop collapsing
onto its own early successes.

We state at the outset what this draft does and does not establish. The
architecture is deployed and its learning loop has closed end to end
under independent witness: a memory mined from failed sessions, recalled
by pattern into a later session on the same problem, cited by the solver,
and confirmed by a compiler. What is **not** yet established is that the
combination outperforms its parts. The retrieval ablation that would
settle it requires a frozen multi-query benchmark that does not yet
exist, and the receipt-update comparison sits below its own preregistered
calibration minimum at one metric-bearing observation.

What this draft contributes instead is an architecture, a set of
falsifiable instruments, and an audited **pre-repair baseline** of the
observation channel itself: across 14 frozen dispatch records, 71% of
recalls surfaced nothing, outcome halves completed at 14%, and the field
intended to record why a memory surfaced carries a vocabulary of exactly
one string. A companion audit finds the field naming independent
witnessing to be uncorrelated with the presence of witness records, and
we exclude it from evaluation accordingly. Those numbers are reported as
findings about our own instrument, not about retrieval, and they are what
a forthcoming 489-problem run is being instrumented to fix. Two framing facts hold
throughout: the system uses commodity agent models — the capability delta
is the memory loop, not model scale — and it is operationally autonomous,
with the human contribution confined to the strategic layer and the split
auditable from the dispatch and receipt ledgers.

---

## 1. Problem and contributions

### 1.1 Storage is not memory

Agent memory systems fail in a characteristic way that has nothing to do
with storage. Our own first deployment demonstrated it precisely: an
agent deliberately recorded four typed memories from a proof session —
correct, well-formed, full-text indexed — and they were operationally
invisible, because nothing connected them to the moments where a later
session would need them. A saved memory surfaced only if the agent
independently thought to search for it, with the right words, at the
right decision point. That is a diary, not a memory.

Three further failure modes constrain any fix.

**Self-report contamination.** If the system learns from the agent's own
claims of success, it learns to claim success. Every signal that updates
retrieval must be independently witnessed — here, by a proof compiler.

**Endogenous confirmation.** A retrieval mechanism that learns which
relations retrieve useful material will concentrate on relations that
retrieve corroboration of what it already found: apparent certainty,
hidden targets. Any learning retrieval loop needs an explicit guard, and
the guard must be demonstrated against the failure it guards, not
asserted.

**Proposal/warrant confusion.** Similarity — lexical or semantic — is
cheap, and wrong often enough that letting it *justify* retrieval quietly
replaces evidence with plausibility. Similarity may propose; only review
may warrant.

The design question: what is the smallest mechanism that makes stored
memories arrive unprompted at the decision points where they matter, gets
better at this from real use, and resists the three failures above?

### 1.2 What this draft claims

Three primary claims are frozen for this draft. Each resolves to a row in
the claim/evidence matrix (§4.5), and each is scoped to the evidence that
currently exists rather than to the evidence we expect.

**C1 — Warrant-disciplined conduction is implementable without
sacrificing recall.** Retrieval can be restricted to independently
reviewed edges while similarity is retained as a proposal source, and the
separation can be verified by mutation testing rather than asserted.
*Status: supported, mechanism-level.*

**C2 — Receipts make retrieval auditable, and the audit binds.** An
offered/outcome receipt channel makes retrieval behaviour inspectable
after the fact, including its own failures; and a preregistered
calibration gate over that channel refuses promotion when observations
are insufficient. *Status: supported, including by the gate refusing
itself.*

**C3 — Structural retrieval features must pay functional rent, and our
own instruments initially failed this test.** Global topology statistics
can be improved without improving retrieval; we demonstrate this on our
own preregistered criterion, retract it, and replace it with a functional
damage measure. *Status: supported as a negative methodological result.*

Two claims are explicitly **not** made in this draft, and are stated here
so the reader does not have to infer their absence:

- That hybrid retrieval (content plus pattern) beats either route alone.
  The ablation exists as an offline instrument and has been run on two
  frozen cases; it has not been run on a benchmark (§4.2, §5.1).
- That receipt-updated operator coefficients improve ranking. The replay
  harness is validated and its live verdict is
  `:below-calibration-minimum` at n = 1 (§5.3).

### 1.3 Contributions

1. A deployed architecture combining typed bitemporal memory, pattern-
   mediated recall, warrant-disciplined conduction, a receipts observation
   channel, receipt-fed coefficient updates, and an exploration-mass floor
   (§2, Appendix A).
2. A set of falsifiability instruments that have each fired at least once
   against their authors: a confirmation-collapse battery, a calibration
   gate that refused promotion, a preregistered spectral prediction that
   failed and was diagnosed, and a functional-damage sweep that reframed
   what "damage" means (§5).
3. An audited pre-repair baseline of the observation channel (§5.5) and
   of the warrant fields (§5.6), reported as engineering findings about
   our own instruments, together with the instrumentation specification
   they imply (§8.2).

---

## 2. Deployed system and trust boundaries

### 2.1 Substrate

A memory is a **dialogue act** — assert, retract, challenge, agree —
recorded as an append-only evidence body plus one typed hyperedge linking
it to its subjects, the session and mission that produced it, the
transcript spans it distills, and the *patterns* it instantiates. The act
vocabulary is the IATC performative set.

The store is **bitemporal**: what is believed now and what was believed
as-of T are both queryable. Correction is therefore an event with a
timestamp rather than an overwrite, which is what makes the correction
record in Appendix D a queryable object rather than a changelog.

### 2.2 Retrieval pipeline

Recall runs from the task's own text, not from an agent's decision to
search:

```
problem text → term extraction → query ladder → text index
     → content matches ──────────────────────────┐
     → matched memories → their patterns → top pattern → attached memories
                                                 │
                                    merge, dedupe, budget → candidates
```

Three properties of this path are load-bearing and each is discussed
where it is measured:

- The **query ladder** (3-term conjunction, then pairs, then singles,
  first non-empty tier wins) exists because the text backend is
  conjunctive and composed queries died: measured on the live store, 1
  term returned 5 hits, 3 returned 3, 7 returned 2, 12 returned 1, and 29
  returned 0, while recall was composing up to 36 terms (§5.5).
- **Content matches merge ahead of pattern expansion.** Before
  2026-07-30 they did not, and pattern arbitration could discard an exact
  content match — a failure traced end to end in §5.1.
- **Only reviewed edges conduct.** Text and embedding similarity may
  nominate; neither can warrant.

### 2.3 The four trust boundaries

The paper's central discipline is that four distinct things are kept
distinct. Conflating any two of them is the mechanism by which a memory
system starts grading its own homework.

| Boundary | What it establishes | What it does **not** establish |
|---|---|---|
| **Review** of an attachment | that an edge is well-formed and its ids resolve | that the memory is true or useful |
| **Warrant** for a memory | that evidence supports the memory's content | that any agent used it |
| **Attribution** by the agent | that the solver *says* it used a memory | that the memory was load-bearing |
| **Witness** of the outcome | that a third party (a compiler) confirms the result | *which* memory produced the result |

The fourth row is the one most easily elided. An independently witnessed
proof does not independently witness the solver's claim about which
memory helped it. Throughout §5 we report attribution and witness
separately and never let one stand in for the other.

**A defect at this boundary, disclosed.** The deployed
`:witness-status` field is computed as a pure function of which scribe
lane drafted the memory:

```clojure
(if (#{:solve-lane :arc-lane} lane) :independently-witnessed :self-asserted)
```

It therefore **cannot fail**: no property of the memory, the proof, or
the receipt can make a solve-lane memory come out self-asserted.

An audit over the frozen mathematics projection
(`warrant-audit-20260730.edn`, 62 attachments carrying the field) shows
the consequence is not merely theoretical — the field is **uncorrelated
with the evidence it names**:

| | carries a witness record | carries none |
|---|---|---|
| `:independently-witnessed` (53) | **1** | **52** |
| `:self-asserted` (9) | **1** | 8 |

Exactly two memories in the whole fixture carry a machine-checkable
witness record — a list of Lean declarations with their proved results —
and they fall **one on each side of the status split**. 98% of the
memories the field calls independently witnessed carry no witness record
at all, while the one memory whose body contains four verified
declarations is labelled `:self-asserted`. A field that lands on both
sides of the only two cases where evidence exists is not a weak signal;
it is no signal.

*(An earlier count over a different population — 59 solve- and arc-lane
drafts, of which 28 carried `:independently-witnessed` with no witness
record — is retained in the evidence ledger. The audit above supersedes
it as the verifiable figure, being computed from a frozen, republishable
artifact.)*

Two things this is not. It is not a claim that the attestations are
false — solve-lane memories distil proofs the compiler checked, and
ground control re-elaborated `#print axioms` independently, so the
underlying witnessing is real. Nor is it a review failure: edge review
checks well-formedness and id resolution, and does so correctly. The
defect is that the *status field* records a lane while the *evidence* for
it lives outside the field's derivation. **Consequently
`:witness-status` is excluded as an independent evaluation signal
everywhere in this paper**, and its repair is a draft-2 gate (§8.2).

### 2.4 Mechanism status table

Every mechanism referenced in this paper carries one of three statuses.
Claims in §5 and §8 are made only at the level the status supports.

| Mechanism | Status | Evidence |
|---|---|---|
| Typed bitemporal hypergraph store | **deployed** | §2.1 |
| Pattern-mediated recall | **deployed** | §5.1 |
| Query tier ladder | **deployed** (2026-07-30) | §5.5 |
| Content matches merged ahead of patterns | **deployed** (`50916c84`, reviewed) | §5.1 |
| Reviewed-edge warrant gate | **deployed**, mutation-tested | §5.4 |
| Receipts (offered/outcome halves) | **deployed** | §5.5 |
| `:memory-use/surfacing-via` attribution | **deployed** (2026-07-30, no data yet) | §5.5 |
| Scalar per-memory Ψ (S6) | **deployed** | §5.3 |
| Pattern-level Ψ-v2 | **implemented dark**, below calibration | §5.3 |
| k-step coupled propagation + floor | **implemented dark**, verified | §5.4 |
| Beta-binomial per-pattern update (Rung 2) | **implemented dark** | Appendix A.2 |
| Budgeted facet refinement (Rung 3) | **implemented dark** | Appendix A.3 |
| Offline damage sweep (D_state) | **implemented**, run on 2 cases | §5.2 |
| Spectral admissibility criterion | **retracted as a gate** | §5.2 |
| Derived mid-tier technique clusters | **proposed** | §5.2 |
| Semantic proposal lane | **chartered** | §8.2 |
| Relation-type θ_r | **proposed**, degenerate at 1 edge type | §5.3 |

---

## 3. Formal model

Only the notation needed to express implemented mechanisms and tested
hypotheses is introduced. Where a mathematical object has no deployed
counterpart, that is stated rather than glossed.

Retrieval state is `s_t = (x_t, θ_t, F_t, B_t)`: activation over
admissible memory nodes; coefficients over typed traversal operators;
facet resolution; traversal budget. For relation-specific graph operators
`Δ_r`, the pattern-conditioned operator is

    Δ_θ = Σ_r θ_r Δ_r

and the coupled dynamics are

    x_{t+1} = Φ(x_t, Δ_{θ_t}, q)        (state update)
    θ_{t+1} = Ψ(θ_t, x_{t+1}, q)        (operator update)

— a diffusion whose Laplacian is itself updated by observation: "updating
*which* Laplacian" (Rob, 2026-07-25), a second-order operator with
location-dependent coefficients selected within a family. The degenerate
regimes are named: **collapse** (all activation on one node —
confirmation) and **dissipation** (flat activation — ranking nothing).
The exploration-mass floor is the coefficient-space constraint that holds
`Δ_θ` in the sustaining band between them.

### 3.1 Mathematical object → implementation status

This table is the honest core of the formal section. The deployed system
is a considerably smaller object than the frame above.

| Object | Deployed counterpart | Status |
|---|---|---|
| `x_t` propagation | one-step: text match → pattern endpoint → attached memories | **deployed, k = 1** |
| `Δ_θ` conductances | pattern attachments; editing a description edits a conductance | **deployed** |
| `Σ_r θ_r Δ_r` (multi-relation) | recall traverses **one** relation (`:memory/assert`); the sum has one term | **degenerate** |
| `θ_r` per relation type | inactive: census sees a single edge type on the recall path | **proposed** |
| `Ψ` operator update | scalar per-memory `(1 + α·used/offered)`, α = 0.5 | **deployed (minimal shadow)** |
| Ψ at pattern granularity | Ψ-v2 replay harness | **dark, below calibration** |
| Query source `q` | problem-file term extraction | **deployed** |
| Boundary conditions `B_t` | reviewed-edge warrant gate | **deployed** |
| Forcing `F_t` | curriculum lane: coverage map + proposed targets | **deployed** |
| Exploration floor ε | lower-bounded simplex, ε = 0.01 live / 0.2 in battery | **deployed in dark path** |
| k-step coupled iteration | Rung 4 harness | **dark, verified** |

The honest summary: **the deployed system instantiates the frame at
k = 1 with a one-term operator sum and a scalar Ψ.** The frame is a
research programme that the deployment shadows, and this draft claims
results only for the shadow.

### 3.2 A structural correspondence, and where it stops

The 2026-07-30 runs exposed a failure whose shape was already named in
our MetaCA work, and the correspondence is worth recording precisely
because it also shows where the analogy stops.

Retrieval collapsed under **conjunctive composition** of query terms
(§5.5). The remedy was not a better composite query but a *schedule* over
the constituents — the tier ladder. Temporal braiding's central
observation has the same shape: *alternating* the elementary updates of
two operators, rather than composing them into a single map, can sustain
a field that either operator collapses on its own. In both settings the
composed map is the dead one.

Where it does **not** hold: the ladder carries no state between tiers (no
temporality); tiers do not reweight one another (no feedback); it
short-circuits at the first non-empty tier rather than exposing a mixing
fraction (a switch, not a dial). There is also a taxonomic caveat — the
tiers are *subsets of one term set*, i.e. successive relaxations of a
single operator, not two distinct operators alternating. Strictly this is
decomposition-under-collapse, not braiding.

Taken as a research direction rather than a result, it points at a
genuinely braided seam: alternate *structurally different* query
operators (lexical, graph-endpoint, embedding); carry feedback so what
tier *n* surfaces reweights tier *n+1* (Ψ acting *within* a recall rather
than between runs); and expose the mixing fraction as a precision/breadth
dial. The first is half-present — the seam runs lexical and graph-endpoint
stages — but they are composed in sequence, not braided, and their
interaction has never been measured.

n is small and the effect is a plumbing repair, not evidence for a
theory. The honest reading: a collapse-under-composition phenomenon
appeared in a second, unrelated substrate, which is weak evidence that it
is a property of conjunctive operator composition rather than of cellular
automata.

---

## 4. Evaluation methodology

### 4.1 What exists and what does not

This draft evaluates against **frozen offline artifacts**, not against a
benchmark. Stating the difference plainly is a precondition for reading
§5 correctly.

| Instrument | Artifact | Scope |
|---|---|---|
| Observation-channel audit | `observation-channel-audit-20260730.edn` | 16 receipt halves |
| Warrant/provenance audit | `warrant-audit-20260730.edn` | 62 attachments |
| Ψ-v2 dark replay | `psi-v2-replay-results-20260728.edn` | 2 joined rows, 1 metric-bearing |
| Damage sweep (D_state) | `damage-state-results-20260730.edn` | 2 frozen query cases |
| Connectivity meter | WS2 readings, stamps `20260727` / `20260730` | whole reviewed graph |
| Collapse battery | WS1 battery file | 1 synthetic corpus |

**The frozen relevance benchmark of Experiment 0 does not exist.** Until
it does, no claim in this paper is a benchmark result, and §5 is labelled
accordingly throughout.

### 4.2 The benchmark this paper needs (specification)

Recorded here so draft 2 inherits a specification rather than a
convention, and so the forthcoming run can be instrumented against it.

**Construction.** A chronological, transport-clean set of theorem-proving
dispatches. For each query, preserve the corpus, retrieval graph,
receipts, and retrieval implementation **as they existed at dispatch
time**. Evaluating an old query against knowledge added only later is
permitted only as an explicitly named counterfactual.

**Labels.** Each query/memory pair labelled *separately* for: relevance
to the problem; whether it surfaced; whether the solver cited it; whether
it was load-bearing in the resulting proof; whether the task outcome was
independently witnessed. The four trust boundaries of §2.3 are why these
are four labels and not one.

**Exclusions.** Timeouts, store outages, malformed responses, and
unavailable transports are **missing or operational-failure
observations, not retrieval misses.** §5.5 shows this is not a
hypothetical concern.

**Metrics** (locked before arms are run): recall at a fixed candidate
budget; reciprocal rank or nDCG; precision at budget; empty-result rate;
retrieval latency.

**Size.** The existing `n ≥ 20` gate is an absolute minimum. Because the
ablation is a *paired* design, the unit of evidence is the **discordant
pair** — a query on which the arms disagree — not the query. A sign test
needs at least 5 discordant pairs to reach p < 0.05 even when unanimous,
and realistically 10–12 to survive a non-unanimous split. With
discordance rate *d*, required n ≈ 12/*d*; a 10–12 query pilot should
measure *d* rather than assuming it.

**Independence.** The corpus is 10 pattern stars of sizes 30, 21, 14, 10,
4, 3, 3, 2, 2, 1, and concrete handles repeat heavily (one on 7 memories,
another on 8, another on 6). Queries drawn from one theorem family are
not independent: **effective n tracks the number of distinct families,
not the number of queries.** The specification is therefore *n queries
spanning ≥ 15 distinct theorem families, ≤ 3 per family*, analysed
clustered by family.

### 4.3 Research questions

- **RQ1.** Does hybrid retrieval (content + pattern) improve relevant
  recall at a fixed budget over either route alone?
- **RQ2.** Which structural relations pay functional rent?
- **RQ3.** Do receipt-updated coefficients improve ranking on later
  observations?
- **RQ4.** Does the exploration floor prevent confirmation collapse?
- **RQ5.** How reliable is the observation channel itself?
- **RQ6.** Do the warrant and provenance fields record what they claim?

### 4.4 Preregistration and post-hoc discipline

Where a prediction was registered before a result, this is stated with
the registering artifact. Two predictions in this paper failed and are
reported as failures rather than repaired after the fact (§5.2, §5.3).
One diagnosis of a failure was itself preregistered and re-tested
(§5.2). The primary analysis does not change metrics or exclusions after
seeing arm results.

### 4.5 Claim/evidence matrix

| Claim | Current evidence | Required evidence | Status |
|---|---|---|---|
| Warrant discipline is implementable and verified | Mutation-tested review gate; content path reuses the same projection | Held-out confirmation at benchmark scale | **Supported (C1)** |
| Receipts make retrieval auditable, and the gate binds | Offered/outcome records; Ψ-v2 refused promotion at n=1; channel audit §5.5 | Receipt-completeness repair | **Supported (C2)** |
| Structural features must pay functional rent | Spectral criterion shown gameable *and* inverted; D_state sweep | D_functional over the benchmark | **Supported as negative result (C3)** |
| The learning loop can close end to end | One witnessed chain (mine → recall → cite → compile) | Replication across problems | **Single witnessed instance** |
| Hybrid recall beats a single route | 2 frozen cases showing complementarity at state level | Frozen multi-query ablation | **Open (RQ1)** |
| Receipt-updated operators improve ranking | Scalar deployed; dark replay at n=1 | Chronological comparison, adequate observations | **Below calibration (RQ3)** |
| Exploration floor prevents collapse | Synthetic battery, both directions | Multi-seed and sensitivity replication | **Mechanism evidence (RQ4)** |
| The observation channel is reliable enough to evaluate on | Audited: 71% empty, 14% outcome completion, single-string attribution | Post-repair re-audit | **Refuted for the pre-repair corpus (RQ5)** |
| `:witness-status` records witnessing | Audited: uncorrelated with witness records, 1 vs 1 across 53/9 | Derivation from evidence presence | **Refuted; field excluded (RQ6)** |
| Global topology predicts retrieval utility | Conflicting, projection-sensitive, criterion inverted | Functional predictive validation | **Retracted as criterion** |

---

## 5. Results by research question

### 5.1 RQ1 — Hybrid retrieval: two cases, no benchmark

**Result: not established. Complementarity demonstrated at state level on
two frozen cases; generalisation untested.**

The deployed pipeline had a defect that motivates the question sharply.
`propose-patterns-by-query` searches evidence text, then converts matching
rows into *pattern* proposals; `recall-by-endpoints` returns memories
attached to the winning pattern. The pipeline was therefore

    query → matching MEMORIES → their PATTERNS → top pattern → that pattern's memories

so a memory that actually matched was returned **only if its pattern also
won**.

Durée run 25 dispatched on problem a92J05, a Rouché row. Eighty-seven
seconds earlier, a memory named
`close-a92J05-by-transferring-the-unit-disk-zero-count` — drafted from the
immediately preceding attempt on *that same problem*, recording its exact
residual goal — had been promoted, attached, and verified
`:attachment-status :reviewed`. It did not surface. Five memories did,
**all from a single pattern**; four were measure-theory memories the
runner dismissed as irrelevant.

Replaying the dispatch's own ladder shows retrieval was not the problem.
The query was `roots outside unit disk filter card`; its first tier,
`"roots outside unit"`, returns 7 memories **including the a92J05
memory**, as do `"roots outside"`, `"outside unit"`, `"unit disk"` and
`"roots"`. The index found the right memory on essentially every tier.
The matched memory sat on `math/missing-dependency-protocol`, the
proposal resolved to `tactic-algebra-interference`, and the match was
discarded in favour of five siblings of a different winner. **The system
found the right answer and threw it away.**

The remedy — return content matches directly alongside pattern-mediated
recall — was implemented and independently reviewed on 2026-07-30
(`50916c84`). Reviewed text matches now survive as `:content-matches` and
merge ahead of endpoint recall; every surfaced memory carries `:via
:content-match | :pattern | :endpoint`. Critically the content path does
**not** widen the warrant set: it reuses the same reviewed-edge
projection, and the negative test was verified non-vacuous by mutation —
disabling the review gate turns it red, with an `:attachment-status
:unreviewed` memory visibly leaking as a content match. This is the
mutation evidence supporting **C1**.

**The offline evidence for complementarity.** The D_state sweep
(`damage_state_sweep.bb`, fixture `20260730`, write-once and
byte-reproducible on rerun) ablates each arm on two frozen cases:

| case | baseline | without content arm | without pattern arm |
|---|---|---|---|
| run 25 | 2 direct + 3 pattern | loses both direct matches, incl. the historically missed memory (Jaccard damage 0.571) | loses 3 pattern neighbours (0.600) |
| Lemniscate | architecture memory direct + 3 pattern neighbours | unchanged — the memory also returns via its pattern | loses all 3 neighbours (0.750) |

The two arms are complementary in shape: direct matching protects exact
nominations from arbitration; pattern expansion supplies related memories
lexical search did not nominate. **Two cases are two cases.** They
motivate RQ1; they do not answer it. Note also that the Lemniscate row is
a case where the content arm was *redundant* — evidence that the
complementarity is conditional, not universal.

### 5.2 RQ2 — Structural rent: a retracted criterion and a sparse damage profile

**Result: the preregistered structural criterion is retracted. D_state
measured on two cases; D_functional unmeasured.**

This subsection reports a methodological failure of our own instrument,
in three passes, because the sequence is the result.

**Pass 1 — a preregistered prediction, run, and failed.** The prediction:
admitting the `:subjects` relation would raise λ₂ above the meter's 0.1
floor with no new memories written. Measured against the frozen
`20260730` export (method validated by reproducing the meter's own
reading, 0.0754 vs 0.0755):

| graph | nodes | components | unnormalised λ₂ | normalised λ₂ |
|---|---|---|---|---|
| pattern + distills (baseline) | 147 | 19 | 0.0754 | 0.0386 |
| + subjects as nodes | 315 | 4 | 0.0836 | **0.0291** |
| + shared-subject projection | 183 | 4 | **0.2002** | **0.0235** |

Admitting subjects as nodes does not clear the floor, and on the
scale-robust normalised Laplacian it makes conduction *worse* (−24.5%).
Concrete handles average 2.7 uses each and 116 of 161 are used once, so
they attach pendant nodes rather than shortcuts. The prediction is
**withdrawn rather than softened.**

**Pass 2 — the repair exposed a defect in the instrument.** Projecting
the relation instead — joining two memories that share a handle — *does*
clear the stated criterion, 0.0754 → 0.2002 against a 0.1 floor. But it
clears it while normalised conductance *falls* by 39%. The reason is
mechanical: the criterion is computed on the **unnormalised** Laplacian,
where adding edges raises algebraic connectivity almost by construction,
and a projection turns every k-memory handle into a k-clique. **A change
that improves nothing about retrieval would have turned the meter
green.**

**Pass 3 — the metric is not merely gameable, it is inverted.** Review by
a second agent (E-memory-topology, Codex-3) made the decisive objection:
a handle touching *k* memories is **one incidence relation, not k(k−1)/2
memory relations**, so the clique expansion was never the right operator.
Recomputing with the degree-normalised hypergraph Laplacian (Zhou; nodes
= memories, hyperedges = handles, no clique expansion):

| operator | largest component | hyperedges in it | λ₂ |
|---|---|---|---|
| patterns only | 33 memories | **1** | **1.0000** |
| patterns + concrete subjects | **126** memories | 51 | 0.0689 |

The patterns-only figure is λ₂ = 1.0 because its largest component *is a
single hyperedge* — one pattern containing 33 memories. A single blob
attains maximal λ₂ by construction. Admitting topical handles raises
reach from 33 to 126 memories, cuts components from 15 to 3, and **lowers
λ₂ precisely because the structure stops being trivial.**

In this regime λ₂ is *anti-correlated* with useful structure. The
criterion does not merely admit edge inflation: **it awards full marks to
the most degenerate topology available.** The 2026-07-27 reading of
λ₂ = 0.99999 was recorded as the one criterion that passed; it passed
because the graph was a 6-node star. **Embodiment A.5 (spectral
admissibility as a retrieval gate) is retracted.** Global spectral
quantities may be reported descriptively; they gate nothing. This is the
evidence supporting **C3**.

What survives as a positive result is **reach**, which is
retrieval-relevant in a way spectra are not: the topical relation makes
four times as many reviewed memories reachable from a seed. Whether that
improves retrieval is a held-out question no spectral quantity settles.
The governing rule adopted from E-memory-topology: *a structural change
is good only when it improves held-out retrieval or witnessed outcomes at
the same read budget, while preserving domain, temporal, review,
provenance and witness invariants.*

**The replacement measure.** For an intervention *A*, functional damage
is

    D_functional(A) = score(G) − score(G \ A)

against a held-out retrieval score. The first sweep measures only the
state-level precursor D_state: divergence in the ordered candidate set
under exact frozen perturbation. Its profile is **sparse but
occasionally large** — only 5 of 55 single-edge removals change the
top-five state, and only 1 of 55 pattern-role removals does. The
exception is instructive: removing the attachment of
`finish-an-outside-root-count-from-an-inside-count-and-no-boundary-roots`
to `tactic-algebra-interference` changes three of five candidates
(Jaccard damage 0.750); removing the whole edge changes four of five
(0.889).

This yields a more precise causal object than "a memory node": the
relevant unit is a **nomination attachment**, whose removal changes which
whole neighbourhood consumes a bounded candidate budget. It is a
discontinuity in proposal selection, not diffusive propagation. In the
Lemniscate case one content-matched attachment gates an entire cluster —
removing its whole edge takes the candidate set from four ids to empty.
The neighbourhood is internally useful by the historical receipt, but
operational access to it is **brittle**.

Whether any of this changes proof outcomes is D_functional and is
unmeasured. No structural score should be fitted to two cases.

### 5.3 RQ3 — Receipt updates: the calibration gate binds

**Result: `:below-calibration-minimum` at n = 1. The mechanism is
validated; the claim is refused.**

Ψ-v2 moves the coefficient from the node (memory) to the edge structure
recall traverses. Credit assignment is the one non-obvious decision:
receipts do not record which pattern surfaced each memory, so attribution
is *reconstructed* deterministically — restrict a memory's reviewed
attachments to those whose pattern terms intersect the recorded query,
fall back to all attachments if empty, split one unit of credit uniformly,
and report the mode (`:matched` / `:fallback-all` / `:unattributable`) per
row. Fractional credit is fine; invented precision is not, so there is no
similarity weighting.

The update is

    θ_p = 1 + α·(used_p / offered_p)   if offered_p ≥ n-min-coeff
    θ_p = 1                            otherwise (per-coefficient abstention)

with α = 0.5, bounded, cold-start neutral, and passed through the
lower-bounded simplex with explicit floor ε.

**Two gates, deliberately different.** Harness activation is
`n-min-coeff = 5.0` fractional offered credits. Live promotion is the
Phase 6 standard: **n ≥ 20 independently witnessed outcomes per
coefficient**, plus interface coordination at a cohort boundary.

**The live result.** Three arms (no-Ψ / deployed scalar / pattern-level)
ran leave-one-out over the frozen receipt corpus. Two rows joined; one
has non-empty `used-ids`. All three arms score MRR 1.0 and hit@1 1.0 —
which says only that all arms retained an already-correct top item in one
row. The other joined row is `:surfaced-not-usable` and is excluded from
MRR as preregistered. No scored row was self-fitted. Twenty pattern
coefficients are reported; **none reaches the activation minimum of 5.0**,
the largest observed counts being 2.0 offered / 2.0 used.

The census found 51 `:pattern-attachment` edges and no second edge type,
so relation-level θ_r is correctly marked `:inactive-degenerate` — the
sum Σ_r θ_r Δ_r has one term and the exploration floor constrains a
singleton.

**The ranking contract is nevertheless proven correct**, on a synthetic
fixture whose answer is computable by hand: six offered and six used
credits to a planted good-route coefficient yield θ = 1.5 and
deterministically flip the ranking from `[b1, g1]` to `[g1, b1]`,
asserted in the harness rather than eyeballed.

A system that publishes its own "not enough evidence yet" verdict
alongside a proven-correct mechanism is the evidentiary posture this
document maintains throughout. **This is the evidence supporting C2**:
the gate is real because it refused its author.

### 5.4 RQ4 — The exploration floor: mechanism evidence, single configuration

**Result: demonstrated in both directions on one synthetic corpus.
Multi-seed replication outstanding.**

On a synthetic corpus with a planted target and a decoy relation seeded
with early accidental corroboration — expectations preregistered in the
battery file before the run — the floor-off ablation collapses as the
theory predicts: θ concentrates entirely on the decoy
(`{repairs 1.0, requires 0.0}`), hiding the planted target. The identical
system with ε = 0.2 recovers the planted target at step 2.

Critically, *both* control arms — fixed endpoint order and one-step typed
ranking — rank the decoy first on this corpus. The recovery is produced
by the iterated coupled dynamics under the floor, not inherited from any
single-step method. Independently witnessed challenge memories remained
reachable at every step of every run; all runs deterministic and fully
audited, with k = 1 identity against the one-step ranker holding exactly.

This is a guard that first demonstrates the failure it guards against,
which is the correct standard. It is also **one corpus, one seed, one
decoy strength, one floor value.** The claim is narrow by construction:
*under the constructed dynamics, the exploration floor prevents or
reverses collapse.* Sensitivity across seeds, decoy strengths, floor
values and traversal budgets is a draft-2 requirement (§8.2).

A related methodological yield from the same workstream: a preregistered
check that "diffusion time ranks inversely with λ₂" scored Spearman
ρ = 0.0 and was retained as a failure. The diagnosis — three of four
configurations sat past the explicit-Euler stability boundary ε < 2/λ_max,
so those runs measured concentration (power iteration), not diffusion —
was itself preregistered and re-tested at ε = 0.1, giving ρ = −0.8,
confirmed. **Step size is part of the operator**: any deployment of
iterated retrieval dynamics must report ε against the spectral radius or
declare the concentration regime deliberately.

### 5.5 RQ5 — The observation channel: an audited pre-repair baseline

**Result: the channel is the binding constraint on every other
experiment. Reported here as an engineering finding about our own
instrument.**

This is the one research question this draft answers at full strength,
and the answer is unflattering. Audit artifact:
`observation-channel-audit-20260730.edn`, produced by
`observation_channel_audit.py` over the frozen `20260728` export;
read-only and deterministic. It reproduces the WS3 join numbers
independently.

| Stage | Count | Rate |
|---|---|---|
| Offered halves (dispatches) | 14 | — |
| … of which recall returned `:ok` | 4 | **29%** |
| … of which `:recall-empty` | 10 | **71%** |
| Outcome halves | 2 | **14% of offered** |
| Joined offered↔outcome rows | 2 | 14% |
| Metric-bearing rows (non-empty `used-ids`) | 1 | **7%** |
| Total memories surfaced across all 14 dispatches | 8 | 0.6/dispatch |

Three findings.

**(i) Missing observations are not negative observations.** Of the 10
empty recalls, 2 carry `:recall-reason :timeout` in this export; a wider
read of the lane records 21 `:timeout` and 13 `:store-unavailable`. The
dispatch path collapsed `:timeout`, `:store-unavailable` and a genuine
empty result into a single runner-visible signal — "no dispatch-time
memories were supplied" — so the runner's report cannot distinguish them.
A substantial share of this lane's recorded "recall surfaced nothing"
observations are **infrastructure failures rather than retrieval
evidence**.

This is not hypothetical, and it caught the present author. An earlier
draft of §5.1 explained a run that surfaced zero memories — despite six
reviewed memories carrying its exact subject handle — as a fragmentation
effect. The persisted offered-half record shows `:recall-reason
:timeout`. Recall never completed; nothing was arbitrated, fragmented or
discarded. **An infrastructure failure was diagnosed as an architectural
one.** The arbitration finding of §5.1 is unaffected — that dispatch
recorded `:recall-status :ok` and returned five memories — but the two
runs are adjacent on the same problem and produced the same runner-facing
sentence for entirely different reasons.

**(ii) The attribution field is populated and uninformative.** All four
rows that surfaced anything carry `:inclusion-reasons`. Across every
entry in the corpus the **vocabulary has size one**: `"reviewed
attachment surfaced by terrain-conditioned dispatch recall"`. The field
is present, well-formed, and cannot distinguish which pattern surfaced
which memory — which is precisely why Ψ-v2 must reconstruct attribution
lexically (§5.3). *(This corrects the Ψ-v2 design note, which records the
field as empty in live rows; it is not empty, it is constant. The
practical consequence is the same, the repair is not.)* The replacement —
`:memory-use/surfacing-via`, recording `:content-match | :pattern |
:endpoint` per memory — is **deployed as of 2026-07-30** and carries no
data in this export because the export predates it.

**(iii) The corpus was starved, and the causes are known and partly
fixed.** Recall was composing conjunctive queries of up to 36 terms
against a backend whose hit count falls off a cliff with term count (1
term → 5 hits, 3 → 3, 7 → 2, 12 → 1, 29 → 0). Measured across five live
rows using real term-extraction output, the composed 3-term query
returned **zero** memories for all five, while the decomposed ladder
surfaced a memory for **three**. Two rows correctly returned nothing at
any tier. Two further defects from the same record: query terms are
partly scraped from the dispatch packet's prose, so operator-written
preamble dilutes the query; and the ladder's tier-selection predicate
tests only `(:candidates p)`, ignoring `:content-matches`, so a tier
finding a content match but no pattern candidate is skipped (latent —
every tier with content matches on the traced row also had candidates).

A separate indexing finding: `"a92J05"` and `"rouche"` each return zero
memories at the deployed limit, although a memory is *named*
`close-a92J05-…` and eight carry the subject `rouche-root-count-transfer`.
The surface is an application-controlled SQLite FTS5 sidecar
(`futon1bi.text-index`, `tokenize='unicode61'`), where hyphens are
ordinary separators. The causes are **bounded-result starvation** — the
targets rank at 64 and 30 under `limit=100` and fall outside smaller
deployed limits because coordination records outrank memories — and
**body-only indexing**: only `:evidence/body` is indexed, the id column is
`UNINDEXED`, and top-level `:subjects` are not indexed at all. The
concrete handles carrying this corpus's topical structure are **absent
from the index**. This is a retrieval-layer fact and must not be confused
with the graph-layer claim retracted in §5.2: indexing subjects would
make them *findable*, a different proposition from the refuted claim that
relating them improves conductance. It has not been done.

**Reading.** The `20260728` export is a **pre-repair baseline**. The
ladder, the content-match merge, and `surfacing-via` all landed on
2026-07-30, after it. Draft 2's first duty is to re-run this audit
post-repair and report the delta.

### 5.6 RQ6 — Warrant fields: the status does not track the evidence

**Result: refuted. `:witness-status` is excluded as an evaluation
signal.**

The full cross-tabulation and its consequences are given at §2.3, where
the defect belongs architecturally. The evaluation-facing summary: across
62 attachments carrying the field, the two memories with actual
machine-checkable witness records fall one on each side of the
53/9 status split, and 52 of 53 memories labelled
`:independently-witnessed` carry no witness record. Artifact:
`warrant-audit-20260730.edn`, regenerable with `warrant_audit.py`.

Two methodological notes, since this audit corrected itself twice in the
course of being run and the corrections are the reason to trust it. A
first pass reported 53 of 53 — a clean 100% — because the record-slicing
walked balanced braces and never reached the memory body where witness
records actually live. A second pass with a two-level walk reported the
same figure for the same reason. Only a hand check of the one memory
known to carry `:witnesses` revealed that the attachment and the body sit
1693 characters apart *at equal brace depth* in this serialization, so no
brace walk of any depth would pair them. The audit now partitions on
marker position instead, and its output reconciles exactly with the raw
count of `:witnesses` occurrences in the file — two, which is the
cross-check that a clean 100% would have failed.

The relevant lesson for the benchmark of §4.2 is that **a suspiciously
clean number is a reason to check the instrument**, and that the check
must be against a quantity derivable independently of the instrument —
here, a raw string count.

### 5.7 The loop has closed once, under witness

Reported separately because it is a single instance and should not be
read as a rate.

The learning loop closed end to end on 2026-07-25/26: a memory mined from
failed sessions — a lemma absent from the library — recalled by pattern
into a revisit of the same problem, cited by the runner as redirecting
its effort, producing a proven lemma and a sorry-count drop, with both
receipt halves written and the outcome witnessed by the compiler rather
than self-reported.

A second episode demonstrates **cross-model transfer**: two sessions by a
codex runner on a construction target, dispatched with recall over a
corpus mined from *zai* sessions. In both, all three surfaced memories
were cited as used with specific behavioural effects; in the second, the
target file reached zero remaining proof obligations, verified
independently at the axiom level (no `sorryAx` transitively; standard
axioms only). The decisive memory — a process rule recording a
previously-hit API absence — carried a two-branch decision structure
("prove the local inequality, or declare the dependency frontier"), and
the two sessions exercised *both* branches: the first declared and priced
the frontier, the second proved the local result by the elementary route,
making the frontier unnecessary. Portability is demonstrated at the level
of decision structure, not merely recalled facts.

Both episodes are attribution-plus-witness: the compiler witnesses the
*outcome*; the runner attributes the *use*. Per §2.3 these are different
claims, and no instrument in this draft independently establishes that
the cited memory caused the outcome. That is what D_functional over the
benchmark is for.

---

## 6. Failure analysis and threats to validity

**Sample size dominates everything.** One metric-bearing row, two frozen
damage cases, one witnessed loop closure, one battery configuration. No
result in §5 except RQ5 rests on a sample that would survive an ordinary
power calculation. Where an effect is reported, it is reported as a
mechanism demonstration or a motivating case.

**Attribution is not causation.** The solver's citation of a memory is
self-report about *use*, made trustworthy at the outcome layer only
because a compiler witnesses the *result*. A runner could cite a memory
that did no work in a proof that succeeded for other reasons. Nothing in
this draft excludes that.

**`:witness-status` is structurally uninformative** (§2.3, §5.6) and is
excluded as an evaluation signal. Any future analysis that reads it
before its repair inherits a field that cannot fail — and which, on
audit, does not track the evidence it names. Note that this threat
propagates backwards: any *earlier* analysis in the ledger that used the
field is suspect, and the ledger is not retroactively corrected.

**Theorem-family dependence.** The corpus concentrates: 39% of memories
sit behind one hub, and that hub — `missing-dependency-protocol` — is a
*process* category, not a mathematical area. Results drawn from this
corpus may reflect the shape of one curriculum rather than a property of
the method.

**Infrastructure missingness is non-random.** Timeouts and store
unavailability are more likely under load, which correlates with
dispatch batch size, which correlates with which problems were being run.
Treating these as missing-at-random would be unjustified.

**Corpus quality bounds what recall can deliver.** 22 of 90 memories
(24%) carry a `:hook` identical to the memory name; the `:level` field has
40 distinct values over 91 drafts, 28 of them singletons. A type
dimension whose vocabulary grows linearly with its instances is not
typing anything. The hook figure was reached independently twice, over
disjoint samples, which is why it is stated as a corpus property rather
than a sampling artifact.

**Dark mechanisms are not deployed mechanisms.** Rungs 2–4 and Ψ-v2 are
verified against their own contracts, not against production traffic. A
contract-correct mechanism can still be the wrong mechanism.

**Author ≠ reviewer is upheld but not blinded.** The decisive objection
of §5.2 came from a second agent, and the §5.5 correction from the
implementing agent. Both are real independence; neither is blinded
review.

---

## 7. Related work

The combination claimed here is novel; the ingredients are not, and
positioning them accurately is part of the claim.

**Spreading activation and cognitive architectures.** Retrieval as
propagation over an associative network descends from Collins and
Loftus's spreading-activation theory of semantic processing. The closest
functional relative is ACT-R's declarative memory, where retrieval
probability depends on a base-level activation that rises with use and
decays with time, plus associative strengths from context. Our `x_t`
propagation and use-driven `Ψ` occupy the same conceptual position. The
differences are architectural rather than mathematical: ACT-R's
activation is not gated by an independent review of the edges it
traverses, and its strengthening signal comes from the model's own
retrieval history rather than from an external witness.

**Relevance feedback and counterfactual learning to rank.** Rocchio-style
relevance feedback established that retrieval should be updated by
evidence of use. The modern line — unbiased and counterfactual
learning-to-rank from implicit feedback — confronts exactly our
self-report problem in a different guise: click logs are biased by
position and by what the ranker chose to show. That literature's remedy
is propensity weighting over a model of the presentation bias. Ours is
different in kind: rather than de-biasing a self-generated signal, we
require the update signal to be **independently witnessed** by a
compiler, and treat the agent's own attribution as a separate, weaker
label (§2.3). The two approaches are complementary, and the propensity
literature is the right source for what our receipts channel should
record about *what was shown*, which is the reason `surfacing-via`
matters.

**Learned sparse and dense retrieval.** DPR, ColBERT, and SPLADE learn
the representation and let similarity do the work. Our position is the
opposite on one axis: similarity may propose but never warrant (§2.3).
This is a deliberate capability sacrifice — we forgo learned recall over
unreviewed material — bought in exchange for an auditable warrant chain.
Whether the trade is worth it is an empirical question this draft does
not settle; the semantic proposal lane (chartered) is designed to test it
by adding embedding proposals strictly under the review gate.

**Graph-structured retrieval.** Random-walk and personalised-PageRank
retrieval over entity graphs, and more recently graph-structured RAG with
community summarisation, share our premise that structure carries
retrieval signal. Our §5.2 result is a caution addressed to that
literature: we adopted a global spectral admissibility criterion,
preregistered it, and found it not merely gameable by edge inflation but
**anti-correlated** with useful structure in the sparse regime, awarding
its maximum to a single-hyperedge blob. Structural quality metrics in
graph retrieval need functional validation, not just topological
plausibility.

**Hypergraph spectral theory.** The correction in §5.2 pass 3 turns on
Zhou-style degree-normalised hypergraph Laplacians: a hyperedge over *k*
nodes is one incidence relation, and clique expansion silently converts
it into k(k−1)/2 relations, inflating connectivity by construction. The
broader framing — retrieval as a diffusion whose operator coefficients
are themselves updated — is Sturm–Liouville in spirit, and normalised
spectral graph theory supplies the scale-robust quantities we should have
used from the start.

**Agent memory systems.** MemGPT's paging between context tiers, the
memory-stream-plus-reflection architecture of generative agents, and
skill-library approaches such as Voyager all address the same practical
problem. Retrieval in these systems is typically scored by a blend of
recency, importance and embedding relevance, where importance is
model-assigned. That is precisely the self-report channel §1.1 rules out.
Our contribution relative to this line is the warrant discipline and the
witnessed-outcome-only update, plus the receipts channel that makes both
auditable.

**Bandits and exploration floors.** The exploration-mass floor is
ε-greedy's guarantee applied in coefficient space rather than action
space, and the minimum-exploration-probability construction in adversarial
bandit algorithms is its nearest formal relative. What is unusual here is
not the mechanism but the acceptance standard: the floor is required to
first *demonstrate* the collapse it prevents (§5.4).

**Provenance.** The bitemporal store and receipt chain are a provenance
graph in the W3C PROV sense, specialised so that the provenance is
consumed by retrieval rather than merely recorded for audit.

**Our own lineage.** The MetaCA work supplies the coupled
express-then-evolve structure of §3 and the braiding correspondence of
§3.2, and the honest report is that the correspondence is structural
rather than instantiated.

*A full bibliography with verified citations is a draft-2 deliverable;
this section names lineages and positions the contribution, and should
not be read as a citation-complete survey.*

---

## 8. Conclusion and research programme

### 8.1 What is established

The architecture is deployed and its parts hold under adversarial
inspection. Warrant discipline is implemented and mutation-tested (C1).
The receipts channel makes retrieval auditable, and its calibration gate
demonstrated that it binds by refusing its author's own promotion at
n = 1 (C2). The requirement that structural features pay functional rent
is supported as a negative methodological result: our own preregistered
spectral criterion was shown to be inverted in the sparse regime and is
retracted (C3). The learning loop has closed end to end once under
independent witness, and memory has transferred across runner models at
the level of decision structure.

What is **not** established is that the combination outperforms its
parts. That is not a hedge; it is the state of the evidence, and every
instrument needed to settle it now exists offline.

### 8.2 Future work for draft 2

Draft 2 is gated on data that a forthcoming run of the 489-problem APM
queue is expected to supply. This subsection is written to double as the
**instrumentation specification** for that run: what it must capture, and
what is unrecoverable if it does not.

#### 8.2.1 Capture requirements (unrecoverable if missed)

These cannot be reconstructed from a finished dispatch. Every problem
that runs without them is a permanently degraded sample.

1. **Per-memory surfacing attribution.** `:memory-use/surfacing-via` is
   **deployed** (`dispatch_with_recall.clj:877`, `memory_recall.clj:363`)
   and supersedes the constant-vocabulary `:inclusion-reasons` field
   (§5.5(ii)). Draft 2 must verify it is populated in live rows before the
   run scales — a deployed writer is not the same as a populated field.
   Without it, Ψ-v2 credit assignment stays lexically reconstructed and
   every coefficient inherits attribution noise.
2. **Outcome-half completion.** Currently **2 of 14 (14%)**. This is the
   single highest-leverage repair in the system: it multiplies the yield
   of every dispatch, and an uncompleted half wastes a full solve. Target
   ≥ 90%, with the completion rate itself reported per cohort.
3. **Dispatch-time corpus snapshots.** Experiment 0 requires
   transport-clean chronological state (§4.2). The corpus grows *during*
   the run, so if the graph and index state are not stamped as problems go
   by, dispatch-time state cannot be reconstructed afterwards and every
   query becomes contaminated by later knowledge.
4. **`:recall-reason` on every empty result.** Already partially present.
   Missing-vs-negative must be separable per row (§5.5(i)), or the
   benchmark's exclusion rule cannot be applied.

#### 8.2.2 Experiments unblocked by data, with their gates

| Experiment | Gate | Projection at 489 dispatches |
|---|---|---|
| **E1** Hybrid ablation (RQ1) | benchmark exists; ≥ 15 theorem families | Reachable — labelling, not accrual, is the cost |
| **E2** D_functional (RQ2) | benchmark + relevance labels | Reachable; harness exists |
| **E3** Receipt replay (RQ3) | `n-min-coeff` = 5.0 dark; **n ≥ 20 witnessed per coefficient** live | See below |
| **E4** Floor sensitivity (RQ4) | none — synthetic | Runnable now |
| **E5** Channel re-audit (RQ5) | post-repair receipts | Runnable as soon as repairs land |
| **E6** Warrant audit | `:witness-status` repair | Blocked on the repair |

**E3 arithmetic, at the audited rates.** At the current 14% outcome-half
completion, 489 dispatches yield ≈ 70 outcome halves and ≈ 35
metric-bearing rows. Credit distributes roughly as the pattern-star sizes
(30/21/14/10/… over 90 memories), so only the **top one or two**
coefficients would clear the n ≥ 20 promotion minimum. At a repaired 90%
completion the same 489 dispatches yield ≈ 440 outcome halves and the top
**five or six** coefficients clear. **The completion repair is worth
roughly five times as much as the dispatch volume it multiplies.**

Note that partial promotion is a legitimate result, not a fudge: the
harness's per-coefficient abstention (θ_p = 1 below minimum) is designed
for exactly this, so "top *k* coefficients promoted, tail abstained" is a
reportable Threshold-B outcome.

#### 8.2.3 Repairs draft 2 should report as done or explicitly deferred

- `:witness-status` derived from the presence and resolution of a witness
  record, so that a solve-lane memory lacking one is **reported rather
  than promoted** (§2.3).
- Text index: index `:subjects` and reconsider the `UNINDEXED` id column;
  raise or stratify the result limit so memories are not outranked out of
  the budget by coordination records (§5.5(iii)).
- Ladder tier-selection predicate to consider `:content-matches`, not
  only `:candidates` (§5.5(iii)).
- Term extraction to stop scraping dispatch-packet prose (§5.5(iii)).
- Connectivity meter to report normalised alongside unnormalised λ₂, or
  be removed from the gate path entirely (§5.2).

#### 8.2.4 Open research questions, unchanged by more data

- Whether the derived mid-tier technique clusters improve retrieval at
  fixed budget, tested as an intervention rather than assumed from
  cluster structure (§5.2).
- Whether a genuinely *braided* retrieval seam — structurally different
  operators alternating with feedback and a continuous mixing fraction —
  outperforms the current sequential composition (§3.2).
- Whether the warrant discipline's capability sacrifice is worth its
  auditability, testable once the semantic proposal lane runs under the
  review gate (§7).

### 8.3 Closing

The honest summary of draft 1 is that we have built a memory system whose
architecture is sound, whose instruments are sharp enough to have cut
their authors six times over (Appendix C), and whose central
effectiveness question is
still open because the observation channel that would answer it was
itself the thing most in need of repair. Reporting that in the body,
rather than discovering it in review, is the point of the exercise.

---

## Appendix A. Claims-grade specification

*This appendix states the mechanism at the precision a patent filing
would require. It is the defensive-publication payload; the body above is
the document.*

**Independent claim — the combination.** A memory system for autonomous
agents comprising:

- **(a) A typed, bitemporal hypergraph store.** A memory is a dialogue-act
  record: one append-only evidence entry (the body, full-text indexed)
  plus one typed hyperedge whose endpoints link the entry to its subjects,
  the session and mission that produced it, the transcript spans it
  distills, and the *patterns* it instantiates. Act vocabulary = IATC
  performatives (assert/retract/challenge/agree/define/…); bitemporal
  (valid-time/system-time) so belief-as-of-T, correction lag, and
  supersession are queryable objects, not file overwrites.

- **(b) Pattern-conditioned recall.** Retrieval is propagation, not
  lookup: query-derived seed terms → full-text match → typed edge to a
  pattern endpoint → the memories attached to that pattern. The pattern is
  the reusable retrieval handle; the memory stays a concrete episode.

- **(c) A warrant discipline (boundary conditions).** Only independently
  REVIEWED edges conduct retrieval. Lexical bridges (pattern descriptions)
  and semantic similarity (embeddings) may *propose* candidates; they
  never *warrant* them. Editing a pattern's description edits a
  conductance, not a warrant.

- **(d) A receipts observation channel.** Every dispatch writes an
  *offered* half (surfaced ids, query, term sources, recall-system
  version); every independently witnessed outcome writes an *outcome* half
  (used ids from the agent's own citation, result, witness refs). Reasoned
  non-use is a first-class outcome. The channel is never-blocking: recall
  failure cannot block dispatch.

- **(e) A receipt-consuming operator update (Ψ).** Retrieval coefficients
  are reweighted from the receipts channel by a bounded, cold-start-
  neutral, audited update rule — retrieval whose *operator* learns from
  witnessed use, not from self-report.

- **(f) An exploration-mass floor.** The coefficient vector retains a
  minimum mass ε > 0 on every admitted relation type at every step — an
  explicit, reported parameter guarding against endogenous confirmation
  collapse.

The claim is the *combination*: (a)–(b) alone is a graph memory; (c)
alone is review policy; (d)–(e) without (c) learns from contaminated
signal; (e) without (f) amplifies confirmation. Together they close a loop
in which retrieval behaviour is updated only by independently witnessed
outcomes, under warrants, with collapse guarded.

**Dependent embodiments** (status dated; see §2.4):

1. *Scalar per-memory Ψ*: multiplier `(1 + α·used/offered)`, α = 0.5,
   cold-start neutral, audited in the receipt. **Deployed** (S6).
2. *One-outcome per-pattern update*: θ multiplied by the posterior-
   probability ratio of a beta-binomial useful-progress model refit on
   exactly one new independently witnessed transition; promotion gated on
   a calibration minimum (n ≥ 20). **Implemented dark** (Rung 2).
3. *Budgeted facet refinement*: next facet expansion chosen by expected
   information gain per cost, with child eligibility gated on witnessed
   parent-edge warrants. **Implemented dark** (Rung 3).
4. *k-step coupled propagation*: iterate x/θ updates under budget with
   per-step audit (entropies, path diversity, challenge reachability,
   explicit termination class) and the floor of (f); acceptance built on a
   confirmation-collapse battery — floor-off ablation must collapse,
   floor-on must recover a planted target. **Implemented dark, verified
   2026-07-27** (Rung 4 / WS1; §5.4).
5. *Spectral admissibility diagnostics*: algebraic connectivity λ₂ and
   spectral gap as a threshold below which retrieval dynamics cannot beat
   direct lookup. **RETRACTED as a gate, 2026-07-30** (§5.2): the
   criterion is anti-correlated with useful structure in the sparse
   regime, awarding its maximum to a single-hyperedge component. Spectral
   quantities may be reported descriptively and gate nothing.
6. *Pattern-level receipt conductances (Ψ-v2)*: fractional credit
   assignment from receipts through reviewed attachments; per-coefficient
   abstention below an observation minimum; promotion gated at n ≥ 20 per
   coefficient. **Implemented dark, verified 2026-07-28**; live verdict
   `:below-calibration-minimum` at n = 1 (§5.3).
7. *Semantic proposal lane*: embedding nearness as an additional proposal
   source strictly under (c). **Chartered** (WS4 / S5).
8. *Curriculum forcing*: coverage maps plus memory-proposed construction
   targets select what the system experiences next (the forcing term F_t).
   **Deployed** (cohort-2 curriculum lane).
9. *Bitemporal learning meters*: correction lag, belief-as-of-T,
   time-to-fill as queryable observables of the learning process itself.
   **Substrate deployed; meters partial.**

## Appendix B. Artifacts and reproduction

| Artifact | Path | Determinism |
|---|---|---|
| Receipts export (frozen) | `holes/labs/M-memory-retrieval/receipts-export-20260728.edn` | bounded read-only capture |
| Channel audit | `holes/labs/M-memory-retrieval/observation-channel-audit-20260730.edn` | deterministic; regenerate with `observation_channel_audit.py` |
| Warrant audit | `holes/labs/M-memory-retrieval/warrant-audit-20260730.edn` | deterministic; regenerate with `warrant_audit.py`; cross-checks against raw `:witnesses` count |
| Ψ-v2 replay | `psi_v2_replay.bb`, `psi-v2-replay-results-20260728.edn` | deterministic; ties broken by memory id |
| Damage sweep | `damage_state_sweep.bb`, `damage-state-fixture-20260730.edn`, `damage-state-results-20260730.edn` | write-once; byte-reproducible on rerun |
| Ψ-v2 design (preregistration) | `psi-v2-design.md` | — |
| Evidence ledger (chronology) | `docs/retrieval-evidence-ledger.md` | — |

## Appendix C. Correction and retraction record

The full chronology, with each correction in the order it occurred, is
preserved in `docs/retrieval-evidence-ledger.md`. The load-bearing
corrections, summarised:

1. **Spectral admissibility retracted** (2026-07-30, three passes): first
   the topical-conduction prediction failed; then the repair was shown to
   turn the meter green without improving retrieval; then a second agent
   showed the operator itself was wrong (clique expansion of a hyperedge)
   and the criterion inverted. §5.2.
2. **An infrastructure failure diagnosed as an architectural one**
   (2026-07-30): a zero-recall run attributed to fragmentation was
   `:recall-reason :timeout`. §5.5(i).
3. **Text-index cause misattributed** (2026-07-30): zero hits blamed on
   atomic indexing of hyphenated compounds; the real causes are
   bounded-result starvation and body-only indexing. Correction supplied
   by the implementing agent. §5.5(iii).
4. **`:inclusion-reasons` characterised as empty** (Ψ-v2 design note,
   2026-07-28): it is populated with a single constant string across the
   corpus. Same consequence for credit assignment, different repair.
   §5.5(ii).
5. **`:witness-status` disclosed as underivable from evidence**
   (2026-07-30) and excluded as an evaluation signal. Strengthened the
   same day by audit: the field is not merely underivable but
   *uncorrelated* with witness records, landing one on each side of the
   only two cases where such records exist. §2.3, §5.6.
6. **The warrant audit corrected itself twice before reporting**
   (2026-07-30): two successive brace-walking implementations returned a
   clean 53-of-53, which was an artifact of never reaching the memory
   body. Caught by hand-checking the one memory known to carry a witness
   record. §5.6.

*Draft 1 closed 2026-07-30. The living increment log of the pre-draft
period is retained in the evidence ledger and is not reproduced here.*
