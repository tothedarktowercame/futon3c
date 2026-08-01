# Receipt-Closed Retrieval over a Typed Memory Hypergraph: an Evolving-Operator Memory System for Autonomous Agents

**Status: LIVING DRAFT** (opened 2026-07-27, claude-6 under
M-memory-retrieval WS6; restructured 2026-07-28 to whitepaper form —
narrative body, claims-grade enumeration retained as Appendix A). Grows
increment by increment as WS1–WS5 evidence lands; each increment dated.
Intended audience: technical readers outside the futon stack (first:
Rob). Sending gate: Joe.

*Provenance note: the mechanism described here has been developed in
public repositories with dated commits (descriptions and working code,
2026-07-22 onward). This document consolidates that disclosure
deliberately, serving as defensive publication. It is not legal advice;
a US filing within the inventor grace period remains a separate
decision, and Appendix A is written to keep that option cheap.*

## Abstract

An autonomous agent that stores what it learns does not thereby remember
it: a memory that surfaces only when the agent happens to issue the
right query is storage, not memory. We describe a memory system in which
retrieval is a dynamical process over a typed hypergraph of memories and
patterns — and in which the retrieval operator itself is updated by
evidence of use. Memories are typed dialogue acts attached to reusable
patterns; recall propagates from query terms through independently
reviewed edges to patterns and their attached memories; every dispatch
and every witnessed outcome writes a receipt; the receipts update the
operator's coefficients; and an exploration-mass floor prevents the
update loop from collapsing onto its own early successes. The system is
deployed in a live learning loop for mathematical theorem proving,
where its first witnessed chain — a memory mined from failed sessions,
recalled by pattern into a later session, cited by the solver, and
confirmed by the compiler — closed the loop end to end. The theory the
deployment shadows, its measured preconditions, and its falsification
instruments are presented with the deltas stated honestly. Two framing
facts: the system uses commodity agent models throughout — the
capability delta is the memory loop, not model scale; and it is
operationally autonomous — no human-authored proof code and no
human-in-terminal proof sessions, with the human contribution confined
to the strategic layer (memory architecture, literature policy,
promotion gates) and the split auditable from the dispatch and receipt
ledgers.

## 1. The problem

Agent memory systems fail in a characteristic way that has nothing to do
with storage. Our own first deployment demonstrated it precisely: an
agent deliberately recorded four typed memories from a proof session —
correct, well-formed, full-text indexed — and they were operationally
invisible, because nothing connected them to the moments where a later
session would need them. A saved memory surfaced only if the agent
independently thought to search for it, with the right words, at the
right decision point. That is a diary, not a memory.

Three further failure modes constrain any fix:

- **Self-report contamination.** If the system learns from the agent's
  own claims of success, it learns to claim success. Every signal that
  updates retrieval must be independently witnessed (here: a compiler).
- **Endogenous confirmation.** A retrieval mechanism that learns which
  relations retrieve "useful" material will concentrate on relations
  that retrieve corroboration of what it already found — apparent
  certainty, hidden targets. Any learning retrieval loop needs an
  explicit guard, and the guard needs to be demonstrated against the
  failure it guards, not asserted.
- **Proposal/warrant confusion.** Similarity — lexical or semantic — is
  cheap and wrong often enough that letting it *justify* retrieval
  quietly replaces evidence with plausibility. Similarity may propose;
  only review may warrant.

The design question: what is the smallest mechanism that makes stored
memories arrive unprompted at the decision points where they matter,
gets better at this from real use, and provably resists the three
failures above?

## 2. The design

Six elements, each answering a piece of §1. (The precise, claims-grade
statement of each is Appendix A; the combination, not any single
element, is the contribution.)

**Typed memories on a hypergraph.** A memory is a dialogue act — assert,
retract, challenge, agree — recorded as an append-only body plus a typed
hyperedge linking it to its subjects, session, mission, the transcript
spans it distills, and the *patterns* it instantiates. The store is
bitemporal: what is believed now and what was believed as-of T are both
queryable, so correction is an event with a timestamp, not an overwrite.

**Patterns as retrieval handles.** Memories stay concrete episodes;
patterns are the reusable hooks they hang on. Recall runs from the
task's own text through matching patterns to their attached memories —
so a memory arrives because the *situation* recurred, not because the
agent remembered to ask.

**Reviewed edges as the only conductors.** The edges recall traverses
must be independently reviewed. Descriptions and embeddings can nominate
candidates, but nomination and warrant are architecturally distinct —
editing a description tunes a conductance; it cannot mint a warrant.

**Receipts as the observation channel.** Every dispatch records what was
offered; every witnessed outcome records what was used, unused, or
rejected with reasons. Honest non-use is data. Recall failure never
blocks work — the channel observes; it does not gate.

**An operator that learns from receipts.** Retrieval coefficients are
updated by bounded, cold-start-neutral, audited rules fed *only* by the
receipts — evidence of witnessed use, never self-report. The deployed
version is deliberately minimal (a scalar per memory); the design ladder
lifts the same update onto patterns, edge types, and ultimately the
coupled dynamics of §3.

**An exploration-mass floor.** The coefficient vector keeps a stated
minimum mass on every admitted relation type at every step. This is the
anti-confirmation guard, and it is falsifiable: remove the floor and the
system must demonstrably collapse onto a decoy; restore it and the
planted target must be recovered (§5).

## 3. The formal frame

Retrieval state s_t = (x_t, θ_t, F_t, B_t): activation over admissible
memory nodes; activation over patterns/typed traversal operators; facet
resolution; traversal budget. For relation-specific graph operators Δ_r,
the pattern-conditioned operator is Δ_θ = Σ_r θ_r Δ_r, and the coupled
dynamics are

    x_{t+1} = Φ(x_t, Δ_{θ_t}, q)        (state update)
    θ_{t+1} = Ψ(θ_t, x_{t+1}, q)        (operator update)

— a diffusion whose Laplacian is itself updated by observation: "updating
*which* Laplacian" (Rob, 2026-07-25), i.e. a second-order operator with
location-dependent coefficients being selected within a family. The
degenerate regimes are named: **collapse** (all activation on one node;
confirmation) and **dissipation** (flat activation; ranking nothing). The
exploration-mass floor is the coefficient-space constraint holding Δ_θ
inside the sustaining band between them.

### 3.1 Correspondence table I — MetaCA / retrieval flow / operator theory

| MetaCA (draft5, coarse/binary) | Retrieval flow (fine/real) | Operator frame |
|---|---|---|
| Phenotype X_x(t), binary | Activation x_t over the typed memory graph | Coarse state of a heat/wave finite-difference update |
| Genotype g_x(t): one rule per site, evolving | θ_t (relation conductances), evolving via Ψ | "Updating which Laplacian" — Sturm–Liouville coefficients, location-dependent |
| Coupled MetaCA step (express rule, then evolve rules) | Φ then Ψ | State update, then operator update |
| Writings/propagators; the bijective core | The family {Δ_θ}; admissible θ region | The operator family being selected within |
| Cycle-structure classification; gcd-1 sustains | Spectral classification of Δ_θ (implemented, WS2) | Which operators produce non-trivial structure over time |
| Edge of chaos: combining peaks at transition | Sustained multi-level structure between heat-death and collapse | Non-trivial structure |

### 3.2 Correspondence table II — theory object → shipped counterpart

| Theory object | Live counterpart | Where |
|---|---|---|
| x_t propagation over the typed graph | Pattern-mediated recall: FTS seed → reviewed edge → pattern endpoint → attached memories | `memory_recall/propose-patterns-by-query` + `recall-by-endpoint` |
| Δ_θ coupling structure (which edges conduct) | Pattern attachments + pattern descriptions (lexical bridge rows); editing descriptions = editing conductances | `wire_math_memory_patterns.clj` |
| Query source term q | Problem-file term extraction (tuned stopwords) | `dispatch_with_recall/problem-term-sources` |
| Temporal braid (alternate rather than compose; draft6 Def. 5) | Query tier ladder: triple → pairs → singles, first non-empty wins. Structural parallel only — no temporality, feedback, or mixing fraction (§3.3) | `dispatch_with_recall/query-ladder` |
| Boundary conditions B_t | The warrant discipline: only reviewed edges conduct | P1 acceptance rule; charter S5 |
| **Ψ (the operator update)** | **S6 receipt-informed ranking: per-memory `(1 + α·used/offered)` — the first live Ψ** | `dispatch_with_recall` receipt stats |
| Forcing F_t | Curriculum lane: coverage map + memory-proposed construction targets | cohort-2 prereg |
| Widened proposal support | Semantic lane (embedding proposals, under B_t) | charter §S5 |

### 3.3 Query decomposition and temporal braiding — a structural correspondence

The 2026-07-30 durée runs exposed a failure with a shape already named in the
MetaCA work, and the correspondence is worth recording precisely because it also
shows where the analogy stops.

The lexical seam queries a **conjunctive** full-text backend. Empirically, hit
count falls off a cliff as terms are added: measured on the live store, 1 term
returned 5 hits, 3 returned 3, 7 returned 2, 12 returned 1, and 29 returned 0.
Recall was assembling a single query from up to 36 terms, so the composed query
was reliably **dead** — and every "recall empty" datum in the codex lane before
that date turned out to be this, or one of two other plumbing faults, rather
than a finding about recall semantics.

The remedy was to stop composing. Instead of one conjunction over the term set,
the seam now walks a *ladder*: the 3-term conjunction, then each 2-term pair,
then singles, stopping at the first tier that returns candidates. Measured
across five live rows using the real term-extraction output, the composed
3-term query returned **zero** memories for **all five**, while the decomposed
ladder surfaced a memory for **three**. Two rows returned nothing at any tier,
correctly — those terrains have no memories in the corpus.

The structural parallel to temporal braiding (draft6 §Braiding, Definition 5,
Finding 3) is exact in one respect and inexact in three, and both halves matter:

**Where it holds.** Braiding's central observation is that *alternating* the
elementary updates of two operators, rather than *composing* them into a single
map, can sustain a field that either operator collapses on its own. The
retrieval case has the same shape: the conjunction of individually-productive
constraints is unproductive, and the remedy is not a better composite but a
*schedule* over the constituents. In both settings the composed map is the dead
one, and the decomposition recovers structure the composite cannot express.

**Where it does not hold.**
1. *No temporality.* A braid alternates within one evolving run and its
   constituents interact through the intermediate coupled state. The ladder is a
   static fallback over independent queries; no state is carried between tiers.
2. *No feedback.* Braiding is interesting partly because the schedule
   participates in river construction — the field the operators act on is
   itself shaped by their alternation. Ladder tiers do not reweight one another.
3. *No mixing fraction.* The braiding result that matters most for control is
   that the mixing fraction moves diversity **smoothly**, turning a switch into
   a dial. The ladder has no continuous parameter: it short-circuits at the
   first non-empty tier, so it is a switch.

There is also a taxonomic caveat: the ladder's tiers are *subsets* of one term
set, i.e. successive relaxations of a single operator, not two distinct
operators alternating. Strictly, this is decomposition-under-collapse rather
than braiding.

**What the correspondence suggests.** Taken as a research direction rather than
a claimed result, it points at a genuinely braided retrieval seam: alternate
*structurally different* query operators (lexical, graph-endpoint, embedding)
rather than subsets of one; carry feedback, so what tier *n* surfaces reweights
tier *n+1* (this is Ψ acting within a single recall rather than between runs);
and expose the mixing fraction as a precision/breadth **dial** instead of a
short-circuit. The first of those is already half-present — the seam runs a
lexical proposal stage and a graph-endpoint stage — but they are composed in
sequence, not braided, and their interaction has never been measured.

n is small and the effect is a plumbing repair, not evidence for a theory. The
honest reading is that the collapse-under-composition phenomenon showed up in a
second, unrelated substrate, which is weak evidence that it is a property of
conjunctive operator composition rather than of cellular automata.

## 4. Architecture and embodiments

*[Increment pending — substrate (typed dialogue-act memories over the
bitemporal hypergraph), the recall stack, the seat separation
(runner/scribe/ground-control/operator; author ≠ reviewer ≠ runner), the
mining lanes, and one subsection per embodiment of Appendix A with its
executable demo and verification record. Sources: M-typed-memories,
M-zai-learning-loop, algorithms/zai-learning-loop.md, E-dynamic-queries
verification blocks.]*

### 4.1 The memory hierarchy: deployed, measured, and proposed

Stated separately from the evidence in §5 so that what is *shipped* is
not confused with what is *designed*.

**Deployed (two tiers, one relation).** Patterns → memories, fan-out
9:1, every memory on exactly one pattern, traversed by a single
hardcoded `:memory/assert` edge read. Memories additionally carry
`:level` and `:lane` fields; **neither is a retrieval handle** — nothing
in the recall path reads them. They cost drafting effort and carry no
retrieval weight, which is the likeliest explanation for `:level`'s
vocabulary drift (§5): a field with no consumer has no pressure toward a
controlled vocabulary.

**Measured (the middle tier already latent in the store).** The
concrete-handle relation partitions the same 90 memories into 29
clusters of sizes 11, 8, 7, 6, 5, 5, 4, 4, … — mathematical technique
neighbourhoods (`connectedComponents_complement_lemniscate_le`,
`young-convolution-L1-L2`, `radial-integration-r3`) that cut across the
pattern tier. These are *observed*, not designed: they are the
components of a relation already recorded on every attachment.

**Proposed (three tiers, two relations).** Patterns (coarse, ~10) →
technique clusters (mid, ~29, derived) → memories (90). The operator
gains a genuine second Δ_r, so Σ_r θ_r Δ_r ceases to be a one-term sum
and the exploration-mass floor ceases to constrain a singleton. Three
commitments keep this from being a re-labelling exercise:

1. *Derived, not annotated.* The mid tier is computed from `:subjects`
   already in the store. A tier that requires a fresh annotation pass
   would decay exactly as `:level` did.
2. *Warrant is unchanged.* The subject relation nominates; it does not
   confer warrant. Reviewed-edge conduction (§2) still gates what
   recall may traverse, so widening the relation set must not widen the
   warrant set — a derived edge inherits the review status of the
   attachment it is derived from, or it does not conduct.
3. *The prediction was falsifiable and cheap. **It was run, and it
   failed.*** The prediction was that admitting the subject relation
   would raise λ₂ above the 0.1 floor with no new memories written.
   Measured against the frozen `20260730` export (method validated by
   reproducing the meter's own reading to 0.0754 vs 0.0755):

   | graph | nodes | components | unnormalised λ₂ | normalised λ₂ |
   |---|---|---|---|---|
   | pattern + distills (baseline) | 147 | 19 | 0.0754 | 0.0386 |
   | + subjects as nodes | 315 | 4 | 0.0836 | **0.0291** |
   | + shared-subject **projection** | 183 | 4 | **0.2002** | **0.0235** |

   **Retraction.** Admitting subjects as nodes does not clear the floor,
   and on the scale-robust normalised Laplacian it makes conduction
   *worse* (−24.5 %). The concrete handles average 2.7 uses each and 116
   of 161 are used only once, so they attach pendant nodes rather than
   shortcuts. The topical-conduction account as stated in the first
   draft of this section is **wrong**, and the paragraph that proposed it
   is withdrawn rather than softened.

   **And the repair exposes a defect in the instrument, which is the more
   valuable result.** Projecting the relation instead — joining two
   memories directly when they share a concrete handle — *does* clear the
   meter's stated criterion, 0.0754 → 0.2002 against a 0.1 floor. But it
   clears it while normalised conductance *falls* by 39 %. The reason is
   mechanical: the meter's λ₂ is computed on the **unnormalised**
   Laplacian, where adding edges raises algebraic connectivity almost by
   construction, and a projection turns every k-memory handle into a
   k-clique. The result is many locally dense clusters that are no better
   connected to each other. **A change that improves nothing about
   retrieval would have turned the meter green.**

   This matters beyond the present question. WS2's criterion is
   preregistered and load-bearing, and as written it can be satisfied by
   edge inflation. It should be normalised — or at minimum report both
   figures — before it is used to gate any claim, including any future
   claim by us that the connectivity floor has been cleared. Until then,
   no `:dynamics-informative` verdict should be accepted without asking
   what happened to the edge count.

   **Third pass, same day: the withdrawal stands but its stated reason
   was wrong, and the metric is worse than gameable — it is inverted.**
   Review by a second agent (E-memory-topology, Codex-3) made the
   decisive objection: a handle touching *k* memories is **one incidence
   relation, not k(k−1)/2 memory relations**, so the clique expansion
   above was never the right operator. Recomputing with the
   degree-normalised hypergraph Laplacian (Zhou; nodes = memories,
   hyperedges = handles, no clique expansion):

   | operator | largest component | hyperedges in it | λ₂ |
   |---|---|---|---|
   | patterns only | 33 memories | **1** | **1.0000** |
   | patterns + concrete subjects | **126** memories | 51 | 0.0689 |

   The patterns-only figure is λ₂ = 1.0 because its largest component
   *is a single hyperedge* — one pattern containing 33 memories. A single
   blob attains maximal λ₂ by construction. Admitting topical handles
   raises reach from 33 to 126 memories, cuts components from 15 to 3,
   and **lowers λ₂ precisely because the structure stops being trivial.**

   In this regime λ₂ is therefore *anti-correlated* with useful
   structure, and WS2's criterion 3 does not merely admit edge inflation:
   **it awards full marks to the most degenerate topology available.** The
   2026-07-27 reading of λ₂ = 0.99999 was recorded as the one criterion
   that passed; it passed because the graph was a 6-node star. No
   promotion decision should rest on that criterion in either direction.

   What survives as a positive result is the **reach** figure, which is
   retrieval-relevant in a way the spectra are not: the topical relation
   makes four times as many reviewed memories reachable from a seed.
   Whether that improves *retrieval* is a held-out question and is not
   settled by any spectral quantity. The governing rule adopted from
   E-memory-topology is: *a structural change is good only when it
   improves held-out retrieval or witnessed outcomes at the same read
   budget, while preserving domain, temporal, review, provenance and
   witness invariants.* The prediction of §4.1 remains withdrawn — not
   because topical relations were shown to hurt, but because **all three
   measurements taken today were of the wrong quantity.**

Retiring or consolidating `:level` is a consequence, not a goal: a type
dimension earns its place by being read, and the design question is
whether the mid tier should *be* a controlled `:level` vocabulary or
whether `:level` should be dropped in favour of the derived clusters.
The census argues for derived — 28 singleton values in 91 drafts is what
an unread field looks like after 24 passes — but this is not yet
settled, and no consolidation has been performed.

## 5. Evidence

**Measurable improvement (system level, already witnessed).** The
enclosing learning loop closed end-to-end 2026-07-25/26: a memory mined
from failed sessions (a lemma absent from the library), recalled by
pattern into a revisit of the same problem, cited by the runner as
redirecting its effort, producing a proven lemma and a sorry-count drop —
with both receipt halves written and the outcome independently witnessed
by the compiler, not self-reported (cohort-1 revisit arm).

**Specific contribution of the evolving-operator mechanism: pending, by
design.** The deployed Ψ is the minimal scalar shadow; the per-edge-type
dynamics are dark or in build, and their marginal contribution over the
scalar baseline is exactly what the control-arm discipline (fixed
endpoint order and one-step typed ranking retained as named
counterfactuals in every trace) is built to measure. We would not be
building it without expecting impact; the document will claim only what
the counterfactuals show. *[Increments: WS3 dark replay of Ψ-v2 vs
scalar Ψ vs no-Ψ on frozen receipts — in build.]*

**The calibration gate binds in practice (WS3, 2026-07-28).** The Ψ-v2
dark replay ran three ranking arms (no-Ψ / deployed scalar /
pattern-level) leave-one-out over the frozen receipt corpus and refused
to claim anything: one metric-bearing row, all arms MRR 1.0, every
pattern coefficient below its activation minimum, verdict
`:below-calibration-minimum`. The ranking contract itself is validated
on a synthetic fixture whose correct answer is computable by hand (a
planted good-route coefficient earns θ = 1.5 and deterministically
flips the ranking; asserted in the harness, not eyeballed). A system
that publishes its own "not enough evidence yet" verdict alongside a
proven-correct mechanism is the evidentiary posture this document
maintains throughout: contracts first, claims only when the
counterfactual arms can speak. The replay also identified the binding
constraint on reaching calibration: outcome-half completion (12 of 14
dispatched jobs had offered receipts but no witnessed-outcome half) —
an operational fix, not a design change.

**The connectivity floor is a property of the reader, not of the corpus
(2026-07-30).** §6 records the deployed corpus as `:component-limited` —
below the connectivity floor at which an operator family beats direct
lookup. A structural census of the 90 promoted memories in the
mathematics domain locates that limit precisely, and it is not corpus
immaturity.

Every one of the 90 memories is attached to **exactly one** pattern
(90/90; the promotion pipeline writes `:patterns [pattern]`, singular).
Under the relation recall actually traverses — recall issues a single
hardcoded `{:type :memory/assert}` hyperedge read — the corpus is
therefore a **disjoint union of 10 stars**, of sizes 30, 21, 14, 10, 4,
3, 3, 2, 2, 1. No memory bridges two patterns, so no path of length > 2
exists anywhere in the graph the operator sees. This is the v0 sweep's
degenerate regime reproduced exactly, and it is *closed under adding
memories*: more memories make the stars fatter, never connected.

The same edges already carry a second, richer role. Alongside
`:patterns`, each attachment records `:subjects` — 133 distinct values
over 365 uses, including concrete mathematical handles
(`connectedComponents_complement_lemniscate_le` on 7 memories,
`young-convolution-L1-L2` on 8, `radial-integration-r3` on 6).
Recomputing components with shared-subject as the edge relation, and
discounting the mission id that trivially joins 88 of 90:

| relation used as the edge | components | largest |
|---|---|---|
| shared **pattern** — what recall traverses | **10** | 30 |
| shared **subject** (adds the concrete handles) | **2** | 87 |
| shared **concrete subject only** (pattern ids excluded) | 29 | 11 |

Admitting one further relation type collapses ten components to two. The
corpus is *already* connected along a relation the **retrieval path**
cannot see.

**A fresh meter reading the same day corrected the first draft of this
section, and the correction is instructive** (WS2 instrument, stamp
`20260730`; the frozen `20260727` reading reproduces unchanged). This
section originally claimed the deployment admits a single relation type,
so that Σ_r θ_r Δ_r is a one-term sum and the meter's criterion 2 (≥ 2
distinct edge types) is what fails. **That was an error of inference:
the relation set of the *graph* was read off the traversal filter of the
*recall path*.** They are not the same. The store also carries
`:distills` edges — memory to the transcript spans it was mined from —
and the meter counts them.

| | 2026-07-27 | 2026-07-30 |
|---|---|---|
| nodes / edges | 83 / 51 | 219 / 233 |
| largest reviewed component | 6 | **146** |
| edge types in it | `[:pattern-attachment]` | `[:distills :pattern-attachment]` |
| λ₂ | 0.99999… | **0.0755** |
| criterion 1 (≥10 nodes) | fail | **pass** |
| criterion 2 (≥2 edge types) | fail | **pass** |
| criterion 3 (λ₂ > 0.1) | pass | **fail** |
| verdict | `:component-limited` | `:component-limited` |

The verdict is unchanged but **the binding constraint has moved**, and
the two readings are different failures wearing one label. On 07-27 λ₂
was 0.99999… — the exact spectral signature of a star K₁,ₙ — on a
6-node component: too small, too star-like. On 07-30 the graph is a
single 146-node component with λ₂ = 0.0755 against a 0.1 threshold:
large, connected, and *weakly* connected. The system moved from
"degenerate because tiny" to "degenerate because diffuse", which is the
`dissipation` regime of §3 rather than `collapse`, and it is close to
clearing (the next eigenvalues are 0.0957 and 0.1103).

This makes the pattern-tier finding sharper rather than weaker. The
connectivity now carrying that 146-node component is `:distills`, which
is **provenance, not topic** — it links memories that happened to be
mined from the same transcript turn. Provenance connectivity supports
diffusion in the graph-theoretic sense while conducting very little
*topical* signal, which is a candidate account of why λ₂ sits low: the
component is held together by edges that do not track what memories are
about. The subject relation measured above is topical, and the
prediction stated in §4.1 is correspondingly about the **spectral gap**,
not about component count, which was already achieved by an edge type we
were not counting.

The third row is the more interesting one for design. The concrete-handle
relation alone yields 29 components of sizes 11, 8, 7, 6, 5, 5, 4, 4 and
a tail of 3s — neither a star forest nor a single blob, but mid-scale
technique clusters. The deployed hierarchy has two tiers (10 patterns
over 90 memories, a 9:1 fan-out with 39 % of the corpus behind one hub,
and that hub — `missing-dependency-protocol` — is a *process* category,
not a mathematical area). The measured cluster structure is the missing
middle tier, and it is recoverable from data already in the store rather
than from a new annotation pass.

Two quality readings from the same census, recorded because they bound
what recall can deliver at the point of use: **22 of 90 memories (24 %)
carry a `:hook` identical to the memory name**, and the memory `:level`
field — nominally a type dimension — has **40 distinct values over 91
drafts, 28 of them singletons** (70 % of the vocabulary, 30 % of the
corpus). A type dimension whose vocabulary grows linearly with its
instances is not typing anything; it is free text with a colon. The hook
figure was reached independently twice, by this author over 68 memories
and by a second agent over a disjoint 18 (6/18), which is why it is
stated as a corpus property rather than a sampling artifact.

**Recall discards its own best evidence (traced end-to-end,
2026-07-30).** The star-forest measurement above predicts poor recall;
a live dispatch the same evening showed the actual failure path, which
is sharper than the prediction and partly independent of it.

Durée run 25 dispatched on problem a92J05 (a Rouché row). Eighty-seven
seconds earlier, a memory named
`close-a92J05-by-transferring-the-unit-disk-zero-count` — drafted from
the immediately preceding attempt on *that same problem*, recording its
exact residual goal — had been promoted, attached, and verified
`:attachment-status :reviewed`. It did not surface. Five memories did,
and **all five belonged to a single pattern**
(`math-formalization/tactic-algebra-interference`); four were
measure-theory memories the runner dismissed as irrelevant.

Replaying the dispatch's own query ladder against the store shows the
retrieval was not the problem. The dispatch query was
`roots outside unit disk filter card`; its first tier,
`"roots outside unit"`, returns 7 memories **including the a92J05
memory**, as do `"roots outside"`, `"outside unit"`, `"unit disk"` and
`"roots"`. The text index found the right memory on essentially every
tier.

The loss is downstream. `propose-patterns-by-query` searches evidence
text, then converts the matching rows into *pattern* proposals;
`recall-by-endpoints` then returns memories attached to the winning
pattern. So the pipeline is

    query → matching MEMORIES → their PATTERNS → top pattern → that pattern's memories

and the memory that actually matched is returned **only if its pattern
also wins**. Here it did not: the matched memory sits on
`math/missing-dependency-protocol`, the proposal resolved to
`tactic-algebra-interference`, and the match was discarded in favour of
five siblings of a different winner. **The system found the right
answer and then threw it away.**

This is a distinct fault from the fragmentation measured above, and the
two compound. Fragmentation guarantees that a technique cluster spans
several patterns; single-pattern resolution then guarantees that at most
one of those patterns can contribute.

**Correction (2026-07-30, fourth pass): a companion claim in the first
draft of this paragraph was wrong, and the error is methodologically
worth more than the claim.** That draft also explained the *preceding*
run on the same row — which surfaced zero memories despite six reviewed
memories carrying its exact subject handle — as the same fragmentation
effect. Inspecting the persisted offered-half record for that dispatch
shows `:recall-reason :timeout`. Recall never completed; nothing was
arbitrated, fragmented, or discarded. **It was an infrastructure failure
that I diagnosed as an architectural one.**

The reason the error was available to make is itself the finding. The
dispatch path collapses `:timeout`, `:store-unavailable` and a genuine
empty result into a single runner-visible signal — "no dispatch-time
memories were supplied" — so the runner's report cannot distinguish
them. Across the offered records in this corpus, **21 carry
`:recall-reason :timeout` and 13 `:store-unavailable`.** A substantial
share of this lane's recorded "recall surfaced nothing" observations are
therefore infrastructure failures rather than retrieval evidence, and
any analysis resting on the aggregate — including parts of the
fragmentation argument above — must be re-derived against
`:recall-reason` rather than against surfaced counts.

The arbitration finding stated in this section is *not* affected: that
dispatch recorded `:recall-status :ok` and returned five memories, all
from one pattern, with the content match discarded. It is diagnosed from
a completed recall. The distinction matters precisely because the two
runs are adjacent on the same problem and produced the same
runner-facing sentence for entirely different reasons.

Two further defects surfaced by the same record, both ours: the query
terms are partly scraped from the dispatch packet's prose
(`:term-sources [{:source :stdin-packet, :terms ["route" "search"
"when" "target" …]}]`), so operator-written preamble text dilutes the
query; and the ladder's tier-selection predicate tests only
`(:candidates p)`, ignoring `:content-matches`, so a tier that finds a
content match but no pattern candidate is skipped. The latter is latent
rather than implicated here — measured tier by tier, every tier with
content matches on this row also had candidates.

Two further indexing observations from the same replay: `"a92J05"` and
`"rouche"` each return **zero** memories at the deployed limit, although
a memory is *named* `close-a92J05-…` and eight carry the subject
`rouche-root-count-transfer`.

**Correction (2026-07-30, second pass).** A first draft of this paragraph
attributed those zeros to atomic indexing of hyphenated compounds, and
implied the store was responsible. Both parts were wrong, and the
correction came from the implementing agent rather than from this
author. The surface is **not** an XTDB-native text index: it is an
application-controlled SQLite FTS5 sidecar (`futon1bi.text-index`)
configured `tokenize='unicode61'`, in which hyphens are ordinary
separators. The real causes are (i) **bounded-result starvation** — the
targets do rank, at 64 and 30 respectively under `limit=100`, and simply
fall outside smaller deployed limits because coordination records
outrank memories — and (ii) **body-only indexing**: only
`:evidence/body` is indexed, the id column is explicitly `UNINDEXED`,
and top-level `:subjects` are not indexed at all.

(ii) is the more consequential and it is squarely ours to fix. The
concrete handles that carry this corpus's topical structure are not
merely hard to reach; **they are absent from the index**. That is a
retrieval-layer fact and should not be confused with the graph-layer
claim retracted in §4.1 — indexing subjects would make them *findable*,
which is a different proposition from the refuted claim that relating
them improves graph conductance. Changing indexed fields requires a
sidecar schema decision and rebuild, and has not been done.

The remedy for the arbitration defect itself is narrower than any of
this: **return content matches directly alongside pattern-mediated
recall**, so nomination by content cannot be overwritten by pattern
arbitration. This was implemented and independently reviewed on
2026-07-30 (`50916c84`). Reviewed text matches now survive as
`:content-matches` and merge ahead of endpoint recall; every surfaced
memory carries `:via :content-match | :pattern | :endpoint`, and
receipts persist the attribution under `:memory-use/surfacing-via`.
Critically, the content path does not widen the warrant set — it reuses
the same reviewed-edge projection, and the accompanying negative test
was verified non-vacuous by mutation (disabling the review gate turns it
red, with an `:attachment-status :unreviewed` memory visibly leaking as
a content match). The live regression now passes: the query
`roots outside unit` returns
`close-a92J05-by-transferring-the-unit-disk-zero-count` as a content
match, the memory that the recorded failure above had discarded.

**Falsifiability instruments already in the record**: the preregistered
confirmation-collapse battery (a guard that must first *demonstrate* the
failure it guards against); hit@1 checkpoint with candidate-set
preservation; calibration gating (a promotion that refuses itself below
n = 20 is evidence the gate is real); determinism and full audit of every
ranking.

**The connectivity precondition is measurable, and measured (WS2,
2026-07-28).** The sparse-corpus limit has an exact spectral statement:
the v0 corpus's four disconnected components are four zero modes of
Δ_θ — no choice of conductances can move activation between them. The
live connectivity meter (preregistered verdict criterion, bounded
read-only queries) returned its first reading: `:component-limited` —
83 nodes, 51 pattern attachments, largest reviewed component 6 nodes
with one edge type. λ₂ ≈ 1.0 *within* that component shows small
pattern-stars mix internally while remaining mutually isolated. This
number is now a standing per-cohort meter: memory-corpus growth
("operator food") has a measurable target, not a vibe.

**A preregistered prediction failed, was diagnosed, and the diagnosis
survived its own preregistered test (WS2, 2026-07-28).** The check
"diffusion time ranks inversely with λ₂" scored Spearman ρ = 0.0 at
step size ε = 0.3 — retained as a failure, not repaired post hoc. The
review diagnosis: three of four operator configurations sat past the
explicit-Euler stability boundary ε < 2/λ_max, so those runs measure
concentration (power iteration), not diffusion; the sole stable
configuration showed exactly the slow diffusion λ₂ predicts. The
corrected claim was preregistered and re-tested at ε = 0.1 (all
configurations stable): ρ = −0.8, confirmed. The methodological yield:
**step size is part of the operator** — any deployment of iterated
retrieval dynamics must report ε against the spectral radius or declare
the concentration regime deliberately. This failed-and-recovered chain
is itself evidence the falsifiability instruments bind.

**Cross-model memory transfer, twice, ending at a machine-verified
zero (2026-07-28).** Two sessions by a codex runner on a construction
target, both dispatched with recall over a corpus mined from *zai*
sessions. In both, all three surfaced memories were cited as used with
specific behavioral effects; in the second, the target file reached
zero remaining proof obligations, verified independently at the axiom
level (no `sorryAx` transitively; standard axioms only). The decisive
memory — a process rule recording a previously-hit API absence —
carried a two-branch decision structure ("prove the local inequality,
or declare the dependency frontier"), and the two sessions exercised
*both branches*: the first declared and priced the frontier, the second
proved the local result by the elementary route, making the frontier
unnecessary for this demand. Memory portability across runner models is
demonstrated at the level of decision structure, not just recalled
facts — the difference between recalling a fact and inheriting
judgment — and the episode ended in the strongest available ground
truth.

**The exploration-mass floor works (WS1, 2026-07-27).** On a synthetic
corpus with a planted target and a decoy relation seeded with early
accidental corroboration (preregistered expectations in the battery file
before the run): the floor-off ablation collapses as the theory predicts
— θ concentrates entirely on the decoy relation (`{repairs 1.0,
requires 0.0}`), hiding the planted target — while the identical system
with ε = 0.2 recovers the planted target at step 2. Critically, *both*
control arms (fixed endpoint order and one-step typed ranking) rank the
decoy first on this corpus: the recovery is produced by the iterated
coupled dynamics under the floor, not inherited from any single-step
method. Independently witnessed challenge memories remained reachable at
every step of every run; all runs deterministic, fully audited, k=1
identity with the one-step ranker holding exactly. This is the
floor mechanism demonstrated in both directions — the failure it guards
against, and the guard.

## 6. Honest deltas and limits

- Live Ψ is scalar per-memory, not per-relation-type; θ currently has one
  coefficient per memory. The dark rungs are the growth target and the
  deployed system is their minimal shadow.
- The v0 sweep finding stands as a limit theorem in miniature: below a
  connectivity floor the operator family is degenerate — a disjoint union
  of small stars has trivial flow whatever the coefficients. Corpus
  connectivity ("operator food") is a precondition, and is measured, not
  assumed: the first live reading (2026-07-28) is `:component-limited`
  — the deployed system is currently *below* the floor at which the
  operator family adds value over direct lookup, and the meter tracks
  progress toward it (WS2, §5). **Sharpened 2026-07-30 by structural
  census (§5):** this is not corpus immaturity awaiting more memories.
  All 90 promoted memories attach to exactly one pattern, so under the
  single relation recall traverses the graph is a disjoint union of 10
  stars *by construction*, and is closed under adding memories. The
  connectivity is present in the store already, on the `:subjects` role
  the operator does not read; admitting it as a relation collapses 10
  components to 2. **Amended the same day by a fresh meter reading:** the
  graph as the meter sees it is *not* single-relation — `:distills`
  edges are also present and counted — and as of 2026-07-30 the largest
  reviewed component spans 146 nodes with two edge types, so criteria 1
  and 2 now pass and the binding constraint is criterion 3, λ₂ = 0.0755
  against a 0.1 floor. The corrected statement: **the recall path**
  traverses one relation and sees a star forest, while the graph is held
  together by a provenance relation that carries little topical signal.
  Below-floor now means *diffuse*, not *disconnected*.
- Iterated θ is a search heuristic, not a calibrated posterior; the
  stronger semantics is explicitly refused until a separately validated
  likelihood model exists.
- Retrieval collapses under **conjunctive composition** of query terms, and
  the deployed remedy (the tier ladder, §3.3) is a switch rather than the
  smooth mixing control its braiding analogue provides. The tiers are also
  relaxations of one operator rather than distinct operators alternating, so
  the braiding correspondence is structural, not an instantiation.
- **`:witness-status` is derived from the lane label, not from evidence
  (2026-07-30).** §2 claims nomination and warrant are architecturally
  distinct. In the deployed codex lane the attachment's
  `:witness-status` is computed as
  `(if (#{:solve-lane :arc-lane} lane) :independently-witnessed
  :self-asserted)` — a pure function of which scribe lane drafted the
  memory. It therefore **cannot fail**: no property of the memory, the
  proof, or the receipt can make a solve-lane memory come out
  self-asserted. A status that cannot fail carries no information beyond
  the label it restates. Concretely, of the 59 solve- and arc-lane
  drafts, **28 carry `:independently-witnessed` with no witness record
  in the memory body at all** (solve 11/37, arc 17/22), so the warrant
  claim is not traceable to a declaration from within the record.

  Two things this is *not*. It is not a claim that the attestations are
  false: solve-lane memories distil proofs the compiler checked and
  which ground control re-elaborated `#print axioms` on independently,
  so the underlying witnessing is real. Nor is it a review failure — the
  edge review checks well-formedness and id resolution, and does so
  correctly. The defect is that the *status field* records a lane, while
  the *evidence* for it lives outside the field's derivation. The fix is
  to derive `:witness-status` from the presence and resolution of a
  witness record, so that a solve-lane memory lacking one is reported
  rather than promoted; that has not been done, and until it is, this
  field should not be read as an independent signal in any evaluation.

- *[Increment pending: known failure cases from the live loop — typed
  miss diagnoses (TeX-encoding term loss, description-vocabulary gaps)
  from the s1-pilot meta-drafts.]*

## 7. Related work

*[Increment pending — graph-diffusion retrieval, learned sparse/dense
retrieval with feedback, spreading activation in cognitive architectures
(ACT-R declarative memory), Sturm–Liouville/spectral graph theory
framing, MetaCA lineage. The positioning to draw: feedback-updated
retrieval operators exist; the combination with warrant-disciplined
conduction, witnessed-outcome-only updates, and an explicit
anti-collapse floor is the contribution.]*

---

## Appendix A. Claims-grade specification

*This appendix states the mechanism at the precision a patent filing
would require. It is the defensive-publication payload and what keeps
the US grace-period option cheap; the body above is the document.*

**Independent claim — the combination.** A memory system for autonomous
agents comprising:

- **(a) A typed, bitemporal hypergraph store.** A memory is a dialogue-act
  record: one append-only evidence entry (the body, full-text indexed)
  plus one typed hyperedge whose endpoints link the entry to its
  subjects, the session and mission that produced it, the transcript
  spans it distills, and the *patterns* it instantiates. Act vocabulary =
  IATC performatives (assert/retract/challenge/agree/define/…);
  bitemporal (valid-time/system-time) so belief-as-of-T, correction lag,
  and supersession are queryable objects, not file overwrites.

- **(b) Pattern-conditioned recall.** Retrieval is propagation, not
  lookup: query-derived seed terms → full-text match → typed edge to a
  pattern endpoint → the memories attached to that pattern. The pattern
  is the reusable retrieval handle; the memory stays a concrete episode.

- **(c) A warrant discipline (boundary conditions).** Only independently
  REVIEWED edges conduct retrieval. Lexical bridges (pattern
  descriptions) and semantic similarity (embeddings) may *propose*
  candidates; they never *warrant* them. Editing a pattern's description
  edits a conductance, not a warrant.

- **(d) A receipts observation channel.** Every dispatch writes an
  *offered* half (surfaced ids, query, term sources, recall-system
  version); every independently witnessed outcome writes an *outcome*
  half (used ids from the agent's own citation, result, witness refs).
  Reasoned non-use is a first-class outcome. The channel is
  never-blocking: recall failure cannot block dispatch.

- **(e) A receipt-consuming operator update (Ψ).** Retrieval coefficients
  are reweighted from the receipts channel by a bounded, cold-start-
  neutral, audited update rule — retrieval whose *operator* learns from
  witnessed use, not from self-report.

- **(f) An exploration-mass floor.** The coefficient vector retains a
  minimum mass ε > 0 on every admitted relation type at every step — an
  explicit, reported parameter guarding against endogenous confirmation
  collapse (early accidental corroboration concentrating the operator on
  relations that retrieve more corroboration, hiding the target).

The claim is the *combination*: (a)–(b) alone is a graph memory; (c)
alone is review policy; (d)–(e) without (c) learns from contaminated
signal; (e) without (f) amplifies confirmation. Together they close a
loop in which retrieval behavior is updated only by independently
witnessed outcomes, under warrants, with collapse guarded.

**Dependent embodiments** (each implemented or chartered; status dated):

1. *Scalar per-memory Ψ*: multiplier `(1 + α·used/offered)`, α = 0.5,
   cold-start neutral, audited in the receipt. **Deployed** (S6).
2. *One-outcome per-pattern update*: θ multiplied by the posterior-
   probability ratio of a beta-binomial useful-progress model refit on
   exactly one new independently witnessed transition; promotion gated
   on a calibration minimum (n ≥ 20). **Implemented dark** (Rung 2).
3. *Budgeted facet refinement*: next facet expansion chosen by expected
   information gain per cost, with child eligibility gated on witnessed
   parent-edge warrants. **Implemented dark** (Rung 3).
4. *k-step coupled propagation*: iterate x/θ updates under budget with
   per-step audit (entropies, path diversity, challenge reachability,
   explicit termination class) and the floor of (f); acceptance built on
   a confirmation-collapse battery — floor-off ablation must collapse,
   floor-on must recover a planted target. **Implemented dark, verified
   2026-07-27** (Rung 4 / WS1; battery evidence §5).
5. *Spectral admissibility diagnostics*: algebraic connectivity λ₂ and
   spectral gap of the operator family as the measurable threshold below
   which retrieval dynamics cannot beat direct lookup. **Implemented,
   first live reading taken 2026-07-28** (WS2; evidence §5).
6. *Pattern-level receipt conductances (Ψ-v2)*: fractional credit
   assignment from receipts through reviewed attachments; per-coefficient
   abstention below an observation minimum; promotion gated at n ≥ 20
   per coefficient. **Implemented dark, verified 2026-07-28** (WS3;
   ranking contract validated on a hand-computable synthetic fixture;
   live verdict `:below-calibration-minimum` at n = 1 — see §5).
7. *Semantic proposal lane*: embedding nearness as an additional
   proposal source strictly under (c). **Chartered** (WS4 / S5).
8. *Curriculum forcing*: coverage maps plus memory-proposed construction
   targets select what the system experiences next (the forcing term
   F_t). **Deployed** (cohort-2 curriculum lane).
9. *Bitemporal learning meters*: correction lag, belief-as-of-T,
   time-to-fill as queryable observables of the learning process itself.
   **Substrate deployed; meters partial.**

---

*Increment log:*
- *2026-07-27 (claude-6): opened; claims, frame + both correspondence
  tables, evidence/deltas at current honest state.*
- *2026-07-27 (claude-6): WS1 landed — embodiment 4 → implemented
  dark/verified; collapse-battery evidence paragraph.*
- *2026-07-28 (claude-6): WS2 landed — embodiment 5 → implemented with
  first live reading; spectral-food-problem + meter-baseline +
  failed-prediction-recovered paragraphs; connectivity delta measured.*
- *2026-07-28 (claude-6): restructured to whitepaper form per Joe —
  abstract + §1 problem + §2 design narrative lead; claims enumeration
  moved to Appendix A (defensive-publication payload); embodiment 6
  (Ψ-v2) added with WS3 status.*
- *2026-07-28 (claude-6): WS3 landed — embodiment 6 → implemented
  dark/verified; §5 calibration-gate-binds paragraph with the n=1
  numbers and the outcome-half bottleneck.*
- *2026-07-28 (claude-6): abstract gains the two framing claims per
  Joe — commodity models (the delta is the memory loop) and
  operational autonomy (strategic/operational split, ledger-auditable).
  Announcement pairing recorded in M-codex-sorry-loop §Horizon.*
- *2026-07-28 (claude-6): §5 cross-model-transfer paragraph — two codex
  sessions, 3/3 zai-mined memories used in each, YoungL2 to axiom-clean
  zero; decision-structure transfer (both branches of e-dfea2de9
  exercised).*
