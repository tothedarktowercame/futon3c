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
  progress toward it (WS2, §5).
- Iterated θ is a search heuristic, not a calibrated posterior; the
  stronger semantics is explicitly refused until a separately validated
  likelihood model exists.
- Retrieval collapses under **conjunctive composition** of query terms, and
  the deployed remedy (the tier ladder, §3.3) is a switch rather than the
  smooth mixing control its braiding analogue provides. The tiers are also
  relaxations of one operator rather than distinct operators alternating, so
  the braiding correspondence is structural, not an instantiation.
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
