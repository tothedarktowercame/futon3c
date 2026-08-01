# E-causal-coupling-top-down — hooking Rob's Pearl scaffold to the receipt-driven retrieval operator

**Status: DERIVE (exploratory).** Opened 2026-07-30 by claude-1 at Joe's
handoff. No measurement here; §8 is a falsifiable prediction, not a result.
Nothing in this note has been agreed with Rob — it is the futon-side reading
of a possibility he raised.

**The prompt (Joe, 2026-07-30):** Rob has built a **top-down** memory
structure using Pearl's *The Book of Why* to scaffold improved causal
reasoning for agents. Joe's system is **bottom-up** (Zai runners, mined
memories, witnessed receipts). *"The thought would be that we'd be able to
hook up his top-down reasoning system and the bottom-up one I've been
developing."* Joe pointed at `TN-coupling-gain.md` as the place to think from.

**Reads (read-only):** `futon3c/docs/retrieval-whitepaper.md` §5–6 + App. A ·
`futon3c/holes/excursions/E-dynamic-queries.md` §Rung 4 ·
`futon5/holes/tech-notes/TN-coupling-gain.md` §§1–4 · `TN-exotype-placement.md`
(protocol) · `TN-baldwin-reconsidered.md` §1 (the tower) ·
`p4ng/main-2026.tex` §Structure learning (BMR, R17) ·
`futon3/holes/war-room.md` WR-24 + revision.

---

## 1. Why this is not "two nice systems could combine"

The bottom-up system has a **measured, named, currently-binding structural
deficit**, and it is exactly the thing a top-down causal graph produces.

Whitepaper §6, second bullet, verbatim in substance: *below a connectivity
floor the operator family is degenerate — a disjoint union of small stars has
trivial flow whatever the coefficients.* Corpus connectivity ("operator food")
is a precondition, it is **measured not assumed**, and the first live reading
(2026-07-28) is `:component-limited` — **the deployed system is currently
below the floor at which the operator family adds value over direct lookup.**

That is the whole argument in one line. The retrieval operator is starved of
edges. Its coefficients cannot rescue it: the limit is structural, and the
whitepaper states it as a limit theorem in miniature. The current plan for
feeding it is organic and slow — the scribe rule lifting cross-memory
references (updates / resolves / contrasts) from prose into typed edges, a
cohort at a time, with three body-level refs and *zero* `:memory/retract`
hyperedges as of 2026-07-28.

**A Pearl-scaffolded top-down structure is a supply of typed, warranted edges
over the domain.** A DAG *is* a typed relation set with a semantics for what
each edge licenses. So the complementarity is not aesthetic:

| | supplies | lacks |
|---|---|---|
| **Rob's top-down scaffold** | structure — what may cause what, and where | witnessed evidence that any particular edge carried weight |
| **Joe's bottom-up loop** | receipts — independently witnessed outcomes, typed, dated | **connectivity** (measured `:component-limited`) |

## 2. Pearl's ladder is already the measurement protocol in Part III

This is why Joe was right to point at `TN-coupling-gain.md`, and it is the
part most likely to interest Rob directly.

The reach protocol (`TN-exotype-placement.md` §Protocol, shared with
`regime_placement.clj`) is: evolve to `t*=60`; **flip one phenotype bit**;
**continue both branches with cloned RNG state**; measure differing phenotype
cells at `dt=59`. Same unit, same exogenous noise, one intervened variable,
two branches compared. That is not a correlation and not merely an
intervention — it is a **twin-network structural counterfactual**, Pearl's
rung 3, implemented literally.

And §4's decisive control is a **path-specific / edge-deletion** comparison,
not a parameter sweep: conservative transport gated on the local phenotype
interface, versus the same construction with the gate replaced by a *constant
probability equal to the rate* — mean swaps unchanged, dynamics still
bijective, rule histogram invariant. The two differ **only in whether the
genotype can see the phenotype**. That is severing one edge while holding the
marginal fixed: do-calculus, in a substrate where the structural equations are
known exactly by construction.

**So the two projects already share an instrument.** Rob's scaffold reasons
over a causal graph; Part III *measures causal reach counterfactually* in a
system whose true SCM is known. That is a rarer asset than either the graph
or the receipts.

## 3. The tower says where each piece goes — and both are missing the same layer

Using `TN-baldwin-reconsidered.md` §1's tower:

| layer | who supplies it |
|---|---|
| **Xenotype** — the global physics; what may act where and when | **Rob's DAG.** A causal graph over a domain is precisely a statement of which mechanisms exist and where they may apply. |
| **Phenotype / genotype** — what actually happened, and the state that produced it | **Joe's receipts + the memory hypergraph.** Dated, typed, independently witnessed. |
| **Exotype** — the *local evaluation regime*: at this decision point, which part of the structure to consult, and how much of the outcome to write back | **NEITHER.** This is the joint build. |

The exotype is the gain. `TN-coupling-gain.md` §1: across eight coordinates
the family offers, reach is silent; the coordinate that governs is **the gain
of the loop from phenotype back to genotype**, graded and monotone.

Note this is the *same missing-middle diagnosis* as
`futon7/holes/E-business-exotype-audit.md` reaches for businesses, from an
unrelated direction. Two independent derivations of "the exotype layer is what
nobody has built" is worth more than either.

## 4. The identification problem — and the exploration floor already solves it

A Pearl-literate reader will ask this immediately, so it should be answered
before it is asked.

Receipts look like a causal goldmine because **a dispatch is an
intervention**: the system did `do(recall = m)` and an independently witnessed
compiler verdict followed. But assignment is **not random** — the S6 ranker
chose which memory to offer, and the ranker is the very thing under
evaluation. Treatment assignment is confounded by the mechanism being
measured. Naively regressing outcomes on offered-memories would recover the
ranker's own preferences, dressed as causal effect. This is the
self-report-contamination failure mode the whitepaper §1 already names, in its
causal-inference clothing.

**The exploration-mass floor is what rescues it, and this is a second job the
floor was not designed for.** It was introduced as an anti-confirmation-collapse
guard: the coefficient vector retains a stated exploration mass so the update
loop cannot collapse onto its own early successes (WS1, verified 2026-07-27 in
both directions — floor-off ablation collapses as predicted, floor-on recovers
a planted target). But a guaranteed floor on non-greedy assignment is exactly
a **propensity lower bound**: it makes assignment probabilistic with known
minimum support, which is the precondition for identification off the receipt
corpus.

**Consequence worth writing into the whitepaper:** the floor's *rate* is not
only a stability parameter, it is an **identifiability** parameter. Whatever
mass the floor holds is the budget for causal estimation. That reframing costs
nothing, is true of the mechanism as already built, and is the kind of thing
that makes the document legible to someone coming from Pearl.

## 5. Direction matters more than connection — the frozen-read hazard

The most likely way this hookup fails is by working in one direction only.

A top-down authored DAG is, by construction, a **snapshot**. Part III's
frozen-phenotype gate (`mmca-clj` `cdc1cd3`) reads a phenotype frozen at `t*`
and does **not exceed the rate-matched blind gate at all** — assimilation
failing by construction. Transferred: retrieval consulting a *fixed* causal
graph is a frozen read. It may be an elaborate, well-warranted, expensive
frozen read, and Part III's answer is that it scores like not looking.

So the acceptance condition is bidirectionality: **the receipts must be able
to revise the graph.** And the revision operator already exists on the futon
side — Bayesian Model Reduction, R17, `futon2/src/futon2/aif/bmr.clj`, in the
paper as `A' = A + a' − a` with `ΔF = ln B(A) + ln B(a') − ln B(a) − ln B(A')`
deciding whether the reduction is adopted. Which gives a clean division:

- **Rob's DAG → the prior structural model** (`a`, and which edges exist at all);
- **receipts → the counts** already accumulated;
- **BMR → the test** of whether a simpler structure explains them better,
  reusing evidence with no new data collection.

That is a complete loop from parts that exist on both sides. It is also the
honest home for the whitepaper's refusal (§6: *iterated θ is a search
heuristic, not a calibrated posterior; the stronger semantics is explicitly
refused until a separately validated likelihood model exists*) — a structural
prior with a reduction test is a candidate for that missing likelihood model.
**Candidate. Not a claim that it is one.**

## 6. What the futon side can offer Rob that he probably cannot get elsewhere

A **known-SCM calibration testbed.** In mmca the structural equations are
given exactly, the reach measure is counterfactual (§2), and the scale is
anchored against elementary rules — 0 → 0.00, 204 → 1.00, 90 → 8.00,
30 → 36.45. So a causal-reasoning scaffold can be *scored* there before it is
trusted anywhere consequential: does it recover the coupling as the governing
coordinate when eight decoys are silent? does it correctly rank the
mobility-matched blind control as no-effect?

This is a necessary and **not sufficient** step toward business data — it
supplies anchors for *causal-reach instruments*, not for organisations (the
unsolved anchor problem of `E-business-exotype-audit` §6 stands). Say it that
way; the value is real and overclaiming it would be the fastest way to lose a
technically careful collaborator.

## 7. Why the channel is credible

`E-business-exotype-audit` §Provenance records the observable: Rob raised the
business-modelling framing *unprompted* in a 2026-07-29 standup with Joe and
Charlie. This note's prompt is a second inbound idea from the same channel
within two days. In the vocabulary of that excursion §7, that is **transport**
— exogenous writes into the genotype — and the measured caveat applies:
*ungated* transport stays ordered; only transport gated on a real interface
clears. Both of these arrived gated on real interfaces (a measured
connectivity deficit; a shared measurement protocol), which is why they are
worth acting on and a constant stream of collegial ideas would not be.

And the whitepaper's stated audience is already *"first: Rob"* with the send
gate at Joe. There is now a substantive reason to send it rather than a
courteous one.

## 7b. STATUS CHANGE — Rob is operationalising it now (Standup #2, 2026-07-30)

**This note was written as if the Pearl scaffold were a thing Rob had built and
might share. Joe reports from Standup #2 that Rob is actively "getting the Book
of Why operationalised" — it is in progress, not in the past.** That changes the
timing of everything below.

- **The offer is worth more now than in a month.** §§4–6 propose receipts as
  interventional data, BMR as the reverse-direction revision operator, and mmca
  as a known-SCM calibration testbed. All three are far cheaper to accept while
  a design is still fluid than after it has hardened. The ontology join (§9 Q2)
  is likewise cheapest now.
- **So `M-becoming-nomad` §4's Week-1 action — send the whitepaper to Rob —
  moves from justified to time-sensitive.** It was already scheduled for the
  week of Aug 3–7; this is the reason not to let it slip.
- **And §9 Q3 ("evaluated, fed, or coupled?") is now answerable by asking
  rather than guessing**, because there is live work to ask about.

**Second item from the same standup, recorded because it belongs to this note's
subject matter:** Rob also has **LeanDojo work sitting locally, unshipped**.
Unshipped work has no external verdict — the same condition WR-24's revision
names, arrived at from the other side of the collaboration. Two observations,
offered without pressure:

1. It sits in the layer `E-ashby-variety-stratum` §7 argues is the highest-value
   constraint supply (mathematics), and adjacent to Joe's own live APM Lean lane
   (claude-9) and the proof-checker product line.
2. There is a **precedented symmetric exchange** here. `mark7-rob-handoff.md`
   had Rob supply the cluster Joe lacked. Shipping discipline is the thing Joe
   has a lot of and Rob's local LeanDojo work is currently missing. That is the
   same trade running the other way, and it is Rob's to want or not.

## 8. The falsifiable prediction, and the bar

**Prediction.** Importing Rob's structure as typed edges over the memory
hypergraph moves the **connectivity meter** off `:component-limited` and above
the floor at which the operator family beats direct lookup. If the meter does
not move, the hookup is decorative and should be said to be.

Bar for a first slice, in the existing dark-first discipline
(`M-memory-retrieval` Interfaces 1–3: frozen corpora, `:live-ordering-changed?
false`, no live promotion without a cohort boundary and Joe):

1. **Import, dark.** Rob's structure expressed as typed relations in the
   existing vocabulary; connectivity meter re-read before and after. One
   number, two readings.
2. **Ablation.** Ranking on frozen receipts with and without the imported
   edges. Structure that does not change ranking on held-out receipts has not
   earned live promotion.
3. **Reverse direction.** At least one BMR reduction proposed *against* Rob's
   graph from accumulated counts, with its `ΔF`. If the receipts can never
   revise the graph, §5 says we have built a frozen read.

**Gated on, and not to be finessed:** the Phase-6 calibration minimum
(n ≥ 20 independently witnessed outcomes; frozen sample was 13). Steps 1–2 are
connectivity and ranking work and do not need it; any *θ-learning* over the
imported relation types does. Cohort exhaust raises n; this excursion does not
manufacture outcomes.

## 9. Open — what would need to come from Rob's side

1. **Form of the structure.** A DAG over domain variables, a typed relation
   schema, or prose scaffolding a model *reasons with*? Only the first two
   import as edges; the third is a different and harder integration.
2. **Whose ontology.** Do his nodes and our patterns/memories name the same
   kinds of thing? The stack's own hardest lesson here is that **identity was
   the blocker, not data** (bulletin 11: 171 shared mission nodes, 0 conflicts,
   only after a canonical-id write gate). Expect the join to be the work.

   **LARGELY ANSWERED 2026-07-30 (Joe).** This question was posed as if Rob were
   a new collaborator. He is not: `futon6/holes/handoffs/` records a
   correspondence from at least March to July 2026, and
   `futon6/holes/mark7-rob-handoff.md` hands him the repo, the run manifests and
   `data/mark7-substrate.tgz` — the concept substrate **plus the futon3 pattern
   library** — to run Superpod windows on his own cluster. Joe: *"he uses
   flexiargs and missions **which he got from me**."* So the shared vocabulary is
   shared *because it was transmitted*, and Rob already holds the artifact. The
   join is much further along than this note assumed; what remains is aligning
   his Pearl-scaffold nodes to that vocabulary, not establishing one.
3. **Direction he wants.** Does he want his scaffold *evaluated* (§6 offers a
   testbed), *fed* (§8 offers receipts as interventional data), or *coupled*
   (both)? These are three different projects and only the third is the one
   Joe described.
4. **Publication and IP shape**, before rather than after — the whitepaper is
   an explicit defensive publication with a disclosure clock running from
   2026-07-22, and this would be joint work touching it.

## 10. What must not be claimed

- That the CA result is evidence *for* the memory system. Different substrate;
  it is a **shared risk with independent measurement**, per bulletin 14
  Finding 3. Coherence, not support.
- That selection will find the coupling. `TN-part-III-b-baldwin-recovery.md`
  is a PLAN with criteria fixed in advance and **nothing measured**.
- That the deployed system currently benefits from its operator. It is
  measured `:component-limited` — *below* the floor where the operator beats
  direct lookup. That admission is the reason this excursion exists, and
  hiding it would remove the argument.
