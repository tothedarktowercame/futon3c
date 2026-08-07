# Deep-research task spec: literature + reference implementations for the causal diagram engine

**Status: SPECIFIED, not yet run.** Written 2026-08-02 to be executed
later by a standalone CLI session with web access. This file is the
complete prompt/contract; the runner needs no other context.

## Context (self-contained)

We are building a generic string-diagram rewriting engine in Clojure
(kernel = a port of chyp: typed open hypergraphs, convex matching,
double-pushout rewriting, rule application as a relation), then layering
a causal-inference rule set on it: Markov-category structure,
interventions as diagram surgery (Jacobs–Kissinger–Zanasi), d-separation
(Fritz–Klingler), do-calculus/identification checked against dagitty and
y0 as oracles. Applications: causal DAG specs of (a) an agent memory
system, (b) a Lean proof pipeline, (c) a collaborator's Lean→Python
transpiler whose causal model is being iteratively constructed from
repair failures. Already cloned for mining:
`/home/joe/code/diagramprover-refs/` — chyp, pyzx, dagitty, y0,
causaleffect.

## Objective

A ranked, annotated census of (1) literature and (2) runnable reference
implementations we should mine, per research question below — with a
"what to take from it" note for each item, and an explicit list of
things that would change our plan (a better kernel to port, an existing
system that already does WS-B, a theorem that blocks or shortcuts a
workstream).

## Research questions

**RQ1 — Diagram rewriting engines beyond chyp.** What other maintained
implementations of string-diagram / monoidal-category rewriting exist?
Check at minimum: DisCoPy, Catlab.jl + AlgebraicRewriting.jl (ACSets,
DPO), homotopy.io, quantomatic lineage, sd/hypergraph rewriting tools
from the Statebox/ZX communities. For each: representation (hypergraph?
combinatorial? term), rewriting modulo which axioms, license, activity,
and whether porting or FFI beats our chyp port.

**RQ2 — Categorical causal inference.** Beyond Fong (causal theories),
Fritz (Markov categories), Fritz–Klingler (d-separation), and JKZ
(surgery): what is the current state? Specifically find Lorenz–Tull
"Causal models in string diagrams" and successors; network vs circuit
diagram treatments; categorical treatments of counterfactuals,
identifiability, and transportability. Which results have ANY
implementation? Which are theorem-only?

**RQ3 — Do-calculus / identification engines.** Census of
implementations: dagitty, y0, causaleffect, pgmpy, Ananke, DoWhy,
anything newer. Which implement full ID/IDC/gID, transportability,
missing-data identification? Which expose testable-implications /
falsification APIs (y0 has falsification.py — who else)? Best oracle
set for differential testing.

**RQ4 — Causal models of programs / deterministic systems.** For the
transpiler use case: actual causation (Halpern–Pearl) implementations;
delta debugging and causal fault localization (Zeller lineage); program
slicing as causal-cone extraction; provenance graphs; any work on
*constructing* causal models of software from failures/repairs
(model-based debugging, spectrum-based fault localization with causal
semantics). What is the strongest prior art for "iterate a causal model
of a compiler from failed fixes"?

**RQ5 — Proofs as diagrams.** Prior art on representing proofs /
tactic traces as string diagrams or typed dataflow: deep-inference
proof nets, Lean proof-term visualization (Paperproof etc.), category
of goal-states treatments, proof mining of tactic corpora. Anything
that already extracts composable typed diagrams from Lean proofs.

**RQ6 — Causal discovery under full observability.** Where the process
is fully visible and deterministic, what does the discovery literature
offer (vs the usual hidden-variable statistical setting)? Structure
learning from execution traces; conditional-independence testing on
deterministic data (known pathologies); anything on model elicitation
loops with a human/agent in the loop.

## Deliverable

Write results into this directory as:

- `deep-research-census.md` — per RQ: ranked items, each with
  {what it is, why it matters to us, what to mine (specific files/
  theorems/APIs), effort estimate (read / port / FFI / oracle-only),
  license}. Lead each RQ section with a 3-sentence verdict.
- `deep-research-deltas.md` — ONLY the findings that should change the
  programme of work in
  `futon3c/holes/missions/M-diagramprover.md` §Programme of Work,
  stated as proposed deltas (per the mission's revision contract).
  Empty file with a "no deltas" line is a legitimate outcome.

Rules: distinguish read vs skimmed vs abstract-only per source; give
URLs/DOIs; no aggregate enthusiasm — an RQ with weak prior art should
say so plainly.

## When to run

After WS-A (engine core) lands and is reviewed, so the census can be
read against a concrete kernel rather than an intention. Not before —
its deltas file targets a real diff surface.
