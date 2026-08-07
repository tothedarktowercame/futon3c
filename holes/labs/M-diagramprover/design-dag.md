# The design DAG, and what may be retracted from it

**Problem this fixes.** Figure 1 of the capability proof was drawn from the
*implementation* — the twelve stages that happened to be wired — and was then
used to adjudicate what the *design* requires. That is circular, and it produced
two bad calls in one session: recommending that CAS-SEL be dropped because "the
model doesn't mention it", and treating a documented expectation (the
deterministic path yields all-thin; real matches need the Tier-1 verify) as
evidence against the feature.

The design is older and larger than the implementation. It lives in the
readiness cards — 38 of them in five layers — and the implemented pipeline is a
*subgraph*. Deciding what can be dropped is a question about the design graph,
not the implementation graph, and it is the kind of question M-diagramprover
exists to answer: **which nodes can be retracted while the terminal claim
remains identified?**

## The five layers, and what the implementation collapsed

| layer | cards | what it is | how Figure 1 rendered it |
|---|---|---|---|
| **A** structure-first concepts | SFC1, SFC-D3, SFC-AGG, SFC-NORM, WARP-ORCH | do we know the terms? concept spine, index, aggregation, normalization, warp/tapestry wiring | one node: **S2 substrate** |
| **B** definition structure | SFC2a, SFC2a-v2, SFC2b | the `:structure` lift: LaTeXML→Clojure transduce, widened, plus LLM symbol grounding | folded inside **S11**; SFC2b unwired |
| **C** the rung ladder | rung-0, rung-1, R2a, R2a-v2, R2b, R2c, R2d, R2-harness, R2-wire | successive verification **depth**: wiring well-formedness → anti-degeneracy → anchor faithfulness → closure → warrant resolution → concept coverage | two edges: **S3's gate** and part of **S5** |
| **D** the cascade | CAS-0, CAS-SEL, CAS-Q1, CAS-Q2, CAS-CERT, rung-3 | per-proof check **selection**: match the menu to *this* proof's sorry-topology instead of a uniform {R2a,R2b,R2c} | rung-3 only; the rest absent |
| **E** cross-cutting | GOLIVE, RAW-CTL, RENDER, COMPREHENSION, STRAT-REC, WARRANT-NORM, PASS3-HARVEST, LEAN-NL | go-live substrate, control arm, rendering, comprehension floor, strategy recognition, hole normalization, pass-3 harvest, Lean↔NL | S3, S5, S9; RENDER and LEAN-NL absent |

Two structures were lost in the collapse, and both are load-bearing:

**The ladder is a ladder.** rung-0 → rung-1 → rung-2 (R2a–R2d) → rung-3 is
increasing verification depth, each rung presupposing the last. Figure 1 shows a
single "gated" edge, which cannot express that rung-2 passes on only half the
corpus while rung-0 and rung-1 pass on all of it. A diagram in which those are
one edge cannot represent partial verification — and partial verification is the
actual state.

**The cascade is a selector, not a stage.** CAS-SEL's purpose is that each proof
gets *its own* check menu, derived from its sorry-topology. In the
implementation this is invisible because every proof gets the same checks. The
design's claim — that Pólya, the RM question-pattern survey and the expository
taxonomy converge name-for-name on one menu — is a claim about the *content* of
that selector, and no edge in Figure 1 can carry it.

## What retraction should mean

Not "does this node do anything today". A node that produces nothing today may
be unbuilt, unwired, or awaiting a verify pass; none of those is evidence about
whether the design needs it. The causal question is:

> Remove node *X*. Is the terminal claim — *these questions are answerable* —
> still **identified** from the remaining graph?

Three cases follow, and they call for different actions:

1. **Retractable.** Every path from *X* to the terminal is duplicated by another
   node. Then *X* is redundant machinery and should be deprecated outright, as
   the APM structure-match tail was — it consumed inputs from another programme
   and had no path to any question this corpus can answer.
2. **Load-bearing.** Some answerable question depends on *X* alone. Then *X*
   stays regardless of whether it currently runs, and the honest status is
   *unbuilt* or *unwired*, not *optional*. CAS-SEL-5's genealogical select is
   the clearest instance: a paper inheriting its citations' patterns is the only
   cross-paper inheritance mechanism in the design, so no other node can supply
   what it supplies.
3. **Undecidable from here.** *X*'s contribution is only visible at a scale we
   have not reached. The preregistration's Class C. These cannot be retracted on
   pilot evidence — the pilot is exactly the regime where they look useless.

CAS-SEL sits in (2) and partly (3): per-proof selection is testable now, the
genealogical half is not.

## What this requires next

- **Draw the design DAG as a graph**, not a table — nodes = cards, edges =
  the consumes/produces relations the cards state, terminal = the question
  classes in the capability proof's §"What can be asked of it". The
  implementation subgraph highlighted inside it.
- **Mark each node's status honestly**: witnessed / built-unwired / unbuilt /
  failing. The current dashboard's `ready | partial | build` conflates
  built-unwired with unbuilt, which is what let CAS-SEL read as "needs wiring"
  when most of it was built and reviewed and what it needed was a verify pass.
- **Then, and only then, run the retraction argument** per node. That is a
  d-separation question over the design graph, which is what the causal engine
  does; doing it by eye is how the last two calls went wrong.

Until that exists, the honest position is that **no card should be retracted on
the strength of the implementation diagram**, because the implementation diagram
is a description of what got built first, not of what the work requires.

## Which conclusions the graph change actually moves

A graph that changes no conclusions is decoration, so this is the test. Running
the same questions against both graphs:

| conclusion drawn from the implementation graph | under the design graph | moved? |
|---|---|---|
| **A3: "98/98 graphs gated PASS"** — verification is complete | The ladder has four rungs. Measured on the corpus: rung-0 (argcheck) **98/98**, rung-1 (substance) **98/98**, rung-2 (semcheck) **49/98**, rung-3 deterministic half only. So the true claim is *"passes rungs 0–1 of 4 corpus-wide, rung 2 on half"*. The count was never wrong; "gated" meant something narrower than a reader would take it to mean | **yes — weakens** |
| **CAS-SEL is not needed** | Load-bearing: the genealogical select is the only cross-paper inheritance mechanism in the design, so no other node supplies what it supplies | **yes — reverses** |
| **The APM structure-match tail is deprecated** | Also retractable here: it consumes another programme's inputs and has no path to any question this corpus can answer | no — **both graphs agree**, which is the evidence that call was right |
| **A12: "12/12 stages ledgered"** | 12 stages is the implementation; the design has 38 cards. The integration claim is over a subgraph and should say which | **yes — weakens** |
| **The two open gaps were accounting, not capability** | A third category appears that the implementation graph cannot express: *built, reviewed, and never run* (CAS-SEL's Tier-1 verify). Neither an accounting gap nor a missing capability — an unexecuted experiment | **yes — adds a category** |
| Entropy gate 0.02 · 5 archetypes · curve rising · 58% missing-warrant | Unchanged. These are measurements over artifacts | no |

The pattern in that column is the interesting part: **every integration
conclusion moves, and no census conclusion does.** That is the same asymmetry
the independent review found — the mechanically-checked counts held, and the
prose about how the pieces fit was where the errors were. A graph is exactly a
claim about how the pieces fit, so a wrong graph corrupts precisely the class of
conclusion that had already proved least reliable.

Which gives the operational rule: **census claims can be made from artifacts,
but sufficiency claims must be made from the design graph**, and the
implementation graph is evidence about neither — it records what was built
first.

## Rung-2 measured, and a correction

The first pass at the table above recorded rung-2 as failing corpus-wide. That
was an artifact of the measuring apparatus, not a property of the corpus:
`iatc_semcheck.bb` invoked bare `python3` rather than the repo venv's
interpreter, so R2d's `edn_format` import raised `ModuleNotFoundError`, the
composer caught a nonzero exit and declared the whole graph failed. Every graph
failed for the same reason, which is the signature of an apparatus fault rather
than a finding — a corpus does not fail uniformly. Same class as the LaTeXML
gap: the dependency was installed, but not on the path the caller searched.

With the interpreter corrected, rung-2 over all 98 graphs:

| sub-check | PASS | FAIL | what it tests |
|---|---:|---:|---|
| **R2a** anchor-faithfulness | 84 | 14 | do node texts match their cited source lines |
| **R2b** closure | 58 | 40 | is every node reachable in the inference structure |
| **R2c** warrant-resolution | 98 | 0 | do resolved warrants point at real support |
| **R2d** concept-coverage | 92 | 0 | are the concepts used actually defined |

**49/98 graphs pass rung-2 overall.** Three things in that table matter more
than the headline:

- **R2b is the binding constraint** — 40 of the 49 failures are orphan nodes:
  extracted claims that never join the inference structure. That is a substantive
  finding about extraction, not a defect. It is also exactly what the 58%
  missing-warrant census rate looks like from the other side.
- **R2c cannot fail.** Its threshold is `:warrant-floor 0.0`, so its 98/98 is a
  configuration, not a finding — the code says it is report-only "until a
  stricter floor is calibrated", and 31 of the passes are at `rate=0.000`. A
  non-gating rung printed in the same PASS column as three gating ones makes any
  aggregate over that column a mixture.
- **R2d returns `NA` on 6 graphs**, which the schema declares as `:na-not-fail`.
  That is deliberate and correctly reported — worth stating only because the
  denominator for R2d is 92, not 98.

So A3 weakens, as the design graph predicted — but less than the first
measurement suggested, and for a reason the first measurement had backwards.
The lesson generalises: a uniform failure across a heterogeneous corpus is
evidence about the harness before it is evidence about the corpus.
