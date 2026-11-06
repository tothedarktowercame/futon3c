# Cascade Live as one argument — draft outline and first cluster theses (2026-11-06)

DRAFT by claude-6 at Joe's direction (2026-11-06): read the whole of Cascade
Live as one written argument, which becomes the draft of the argument for
Joe's Mozilla fellowship application and firms up over the next month. The
cluster theses below are first guesses from the Giraffe readings' `purpose`
lines, for Joe to correct; none is settled.

Sources: Giraffe run `~/code/storage/giraffe/20261106T154554.219Z-1ba3d4c2/`
(`work-clusters.json`, 37 clusters over 105 items), the corpus generation's
readings, `apex-thesis.json`, `intermediate-theses.json`,
`ways-of-working.json`.

## The argument, section by section (what Cascade Live would read as)

1. **The question.** The apex thesis (2026-09-06): can a community of humans
   and machines doing open-ended work leave an account of itself structured
   enough that the work can steer itself, so that activity becomes inference?
2. **The grounds: four therefores and the body.** Wings A (records carry
   warrant), B (one queryable self-account), C (feedback reaches every
   participant), D (accountable next-action choice), and V (stays alive and
   paid).
3. **The practice: ways of working.** The declared ways, which are on, off,
   broken or not built, and how they came to be declared.
4. **What was built first, and why.** The completed missions and packets,
   grouped. The argument here has to say why a year of building came before
   the backlog could be worked: which of the grounds each finished piece made
   possible. *(Not yet drafted: needs a reading of the ~250 closed corpus
   members, grouped the same way as the open ones.)*
5. **The workflow.** How work moves: Uxbridge/WIP, selection by the War
   Machine, cascades, review, closure into packets. *(Section is a
   2026-08-22 snapshot on the page today.)*
6. **The open packets.** Each cluster of open work with its thesis: what it
   proposes to build, which ground it serves, who benefits. First eight below.
7. **What is left undecided.** Items gated on Joe, and the questions that
   decide them.

## First cluster theses (the eight largest Giraffe clusters)

Each: a proposed thesis, the ground it serves, who benefits, and the items.
"Who benefits" uses the placement vocabulary (self, company, grant-math,
product, job).

### P1. The stack keeps its own account current (11 items) — ground B

*Thesis:* a working system can describe itself accurately only if the
description is maintained by the same loop that changes the system; drift
between code, documentation and essay should be detected and repaired by
consent-gated writers, not by periodic human clean-up.
Items: M-self-documenting-stack, M-autonomous-doc-maintenance,
M-stack-essay-code-alignment, M-vsatarcs-writer, M-stack-geometry,
M-hypergraph-operator, M-repl-wins-over-cli, M-zaif-harness,
M-self-improvement-loop, M-a-sorry-enterprise, E-futon1b-operational-switchover.
Benefits: self, product. *Note:* M-jvm-arxana (Joe with codex-21) supplies
the technology this packet would run on. *Open:* M-self-improvement-loop
(Joe's own review loop) and M-a-sorry-enterprise (the company as a
sorry-driven organisation) may be a separate packet about running the
company, and E-futon1b-operational-switchover is infrastructure.

### P2. What was said, and when it was in force (5 items) — ground A

*Thesis:* operator speech, commits and agent replies on one bitemporal
timeline make every rule and decision traceable to the turn that made it, and
let the system induce its own rules from that record.
Items: M-象-2000, M-象-cascade, M-autoclock-in, E-pattern-promotion-and-gc,
E-proof2a-holes.
Benefits: self, product. *Open:* E-proof2a-holes reads as a separate proof
effort linked by name only.

### P3. Questions as the shape of mathematical knowledge (5 items) — ground B

*Thesis:* a query and a scope are the same object seen from two sides, so a
corpus of real questions (MathOverflow) can order which parts of mined
mathematics get made explicit, and distributed formalisation can proceed
hole by hole.
Items: E-queries-and-scopes, E-question-driven-scope-coverage,
E-mo-query-phylogeny, E-iatc-model, E-distributed-formalisation.
Benefits: grant-math.

### P4. Patterns that are earned, not assumed (5 items) — ground D

*Thesis:* a pattern library is worth using only if applications of patterns
are recorded as typed evidence and the library learns from them; the old
cosine-retrieval substrate is retired because its signal was an artefact.
Items: M-pattern-mining, M-pattern-application-diagnostic,
M-a4a-structure-node, M-learning-loop, M-trip-journal.
Benefits: self, product.

### P5. From solved proofs to transferable capability (4 items) — ground D

*Thesis:* solved problems are worth something beyond themselves only if what
was learned transfers to sealed new problems, measured, with capability
packets promoted or rejected on evidence.
Items: M-apm-capability-ratchet, M-apm-solutions, M-diagramprover,
M-futonzero-prelim-practice.
Benefits: grant-math, job.

### P6. Memory claims settled by experiment (4 items) — ground A

*Thesis:* claims about what an agent memory system does (attachment,
receipt bias, reachability, granularity) are settled by preregistered
experiments, and the white paper reports what failed as well as what held.
Items: E-memory-whitepaper-v2-programme, E-memory-whitepaper-v3-programme,
E-memory-v3-programme, E-memory-v3-staging.
Benefits: product, grant-math.

### P7. The operator's observations as evidence (3 items) — ground C

*Thesis:* what the operator says about the system is evidence the machine
should update on, once each observation is typed, dated and confirmed.
Items: E-wm-operator-observations, M-evidence-landscape-index,
M-capability-levels.
Benefits: self.

### P8. A differentiable view of the stack (3 items) — ground D

*Thesis:* if the stack's structure is held as a metric substrate, gradients
can propose structural edits and rank next moves, complementing discrete
search.
Items: M-differentiable-substrate, M-differentiable-code,
M-mission-scopes-into-substrate-2.
Benefits: self, grant-math.

## Section 4 material: the Futon City districts (built beside open)

Joe (2026-11-06): the "Mission clusters" section of Cascade Live comes from
clustering the Futon City graph, and behind its clustered patterns are
missions completed or features built; a variant went into
`~/code/powerbi-tui/skills-one-pager.md` (Figure 4, "~9,800 work-units across
~200 mission-districts"). Below: the 12 districts served by
`/api/alpha/cascade-real/graph` (`clusters`, 117 missions), each mission's
status taken from the first Status line of its `M-*.md` (a regex, so a few may
be misread; "no file" ones omitted).

| district | built (closed) | still open |
|---|---|---|
| 00 futon1a / daily | futon1a-evidence, futon1a-rebuild, futon1a-workplan, forum-refactor, transport-adapters, f7-lead-report | pattern-ingest, essay-corpus-substrate, daily-scan, demonstration-foundry |
| 01 agency unified | agency-unified-routing, invariant-queue-unstuck, portfolio-inference, reachable-from-boot, single-locus, structural-law, evidence-viewer-refinements, self-representing-stack | evaluate-policies, zaif-harness, bounded-disposition, cyder, state-snapshot-witness, vsatarcs-invariants-integration, trip-journal |
| 02 typed holes | operational-readiness, pilot-appearance, typed-holes, typed-holes-example-first-flights, explore-aiqa | none |
| 03 substrate live | live-geometric-stack, bounded-in-flight-state | chipwitz-corps, mission-scopes-into-substrate-2, populate-substrate-2, the-perfect-crime, web-arxana-ui-improvements, war-machine-wiring, live-efe-map, metric-harness |
| 04 stack code | futonzero-capability, aif-head, futon3x-e2e, memes-arrows-patterns-diagrams, three-column-stack | patterns-done-right, the-futon-stack, text-sidecar, stack-inhabitation, categorical-code, differentiable-code, a-sorry-enterprise, self-improvement-loop, stack-geometry, artificial-stack-exchange |
| 05 custom harness | futon1b-port, points-de-fuite, wm-policies-documented-run-2026-06-24, mission-peripheral | futonzero-mvp, custom-harness, single-entry-point, smart-emacs-cursor, vsatarcs-writer |
| 06 pattern learning | capability-star-map, writing-ethics | a4a-structure-node, mission-coherence-patterns, pattern-application-diagnostic, pattern-mining, pattern-retrieval-calibration, pattern-posteriors, or-training-as-learning-system, peeragogy-rewrite, simulating-or-training-as-learning-system, learning-loop, superpod-mark3 |
| 07 interest expressions | f6-arxiv, f6-eval, peripheral-phenomenology, interest-network-coupling | fold-ansatz, weird-modernism, futon-enrichment, expressions-of-interest, hypergraph-operator |
| 08 rational reconstruction | stepper-calibration | diagramprover, stack-stereolithography |
| 09 war machine | aif2, wm-policies | goals-and-holes, operational-vocabulary, action-cost-modelling, war-machine, war-machine-pilot, war-machine-tuning, recommendation-bindings |
| 10 codex / IRC | codex-agent-behaviour, codex-irc-execution, improve-irc, typed-bells | none |
| 11 coordination rewrite | coordination-rewrite, f6-agents, f6-ingest, f6-recursive, peripheral-gauntlet | none |

Reading: three districts are finished (typed holes, codex/IRC, coordination
rewrite): the coordination layer was built first. Districts 03, 04, 06 and 09
are mostly open: substrate, stack self-description, pattern learning and the
War Machine are where the open packets live. Each open packet (P1–P8) should
name the district it continues and the built missions it stands on; e.g. P4
"patterns that are earned" continues district 06, standing on
capability-star-map; P1 spreads across 01, 04, 05 and 07, which is a sign it
is more than one packet.

## Next

- Joe corrects P1–P8 (one at a time, read aloud if wanted).
- The remaining 29 clusters, then the ~470 open items with no recorded
  links, grouped by repo and wing.
- Section 4 (what was built first) from the closed members.
- Once settled, these theses become the packet layer on Cascade Live,
  between the wings and the item rows.
