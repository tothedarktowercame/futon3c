# Memory retrieval white-paper plan

**Status:** proposed finishing strategy  
**Target:** `futon3c/docs/retrieval-whitepaper.md`  
**Opened:** 2026-07-30  

## Purpose

The current retrieval white paper is a valuable evidence ledger, but it is not
yet shaped like a finished white paper. It currently serves three purposes at
once:

1. a report on the deployed retrieval system;
2. a research programme for an evolving graph operator;
3. a defensive-publication and claims record.

The chronology of experiments, corrections, retractions, implementation
increments, and open claims is useful and should be preserved. It should not,
however, determine the structure of the finished paper.

The proposed strategy is therefore:

> Preserve the present document as an evidence ledger, freeze a small set of
> claims and the evidence required for them, and write the finished white
> paper from that claim/evidence matrix.

## Recommended paper identity

The most achievable credible paper is a systems and evaluation white paper:

> A typed, warrant-disciplined hybrid memory system can close an auditable
> retrieval--outcome loop. We evaluate which retrieval paths improve recall
> under a fixed budget, and distinguish deployed mechanisms from experimental
> extensions.

The evolving-operator theory remains important, but until its marginal effect
has been measured it should be presented as a formalisation and research
programme rather than as an established end-to-end result.

This gives two distinct completion thresholds.

### Threshold A: credible systems paper

The paper evaluates the deployed hybrid retrieval system, its structural
retrieval paths, and its warrants and receipts. Receipt-updated typed
conductances may appear as future work or as a dark experimental extension.

This threshold is attainable without waiting for every operator coefficient
to accumulate enough observations.

### Threshold B: full evolving-operator paper

In addition to Threshold A, the paper demonstrates on held-out or
chronologically subsequent queries that receipt-updated pattern or
relation-type coefficients improve retrieval over no-update and scalar-update
baselines.

This threshold requires substantially more clean outcome data. The systems
paper should not be held open indefinitely while waiting for it.

## Candidate primary claims

Freeze no more than three primary claims:

1. **Hybrid retrieval:** combined content and reviewed-pattern retrieval
   improves relevant recall at a fixed budget over either route alone.
2. **Structural rent:** typed structural relations help only where ablation
   demonstrates functional benefit; global connectivity alone is not evidence
   of retrieval value.
3. **Auditable adaptation:** offered/outcome receipts provide an auditable
   basis for adapting retrieval, while more granular operator learning is
   claimed only when calibration and held-out evidence support it.

Everything else should be marked as a secondary finding, implementation
detail, limitation, or research hypothesis.

The wording of these claims must be updated if the experiments do not support
them. In particular, a null result from the hybrid ablation is a result about
the present system, not a reason to change the evaluation after the fact.

## Current blockers

### No unified evaluation method

The evidence section currently mixes:

- end-to-end cases;
- synthetic mechanism tests;
- topology measurements;
- implementation defects and fixes;
- infrastructure failures;
- negative and below-calibration results.

A finished paper needs a Methods or Evaluation section that defines the
corpus, chronological cutoffs, query set, relevance judgments, baselines,
budgets, metrics, exclusions, and software/data versions before presenting
results.

### Deployed, dark, and proposed mechanisms blur together

The formal frame describes graph propagation and an evolving operator. The
deployed system is currently closer to a lexical query ladder, content match,
pattern expansion, and scalar per-memory receipt reweighting.

Every mechanism should therefore be labelled:

- **deployed**;
- **implemented dark**;
- **proposed or chartered**.

The abstract and conclusions should make claims only at the level supported by
the corresponding status and experiment.

### Evidence-channel semantics need an audit

The paper currently places several distinct ideas too close together:

- review of an attachment;
- evidential warrant for a memory;
- the agent's attribution that it used a memory;
- independent witnessing of the task outcome.

An externally witnessed successful outcome does not by itself independently
witness the agent's claim about which memory it used. These fields and claims
must be separated.

In addition, `:witness-status` is currently derived from a lane label rather
than from the presence and resolution of evidence. It should be repaired or
excluded as an independent evaluation signal before publication.

### Superseded claims remain in the specification

The spectral-gap embodiment still describes a threshold below which retrieval
dynamics cannot beat direct lookup. The topology work has shown that the
chosen statistic is projection-sensitive and can reward degree inflation
without corresponding functional improvement.

That embodiment should be retracted or reframed as a diagnostic research
question. Global spectral quantities may be reported descriptively, but they
should not gate a retrieval claim until validated against functional outcomes.

### Core prose sections remain unfinished

The architecture and related-work sections are placeholders. These are
structural parts of the argument, not copy-editing tasks to leave until the
end.

## Experimental programme

### Experiment 0: frozen benchmark and evidence model

All later experiments depend on one shared benchmark.

Construct a chronological, transport-clean set of theorem-proving dispatches.
For each query, preserve the corpus, retrieval graph, receipts, and retrieval
implementation as they existed at dispatch time. Avoid evaluating an old query
against knowledge added only later unless that is an explicitly named
counterfactual.

Label each query/memory pair separately for:

- relevance to the problem;
- whether it surfaced;
- whether the solver cited or reported using it;
- whether it was load-bearing in the resulting proof;
- whether the task outcome was independently witnessed.

Timeouts, store outages, malformed responses, and unavailable transports are
missing or operational-failure observations, not retrieval misses.

The existing `n >= 20` gate is a useful absolute minimum. A set of 30--50
varied queries would give more stable comparisons. Guard against
pseudoreplication where many rows are minor variants of the same theorem or
memory.

Use a small locked metric set:

- recall at a fixed candidate budget;
- reciprocal rank or nDCG;
- precision at the candidate budget;
- empty-result rate;
- retrieval latency.

The primary analysis should not change metrics or exclusions after seeing the
arm results.

### Experiment 1: deployed retrieval ablation

At the same corpus state and candidate budget, compare:

1. content-only retrieval;
2. pattern-only retrieval;
3. endpoint-only retrieval, if distinct from the pattern arm;
4. the deployed hybrid;
5. optionally, the tiered query ladder against a single conjunctive query.

This is the paper's most important experiment because it tests whether the
deployed combination pays for its added machinery.

The first topology experiment provides two motivating cases, not a result:
direct content preserved the historically missed run25 exact memory, while
pattern expansion supplied three of four historically used memories in the
Lemniscate case. The benchmark determines whether those complementary
strengths generalise.

### Experiment 2: structural rent

Expand topology evaluation from structural appearance to functional effect.
Run the following over the frozen benchmark.

#### 2a. Relation rent

Ablate one retrieval source or relation at a time, including:

- patterns;
- subjects;
- distillation/provenance relations;
- any proposed middle-tier cluster relation.

Keep the retrieval budget fixed. Measure changes in relevant recall, ranking,
path diversity, and empty-result rate.

#### 2b. Damage sensitivity

Remove or disable:

- an individual attachment;
- a pattern role;
- a memory or pattern node;
- a relation family;
- a whole cluster.

For an intervention \(A\), define functional damage as:

\[
D_{\mathrm{functional}}(A)
  = \operatorname{score}(G) - \operatorname{score}(G \setminus A).
\]

A structural feature pays rent only if its removal causes reproducible
functional damage. This extends the first offline `D_state` sweep toward the
more important `D_functional` measurement.

#### 2c. Hierarchy intervention

Compare the flat deployed pattern organisation with one derived middle-tier
technique or cluster organisation. Hold the corpus, query, and candidate budget
constant.

This tests the proposed hierarchy as a retrieval intervention, rather than
testing whether an arbitrary clustering happens to look graph-like.

#### Structural diagnostics

Record reachability, component structure, degree concentration, normalised and
unnormalised spectra, cluster liveness, and MetaCA-style damage as explanatory
diagnostics. Do not use them as success criteria unless they predict held-out
functional outcomes.

### Experiment 3: receipt-update replay

When sufficient observations exist, replay the frozen outcome sequence
chronologically and compare:

1. no receipt update;
2. deployed scalar per-memory update;
3. pattern-level update;
4. typed or relation-level update, if sufficiently observed.

Evaluate an update on later observations, not the same observations from which
its coefficients were estimated. Leave-one-out evaluation may supplement the
chronological analysis but should not substitute for a realistic information
boundary.

If there are too few observations per coefficient family, retain the honest
verdict `:below-calibration-minimum`. This experiment is a publication gate
only for Threshold B.

### Experiment 4: exploration-floor robustness

Retain the synthetic confirmation-collapse battery as mechanism evidence, with
a deliberately narrow claim: under the constructed dynamics, the exploration
floor prevents or reverses collapse.

Repeat it across:

- random seeds;
- decoy strengths;
- exploration-floor values;
- traversal budgets.

A replay-based stress test using real retrieval candidates would strengthen
the evidence, but the synthetic battery need not become the paper's main
effectiveness experiment.

### Experiment 5: operational reliability

Report the reliability of the retrieval observation channel separately from
retrieval quality:

- attempted and completed recalls;
- timeouts;
- store or transport unavailability;
- malformed observations;
- latency distribution;
- completeness of offered/outcome receipt pairs.

This prevents infrastructure failures from appearing as negative relevance
judgments while still making production reliability visible.

### Experiment 6: warrant and provenance audit

Before treating warrant discipline as a primary contribution, audit and report
separate counts for:

- reviewed attachments;
- memories with resolved evidence-backed warrants;
- solver citations or use attributions;
- independently witnessed task outcomes;
- complete offered/outcome receipt pairs.

Repair `:witness-status` so that the status can fail when the corresponding
witness record is absent or unresolved. If that repair is not complete, the
field must not be used as an independent experimental signal.

## Proposed paper structure

Write a clean paper organised by argument rather than by discovery date.

1. **Problem and contributions**
   - Why stored information is not automatically usable memory.
   - The three frozen contributions.
2. **Deployed system and trust boundaries**
   - Data model and bitemporality.
   - Retrieval pipeline.
   - Proposal, review, attribution, and outcome-witness boundaries.
   - Deployed/dark/proposed status table.
3. **Formal model**
   - Only the notation needed to express implemented mechanisms and tested
     hypotheses.
   - Explicit mapping from mathematical object to implementation status.
4. **Evaluation methodology**
   - Benchmark, chronological boundaries, labels, baselines, metrics,
     exclusions, and versions.
5. **Results by research question**
   - Does hybrid recall improve retrieval?
   - Which structural relations pay rent?
   - Do receipt updates improve later ranking?
   - Does the floor prevent collapse?
   - How reliable is the observation channel?
6. **Failure analysis and threats to validity**
   - Small sample sizes, attribution uncertainty, theorem-family dependence,
     infrastructure missingness, and dark-mechanism limitations.
7. **Related work**
   - Graph and hypergraph retrieval.
   - Relevance feedback and learned sparse/dense retrieval.
   - Spreading activation and cognitive architectures.
   - Auditable agent memory and provenance.
   - MetaCA and adaptive-operator lineage.
8. **Conclusion and research programme**
   - Established results first.
   - Explicit next tests for unestablished claims.

Appendices should contain:

- the claims-grade defensive-publication specification;
- detailed implementation/status mappings;
- full experiment protocols and artifact references;
- the correction, retraction, and increment ledger.

The body should state the final finding and its scope. The appendix may retain
the chronology by which that finding was reached.

## Claim/evidence matrix

Before rewriting, maintain a table of this form:

| Claim | Current evidence | Required evidence | Publication status |
|---|---|---|---|
| Hybrid recall beats a single route | A few complementary live cases and the first topology sweep | Frozen multi-query ablation | Open |
| Reviewed graph structure improves retrieval | End-to-end pattern-mediated cases | Relation-rent and damage experiments | Open |
| Receipts close an auditable learning loop | Offered/outcome records and witnessed proof outcomes | Receipt-completeness and provenance audit | Partially supported |
| Receipt-updated operators improve ranking | Scalar deployment; dark replay below calibration | Chronological comparison with adequate observations | Below calibration |
| Exploration floor prevents collapse | Synthetic confirmation-collapse battery | Multi-seed and sensitivity replication | Mechanism evidence |
| Global topology predicts retrieval utility | Conflicting and projection-sensitive spectral results | Functional predictive validation | Retracted as current criterion |

Each statement in the abstract and conclusion should resolve to a row in this
matrix. Claims without an evidence row should be removed or identified as
hypotheses.

## Editorial workflow

### Phase 1: claim freeze

- Choose Threshold A or Threshold B as the target.
- Freeze the primary claims and research questions.
- Mark all mechanisms deployed, dark, or proposed.
- Freeze the metrics and minimum observation counts.

### Phase 2: evidence integrity

- Audit witness provenance and receipt completeness.
- Correct operational failures misclassified as retrieval results.
- Freeze chronological corpus snapshots or reconstruct their boundaries.
- Record implementation and dataset versions.

### Phase 3: core evaluation

- Build the shared benchmark.
- Run the deployed retrieval ablation.
- Run structural-rent and damage experiments.
- Run floor sensitivity tests.
- Run receipt replay if the data clear the calibration gate.

### Phase 4: clean rewrite

- Snapshot the current paper as the evidence ledger.
- Start the paper body from the frozen outline rather than editing the
  chronology into place.
- Populate results from frozen artifacts.
- Reduce MetaCA and spectral analogies to the parts that explain or predict an
  evaluated mechanism.
- Move the claims-grade and correction records to appendices or a companion
  dossier.

### Phase 5: adversarial review

For every major statement, ask:

- Is it deployed, dark, or proposed?
- Is the outcome independently witnessed?
- Is memory use independently established or merely attributed by the agent?
- Was the hypothesis and metric fixed before the result?
- Is a missing observation being treated as a negative one?
- Does the baseline receive the same corpus, query, and budget?
- Could the structural metric be improved without improving retrieval?

Reproduce the main tables from a clean checkout or frozen artifact bundle.

### Phase 6: publication pass

- Complete architecture and related work.
- Replace pending markers with results or explicit future work.
- Remove the living-draft banner and increment log from the paper body.
- Check notation and terminology globally.
- Ensure abstract, results, limitations, conclusion, and claims appendix agree.
- Publish the benchmark definition, arm configurations, result tables, and
  provenance audit with the paper.

## Definition of done

The systems-paper threshold is met when:

- the three primary claims are frozen and accurately scoped;
- deployed, dark, and proposed mechanisms are visibly distinct;
- the benchmark and exclusions are reproducible;
- the deployed retrieval ablation is complete;
- structural claims are supported by functional ablation rather than topology
  alone;
- witness, attribution, and receipt semantics have been audited;
- all abstract and conclusion claims point to reported evidence;
- architecture and related work are complete;
- the correction history is preserved outside the main argumentative flow.

The evolving-operator threshold additionally requires:

- adequate observations at the declared calibration level;
- a chronological or genuinely held-out comparison against no-update and
  scalar-update baselines;
- a measured benefit, null result, or failure boundary reported without
  changing the preregistered evaluation.

## Immediate next action

Create the frozen benchmark and its labeling/provenance schema before adding
more mechanisms. It is the common instrument needed to evaluate hybrid recall,
topological rent, functional damage, receipt updates, and future cluster
liveness on comparable evidence.
