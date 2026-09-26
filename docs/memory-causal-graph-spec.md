# The memory system's causal graph — authored spec, v1

**2026-08-01.** Companion to `memory-causal-graph-spec.json`, which is the
artifact. This is the DAG promised in the cover note's question 1 and in your
reply's "best immediate integration" — authored for encoding in your causal
engine, in your discipline as we understand it from the mission pack:
component-grain variables, mediators preserved, arrows carrying mechanisms
with evidence rather than narrative plausibility, time-indexed state instead
of cycles, and the measurement layer held separate from causal structure.

**20 variables, 34 evidenced arrows, 1 flagged conjecture, 4 removable leak
edges, 9 sensors, 2 regime axes, 2 interventions, 3 requested receipts.**

## How to read it

The causal spine follows your proposed component DAG, refined against our
code and V2's measurements:

```
problem + corpus + repo (state at t)
    -> route_intent -> query_terms -> lexical_candidates
    -> pattern_projection            [the attachment layer — C1 lives here]
    -> surfaced_set
    -> info_channel AND/OR policy_channel     [your edge-target distinction]
    -> runner_trajectory  <- repo_search      [the parallel grep channel]
    -> lean_witness -> outcome
    -> corpus/repo state at t+1               [the loop, time-indexed]
```

Four things we ask you to notice, because they are where the spec does work:

1. **`route_intent` is an explicit variable with two children** — it feeds
   the recall query (`query_terms`) *and* the repository search
   (`repo_search`). That is the sharpest form of our route-relativity
   finding: the route determines the vocabulary of *every* search channel,
   which is why a static connectivity measure of the corpus cannot predict
   usefulness. Route also carries a regime axis for transport-style
   queries.

2. **The one `conjectured` arrow annotation is C1**, the paper's central
   open claim: that the attachment layer (`pattern_projection`), not the
   lexical stage, generates the 64% empty-surfacing rate. Everything else
   is evidenced. If your encoding wants a cleaner separation, treat the
   causal arrow as established (memories demonstrably arrive via pattern
   attachments — E1 measured the arm share at 45%) and the conjecture as a
   claim about *effect dominance* on that edge.

3. **The leak edges are the point of the E2 isolation design.** L1–L4 are
   backdoor paths from post-solution knowledge into the trajectory. The
   `apmablate` account is graph surgery that severs them. Receipt Q2 asks
   the engine to confirm that severing them is what makes the ablation
   identify anything — turning our isolation checklist into a checkable
   claim.

4. **The sensors are the Phase-0 repair list.** Each planned sensor
   (S05–S07) is a measurement-layer node observing a causal node that
   currently has no sensor; the repairs were derived independently from
   V2's failures, and they tile the graph one-per-node. Missingness
   annotations per sensor carry the recency-bias facts your reply said
   must be modeled; the missingness *weighting* stays in our analysis
   code — what we want from the engine is structural: which analyses a
   given sensor's coverage licenses.

## The three receipts we request

Stated fully in `requested_receipts` in the JSON; in brief:

- **Q1** — what the cohort randomization (`do(recall_enabled)`) identifies,
  and what the lane/family subgroup contrasts additionally require.
- **Q2** — what E2's `do(withheld_ids = {M})` identifies given the L1–L4
  surgery, and the mediation decomposition (info vs policy channel) that
  becomes available once sensor S05 exists. Per your correction: stated as
  ITT of single-memory availability, not content effect.
- **Q3** — the filter-equivalence d-separation check, run on **both** corpus
  topologies. This one carries a falsifiable prediction: on the current
  star-forest, filtering M from the surfaced set should be equivalent to
  removing M from the store; after the multi-attachment repair populates
  the graph, we predict the equivalence **breaks** (removing M then changes
  what else surfaces through shared patterns). If the engine confirms the
  divergence, that becomes a preregistered expectation about the
  graph-population repair — the "above" predicting a consequence of a
  repair before the "below" ships it.

## What is deliberately out of v1

The goal-level conative graph (queue blocking relations) — per your Q1
caution, those are dependency relations until an intervention on a blocker
has characterized downstream consequences; v2 material. Statistical
estimation and missingness weighting — ours. Write-side identity /
duplication (our E7) — separable.

## Revision contract

This spec follows your live-model discipline: it is versioned, it is wrong
somewhere, and the intended loop is that encoding it in the engine exposes
the first errors (a variable that should split, a missing mediator, an
arrow the side conditions reject). Corrections land as v2 with the deltas
recorded, not as silent edits.
