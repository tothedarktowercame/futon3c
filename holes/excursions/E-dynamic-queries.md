# E-dynamic-queries — retrieval that changes its query operator

**Date chartered:** 2026-07-23
**Type:** E-prefix excursion; bounded dark companion to
`M-typed-memories` and `M-shared-memory-control-build-test`.
**Status:** RUNG 1 ACCEPTED DARK; PHASE 5 CHECKPOINT INFORMATIVE
(2026-07-23).
**Boundary:** this excursion may rank only inside the Phase 1–4 admissible
dark subgraph. It cannot admit records, change domain/lifecycle/witness gates,
mutate memory, alter shared receipts, or affect live mission ordering.

## HEAD

When the desired memory is not known until it is encountered, retrieval is not
only propagation over a fixed graph. It is a coupled dynamics over candidate
contents and the patterns used to retrieve them: evidence changes which typed
relations should govern the next traversal.

## Theoretical frame

Use the bounded state

\[
s_t=(x_t,\theta_t,F_t,B_t),
\]

where:

- \(x_t\) is activation over currently admissible memory or mission nodes;
- \(\theta_t\) is activation over patterns and typed traversal operators;
- \(F_t\) is the current facet resolution;
- \(B_t\) is the remaining traversal budget.

For relation-specific graph operators \(\Delta_r\), a pattern-conditioned
operator has the form

\[
\Delta_{\theta_t}=\sum_r\theta_{t,r}\Delta_r.
\]

A fuller system would couple content and operator updates:

\[
x_{t+1}=\Phi(x_t,\Delta_{\theta_t},q),\qquad
\theta_{t+1}=\Psi(\theta_t,x_{t+1},q).
\]

Rung 1 deliberately implements only the smallest executable projection of
this idea: one deterministic propagation step from active patterns through
typed, already-admitted control relations to candidate missions. It is not a
general Laplacian engine and it does not learn \(\theta\).

## Relationship to the accepted engineering plan

Phase 4 of `M-shared-memory-control-build-test` is accepted dark. Its existing
writer, endpoint recall, admissibility projection, bitemporal lifecycle,
domain and independent-witness gates, full bodies, and use receipts remain
authoritative.

The sequencing is:

1. freeze the Phase 4 projection as the theory layer's input contract;
2. build Rung 1 now as a detached replay in parallel with Codex-4's Phase 5;
3. checkpoint before Phase 5 is completed, carrying over trace fields only if
   the replay is informative;
4. build budgeted facet refinement after Phase 5 supplies cascades and holes;
5. evaluate outcome-conditioned operator updates with Phase 6's independently
   witnessed outcomes.

The existing endpoint order is always retained as the control arm.

## Rung 1 — fixed typed re-ranker

### Construction

`futon3c.peripheral.dynamic-queries/fixed-typed-ranking` consumes only the
`:projection` returned by the Phase 4 dark adapter. It:

1. records the projection's candidate order as `:control-ranking`;
2. starts from explicit candidate activation \(x_t\);
3. propagates explicit pattern activation \(\theta_t\) through a fixed
   relation-weight table;
4. counts each distinct `(pattern, relation)` path once, so repeated attached
   memories do not masquerade as independent evidence;
5. returns a deterministic `:typed-ranking` with auditable contribution rows.

The demonstration weights are illustrative and preregistered in the demo
script; they have no earned probabilistic or utility semantics.

### Invariants

- The typed and control rankings contain exactly the same candidate IDs.
- Candidate activation for anything outside the admitted set is rejected.
- Negative, infinite, unknown-relation, and malformed weights are rejected.
- Challenged, cross-domain, self-asserted, proposed, retracted, and blocked
  material cannot be reintroduced after Phase 4 filtering.
- The result always reports `:live-ordering-changed? false`.

### Demonstration

Run:

```bash
clojure -M scripts/run_dynamic_queries_demo.clj
```

The demo reads the reviewed Phase 4 WM corpus, calls the ordinary dark adapter,
then prints the fixed endpoint order beside the typed order and its
per-relation contributions. The blocked liveness mission remains absent.

### Rung 1 acceptance

Rung 1 is structurally accepted when:

1. the pure re-ranker preserves the admitted candidate set;
2. its ranking and reasons are deterministic;
3. a Phase 4 fixture replay shows a non-trivial, fully explained ordering;
4. the ordinary endpoint ordering remains present as counterfactual control;
5. relevant Clojure tests, clj-kondo, and parenthesis checks pass.

This is an instrumentation and falsifiability gate, not yet a retrieval-quality
claim. Held-out hit@k comparisons belong to the checkpoint dataset rather than
being manufactured from the small reviewed Phase 4 fixture.

### Verification, 2026-07-23

- Focused Rung 1 plus Phase 4 regression suite: 8 tests, 46 assertions,
  0 failures, 0 errors.
- `clj-kondo`: 0 errors, 0 warnings.
- `check-parens.el`: clean for the implementation, tests, and demo.
- Executable fixture replay: candidate set preserved; blocked
  `M-wm-tripwires` absent; typed order differs from the control with one
  contribution trace per admitted `(pattern, relation)` path;
  `:live-ordering-changed? false`.

### Phase 5 checkpoint, 2026-07-23

Independent Codex-4 review exercised Rung 1 against the completed Phase 5
reason-bearing frontier rather than the earlier flat Phase 4 projection. The
preregistered fixture target is `M-shared-memory-control-build-test` at \(k=1\).
Fixed endpoint order scored hit@1 = 0; the typed replay scored hit@1 = 1 while
preserving the exact three-candidate set. This is one exploratory target, not
a performance estimate.

The difference passed the instrumentation gate: every ranked candidate has a
typed contribution; the trace reports 3 distinct `(pattern, relation)` paths
over 3 patterns and 2 relation types, the four-step query budget, and fixed
ordering as `:counterfactual-ranking`. These fields now appear under Phase 5
`:retrieval-checkpoint`. The checkpoint has `:selected-mission nil` and
`:live-ordering-changed? false`; it cannot feed a choice back into the
frontier.

Run the integrated demonstration with:

```bash
clojure -M scripts/run_phase5_strategic_cascade_demo.clj
```

## Later rungs

### Rung 2 — outcome-conditioned operator update

Update \(\theta_t\) once from an independently witnessed outcome, then rerank
once. Gate on calibration, abstention, unsupported-answer rate, and recovery
from a misleading seed as well as target rank.

### Rung 3 — budgeted facet refinement

Choose the next facet or relation expansion by expected information gain under
explicit depth and query budgets. Preserve path provenance and diversity, and
ensure repeated supportive paths do not count as independent evidence.

## Principal epistemic risk

The main failure mode is endogenous confirmation: early accidental evidence
concentrates the operator on relations that retrieve more corroboration,
creating apparent certainty while hiding the target region. Later rungs must
therefore preserve exploration mass, keep independently witnessed challenges
reachable, expose entropy and path diversity, and test recovery from
misleading initial activation.

## Whistle agreement

The 2026-07-23 discussion involved Codex-4 (Phase 4 owner), Zai-3
(pattern-conditioned recall), and Codex-3 (mathematical/epistemic challenge).
They agreed on the three-rung plan and the hard Phase 1–4 boundary above. A
Claude mentor whistle was unavailable because that environment's subscription
access was disabled.
