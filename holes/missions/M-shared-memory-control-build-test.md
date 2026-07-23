# Mission: Shared memory/control build-test regime

**Date:** 2026-07-23
**Status:** DERIVE — Phases 1–3 ACCEPTED live; Phase 4 is next
**Owners:** Joe + Zaif/WM implementers
**Systems:** futon1b store, futon3c Zaif runners, futon2 War Machine, p4ng control patterns
**Cross-references:** `M-typed-memories`, `CODEX-HANDOFF-p0-memory-record.md`,
`E-memory-latency.md`, futon2 `M-wm-strategic-mission-selection`, p4ng
`main-2026.tex`

## Objective

Build one typed, bitemporal, hypergraphical memory-and-control substrate and
validate it in increasing-cost domains:

```
pure contract tests
  -> live shared-store tests
  -> inexpensive Zaif mathematics runs
  -> dark WM strategic decisions
  -> operator-gated WM decisions
  -> bounded live WM selection
```

The phases are promotion gates, not a calendar. A phase advances only when its
acceptance evidence is recorded. Failure returns to the smallest owning layer;
it never triggers a compensating score, an embedding fallback, or a premature
live flip.

## Standing invariants

1. **One substrate, explicit domains.** Zaif and WM memories use the same base
   record, hyperedge, endpoint query, bitemporality, provenance, retraction,
   and use/outcome receipt. `domain`, author, subject, mission/session, pattern,
   and witness identity remain explicit.
2. **Identity is contextual.** Author, session, domain, and decision identity
   are server/controller stamped, not trusted from agent payloads.
3. **Episodes are not scores.** A memory remains a concrete assertion or
   episode. Counts, citations, similarity, and attachment do not become value.
4. **Proposal is not evidence.** FTS or embeddings may propose an attachment.
   Only a typed witnessed edge can support a candidate.
5. **No self-certification.** The writer or selector may report use; an
   independently observed result determines outcome evidence.
6. **Unknown is not zero.** Missing evidence remains an explicit hole.
   Contradiction, infeasibility, and absence are distinct states.
7. **Bounded reads.** Every query has a limit, stable ordering/cursor, compact
   projection, latency observation, and an overload/failure result.
8. **No live scheduler mutation before Phase 7.** All prior WM phases are pure,
   fixture-backed, replay, dark, or operator-visible shadow work.

## Phase 0 — Freeze the baseline and contracts (complete)

### Build

- P0 `memory_record` writes one evidence entry and one `:memory/assert`
  hyperedge with stable identity.
- Record the live WM additive baseline:
  `0.25 central + 0.45 strategic + 0.30 phase-doable`, followed by gates and
  non-progress decay.
- Add the dark typed mission/control projection kernel.
- State the strategic policy grain separately from tactical cascades.

### Tests and evidence

- P0 fixture suite and live write/read acceptance already recorded by
  `M-typed-memories`.
- Dark WM projection tests reject malformed, unattributed, proposed-only, and
  blocked support.
- Freeze representative Zaif and WM fixtures before changing retrieval.

### Exit

Complete. The additive WM controller remains the named baseline, not `E_S`.

## Phase 1 — Make the shared retrieval seam real

**Implementation checkpoint, 2026-07-23:** the futon1b endpoint query now treats
`end + type` conjunctively and pushes both predicates into the bounded query.
Futon2 owns the pure compact-projection/use-receipt contract; futon3c consumes
it through a bounded, domain-explicit recall client. New Zaif memory writes
stamp a controller-supplied domain in hyperedge properties. Fixtures exercise
both mathematics and WM records through the same functions.

The in-process futon1b HTTP suite passes 53/53, including conjunctive and empty
`end + type` cases. Focused futon2 contract/control tests pass 8 tests / 29
assertions. Focused futon3c recall/write/shared-contract tests pass 17 tests /
260 assertions. Kondo and paren gates are clean.

A bounded read-only probe before deployment deliberately exposed the
old serving behavior: requesting the a94A06 field-simp memory by both its exact
evidence endpoint and `type=memory/assert` returned three unrelated typed
memories, proving that the type-first conditional was genuinely live rather
than merely a source-level concern.

**Deployment and live acceptance, 2026-07-23:** ACCEPTED.

- The standalone `futon1b-server.service` was gracefully restarted because it
  has no Drawbridge endpoint. PID `1097610` was replaced by `3747853`; health
  recovered with the XTDB node open. The futon3c Agency JVM was not restarted.
- The previously failing a94A06 query then returned exactly
  `hx-mem-6bcbb51e-f368-4bcc-afbf-97fcc616772d` (`:count 1`).
- The shared contract, substrate client, memory writer, Zai API, recall client,
  HTTP registration path, and dev Zai factory were loaded through Drawbridge.
- A fresh runner, `zai-5`, was auto-registered at `/home/joe/code/apm-lean`
  with controller-stamped `:memory-domain :mathematics`. Its live
  `memory_record` call created
  `e-9e32e848-089b-4ec7-8aff-63d6558e7db6` /
  `hx-mem-9e32e848-089b-4ec7-8aff-63d6558e7db6`.
- A reviewed attachment added the
  `math-formalization/tactic-algebra-interference` endpoint while retaining
  `:witness-status :self-asserted`. The shared client recalled the one compact
  mathematics memory in 2714.6 ms with zero malformed or missing records.
- The same writer created dark WM memory
  `e-9d36c1b0-5f07-4a44-bfb7-b23825b6ee4b` /
  `hx-mem-9d36c1b0-5f07-4a44-bfb7-b23825b6ee4b`, attached to `p4ng/R15`.
  The same client recalled it in 2445.1 ms with its mission, provenance,
  domain, state, and self-asserted witness status intact.
- Reversing the requested domains returned zero memories in both directions
  and `:domain-excluded 1`, demonstrating explicit isolation rather than
  accidental absence.
- The same use-receipt constructor accepted both live ids and produced
  `:pending-outcome` receipts. Persistence and outcome attachment remain Phase
  2 work.
- Targeted Zai auto-registration test: 1 test / 12 assertions, green. The full
  HTTP namespace still has seven unrelated pre-existing expectation drifts
  (99 tests / 494 assertions); the changed registration test itself is green.

Phase 1 acceptance is therefore closed. The live memories remain concrete
self-asserted test episodes, not value evidence. Phase 2 may now wire
pattern-conditioned recall into `psr_search` / `psr_select` and persist actual
use/outcome receipts.

### Build

- Give base memories controller-stamped `domain` identity without rewriting
  P0 bodies; add it through contextual evidence/hyperedge properties.
- Make the futon1b endpoint query support a bounded `end` lookup with an
  optional type filter. Resolve the current `type`-versus-`end` conditional
  limitation rather than querying a whole type and filtering client-side.
- Define one compact memory projection used by futon3c and futon2:
  id, act/kind, hook, provenance, valid/system time, volatility/current state,
  domain, pattern/mission endpoints, and witness status.
- Define a common memory-use receipt:
  decision/session id, memory ids surfaced, memory ids used, pattern/cascade
  id, domain, inclusion reason, and later independent outcome id.

### Tests

- Pure contract tests for math and WM fixtures against the same validator.
- Negative tests for identity smuggling, missing provenance, wrong domain,
  proposed-only attachment, retracted memory, and unknown endpoint.
- In-process store tests for `end`, `end + type`, stable ordering, cursor,
  limit, current/as-of behavior, and overload/failure-as-data.
- Cross-domain isolation test: a math query cannot silently consume a WM
  episode unless the query explicitly permits that domain.

### Acceptance

From the same client function, one mathematics pattern endpoint and one p4ng
control endpoint each return the correct compact memory and no unrelated
memory. No vector or free-text search is used. Query bounds and elapsed time are
recorded. This is the only gate into Phase 2.

### Rollback

Keep P0 writes intact; disable the new projection/query route. No stored episode
requires migration.

## Phase 2 — Zaif pattern-conditioned recall vertical slice

**Deployment and live acceptance, 2026-07-23:** ACCEPTED.

- `psr_search` now enriches each bounded pattern candidate with compact hooks
  from reviewed attachments. `psr_select` is the full-body retrieval trigger
  and stores the exact surfaced set under its PSR. No attachment yields an
  explicit hole; substrate failure remains failure-as-data.
- Agent-supplied pattern subjects are stamped `:attachment-status :proposed`.
  Recall admits only explicit `:reviewed` attachments; absence of review
  metadata fails closed. Challenged and volatile memories remain labelled.
- `pur_update` accepts used ids, reasoned rejections, and a separately
  persisted outcome id. It writes a shared use receipt whose surfaced, used,
  rejected, and unused sets are explicit. The outcome id is accepted only
  when the referenced entry exists, has a different author, and carries
  `:memory-outcome/witness-status :independently-witnessed`.
- The three canonical earlier-session a94A06 memories were reviewed and
  attached to `math-formalization/tactic-algebra-interference`; the Phase 1
  mathematics and WM attachments were also stamped reviewed so the stricter
  read rule did not regress them.
- Fresh runner `zai-6`, session
  `zai-93d787f8-4f58-41d2-a5e5-88e5345483ee`, retrieved three bounded hooks
  and then their full bodies under PSR
  `psr-9fbf1e64-3104-4757-813f-756e1864c871`.
- The natural semantic query about `field_simp` and polynomial-denominator
  normalization did **not** return the intended pattern; a second id-like
  query did. This is a recorded candidate-construction prediction error, not
  hidden by successful recall after selection.
- In `/home/joe/code/apm-lean/FutonPhase2MemoryTrial.lean`, `zai-6` materially
  used earlier-session memory
  `e-6bcbb51e-f368-4bcc-afbf-97fcc616772d` to rewrite the nonzeroness
  hypothesis into normalized denominator form before `field_simp`. The
  preregistered irrelevant polynomial identity used `ring` directly.
- `codex-4` independently reran
  `timeout 120s lake env lean FutonPhase2MemoryTrial.lean` against SHA-256
  `6cbcaf350b3f5f08063d540adf23798b4adf1a526556c2e47c447b0b3de3775b`;
  exit zero was recorded separately as
  `e-c830f282-b5fe-4475-bde7-c4a18246c441`.
- Final PUR `pur-925e34b4-326e-42e2-956f-d304ef00bed0` and memory-use evidence
  `e-f7c2532b-d6a0-4744-a0c8-945e0c45f596` cite that outcome. The receipt
  surfaces all three memories, uses only `e-6bcbb51e...`, rejects
  `e-3c8277ed...` as an irrelevant derivative-composition episode, and
  leaves the redundant `e-9e32e848...` restatement unused.
- The live run found and repaired two boundary mismatches: the tool advertised
  prediction error as prose while the PUR gate required a map, and the HTTP
  backend stringified reason maps keyed by evidence ids. Prediction-error
  prose is now normalized to `{:description ...}`; reasons persist as vectors
  of `{memory-id, reason}` rows. Direct store read-back confirms both shapes.
  Two earlier successful test receipts remain append-only observations of the
  pre-repair representation; the final id above is the acceptance receipt.
- Focused futon3c gate: 22 tests / 292 assertions after the live fixes;
  the final Phase 2 namespace is 3 tests / 26 assertions. The focused futon2
  shared contract is 5 tests / 25 assertions. Fixtures cover proposed,
  challenged, volatile, missing-body, unavailable-store, stale-pattern,
  rejected, unused, ablated, and independently witnessed paths.

This establishes the retrieval/use/outcome mechanism, not a claim that memory
improves theorem-proving success. The external checker is the recall-disabled
execution ablation for the frozen artifact; the pure integration test also
compares recall-enabled and explicitly unavailable seams. The semantic-query
miss remains an owning Phase 3 input rather than a reason to introduce an
embedding fallback.

### Build

- Attach reviewed pattern endpoints to the existing a94A06 memory corpus.
- Enrich `psr_search` results with bounded supporting and challenging memory
  hooks from Phase 1.
- Make `psr_select` return the selected memory bodies.
- Add `memory_ids` surfaced/used to the PUR or shared use receipt.
- Record the independently witnessed proof result separately.

### Tests

- Unit tests: proposed attachments do not surface as warrants; challenged and
  volatile memories remain labelled; no attachment produces an explicit hole.
- Integration test: search → select → retrieve → cite/use receipt → external
  result, using an earlier-session memory.
- Ablation: run the same fixture with recall enabled and disabled.
- Failure injection: unavailable store, partial body/edge state, stale pattern,
  and memory that is retrieved but deliberately rejected.

### Live mathematics trial

Use a small preregistered set of fresh problems, including at least one where a
stored memory should help and one counterexample/irrelevant case. Record:

- candidate pattern and inclusion reason;
- memories surfaced, selected, cited, rejected, or unused;
- retrieval latency;
- proof outcome from the external checker;
- correction/prediction error.

**Phase 2 live preregistration, 2026-07-23 (before dispatch):**

- Fresh runner: `zai-6`, domain `:mathematics`, cwd
  `/home/joe/code/apm-lean`.
- Relevant case: prove an inverse/product identity whose goal uses the
  normalized denominator `1 - u + u^2` while its nonzeroness hypothesis uses
  `u^2 - u + 1`. Expected useful earlier-session memory:
  `e-6bcbb51e-f368-4bcc-afbf-97fcc616772d`.
- Irrelevant case: prove an ordinary polynomial square identity by `ring`;
  denominator and `HasDerivAt.comp` memories should not be silently credited.
- Recall-enabled run: the runner must call `psr_search`, select
  `math-formalization/tactic-algebra-interference`, receive full memory bodies,
  and write a standalone trial file. It must stop before `pur_update`.
- Independent check: `codex-4` runs `lake env lean` against that file and
  records the result as a separate `:pattern-outcome` evidence entry stamped
  `:independently-witnessed`.
- Receipt closure: only after receiving that evidence id may the runner call
  `pur_update`, citing memories actually used and explaining deliberate
  rejections. The same two theorem statements are also checked with recall
  disabled by a clean external checker invocation; this is a mechanism
  ablation, not an improvement claim.

### Acceptance

At least one fresh run retrieves and cites an earlier-session memory through its
pattern endpoint and receives an external outcome. Irrelevant/challenging
memory is not silently promoted. Every surfaced/used id is traceable. This
proves mechanism, not general mathematical improvement.

## Phase 3 — Memory lifecycle and cheap repeated trials

### Build

- Enable challenge, retract/supersede, and current/as-of queries.
- Preserve both original and correcting episodes.
- Compute correction lag and retrieval-to-use latency.
- Add a bounded trial harness for repeated Zaif mathematics runs using the same
  read/write/use/outcome path.

### Tests

- Designated volatile topology fixture is superseded without deletion.
- Current and as-of queries disagree exactly where expected.
- A retracted attachment disappears on the next projection.
- A failed recalled strategy records an outcome and challenge; it does not
  erase the episode or train itself as success.
- Resource test exercises bounded concurrency and the futon1b admission/error
  path without unbounded corpus scans.

### Acceptance

One real correction completes the full bitemporal loop, and repeated math runs
produce an auditable table of recall, use, external outcome, and correction
events. No success-rate claim is required yet.

### Live acceptance, 2026-07-23

ACCEPTED, as an audit table rather than a success-rate estimate.
The machine-readable ledger is
`holes/labs/M-typed-memories/phase3-trial-results.edn`.

| Trial | Query result | Use and external outcome | Correction/residual |
|---|---|---|---|
| 1, Phase-2 residual | Natural `field_simp`/denominator query promoted `tactic-algebra-interference` to rank 1 through reviewed memory support | Used `e-6bcbb51e`; independent Lean exit 0 at evidence `e-16c5d419…` | The general syntactic lesson held, but its claimed normal form was wrong. It was challenged and superseded by reviewed correction `e-aa9c729b…`; correction lag 5,109 ms. |
| 2, pattern-id control | Intended pattern ranked first; after the lifecycle read fix, selection returned the correction and excluded the superseded original | Used `e-9e32e848`; independent Lean exit 0 at typed evidence `e-b2e576e1…`; PUR `pur-4da1c5db…` | A superseded edge consumed a pre-filter result slot. Bounded 3× overfetch followed by lifecycle filtering fixed the projection. An intentional exact PUR replay returned the same PUR and receipt with `:idempotent-replay? true`. |
| 3, memory-hook paraphrase | Full conjunctive FTS initially missed. Bounded lexical token fallback found correction `e-aa9c729b…` and promoted the intended pattern from absent to rank 1 | Used the corrected memory; independent Lean exit 0 at JSON-posted evidence `e-034ee8f8…` | Sequential token fanout worked but cost about 44 s, so it was collapsed to one bounded token-disjunction query. No embedding fallback was introduced. |

The lifecycle implementation preserves original and correcting evidence, uses
XTDB2 valid-time for supersession/retraction, exposes current/as-of reads, and
records correction lag and retrieval-to-use latency. The bounded trial harness
refuses more than 20 trials or concurrency above 4 and labels its result
`:audit-only-no-success-rate`.

Two additional boundary defects were found and fixed during acceptance:

- exact `pur_update` retries are now idempotent over the full normalized
  post-use assertion, without suppressing a genuinely changed correction;
- generic JSON evidence writes normalize the typed memory witness enum, so an
  external checker can satisfy the no-self-certification gate without an
  in-process write.

## Phase 4 — WM memories on the same seam, dark only

### Build

- Add a WM controller-facing writer/projection that uses the Phase 1 contract;
  do not fork `memory_record` or create a second store.
- Stamp WM domain, mission, observation/intervention, p4ng control-pattern,
  provenance, and witness endpoints.
- Curate a small reviewed corpus for WM compliance, typed memories,
  policy-conditioned EIG, tripwires, and at least one blocking/counterexample
  episode.
- Feed recalled bodies—not merely memory ids—into the existing dark
  `mission_control_graph` candidate projection.

### Tests

- Run the Phase 1 contract suite unchanged against WM records.
- Fixture test: each active p4ng pattern retrieves the intended supporting and
  challenging episodes and exposes the intended mission relation.
- Blocked/retracted/proposed-only edges cannot admit a mission.
- Cross-domain proposal is visible in audit but cannot become witnessed WM
  evidence without a WM-domain witness.

### Acceptance

A dark WM query selects a p4ng endpoint, retrieves a concrete WM episode, and
projects a related mission with complete reasons. The same query and receipt
code used by Zaif is present in the trace. Live mission ordering is unchanged.

## Phase 5 — Outer strategic cascades and policy holes

### Build

- Reuse the cascade representation to construct a slow control-pattern
  cascade from current deficits, tripwires, and policy holes.
- Treat each cascade step as a bounded memory/mission subgraph query.
- Produce a reason-bearing mission or short mission-cascade frontier.
- Preserve missing transitions as explicit holes.
- Add the R17''' mint lane: repeated witnessed holes plus memories may propose
  a new control pattern at an unearned prior; they cannot promote it.

### Tests

- Golden cascades with known dependencies, branch, blocker, and hole.
- Determinism under stable inputs and explicit explanation for every included
  or excluded mission.
- Metamorphic tests: retracting a warrant removes its support; adding an
  irrelevant memory changes nothing; changing endpoint order changes no set
  semantics.
- Mint test: no adequate pattern deposits a traceable candidate; one memory,
  similarity, or author declaration cannot promote it.

### Acceptance

For a reviewed dark WM state, the outer cascade explains the candidate frontier,
the retrieved episodes, dependencies, blockers, and holes without centrality or
embedding similarity. A reviewer can reconstruct the frontier from the trace.

## Phase 6 — Strategic outcome model and factor ablations

### Build

- Accumulate independently witnessed dark/replay WM transitions.
- Predict mission outcomes and uncertainty separately from executable support,
  proposal potential, and habit.
- Compare the current additive controller with:
  - direct control-cascade support only;
  - support plus recalled outcome model;
  - each of those with and without centrality;
  - any calibrated log-linear/product-of-experts model whose terms have earned
    probabilistic semantics.

### Tests

- Preregister the judgement/outcome set and freeze it before comparison.
- Report ranking agreement, top-choice accuracy where a gold choice exists,
  log loss/Brier score for outcomes, calibration intervals, coverage, abstention,
  explanation completeness, and latency.
- Bootstrap or paired intervals accompany differences; small samples remain
  labelled exploratory.
- Counterfactual trace reports what the additive controller would have chosen.

### Acceptance

Centrality is retired only if its held-out ablation is neutral or beneficial
and the direct model preserves coverage. A new outcome model advances only if
it is calibrated enough to beat or complement the named baseline without
conflating unknown, blocked, and negative evidence.

## Phase 6b — Optional embedding experiment

This phase is off the critical path and may begin only after Phase 4 provides
reviewed typed edges.

- Compare lexical/structural proposal, generic embedding, and a dedicated
  mission-plus-control-pattern embedding.
- Score relation proposal recall/precision on held-out edges.
- Never allow a vector proposal to bypass witness/review status.

Acceptance is informative only. Losing to the non-vector baseline causes no
architectural rollback because direct endpoint recall is already operational.

## Phase 7 — Strategic policies and `E_S`, shadow first

### Build

- Give length-1 missions, then short mission cascades, stable strategic policy
  identities.
- Learn a separate Dirichlet habit `E_S` from selection frequency.
- Keep `E_S`, predicted `G_S`, hard support, proposal reasons, and recalled
  memories separately visible.
- Run alongside the live controller without affecting its choice.

### Tests

- Unit/property tests for normalization, unseen-policy mass, identity changes,
  temperature limits, and separation from outcome value.
- Replay and dark traces report model winner, baseline winner, disagreement,
  uncertainty, reasons, and memory ids.
- Verify tactical `E_T` counts cannot leak into strategic `E_S`.

### Acceptance

A fixed shadow window completes with no unexplained candidates, no identity or
provenance loss, and reviewed disagreement cases. Promotion is an operator
decision based on that evidence, never an automatic metric threshold.

## Phase 8 — Operator-gated canary and bounded live use

### Build/test ladder

1. **Advice only:** show strategic recommendation and counterfactual baseline;
   operator still chooses.
2. **Confirm-to-enact:** recommendation may execute only after explicit
   operator confirmation.
3. **Bounded autonomy:** only an allow-listed reversible mission class, with
   resource caps, tripwire, and immediate fallback to the additive controller.

At every rung, independently record predicted and observed outcomes, memory
use, operator override, and rollback reason. Advance one rung at a time after a
reviewed window; any safety, provenance, query-bound, or explanation failure
returns to advice-only.

### Acceptance

The canary demonstrates stable operation and useful reviewed decisions without
loss of operator/completion gates. The old controller remains available until
an explicit retirement mission closes it.

## Per-phase completion packet

Every phase reports:

- exact files and commit SHAs;
- schema/API version and stored-data migration status;
- fixtures and test namespaces run, with test/assertion counts;
- clj-kondo and check-parens results for changed Clojure/EDN;
- live-store queries used, all with explicit limits;
- latency/resource observations and injected failures;
- acceptance evidence and unmet criteria;
- whether any serving namespace was reloaded (Drawbridge only) and confirmation
  that no JVM restart occurred;
- rollback procedure and current live-effect statement.

## Immediate next packet

Phase 1 is next. It should be split into two reviewable changes:

1. futon1b bounded `end + optional type` query and contract tests;
2. shared compact projection/use-receipt contract consumed from futon3c and
   futon2 fixtures.

Do not begin `psr_search` enrichment, WM live integration, embeddings, or
mission-score replacement until both Phase 1 changes pass their acceptance
gate.
