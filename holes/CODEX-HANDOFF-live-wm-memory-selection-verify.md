# CODEX-HANDOFF — make the verified memory/query chain real in the live WM

Prepared: 2026-07-24 by codex-4  
Delivery: Joe will pass this file directly to the next agent.

## Goal

Close the gap found by the post-hoc VERIFY:

> The typed-memory/dynamic-query/strategic-policy chain composes correctly on
> the frozen corpus, but the live Phase 5 p4ng endpoints currently contain no
> WM memories. Make the chain run from real shared-store records, demonstrate
> it end to end, and only then connect its reason-bearing result to live War
> Machine selection.

This is not an advice-only exercise. Joe has already rejected both
operator-override semantics and replacing a live recommendation with
`abstain`. The adaptive result should eventually become the live strategic
selection inside the existing admissible domain. Downstream act/completion
gates remain authoritative over enactment.

Do not start a War Machine click during this packet. Finish with a live
selection trace ready for the click.

## Current source anchors

- Futon3c `master`: `a28c85c` —
  `verify typed-memory dynamic-query chain post hoc`
- Futon2 `M-propagators-ant-gate`: `191e168` —
  `separate strategic recommendation from actuation`

Both were pushed before this hand-off.

## Read first

1. `holes/verification/V-typed-memory-dynamic-queries-20260724.md`
2. `holes/excursions/E-dynamic-queries.md`
3. `holes/missions/M-typed-memories.md`, especially P1/P1b and the retrieval
   strategy
4. `holes/missions/M-shared-memory-control-build-test.md`, Phases 4–8
5. `holes/labs/M-typed-memories/phase4-wm-corpus.edn`
6. `holes/labs/M-typed-memories/phase5-outer-cascade.edn`
7. `test/futon3c/peripheral/posthoc_system_verification_test.clj`

Then inspect:

- `src/futon3c/peripheral/memory_write.clj`
- `src/futon3c/peripheral/memory_lifecycle.clj`
- `src/futon3c/peripheral/memory_recall.clj`
- `src/futon3c/peripheral/wm_memory.clj`
- `src/futon3c/peripheral/dynamic_queries.clj`
- `src/futon3c/peripheral/strategic_cascade.clj`
- `src/futon3c/peripheral/strategic_outcomes.clj`
- `src/futon3c/peripheral/strategic_policies.clj`
- `src/futon3c/peripheral/strategic_canary.clj`
- `futon2/src/futon2/aif/policy.clj`
- `futon2/scripts/futon2/report/war_machine.clj`
- `src/futon3c/aif/live_recommendation.clj`
- `src/futon3c/aif/stack_generator.clj`

Honor both repositories' `AGENTS.md`.

## Verified starting facts

### What works

The seven existing demonstrations all exit zero. The new integration witness
composes Phase 4 → Phase 5/Rung 1 → Phase 6 → Phase 7 → Phase 8 in one process:

```bash
clojure -M:test \
  -n futon3c.peripheral.posthoc-system-verification-test
```

Result: 1 test, 21 assertions, zero failures/errors.

Component suites:

- Futon3c: 31 tests / 211 assertions, green.
- Futon2 memory/selection contracts: 43 tests / 172 assertions, green.

Frozen results reproduced:

- Rung 1: fixed hit@1 `0`, typed hit@1 `1`, same three candidates.
- Rung 2: one independently witnessed failure scales only R5 by `6/7` and
  recovers the held-out target.
- Rung 3: three facets, two warranted transitions, seven evidence paths,
  three challenge memories, and an explicit R5 budget hole.
- Phase 6: Brier `0.139` vs `0.270`, log loss `0.447` vs `0.736`,
  support+outcome `4/4` vs additive `2/4`.
- Phase 7: identities/provenance/candidate set preserved; `4/4` agreement
  with frozen independent review labels.
- Phase 8: recommendation carries two full memory reasons, `E_S`, `G_S`,
  hard support, and provenance.

### What does not work

Current live probes of:

- `p4ng/R9-independent-witness`
- `p4ng/R6-candidate-pattern-action-space`
- `p4ng/R5-policy-evaluation`
- `p4ng/R10-liveness`

returned zero WM memories and zero candidates.

The shared substrate itself is healthy:

- the mathematics control endpoint returned five current reviewed bodies;
- `p4ng/R15` returned live WM memory
  `e-9d36c1b0-5f07-4a44-bfb7-b23825b6ee4b`.

That R15 record is reviewed as an attachment but remains
`:witness-status :self-asserted`; it is not independent outcome evidence.

The Phase 5–8 demonstrations currently inject
`phase4-wm-corpus.edn` through a fixture `recall-fn`. They do not demonstrate a
live memory-to-selection path.

### Calibration boundary

Rung 2/Phase 6 has 13 training observations against a required minimum of 20.
Its mandatory result is:

```clojure
{:advance? false
 :decision-reason :exploratory-sample-too-small}
```

Do not relabel the exploratory entropy or outcome values as calibrated
probabilities. Also do not use this refusal as an excuse to route around all
new machinery with the additive controller. Report calibrated and exploratory
terms separately.

## Work packet A — first-class attachment review

There is no proper review operation. Earlier live attachments were promoted by
manually reposting hyperedges. Build an explicit operation, probably beside
`memory_lifecycle.clj`, with a contract such as:

```clojure
(review-attachment!
 ctx
 {:memory-id ...
  :review-evidence-id ...
  :verdict :approve
  :pattern-ids [...]})
```

Required behavior:

1. Fetch the current `:memory/assert` edge and its evidence body.
2. Fetch the review evidence entry.
3. Require the review evidence to:
   - name the memory and exact pattern attachment;
   - carry an approval/challenge verdict;
   - have a reviewer author distinct from the memory author;
   - carry provenance and time;
   - never be the memory author's own assertion.
4. On approval, repost the edge with:
   - `:attachment-status :reviewed`;
   - explicit review evidence/reviewer provenance;
   - an earned witness status only when the evidence supports that status.
5. On challenge/rejection, preserve the episode but do not admit the
   attachment.
6. Exact retries must be idempotent. A changed verdict must not be hidden as a
   replay.
7. No direct compact-memory fixture import and no mutation of the evidence
   body.

Tests must cover author/reviewer collision, missing review evidence, pattern
mismatch, stale/retracted memory, exact replay, changed verdict, wrong domain,
and successful recall after approval.

Do not invent an independent reviewer. The agent that authors the controller
episode cannot certify it. Arrange a distinct checker identity or leave the
record proposed.

## Work packet B — minimal live WM verification corpus

Write through `wm-memory/record-episode!`, not directly to Futon1b tables.

Create the smallest corpus that exercises the Phase 5 graph:

| Pattern | Required live episode |
|---|---|
| R9 independent witness | independently checked test/trace evidence |
| R6 candidate action space | the current post-hoc retrieval/integration finding |
| R5 policy evaluation | the one-outcome `6/7` update and its limitations |
| R10 liveness | current scheduler/endpoint liveness plus a visible challenge or blocker |

Each record must have:

- domain `:war-machine`;
- mission and pattern endpoints;
- observation/intervention kind;
- concrete full body;
- author/session/provenance;
- separate review evidence;
- reviewed attachment;
- honest witness status.

Use fresh ids. Do not overwrite the historical fixture ids.

The challenging/blocking records should remain visible in the projection; a
positive-only corpus is not an acceptable demonstration.

## Work packet C — live-store end-to-end VERIFY

Rerun the complete chain without injecting `:recall-fn`.

The trace must prove:

1. the real shared endpoint reader returns full bodies for R9/R6/R5/R10;
2. domain, lifecycle, attachment, and witness filters are active;
3. the Phase 5 admissible set is non-empty;
4. blockers and challenges remain visible;
5. fixed and typed rankings contain exactly the same candidate ids;
6. relation contributions cite real live memory ids;
7. path diversity and query budget are explicit;
8. Rung 2 consumes at most one separately witnessed outcome per update;
9. Phase 6 calibration/sample status remains explicit;
10. Phase 7 policy identity, `E_S`, `G_S`, support, memories, and provenance
    survive composition;
11. the final trace contains a concrete selected strategic policy/mission,
    not `nil`, `advice-only`, or a request for operator override;
12. no action outside the Phase 1–4 admissible subgraph can appear.

Freeze the live result as a new evidence artifact; do not edit the old replay
fixtures to make it pass.

## Work packet D — repair the live selection boundary

Only after packet C passes:

1. Make the verified strategic result the authoritative strategic selection.
2. Keep these as named counterfactuals:
   - fixed endpoint order;
   - additive/controller ranking;
   - tactical/scheduler habit ranking.
3. Carry real memory ids, typed relation contributions, budget, paths,
   blockers, calibration status, `E_S`, and `G_S` into
   `judgement.decision`.
4. Actual abstention belongs only to downstream act/completion gates.
5. Do not request Joe to approve or override an `abstain`.
6. Do not silently let tactical `E_T` become strategic `E_S`.
7. Do not present exploratory Phase 6 values as calibrated.

### Remove the two temporary bypasses

Futon2 commit `191e168` forced controller-head authority and froze scheduler
habit as counterfactual-only. Replace that bypass with the verified strategic
selection; retain only useful trace fields.

Futon3c commit `ce9aef4` added an independent presentation re-ranker. It
currently admits post-selection `:held-for-arming?` cascades whose placeholder
score `0.0` can outrank the true decision. Remove this recomputation.

The UI projection should display the authoritative `judgement.decision` and
its counterfactuals. It must never independently manufacture another winner.

## Acceptance trace

Before hand-off completion, produce one trace containing:

```clojure
{:candidate-domain ...               ; exact admitted ids
 :live-memory-ids [...]
 :fixed-ranking [...]
 :typed-ranking [...]
 :outcome-conditioned-ranking [...]
 :strategic-policy-ranking [...]
 :selected-policy-id "pi-s-..."
 :selected-mission-ids [...]
 :relation-contributions [...]
 :path-diversity {...}
 :budget {...}
 :blockers [...]
 :calibration {:status ... :sample-count ... :minimum ...}
 :counterfactuals
 {:fixed ...
  :additive-controller ...
  :scheduler-habit ...}
 :actuation
 {:status :pending-downstream-gates
  :authorized? false}}
```

The live HTTP representation and `judgement.decision` must agree on the same
selected policy and mission.

Required negative traces:

- remove one review → its memory cannot admit a mission;
- retract one warrant → its support disappears;
- remove a transition warrant → the child facet is ineligible;
- inject a misleading seed → one witnessed failure recovers or records a
  reasoned non-recovery;
- insert a held cascade with placeholder score `0.0` → it cannot become the
  displayed winner;
- no-op may be compared, but cannot suppress a valid strategic
  recommendation.

## Test and operational gates

- Focused existing suites must remain green.
- Add integration coverage for the real review operation and the unified
  decision/UI projection.
- `clj-kondo`: zero errors/warnings on changed Clojure.
- `git diff --check`: clean.
- Use Drawbridge for Futon3c hot-loading; do not restart the JVM.
- Do not restart Futon1b unless a server-side code change truly requires it.
- Record all live writes and review evidence ids.
- Commit and push Futon2 and Futon3c separately.
- Report unrelated repository-wide failures rather than editing them.
- Do not start the live War Machine click in this packet.

## Completion report

Return:

1. commit SHAs and branches;
2. exact live memory, review, and receipt ids;
3. tests/assertions and lint results;
4. live endpoint latency and audit counts;
5. the authoritative selection plus all three counterfactuals;
6. calibration/sample status;
7. negative-injection results;
8. rollback instructions;
9. confirmation that no click was started.

If the real live chain still returns an empty frontier, stop and report the
first failed seam. Do not substitute the additive controller and call the
verification successful.
