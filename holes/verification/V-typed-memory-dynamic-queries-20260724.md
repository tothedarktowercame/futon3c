# Post-hoc VERIFY — typed memories and dynamic queries

Date: 2026-07-24  
Verifier: codex-4  
Scope: `M-typed-memories`, `E-dynamic-queries`, and Phases 4–8 of
`M-shared-memory-control-build-test`

## Verdict

**The frozen mechanism chain works as specified. The live War Machine chain
does not yet work end to end.**

This distinction blocks promotion. The replay demonstrates that the typed
operators, outcome update, facet budget, strategic policy identity, and
reason-bearing recommendation compose. A current live-store probe demonstrates
that the four Phase 5 p4ng endpoints have no WM memory bodies, so the same
chain cannot currently be constructed from the live shared substrate.

## Claim matrix

| Claim | Evidence | Result |
|---|---|---|
| Shared endpoint recall is operational | Current mathematics endpoint returned five reviewed current bodies; current `p4ng/R15` returned one reviewed WM body | PASS |
| Rung 1 preserves the candidate set and improves the frozen held-out hit@1 | Fixed hit@1 `0`; typed hit@1 `1`; three candidates preserved | PASS, replay |
| Phase 5 produces a reason-bearing frontier | Three typed paths, two relation types, explicit budget, blocker, dependency and holes | PASS, replay |
| Rung 2 performs exactly one outcome-conditioned operator update | Independently witnessed failure scales only R5 by `6/7`; frozen gold moves to rank 1 | PASS, replay |
| Rung 2 has earned promotion | Sample count `13`, required `20`; `promotion-eligible? false` | FAIL as required by the design |
| Rung 3 refines facets under a budget without treating memory multiplicity as evidence | Three selected patterns, two warranted transitions, seven evidence paths, three challenge memories, R5 left as budget hole | PASS, replay |
| Phase 6 improves the named point-estimate baseline | Brier `0.139` vs `0.270`; log loss `0.447` vs `0.736`; support+outcome accuracy `4/4` vs additive `2/4` | PASS, exploratory replay |
| Phase 6 is sufficiently calibrated for promotion | Six held-out outcomes and four judgements, wide intervals; `advance? false` | FAIL as required by the design |
| Phase 7 preserves policy identity, provenance, memory reasons, `E_S`, and `G_S` | Candidate/provenance/identity checks pass; reviewed agreement `4/4`; three reviewed disagreements with additive baseline | PASS, replay |
| Phase 8 composes a concrete reason-bearing recommendation | Two-mission policy carries `e-wm-eig-support` and `e-wm-memory-support`, typed relations, `E_S`, and `G_S` | PASS, replay |
| Phase 5–8 can run from current live WM endpoint memories | `R9`, `R6`, `R5`, and `R10` each returned zero WM memories and zero candidates | **FAIL — live integration gap** |

## Executable post-hoc witness

`test/futon3c/peripheral/posthoc_system_verification_test.clj` composes the
frozen Phase 4 corpus through:

1. Phase 5 outer frontier;
2. Rung 1 typed checkpoint;
3. Phase 6 outcome ablation;
4. Phase 7 strategic-policy window;
5. Phase 8 reason-bearing recommendation.

It asserts the positive mechanism claims and the mandatory non-promotion
claims in one process.

Command:

```bash
clojure -M:test \
  -n futon3c.peripheral.posthoc-system-verification-test
```

Result: **1 test, 21 assertions, zero failures/errors**.

The component suite was also rerun:

```bash
clojure -M:test \
  -n futon3c.peripheral.dynamic-queries-test \
  -n futon3c.peripheral.wm-memory-test \
  -n futon3c.peripheral.strategic-cascade-test \
  -n futon3c.peripheral.strategic-outcomes-test \
  -n futon3c.peripheral.strategic-policies-test \
  -n futon3c.peripheral.strategic-canary-test
```

Result: **31 tests, 211 assertions, zero failures/errors**.

The shared Futon2 contract/selection suite returned **43 tests, 172
assertions, zero failures/errors**.

All seven demonstrations completed with exit zero:

- `run_dynamic_queries_demo.clj`
- `run_phase5_strategic_cascade_demo.clj`
- `run_dynamic_queries_rung2_demo.clj`
- `run_dynamic_queries_rung3_demo.clj`
- `run_phase6_strategic_outcomes_demo.clj`
- `run_phase7_strategic_policy_shadow.clj`
- `run_phase8_advice_only_canary.clj`

## Current live-store probes

### Shared mathematics control

Endpoint:
`math-formalization/tactic-algebra-interference`

Result:

- seven matching edges;
- five current reviewed bodies returned;
- two lifecycle-excluded;
- projection lookup `0.055 ms`;
- service total `56.75 ms`;
- caller wall `62.61 ms`.

This verifies that the shared writer/edge/projection/body-hydration machinery
is operational in the current store.

### Existing WM control

Endpoint: `p4ng/R15`

Result:

- one current reviewed body,
  `e-9d36c1b0-5f07-4a44-bfb7-b23825b6ee4b`;
- full body hydrated;
- witness status remains `self-asserted`;
- service total `34.70 ms`;
- caller wall `37.54 ms`.

This verifies the WM domain uses the same live retrieval seam, but this record
is not independently witnessed value evidence.

### Phase 5 cascade endpoints

Endpoints:

- `p4ng/R9-independent-witness`
- `p4ng/R6-candidate-pattern-action-space`
- `p4ng/R5-policy-evaluation`
- `p4ng/R10-liveness`

Result:

- zero matching edges at every endpoint;
- zero hydrated bodies;
- zero admitted missions;
- empty use receipt;
- initial cold projection call `7.49 s`, subsequent calls `88–110 ms`.

Therefore the Phase 5–8 replay currently depends on
`phase4-wm-corpus.edn`; it is not a projection of the live store.

## What this verification falsifies

It is not currently justified to say that:

- strategic memory influenced the live War Machine recommendation;
- dynamic queries produced the live mission order;
- Phase 7 `E_S/G_S` selected the live action;
- the replay corpus is already present as reviewed live memories; or
- a fixture success authorizes promotion.

The controller-head bypass and the contradictory UI re-ranking introduced in
commits `191e168` and `ce9aef4` do not repair this integration gap.

## Required gate before live authority

1. Add a first-class attachment-review operation. It must persist the review
   act, verify reviewer/author separation, and then project
   `:attachment-status :reviewed` plus the earned witness status.
2. Record a minimal current WM corpus through `wm-memory/record-episode!`;
   do not import fixture-shaped compact memories directly.
3. Independently review the R9/R6/R5/R10 attachments.
4. Recall full bodies from the live store and rerun the same integration
   witness without an injected `recall-fn`.
5. Require a non-empty reason-bearing frontier, fixed and adaptive rankings,
   memory IDs, relation contributions, budget/path diversity, and explicit
   calibration status in the resulting decision trace.
6. Only then decide which verified model is authoritative. The existing
   sample-size refusal must not be relabelled as calibration merely to turn a
   feature on.

Until this gate passes, the accurate status is:

- shared typed memory substrate: **live and working**;
- dynamic/strategic mechanism: **working on frozen replay**;
- live WM memory-to-selection composition: **not yet working**.
