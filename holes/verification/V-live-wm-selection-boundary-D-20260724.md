# Packet D — live War Machine selection boundary

Date: 2026-07-24

Status: accepted live projection; no War Machine click started

Evidence receipt: `e-47c4e886-64bc-4e02-801b-0b8306916641`

Packet D replaces the controller-head bypass with the reviewed
reason-bearing strategic selector and removes the independent presentation
re-ranker. The selector is authoritative on chain-integrity and auditability
grounds inside the unchanged Phase 1–4 admissible set. This is not a
demonstrated-better-selection claim. Rung 1 relation weights remain
illustrative; earned semantics still require at least 20 independently
witnessed live transitions.

## Live agreement

Scheduler tick 44 completed at `2026-07-24T20:25:47.067930042Z` in
211717 ms. The cached `judgement.decision` and `next-move-live` projection
agree:

- policy: `pi-s-9dbc2ceb3317bc38050c41ce`;
- selected missions: `M-shared-memory-control-build-test`, then
  `M-aif-policy-conditioned-eig`;
- displayed target: `M-shared-memory-control-build-test`;
- selected memories: the independently reviewed R5 and R6 records;
- path diversity: three paths, three patterns, two relation types;
- budget: 4/4 consumed;
- calibration: 13/20, `:exploratory-sample-too-small`;
- actuation: `:pending-downstream-gates`, unauthorized and unexecuted.

Counterfactual rankings remained inside the same three candidates:

1. fixed: EIG, shared-memory, compliance;
2. additive controller: EIG, shared-memory, compliance;
3. scheduler habit: EIG, compliance, shared-memory.

One no-op remained in the live comparison set and did not suppress the
strategic recommendation. Presentation reports
`:source :judgement.decision` and performs no recomputation.

## Negative acceptance

All six required injections pass in
`live-wm-selection-negative-trace-20260724.edn`:

- review removal prevents admission;
- relation-warrant retraction removes support;
- transition-warrant removal makes the child facet ineligible;
- one witnessed failure recovers from the misleading seed;
- a held cascade with placeholder score `0.0` cannot win presentation;
- no-op cannot suppress the authoritative recommendation.

## Gates

- Futon3c: 45 tests, 300 assertions, zero failures/errors.
- Futon2: 65 tests, 284 assertions, zero failures/errors.
- `clj-kondo`: zero errors and zero warnings.
- `check-parens`: clean.
- EDN parsing and `git diff --check`: clean.
- Futon3c changes were hot-loaded with Drawbridge; no JVM was restarted.
- Futon1b was not restarted.

After the accepted tick, a later refresh encountered a transient
authoritative-store read failure and failed closed without overwriting tick
44. A subsequent refresh was still in flight when this evidence was frozen.
The last accepted decision therefore remains tick 44; no additive fallback
or presentation winner was substituted.
