# Joining dispatches to the attachment snapshot

For an offered dispatch receipt, take `evidence/body.job-id`, `evidence/at`, and the surfaced memory ids. Join each surfaced id to `attachments.memory-id`; each matching row supplies its endpoint and the endpoint's snapshot density from `pattern-aggregates`.

This is a snapshot join, not an as-of reconstruction. For a dispatch before the watermark, `staleness-bounds-seconds.lower=0` and `.upper` is the elapsed dispatch-to-snapshot interval. `reviewed-by-dispatch=true` only proves that the recorded review timestamp is no later than the dispatch; it does not prove that the edge remained unchanged throughout the interval. A causal cohort should export at dispatch time or use a future bitemporal as-of export.

## Worked examples (verbatim artifact records)

### `invoke-1785449302666-400-2956dd68`

```json
{
  "dispatch-at": "2026-07-30T22:08:23.367530271Z",
  "job-id": "invoke-1785449302666-400-2956dd68",
  "matched-attachment-rows": [
    {
      "asserted-at": "2026-07-29T07:42:38.256437543Z",
      "asserted-at-source": "memory-evidence-entry",
      "attachment-status": "reviewed",
      "dispatch-surfacing-via": "content-match",
      "edge-id": "hx-codexpilot-bridge-radial-integrand-order-with-pointwise-commutativity",
      "edge-state": "current",
      "endpoint-density-at-snapshot": 20,
      "memory-id": "e-codexpilot-bridge-radial-integrand-order-with-pointwise-commutativity",
      "pattern-id": "math-formalization/tactic-algebra-interference",
      "review-evidence-id": "e-review-claude9-earlier-bridge-radial-integrand-order-with-pointwise-commutativity",
      "review-verdict": "approve",
      "reviewed-at": "2026-07-30T17:00:09.542740Z",
      "reviewed-by-dispatch": true,
      "reviewer": "claude-9",
      "system-time": "2026-07-30T17:00:09.542740Z"
    },
    {
      "asserted-at": "2026-07-28T13:23:46.923559991Z",
      "asserted-at-source": "memory-evidence-entry",
      "attachment-status": "reviewed",
      "dispatch-surfacing-via": "content-match",
      "edge-id": "hx-codexpilot-prove-eLpNorm-translation-through-the-underlying-lintegral",
      "edge-state": "current",
      "endpoint-density-at-snapshot": 20,
      "memory-id": "e-codexpilot-prove-eLpNorm-translation-through-the-underlying-lintegral",
      "pattern-id": "math-formalization/tactic-algebra-interference",
      "review-evidence-id": "e-review-claude9-earlier-prove-eLpNorm-translation-through-the-underlying-lintegral",
      "review-verdict": "approve",
      "reviewed-at": "2026-07-30T17:00:09.542740Z",
      "reviewed-by-dispatch": true,
      "reviewer": "claude-9",
      "system-time": "2026-07-30T17:00:09.542740Z"
    },
    {
      "asserted-at": "2026-07-30T10:06:08.109697023Z",
      "asserted-at-source": "memory-evidence-entry",
      "attachment-status": "reviewed",
      "dispatch-surfacing-via": "content-match",
      "edge-id": "hx-codexpilot-apply-radial-R3-integration-through-a-one-dimensional-profile",
      "edge-state": "current",
      "endpoint-density-at-snapshot": 30,
      "memory-id": "e-codexpilot-apply-radial-R3-integration-through-a-one-dimensional-profile",
      "pattern-id": "math/measure-integration-api",
      "review-evidence-id": "e-review-codex-5-apply-radial-R3-integration-through-a-one-dimensional-profile",
      "review-verdict": "approve",
      "reviewed-at": "2026-07-30T17:26:31.232876857Z",
      "reviewed-by-dispatch": true,
      "reviewer": "codex-5",
      "system-time": "2026-07-30T17:26:31.232876857Z"
    },
    {
      "asserted-at": "2026-07-29T07:42:39.580901025Z",
      "asserted-at-source": "memory-evidence-entry",
      "attachment-status": "reviewed",
      "dispatch-surfacing-via": "pattern",
      "edge-id": "hx-codexpilot-avoid-euclidean-measurable-space-diamond-by-direct-general-instantiation",
      "edge-state": "current",
      "endpoint-density-at-snapshot": 30,
      "memory-id": "e-codexpilot-avoid-euclidean-measurable-space-diamond-by-direct-general-instantiation",
      "pattern-id": "math/measure-integration-api",
      "review-evidence-id": "e-review-claude9-earlier-avoid-euclidean-measurable-space-diamond-by-direct-general-instantiation",
      "review-verdict": "approve",
      "reviewed-at": "2026-07-30T17:00:09.542740Z",
      "reviewed-by-dispatch": true,
      "reviewer": "claude-9",
      "system-time": "2026-07-30T17:00:09.542740Z"
    },
    {
      "asserted-at": "2026-07-26T21:25:00.136975313Z",
      "asserted-at-source": "memory-evidence-entry",
      "attachment-status": "reviewed",
      "dispatch-surfacing-via": "content-match",
      "edge-id": "hx-mem-ca13d43b-128b-4ca3-a1c9-89ab32584ef6",
      "edge-state": "current",
      "endpoint-density-at-snapshot": 5,
      "memory-id": "e-ca13d43b-128b-4ca3-a1c9-89ab32584ef6",
      "pattern-id": "math/series-evaluation-api",
      "review-evidence-id": "e-review-math-pudding-v2-translate-interval-integrals-with-integral-comp-add-right",
      "review-verdict": "approve",
      "reviewed-at": "2026-07-26T21:38:25.245831173Z",
      "reviewed-by-dispatch": true,
      "reviewer": "joe",
      "system-time": "2026-07-26T21:38:25.245831173Z"
    }
  ],
  "offered-evidence-id": "e-f91e7d00-8772-4b3d-ba0b-7dd1265ddc47",
  "snapshot-lag-seconds": 310133,
  "staleness-bounds-seconds": {
    "lower": 0,
    "upper": 310133
  },
  "surfaced-memory-ids": [
    "e-codexpilot-apply-radial-R3-integration-through-a-one-dimensional-profile",
    "e-codexpilot-bridge-radial-integrand-order-with-pointwise-commutativity",
    "e-ca13d43b-128b-4ca3-a1c9-89ab32584ef6",
    "e-codexpilot-prove-eLpNorm-translation-through-the-underlying-lintegral",
    "e-codexpilot-avoid-euclidean-measurable-space-diamond-by-direct-general-instantiation"
  ],
  "temporal-verdict": "snapshot-state-only-not-proof-of-state-at-dispatch"
}
```

### `invoke-1785464073951-441-1c05a75c`

```json
{
  "dispatch-at": "2026-07-31T02:14:34.764206330Z",
  "job-id": "invoke-1785464073951-441-1c05a75c",
  "matched-attachment-rows": [
    {
      "asserted-at": "2026-07-30T18:18:43.886837020Z",
      "asserted-at-source": "memory-evidence-entry",
      "attachment-status": "reviewed",
      "dispatch-surfacing-via": "pattern",
      "edge-id": "hx-codexpilot-bound-polynomial-sum-degree-by-a-common-summand-bound",
      "edge-state": "current",
      "endpoint-density-at-snapshot": 20,
      "memory-id": "e-codexpilot-bound-polynomial-sum-degree-by-a-common-summand-bound",
      "pattern-id": "math-formalization/tactic-algebra-interference",
      "review-evidence-id": "e-review-claude9-p23-bound-polynomial-sum-degree-by-a-common-summand-bound",
      "review-verdict": "approve",
      "reviewed-at": "2026-07-30T18:19:20.207272Z",
      "reviewed-by-dispatch": true,
      "reviewer": "claude-9",
      "system-time": "2026-07-30T18:19:20.207272Z"
    },
    {
      "asserted-at": "2026-07-28T13:23:45.419415402Z",
      "asserted-at-source": "memory-evidence-entry",
      "attachment-status": "reviewed",
      "dispatch-surfacing-via": "content-match",
      "edge-id": "hx-codexpilot-derive-integrable-from-nonzero-bochner-integral",
      "edge-state": "current",
      "endpoint-density-at-snapshot": 20,
      "memory-id": "e-codexpilot-derive-integrable-from-nonzero-bochner-integral",
      "pattern-id": "math-formalization/tactic-algebra-interference",
      "review-evidence-id": "e-review-claude9-earlier-derive-integrable-from-nonzero-bochner-integral",
      "review-verdict": "approve",
      "reviewed-at": "2026-07-30T17:00:09.542740Z",
      "reviewed-by-dispatch": true,
      "reviewer": "claude-9",
      "system-time": "2026-07-30T17:00:09.542740Z"
    },
    {
      "asserted-at": "2026-07-30T21:32:10.217582060Z",
      "asserted-at-source": "memory-evidence-entry",
      "attachment-status": "reviewed",
      "dispatch-surfacing-via": "content-match",
      "edge-id": "hx-codexpilot-derive-the-unit-ball-volume-recursion-through-Wallis-parity-formulas",
      "edge-state": "current",
      "endpoint-density-at-snapshot": 30,
      "memory-id": "e-codexpilot-derive-the-unit-ball-volume-recursion-through-Wallis-parity-formulas",
      "pattern-id": "math/measure-integration-api",
      "review-evidence-id": "e-review-codexpilot-derive-the-unit-ball-volume-recursion-through-Wallis-parity-formulas",
      "review-verdict": "approve",
      "reviewed-at": "2026-07-30T21:32:11.507318864Z",
      "reviewed-by-dispatch": true,
      "reviewer": "claude-9",
      "system-time": "2026-07-30T21:32:11.507318864Z"
    },
    {
      "asserted-at": "2026-07-29T07:42:35.869699666Z",
      "asserted-at-source": "memory-evidence-entry",
      "attachment-status": "reviewed",
      "dispatch-surfacing-via": "content-match",
      "edge-id": "hx-codexpilot-normalize-r3-radial-coefficient-to-four-pi",
      "edge-state": "current",
      "endpoint-density-at-snapshot": 30,
      "memory-id": "e-codexpilot-normalize-r3-radial-coefficient-to-four-pi",
      "pattern-id": "math/measure-integration-api",
      "review-evidence-id": "e-review-claude9-earlier-normalize-r3-radial-coefficient-to-four-pi",
      "review-verdict": "approve",
      "reviewed-at": "2026-07-30T17:00:09.542740Z",
      "reviewed-by-dispatch": true,
      "reviewer": "claude-9",
      "system-time": "2026-07-30T17:00:09.542740Z"
    },
    {
      "asserted-at": "2026-07-28T13:23:48.237378710Z",
      "asserted-at-source": "memory-evidence-entry",
      "attachment-status": "reviewed",
      "dispatch-surfacing-via": "content-match",
      "edge-id": "hx-codexpilot-reduce-probability-kernel-L2-contraction-to-young",
      "edge-state": "current",
      "endpoint-density-at-snapshot": 7,
      "memory-id": "e-codexpilot-reduce-probability-kernel-L2-contraction-to-young",
      "pattern-id": "math/proof-architecture",
      "review-evidence-id": "e-review-claude9-earlier-reduce-probability-kernel-L2-contraction-to-young",
      "review-verdict": "approve",
      "reviewed-at": "2026-07-30T17:00:09.542740Z",
      "reviewed-by-dispatch": true,
      "reviewer": "claude-9",
      "system-time": "2026-07-30T17:00:09.542740Z"
    }
  ],
  "offered-evidence-id": "e-dc72d439-2010-432a-9098-81e40355d851",
  "snapshot-lag-seconds": 295361,
  "staleness-bounds-seconds": {
    "lower": 0,
    "upper": 295361
  },
  "surfaced-memory-ids": [
    "e-codexpilot-derive-integrable-from-nonzero-bochner-integral",
    "e-codexpilot-derive-the-unit-ball-volume-recursion-through-Wallis-parity-formulas",
    "e-codexpilot-normalize-r3-radial-coefficient-to-four-pi",
    "e-codexpilot-reduce-probability-kernel-L2-contraction-to-young",
    "e-codexpilot-bound-polynomial-sum-degree-by-a-common-summand-bound"
  ],
  "temporal-verdict": "snapshot-state-only-not-proof-of-state-at-dispatch"
}
```

### `invoke-1785473298737-474-6e1af56a`

```json
{
  "dispatch-at": "2026-07-31T04:48:19.789546160Z",
  "job-id": "invoke-1785473298737-474-6e1af56a",
  "matched-attachment-rows": [
    {
      "asserted-at": "2026-07-28T13:23:45.419415402Z",
      "asserted-at-source": "memory-evidence-entry",
      "attachment-status": "reviewed",
      "dispatch-surfacing-via": "content-match",
      "edge-id": "hx-codexpilot-derive-integrable-from-nonzero-bochner-integral",
      "edge-state": "current",
      "endpoint-density-at-snapshot": 20,
      "memory-id": "e-codexpilot-derive-integrable-from-nonzero-bochner-integral",
      "pattern-id": "math-formalization/tactic-algebra-interference",
      "review-evidence-id": "e-review-claude9-earlier-derive-integrable-from-nonzero-bochner-integral",
      "review-verdict": "approve",
      "reviewed-at": "2026-07-30T17:00:09.542740Z",
      "reviewed-by-dispatch": true,
      "reviewer": "claude-9",
      "system-time": "2026-07-30T17:00:09.542740Z"
    },
    {
      "asserted-at": "2026-07-29T09:41:48.735988708Z",
      "asserted-at-source": "memory-evidence-entry",
      "attachment-status": "reviewed",
      "dispatch-surfacing-via": "content-match",
      "edge-id": "hx-codexpilot-poisson-ae-convergence-bridge",
      "edge-state": "current",
      "endpoint-density-at-snapshot": 40,
      "memory-id": "e-codexpilot-poisson-ae-convergence-bridge",
      "pattern-id": "math/missing-dependency-protocol",
      "review-evidence-id": "e-review-claude9-earlier-poisson-ae-convergence-bridge",
      "review-verdict": "approve",
      "reviewed-at": "2026-07-30T17:00:09.542740Z",
      "reviewed-by-dispatch": true,
      "reviewer": "claude-9",
      "system-time": "2026-07-30T17:00:09.542740Z"
    },
    {
      "asserted-at": "2026-07-26T11:34:37.093283048Z",
      "asserted-at-source": "memory-evidence-entry",
      "attachment-status": "reviewed",
      "dispatch-surfacing-via": "pattern",
      "edge-id": "hx-mem-1ac936fb-04e8-460e-a710-37fac474401c",
      "edge-state": "current",
      "endpoint-density-at-snapshot": 7,
      "memory-id": "e-1ac936fb-04e8-460e-a710-37fac474401c",
      "pattern-id": "math/proof-architecture",
      "review-evidence-id": "e-review-math-symmetric-interval-law-to-dyadic-differentiation",
      "review-verdict": "approve",
      "reviewed-at": "2026-07-26T11:57:12.797163791Z",
      "reviewed-by-dispatch": true,
      "reviewer": "joe",
      "system-time": "2026-07-26T11:57:12.797163791Z"
    },
    {
      "asserted-at": "2026-07-26T21:24:52.484557472Z",
      "asserted-at-source": "memory-evidence-entry",
      "attachment-status": "reviewed",
      "dispatch-surfacing-via": "pattern",
      "edge-id": "hx-mem-4adf0546-2bed-4f35-93f1-032cd254177f",
      "edge-state": "current",
      "endpoint-density-at-snapshot": 7,
      "memory-id": "e-4adf0546-2bed-4f35-93f1-032cd254177f",
      "pattern-id": "math/proof-architecture",
      "review-evidence-id": "e-review-math-pudding-v2-tendsto-in-measure-ae-subsequence-api",
      "review-verdict": "approve",
      "reviewed-at": "2026-07-26T21:37:17.271697839Z",
      "reviewed-by-dispatch": true,
      "reviewer": "joe",
      "system-time": "2026-07-26T21:37:17.271697839Z"
    },
    {
      "asserted-at": "2026-07-28T13:23:48.237378710Z",
      "asserted-at-source": "memory-evidence-entry",
      "attachment-status": "reviewed",
      "dispatch-surfacing-via": "content-match",
      "edge-id": "hx-codexpilot-reduce-probability-kernel-L2-contraction-to-young",
      "edge-state": "current",
      "endpoint-density-at-snapshot": 7,
      "memory-id": "e-codexpilot-reduce-probability-kernel-L2-contraction-to-young",
      "pattern-id": "math/proof-architecture",
      "review-evidence-id": "e-review-claude9-earlier-reduce-probability-kernel-L2-contraction-to-young",
      "review-verdict": "approve",
      "reviewed-at": "2026-07-30T17:00:09.542740Z",
      "reviewed-by-dispatch": true,
      "reviewer": "claude-9",
      "system-time": "2026-07-30T17:00:09.542740Z"
    }
  ],
  "offered-evidence-id": "e-e2dc77cc-1e46-4c27-9f70-428b533b389d",
  "snapshot-lag-seconds": 286136,
  "staleness-bounds-seconds": {
    "lower": 0,
    "upper": 286136
  },
  "surfaced-memory-ids": [
    "e-codexpilot-reduce-probability-kernel-L2-contraction-to-young",
    "e-codexpilot-poisson-ae-convergence-bridge",
    "e-codexpilot-derive-integrable-from-nonzero-bochner-integral",
    "e-1ac936fb-04e8-460e-a710-37fac474401c",
    "e-4adf0546-2bed-4f35-93f1-032cd254177f"
  ],
  "temporal-verdict": "snapshot-state-only-not-proof-of-state-at-dispatch"
}
```
