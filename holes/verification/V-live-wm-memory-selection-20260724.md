# Live War Machine memory/selection verification

Date: 2026-07-24
Status: Packet C ACCEPTED DARK; no War Machine click started

This is Packet C of
`CODEX-HANDOFF-live-wm-memory-selection-verify.md`: an end-to-end run from
the ordinary live store, without an injected recall function. It verifies
that the reason-bearing selector has an inspectable, fail-closed evidence
chain inside the unchanged Phase 1–4 admissible set. It does **not** promote
that selector into live mission selection; that is Packet D.

## Reviewed bootstrap records

| Facet | Memory | Independent review evidence |
|---|---|---|
| R9 | `e-4b9716ef-6df4-4e89-acc8-7394d06c177d` | `b4904003-6c63-4ffc-91fe-402ab55dc7a8` |
| R6 | `e-d13a4076-feb0-4906-b2b3-6a1d9e275692` | `b1cf276f-c4c0-4757-a00e-ae3f02e3ca00` |
| R5 | `e-7782c5f4-2a2b-465e-afcf-6c8947b144ba` | `03f524fb-690f-4c11-8c10-a8b20bc73cc2` |
| R10 liveness | `e-d6e04968-d665-403e-bf1d-7ad86cff2745` | `167f259f-22fb-4b5c-b87e-44527b014e86` |
| R10 latency blocker | `e-9026ee4c-5cc2-4e74-ae68-5dae873fd708` | `e57a0685-cf1f-4f80-a40b-ea4025d4de8e` |

All five attachments were approved by `claude-4` with
`:independently-witnessed` status. The R5 claim is deliberately narrower:
it independently witnesses a frozen replay result, not a live WM outcome.

## Authoritative trace

The machine admitted exactly:

- `M-aif-policy-conditioned-eig`
- `M-shared-memory-control-build-test`
- `M-wm-aif-policy-grain-compliance`

It selected policy `pi-s-9dbc2ceb3317bc38050c41ce`, then
`M-shared-memory-control-build-test` and
`M-aif-policy-conditioned-eig`. The selected evidence was the R5 and R6
memory pair. The trace contains three distinct paths, three patterns, two
relation types, and a fully consumed 4/4 budget. The R10 blocker excludes
`M-wm-tripwires`; its blocked dependency holds
`M-wm-aif-policy-grain-compliance`.

Packet C counterfactual rankings:

1. Fixed: EIG, shared-memory, compliance.
2. Additive controller: EIG, shared-memory, compliance.
3. Scheduler habit: compliance, EIG, shared-memory.

Calibration remains refused: 13 observations against a minimum of 20,
`:exploratory-sample-too-small`. The one-outcome replay recovers from its
misleading seed, but is not presented as calibrated probability or live
outcome evidence.

## Latency gate

The serving JVM's first R9 recall in the final pre-tick probe took
7035.73 ms; the complete selector took 7513.62 ms. An immediate cache
recheck took 547.95 ms total, with all four endpoints between 97.22 and
199.27 ms. Therefore a click must:

1. warm all four full-body endpoints in the serving JVM immediately before
   selection;
2. verify every endpoint remains at or below 1000 ms;
3. queue within 5000 ms while the projection revision is unchanged; and
4. block and repeat warm-up if eviction or revision change is observed.

No click was run in this packet.

## Gates

- Packet C focused suites: 29 tests, 221 assertions, zero failures/errors.
- Changed Clojure: `clj-kondo` zero errors and zero warnings.
- Changed Lisp/Clojure: `check-parens` clean.
- `git diff --check`: clean.
- Neither Futon3c nor Futon1b was restarted.

The frozen store-backed evidence receipt is
`e-f80e7837-4ca1-4130-8060-31c9d984ab8b`.

The machine-readable preregistration and result are:

- `holes/labs/M-typed-memories/live-wm-selection-input-20260724.edn`
- `holes/labs/M-typed-memories/live-wm-selection-trace-20260724.edn`
