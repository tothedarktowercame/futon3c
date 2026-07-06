# E-first-flights-transferred-work — close the residual first-flights obligations outside the mission

Date: 2026-07-06
Status: CLOSED — W1 closed; W2 closed

## HEAD

`M-first-flights` is closed/transferred as a War Machine mission target. Phase A
is complete and operator-reviewed. Residual work must not be hidden by shuttling
it into another mission as an implicit dependency.

This Excursion owns the residual first-flights closure work independently.
`M-fold-ansatz` is related context for fold-frontier mechanics, but this
Excursion is not blocked on `M-fold-ansatz` closing.

## Operator ruling

Joe, 2026-07-06:

> We should not only close/transfer the mission but also close the transferred
> work by belling that out. My only concern about M-fold-ansatz is that we don't
> want to just shuttle work around from mission to mission. Let's move anything
> that needs closing into Excursions and independently close them instead. We can
> make a note about the related work in M-fold-ansatz — and if it happens to
> close in the process, fine, but I don't think it's a dependency.

## Workstreams

### W1 — Phase-B policy-grade G(s, pi)

Question: is `:sorry/first-flights-phase-b-policy-grade-G` independently
closeable as `:addressed` by the fold-realized -> gamma calibration seam?

Current evidence:

- `futon2/holes/phase-b-transfer-proof-2026-07-06.md` says yes:
  `futon2.aif.fold-realized` produces `{:policy :expected-G :realized-G :tick}`
  with typed holes and recomputable coverage-to-delta-G.
- `futon2/test/fold_realized_zero_coverage_test.clj` passed in that proof.

Dispatch: `zai-5`, job `invoke-1783341988882-673-0d5a0f7c`, requested
2026-07-06 by `codex-1`.

Verdict: **CLOSED**, independently verified by `zai-5`.

Evidence note: `futon3c/holes/excursions/E-first-flights-policy-grade-G-closure.md`,
commit `b88a81f`.

Summary: `clojure -M test/fold_realized_zero_coverage_test.clj` passed 18/18
assertions; the scale-match invariant was independently checked; the
`:sorry/first-flights-phase-b-policy-grade-G` `:addressed` edit remains
justified.

### W2 — typed-grounds / return-channel tail

Question: does the remaining `arr-7535a5b6-e59` / typed-grounds tail close from
existing evidence, or should it remain open here as an Excursion-owned obligation?

Current evidence:

- `M-first-flights` checkpoint 22 says pretty-print discharged the rendering
  half, while grounds-in-prose remained design-hungry.
- The closure demo did not prove this tail closed; it only proved the mission
  should stop being the live target for it.

Dispatch: `zai-6`, job `invoke-1783342003563-674-0ed65444`, requested
2026-07-06 by `codex-1`.

Initial verdict: **HOLD**, independently verified by `zai-6`.

Evidence note: `futon3c/holes/excursions/E-first-flights-typed-grounds-tail-closure.md`,
commit `7b37d67`.

Summary: `arr-7535a5b6-e59` remains open because the `:typed-grounds` node in
`futon3c/holes/flights/first-flights-wiring.edn` is still
`:satiety {:hungry-for :design}`. Checkpoint 22 discharged the rendering half
only; the typed-grounds migration has no design, plan, or construction yet.
This hold lives here and does not block `M-fold-ansatz`.

Routing note (Joe, 2026-07-06): W2 is best understood as a follow-on action
from `M-action-vocabulary`, not as a fold/deposit/gamma blocker. The missing
work is vocabulary/design for how prose grounds become typed, queryable action
trace fields. It belongs at the trace/learning vocabulary boundary, not in
`M-fold-ansatz`.

Interactive closure (codex-1 + Joe, 2026-07-06): **CLOSED** at the producer /
spec / witness grain.

What changed:

- `futon3c.aif.flight-record` now emits typed maps for every ground the producer
  derives. Each map carries `:ground/kind` plus kind-specific witness fields.
- `holes/specs/flight.spec.edn` documents the ground vocabulary.
- `holes/specs/traces/flight-typed-ground-witness.edn` is a generated conforming
  witness record with typed grounds.
- `holes/flights/first-flights-wiring.edn` flips `:typed-grounds` from
  `{:hungry-for :design}` to `:full`.

Compatibility boundary: historical records and pre-wrapped pilot-supplied cells
may still carry legacy prose grounds. The closed obligation is the producer-side
typed-grounds design and construction for new records, not retroactive rewriting
of old traces.

## Related work, not a dependency

`M-fold-ansatz` may benefit from the fold-realized / gamma evidence and any
typed-grounding representation that falls out of this Excursion. That relation
is informational. This Excursion must close or hold on its own evidence.
