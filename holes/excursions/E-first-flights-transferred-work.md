# E-first-flights-transferred-work — close the residual first-flights obligations outside the mission

Date: 2026-07-06
Status: OPEN — commissioned closure tasks running

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

Ground-control task: independently re-check the proof and, if valid, record this
workstream as CLOSED in this Excursion.

### W2 — typed-grounds / return-channel tail

Question: does the remaining `arr-7535a5b6-e59` / typed-grounds tail close from
existing evidence, or should it remain open here as an Excursion-owned obligation?

Current evidence:

- `M-first-flights` checkpoint 22 says pretty-print discharged the rendering
  half, while grounds-in-prose remained design-hungry.
- The closure demo did not prove this tail closed; it only proved the mission
  should stop being the live target for it.

Ground-control task: independently census the typed-grounds tail, identify the
source of record for `arr-7535a5b6-e59`, and record CLOSED or HOLD here with
named evidence.

## Related work, not a dependency

`M-fold-ansatz` may benefit from the fold-realized / gamma evidence and any
typed-grounding representation that falls out of this Excursion. That relation
is informational. This Excursion must close or hold on its own evidence.
