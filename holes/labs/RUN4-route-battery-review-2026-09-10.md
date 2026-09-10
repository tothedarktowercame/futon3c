# RUN4 route battery review — 2026-09-10

Reviewed fe57ac9c and 455a7a6a independently. Acceptance tests 5/22 and battery
2/9 pass. The original /tmp/run4-duplicate-coverage-review.clj now returns
route-conformance false: visible/durable identity bijection correction accepted.

Battery scope is not accepted as recording completeness. The existing test
helper's bundle contains identity, digests, classification and a route, but no
realized-recording artifact. Invoking that helper with its green visibility and
matching source digests now returns all four checks true, decision
operator-decision-required, and missing-evidence []. This reproduces the scope
error without any live operation: the reader is stubbed exactly as in the
committed unit fixture. The four generated rows all concern routes. No row
checks recording, and no persisted battery is read.

Separately, run4-battery/validate calls step/advance-pin with run-id ::all.
step_acceptance.clj filters red rows by exact run-id; ordinary bundle rows use
actual string run IDs. Therefore this invocation exercises no actual run's
red-row rejection. The preceding every-green predicate prevents a red battery
from passing this component, but the claimed shared consumer exercise is
vacuous. Use each real run ID and a control proving shared-consumer rejection.

Required follow-up: distinguish route battery from recording evidence; derive
recording checks from the actual marked artifact and schema/identity validation,
keep first-trial absent measurements unknown, persist/read exact battery bytes
under server-owned roots, and demonstrate actual run-ID consumer behavior.
Do not report operator-decision-required while the recording obligation is
unverified. The separate controller lifecycle/visibility work remains unfinished.
No live service, store, pin or acceptance operation occurred during review.
