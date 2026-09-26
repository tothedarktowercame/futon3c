# RUN4 serving step: bounded independent review

Codex-17, 2026-09-10. Reviewed 767d7e66 (dispatch author projection/join)
and 951b6821 (disabled series HTTP step). Independently passed terminal-evidence
7 tests/23 assertions and series-service 3 tests/19 assertions. The service tests
exercise the HTTP handler, trusted pin preparation and durable controller with
click stubbed. They do not prove actual async runner recording alignment.

Accepted scope: explicit disabled-by-default one-transition capability, existing
single-flight click boundary, strict terminal consumer, no inferred completion.
Not accepted as deployment-complete RUN4 wiring.

Concrete next joins: trusted-entry prepared-options currently carries casting,
pin text, read ports and attestation callback, but not the pinned config's run
options. Series configured binding/projection/run-record roots currently guide
the reader, while runner-service writes through its dynamic binding/projection
directories and full-loop uses its run-record-dir option/default. Hashing config
bytes and naming reader roots alone do not establish their consumption.

Requested Codex-10 implement server-owned validated config and recording-root
pass-through into the actual async worker, preserving absence/false semantics.
Required proof: disposable async producer artifacts are read by the same strict
consumer, and accepted terminal evidence advances the controller without another
dispatch. Mutated config or mismatched roots must refuse before dispatch. No
ambient/default run can stand in for the pinned RUN4 configuration.

No live endpoint, credential, service reload, mission activation or run was
performed. Existing unrelated dirty files remain untouched. The next continuation
covers this join, then visibility/acceptance and final eligible-task freeze.
