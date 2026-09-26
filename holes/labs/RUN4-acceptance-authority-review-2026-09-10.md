# RUN4 acceptance uses durable evidence, not the display

Codex-17, 2026-09-10. Independent acceptance-report tests pass 3/15 at
29389e51. Accept 5b60db89/29389e51 as removal of false-green checks, not as a
completed acceptance bridge.

The reported missing full-loop ID in visibility is not a missing producer
identity. run4_terminal_evidence/read-terminal-evidence already verifies the
admission-request series/trial/pin identity against the projection, joins its
click/run binding, and validates the full-loop run record against the projection
source digest. read-run-record! returns that validated record; the public reader
currently discards it and returns only classification. The display must not
become the authority merely because the report initially used it as input.

Next implementation is a public validated evidence bundle from the same shared
read path, retaining captured source pins and full-loop run ID/route. Existing
terminal port remains a classification projection. Acceptance uses the bundle
and the existing U49 route classifier/conformance rules, plus actual shared
step-acceptance battery evidence. The old generator's output must stay stable.
U49 currently classifies drawn, measured, ruling-unrealised, excluded dependency,
refutation and unmapped hops; preserve those distinctions and its nonempty-route
requirements. A route carrier conversion must check continuity without silently
dropping hops. No ledger deposit is authorized by this preparation.

Similarly, not-attempted visibility should reuse an exposed validated controller
lifecycle view for busy admission/predecessor stops, not an incomplete duplicate.
No new model choice or operator input is required to expose existing verified
records. No live run, registry, ledger or service operation occurred here.
