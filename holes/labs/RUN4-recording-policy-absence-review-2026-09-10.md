# Selected-policy absence review — 2026-09-10

Independent b2eecb19 realized-recording suite: 4 tests / 16 assertions pass.
Worker identity is separate from selected-action, and trial identity no longer
masquerades as decision or tick identity. These corrections address the prior
positive-case finding.

Remaining absent-case mismatch: a missing projected selected-action sets
:execution :selected false and :execution :policy :status :not-applicable,
with reason :selected-policy-not-projected. Lack of projection is unavailable
evidence, not evidence of non-selection or inapplicability. Preserve unknown
or refuse if the current recording carrier cannot express it. Do not satisfy
the validator's policy constraint by selecting a semantically false status.
If a validator change is required, it must admit only the explicitly modeled
unknown branch and retain all existing observed-policy validation. Add a
negative control preventing absent evidence from asserting false/inapplicable.
This is remaining work in 5e205db5, not a new parallel implementation request.
No live data or service changes occurred.
