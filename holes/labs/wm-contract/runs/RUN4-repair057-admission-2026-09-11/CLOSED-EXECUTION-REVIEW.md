# Symmetric cohort reader review: changes required

Reviewed 30d51053 / cda9b538 / 66015ccc. Missing-authority rejection and
outcome checks improve the previous boundary, but the new closed-execution
API still admits corrupt execution evidence.

Retained executable reproduction review-closed-execution.clj calls the actual
closed-execution API. It uses the new positive fixture's cohort, which manually
writes only time-step and closed (not the required seven-cell lifecycle).
It changes the close's cohort/id to :foreign-cohort and attempt/id to
attempt-999, while requesting claimed-cohort/attempt-001.

Actual result still returns:
{:id "claimed-cohort--attempt-001", :cohort-id :claimed-cohort,
 :attempt-id "attempt-001", :outcome :grounded-change}

The rejecting assertion fails. Ledger attempt-summary takes identity from the
first event and outcome from a later close without joining their identities or
validating required checkpoints. Closed-execution currently trusts that summary.
A syntactically real activation file is not proof of a valid closed execution.

Required correction: capture the preregistration once, verify every retained
event's cohort/attempt/ordinal/sequence and checkpoint cell against it, require
the complete legal lifecycle and valid close, and reject foreign/truncated/
extra/trailing/corrupt data before minting execution authority. Positive tests
must create the lifecycle with the real activation/start/append/close APIs;
the current hand-written two-cell fixture cannot establish producer compatibility.
Use the pinned snapshot rather than reopening the preregistration after preflight.

A further source-level join remains: resolve-from-durable! still receives only
verification-cohort plus the historical raw attempt number; it does not read
the existing strict historical bundle binding that cohort/attempt to the exact
verification transition. Another historical-admission cohort with the same local
attempt and expected outcome must not authorize this verification. Use the
existing authoritative historical projection/store/cohort join, not caller
association. Preserve the live immutable raw identity as provenance.

The earlier qualified-identity repro now refuses at missing config shape, as
reported. That is necessary but does not exercise these supplied-authority cases.
No live reload, capacity, reset, admission or historical modification occurred.
Joe's trial authorization remains active after the evidence gates.
