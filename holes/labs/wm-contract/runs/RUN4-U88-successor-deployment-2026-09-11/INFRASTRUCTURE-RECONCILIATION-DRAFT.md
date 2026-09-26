# Infrastructure reconciliation state — draft, not executed

The live successor attempt has six cohort checkpoints, an immutable incomplete
click binding, no run record or terminal projection, and no controller terminal.
It is neither a task success nor a task failure/block.

The smallest honest append-only extension is a separate non-terminal record
`:wm/run4-infrastructure-reconciliation-v1` carrying exact series, trial,
controller attempt, click, execution cohort, cohort attempt, wrapper attempt,
repair-finding identity, manifest/pin source digests, and captured digests for
every joined durable artifact. Admission of
that state must require strict rereads of every referenced artifact and prove
that no task dispatch/build evidence exists. It stops advancement and never
authorizes redispatch.

The record is not checkpoint `007`, a controller terminal, or evidence for
`agent-unavailable` completion. It validates the positive construction,
dispatch, build and adjudication `not-reached-*` producer cells, rather than
inferring non-dispatch from absent files. It must not overwrite the binding,
synthesize a projection/run record, or map the event to `:succeeded`,
`:failed`, `:blocked`, or busy `:not-attempted`. A later independently reviewed
operator disposition may define a closure transition while retaining this
record. No step in this document has been executed; the Lean draft states the
identity/lifecycle shape but does not prove this runtime correspondence.

The implemented constructor and immutable publisher are build-only. They have
not read or written the live roots. Publication is no-clobber and idempotent
only for byte-equivalent evidence; conflicting evidence refuses.

The retained artifacts do **not** contain a producer-written edge from the
controller click/wrapper identity to the execution-cohort ID and local attempt.
Accordingly the record captures the two evidence groups but marks
`:cross-store-association :unknown`. The expected identities cannot turn that
caller context into historical evidence. This record prevents replay of the
already reserved controller attempt; it does not prove that the cohort prefix
belongs to that click, and it cannot support a task or cohort verdict.
