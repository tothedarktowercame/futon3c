# Infrastructure reconciliation state — draft, not executed

The live successor attempt has six cohort checkpoints, an immutable incomplete
click binding, no run record or terminal projection, and no controller terminal.
It is neither a task success nor a task failure/block.

The smallest honest append-only extension is a terminal state
`:infrastructure-reconciliation-required` carrying exact series, trial,
controller attempt, click, execution cohort, cohort attempt, binding digest,
repair-finding reference, and the observed checkpoint prefix. Admission of
that state must require strict rereads of every referenced artifact and prove
that no task dispatch/build evidence exists. It stops advancement and never
authorizes redispatch.

After independent review, a reconciliation operation could append the missing
cohort close as `:agent-unavailable`, then append the controller reconciliation
cell referencing that close and the immutable failed binding. It must not
overwrite the binding, synthesize a projection/run record, or map the event to
`:succeeded`, `:failed`, `:blocked`, or busy `:not-attempted`. A later explicit
operator disposition may close the reconciliation state while retaining both
records. No step in this document has been executed.
