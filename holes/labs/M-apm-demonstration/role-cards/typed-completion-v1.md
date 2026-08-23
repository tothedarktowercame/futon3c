# APM typed completion contract v1

This contract applies to every live APM role: Solver, Student, Guide, Scribe,
Proctor, Promotion Proctor, and Analyst.

Conversational output is explanatory only and is never a campaign receipt.
After canonical job announcement, Ground Control registers immutable authority
and injects one exact job-scoped `apm-submit-role.py` command into the activation
prompt. The role writes an observational JSON payload with
`command-own-exit`, `outcome`, `failure-account`, and role-specific `evidence`,
runs that command, and fixes every field-level error before ending.

The role must not submit frame, problem, role, agent, dispatch, attempt,
session, snapshot, or job authority. The controller derives those values from
the registered job. Missing submission means incomplete. Identical replay is
idempotent; conflicting submission or authority fails closed.
