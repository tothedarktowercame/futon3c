# RUN4 terminal-evidence consumer mapping

Build/test-only scope. The consumer joins captured admission reservation and
click-result bytes to the click/run binding, terminal projection, and full-loop
run record. Each EDN input is parsed as exactly one form; digests bind the
captured projection and run-record snapshots. Exact series, trial, pin, click,
run, outer-attempt, and internal-attempt identities must agree.

| Durable producer state | Series result | Rationale |
|---|---|---|
| `:grounded-change`, approved executed independent review gate, matching reviewed commit, and resolved/moved grounding witness | `:succeeded`, infrastructure safe | Current producer fields establish the complete author/reviewer/build/grounding ladder. |
| `:build-failed` with typed `:build-failed` at `:reviewer-wait`, a persisted commit, and an executed rejecting independent-review gate | `:failed`, infrastructure safe | The task reached a bounded reviewed build and the typed producer result establishes a task-local failure. Earlier build-resolution failures lack enough projected evidence and remain unmapped. |
| Typed agent/substrate/dispatch/transport/initialization or untyped machine failure | `:blocked`, infrastructure unsafe | These are infrastructure or unknown-machine stops, not task verdicts. |
| Missing click/run binding | no evidence (`nil`) | Wait; click acceptance or service state is not completion. |
| Binding declares a missing projection or run record | refusal | This is a broken durable reference requiring reconciliation, not evidence absence before producer publication. |
| Other well-formed outcome | no evidence (`nil`) | No commissioned mapping exists. |
| Corrupt, stale, conflicting, truncated, or identity-mismatched artifact | refusal | Reconciliation is required; never advance. |

Semantic forks intentionally remain open: reviewer rejection versus task block,
artifact-only outcomes, operator cancellation, guardrail/selection abstention,
and recovery/discharge outcomes do not yet have an unambiguous series mapping.
The accepted projection does not retain the dispatch judgment's author job ID,
so the consumer can require an actual dispatch checkpoint and distinct returned
author/reviewer IDs but cannot join the author ID back to dispatch. That is the
remaining producer-field gap; it is not inferred.
The controller preflight remains the authority for task-pin source freshness:
it re-reads the manifest, packet, config, and pin sources before invoking this
port. This consumer independently detects drift of the terminal projection and
run-record evidence snapshots; it does not claim that a pin digest alone proves
fresh source files outside controller preflight. The module is a read-only port
for the existing series controller. It neither
schedules nor launches work, and it makes no new fsync or power-loss claim.
