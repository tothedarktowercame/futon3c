# APMV3 reliability: bounded diagnostic census

Read-only investigation requested by Joe after F218's watchdog recovery.
Source: `data/apm-campaigns/jit-all-open-v3/queue-state.edn`, observed
2026-09-11, plus the F218 recovery note and existing transport-conformance
technote. No proof sources or sealed evaluation contents were inspected.

## Counts

The queue has 49 prior frame records: 12 completed, 7 statement-refuted
dispositions, and 30 parked. The most recent 25 prior frames, F193–F217, have
12 completed, 7 statement-refuted dispositions, and 6 parked. Completed means
the frame workflow completed; 10 of those results are partial and 2 closed.
These are recorded dispositions, not a count of interventions: a completed
frame may have required repairs, and a correct protective park is not itself
a defect. Frame counts are not unique-problem counts.

Recorded park decision classes include 13 session-identity-erased cases,
2 wrapper/terminal races, 2 successor-dispatched-into-running-seat cases,
and 1 completed-submission-unreconciled case. These classifications come from
existing adjudication records, not fresh independent replays of every event.

Concrete examples from those records:

- F169 and twelve other frames: collection cancelled the wrapper before
  capturing required session identity. Valid Student work then failed a
  provenance check. The existing record identifies repair f4ae8d5a.
- F177/F194: cancellation returned HTTP 409 because the producing job had
  already finished; the collector treated that as reconciliation failure.
- F200/F202: subsequent solver rounds started before the previous writer
  released the session. Resulting dispatch failures were classified as session
  mismatch or repeated lack of solver progress.
- F206: a Student's non-compiling attempted proof was classified as a
  machinery/terminal fault, despite that attempt being an intended measurement.
- F208: external quota failure consumed rounds and led to a strategy checkpoint.
- F209: dead job state, delayed polling, and watchdog deadlines prevented
  straightforward progress after resume.
- F218: a memory-store read threw a transport timeout out of a coordinator
  tick; the retained tick intent eventually exceeded its external deadline.
  See `TN-F218-watchdog-recovery-2026-09-11.md` for direct observation and recovery.

## Interpretation and repair priority

The recurring weakness is agreement between submission acceptance, job/process
termination, session release, durable phase state, and monitoring. Different
orders of these events produce different outcomes where reconciliation should
preserve already accepted work. Failure classification also confuses expected
mathematical incompleteness, unavailable infrastructure, and invalid evidence.
That makes local disruptions become frame parks or campaign stops.

The transport-conformance programme already specifies bounded retries and
distinguishes failed observations from negative evidence. F218 shows that its
existence does not establish coverage of every exception path. This census
does not identify the original store-latency cause or prove an exact missing
catch location.

Recommended next engineering slice: replay the observed submission/termination/
session-release orderings and the F218 store timeout through the actual shared
coordinator path. Require accepted work to survive reconciliation, capacity and
transport faults to retain their own budgets, and watchdog recovery to agree
with the durable phase state. Preserve evidence-validity and workspace guards.
Then measure consecutive frames without operator intervention, separately from
proof completion and legitimate statement-refutation outcomes. Passing isolated
component tests or observing resumed ticks does not establish that property.

No campaign state, source, runtime, or watchdog configuration changed during
this census. At the final observation F218's watchdog remained watching and
its promotion proctor was the active job.
