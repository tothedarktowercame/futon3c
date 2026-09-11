# V4 production preparation — first integration packet

Joe commissioned preparation for production on 2026-09-11, retaining V3's
working execution and recovery behavior. This packet is implemented and tested,
but **V4 is not production-ready**. No V4 queue is started, no live namespaces
are reloaded, no pattern is published, and no new Student trial is claimed.

## Implemented: executed revision review

`src/futon3c/apm/pattern_revision_review.clj` supplies three production adapter
operations through the existing V3 Agency and typed-submission ports:

- `prepare!` reads an actually completed author's Agency job and its persisted,
  token-authenticated typed submission. The proposal is taken from
  `:payload :evidence :pattern-revision-proposal`, not a caller's author label.
  It checks the submission's content hash and registered authority, job/actor
  identity and observed session. It pins source and candidate bytes under two
  separately configured roots, rejects path/symlink escapes and stale hashes,
  and resolves a different reviewer actor **and session**.
- `dispatch!` announces a deterministic job, registers immutable typed authority,
  then activates through `job-port`. The review's authority includes the proposal,
  source/candidate byte hashes, author job/submission/session and reviewer session.
  The prompt supplies the existing typed completion command with the actual job
  ID and configured Agency endpoint. The review has a 15-minute execution budget;
  there is no automatic retry. Terminal replay never reactivates completed work.
- `collect!` distinguishes pending jobs from completed reviews. It joins actual
  Agency completion to the registered typed request and content-addressed
  submission, checks the pinned reviewer session, source/candidate freshness,
  candidate/proposal identity and verdict. Its deterministic receipt records
  accept/reject/cannot-judge and reasons. It always says
  `:publication/authorized? false` and `:mathematics/verified? false`.

The new `:pattern-revision-review` is an **outer learning job schema**, not a new
Petri-net frame transition or a frame-close certificate. `typed_role_submission`
rejects malformed review payloads and wrong proposal/candidate hashes *before*
writing an immutable submission, allowing correction without inventing another
job to recover a bad deposit. Existing phase schemas keep their behavior.

This establishes the provenance of a review assertion, not that a reviewer read
every file or that the proposed mathematics is correct. Trigger evidence/node
references and whether the file implements the named pattern require review.
Publication must still recheck the base under its own lock; a prior `collect!`
is not a lock or permission to overwrite the library. The returned request
contains a submission credential and must stay in private controller state.

## Implemented: missing observations stay unknown

The Python prototype now accepts explicit `"unknown"` alongside booleans for
retrieval, reading, applicability, proof use and usefulness. Unknown is retained
in the event record and never establishes successful use. Positive proof use
still requires positively established reading/applicability; usefulness still
requires positively observed proof use and separate review evidence. Missing
fields and null are refused. The prototype remains offline: its supplied string
identities are not replaced or authenticated merely by this schema change.

## Keep the V3 execution path

The V4 release should select a learning protocol on the existing JIT queue,
not copy the coordinator or create a parallel supervisor implementation. Reuse:

| Behavior | Existing implementation to retain |
| --- | --- |
| One durable current frame, no premature successor | `problem_queue_supervisor.clj`, `queued_frame_adapter.clj` |
| Canonical role/job authority and actual terminal collection | `typed_role_submission.clj`, `job_port.clj`, `live_job_driver.clj` |
| Immutable snapshots, independent reviews, canonical evidence endpoints | `memory_snapshot.clj`, `live_promotion.clj`, `promotion_review_store.clj`, substrate clients |
| Five-second warnings, thirty-second read deadline, frame-end repair hold | `read_health.clj`, `store_read_hold.clj`, `jit_queue_coordinator.clj` |
| Banked solves, workspace retirement, resumable existing frames | `queued_frame_terminal.clj`, `workspace_lifecycle.clj`, queue resumption APIs |
| Durable watchdog and explicit display of actual queue state | `durable_coordinator.clj`, Voxterm lifecycle projection |

The existing filename `apm-cycle-contract-v4.json` is a **contract version used
by current V3**; it is not evidence that the pattern-first product V4 is already
implemented or deployed. Keep those identities distinct.

## Remaining release gates, with concrete consumer seams

1. **Student plan before detailed construction.** Wire stable obligation/node IDs,
   definitions, conditions, ordinary deductions and exact method/example pins
   into the actual Student request and typed report. Reuse the established
   memory-access gate; do not leak the Codex reference solution while building
   teaching material. Neither method citations nor draft patterns discharge a
   proof obligation. The retained `plan_review.py` is completeness checking,
   not a mathematical validator or a production parser for arbitrary payloads.
2. **TA response and durable plan revision.** Connect the real independent TA
   judgment to that exact plan and its next Student attempt. Preserve distinct
   retrieval/applicability/execution diagnoses and per-node revisions. The new
   adapter reviews a library proposal; it does not yet implement this dialogue.
3. **Canonical publication, then fresh next-use delivery.** Implement the library
   owner's publisher around the accepted executed-review receipt, source compare
   under lock, commit/index revision and publication receipt. Retain prior bytes
   and rejected proposals. Deliver the actual new revision through the existing
   retrieval consumer; include index/cache revision in the read receipt. The
   offline Python library must not become a second canonical library.
4. **One bounded end-to-end development rehearsal.** Freeze an exposed positive
   case and an applicability contrast, run distinct real TA/Student actors,
   publish only a reviewed change, then observe a separate next use. Verify
   restart/replay at each boundary and preserve the V3 warning hold. Record
   unknowns, failed uses and correct refusals. The earlier transfer pilot is
   exposed development evidence, not an unseen evaluation.
5. **Reproducible handover pin.** At inspection the canonical checkout contains
   uncommitted `durable_coordinator.clj` and its tests owned by other work. Do not
   overwrite or silently adopt them. The owner must finish and pin that work;
   then verify the code actually loaded and hand over at a quiescent frame boundary.
   No queue-state copying, resetting frame counters or reopening completed proofs.

There is no reason to restart V2 or topology, change RUN4, or move the running
V3 to the prototype to accomplish these gates.

## Validation performed

- Native review adapter: 6 tests / 40 assertions, passing. Uses real temporary
  typed-submission records and byte-pinned files, with a mocked Agency transport;
  it does not claim a real independent review.
- Typed submission regression/strict admission: 18 tests / 82 assertions, passing.
- Offline Python suite: 16 tests, passing, including unknown-use controls.
- Retained a93A01 provenance verifier: 11 nodes, acyclic obligations and matching
  source/pattern revisions. No fresh Lean check or Student invocation.
- clj-kondo, workspace check-parens and diff checks pass.
