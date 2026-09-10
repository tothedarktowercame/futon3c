# U88 deployment verification — 2026-09-10

The isolated configuration-to-report chain is independently verified. Production
is disabled and not eligible to launch. No live run or successful U88 worker
execution is claimed.

Author fixture: 8050a03abf112625a674b99dadae61f7ae8d4251.
Independent reviewer: Codex12, Agency job
invoke-1789069200089-19292-d82f6274. Reviewer reproduced 1 test / 23 assertions,
zero failures/errors, lint 0/0, parentheses and diff checks clean. Codex17's
original execution also passed 1/23. Only the task core and environment reads
are fixture ports; the actual materializer, mission parser, authenticated
HTTP handler, pinned selection, async wrapper, durable evidence writers/readers,
controller, visibility and authenticated report participate. OPEN status and
recomputed pins exist only in disposable storage. Reporting changes no file
bytes; missing authentication refuses and missing recording prevents readiness.

Codex17 re-ran read-only preflight after review: sources current; template
disabled; mission expected-draft-eligibility-refusal; credential unprovisioned;
all six roots missing; declaration supported; consumer-state
unknown-not-loaded; eligible-to-launch false. The consumer-state field is not
an inspection of the live JVM. No live effective-value attestation exists in
this receipt.

Independently rechecked SHA-256:

- Fixture: 782dc9b9a9d100ed28be8b4b795cfaff1c3957f2fcfd0a8f32f33523177bd1c6
- Deployment template: a008091cf7b4745e4f9e9f75e547b0c4a2f7fda59f44dc442a3cde6ce5d51819

Remaining production steps, not executed or authorized by this receipt:

1. Provision the server-owned bearer through a private mechanism; never put its value in a receipt or source control.
2. Create and permission controller/admission, binding, projection, run-record, recording and visibility roots from the template.
3. Activate the reviewed U88 mission through its actual discovery lifecycle; freeze new source/task/series/template hashes after that source change. The DRAFT pins cannot be reused unchanged.
4. Install the actual server-owned mission resolver and admissibility consumers, then verify current environment AND loaded effective values at the serving boundary. A mismatch is a refusal, not permission to reload or restart.
5. Explicitly enable RUN4 and series serving under operator control, rerun production preflight/eligibility checks, and only then consider launch.

Operator acceptance remains separate and false. Unknown observations stay
unknown. This receipt changes no registry, worklist, mission, credentials,
production store, process environment or service.
