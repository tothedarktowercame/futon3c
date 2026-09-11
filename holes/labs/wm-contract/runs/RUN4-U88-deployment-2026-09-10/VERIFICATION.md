# U88 deployment verification — 2026-09-10

## Activation and exact-pin refresh — 2026-09-11

Joe explicitly authorized OPEN activation and exact pin freeze. The canonical
mission now resolves from `futon2/holes/missions`; production resolution and
guardrails accept exactly `{:type :advance-mission :target
"M-u88-contextual-preferences"}` and refuse alias targets. The retained draft
and preparation basis remain unchanged historical evidence. RUN4 startup and
automatic start remain false; no route invocation, dispatch, recording,
acceptance, credential read, registry write, or worklist write occurred.

Current exact hashes:

- canonical mission: `8212ec999123468aa10a8bfc657028a0d06ec3c51be7b97a0bcb517f4040c146`
- task pin: `9cf34ffcff3a78bbe2b60e5c8cbfa674887ff3c34de5a41c49ea57315b63b717`
- series pin: `223d0eb87a1d975b9c25ee1b8cdef6be7b16849f97b01e8a6ee6095d18739778`
- run config: `fcc70191f2fe0ecbdc345632a32f87b4e1287f7dff947545ad3ced9814358c56`
- disabled deployment template: `88cdee965a6720447ee9c826c8763057bbece8cf4f4a2dbe6f15460034aa55d1`


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
controller, visibility and authenticated report participate. The historical isolated verification used disposable OPEN bytes; the 2026-09-11 update above supersedes that production-state observation. Reporting changes no file
bytes; missing authentication refuses and missing recording prevents readiness.

Codex17 re-ran read-only preflight after review: sources current; template
disabled; mission open-activated; credential unprovisioned;
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
3. Mission activation and exact source/task/series/template pin refresh are complete; keep them byte-exact through review.
4. Install the actual server-owned mission resolver and admissibility consumers, then verify current environment AND loaded effective values at the serving boundary. A mismatch is a refusal, not permission to reload or restart.
5. Explicitly enable RUN4 and series serving under operator control, rerun production preflight/eligibility checks, and only then consider launch.

Operator acceptance remains separate and false. Unknown observations stay
unknown. This receipt changes no registry, worklist, mission, credentials,
production store, process environment or service.
