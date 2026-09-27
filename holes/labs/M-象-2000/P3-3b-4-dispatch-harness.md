# P3-3b-4 — WM dispatch and bell job harness

Commits: futon2 `c19f2de48`; futon3c `f7e0c69d`.

## Producer and preservation

`futon2.aif.full-loop-runner/dispatch!` takes :run-id from its options, the
current run identity assigned by run-opportunity! and carried through author,
reviewer and repair dispatch options. It is run-scoped, not a freshly minted
job or attempt id. A nonblank string produces harness war-machine / producer-context
with that execution-id; missing/non-string/blank produces unknown with the reason
"runner has no usable :run-id". Existing run identity rules are unchanged.

Futon3c's bell handler validates a present harness by calling the shared
futon1b-harness/refusal and normalize functions (the same contract used by
futon1b), before creating a job. Invalid/null/reserved Zai inputs return 400 with
a typed reason and create no job or edge. Absent remains absent. Valid normalized
context is retained on the job, public GET projection and compact terminal job,
and copied to the mesh-edge evidence entry. No caller-name inference or grant
is added. Job-id reuse with different harness context returns 409 before writing;
a second check in ledger creation prevents a race from silently changing context.
The request commission's existing authorization digest is not redefined.

This packet does not propagate context to the recipient's subsequent tool/turn
acts: the scope is the dispatch payload, stored job, and its mesh edge.

## Tests and limits

Lint has no errors/warnings in both repos; parens checks passed all five changed
Clojure files. A missing explicit substrate require in the existing futon2 test
namespace was added to clear its preexisting lint warning. Fresh tests were needed
because the source changed; no prior warrant covers this revision.

Passing targeted checks:
- new bell-harness-test: 1 test / 45 assertions. Real handler, isolated durable
  job file, ledger and mesh evidence path; delivery alone suppressed. Uses the
  mesh ledger's explicit test-store binding. Present, absent, six invalid forms,
  GET job projection and id-reuse conflict are covered.
- coordination-ledger-test: 5 tests / 23 assertions.
- runner dispatch tests (new harness test and existing work-mode test): 2 tests /
  11 assertions. Actual dispatch payload construction, transport replaced.

Broader checks are **not all green**:
- transport.http-test: 131 tests / 671 assertions, 21 failures and 4 errors.
  The nine failing test vars were rerun against pre-change HEAD HTTP and ledger
  source in a separate test JVM: exactly 21 failures / 4 errors reproduced
  (47 passing assertions). Failures include existing health-count, delivery,
  archive, stack-report and whistle-timeout cases; not attributed to this patch.
- full-loop-runner-test (205 tests in the namespace) exceeded the 240-second
  timeout without a failure/error reported before termination. No full-pass
  claim. The dispatch-specific tests then completed cleanly. No full suite ran.

Logs: /tmp/p3-3b4-*.log. Pre-change sources and baseline runner:
/tmp/p3-3b4-baseline/. Tests used isolated processes; baseline source was never
loaded into the live JVM. Timed-out process was terminated; no background process
was left. No WM run or click was started for deployment validation.

## Live validation

Job `invoke-1790550208522-25776-bf4a286b`: GET exposes the supplied harness.
Mesh evidence `e-696aae6c-5b31-4d81-a038-733f04a22075`: LIST at system-as-of
`2026-09-27T23:03:30.684471Z` exposes the identical field.
Raw accepted response/job/edge records: /tmp/p3-3b4-live.json.

Reloaded futon3c.social.coordination-ledger and futon3c.transport.http from
canonical master via proof-eval require :reload. Resource resolved to
file:/home/joe/code/futon3c/src/futon3c/transport/http.clj.
GET /api/alpha/agents afterwards returned 200. The futon2 full-loop-runner namespace
was absent from the running JVM, so no futon2 reload was performed.

The bell route and queue have no same-caller/recipient rejection; exact-seat GET
confirmed codex-5 was registered. Exactly one self-bell was submitted, from/to
codex-5, mode brief, asking only ACK and explicitly prohibiting tools/writes/further
dispatch. Harness is none / producer-context / source-ref P3-3b-4-live-probe.
No additional completion bell is sent to claude-17; this packet replies in-thread.
