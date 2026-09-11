# Queue review — 2026-09-11, Codex-17

Reviewed 4d4599fd. Not accepted for activation yet.
Existing focused tests independently pass: 5 tests / 35 assertions.

Executed review-repro.clj in a disposable JVM and roots:
- resume on a held queue with its scheduler still registered throws :already-started.
- It has already overwritten the durable :held state with :stopped / :operator-resume-required.
- A directory at queue-state.edn returns a fresh :not-started state/cursor 0 rather than refusing.

Additional inspected concerns requiring gates:
- start!/resume! state mutations do not share tick!/stop!'s JVM/OS lock.
- valid-state? admits arbitrary in-flight maps, missing fields, non-ISO timestamps,
  and cursor movement without completed-entry evidence.
- config-digest omits execution authority roots and full cohort binding; replacing
  server/controller roots can retain the queue digest and cursor.
- A catch after successful started-state publication uses the earlier state,
  potentially losing its recorded click identity.
- On restart, the persisted in-flight click is not used for waiting; only a new
  :trial-started return invokes await-click!.
- Current positive progress tests redefine series/step!. The only actual service
  test uses deliberately malformed config; it does not establish composed success.
- stop! validates mutable source files first: source drift may prevent an operator
  stopping local scheduling. Scheduled exceptions outside the inner catch vanish.

Require focused corruption/race/recovery controls plus a materialized disposable
async service/queue lifecycle using actual controller, cohort, binding, recording,
visibility and terminal readers. Task core/environment may be isolated ports;
validator/step substitutes cannot establish that composed gate.

No live queue, capacity, click, restart or old evidence mutation occurred.
