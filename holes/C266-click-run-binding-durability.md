# C266 — click/run binding durability and terminal ordering

Date: 2026-09-01

## Result

The click/run binding now uses the same durable replacement discipline as the
invoke-jobs ledger: a same-directory temporary file, complete write and file
force, atomic rename, then parent-directory force. The committed binding also
carries the terminal `:outcome`, so it is independently a record of the fact
the service is about to publish in memory.

The two commit phases are explicit:

- a failure before rename throws `{:committed? false :durability
  :not-committed}`; no binding exists and the public worker closes as
  `:service-failed`;
- a failure after rename returns the committed binding with `:durability
  :unconfirmed` and a typed warning. The service closes with the same run id
  and outcome recorded on disk rather than misreporting the committed binding
  as absent or rolling back terminal state.

A confirmed write is projected into terminal status as
`:binding-durability :confirmed`. Thus binding durability is visible to the
consumer rather than implied by the mere presence of a path.

This does not address the two semantic C262 findings: sequential clicks may
still bind one run id, and `:identity-mismatch` may still coexist with a
present run-id. Those remain separately owned.

## Controls and focused verification

Canonical invocations:

```sh
clojure -M:test -n futon3c.wm.runner-service-test
task_cp=$(clojure -Spath -M:test)
java -cp "$task_cp" clojure.main -e \
  '(require (quote futon3c.wm.chain-rehearsal-test))
   (let [r (clojure.test/run-tests (quote futon3c.wm.chain-rehearsal-test))]
     (System/exit (+ (:fail r) (:error r))))'
clj-kondo --lint src/futon3c/wm/runner_service.clj \
  test/futon3c/wm/runner_service_test.clj
emacs --batch -Q -l ../futon4/dev/check-parens.el \
  src/futon3c/wm/runner_service.clj \
  test/futon3c/wm/runner_service_test.clj
```

Results:

- runner service: 11 tests / 86 assertions, exit 0;
- click-to-certificate chain: 1 test / 16 assertions, exit 0;
- clj-kondo: 0 errors / 0 warnings, exit 0;
- check-parens: exit 0.

The pre-rename control proves binding absence plus `:service-failed`. The
post-rename control injects failure before directory force and proves the disk
record and terminal status retain the same run id and `:grounded-change`
outcome while status reports durability unconfirmed.

No production click ran, and no live ledger or live binding was touched. Per
packet scope, full suites and the workspace gate were skipped.
