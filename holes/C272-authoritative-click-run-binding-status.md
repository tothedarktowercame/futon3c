# C272 — authoritative click/run binding status

Date: 2026-09-01

## Result

Click/run bindings now expose one authoritative `:binding-status`:

- `:verified` — a returned run id and readable run record agree on run and
  click identity;
- `:absent` — the runner returned no usable run id;
- `:unavailable` — an id was returned but no readable record can verify it;
- `:identity-mismatch` — the record contradicts the returned run or click id;
- `:duplicate` — a prior committed binding observed the same returned run id.

The runner-returned id is preserved only as evidence under
`:run-id-observation`, shaped either as `{:status :present :source
:runner-return :value ...}` or a reason-bearing absent variant. The former
top-level `:run/id` and `:run-id-status :present` verdict fields are no longer
emitted by the binding or service summary. Thus an identity mismatch cannot
simultaneously assert that identity is good; consumers read `:binding-status`
for meaning and the nested observation only for what the runner said.

Duplicate detection reads prior committed binding records, including the
legacy v1 top-level `:run/id` carrier. The second fresh click is classified
`:duplicate` and names `:duplicate-of-clicks`. This delivery makes the
producer-boundary defect expressible; it does not prevent the producer from
returning duplicate ids.

Unreadable binding history fails loudly during duplicate classification rather
than silently certifying uniqueness.

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
  test/futon3c/wm/runner_service_test.clj \
  test/futon3c/wm/chain_rehearsal_test.clj
emacs --batch -Q -l ../futon4/dev/check-parens.el \
  src/futon3c/wm/runner_service.clj \
  test/futon3c/wm/runner_service_test.clj \
  test/futon3c/wm/chain_rehearsal_test.clj
```

Results:

- runner service: 13 tests / 103 assertions, exit 0;
- click-to-certificate chain: 1 test / 16 assertions, exit 0;
- clj-kondo: 0 errors / 0 warnings, exit 0;
- check-parens: exit 0.

Controls construct every status. In particular, the mismatch case retains
`expected-run` only in `:run-id-observation`, reports
`:binding-status :identity-mismatch`, and contains neither top-level `:run/id`
nor `:run-id-status`. Two sequential clicks returning `run-duplicate` classify
the first `:unavailable` and the second `:duplicate` with the first click id as
provenance.

No production click or live binding was written. Full suites and the workspace
gate were skipped per focused scope.
