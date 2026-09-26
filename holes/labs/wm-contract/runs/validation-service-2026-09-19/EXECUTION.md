# Generic validation service — slice 1 execution

Source commit tested: `5a4f1a7e93ae198540e1cac6ca1ff72b2a2ff1d0`

## Source pins

- `src/futon3c/test_registry/validation.clj`: `0a691825e3fb936270f185b4521f8b04a23d0187ecbddae482095cc6e35e6a48`
- `test/futon3c/test_registry/validation_test.clj`: `68d7fb74be2290f64a1a805b7bca880fdf2b763b614d6dea27279be2fdfb738a`
- `test/futon3c/test_registry/validation_fixture_test.clj`: `af37ce72608f9994de95dda539464fe124e6d42a025bc235df44621981a3960b`

## Static gates

```text
$ clj-kondo --lint src/futon3c/test_registry/validation.clj test/futon3c/test_registry/validation_test.clj test/futon3c/test_registry/validation_fixture_test.clj
linting took 14ms, errors: 0, warnings: 0

$ emacs -Q --batch -l /home/joe/code/futon4/dev/check-parens.el --eval '(arxana-check-parens-cli)' -- --no-defaults src/futon3c/test_registry/validation.clj test/futon3c/test_registry/validation_test.clj test/futon3c/test_registry/validation_fixture_test.clj
OK

$ git diff --check -- src/futon3c/test_registry/validation.clj test/futon3c/test_registry/validation_test.clj test/futon3c/test_registry/validation_fixture_test.clj
[exit 0, no output]
```

## Single focused fresh-JVM run

```text
$ clojure -X:test :nses '[futon3c.test-registry.validation-test]'
WARNING: Unknown module: org.apache.arrow.memory.core specified to --add-opens

Running tests in #{"test"}

Testing futon3c.test-registry.validation-test

Ran 2 tests containing 11 assertions.
0 failures, 0 errors.
[exit 0]
```

The end-to-end control registered the tiny fixture namespace through the real
`register-run!`, bound `generic/component`, observed `:current`, modified and
restored a closure source and observed `:stale` with that exact path, opened an
incident and observed `:revalidation-open`, rejected the pre-incident warrant,
registered and bound a fresh warrant, closed the incident, and observed
`:current` again. It also asserted this CLI-format readout:

```text
generic/component current test-registry-<content-sha256>
SUMMARY {:current 1}
```

The concrete warrant suffix is generated from the run record and intentionally
is not authored as a fixture constant; the test asserts the live prefix and
summary.

No serving JVM was contacted and no HTTP namespace was changed. Pre-existing
unrelated worktree entries `holes/excursions/apex-thesis.json` and
`scripts/zai-usage` were not staged or modified by this packet.
