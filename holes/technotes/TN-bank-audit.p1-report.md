# P1 bank-audit implementation report

Date: 2026-08-26

## Commits

- Implementation: `1f53cf03c53b25e407a7812e440c4971577f4294`
- This report is committed separately after the implementation commit.

## Files touched

- `src/futon3c/apm/bank_audit.clj`
- `test/futon3c/apm/bank_audit_test.clj`
- `holes/technotes/TN-bank-audit.p1-report.md`

No pre-existing working-tree changes were staged or modified.

## Validation

Lint command:

```text
clj-kondo --lint src/futon3c/apm/bank_audit.clj test/futon3c/apm/bank_audit_test.clj
```

Output:

```text
linting took 10ms, errors: 0, warnings: 0
```

Parenthesis command (the checker does not have its executable bit set, so it
was loaded using the invocation documented in the file itself):

```text
emacs -Q --batch -l ../futon4/dev/check-parens.el --eval '(arxana-check-parens-cli)' -- src/futon3c/apm/bank_audit.clj test/futon3c/apm/bank_audit_test.clj
```

Output:

```text
OK
```

New test command:

```text
clojure -M:test -n futon3c.apm.bank-audit-test
```

Output:

```text
WARNING: Unknown module: org.apache.arrow.memory.core specified to --add-opens

Running tests in #{"test"}

Testing futon3c.apm.bank-audit-test

Ran 2 tests containing 4 assertions.
0 failures, 0 errors.
```

Registry smoke command:

```text
clojure -M:test -n futon3c.agency.registry-test
```

Suite result (the intervening invoke traces and the expected simulated status
publication failure were test diagnostics):

```text
Ran 51 tests containing 192 assertions.
0 failures, 0 errors.
```

`git diff --check` also passed with no output before the implementation commit.

## Spec observations

The specified `/home/joe/code/apm-lean/docs/banking-solved-problems.md` was not
present because that checkout currently has
`repair/m97A06-energy-regularity` checked out. I read the document at commit
`1fe2052c` from the separate master worktree
`/home/joe/code/apm-lean-master-bank-t01A03/docs/banking-solved-problems.md`.
Its contents agree with the packet's digest-comparison requirement.

No other part of the specification appeared incorrect. The implementation is
read-only, and neither implementation nor tests write to or elaborate anything
in apm-lean.
