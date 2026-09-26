# P2 verified solved-head pinning report

Date: 2026-08-26

## Commits and files

- P1 dependency: `1f53cf03c53b25e407a7812e440c4971577f4294`
- P2 implementation and tests: `870e872512d8ab7e018f62d41de90394b32d7991`
- This report is committed separately after the implementation commit.

Files touched:

- `src/futon3c/apm/bank_audit.clj`
- `test/futon3c/apm/bank_audit_test.clj`
- `holes/technotes/TN-bank-audit.p2-report.md`

No pre-existing shared-checkout change was staged or modified.

## Implementation

`verify-and-pin!` accepts a P1 classification plus frame, problem, head, and
repository. It skips any classification other than `:unbanked` before invoking
Git or Lean. For an unbanked entry it:

1. reads `problems/<pid>/lean/Main.lean` from the exact head with `git show`;
2. writes a temporary copy with `#print axioms apm_<lowercase-pid>` appended;
3. invokes only `lake env lean <absolute-temp-file>` from the supplied repo;
4. requires exit zero and exactly
   `[propext, Classical.choice, Quot.sound]`;
5. only then runs `git update-ref
   refs/apm/banked-solves/<frame>/<pid>/<head> <head>`.

The Git and Lean effects are injected for tests. The production path checks
that `.lake/build/lib/Mathlib.olean` is absent before elaboration and remains
absent afterwards. A present or newly created file refuses the operation.

The ref shape and `git update-ref` operation follow the established student
preservation implementation at
`src/futon3c/apm/workspace_lifecycle.clj:280-283` and `:357-364`. The important
ordering differs intentionally: existing rejected-candidate plumbing pins
before candidate validation, while solved-head pinning must pass the sorryAx
gate before it writes the ref.

## Gates

Final combined gate command:

```text
clj-kondo --lint src/futon3c/apm/bank_audit.clj test/futon3c/apm/bank_audit_test.clj && emacs -Q --batch -l ../futon4/dev/check-parens.el --eval '(arxana-check-parens-cli)' -- src/futon3c/apm/bank_audit.clj test/futon3c/apm/bank_audit_test.clj && clojure -M:test -n futon3c.apm.bank-audit-test && test ! -e /home/joe/code/apm-lean/.lake/build/lib/Mathlib.olean
```

Output:

```text
linting took 14ms, errors: 0, warnings: 0
OK
WARNING: Unknown module: org.apache.arrow.memory.core specified to --add-opens

Running tests in #{"test"}

Testing futon3c.apm.bank-audit-test

Ran 5 tests containing 16 assertions.
0 failures, 0 errors.
```

The final absence check exited zero. A separate explicit check printed:

```text
.lake/build/lib/Mathlib.olean absent
```

`git diff --check` passed with no output before the implementation commit.

The test cases assert:

- clean exact axioms pin and use a ref containing frame/problem/head;
- `sorryAx` refuses and no `update-ref` call occurs;
- nonzero Lean exit refuses and no `update-ref` call occurs;
- `:banked` and `:head-unresolvable` classifications invoke neither injected
  effect;
- the test repository has no project-local `Mathlib.olean` after the clean
  path.

## Live checks

The live invocation used `/home/joe/code/apm-lean` as the Lake working
directory and called `verify-and-pin!` for the two full commit IDs resolved by
`git rev-parse`:

```text
d84e28b164f3355c53089c19a58f2056e8c1b6db
f7de688760b7f4175ac3787cd2e2f5dae57a75f3
```

Solved f42 head result:

```clojure
{:status :pinned
 :ref "refs/apm/banked-solves/f42/a97J07/d84e28b164f3355c53089c19a58f2056e8c1b6db"}
```

Base revision result:

```clojure
{:status :refused
 :reason :sorry-ax
 :axioms ["propext" "sorryAx" "Classical.choice" "Quot.sound"]}
```

Prefix-form ref enumeration confirmed exactly the new solved ref:

```text
git -C /home/joe/code/apm-lean for-each-ref --format='%(refname) %(objectname)' refs/apm/banked-solves/
refs/apm/banked-solves/f42/a97J07/d84e28b164f3355c53089c19a58f2056e8c1b6db d84e28b164f3355c53089c19a58f2056e8c1b6db
```

No base-check ref exists. After both elaborations,
`/home/joe/code/apm-lean/.lake/build/lib/Mathlib.olean` remained absent.

## Spec observations

The corrected banking document was read at apm-lean master `31e53fba` from
`/home/joe/code/apm-lean-master-bank-t01A03/docs/banking-solved-problems.md`.
Its corrected account of `refs/apm/` agrees with the implementation found in
`workspace_lifecycle.clj`.

No substantive premise in the P2 packet was wrong. The only mechanical detail
is that `../futon4/dev/check-parens.el` is not executable, so it was invoked
through the Emacs batch command documented in that file.

No branch, master commit, or `status.json` in apm-lean was created or changed.
The only apm-lean mutation was the explicitly requested verified ref.
