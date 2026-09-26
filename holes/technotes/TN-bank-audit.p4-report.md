# P4 report: sweep pinned solves onto master

Date: 2026-08-26

## Result

Implemented `futon3c.apm.bank-sweep/sweep-to-master!`. The sweep fetches
`origin`, classifies solved receipts by proof-content digest against
`origin/master`, verifies that the exact solver head is the value of its
`refs/apm/banked-solves/<frame>/<problem>/<head>` ref, and banks only solves
which pass both checks.

Eligible solves are applied in a fresh temporary worktree based on
`origin/master`. Each produces two commits in order: the byte-identical Lean
file with the original solve subject, then the recomputed `status.json`. The
temporary worktree is removed in `finally`; its named `bank/sweep-<uuid>`
branch remains, so locally created commits survive a dry run or rejected push.
Pushing defaults off. When enabled, the only push form is
`git push origin HEAD:master`; non-fast-forward rejection is reported and is
never overridden.

Implementation commit:

- `457d48d43a077c953532619c42e2d9abc3fdd1e9` — `Add pinned solve sweep to apm-lean master`

P3 review fix read before implementation:

- `7c0a9338` — retained the original meaning of `:branch-retained?` and made
  `pin-solve-fn` optional. P4 does not alter either behavior.

## Files touched

- `src/futon3c/apm/bank_audit.clj`
  - Added an optional `:master-rev` (default `"master"`) so the sweep compares
    against the fetched `origin/master` without changing P1 callers.
- `src/futon3c/apm/bank_sweep.clj`
  - New sweep implementation.
- `test/futon3c/apm/bank_sweep_test.clj`
  - Fully injected campaign, Git, and revision-reader fixtures.
- `holes/technotes/TN-bank-audit.p4-report.md`
  - This report.

No other file was staged or committed. The checkout contained unrelated
modified and untracked files; they were left untouched.

## Acceptance coverage

The five fixture tests cover:

1. A pinned, unbanked solve creates the Lean commit then the metadata commit,
   with exactly the requested path supplied to each commit.
2. An unbanked solve whose exact pin does not resolve to its head is refused as
   `:not-pinned`, with no commits.
3. An already-banked solve is skipped, with no commits and no push.
4. The successful banking fixture runs with `:push? false` and asserts that no
   push is invoked.
5. A non-fast-forward push is reported; the two commits remain on the named
   branch, the push call contains no `--force`, and no argument is a `+`
   refspec.
6. A second sweep after master content matches banks nothing and creates no
   additional commits.

The pin check resolves the ref and compares its value with the full head SHA;
the ref's name alone is not accepted as evidence.

## Gates

Lint and parentheses:

```text
$ clj-kondo --lint src/futon3c/apm/bank_audit.clj src/futon3c/apm/bank_sweep.clj test/futon3c/apm/bank_sweep_test.clj test/futon3c/apm/bank_audit_test.clj
linting took 23ms, errors: 0, warnings: 0

$ emacs --batch -Q -l ../futon4/dev/check-parens.el -- src/futon3c/apm/bank_audit.clj src/futon3c/apm/bank_sweep.clj test/futon3c/apm/bank_sweep_test.clj test/futon3c/apm/bank_audit_test.clj
[no output; exit 0]
```

New sweep tests:

```text
$ clojure -M:test -n futon3c.apm.bank-sweep-test
Testing futon3c.apm.bank-sweep-test
Ran 5 tests containing 28 assertions.
0 failures, 0 errors.
```

P1/P2 regression tests:

```text
$ clojure -M:test -n futon3c.apm.bank-audit-test
Testing futon3c.apm.bank-audit-test
Ran 5 tests containing 16 assertions.
0 failures, 0 errors.
```

Terminal/retirement suite:

```text
$ clojure -M:test -n futon3c.apm.queued-frame-terminal-test
Testing futon3c.apm.queued-frame-terminal-test
Ran 10 tests containing 69 assertions.
21 failures, 0 errors.
```

The count remains exactly the 21 pre-existing failures established at baseline
commit `3d91dd0e`. They are all in
`five-problem-no-dispatch-terminal-qualification`, where `queue/tick!` returns
a nil status; P4 does not touch that path.

No Lean or Lake command was run. Before and after the live dry run,
`/home/joe/code/apm-lean/.lake/build/lib/Mathlib.olean` was absent.

## Live dry run

Exact command (with `:push? false`):

```text
$ clojure -M -e '(require (quote futon3c.apm.bank-sweep)) (prn (futon3c.apm.bank-sweep/sweep-to-master! {:campaign-dir "/home/joe/code/futon3c/data/apm-campaigns/jit-all-open-nontopology-v1" :repo "/home/joe/code/apm-lean" :push? false}))'
{:banked [],
 :skipped [{:frame "f34", :problem-id "a95J03", :head "5744025dcf3d278a0973841c7c3e6e8735e496aa", :status :banked, :reason :banked}
           {:frame "f35", :problem-id "a95J04", :head "e885b1320f5b5135de85121c748579b4b1c06ad3", :status :banked, :reason :banked}
           {:frame "f37", :problem-id "a96A08", :head "6a96cff2f745981b34ec58efa8761c30a59251ed", :status :banked, :reason :banked}
           {:frame "f40", :problem-id "a97J05", :head "143e17dd25b60d09f1546f669d097f462490d073", :status :banked, :reason :banked}
           {:frame "f41", :problem-id "a97J06", :head "6bf011133fc46aa9bb38e90f302b602b837c646a", :status :banked, :reason :banked}
           {:frame "f42", :problem-id "a97J07", :head "d84e28b164f3355c53089c19a58f2056e8c1b6db", :status :banked, :reason :banked}],
 :refused [], :pushed? false, :reason :nothing-to-bank}
```

This confirms that the current live campaign terminates with nothing to bank
and makes no worktree or commit. As requested, it is not evidence that the
live commit path works: the injected tests are the only execution of that path
in this packet. No real push was attempted.

## Spec findings

Nothing in the behavioral specification proved wrong. The explicit safety
requirements are compatible with the existing P1/P2/P3 design. The only
distinction worth recording is that `git worktree remove --force` is used for
the required unconditional temporary-worktree cleanup; the prohibition on
force applies to pushing, and tests inspect push calls specifically.
