# C227 — bounded receipts require a repository basis

Date: 2026-08-31. Owner: `wm-organization`.

## Decision

`bg.py launch-test` now requires an explicit `--dir` that Git recognizes as a
repository. It refuses the submission before creating a transient unit when
the directory is absent or unreadable. The command string is deliberately not
parsed to guess a working directory: shell text is not a typed repository
port, and systemd's default working directory is not evidence about the code
under test.

`bounded_test_job.py` also fails closed as a defensive backstop. If invoked
directly or by an older submitter with an unreadable start or finish basis, its
top-level receipt says:

```json
{"verdict":"fail", "outer-exit":125,
 "reason":"repository-basis-unavailable"}
```

A basis that changes during the run similarly fails with
`repository-basis-changed`. The nested observations remain for diagnosis, but
callers no longer need to discover the failure two levels down. C220's
`tested-commit` predicate remains unchanged and continues to reject legacy or
unattributable receipts.

## Controls

```sh
python3 -m unittest scripts/test_bounded_test_job.py
python3 scripts/bg.py launch-test 'true' --agent wm-organization \
  --label c227-no-basis --window control
```

The suite has six tests. The direct-runner control executes a successful
command in a non-Git temporary directory and observes receipt exit 125 with
`repository-basis-unavailable`. The submitter control exits 1 with
`state=refused` and creates no bounded job.

The full workspace gate was not run because C222 identified it as contended
and this dispatch explicitly excluded it.
