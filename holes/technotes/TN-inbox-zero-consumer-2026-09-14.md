# Inbox-zero board consumer: implementation and blocked live cleanup

The board now has a consumer in `futon3c.inbox-zero.board-consumer`.
It replays the proposal and certificate in the current process, resolves one
repository root, re-reads watcher state, and scans Git through the existing
`futon3.inbox-zero.watcher/observe-repo` boundary. New observations are read-only
overlays: the background watcher remains the only writer of `state.edn`.

Promotion uses the existing `plan-promotion`, dirty-set attribution projection,
sensitivity screen, gate runner, and `promote-exec/execute-plan!`. Claims that
precede a clean transition are excluded. No new ownership claims are manufactured.
Explicit validation gates are required for each executable plan. Activity is
rechecked after gates and again after journalling the intent, immediately before
calling the executor. The executor retains its empty-index and exact-path checks.
It does not push or send agent messages.

Each proposal, intent, commit witness, and refusal has durable evidence. Commit
messages cite the board, input, and verb digests. Refusals use
`:record/type :inbox-zero/refusal` and `:refusal/reason`. An intent without a
terminal record remains evidence of an interrupted attempt, not proof of a commit.
Consumers sharing a watcher state path exclude each other with a file lock.
This does not claim to lock editors or other Git writers out of a working tree.

## Aggregation correction

The previous live board counted **any historical dirty observation** older than
24 hours. An eventual clean observation could not clear the flag. Selecting only
the latest observation also gives the wrong result: another edit (or HEAD change)
would reset the age of continuously dirty work.

Aggregation now groups by root/worktree/path, takes the latest uninterrupted dirty
history, and measures its start. Clean transitions end that history; new dirt
starts a new one. Deletions and renames count too. Existing ignored-by-design
rules are unchanged. This board measures the dirty-age clause, not the upstream,
unpushed-commit, or linked-worktree clauses of the full inbox-zero definition.

## Live result, 2026-09-14

The reported **11** becomes **4** after correcting aggregation. That is a
measurement correction, not seven cleaned repositories. The actual consumer
cycle was **4 -> 4**, with **zero cleanup commits**:

| Repo | Typed refusal | Evidence / next structural step |
|---|---|---|
| futon0 | `:unattributed` | `README-emacs.md` has no current valid claim. Establish attribution or review through the established sweeper. |
| futon2 | `:in-flight` | Recent watcher activity. Retry after the repo is quiet. |
| futon3 | `:unattributed` | Two `checks/__pycache__/*.pyc` files, `library/.spider/fleet-2026-09-08.edn`, and `resources/sigils/patterns-index.tsv`. Classify generated/runtime material and establish attribution for authored work before promotion. |
| futon3c | `:in-flight` | The consumer implementation itself is active work here. Existing run evidence was left intact. |

No validation-required refusal was reached: activity and attribution blocked all
live plans first. The live cleanup requested by Joe therefore remains blocked;
the isolated integration test below is not a substitute for that live result.

Compact certificates and complete refusal details are committed in
`inbox-zero-consumer-2026-09-14.edn` alongside this note. Full input packets, traces,
certificates, and post-cycle watcher overlays are in
`/home/joe/code/storage/inbox-zero/board-consumer-2026-09-14.edn` (73,187 bytes),
SHA-256 `e771eccaeb2015889394a6bb3e86f019639c2f5e08444bf72e4b3b91a2e89499`.

Before and after board digest:
`sha256:6070d733e67e2a35e32788a53e341cb6fb8ff04699e6d3bbd05555ddd6c56a20`.
Before input digest:
`sha256:a715b8be299ad8216426f7f3018b5ac85060db05fc19cf9d4f22da67fa20ee81`.
After input digest:
`sha256:b70cb08d21b305ea68f7d7bfebf5e9316de9db195abad369dae9878cbb24eda2`.
Verb digest:
`sha256:c31eba18765dba06b8368a95477c948d7c357d04a86907afacdb8a2365456829`.

The verb digest binds in-process function identity. A new process produces its
own certificates; do not expect archived process-local digests to replay there.
`consume!` deliberately refuses foreign-registry or modified certificates. The
cycle runner generates and consumes its proposals in the same process. Each
flagged repo gets its own board invocation, so an in-flight first repo cannot
starve all subsequent repos.

## Running and validation

From canonical `futon3c`:

```sh
clojure -J-Xmx2g -M -m futon3c.inbox-zero.board-consumer \
  '{:ledger-path "/home/joe/code/storage/inbox-zero/board-consumer.edn"}'
```

This runs one bounded cycle, not a background service. With no `:gate-specs`,
otherwise eligible plans produce `:validation-required`. Supply the existing
gate-runner vector in the EDN options, including the appropriate focused tests
for source changes; gates are run in the target repo. `consume!` also accepts the
`:inputs`-bearing return value from `inbox-zero-board-live/run-live!` directly,
with `:state-path`, `:record!`, and `:gate-specs` options.

Focused test commands (each namespace separately):

```sh
clojure -M:test -n futon3c.inbox-zero.board-consumer-test
clojure -M:test -n futon3c.agents.chip-board-live-test
clojure -M:test -n futon3c.agents.chip-board-test
clojure -M:test:test-all -i :slow -n futon3c.inbox-zero.board-consumer-integration-test
```

The `^:slow` integration test runs real Git in a disposable repository, uses a
fixture claim and the real promotion executor, and checks **1 -> 0**, clean Git
status, the actual commit SHA, certificate trailers, replay verification, and an
unchanged watcher snapshot. Ordinary tests stub shell boundaries and exercise
forged certificates, missing/stale claims, edits during validation and after
intent recording, failed gates, occupied-index outcomes, and journal failure.
All changed Clojure files also pass clj-kondo with zero errors/warnings and
`futon4/dev/check-parens.el`.
