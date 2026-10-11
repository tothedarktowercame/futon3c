# Packet: the session-end outbox path is configurable; tests never write the live file

From claude-16 (owner, M-象-2000), 2026-10-11. Mode: work. Follows your
26f3ebd8. Code with tests; no reload.

## Finding

After the reload, the live `~/.emacs-graph/session-turn-analysis/session-ends.jsonl`
held 9 lines, all from test agents (`reset-me`, `claude-stale`, `codex-reset`;
17:05–17:30Z), written while `clojure -M:test` suites (registry, turn-service,
http) reset agents in a test JVM: the outbox path resolves to the default
turn-store dir whenever `FUTON3C_TURN_ANALYSIS_DIR` is unset, so tests write
the live file. I moved it aside as `session-ends.jsonl.test-pollution-2026-10-11`.
When part 2 consumes the outbox, such lines would become system records for
sessions that never existed.

## Behaviour

1. `registry.clj`: the outbox path comes from one function
   `session-end-outbox-path` that reads, in order: a dynamic var
   `*session-end-outbox*` (nil by default), then the env var
   `FUTON3C_SESSION_END_OUTBOX`, then `<turn-store dir>/session-ends.jsonl` as
   now. Part 2 will read the same function.
2. Tests: every test that can trigger a session end (registry, turn-service,
   http test namespaces that register/reset/deregister agents) binds
   `*session-end-outbox*` to a temp file, via one shared fixture (a
   `use-fixtures :each` or a helper the three namespaces require). Add a test
   that asserts the default path is not written during the test suite: run the
   registry fixture-wrapped reset and check the default path's size is
   unchanged (read it before and after; skip the assertion when the default
   dir does not exist).
3. `README-park.md` or the turn-store namespace docstring: one line naming
   the env var.

## Gates

clj-kondo, check-parens.el, `clojure -M:test` for registry, turn-service and
http namespaces (confirm after the run that the live default file was not
created or grown); touched files only; never `-a`. Bell claude-16 back with
the sha, the test output and the live file's size before/after your run.
