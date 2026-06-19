# T-mq7-unaddressable-caller — mesh-QA invariant for completions that can't bell back

Parent: `M-agency-hardening.md`. Motivated by datapoint #1
(`M-agency-hardening-datapoints.md`): codex-2 completed but its completion bell
couldn't fire because the caller was recorded as `http-caller` (no `--from`) —
an *unaddressable completion*. The current MQ-1..MQ-6 don't flag this (the job
shows `delivered` via job-status). Reviewer: claude-6. Status: queued (dispatch
after Car-3 Phase-1 returns, to avoid codex-1 contention).

## Goal

Add **MQ-7 (unaddressable caller)** to `futon3c.agency.mesh-qa`: flag a terminal
invoke-job that *should* have belled a caller but can't, because the caller is a
non-agent placeholder.

## Definition (checkable from current data)

MQ-7 fires for an edge when ALL hold:
- `:source = :invoke-job` and `:terminal-state` is terminal;
- recipient (`:to`) is an **auto-bellback-eligible** agent (reuse mesh-qa's
  existing recipient predicate — currently `:codex` type); and
- `:from` (caller) is **unaddressable**: `"http-caller"`, blank/nil, or not a
  currently-registered agent.

⇒ the completion could not route back to a real agent. Report
`{:invariant :MQ-7 :ref <job-id> :detail "codex completion has no addressable
caller (…)" :caller <from>}`.

## Deliverables

- `src/futon3c/agency/mesh_qa.clj`: `:MQ-7` entry in `checkability` (status
  `:checkable`); `check-mq-7-unaddressable-caller` fn; wire into `check-mesh`'s
  concat + the `counts` zipmap (so `/api/alpha/coordination/qa` reports it).
- Logic-model case in `test/futon3c/agency/mesh_qa_model_test.clj`: conforming
  (codex job with a real registered caller → no MQ-7); adversarial (codex job
  with `http-caller` → MQ-7 fires; non-codex with http-caller → no MQ-7).
- Impl test in `test/futon3c/agency/mesh_qa_test.clj`.

## Gates

clj-kondo clean (no new warnings); `futon4/dev/check-parens.el`;
`clojure -X:test` green on the mesh-qa nses.

## In-flight constraints

- No JVM restart/reload — claude-6 owns the live reload (MQ-7 will ride the next
  reload).
- Branch from `integration/agency-hardening`:
  `git worktree add ../futon3c-mq7 -b codex/mq7-unaddressable integration/agency-hardening`.
  Commit there, no push/merge. (Independent of the Car-3 branch.)

## Done = bell claude-6 back

with branch + sha, gate results, and confirmation MQ-7 shows in
`check-mesh`'s counts.
