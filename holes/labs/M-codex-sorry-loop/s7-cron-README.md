# S7 Codex sorry-loop cron

`scripts/codex_sorry_cron.py` makes at most one dispatch per invocation.
It uses a nonblocking flock and all scheduling signals fail closed.

## Gates

- Newest local `token_count.rate_limits` must be under 50% used and ≤24h old.
- At most one other Codex is invoking by default.
- Runner is idle/local/invoke-ready, excluding codex-4 and codex-5; 6–8 lead.
- Any `:dispatched` queue row blocks the next dispatch until ground control
  resolves it.
- A `problems/<id>/` row is skipped while a live Zai roster entry names that id.

Cold start: run any manual Codex session to refresh the local usage signal.
No API key or remote usage request is used.

Environment: `CODEX_SESSIONS_DIR`, `CODEX_SORRY_QUEUE`,
`CODEX_SORRY_TEMPLATE`, `CODEX_SORRY_STATE_DIR`,
`CODEX_SORRY_PROGRESS`, `CODEX_SORRY_LOG`, `CODEX_SORRY_LOCK`,
`CODEX_SORRY_AGENCY_BASE`, `CODEX_SORRY_MIN_HEADROOM_USED` (default 50),
`CODEX_SORRY_MAX_OTHER_INVOKING` (default 1), and
`CODEX_SORRY_HTTP_TIMEOUT`.

Queue: 83 rows = four fixed prereg targets, five other missing-lemma
occurrences in census order, and 74 hard-step file groups. There are no
scaffold rows.

## Dry-run sample (2026-07-28)

The live default correctly closed with two invoking Codex agents:

`concurrency-gate-closed invoking=2 max-other=1 agents=codex-4,codex-6`

A dry-run-only diagnostic bound of two then exercised the remaining gates:

`gates-open used=0 age-seconds=7 other-codex-invoking=2`

`DRY RUN row=schwarz-equality-case runner=codex-7
file=ConstructionTargets/SchwarzEquality.lean`

It printed the complete axiom-clean/literature packet and changed neither
queue nor progress state.

## Owner-gated installation

First run one reviewed manual cycle. The owner may then install:

`30 * * * * cd /home/joe/code/futon3c && /usr/bin/python3 scripts/codex_sorry_cron.py >> /home/joe/code/futon2/logs/codex-sorry-cron.log 2>&1`

This delivery does not modify crontab.
