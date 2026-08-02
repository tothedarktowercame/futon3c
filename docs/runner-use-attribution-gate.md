# Runner use-attribution gate

The production acceptance hook is `memory_outcome_sweeper.py`. It is the first
offline point at which both inputs exist: the terminal Agency report from the
invoke-job ledger and the authoritative surfaced-id set from the persisted
offered receipt. A completed-with-memories run is not written as a clean
outcome unless every offered id has exactly one `USED` or `IGNORED` line.

State lives under `.state/runner-gate/` (override with
`RUNNER_GATE_STATE_DIR`):

- `agents/<agent>/violations.jsonl` is the durable per-agent ledger;
- `adjudications/<job-hash>.json` is the idempotency marker;
- `agents/<agent>/corrections.jsonl` tracks reviewed correction delivery;
- `review-required/` is the fail-safe path for gate/deposit failures;
- `stop-the-line/<agent>.json` is the dispatch-blocking flag;
- `meta-learning.jsonl` records threshold crossings and operator clears.

The cron filters flagged agents before runner selection. It prepends any
pending reviewed correction to that agent's next packet and records which job
received it. Auto-requeue, the Type-A axiom gate, and adaptive sampling remain
separate increments.

## Operator clear

Inspect, diagnose, and then clear explicitly:

```sh
python3 scripts/runner_gate.py --status codex-7
python3 scripts/runner_gate.py --clear-stop codex-7 --operator joe
```

Clearing removes only the stop flag; it does not erase violations,
adjudications, or meta-learning history.
