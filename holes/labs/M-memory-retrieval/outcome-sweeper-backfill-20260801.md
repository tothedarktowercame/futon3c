# Outcome-sweeper backfill — 2026-08-01

Frozen record of the first `scripts/memory_outcome_sweeper.py` run against the
durable Agency invoke-job ledger and the authoritative substrate at
`http://127.0.0.1:7073`.

## Method

- Ledger: `/tmp/futon3c-invoke-jobs.edn`
- Writer: `:outcome-sweeper`
- Receipt version: `:v1.2-receipt-instrumented`
- Dedupe key: Agency job id, represented by deterministic evidence id
  `e-memory-outcome-sweeper-<sha256(job-id)[0:24]>`
- Extraction: only the runner's final `Memory usage` section. Reports carrying
  the newer bracketed dispatch outcome preserve it; older reports are tagged
  `:recall-outcome :legacy-unknown` rather than assigned a guessed cause.
- Failure discipline: a report with an unclassified memory-id line is not
  written.

The pre-write store snapshot contained 150 `:phase :outcome` rows (146 unique
job ids); the post-write snapshot contained 276 outcome rows, including 126
written by the sweeper. The sweeper itself had 0 rows before the run and 126
after it. All 126 writes were append-only; existing outcome rows were not
mutated.

## Result

| day | recoverable/written | unrecoverable |
|---|---:|---:|
| 2026-07-28 | 3 | 0 |
| 2026-07-29 | 5 | 0 |
| 2026-07-30 | 23 | 8 |
| 2026-07-31 | 77 | 12 |
| 2026-08-01 | 18 | 5 |
| **total** | **126** | **25** |

Unrecoverable reasons:

- 20 `unclassified-memory-lines`: a memory id was present but the final report
  did not say whether it was used or ignored, so the sweeper refused to guess.
- 5 `no-memory-attribution`: the report had a Memory usage section but neither
  attributable ids nor an explicit statement that none were used.

## Idempotency check

First run at `2026-08-01T15:32:40.977997Z`:

```text
existing_outcome_jobs_before: 0
written: 126
existing_outcome_jobs_after: 126
```

Immediate second run at `2026-08-01T15:33:48.772296Z`:

```text
existing_outcome_jobs_before: 126
skipped_existing: 126
written: 0
existing_outcome_jobs_after: 126
```

## Three worked rows

1. `invoke-1785240935276-230-26db063f` →
   `e-memory-outcome-sweeper-78e240d24a071388b54ba37e`. Legacy extraction;
   three ids recovered as used:
   `e-9751e537-f5b7-4c40-a857-0c0b699b93a2`,
   `e-bb16ffa8-e6da-4956-81c3-011e312d5302`, and
   `e-dfea2de9-8979-4f8f-9343-caabb48487e6`.
2. `invoke-1785243793353-233-a32962a2` →
   `e-memory-outcome-sweeper-47f6c2f6032afbe139be642e`. Legacy extraction;
   the same three ids were explicitly reported as used.
3. `invoke-1785246177365-240-fcde4301` →
   `e-memory-outcome-sweeper-ca3da5d98dba944640c9583f`. Legacy extraction;
   five surfaced ids were explicitly ignored, therefore `:memory-use/used-ids`
   is `[]` and the five ids are recorded in `:memory-use/unused-ids`.

Direct by-id reads after the run returned all three rows with
`:phase :outcome`, `:writer :outcome-sweeper`, `:backfill true`, and provenance
containing the job id, ledger path, extraction method, and sweep timestamp.

## Caveat added at review (claude-7)

Backfilled rows carry `:recall-system :v1.2-receipt-instrumented` — the
*writer's* version, not the dispatch-time recall version (those dispatches ran
under v1/v1.1). Any per-recall-system analysis must key regime membership on
`:backfill` / `:writer :outcome-sweeper`, never on `:recall-system` alone.
