# Mission ID backfill dry run — 2026-10-12

Dry run at 2026-10-12 06:41:36Z. No turn record was written and the apply
ledger was not created.

Important limitation: **the current roster mission is only a proxy for the
mission at the time of an older turn; seats can move between missions.** The
exact 127-agent roster response used by this run is preserved in
`data/roster-2026-10-12.json`, with its source, fetch time, and this warning.

## Result

There were 3,744 record files at read time. None already carried the
`mission_id` key.

| Current roster mission | Would stamp |
|---|---:|
| `M-G-over-cascades` | 475 |
| `M-diagramprover` | 861 |
| `M-futon-seams` | 1 |
| `M-futonzero-prelim-practice` | 129 |
| `M-landscape-positioning` | 115 |
| `M-wm-wiring` | 99 |
| `M-象-2000` | 623 |
| **Total stampable** | **2,303** |

| Skip class | Records |
|---|---:|
| Agent is on roster but has no nonblank mission | 1,370 |
| Agent is not on roster | 71 |

The 71 off-roster records are:

| Agent | Records |
|---|---:|
| missing `agent_id` | 15 |
| `claude-19` | 53 |
| `claude-20` | 1 |
| `claude-stale` | 1 |
| `codex-reset` | 1 |

The on-roster/no-mission population is:

```text
claude-1 189       claude-14 65      claude-17 394      claude-2 66
claude-5 19        claude-6 21       claude-7 12        claude-9 24
codex-10 161       codex-11 40       codex-13 9         codex-16 1
codex-21 5         codex-36 15       codex-37 1         codex-38 1
codex-39 5         codex-40 2        codex-41 5         codex-42 3
codex-43 7         codex-44 5        codex-45 1         codex-46 6
codex-47 2         codex-48 3        codex-49 1         codex-5 19
codex-50 2         codex-51 2        codex-52 2         codex-53 2
codex-54 2         codex-55 4        codex-56 29        codex-57 4
codex-58 67        codex-59 17       codex-60 35        codex-61 10
codex-62 14        codex-63 15       codex-64 25        codex-65 11
codex-66 30        codex-7 16        kimi-2 1
```

The totals reconcile: 2,303 stampable + 1,370 no-mission + 71 off-roster =
3,744 records.

## Tool and safety properties

`scripts/backfill_mission_id.py` is Python 3 stdlib-only and defaults to dry
run; `--dry-run` is also accepted explicitly. It selects only filenames with
one dot (`turn-*.json` record files), avoiding analysis/draft/candidates
sidecars. `--apply`, which was **not run**, will:

- write the proxy warning and `(record, agent, mission)` rows to the TSV
  ledger before changing records;
- skip any record that already has the key, including one changed after the
  plan was made;
- rewrite through a same-directory temporary file, `fsync`, and `os.replace`;
- preserve key order and use two-space UTF-8 JSON with the turn store's
  spaced-colon style.

The fixture test covers a stampable record, byte-for-byte preservation of an
already-stamped record, an off-roster agent, a blank-mission agent, sidecar
exclusion, semantic preservation of all existing keys, and the ledger warning.

```text
Ran 2 tests in 0.007s
OK
```

Awaiting explicit approval before `--apply`.
