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

## Applied from the clock store

Claude-16 approved `--apply`; it ran successfully on 2026-10-12 UTC. The
ledger was written before any record rewrite. The apply read 3,750 record
files: 3,407 were atomically rewritten, 5 already carried the target key, 338
had no exact clock row, and no matching clock row lacked both targets.

| Clock mission | Actually written |
|---|---:|
| `M-G-over-cascades` | 248 |
| `M-autoclock-in` | 1 |
| `M-diagramprover` | 1,008 |
| `M-futon-seams` | 3 |
| `M-futonzero-prelim-practice` | 128 |
| `M-landscape-positioning` | 132 |
| `M-the-perfect-crime` | 19 |
| `M-warrant-limit` | 113 |
| `M-wm-wiring` | 180 |
| `M-象-2000` | 368 |
| **Mission total** | **2,200** |

| Clock excursion | Actually written |
|---|---:|
| `E-agency-work-orders` | 322 |
| `E-aif-cascade` | 85 |
| `E-cascade-real` | 139 |
| `E-convert-operator-turns-to-patterns` | 604 |
| `E-kimi-task-103` | 12 |
| `E-kimi-task-28` | 11 |
| `E-xiang-turn-evidence-outage-2026-10-11` | 34 |
| **Excursion total** | **1,207** |

The TSV contains 3,407 unique record rows. A read-back checked every row and
found the selected key equal to the ledger value in all 3,407 records, with
zero errors. A post-apply dry run found zero remaining exact-clock candidates;
the 338 records without a clock row remained outside the ledger and were not
written by this operation.

The turn recorder remained live during verification. One further already-
stamped record arrived after the apply snapshot, so the later dry run saw
3,751 files and 3,413 records with their selected target key; this does not
change the 3,407-record applied ledger.

## Revised authoritative dry run: clock store per session

The roster-proxy plan above is superseded and **must not be applied**. A new
dry run at 2026-10-12 UTC joined records exactly on `(agent_id, session_id)`
to `data/clock-sessions-2026-10-12.json`. The export contains 462 unique
pairs; the tool rejects duplicate pairs and has no roster fallback.

There were 3,749 record files at read time. Four already contained the key
selected by their clock row and were left alone.

| Clock mission | Would stamp |
|---|---:|
| `M-G-over-cascades` | 248 |
| `M-autoclock-in` | 1 |
| `M-diagramprover` | 1,008 |
| `M-futon-seams` | 3 |
| `M-futonzero-prelim-practice` | 128 |
| `M-landscape-positioning` | 132 |
| `M-the-perfect-crime` | 19 |
| `M-warrant-limit` | 113 |
| `M-wm-wiring` | 180 |
| `M-象-2000` | 368 |
| **Mission total** | **2,200** |

| Clock excursion (when no mission is set) | Would stamp |
|---|---:|
| `E-agency-work-orders` | 322 |
| `E-aif-cascade` | 85 |
| `E-cascade-real` | 139 |
| `E-convert-operator-turns-to-patterns` | 604 |
| `E-kimi-task-103` | 12 |
| `E-kimi-task-28` | 11 |
| `E-xiang-turn-evidence-outage-2026-10-11` | 34 |
| **Excursion total** | **1,207** |

| Outcome | Records |
|---|---:|
| Would stamp from an exact clock row | 3,407 |
| Already has the clock-selected target key | 4 |
| No `(agent_id, session_id)` clock row | 338 |
| Matching clock row but no mission or excursion target | 0 |
| **Record files** | **3,749** |

The totals reconcile: 3,407 planned + 4 already stamped + 338 without a
clock row = 3,749. Absence remains absence for the 338 records.

The revised stdlib-only script reads only the committed clock export. On
`--apply` it would first write a ledger containing record, agent, session,
selected key, value, and clock `decided_at`, then use the same same-directory
temporary-file, `fsync`, and `os.replace` protocol. `--apply` was not run and
the ledger does not exist. Tests cover the exact session join, mission and
excursion-only targets, a missing clock row, a blank clock target, sidecar
exclusion, byte-for-byte preservation of an existing target, and semantic
preservation of all other fields.

```text
Ran 2 tests in 0.007s
OK
```

Awaiting explicit approval before `--apply`.

## Applied (2026-10-12, approved by claude-16; the Codex job died on "model at capacity" after writing, before committing)

Counted on disk by claude-16 after the apply: 3,751 record files; `mission_id`
on 2,207 (2,200 backfilled + the live-stamped ones), `excursion_id` on 1,207.
Ledger: `data/mission-id-backfill-2026-10-12.tsv`. Spot-check: `turn-ZkXF30`
keeps every prior key and gained `mission_id` M-象-2000 from its session's
clock. The follower (minus40) re-mirrors rewritten files on mtime.
