# Reply-text backfill before 2026-10-04

Read at 2026-10-11T05:36:06Z. This first, read-only phase was committed before any POST.

## Coverage

The population is every analysed operator record with `created_at < 2026-10-04T00:00:00Z`: 1,934 records, spanning 2026-09-22 through 2026-10-03. This is larger than the packet's estimate of about 1,380. Nineteen already had reply text because the earlier sampled backfill included them. Of the full population, 1,015 carry an `evidence_id`.

The implemented join is unchanged: first accept exactly one assistant chat-turn directly (or through one intermediate evidence node) beneath the operator `evidence_id`; otherwise take the first assistant chat-turn in the same session after `created_at` and before the next operator record. A disagreement between those joins is refused. There were no disagreements.

| Outcome | Records |
|---|---:|
| evidence chain (join 1) | 154 |
| session/time window (join 2) | 813 |
| none | 967 |
| disagree / ambiguous | 0 |

### By day

| Day | Join 1 | Join 2 | None |
|---|---:|---:|---:|
| 2026-09-22 | 0 | 32 | 23 |
| 2026-09-23 | 0 | 79 | 10 |
| 2026-09-24 | 0 | 12 | 0 |
| 2026-09-25 | 0 | 12 | 1 |
| 2026-09-26 | 0 | 75 | 27 |
| 2026-09-27 | 0 | 136 | 46 |
| 2026-09-28 | 0 | 113 | 37 |
| 2026-09-29 | 46 | 96 | 14 |
| 2026-09-30 | 63 | 127 | 13 |
| 2026-10-01 | 19 | 60 | 111 |
| 2026-10-02 | 22 | 49 | 228 |
| 2026-10-03 | 4 | 22 | 457 |

### By agent

| Agent | Join 1 | Join 2 | None |
|---|---:|---:|---:|
| claude-1 | 28 | 143 | 8 |
| claude-2 | 1 | 12 | 1 |
| claude-3 | 9 | 15 | 1 |
| claude-4 | 35 | 84 | 2 |
| claude-5 | 0 | 15 | 1 |
| claude-6 | 0 | 11 | 1 |
| claude-7 | 0 | 5 | 4 |
| claude-8 | 0 | 89 | 10 |
| claude-9 | 0 | 20 | 3 |
| claude-12 | 0 | 31 | 4 |
| claude-14 | 0 | 29 | 1 |
| claude-17 | 71 | 230 | 20 |
| claude-18 | 10 | 76 | 29 |
| claude-19 | 0 | 53 | 0 |
| claude-20 | 0 | 0 | 1 |
| all Codex seats | 0 | 0 | 870 |
| kimi-2 / zai-1 / zai-5 | 0 | 0 | 4 |

The full run would require 948 one-second POSTs and then replay of every record in all touched sessions, exceeding the packet's approximately 40-minute limit. Therefore this run proceeds with the two explicitly prioritised sessions: `564c8e50-c240-46fc-81ad-55afafbc63ee` and `9ef8e479-e1b7-4755-894d-1264cacb6c3b`. They contain 419 unique matches, of which 19 were already populated, leaving 400 POST candidates. The remaining matches are deferred to a second packet.
