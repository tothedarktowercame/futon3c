# Remaining pre-2026-10-04 reply-text backfill

Run on 2026-10-11 UTC as the continuation of `LAB-2026-10-11-reply-text-backfill-pre-1004.md`.

## Scope and application

The frozen coverage census supplied 548 uniquely matched records outside the two sessions handled in the first packet. They span 15 sessions. The 967 unmatched records were excluded. The immutable before-state ledger was committed as `87b5dd2e` before the first write.

All 548 records were sent to the live Agency happened route in per-session `created_at` order, one request per second. Every request returned HTTP 200, and every record read back with reply text exactly equal to the selected evidence text.

The live `compute-ports!` function was then called with `{:session-cap 10000}` for all 602 settled records in the 15 touched sessions, in session/time order and at one call per second. 601 produced ports. `turn-GMrAyu` refused; an immediate live health receipt said `No acts for turn claude-8-turn-197`.

## Session tails

Every touched tail lacked a ports map before this packet, so its before count is unavailable (blank in the ledger), not a measured zero. After recomputation the tails contain 897 open ports. Every open act ID is an operator fragment (`-f-`); no tail contains an agent paragraph port (`-r-`).

| Session | Tail | Before | Operator ports by kind | Agent ports by kind | After |
|---|---|---:|---|---|---:|
| `14ebf970-6e85-4f6f-a8bc-968cabb1394a` | `turn-aTGI2W` | — | ask-action 1; clarify 1; propose 2; report-problem 1; verify 4 | none | 9 |
| `2a5fa32b-238e-422d-9551-994bf350bf65` | `turn-zGTmPm` | — | ask-action 12; clarify 6; delegate 4; propose 60; report-problem 8; verify 2 | none | 92 |
| `402e0cbd-0cf8-40d3-a986-ed1e0cb1e7de` | `turn-nergnI` | — | ask-action 10; clarify 3; propose 6; report-problem 16; verify 1 | none | 36 |
| `66c595ba-5098-4ba8-8ae4-def07343f76a` | `turn-CrdCfB` | — | ask-action 21; clarify 12; delegate 10; propose 37; report-problem 37; verify 14 | none | 131 |
| `6799e67a-0da8-4bc6-829e-1ff2c07d50c8` | `turn-VR7PGI` | — | ask-action 2; clarify 9; delegate 1; propose 16; verify 2 | none | 30 |
| `6cf867e9-6bab-452d-ba2d-67a561959d12` | `turn-R0O0he` | — | propose 1; report-problem 1; verify 1 | none | 3 |
| `92706bcc-a97f-472d-b391-4f2ff0052041` | `turn-vv6GKG` | — | ask-action 2; delegate 1; propose 3; report-problem 4; verify 1 | none | 11 |
| `9824b707-fdaf-40c6-886e-9ce6eed1cee9` | `turn-zMtvV3` | — | ask-action 14; clarify 18; delegate 13; propose 34; report-problem 15; verify 3 | none | 97 |
| `b24467ae-519b-4310-b7c2-201cd5d78938` | `turn-jNu4PX` | — | ask-action 14; clarify 25; delegate 11; propose 30; report-problem 31; verify 10 | none | 121 |
| `b45799f6-d083-4e87-876c-7bb8a3b347ba` | `turn-4TqhsT` | — | ask-action 2; clarify 1; report-problem 1; verify 2 | none | 6 |
| `cb1be3dd-5b2d-4986-911c-74c178f84408` | `turn-XrAwwe` | — | ask-action 53; clarify 92; delegate 3; propose 78; report-problem 34; verify 3 | none | 263 |
| `de4c2047-bf32-4b18-bd55-8f97e94c6252` | `turn-2yHv3v` | — | ask-action 3; clarify 3; delegate 4; propose 2; report-problem 4; verify 1 | none | 17 |
| `df410037-e8b2-4493-86fc-2bce92d5179a` | `turn-Kp3fSa` | — | ask-action 7; clarify 2; delegate 6; propose 8; report-problem 13; verify 3 | none | 39 |
| `e070e50d-4406-42b4-b7ee-65335695ff8d` | `turn-V6GXQL` | — | ask-action 2; clarify 3; delegate 3; propose 26; report-problem 4; verify 2 | none | 40 |
| `e1f64e20-157f-415d-bcd8-056b97561996` | `turn-ISGjBU` | — | ask-action 1; delegate 1 | none | 2 |
| **Total** | | **—** | **ask-action 144; clarify 175; delegate 57; propose 303; report-problem 169; verify 49** | **none** | **897** |

## Isolation check

The labelled session `ec3a9b31-6656-4530-90db-53a22762c17f` was not targeted. Its 30-record digest was `761c5ee7583f140481db64c78a1255392a4b2adb76bf0872642b075a76863954` before the writes and remained identical afterward.

The evidence objects on futon1b retain their pre-backfill bodies. All record mutation went through the live Agency service.
