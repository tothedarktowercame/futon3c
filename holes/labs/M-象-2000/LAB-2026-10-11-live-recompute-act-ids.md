# Live recompute after record-qualified act IDs, 2026-10-11

Read/reload/recompute window: 2026-10-11 05:19--05:29 UTC. Source commit:
`0d3a90fb` (`xiang: qualify reused turn act ids by record`). No JVM restart
was performed.

## Reload receipt

The logic, adapter, and service namespaces were reloaded from master through
`scripts/proof-eval.sh`. A synthetic session used two records with the same
`turn_id`, one clarify and one constrain. The serving JVM returned:

```clojure
{:acts
 [{:id "reused-turn-f-s1-0"
   :record "record-a" :kind :clarify :port? true :standing? false}
  {:id "reused-turn-r-0"
   :record "record-a" :kind :report :port? false :standing? false}
  {:id "reused-turn@record-b-f-s1-0"
   :record "record-b" :kind :constrain :port? false :standing? true}
  {:id "reused-turn@record-b-r-0"
   :record "record-b" :kind :report :port? false :standing? false}]
 :distinct-ids true}
```

The second occurrence is qualified by its durable record ID, and every act
carries its record.

## Before ledger and population

The before-state ledger was committed as `9d7e744d` before the first live
compute call. It contains:

- 459 analysed operator records with nonblank reply text, in 18 sessions;
- each record's and each session tail's `still_open` and
  `closed_this_turn` counts;
- 59 reused `(session_id, turn_id)` keys since 2026-10-01, represented by
  their 118 individual record rows and current port counts.

The 459 records were replayed per session in `created_at` order, no more than
one call per second, through the existing service with
`{:session-cap 10000}`. There were 451 successes and the same eight expected
no-act refusals. No other record refused.

## Totals

| Scope | Open before | Open after | Delta | Closed-this-turn before | Closed-this-turn after | Constraints after | Inferred after |
|---|---:|---:|---:|---:|---:|---:|---:|
| All 459 record snapshots | 38,940 | 53,602 | +14,662 | 215 | 139 | 0 | 19,116 |
| 18 session tails | 781 | 1,677 | +896 | 9 | 9 | 0 | 367 |
| 118 reused-turn record rows | 667 | 3,019 | +2,352 | — | — | 0 | — |

The three residual constraint ports from the preceding replay are gone. The
tail constraint count is zero.

## Records whose open count grew

110 record snapshots grew, all in the five sessions that contain the
historical turn-ID reuse. They share one reason: qualifying the second and
later occurrence stops distinct acts from collapsing onto an earlier act ID,
so the full-history query can now return the separately addressable ports.
The ledger names every record and its exact `open_growth`; the grouped account
is:

| Session | Records that grew | Sum of growth | Per-record range |
|---|---:|---:|---:|
| `01a0f4ae-af56-7191-b610-291a1bcfaeed` | 11 | +1,486 | +132 to +137 |
| `01a0fe50-5eb3-75b2-b686-b434dc17c357` | 6 | +914 | +148 to +155 |
| `01a0ffa6-1850-7dc2-a283-9b6eb06d5913` | 4 | +451 | +109 to +115 |
| `564c8e50-c240-46fc-81ad-55afafbc63ee` | 26 | +10,334 | +3 to +502 |
| `9ef8e479-e1b7-4755-894d-1264cacb6c3b` | 63 | +1,693 | +1 to +122 |

The five affected session tails changed by +132, +154, +109, +498, and +3
respectively, totalling the +896 tail delta. This growth is the removal of ID
aliasing, not newly inferred acts; the source records and table were unchanged.

## Refusals

The recurring `No acts for turn` records are unchanged. Their readings carry
only adapter-unknown `agree` and/or `challenge` intents and their replies make
no kinded act:

- `turn-5JBr6g`
- `turn-JTuY1v`
- `turn-968NVf`
- `turn-qX81HV`
- `turn-WtIZCU`
- `turn-mVytXT`
- `turn-VgUJ8K`
- `turn-ABYrM4`

## Labelled session

Session `ec3a9b31-6656-4530-90db-53a22762c17f` is unchanged at 56 open
ports in its latest eligible snapshot. Of the checkpoint's 24 rows, four
close, both constraints are standings absent from ports, and the other 18
remain open.

| Since (UTC) | Kind | Operator text (start) | Live result |
|---|---|---|---|
| 10-10 19:36 | report-problem | i tried to open a tunnel | open |
| 10-10 19:39 | ask-action | can you grab a 40°belowdb spec | open |
| 10-10 19:45 | propose | We should explore the other options | open |
| 10-10 22:13 | ask-action | did you get any results back? | open |
| 10-10 22:22 | propose | read claude code memories | open |
| 10-10 23:24 | propose | insertion time vs record time | open |
| 10-11 01:15 | ask-action | are they still going? | open |
| 10-11 01:16 | clarify | what is milestone 4? | open |
| 10-11 01:45 | clarify | I hope this makes sense | open |
| 10-11 01:45 | propose | run all of FUTON in a debugger | **closed: explicit quote** |
| 10-11 02:10 | propose | turn off the standard memory system | open |
| 10-11 02:22 | clarify | That's NOT a directory based search | open |
| 10-11 02:22 | propose | merge this workstream with M-象-2000 | open |
| 10-11 02:25 | ask-action | the 3/2-category orientation document | open |
| 10-11 02:27 | clarify | Is that clarifying? | **closed: sole-port** |
| 10-11 02:33 | propose | useful guiding principle for our DB | open |
| 10-11 02:36 | constrain | Let's not reinvent the wheel | **standing; absent from ports** |
| 10-11 02:42 | propose | work with that extra futon1b | open |
| 10-11 02:42 | propose | hook it all up | open |
| 10-11 02:42 | constrain | don't read every agent turn twice | **standing; absent from ports** |
| 10-11 02:42 | propose | agent chat is a crime scene | open |
| 10-11 02:44 | delegate | bell the work and plan to claude-16 | **closed: sole-port** |
| 10-11 03:36 | delegate | backfill to codex-70 | open |
| 10-11 03:36 | ask-action | restore the setting and backfill | **closed: sole-port** |
