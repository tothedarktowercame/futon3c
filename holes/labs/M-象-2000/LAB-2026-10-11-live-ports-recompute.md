# Live ports recompute, 2026-10-11

Read/reload window: 2026-10-11 04:50--05:00 UTC. The source commits were
`42379dfa` (table-typed sole-port inference) and `f07e51d6` (call-scoped
session cap). No JVM restart was performed.

## Reload receipt

The three namespaces were reloaded from the master checkout through
`scripts/proof-eval.sh`:

```clojure
(require 'futon3c.logic.xiang
         'futon3c.xiang.turn-acts
         'futon3c.xiang.turn-service
         :reload)
```

The serving JVM returned:

```clojure
{:arglists ([svc id]
            [svc id {:keys [session-cap]
                     :or {session-cap ports-session-cap}}])
 :synthetic
 {:inferred [{:answer "r" :opener "o" :rule :sole-port :effect :closes}]
  :closed-this-turn ["o"]}}
```

The synthetic history was one `:clarify` operator act and one untargeted
`:explain` reply act in the same turn.

## Population and method

The immutable before-state ledger was committed as `d01ca844` before the
first call. It selected every one-dot record whose status was `analyzed`,
whose origin was operator (including the historical nil spelling), and whose
`reply_text` was nonblank. Records whose session or source named
`(awaiting session)` were excluded; none of those met the reply-text predicate
(`turn-Rbl3PJ` has no reply text).

There were 455 records in 18 sessions. They were ordered first by session and
then by `created_at`. Each was passed, at no more than one call per second, to
the existing live service:

```clojure
(compute-ports! service record-id {:session-cap 10000})
```

No `with-redefs`, direct record writes, or store writes outside the service
were used. There were 447 successful recomputations and 8 refusals. The TSV
now contains before and after counts, inferred-witness count, status, and the
refusal detail for every row.

## Totals

| Scope | Before open | After open | Before closed-this-turn | After closed-this-turn | After inferred witnesses |
|---|---:|---:|---:|---:|---:|
| All 455 record snapshots | 26,217 | 44,385 | 61 | 223 | 17,043 |
| 18 session tails | 923 | 873 | 2 | 7 | 333 |

The record-snapshot open total rises because the previous computations used a
50-record session window, while this replay deliberately used the complete
history. The session-tail measure is the comparable current-state total: it
falls by 50.

## Refusals

All eight refusals were `No acts for turn`. Their analyses contain only the
adapter-unknown intents `agree` and/or `challenge`, and their replies produce
no kinded reply act, so the adapter correctly has no timestamp-bearing act for
the requested turn.

| Record | Turn | Created (UTC) | Intents |
|---|---|---|---|
| `turn-5JBr6g` | `claude-3-turn-5` | 2026-10-04 14:46:23 | agree |
| `turn-JTuY1v` | `claude-12-turn-6` | 2026-10-09 04:05:18 | challenge, challenge |
| `turn-968NVf` | `claude-12-turn-131` | 2026-10-10 03:35:35 | challenge |
| `turn-qX81HV` | `claude-12-turn-145` | 2026-10-10 14:59:19 | challenge x3 |
| `turn-WtIZCU` | `claude-11-turn-33` | 2026-10-09 04:10:49 | challenge |
| `turn-mVytXT` | `claude-11-turn-49` | 2026-10-09 13:53:45 | challenge |
| `turn-VgUJ8K` | `claude-11-turn-53` | 2026-10-09 14:56:23 | challenge, agree |
| `turn-ABYrM4` | `claude-11-turn-54` | 2026-10-09 14:58:41 | challenge |

## Labelled session

Session `ec3a9b31-6656-4530-90db-53a22762c17f` agrees with the checkout
replay: exactly four of the 24 checkpoint rows close. The debugger proposal
has an explicit quoted-target witness; the other three use `:sole-port` and a
closing adjacency-table entry. Neither constraint closes.

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
| 10-11 02:36 | constrain | Let's not reinvent the wheel | open |
| 10-11 02:42 | propose | work with that extra futon1b | open |
| 10-11 02:42 | propose | hook it all up | open |
| 10-11 02:42 | constrain | don't read every agent turn twice | open |
| 10-11 02:42 | propose | agent chat is a crime scene | open |
| 10-11 02:44 | delegate | bell the work and plan to claude-16 | **closed: sole-port** |
| 10-11 03:36 | delegate | backfill to codex-70 | open |
| 10-11 03:36 | ask-action | restore the setting and backfill | **closed: sole-port** |

The session's latest eligible record moved from 62 to 59 open ports: the
explicit quote was already reflected before this replay, while the three new
sole-port closures account for the live delta.
