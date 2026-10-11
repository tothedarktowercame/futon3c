# Live recompute after the port declaration, 2026-10-11

Read/reload/recompute window: 2026-10-11 05:04--05:15 UTC. Source commit:
`e28ab5c8` (`port-kinds`, per-act `:port?`/`:standing?`, and `constrain`
removed from the port table). No JVM restart was performed.

## Reload receipt

The same three namespaces were reloaded from the master checkout through
`scripts/proof-eval.sh`:

```clojure
(require 'futon3c.logic.xiang
         'futon3c.xiang.turn-acts
         'futon3c.xiang.turn-service
         :reload)
```

The serving JVM returned this receipt for a synthetic turn containing one
operator `constrain` fragment and one marked report reply:

```clojure
{:port-kinds
 #{:delegate :promise :ask-action :offer :propose
   :report-problem :clarify :verify}
 :acts
 [{:kind :constrain :port? false :standing? true}
  {:kind :report :port? false :standing? false}]
 :ports {:still-open [] :inferred []}}
```

Thus the reloaded code treats the constraint as a standing and not a port.

## Population and execution

The before-state ledger was committed as `12d09f66` before the first call.
It selected every analysed operator record with nonblank reply text, excluding
placeholder sessions. Four records had arrived since the preceding replay:
459 records in 18 sessions were processed, per session in `created_at` order,
at no more than one call per second:

```clojure
(compute-ports! service record-id {:session-cap 10000})
```

451 calls succeeded. The same eight no-act records refused; there were no new
refusals. No record's `still_open` count grew.

## Totals

| Scope | Open before | Open after | Delta | Constraint before | Constraint after | Closed-this-turn after | Inferred after |
|---|---:|---:|---:|---:|---:|---:|---:|
| All 459 record snapshots | 44,541 | 38,940 | -5,601 | 5,537 | 51 | 215 | 16,582 |
| 18 session tails | 878 | 781 | -97 | 96 | 3 | 9 | 325 |

The before tail constraint count was 96, rather than the earlier census's
111; this ledger is the committed baseline immediately before this replay.

## Residual three constraint ports: reused act IDs

The expected tail constraint count was zero, but three remain in two sessions.
This is not a failure to stamp the constraint acts: each is stamped
`:port? false :standing? true`. Their stable IDs collide with older acts in
the same session because the agent turn counter was reused. The fact database
therefore contains a `port` fact from the older act under the same ID.

| Act ID | Older port fact | Later standing fact |
|---|---|---|
| `codex-19-turn-3-f-s1-0` | clarify, 2026-10-03 02:59 | constrain, 2026-10-04 01:54 |
| `codex-19-turn-3-f-s3-0` | propose, 2026-10-03 02:59 | constrain, 2026-10-04 01:54 |
| `claude-17-turn-43-f-s2-0` | clarify, 2026-09-27 18:38 | constrain, 2026-10-04 05:56 |

The affected session tails are `01a0ffa6-1850-7dc2-a283-9b6eb06d5913`
(two) and `564c8e50-c240-46fc-81ad-55afafbc63ee` (one). Repairing historical
act-ID collisions is outside this packet; no table or record was adjusted to
force the expected zero.

## Refusals

All eight are the prior `No acts for turn` set. Their analyses contain only
adapter-unknown `agree` and/or `challenge` intents, and their replies produce
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

Session `ec3a9b31-6656-4530-90db-53a22762c17f` has the required result:
four of the 24 labelled moves close, both labelled constraints are absent from
`still_open`, and the other 18 remain open. Its latest eligible snapshot moved
from 59 to 56 open ports because it contained one additional constraint beyond
the labelled 24.

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
