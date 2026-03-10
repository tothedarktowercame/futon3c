# "Bell When Done" — Agent Completion Signalling

How agents signal task completion and how the system reacts.

## The Pipeline

```
Agent returns from work
       |
       v
Registry marks agent :idle
  (registry.clj mark-local-invoke-state!)
       |
       v
bell-tickle-available!  (dev.clj ~line 1930)
  |                |
  |                v
  |        Emits availability-bell evidence
  |        {:coord/type :agent-availability-bell
  |         :agent-id   "codex-1"
  |         :availability :available
  |         :invoke-status :done}
  |        Tags: :tickle :availability-bell :coordination
  |
  v
Post-invoke hook fires
       |
       v
fm-dispatch-mechanical!
  Check: agent idle? + obligations available? + not in cooldown?
  YES -> page agent with new work
  NO  -> pass
```

Agents don't explicitly ring a bell. The bell is implicit: the agent returns,
the registry transitions, and the availability signal + mechanical dispatch
fire as side effects.

## Registry Status Transitions

Two states: `:invoking` and `:idle`.

- **Mark invoking**: sets `:agent/invoke-started-at (Instant/now)`
- **Mark idle**: clears `:agent/invoke-started-at`, updates `:agent/last-active`

The FM conductor reads these to decide whether an agent is available for work.

## Mechanical Dispatch (Post-Invoke Hook)

Located in `dev/futon3c/dev/fm.clj`.

When an agent goes idle, the hook calls `fm-dispatch-mechanical!` which:

1. Checks: agent is idle, obligations exist, agent not in cooldown
2. If yes: pages agent via IRC with the next obligation
3. Cooldown: 3 minutes per agent after paging (prevents thrashing)

State tracked: `{:last-paged {agent-id -> epoch-ms}, :paged-obligations {agent-id -> #{ob-ids}}}`

## Bells and Whistles (Companion Primitives)

Both live in `src/futon3c/social/`.

| Primitive | Semantics | Blocking? | Use case |
|-----------|-----------|-----------|----------|
| Bell | "tell" | No (202) | Standup invitations, liveness checks |
| Whistle | "ask" | Yes | Query agent status, request review |

### Bell Types

- **Standup bell** (`ring-standup!`): joins agents into an IRC room for
  co-present conversation. Emits bell-ring + arrival evidence.
- **Test bell** (`ring-test-bell!`): liveness check via UUID secret ack.
  Currently **skipped** by tickle config — always falls through to IRC paging.

### Availability Bell

Not a bell in the `bells.clj` sense. It's a coordination event emitted by
`bell-tickle-available!` and consumed by the tickle agent as a special invoke
payload (`:coord/type :agent-availability-bell`).

## Standalone Tickle vs FM Conductor

Two systems can scan for stalled agents:

| System | Entry point | What it does |
|--------|-------------|--------------|
| Standalone tickle | `start-tickle!` | Periodic scan, page stalled agents via IRC |
| FM conductor | `start-fm-conductor!` | Obligation-driven dispatch + post-invoke hook |

Both default to **off** at boot (env `FUTON3C_TICKLE_AUTOSTART`,
`FUTON3C_FM_CONDUCTOR_AUTOSTART`). Use launch script flags:

```
./scripts/dev-linode-env --start-tickle    # standalone watchdog
./scripts/dev-linode-env --start-fm        # FM conductor
./scripts/dev-linode-env --start-all       # both
```

Running both simultaneously produces duplicate stall messages — the standalone
tickle pages agents that the FM conductor is already managing.

## Key Files

| File | Role |
|------|------|
| `src/futon3c/social/bells.clj` | Standup + test-bell implementations |
| `src/futon3c/social/whistles.clj` | Synchronous ask primitive |
| `src/futon3c/agents/tickle.clj` | Availability bell, watchdog scan/page/escalate |
| `src/futon3c/agency/registry.clj` | Agent status transitions |
| `dev/futon3c/dev/fm.clj` | FM conductor, post-invoke hook |
| `dev/futon3c/dev.clj` | `bell-tickle-available!` entry point |

## Invariant (T-3): No Feedback Loops

The conductor posts to IRC as "tickle" but never reads its own output.
Agent completion triggers the hook via registry state change, not via
IRC mention parsing.
