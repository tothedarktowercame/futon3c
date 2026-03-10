# Conductor Safety Net Spec

The bell-driven conductor dispatches tasks immediately when agents bell idle.
The safety net is a periodic check (default 20 minutes) that catches anything
the bell pathway missed. It should not nag — it should diagnose and act.

## What the safety net checks

### 1. Stuck agent detection

An agent is "stuck" if:
- Status is `:invoking`
- `invoke-started-at` is more than `stuck-threshold-ms` ago (default: 15 min)
- No `invoke-activity` update in the last 10 minutes

**Data available now** (in `registry.clj`):
- `:agent/invoke-started-at` — Instant when invoke began
- `:agent/invoke-activity` — last activity string (updated by agent tooling)
- `:agent/status` — `:idle` | `:invoking`

**Action**: Log a warning. If the agent has been stuck for 2 consecutive
safety-net cycles (40 min), escalate to mentor via `whistles/whistle!`.
Do NOT kill the invoke — it may be doing real work (e.g., codex SAT search).

### 2. Orphaned assignments

A task is "orphaned" if:
- Status is `:assigned` in the task queue
- The assigned agent is `:idle` in the registry (invoke finished but
  `complete-task!` / `fail-task!` was never called)

This happens when the dispatch future crashes without reaching the
complete/fail path.

**Action**: Call `fail-task!` for the orphaned assignment, returning the
task to `:pending`. Log the recovery. The next idle bell (or safety-net
drain) will re-dispatch it.

### 3. Unconsumed bells

Bells that arrived between cycles but weren't consumed by the immediate
`on-idle` handler. This is what `dispatch-from-queue!` already does.

**Action**: Drain and dispatch (existing behavior, keep it).

### 4. Idle agents with available tasks

An agent is idle, tasks are available, but no bell fired (e.g., tasks were
seeded after the agent went idle, or the agent was registered after the
conductor started).

**Action**: For each idle agent, call `dispatch-task!` once. This is the
"catch cold agents" path.

### 5. Task pool health summary

Every cycle, emit a one-line summary to the server log:

```
[safety-net] 3 tasks (1 available, 1 assigned, 1 completed) | agents: codex-1=invoking(12m) claude-2=idle | no issues
```

or:

```
[safety-net] WARNING: codex-1 stuck (38m, no activity) — escalating to mentor
```

This gives joe a glanceable log without IRC noise.

## What the safety net does NOT do

- **Page agents on IRC** — that's the old tickle behavior. No unsolicited
  IRC messages from the safety net.
- **Kill stuck invokes** — an invoke that's been running for 30 minutes
  might be doing real work. The safety net reports, it doesn't kill.
- **Reprioritize tasks** — priority is set when tasks are seeded. The
  safety net dispatches what's available, it doesn't reorder.
- **Create new tasks** — task creation is a mentor/principal decision,
  not a mechanical one.

## Escalation ladder

```
0-20 min   : bell-driven dispatch handles everything
20 min     : safety-net cycle 1 — drain bells, catch orphans, log summary
40 min     : cycle 2 — if same agent still stuck, whistle mentor
60 min     : cycle 3 — if mentor whistle got no response, log for joe
```

The escalation is: observe → log → whistle mentor → log for principal.
At no point does the conductor take autonomous corrective action beyond
returning orphaned tasks to the pool.

## Implementation sketch

```clojure
(defn safety-net-cycle!
  "One safety-net pass. Called every step-ms by the conductor loop."
  [config conductor-state]
  (let [;; 1. Drain unconsumed bells (existing)
        queue-results (dispatch-from-queue! config conductor-state)
        ;; 2. Detect stuck agents
        stuck (detect-stuck-agents {:threshold-ms (* 15 60 1000)})
        ;; 3. Recover orphaned assignments
        orphans (recover-orphaned-tasks!)
        ;; 4. Catch idle agents with available tasks
        cold-dispatches (dispatch-to-idle-agents! config)
        ;; 5. Escalate if needed
        _ (when (seq stuck)
            (escalate-stuck! stuck config conductor-state))
        ;; 6. Log summary
        _ (log-safety-summary! queue-results stuck orphans cold-dispatches)]
    (concat queue-results cold-dispatches)))
```

### detect-stuck-agents

```clojure
(defn- detect-stuck-agents [{:keys [threshold-ms]}]
  (let [now (Instant/now)]
    (->> @reg/!registry
         (filter (fn [[_ a]]
                   (and (= :invoking (:agent/status a))
                        (:agent/invoke-started-at a)
                        (> (.toMillis (Duration/between
                                        (:agent/invoke-started-at a) now))
                           threshold-ms))))
         (mapv (fn [[aid a]]
                 {:agent-id aid
                  :invoking-since (:agent/invoke-started-at a)
                  :last-activity (:agent/invoke-activity a)
                  :elapsed-ms (.toMillis (Duration/between
                                           (:agent/invoke-started-at a) now))})))))
```

### recover-orphaned-tasks!

```clojure
(defn- recover-orphaned-tasks! []
  (let [assigned (:assigned @tq/!task-queue)]
    (->> assigned
         (filter (fn [[agent-id _]]
                   (= :idle (:agent/status (get @reg/!registry agent-id)))))
         (mapv (fn [[agent-id task-id]]
                 (tq/fail-task! agent-id)
                 {:recovered task-id :from agent-id})))))
```

## Configuration

| Key | Default | Description |
|-----|---------|-------------|
| `:step-ms` | 1,200,000 (20 min) | Safety-net cycle interval |
| `:stuck-threshold-ms` | 900,000 (15 min) | Time before agent flagged stuck |
| `:escalate-after-cycles` | 2 | Stuck cycles before whistling mentor |
| `:mentor-agent-id` | `"claude-2"` | Who to whistle on escalation |
