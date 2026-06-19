# E-drive-prior-art — survey of the stack's previous answers to the drive problem

**Date:** 2026-06-11 (post Arc-2, pre autorunner-discussion)
**Author:** fable-1 (ground control), at Joe's direction
**Question:** Arc-2's autorunner failed five ways. The stack contains at
least six prior attempts at the same problem — keeping work moving without
the operator hand-cranking it. What exists, what state is each in, and what
do they compose into? Per Joe: everything except peripherals-proper gets
read with suspicion; the WM should *be* a peripheral.

## The three concrete questions, answered first

1. **Is the WM registered with Cyder?** HALF. `war-machine-scheduler` (the
   scan loop) IS registered — `:daemon`, layer `:repl`, with a real
   `:step-fn` (scheduler.clj:223). **The WM *pilot* is NOT registered at
   all** — its begin/close cycles run as ad-hoc Drawbridge evals with no
   process record, no state-fn, no surface.
2. **Is it steppable?** The scheduler yes; the pilot no — and the pilot is
   the thing arcs need to step.
3. **Can Tickle regulate it?** Not today: Tickle's watchdog reads agent
   liveness (agency registry + evidence timestamps) and its conductor
   assigns work items, but nothing connects either to a Cyder-registered,
   steppable WM-pilot process — because there isn't one.

## The inventory, with suspicion ratings

| artifact | what it actually is | state | suspicion |
|---|---|---|---|
| **peripheral/cycle.clj** | generic phase-gated cycle machine (proof + mission peripherals instantiate it); tool envelopes, required outputs, fruit-fn | LIVE, mature, multiply-instantiated | **trustworthy** (Joe's exemption confirmed by reading) |
| **Cyder** (cyder.clj, 278 ln) | process registry: every long-running process registers (I-6), stoppable (I-7), inspectable (I-8), one surface (I-9), REPL-layer additionally steppable/jackable (I-10). Two layers `:repl`/`:infra` | LIVE — 14 registered processes right now | **registry sound; COVERAGE half-baked** — pilot absent; several `:repl` entries lack the step-fn their layer promises (M-proof-peripheral, M-tickle-overnight registered as state-machines with `step-fn? false`) |
| **process_watchdog** (363 ln) | passive monitor over Cyder-style process records; stall detection + alert/recovery events | LIVE and battle-proven (caught the multi-watcher venv wedge 06-10) | **low** — but it DETECTS, never drives; it is the alarm, not the metronome |
| **tickle.clj** (346 ln) | passive agent-liveness watchdog: infers stalls from shared evidence timestamps, "umwelt darkness" (never reads agent internals), pages/escalates | code present; not running as a registered process now | **low on design, medium on wiring** — the umwelt-dark principle is right; nothing arms it |
| **tickle_orchestrate** (878 ln) | conductor mode: fetch → assign → review → report workflows; FM proof round-robin | code present | **HIGH** — requires `clojure.java.shell` and invokes agents by shelling out, predating the hardened registry invoke path (I-1/I-2 smell: transport creating processes). Its *workflow shapes* are good prior art; its *invocation layer* is superseded |
| **tickle_queue** (325 ln) | task pool + idle-bell dispatch: priorities, dependencies, per-agent failure tracking; "the queue is the single source of truth for work dispatch" | code present, registry `!on-idle` seam exists | **medium** — the right SHAPE (this is what arc-2's kick log specifies), but unwired end-to-end and unexercised under load |
| **tickle_work_queue** (329 ln) | domain-specific overnight feeder (313 PlanetMath CT entries through extraction) | one-campaign artifact | **high for reuse** — evidence that overnight batching worked once; not a general mechanism |
| **M-tickle-overnight / M-sliding-blackboard** | state-machine residue registered in Cyder from earlier campaigns | stale registry entries | cleanup candidates |
| **pocketwatch** (futon5a, stack-annotations tick entities) | tick-warning instrument over measured operator work-patterns (hermit/hobby/foraging/cargo warnings keyed to thresholds like stack-commit-share >70%), feeding WM beliefs | LIVE as belief inputs (visible in every judgement's per-tick prediction maps) | **low as instrument** — and the interesting precedent: ticks conditioned on MEASURED STATE, not wall-clock. The state-aware pacing idea already exists here |
| **orchestration patterns** (library/) | consent-gate, pattern-warranted-choice-point | live vocabulary | trustworthy |

## What they compose into (the convergent design, not yet built)

Every piece of the arc-3 driver exists; none of them are connected:

1. **The WM pilot becomes a real peripheral process**: registered with
   Cyder (`:peripheral`, layer `:repl`) with honest contracts —
   `:state-fn` = current cycle/turn/arc position (the inspectability Joe
   demanded), `:stop-fn` = the visible stop control, and `:step-fn` that
   respects I-3 (inhabited, not delegated): a step does not impersonate the
   pilot, it **dispatches the next-cycle task to the pilot agent** through
   the queue → idle-bell → registry invoke. Stepping = requesting one more
   inhabited turn.
2. **tickle_queue is the task pool** the step draws from — and the
   STATE-AWARENESS arc-2 lacked lives here: pilot reports (wall-report,
   blocked-report) update/reorder the pool instead of being flown past.
3. **tickle.clj watchdog + process_watchdog stay the passive alarms**
   (agent-liveness and process-liveness respectively) — detection separate
   from drive, as now.
4. **Pacing conditions pocketwatch-style**: the conductor's tick rate keyed
   to measured state (pilot idle + pool non-empty + no stop-the-line), not
   bare wall-clock.
5. **tickle_orchestrate's workflow shapes** (assign → review → report)
   inform the task types; its shell-invocation layer is NOT carried over.

Arc-2's five autorunner failures map onto this design as requirements:
no-drive → step-fn exists; slow-metronome → pacing conditions; session-
private → Cyder registration + surface; killed-by-reset → JVM-resident;
state-blind → queue updated by pilot reports.

## Cleanup list (deferred until the solution is in, per Joe)

- tickle_orchestrate: supersede shell-invocation with registry invoke;
  keep workflow shapes; then retire or rewrite (878 lines, most at risk).
- tickle_work_queue: archive as campaign artifact (its resumability trick
  — skip-on-existing-evidence — is worth keeping as a pattern).
- Cyder registry hygiene: stale state-machine entries
  (M-tickle-overnight, M-sliding-blackboard); `:repl`-layer entries
  without step-fns violate their own layer contract — either supply or
  reclassify.
- The session-side autorunner scripts (/tmp) are already deleted.
- Naming: five tickle_* namespaces for four different concepts invites
  the next half-baked addition; the consolidation should leave ONE
  conductor ns + ONE watchdog ns.
