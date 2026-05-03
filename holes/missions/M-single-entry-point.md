Status: open

# M-single-entry-point: One JVM, one launcher for the local dev stack

**Phase:** IDENTIFY (2026-05-02)
**Parent:** M-the-futon-stack (Q4 — is the stack set up so the invariants can work?)
**Sibling:** M-reachable-from-boot (boot-time reconstruction discipline)

## Tension

The local dev stack accumulates orphaned long-running JVMs. As of 2026-05-02 the audit surfaced three:

| PID | What | Started | RSS | Reachable? |
|-----|------|---------|-----|-----------|
| 3394975 | `webarxana.server.core` Ring on :3100 | Apr 12 | 84 MB | parent shell long gone — orphaned to systemd |
| 3868165 | shadow-cljs `watch app` (webarxana UI) | Apr 14 | 431 MB | reparented to systemd after node parent died |
| 3840938 | shadow-cljs `watch app` (war-machine UI) | Apr 22 | 978 MB | same |

None had active connections. ~1.5 GB resident. They survived multiple `dev-laptop-env` restarts because nothing in the futon3c lifecycle owned them — they were started by hand and forgotten.

The pattern is a coupling weakness: dev infrastructure that operates outside the substrate's awareness can drift indefinitely. The boot-time invariants this stack runs (mission-doc, branch, artifact-live-copy) can't surface what they don't see, and JVM-level state — listening ports, RSS, process trees — is in that blind spot.

## Claim

A single invocation of `./scripts/dev-laptop-env` should be the sole launcher of every long-running dev process related to the futon stack. SIGINT to that launcher should stop everything cleanly. No orphan dev JVMs across restarts.

This is a structural commitment, not a literal "exactly one PID" rule — sub-processes are fine **iff** the launcher owns them and the shutdown hook tears them down.

## Scope in

- `webarxana.server.core` (Ring server on :3100)
- `shadow-cljs watch app` for war-machine UI (`/home/joe/code/futon0/web/war-machine`)
- `shadow-cljs watch app` for webarxana UI (`/home/joe/code/futon4/dev/web/webarxana`)
- The `dev-laptop-env` startup + shutdown sequencing for each
- Cyder registration for each subsystem so it appears in `(dev/status)` and the HUD
- Integration with the existing shutdown hook in `dev/futon3c/dev/bootstrap.clj` (already updated 2026-05-02 to stop the multi-watcher first)

## Scope out

- Production deployment of webarxana / war-machine
- Cross-machine federation (peer dev environments)
- Refactoring the webarxana or war-machine code itself
- Replacing shadow-cljs with another build tool

## Two candidate integration patterns (DERIVE)

### A. In-JVM embedding

Add `futon4/dev/web/webarxana` as a `:local/root` dep in futon3c's `deps.edn`. Mount `webarxana.server.core/start!` in `bootstrap.clj` next to the futon5 nonstarter block; register stop-fn with cyder.

For shadow-cljs: add `thheller/shadow-cljs` as a futon3c dep, call `(shadow.cljs.devtools.api/watch :app)` per project from the dev startup. Each project's `shadow-cljs.edn` discovered via path config.

- **Pro:** literally one JVM, single classpath, single shutdown. Watcher subsystems can post evidence directly to futon1a in-process.
- **Con:** shadow-cljs pulls hundreds of MB (ClojureScript + Closure compiler); risk of dep version conflicts with futon3c's existing classpath; shadow-cljs build-server has its own internal state that may not survive code-reload cleanly.

### B. Managed sub-processes

Dev script spawns `shadow-cljs watch app` per project as child processes via `ProcessBuilder`. Each process group is captured (`setsid` or `Process.toHandle()`), registered with cyder as a `:daemon`, and the shutdown hook SIGTERMs them before futon1a goes down. `webarxana.server.core` integrates in-JVM (it's just Ring; the heavy classpath is shadow-cljs).

- **Pro:** lower classpath risk; isolation between dev tooling and futon3c proper; current shadow-cljs setup unchanged in form, only in ownership.
- **Con:** still > 1 JVM at runtime (but single entry point); needs careful handling of stuck children (SIGTERM → wait → SIGKILL fallback).

Neither is decided. Default lean: **A for the Ring server, B for shadow-cljs** — pick the cheaper integration per subsystem.

## Open questions blocking ARGUE

- **Q-classpath-compat:** does adding `thheller/shadow-cljs` as a futon3c dep introduce conflicts? Test by building a deps.edn snapshot and running `clj -M:dev -e '(require [...])`.
- **Q-shadow-cljs-shared-server:** can two `(shadow/watch :app)` calls in the same JVM coexist if they reference different `shadow-cljs.edn` configs in different working dirs? (shadow-cljs has historically assumed CWD; needs verification.)
- **Q-orphan-survival:** the 2026-05-02 audit found shadow-cljs JVMs **reparenting to systemd** after their node parent died. Pattern B needs a robust process-group strategy so child JVMs *can't* survive the launcher.
- **Q-existing-bootstrap-shape:** how is the current `start-futon5!` / `start-irc!` pattern in `bootstrap.clj` shaped? New subsystems should match that convention rather than invent a new one.

## Completion criteria

1. After a fresh boot, every long-running JVM the user expects (webarxana Ring, war-machine UI hot-reload, webarxana UI hot-reload) is reachable.
2. After SIGINT to `dev-laptop-env`, `ps -ef | grep -E '\bjava\b' | grep -v grep` shows no orphans (no parent==1 reparenting).
3. `(dev/status)` lists each new subsystem.
4. A second `./scripts/dev-laptop-env` invocation while one is already running fails fast with a recognizable error (no double-launch silent collision on shared ports).

## Risk register

- **Classpath fragility (A):** version skew between shadow-cljs and futon3c's existing deps could surface only in the wild.
- **Process-group escape (B):** as the 2026-05-02 audit demonstrated, naive `ProcessBuilder` lets children survive parent death. Needs `setsid` or equivalent.
- **Hidden state in shadow-cljs:** the build server caches things on disk; restart semantics may differ from the user's mental model.
- **Operator regression:** any change to the launcher script changes muscle memory; transient brittleness expected during rollout.

## Evidence (2026-05-02 audit)

Recorded in M-the-futon-stack.md Checkpoint 2026-05-02 ("substrate self-diagnosed and self-repaired") under the latent-risk addition: **silent-failure-in-counters** is the same shape as **silent-survival-of-orphan-processes** — counters and process trees both surface only what's queried; absent queries, things drift.

## Next-move

DERIVE → run Q-classpath-compat (build a futon3c branch with shadow-cljs added; load deps; run tests). If green, commit to Path A for shadow-cljs. If red, commit to Path B and design the process-group escape mitigation.
