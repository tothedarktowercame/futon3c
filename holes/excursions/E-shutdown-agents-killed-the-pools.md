# Excursion: E-shutdown-agents-killed-the-pools — one scratch form, seven minutes of rejected tasks

**Date:** 2026-09-20
**Status:** CLOSED — pools reinstalled in the live JVM by attach; `/eval` now refuses the form
and survives the state it creates (`src/repl/http.clj`).
**Repo:** futon3c — `src/repl/http.clj` (the `/eval` tool bridge every CLI agent uses).

## What happened

At 17:09:08Z a scratch file (`/tmp/a-drop2-discovery.clj`, written for the A-drop-2 discovery
job) was POSTed to `localhost:6768/eval`. Its last form was `(shutdown-agents)` — the line
that makes a one-shot `clojure -M` script exit promptly instead of hanging on the agent
threads. The eval returned `:ok true`, with its output, and shut down
`clojure.lang.Agent/soloExecutor` and `pooledExecutor` for the whole JVM.

Those two pools are behind every `future`, `send`, `send-off` and `pmap` in the process, and
nothing in Clojure restarts them. What followed:

- Agency invoke rejected every task it tried to run: eight jobs failed between 17:08:13Z and
  17:11:50Z (claude-4, claude-12, codex-1, plus auto-bellbacks). Two were resends of Joe's
  ruling on the C-family fix; the resends failed the same way. The agents' surfaces showed
  only `RejectedExecutionException: Task ... rejected from ThreadPoolExecutor@53ce41e7
  [Shutting down, ...]`, which is how Joe first saw it.
- `/eval` answered every request with that same exception, because the handler evaluated in a
  `future` to get its timeout. So did Drawbridge `/repl`, which hands nREPL messages to a
  `future` too.
- There was therefore **no way to repair the JVM through the JVM's own eval surfaces** — the
  one form that fixes it, `(set-agent-send-off-executor! ...)`, could not be delivered.

## How it was repaired without restarting

A `java.lang.instrument` agent, attached to the live process, replacing the two static fields:

```java
// agentmain(String outPath, Instrumentation inst)
Class<?> agentCls = null;                       // find it via the loaded-class list, so the
for (Class<?> c : inst.getAllLoadedClasses())   // agent does not depend on its own loader
  if ("clojure.lang.Agent".equals(c.getName())) { agentCls = c; break; }
Field solo = agentCls.getField("soloExecutor");     // public static volatile, so settable
Field pooled = agentCls.getField("pooledExecutor");
solo.set(null, Executors.newCachedThreadPool(f("clojure-agent-send-off-pool-restored-")));
pooled.set(null, Executors.newFixedThreadPool(2 + Runtime.getRuntime().availableProcessors(),
                                              f("clojure-agent-send-pool-restored-")));
```

Build as a jar with `Agent-Class: RestoreAgent` in the manifest (include the inner/lambda
classes — a jar holding only the outer class fails at `NoClassDefFoundError RestoreAgent$1`),
then `VirtualMachine.attach(pid).loadAgent(jar, outPath)` from a second JVM with
`-cp` on the JDK's `jdk.attach`. Dynamic loading still works without flags on JDK 21.
Surviving threads keep the name they were created with, so a thread dump afterwards shows
`clojure-agent-send-off-pool-restored-N` alongside whatever old loops outlived the shutdown.

## The two changes that close it

1. **`/eval` refuses `(shutdown-agents)` and `(System/exit ...)` on the dev-serve profile**
   (`pool-destruction-attempt?`), in the same blunt textual style as the tools.namespace
   guard next to it, and for the same reason: it runs before parsing, and a false negative
   costs the running image. The dev-admin surface still allows them — a deliberate operator
   shutdown is not this incident.
2. **`/eval` no longer depends on the pool it needs to repair.** When `future` is rejected,
   the handler evaluates on the request thread, gives up the timeout rather than the
   endpoint, and marks the response `:on-request-thread true`. That is the path by which a
   future incident can be fixed with one POST instead of an attach.

Verified by making the state on purpose in a throwaway JVM: `(shutdown-agents)` through
`/admin/eval`, then `/eval` still answered (`:on-request-thread true`), then
`(set-agent-send-off-executor! (java.util.concurrent.Executors/newCachedThreadPool))` through
`/eval` put futures back. The incident file itself is refused on `/eval`, and a form that
merely mentions the name inside a string is not.

## Note for the agents

A file you intend to run both ways — `clojure -M -e` and POSTed to `/eval` — must not end
with `(shutdown-agents)`. The server is not a script; it has been up for days, and the form
is irreversible from Clojure. Nothing needs to replace it: an eval that returns leaves no
threads to wait on.
