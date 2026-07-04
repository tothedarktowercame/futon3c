# E-codex-resume-after-JVM-restart — give codex a JVM-owned supervisor so it resumes like claude

**Status: IDENTIFY (2026-07-04). Owner: claude-10 (dispatch/review). Ratified by Joe
2026-07-04 (emerged from a live codex-4 resume failure after a JVM crash).**

## The gap (operator intake)

Resume-after-JVM-crash "just works" for **claude** agents but not **codex** agents.
Joe's prior: they were assumed similar. The live symptom (2026-07-04): after the JVM
crashed, `codex-4` came back as a **ghost** — roster entry `status: "restored"`,
session-id preserved (`019f2d52…`), but **no process**, `last-active` frozen ~1h
back. Typing in the codex REPL + RET did nothing (emacs frozen on a synchronous
submit to the dead-but-registered agent).

## Root cause — it is NOT session persistence; it is process ownership

Both agents persist their session on disk and both have a native `--resume`:

| | session persisted? | who owns the process | resume after crash |
|---|---|---|---|
| **claude** | yes (transcript + `claude --resume`) | **JVM pouch (a supervisor)** | **automatic** |
| **codex**  | yes (rollout + `codex … resume`)      | **external** (emacs/codex-picker); JVM only *detects* | **manual** |

- **claude is JVM-owned.** `agent_pouch.clj:4` — one pouch owns "one long-lived
  `claude --print --input-format stream-json`"; `agent_pouch.clj:310` (re)spawns it
  with `["--resume" session-id]`. So the pouch is a **supervisor**: whenever the
  process is missing (evicted/crashed), the next turn re-spawns `claude --resume`
  from disk. The JVM owns the whole lifecycle → resume is automatic.
- **codex is external + only observed.** `registry.clj:1074` — "Best-effort
  **detection** of local `codex exec --json resume <sid>` processes … to **surface
  external Codex activity** (e.g. emacs codex-repl)." codex is launched *outside* the
  JVM (`codex-picker`, emacs codex-repl); the JVM merely detects/surfaces it. Nothing
  owns or re-spawns it. So "restore" = metadata only; the dead process stays dead
  until a human runs `codex-picker`.

So the warm pouch is not just a session cache — it is also the **supervisor** that
auto-respawns. codex has the session-equivalent (`codex exec --json resume <session>`
— the very command `registry.clj` greps for) but **no supervisor-equivalent**. That
missing ownership layer is the whole gap.

## What would close it

A **"codex pouch"**: a JVM-owned supervisor that re-spawns
`codex exec --json resume <session>` on demand, mirroring `agent_pouch`'s
`claude --print --resume`. Not new persistence (codex has it) — just the
ownership/supervision that makes resume automatic. On a `restored`-with-no-process
codex, a queued turn (or a poll) would trigger the respawn instead of hanging.

Kill criterion: if it turns out codex's external-launch model is load-bearing for a
reason we don't yet see (permissions, TTY, the emacs surface owning the pty), this
may need to stay operator-launched — in which case the win shrinks to "detect ghost +
surface a one-click resume" rather than full auto-respawn. Decide from the findings.

## Cross-refs

`agent_pouch.clj` (the claude supervisor to mirror), `registry.clj:1074` (codex
detection), `scripts/codex-picker` (the manual resume path + session-id file
`/tmp/futon-codex-session-id`), M-kangaroo / [[project_kangaroo]], E-zai-agent-upgrades
U1 (transcript persistence — the adjacent non-claude-durability thread).
