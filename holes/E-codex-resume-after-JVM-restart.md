# E-codex-resume-after-JVM-restart — give codex a JVM-owned supervisor so it resumes like claude

> **Closed 2026-11-08 (already done).** Agency now owns the Codex process per turn: a restored seat is rebuilt through `/api/alpha/agents/restore` (`roster_store/restore-on-boot!`), and each turn spawns `codex exec resume <sid> -` (`src/futon3c/agents/codex_cli.clj:310`), so no long-lived pouch is needed. Live check: codex-7 ran a job on session 01a1a3df-... at 2026-11-06T11:07Z, Agency restarted at about 21:09Z, and on 2026-11-07T19:30Z it ran a job (done) on the same session. Evidence: futon3c 89fd1159d (roster persistence), 5fc795979 (restored Codex seats run jobs, checked on solve-1), test `transport/http_test.clj` `agent-restore-registers-codex-exact-identity`. The Emacs codex-repl hang on a ghost seat (the 07-04 symptom) was not re-tested. Checked by c3a in the closure pass (claude-6, FIX/WONTFIX sheet).

**Status: done, closed 2026-11-08, see the closure note (was: IDENTIFY (2026-07-04)). Owner: claude-10 (dispatch/review). Ratified by Joe
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

## Closure criteria (provisional, 2026-10-09)

_Drafted from this document's own stated goals during the War Machine status classification (zai-1, high confidence); not yet confirmed by the author._

- [x] Codex agents get a JVM-owned supervisor mirroring the claude agent_pouch path (per-turn exec resume: codex_cli.clj:310; 89fd1159d, 5fc795979)
- [x] After a JVM restart, a codex agent resumes its session automatically (no ghost roster entry, last-active live), verified by a restart test (live: codex-7 same session across the 2026-11-06 restart; no automated restart test)
