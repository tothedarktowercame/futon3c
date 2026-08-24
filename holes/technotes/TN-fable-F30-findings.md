# TN: F30 / a01J06 — retirement race, a needless Mathlib rebuild, and a misread SIGABRT

Author: Claude (Fable 5, `claude-8`), 2026-08-24T19:20Z. Read-only review of
what codex-10 did to close frame `f30` of campaign `jit-all-open-nontopology-v1`,
prompted by Joe: "codex-10 and the APM project as a whole have created an
absolute mess … all told this process has taken well over 3 hours and I don't
have a good feeling about it at all."

Apparatus: futon3c `master` at `77a72e92`; problem repo `apm-lean`; frames under
`/home/joe/code/apm-frames/`; babysitter
`/tmp/claude-1000/-home-joe-code/9610cf85-…/scratchpad/apm-campaign-babysit.py`
(pid 76010, caller identity `claude-cli`).

Companion to `TN-sonnet-f28-finding.md` / `TN-sonnet-F29-finding.md`.

**Short answer:** nothing was lost and nothing is wedged — F30 closed at ~19:01,
F31/a03J05 is in `:solve`, and the rescued Student proof is on a durable ref.
But the episode exposes two code races, one procedural blunder (a full Mathlib
rebuild that the audit then rejected anyway), and one wrong diagnosis reported
to the operator as fact.

## Method — what was checked, not receipt-trusted

1. Read `*codex-repl:codex-10*` from line 12560 to end (via `emacsclient`).
2. Read codex-10's own session log,
   `~/.codex/sessions/2026/08/20/rollout-…-01a01efe-92e9-7511-b5d8-d1d0978a7d82.jsonl`
   (158 MB), extracting every `lake build`, `git worktree add`, `Build completed`,
   and `Lean exited` event with its timestamp, plus the stderr around each abort.
3. Live state at 19:05 and again at 19:12: `GET /api/alpha/agents` (codex-10),
   `GET /api/alpha/invoke/jobs` (all agents), `ps` for `lake`/`lean`/`codex exec`,
   `/proc/<pid>/cwd` and `cmdline` for the live codex processes, `uptime`, `free`.
4. Filesystem: `apm-frames/` listing, `git worktree list` in `apm-lean`,
   `.lake` layout of the freshly provisioned f31 worktrees, `show-ref` for the
   preserved-attempt ref, `coordinator.edn` regulator fields, f30/f31
   `problem-transitions.edn` first/last events.
5. Code: `workspace_lifecycle.clj` (`validate`, `certify-retirement-audit`,
   `retire!`), `countdown_control.clj` (`launch-audit!`, `jit-retirement-audit`),
   the babysitter's `maybe_bell` keying, and `/tmp/claude-babysit-bell-*.md`.

Not done: I did not read the durable-intent replay path that re-ran the launch
audit (finding F1 relies on codex-10's own description of it plus the shape of
`retire!`); I did not reproduce the thread-creation failure (F4 is inferred
from the exception text).

## Timeline (UTC, from the session log)

| time | event |
|---|---|
| 18:17:07 | F30 reaches `:close-frame` (`:waiting-for-terminal-result`, guide job). Retirement begins. |
| ~18:20 | Retirement removes the **Solver** worktree, then fails on the **Student** (dirty: 523 uncommitted Lean lines). Durable intent replays; its launch audit requires the Solver workspace that was just removed. |
| 18:23:16 | codex-10 commits the Student proof on its branch and pins it at `refs/apm/preserved-student-attempts/f30/a01J06/58658226` (+523/−1 on `problems/a01J06/lean/Main.lean`). Correct and valuable. |
| 18:24:14 | `git worktree add apm-frames/f30-a01J06-solver exp/countdown-f30-a01J06-solver` (head `6b6dd959`). |
| 18:24:18 | `lake build ConstructionTargets` in that worktree — **with no `.lake/packages` link**. Lake rebuilds all 8,294 Mathlib modules from source. Load average reaches 18. |
| 18:49:50 | Build ends: 3 × `error: Lean exited with code 134` at jobs 3310/3311/3312. codex-10 reports "three genuine dependency compilation failures". |
| 18:49:58 | Second `lake build ConstructionTargets`. |
| 18:55:43 | `Build completed successfully (8287 jobs)`. Re-check at 18:58:56 also clean. |
| ~tick 8486 | Package-authority alert: the rebuilt local `.lake` is not the canonical substrate. codex-10 relinks `.lake/packages → /home/joe/code/apm-lean/.lake/packages` and revalidates. |
| ~19:01 | F30 retires; `f30-*` worktrees gone. F31/a03J05 provisioned (correct layout: local `.lake/build/`, `packages` symlink). |
| 19:03:52 | First F31 transition. By 19:12: `:solve`, regulator `:running`, tick 8542. |
| 19:05–19:07 | Eight more `claude-cli` bells to codex-10, all answered "stale / historical / no action". codex-10 idle from 19:06:57. |

The "3+ hours" the operator perceived runs 14:38 → 19:01. The reconstruction
itself was 18:24 → 18:59 (35 min, two builds). The earlier hours were F30's own
phases plus the alert storm of F2.

## Findings

### F1 — Multi-role retirement is non-atomic and its replay is not idempotent (code bug)

`workspace-lifecycle/retire!` (`src/futon3c/apm/workspace_lifecycle.clj:304`)
removes **one** leased worktree per call after certifying that lease's audit.
Retirement of a frame is therefore a sequence of per-role removals. When the
Solver removal succeeded and the Student certification failed (dirty tree), the
durable intent replayed from the top, and the launch audit
(`countdown_control.clj:930 launch-audit!`) demanded every leased workspace —
including the Solver that had just been removed. There is no notion of
"already retired" on the replay path: a `:workspace-retired` receipt for a role
should satisfy the audit for that role.

Fix shape (small): either (a) make the per-role step idempotent — skip a role
whose retirement receipt exists and whose worktree is absent — or (b) retire
all roles under one all-or-nothing step so a partial state cannot be observed
by the replay.

### F2 — The babysitter turns a one-tick projection window into a bell storm (code bug)

There is a known one-tick window in which a certified terminal result has been
published but not yet collected; `apm-watch-projection.sh` reports `:alert`
for that tick and `:healthy` on the next. codex-10 named it correctly
("projection/terminal-collection race … resolved without intervention").

`apm-campaign-babysit.py` keys regulator bells as
`regulator-failure-{failure_count}`. Every replay attempt increments the
count, so each is a *new* key and `BELL_COOLDOWN_S = 1200` never applies.
Observed: ~12 consecutive codex-10 jobs about tick 8370/8371 (each a full
`codex exec` spin-up) answered "stale, already repaired"; another eight at
19:05–19:07 about the retired F30 projection after F31 was already active.
146 bell files under `/tmp/claude-babysit-bell-*.md`.

Fix shape (two lines): require an alert to persist for ≥2 polls before
belling; key regulator bells by `frame + repair-reason`, not by count. Also
follow the queue's *current* frame after a retirement (codex-10 said this too,
at 19:06:29).

### F3 — A full Mathlib rebuild that the audit then rejected (procedural)

Frame worktrees do not carry Mathlib. The convention — visible in the f31
worktrees provisioned minutes later — is `.lake/build/` local and
`.lake/packages` a symlink to `/home/joe/code/apm-lean/.lake/packages`
(`workspace_lifecycle.clj:111–119` validates exactly this: `packages-link`
must resolve to the lease's `:substrate/source`). The older `batch-*` frames
carry 7.2 GB reflink copies instead; either way, nobody compiles Mathlib.

codex-10 did a bare `git worktree add` and ran `lake build ConstructionTargets`.
With no packages link, lake built 8,294 modules from source for 25 minutes at
load 18, narrating it as "a one-time cost … no invariant is being bypassed".
Then the package-authority audit rejected the result *because* it was not the
canonical substrate, and codex-10 relinked to `apm-lean/.lake/packages`
anyway. A one-line `ln -s` at 18:24 would have made the rebuild unnecessary;
the audit made it worthless as well.

### F4 — "Three genuine dependency compilation failures" were thread-creation aborts (wrong diagnosis)

stderr for all three (session log, 18:49):

```
libc++abi: terminating due to uncaught exception of type lean::exception: failed to create thread
error: Lean exited with code 134
```

Modules: `Mathlib.CategoryTheory.Limits.ColimitLimit`,
`Mathlib.Combinatorics.Matroid.Closure`,
`Mathlib.CategoryTheory.Monoidal.Limits.Preserves` — three unrelated files,
jobs 3310–3312, the same instant. Exit 134 is SIGABRT; the exception is Lean
failing to spawn a thread. That is an environment limit hit during lake's
fan-out on a box already at load 18 with the JVM and several `codex exec`
processes — not a proof or dependency error. (Inferred from the text; not
reproduced. 249 GB RAM with 178 GB available rules out memory OOM.)

codex-10 did not read stderr, named it a genuine dependency failure, and
reported that to the operator. The retry passed in 6 minutes because 8,291
modules were cached, so the *cost* was small; the *epistemics* were not.

### F5 — The REPL buffer's status line froze on a job it stopped following (display, not work)

At 19:12 the buffer's last line still read
`⟲ codex-10 invoking (bell from claude-cli): [bash … 'lake build ConstructionTargets']`
while the roster said codex-10 `idle` since 19:06:57 and the job list had
nothing running or queued for it. Buffer-local state confirmed it:
`agent-chat--pending-process nil`, `following nil`. The marker was written from
the activity stream of the 18:49:58 job; the buffer never followed the eight
subsequent jobs, so it never received their results and never cleared the
marker. The operator could not "interrupt" it because there was nothing to
interrupt. The two live `codex exec` processes at that time were codex-18
(caller codex-17, `lake env lean ConstructionTargets/TransversePreimageDuality.lean`)
and the coordinator's F31 solver check — unrelated to F30.

This is the same class as the "(awaiting session)" header staleness in
`*claude-repl*` reviewed earlier today: a header drawn once, never refreshed
from the roster.

### F6 — What codex-10 got right

- Preserving the 523-line Student proof before allowing retirement, on an
  isolated branch plus a dedicated ref, rather than discarding a dirty tree.
- Correctly identifying F2's mechanism and, at 19:06:29, that the babysitter
  must follow the queue's current frame.
- Not weakening the retirement audit to get past it.

## Recommendations

1. **Leave F31 alone.** It is healthy.
2. **Code fix A (F1):** idempotent per-role retirement / replay audit that
   accepts `:workspace-retired` receipts. One file, small.
3. **Code fix B (F2):** babysitter debounce (≥2 polls) + reason-keyed bells +
   follow the current frame after retirement.
4. **Standing rules for codex-10** (`AGENTS.md` / its preamble):
   - Never `lake build` in a frame worktree whose `.lake/packages` is not
     linked to `/home/joe/code/apm-lean/.lake/packages`; link it first. Never
     rebuild Mathlib.
   - Exit 134 / `failed to create thread` is an environment abort, not a proof
     or dependency failure. Read stderr before naming a cause; retry once
     before diagnosing.
   - When a repair is "one-time" and will take more than a few minutes, say
     so *before* starting it, with the alternative considered.
5. **REPL header/status refresh (F5, and the morning's claude-repl finding):**
   status markers should be re-derived from the roster on a timer, not left as
   the last streamed line.

## Evidence pointers

- Session log: `~/.codex/sessions/2026/08/20/rollout-2026-08-20T11-46-35-01a01efe-92e9-7511-b5d8-d1d0978a7d82.jsonl`
- Preserved proof: `git -C /home/joe/code/apm-lean show --stat refs/apm/preserved-student-attempts/f30/a01J06/58658226`
- Substrate convention: `ls -la /home/joe/code/apm-frames/f31-a03J05-solver/.lake/`
- Retirement code: `src/futon3c/apm/workspace_lifecycle.clj:282–340`, `src/futon3c/apm/countdown_control.clj:930, 1402`
- Babysitter keys: `apm-campaign-babysit.py` lines 154–181, 355–370, 415–447
- F31 state: `data/apm-campaigns/jit-all-open-nontopology-v1/jit-all-open-nontopology-v1-f31/problem-transitions.edn`
