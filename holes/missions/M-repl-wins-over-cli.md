# Mission: REPL Wins Over CLI
Status: parked

**Date:** 2026-03-29
**Status:** :testing
**Owner:** Claude (claude-repl.el, agent-chat.el, dev.clj), Joe (testing, direction)
**Cross-ref:** M-apm-solutions (first consumer), M-walkie-talkie (ArSE endpoints), M-structural-law (invariants), M-peripheral-gauntlet (peripheral contract)
**Repos:** futon3c (emacs/, dev/, src/transport/http.clj)

## The Problem

The CLI (`claude -p` in Alacritty, `codex` in a terminal) "just works" for
basic interaction: interrupt stops the agent, tool calls show full detail,
the interface is responsive. But the CLI is a dead end for the futon stack:

- No evidence logging (violates the invariant: **all substantive agent turns
  are logged as evidence**)
- No agent identity continuity (each CLI session is a disposable one-shot,
  violating I-1)
- No peripheral integration (no proof cycles, no ArSE, no Tickle orchestration)
- No frame persistence (work vanishes when the terminal closes)
- No coordination (can't route IRC, can't share sessions across workspaces)

The Emacs REPL surfaces (claude-repl, codex-repl) have all of these
capabilities via Agency. But their basic UX is worse than the CLI in ways
that make Joe reach for the terminal every time. The mission is to close
every UX gap so that the REPL is strictly better than the CLI, making the
CLI a legacy interface that incurs evidence debt.

## Success Criteria

The REPL is "winning" when Joe stops opening Alacritty for Claude/Codex
interaction. Concretely:

1. **Interrupt works** — C-c C-c kills the server-side process tree, not
   just the local curl. The agent actually stops.
2. **Tool visibility** — tool calls show what was requested (input) and
   what came back (output), not just `[Bash]` markers.
3. **Frame persistence** — every turn is a frame in SQLite with prompt,
   assistant text, tool events, and tool output. Queryable after the fact.
4. **Cursor-over inspection** — hovering over a tool marker in the chat
   buffer pops up the frame detail (posframe on GUI, transient window on
   terminal). No side windows stealing screen space.
5. **Cursor stability** — streaming text does not yank the cursor or scroll
   the view when the user is reading earlier content.
6. **Port discovery** — the REPL finds the running Agency server without
   manual configuration (multi-base probe like codex-repl).
7. **Attach to agents** — completing-read over live registered agents, not
   a raw string prompt.
8. **Session continuity** — agent identity, session file, evidence chain
   survive buffer clear, reconnect, and Emacs restart.

## What Landed (2026-03-29, first session)

### Server-side (dev.clj)

| Change | Status |
|--------|--------|
| Enriched `tool_use` events with `tool_details` (name, id, input) | Done, pending reload |
| `tool_result` events forwarded through invoke sink | Done, pending reload |
| `register-invoke-control!` in Claude invoke path | Done, hot-swapped |
| `clear-invoke-control!` in finally block | Done |

### Client-side (agent-chat.el)

| Change | Status |
|--------|--------|
| `agent-chat-popup-show/hide/toggle` (posframe + terminal fallback) | Done |
| `agent-chat--server-interrupt` POST to interrupt-invoke endpoint | Done |
| `agent-chat-interrupt` calls server-side interrupt | Done |
| `agent-chat-scroll-to-bottom` respects user cursor position | Done, needs debugging |
| `futon3c-blackboard-close-agents-windows` also closes `*invoke:*` | Done |

### Client-side (claude-repl.el)

| Change | Status |
|--------|--------|
| `defvar` -> `defvar-local` for multi-buffer isolation | Done |
| `claude-repl-buffer-name` defcustom | Done |
| `claude-repl--open-instance` generalized entry point | Done |
| `claude-repl-attach-agent` with completing-read | Done |
| `claude-repl--fetch-claude-agent-ids` registry query | Done |
| Session file directory creation | Done |
| Tool input preview formatting (inline + popup) | Done |
| `cursor-sensor-functions` on tool overlays | Done, not firing interactively (debugging needed) |
| SQLite frame store (sessions, frames, frame_events, frame_artifacts) | Done |
| Frame capture during streaming (text, tool_use, tool_result) | Done, untested |
| Frame open on send, close on done | Done |

### Configuration (init.el)

| Change | Status |
|--------|--------|
| `agent-chat-agency-base-url` set to port 47070 | Done |

## Checkpoint 2 — 2026-04-09 (Entry ceremony + evidence landscape)

**What was done:**

- `cr`/`cx` auto-start Agency via Emacs `*server*` shell buffer if not running
- `cr new`/`cx new` shorthand accepted (bare `new` alongside `--new`)
- Fixed heredoc quoting (`\"` → bare `"`) — eval was always broken, hidden by `-t`
- Inside-Emacs path: no more `emacsclient -t` hang; uses `-n --eval` + `exit 0`
- `add-to-list 'load-path` in eval — works on fresh daemon without futon config
- Ghost reclamation: `/agents/auto` reuses idle no-session agent nicks
- Stale session file cleanup on ghost reclamation
- Failed invokes no longer corrupt session files (gated on `ok?`)
- cwd passed from `cr` to invoke-fn; ProcessBuilder `.directory()` set — fixes
  "No conversation found with session ID" on resume
- Inhabitation turn: `cr new` fires async bell with surface contract, establishing
  real session from first breath
- `*agents*` display shows session IDs; "no session" flags ghosts
- `*processes*` routed to HUD frame; `project-processes!` respects `external-hud-enabled?`
- `*invoke:*` side-window suppressed via `display-buffer-in-side-window` advice
  when external HUD mode is on
- Repl-parity ratchet claims doc: `docs/repl-parity-claims.edn` (12 claims, 4 verified)
- Context retrieval: futon3a semantic search (MiniLM embeddings) on A→B protopattern,
  results logged as evidence + desktop notification with turn certificate
- Session resumed from CLI to REPL mid-conversation (`:session/resume-from-cli` verified)
- Excursion E-evidence-explorer logged for the read-side query function

**Files changed:**
- `bin/cr`, `bin/cx` (entry ceremony, cwd, inhabitation, load-path)
- `src/futon3c/agency/registry.clj` (`find-reclaimable-agent`)
- `src/futon3c/transport/http.clj` (ghost reclaim, cwd passthrough, session file cleanup)
- `src/futon3c/blackboard.clj` (`project-processes!` no-display, session ID in agents display)
- `emacs/futon3c-blackboard.el` (HUD layout with `*processes*`, side-window advice)
- `dev/futon3c/dev.clj` (cwd on ProcessBuilder, no-display invoke start, session persist
  gated on ok?, context retrieval + notification + evidence)
- `docs/repl-parity-claims.edn` (new file)

**Test state:** Manual testing throughout. Entry ceremony verified on GUI Emacs
and terminal. Session resume verified. Context retrieval + notification verified.

**Status:** :testing — entry ceremony solid, evidence pipeline live, Phase 2 UX
gaps (cursor-sensor, scroll stability, frame inspector) remain open.

**Next:** E-evidence-explorer (query function for evidence chain), Phase 2 UX ratchet.

## Checkpoint 3 — 2026-04-25 (Evidence pipeline was a Potemkin village; invariant landed)

**What was done:**

- Forensic audit on 2026-04-24 found that on `:laptop` role, `make-evidence-store`
  fell through to a volatile in-memory atom because `dev/futon3c/dev/config.clj`
  was missing `:direct-xtdb?` from the `:laptop` role-defaults entry. Result: every
  REPL turn was logging "evidence" that died with each JVM restart. The viewer
  showed today's data because the JVM had been up that long, not because anything
  was persistent. M-aif-head's prior closure on 2026-03-15 had documented an
  `estore` integration that did exist (`futon3c.evidence.store`, XTDB-backed via
  futon1a) but pointed at a path that had never been wired on laptop.
- Defined and committed `I-evidence-per-turn` (commit `c6f2c32`):
  `src/futon3c/evidence/invariant.clj` exposes `check-store-backing` (passes only
  for `XtdbBackend`) and `verify-persisted` (read-back through the same backend
  after append). `test/futon3c/evidence/invariant_test.clj` covers all four store
  shapes against a real in-memory XTDB node — 8 tests, 25 assertions, 0 failures.
- Working-tree edits (not yet committed pending unrelated in-flight work in those
  files) wire the invariant at three sites: `dev/bootstrap.clj` runs
  `check-store-backing` after `reset! !evidence-store` and prints a red banner if
  the store is not XTDB-backed; `dev/futon3c/dev/invoke.clj:emit-invoke-evidence!`
  and `src/futon3c/transport/http.clj:emit-invoke-evidence!` are now synchronous,
  call `verify-persisted` after each append, and log a `VIOLATION` line with the
  evidence-id on failure (no more silent `(catch Throwable _ nil)` drops). Plus
  the `:laptop` role gets `:direct-xtdb? true`.
- Sister fix in `futon4/dev/arxana-browser-evidence.el` and `arxana-browser-lab.el`:
  fallback port `:8080` → `:7071`. Joe's `futon-config.el` now sets the live
  values explicitly so Emacs doesn't depend on the defcustom default.
- End-to-end verified: a chat turn before JVM restart was readable via the
  evidence-viewer at `:7071/evidence-viewer/#/timeline` after restart. First time
  the pipe has been observably end-to-end.

**Files changed (committed):**
- `src/futon3c/evidence/invariant.clj` (new, 106 lines)
- `test/futon3c/evidence/invariant_test.clj` (new, 123 lines)

**Files changed (working tree, pending unrelated cleanup):**
- `dev/futon3c/dev/config.clj` (one line — `:direct-xtdb? true` on `:laptop`)
- `dev/futon3c/dev/bootstrap.clj` (boot-time invariant check)
- `dev/futon3c/dev/invoke.clj` (sync + verify-persisted)
- `src/futon3c/transport/http.clj` (sync + verify-persisted)
- `futon4/dev/arxana-browser-evidence.el`, `arxana-browser-lab.el` (port default)

**Test state:** Unit tests pass for the new namespace (8/8). Full
`futon3c.transport.http-test` and `futon3c.evidence.*` suites pass with no
regressions (140 tests, 494 assertions across those packages). Live test
of evidence persistence across JVM restart confirmed by inspecting
`:7071/evidence-viewer/#/timeline` after `make dev-laptop` was restarted.

**Status:** :testing — invariant active in committed library code, wiring active
in working tree pending repo cleanup. Three threads opened off this checkpoint
(see Excursions below).

**Next:** Vitality-scan evidence-accumulation probe (active thread); excursions
on invariants audit and evidence-viewer deep-dive logged for follow-up.

### Excursions logged off Checkpoint 3

- **E-candidate-invariants-audit:** Across the futon* repo family there is a
  body of "Candidate Invariants" — claims that the system enforces some
  property — and as of 2026-04-25 there is no clarity about which are
  enumerable, which are checked at runtime, which are tested against both
  pass and fail inputs, and which are firing in production. The
  `I-evidence-per-turn` work is the proposed template: canonical def-string,
  two check functions, real-backend test, named enforcement sites that
  `rg <invariant-name>` finds. Excursion goal: enumerate all candidate
  invariants in the codebase (likely sources include `holes/missions/`,
  `src/**/invariant*.clj`, doc sections marked "Candidate Invariants"),
  classify each as design-only / check-exists / check-tested /
  check-firing-in-prod, and produce a punch list with file:line citations.
  Closes when every invariant is one of (a) enforced and tested, (b)
  explicitly downgraded to design-only with a known reason, or (c) deleted.

- **E-stack-hud-cleanup (port manifest):** see
  `futon0/analysis/excursions/E-stack-hud-cleanup.md` §6 for the
  ranked list of stack-hud-1 → stack-hud-2 port candidates as of
  2026-04-25 (voice, hot-reload, services, git, vitality, reminders,
  liveness keep; focus needs verify; affect deferred to per-agent;
  boundary/musn/pattern-sync drop).

- **E-stack-hud-cleanup:** The Vitality-scan evidence-accumulation probe
  (active thread, Layer 1) lands a probe in
  `futon0/scripts/futon0/vitality/scanner.clj` and a render in
  `futon0/contrib/stack-hud.el`. The follow-on (Layer 2) is a reazon-based
  surface check that fires when the hud's *visible* evidence delta is zero
  during an active turn window — i.e. an alarm that audits what the hud
  surfaces, not what the JVM holds, so a silent projection failure (cache,
  filter, stale read) is caught. Reazon plumbing already exists in
  `futon3c/emacs/agent-chat-invariants.el:171-199`. This excursion also
  carries a process commitment: futon0 is by design a cross-futon interface
  layer, so stack-hud may legitimately depend on any futon — but *which*
  futons it depends on for *which* surfaces should be made explicit. A
  wiring diagram (cross-futon dependency map: scanner → futon1a HTTP, hud →
  scanner snapshot, hud → futon3a/futon3c/etc.) is part of the deliverable
  for this excursion. Closes when (a) Layer 2 reazon check is registered and
  fires on the synthetic test-case "turns happened, hud evidence delta = 0",
  (b) the wiring diagram is committed alongside the cleanup, and (c) the
  cleanup pass has no half-rendered or unused sections in the hud.

- **E-evidence-viewer-deep-dive:** The evidence-viewer at
  `:7071/evidence-viewer/` and the Arxana Browse Evidence/Sessions views have
  three live issues that deserve concentrated attention rather than ad-hoc
  fixes. (i) Per-turn pattern annotations from futon3a semantic search land as
  one composite `:coordination` entry per turn with patterns inside
  `:evidence/body "results"` — they are not first-class annotations linked by
  `:evidence/in-reply-to` to the turn's forum-post entry, so queries like
  "show me annotations on this turn" miss. Proposed fix α: rewrite
  `dev/futon3c/dev.clj:emit-context-evidence!` to emit one entry per retrieved
  pattern, each with `:evidence/in-reply-to` on the turn id, typed as a pattern
  annotation. (ii) `emit-context-evidence!` still has a silent
  `(catch Throwable _ nil)` at line 735. Proposed fix β: bring it under
  `I-evidence-per-turn` — synchronous, verify-persisted after append, log
  VIOLATION on failure. (iii) `futon4/dev/arxana-browser-evidence.el:106,
  813-830` keeps an open-session row cache (`row-v3`) that returns cached
  rows for the lifetime of Emacs without a freshness threshold; on 2026-04-24
  this lied with a 9-day-old row claiming 83 turns when the underlying store
  had been wiped by JVM restarts. Proposed fix: 30s TTL on cache hits, or
  invalidate when the buffer's `--last-evidence-id` advances past the
  cached value. Closes when (α) and (β) ship and (iii) is either fixed or
  explicitly downgraded with a documented reason.

## What Remains

### Phase 2: Close the UX gaps

| Gap | What to do | Effort |
|-----|------------|--------|
| Cursor-sensor not firing | Debug why `cursor-sensor--detect` doesn't trigger on interactive motion in read-only buffer. May need `keymap` on overlays with explicit motion hooks instead. | Small |
| Scroll stability | `pos-visible-in-window-p` check is correct but not working at all positions. May need to save/restore window-point explicitly around stream inserts. | Small |
| Multi-base API discovery | Port `codex-repl--resolved-api-base` pattern: probe primary, fallback, Drawbridge-discovered URLs. | Medium |
| Frame inspector command | `C-c C-f` opens frame by completing-read, renders from SQLite. Port from codex-repl `codex-repl-show-frame`. | Medium |
| Tool output in popup | Once `tool_result` events flow and frames capture them, the cursor-over popup renders output from the frame. | Small (once cursor-sensor works) |
| Verify interrupt end-to-end | Test C-c C-c on a live invoke after server reload. | Trivial |

### Phase 3: Beyond CLI parity

| Feature | What it gives you | Effort |
|---------|-------------------|--------|
| Evidence-aware replay | Browse past sessions from SQLite, re-render frames | Medium |
| Peripheral integration | Proof cycle, ArSE ask/answer from the REPL buffer | Large (depends on M-peripheral-gauntlet) |
| Tickle orchestration | Start/monitor Tickle runs from the REPL | Medium |
| Cross-agent coordination | Route between Claude and Codex agents via walkie-talkie | Already partially done (M-walkie-talkie) |

## Design Principles

1. **The chat buffer is lean.** Tool markers with input preview, streamed
   text, nothing else. Detail lives in frames, shown on demand.

2. **Frames are the unit of persistence.** Every turn is a frame. Frames
   go in SQLite, not Emacs buffers. Buffers are views, not storage.

3. **Popups, not side windows.** Posframe child frames for contextual
   detail. No `*invoke:*` or `*agents*` windows consuming layout space.
   Terminal fallback: transient bottom window that auto-dismisses.

4. **Cursor belongs to the user.** Streaming never moves point or scrolls
   unless the user is actively following the output at the bottom.

5. **The server does the work.** Enriched stream events, interrupt control,
   session management — the REPL is a thin display layer over Agency.

## Architectural Invariant

**CLI use incurs evidence debt.** Every substantive agent turn that happens
outside the REPL/Agency surface is a turn that lacks evidence logging,
frame persistence, and coordination metadata. The target invariant from
JOE.md: _all substantive agent turns are logged as evidence_. The REPL
is the surface that enforces this; the CLI is the surface that violates it.

The mission succeeds when the evidence debt from CLI usage drops to zero
because the REPL is simply better to use.

## 2026-05-25: Cursor-Control Latency Fix (claude-repl path)

A long-standing felt symptom — "1-2-3-crocodile" delays before the cursor
takes control in `*claude-repl:claude-N*` buffers, occasionally manifesting
as full 8+ second Emacs blackouts — was instrumented, attributed, and
patched. The Codex-REPL side likely has analogous problems but is being
held off because Codex is currently self-improving its REPL surface;
return here once that work settles.

### Diagnostic chain

1. **`emacs/joe-input-trace.el`** (new) — command-loop tracer with three
   wrap layers: `pre/post-command-hook`, `timer-event-handler` via
   `advice-add :around`, and around-advice on specific process-filter
   symbols (`eat--filter`, `url-http-generic-filter`, `jsonrpc--process-filter`,
   `comint-output-filter`, `internal-default-process-filter`,
   `url-retrieve-synchronously`). Records to `*joe-input-trace*` buffer
   with no per-event disk I/O. M-x `joe-input-trace-{enable,disable,dump,clear}`.
2. **First live trace** showed three stalls of 1.6-2 s where keys queued
   (`input-pending=t`) but neither commands nor timers fired during the
   gap. With the timer + filter tracer enabled, the next trace caught an
   8.5 s blackout with **zero elisp activity inside it** — no timer firings,
   no filter firings, no commands. The Emacs main loop itself was blocked
   at the C level.
3. **Static diff** between the stalled buffer (`*claude-repl:claude-10*`,
   `claude-repl-mode`) and the responsive one (`*codex-repl:codex-8*`,
   `codex-repl-mode`) revealed `cursor-sensor-mode` on in claude-10 with
   **652 text-property regions** carrying `claude-repl--tool-overlay-sensor`,
   vs zero such regions in codex-8. That's the "buffer where control was
   established vs not yet established" distinction Joe felt — really two
   different mode setups.
4. **Automated bench** — `joe-input-trace-bench-sensor-cycle` jumps point
   through tool-overlay regions, forces synchronous `(redisplay t)`,
   measures wall-clock per phase. Reproducible without live typing, so
   any fix can be verified by re-running one command. With sensor mode
   on: **~130 ms per cycle**. With sensor mode off: 6-7 ms. 20× slowdown
   attributable to the mode.
5. **CPU profile of the bench** named the actual hotspots in
   `agent-chat.el`:
   - `(require 'posframe nil t)` called on every popup-show AND every
     popup-hide; posframe isn't installed, but `require` still does a
     load-path scan each call. **39 % of CPU.**
   - `(special-mode)` re-invoked on the popup buffer every show,
     re-firing `global-corfu-mode-enable-in-buffer` and the rest of the
     global mode-enable hooks. **10 % of CPU.**

### Fix

Two-line cache in `emacs/agent-chat.el`:

- `agent-chat--posframe-available` defvar set once at load time; show/hide
  consult the cache instead of re-`require`-ing.
- `(unless (derived-mode-p 'special-mode) (special-mode))` so mode setup
  only runs on first popup creation.

Sympathetic patch in `emacs/claude-repl.el`: debounced
`claude-repl--tool-overlay-sensor` via `run-with-idle-timer` so popup-show
schedules on a 0.15 s idle rather than firing every redisplay tick.
Smaller win once `agent-chat.el` was fixed, but still prevents popup
flashing during fast cursor traversal across many regions.

### Result

| Bench (claude-7, 1763 sensor regions, 389 KB) | Before | After |
|---|---|---|
| Steady-state per sensor cycle | ~130 ms | **0 ms** |
| First-iter baseline (big-jump redisplay) | ~1900 ms | ~1900 ms (unrelated) |
| Popup content correctness | rendered | rendered (verified) |

Reload protocol: `emacsclient --eval '(load-file "/home/joe/code/futon3c/emacs/agent-chat.el")'`
then same for `claude-repl.el`. No JVM restart; smart-cursor instance
state preserved.

### Hypothesis for Codex-REPL (return here later)

`codex-repl-mode` doesn't install `cursor-sensor-functions` text properties,
so the *specific* cursor-sensor stall mechanism doesn't apply. But:

- Codex-REPL surfaces have their own popup paths via the same
  `agent-chat.el` infrastructure (`agent-chat-popup-show / -hide`,
  `agent-chat-update-progress`) — so the `(require 'posframe nil t)`
  load-path scan cost was hitting them too. The agent-chat.el patch
  *already* benefits codex-repl indirectly; no separate change needed
  for that hotspot.
- The recurring `codex-repl--refresh-invoke-dashboard` timer (~500 ms
  cadence; visible in the live trace as the `chat-buffer` anonymous
  timer firing into `*codex-repl:codex-8*`) calls `agent-chat-update-progress`
  on each tick. If that path does any synchronous redisplay or
  window-fitting work, it would manifest as periodic stutter (different
  signature from the claude-repl burst, but same family).
- The blackboard-backpressure pattern (futon3c hot paths blocking on
  emacsclient round-trips) could also surface here, but is mostly out
  of scope for REPL-side fixes.

When Codex's own self-improvement work settles, the procedure is:

1. Run `joe-input-trace-enable`, drive Codex-REPL through its typical
   load (long agent reply with many tool calls, plus user typing
   alongside), dump.
2. Look for `input-pending=t` gaps and per-timer elapsed-ms outliers,
   especially `codex-repl--refresh-invoke-dashboard` and any anonymous
   chat-buffer timers.
3. If a hotspot surfaces, CPU profile the suspect path with `(profiler-start
   'cpu)` and search the calltree for `(require ...)` and any `run-mode-hooks`
   re-invocation.
4. Verify with the bench harness — `joe-input-trace-bench` (simple insert)
   or a Codex-specific cycle bench built on the same primitives.

The bench harness itself is the durable artifact. Any future
perceived-latency regression in any agent-chat-backed REPL buffer can be
caught by re-running it.
