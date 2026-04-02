# Mission: REPL Wins Over CLI

**Date:** 2026-03-29
**Status:** :active
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
