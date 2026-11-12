# T-futon-client-on-a-phone: Emacs on the phone as a futon client, with Zone as the server

**Status:** IDEA — recorded for later (Joe, 2026-11-12); no work scheduled.

Joe, 2026-11-12: "what would it take to *run* Futon on a phone? ... in
principle phone2 could become another endpoint just like Lucy", and then: "I
think a JVM + Emacs, or maybe we could even rewrite the main parts of Agency
entirely in Emacs, could do it, so that I run Emacs locally on the phone and
get the server behind it. I'm able to connect to remote REPLs already. It
wouldn't really be running FUTON but running a "futon client" seems
plausible."

## Why a client, not a node

Zone runs futon3c (~18 GB resident) and futon1b (8 GB heap); a Pixel 8 has
8 GB of RAM, and Android stops long-running background processes (Termux's
child-process cap, memory pressure, battery). A full node would need small
heaps and a slice of the store, and sync with Zone. A client needs neither:
Emacs on the phone, Agency and the stores on Zone, reached over the existing
tunnel (`ssh -L 7070:localhost:7070 -L 7073:localhost:7073 zone`).

## What already works that way

The REPL buffers are a futon client today: `claude-repl.el`, `agent-chat.el`
and the other seat REPLs talk to Agency over HTTP, and the claude/codex
processes run on Zone. With the ports forwarded, they would see the same
`localhost:7070` from the phone.

## What assumes it runs on Zone (count, 2026-11-12, futon3c/emacs, 44 files)

- 14 files talk HTTP to :70xx (fine through the tunnel);
- 23 files shell out (call-process, shell-command, make-process), e.g.
  agent-mission-control, mission-mode, flight-mode, xiang-brief,
  session-overview: each runs a script or binary that lives on Zone;
- 21 files name paths under /home/joe or ~/code;
- 4 files read 象's turn records from ~/.emacs-graph directly (the stepper).

So a phone client needs: those reads served over HTTP instead of the local
disk; the shell-outs moved behind Agency routes, or run over ssh; and paths
made configurable. That is a client-side refactor of futon3c/emacs, not a
rewrite of Agency.

## First step, when picked up

Termux Emacs on phone2, futon3c/emacs from a local checkout, ports forwarded,
and one seat REPL (claude-repl) used for a session. List what fails; each
failure is one of the three kinds above.

Related: futon1b `holes/M-xtdb-fast.md` (smaller, faster stores also help a
future phone node).
