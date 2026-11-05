# T-bug-report-feed-ui — voice bug reports behave unlike normal Voxterm turns

**Status (2026-11-02): ACTIVE. Owner: codex-7.**

## Problem

The spoken `🐞 BUG REPORT` cue durably collects a self-contained report and can
push it to an attached agent, but its first live use exposed missing UI
contracts at the boundary between Voxterm, Emacs and the Agency:

- report dispatch bypassed the normal insertion path, so “rocket” left ghost
  preview text in the attached REPL;
- completed collection opened the generated `BUG-*.md` and stole the cursor;
- collection ran in an `emacsclient` temporary buffer rather than the focused
  REPL, producing the wrong context;
- attachment was invisible where Joe reads agent responsibility: the per-turn
  `Cooked for …` line;
- a pushed bell was easy to miss when `agent-follow-mode` was off.

## Implemented

- Spoken cue and durable asynchronous report collection.
- `M-x futon-bug-feed-attach`, `-detach`, and `-status`, with a persistent
  private attachment file.
- Explicit preview cleanup for report dispatch.
- Background report registration without selecting its buffer.
- Focused Voxterm target used as the Emacs context source.
- Attached responsibility rendered on the attached agent's Cooked line.

## Acceptance

In continuous mode, say “bug report … rocket” while `codex-7` is attached.
The preview disappears; focus remains in the REPL; its completed-turn Cooked
line says `🐞 bugs attached`; the generated report names that REPL as context;
and one pushed request reaches `codex-7`. Detaching removes the marker and
subsequent reports remain durable without being pushed.

## Completion criteria

- The implemented UI contracts are landed in code (preview cleanup, background registration, focused-REPL context, Cooked-line marker, attach/detach/status) — check: `/home/joe/code/futon3c/emacs/report-futon-bug.el` and its test `/home/joe/code/futon3c/emacs/test/report-futon-bug-test.el` **Met.**
- Live acceptance (continuous mode): saying 'bug report … rocket' with codex-7 attached leaves no preview, keeps focus in the REPL, shows `🐞 bugs attached` on the Cooked line, names that REPL in the report, and pushes one request to codex-7 — check: a dated witness note here or in the voxterm log **Not met.**
- Detach: the marker is removed and subsequent reports stay durable without being pushed — check: same witness note **Not met.**
