# PACKET: frames carry the memory layer (E-prove-memory-useful, G3 re-scoped)

From claude-3. Read `holes/excursions/E-prove-memory-useful.md`, section
"G3's premise, and G5 first", before starting.

## Goal (one behaviour)

`scripts/turn_frames.py` adds `memory` to a frame when the frame's window
holds one or more `memory-call` coordination rows (written by
`minus40/scripts/call_hook.py`, tags `memory-call minus40`, body
`{event "memory-call", tool, args, result-text, error?, at}`). Rows are
assigned to frames by the same window rule that puts non-operator rows into
HAPPENED; reuse it, do not write a second one.

    memory {calls [{tool, args, at, error, evidence-id,
                    reached [{name, project}]}]}

`reached` comes from `scope_view_data.memories_named(tool, result_text,
facts_by_text)`: import it (do not copy). `facts_by_text` is only needed for
`m40_state`; build it lazily via `scope_view_data.m40_facts()` only when a
frame has an `m40_state` call, and if that import fails leave that call's
`reached` empty and add a `warnings` line on stderr.

A frame with no such rows: leave `memory` absent (the page already prints
the absence sentence). No flag: the rows are already in the fetched
session evidence, so this costs no extra read of futon1b.

Check that the page (`web/stepper/stepper.ts`, the MEMORY section you
wrote) renders `calls[].reached`; if the field names it expects differ,
change the page to match this shape, rebuild `stepper.js`, and extend
`test_stepper.mjs`.

## Files

- in: `scripts/turn_frames.py`, `scripts/scope_view_data.py`,
  `web/stepper/stepper.ts`, `minus40/scripts/call_hook.py`.
- out: `scripts/turn_frames.py`, `scripts/test_turn_frames.py`, and the page
  files only if the shapes differ.

## Acceptance

1. Tests in `test_turn_frames.py`: a session with an operator turn and a
   `memory-call` row inside its window gives that frame `memory.calls` with
   the names `memories_named` reads (use a real `m40_find` result text, e.g.
   the one in evidence `e-6ea042af-b894-47f4-a367-490471906e1e`); a row
   after the next operator turn lands on the next frame, not this one; a
   frame with no row has no `memory` key.
2. Live: `python3 -I scripts/turn_frames.py ff41903c-6510-4c5e-a293-01324d39fb45 --limit 5`
   shows a frame whose `memory.calls[0]` is the `m40_find ["bellback" "park"]`
   call with 9 reached names. Paste that frame's `memory` in your bell.
3. `tsc --strict` and `node web/stepper/test_stepper.mjs` green if the page
   changed. Do not reload or restart the :7070 JVM (the frames cache runs the
   script, so no reload is needed); do not publish to /var/www.
4. Commit on futon3c master.

Bell claude-3 back with a summary + commit shas.
