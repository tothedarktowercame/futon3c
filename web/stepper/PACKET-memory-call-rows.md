# PACKET: memory-call rows for Claude seats (E-prove-memory-useful, G5)

From claude-3 (owner of E-prove-memory-useful since 2026-10-17, taking over
from claude-16). Read `futon3c/holes/excursions/E-prove-memory-useful.md`,
sections "The gaps" (G3, G5) and "Checkpoint", before starting.

## Why this comes before G3

The checkpoint named G3 (`turn_frames.py --memory` over Zai `turn-round`
evidence) as the next piece. It cannot pass as written: `turn_frames.py` on
zai-1's session `zai-c02b13ca-b1db-4357-8f76-b6f9382b7a1f` returns `[]`,
because frames are built from operator chat-turn rows
(`is_operator_turn`) and a Zai session has none (it has only `turn-start`,
`turn-round`, `turn-commits`, `clock-decision`). The stepper shows Joe's
turns on Claude seats, and those seats leave no record of their minus40
calls at all. So the first row that can ever appear in a stepper frame's
MEMORY section has to come from a Claude seat. That is this packet.

## Goal (one behaviour)

A new Claude Code PostToolUse hook script, `minus40/scripts/call_hook.py`,
that for each call to a minus40 MCP tool (`tool_name` matching
`mcp__minus40__m40_*`) POSTs one evidence row to Agency, so the row lands in
the seat's session and `turn_frames.py` already picks it up in HAPPENED.

Row shape (match how `futon3c/emacs/agent-chat.el` writes its
`turn-commits` coordination rows: find that POST and mirror its route,
type, author, session-id and origin fields; do not invent a new route):

    evidence/type      coordination
    evidence/tags      ["memory-call" "minus40"]
    evidence/author    the seat's agent id (env FUTON3C_AGENT_ID or the
                       same source agent-chat.el uses; if none, "unknown")
    evidence/session-id   payload session_id
    body {event "memory-call", tool "m40_find" (the name without the
          mcp__minus40__ prefix), args <tool_input as given>,
          result-text <the tool_response text, capped at 20000 chars
          with a "…[+N chars]" suffix as zai_api.clj does>,
          error? <bool>, at <ISO time of the hook run>}

Do NOT parse memory names in the hook. `scope_view_data.memories_named`
(futon3c/scripts) already parses the m40_mcp renderers; the reader (G3)
will apply it to `result-text`. A second parser here would drift.

Like `ingest_hook.py`: never block or fail the tool, exit 0 whatever
happens, POST timeout ≤ 3 s, log to `$MINUS40_HOOK_LOG`.

## Not in this packet

- Editing `~/.claude/settings.json` to register the hook: claude-3 does
  that after review (matcher `mcp__minus40__.*`).
- G3's reader side, the SessionStart brief as an `injected` row: later
  packets.

## Files

- in (read only): `minus40/scripts/ingest_hook.py`, `minus40/scripts/m40_mcp.py`,
  `futon3c/emacs/agent-chat.el` (the turn-commits POST),
  `futon3c/src/futon3c/agents/zai_api.clj` (the result-text cap),
  `futon3c/scripts/turn_frames.py` (confirm a non-operator row in the
  window lands in HAPPENED; say in your bell if it does not).
- out: `minus40/scripts/call_hook.py`, `minus40/scripts/test_call_hook.py`.

## Acceptance

1. Tests (stdlib unittest, same style as `test_ingest_hook.py`) with a local
   HTTP server standing in for Agency: an m40_find payload posts exactly one
   row with the shape above; a non-minus40 tool posts nothing; a 25000-char
   result is capped with the suffix; Agency down → exit 0, nothing raised,
   one log line; an `error` tool_response sets `error? true`.
2. One live row: pipe a real-shaped payload (session id
   `call-hook-test-<date>`, a real `m40_find` result text) to the script
   against :7070, then read it back with
   `GET /api/alpha/evidence?session-id=call-hook-test-<date>` and paste the
   row in your bell.
3. Commit in minus40 (master). Python only, so no clj-kondo/parens gates.

Bell claude-3 back with a summary + commit shas.
