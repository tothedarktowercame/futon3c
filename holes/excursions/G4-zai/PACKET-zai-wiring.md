# PACKET: Zai harness injects memories before round 1 (E-prove-memory-useful, G4 for Zai seats, wiring half)

From claude-3. Read futon3c/holes/excursions/E-prove-memory-useful.md, gap G4
and section "G6, first run", and futon3c/holes/excursions/G4-zai/CONTRACT.md:
the CLI you call. Another seat (codex-23) is writing inject.py in parallel;
until it lands, test against a stub script.

## Goal (one behaviour)

In `src/futon3c/agents/zai_api.clj`, at the point a new turn's prompt is
appended (`(swap! !messages conj {:role "user" :content (str prompt*)})`,
near `persist-turn-start!`, ~line 2326), for a turn whose caller is NOT in
`continuation-callers` (auto-bellback, parked-resume):

1. run inject.py with `--agent <agent-id>` and the prompt on stdin, through a
   shell indirection like `m40-shell` so unit tests never start it; bound
   it at 20 s; any failure -> no injection, never a failed turn;
2. if `block` is non-blank, the user message content becomes
   `block + "\n\n" + prompt*`; the system message is not touched;
3. `persist-turn-start!` gains `:injected [ids]` and `:injected-ms` in its
   body (always present: `[]` when nothing was injected or the agent is not
   enabled, so absence is typed), and `:prompt` stays the packet as sent
   (the block is recoverable from the ids).

Only `memory-mode :full` seats inject (a `:none`/`:files` seat is a
comparison condition and must stay clean).

## Gates

clj-kondo on the file (no new warnings), `futon4/dev/check-parens.el`, and
`clojure -M:test -n futon3c.agents.zai-api-test` (find the existing test
namespace for zai_api; add tests there): enabled stub -> content prefixed
and :injected recorded; disabled -> content unchanged and :injected [];
continuation caller -> not called; stub exits non-zero / times out -> turn
proceeds, :injected [].

Do NOT reload or restart the :7070 JVM; claude-3 reloads after review.
Commit on futon3c master.

Bell claude-3 back with a summary + commit shas.
