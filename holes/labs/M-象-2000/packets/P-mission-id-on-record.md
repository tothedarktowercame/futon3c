# Packet: stamp the seat's mission id onto every turn record (warp thread)

From claude-16 (owner, M-象-2000), 2026-10-12. Mode: work. Code with tests; no
reload (I will batch the reload). One field, one behaviour.

## Why

Joe (2026-10-12): "effectively there is one system, and a session or thread is
an interesting efficiency but not the be all end all." The unit becomes the
conversation: all records whose seat was on one mission, across sessions and
seats. Today 0 of 3,735 records carry a mission id; it lives only on the
roster (`:agent/metadata :mission-id`, see `turn_service.clj` ~1881 for how
the bell path already reads it).

## Behaviour

1. `record-turn!` (`src/futon3c/xiang/turn_service.clj` ~584): after
   `make-record`, set `:mission_id` on the record to the recipient seat's
   current mission id (the `agent-id`'s roster entry, `:agent/metadata
   :mission-id`, trimmed, non-empty) when present; otherwise do not set the
   key (absence is typed by absence here, never `""` or `nil`). Read the
   roster through whatever `cfg svc` function the service already uses for
   `agent-session`; do not call the registry directly if an injected fn
   exists (tests inject it).
2. Bell records (origin "agent") take the recipient's mission id the same way.
3. The session-end system record (origin "system") takes the mission id of
   the agent whose session ended, same source.
4. `GET /api/alpha/xiang/turns/:id` returns it as `mission_id` without
   further change (it returns the record map).

## Tests (`turn_service_test.clj`)

1. With an injected roster fn returning a mission id for the seat, a recorded
   operator turn has `:mission_id` equal to it.
2. With the roster fn returning nil / "" / "  ", the record has no
   `:mission_id` key.
3. A bell record carries the recipient's mission id.

## Gates

clj-kondo, check-parens.el, `clojure -M:test` for xiang; touched files only;
never `-a`. Bell claude-16 back with the sha and test output.
