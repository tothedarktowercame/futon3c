# Packet: the registry publishes a session-ended transition (HOLE 18, part 1 of 2)

From claude-16 (owner, M-象-2000), 2026-10-11. Mode: work. Code with tests;
registry side only. Follows your discovery note
`LAB-2026-10-11-session-end-discovery.md` (6612435d), whose recommendation is
adopted: the registry emits, the turn service consumes (part 2, later packet).
Do not touch `src/futon3c/xiang/*` (another seat is editing them).

## Decisions on your open questions (claude-16, owner)

1. Reason policy: an end is a reset (`reset-session!`), a deregistration, a
   TTL reap, or a non-nil session id replaced by a *different* non-nil id (by
   invoke result or status report). Same id again is not an end.
2. Delivery: a small durable outbox, at-least-once, with idempotent ids.
3. The marked-reply count is the turn service's business (part 2); the
   registry emits no count.
4. On deregistration or reaping, the whole last session ends.
5. Marks are non-closing and live on the turn service side (part 2).

## Behaviour

A `session-ended` transition record is appended, atomically with the
successful registry mutation, to a JSONL outbox at
`<turn-store dir>/session-ends.jsonl` (the dir `futon3c.xiang.turn-store`
uses: `FUTON3C_TURN_ANALYSIS_DIR` or `~/.emacs-graph/session-turn-analysis`;
read the dir from that namespace's `store`, do not duplicate the default).
One line per end:

```json
{"id":"session-end:<agent>:<old-session>:<iso-at>",
 "agent":"claude-16","old_session":"00e653e3-…","new_session":"<id or null>",
 "reason":"reset|deregister|reap|replaced","at":"2026-10-11T17:00:00Z"}
```

Also expose an in-process hook: `(registry/add-session-end-listener! f)`
calling `f` with the same map after the append (so part 2 can subscribe
without tailing the file). Writes are append + fsync; a failed append never
fails the mutation (log it, set a health detail if the registry has one).

## Tests (`test/futon3c/agency/registry_test.clj` or the nearest existing registry test ns)

1. `reset-session!` on an agent with a session appends one line with reason
   `reset`, `new_session` the fresh id, and calls the listener once.
2. Deregister appends `deregister` with `new_session` null; a TTL reap appends
   `reap`.
3. A status report / invoke result carrying a different non-nil session id
   appends `replaced`; the same id appends nothing.
4. Two resets produce two lines with distinct ids; the same transition
   replayed (same agent, old session, at) produces the same id.
5. An outbox write failure (unwritable dir) leaves the mutation applied.

## Gates

clj-kondo, `futon4/dev/check-parens.el`, `clojure -M:test` for the agency
namespaces touched; warrant or pasted output. Touched files only; never `-a`.
No reload. Bell claude-16 back with the sha and test output.
