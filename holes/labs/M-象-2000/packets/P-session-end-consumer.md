# Packet: the turn service consumes session-ended transitions (HOLE 18, part 2 of 2)

From claude-16 (owner, M-象-2000), 2026-10-11. Mode: work. Code with tests; no
reload, no live recompute. Reads the registry's outbox (codex-71: 26f3ebd8,
0a82894f: `registry/session-end-outbox-path`, `registry/add-session-end-listener!`).
Do not change `registry.clj`. Bump `turn-acts/rules-version` to "2026-10-11.3".
Design: `LAB-2026-10-11-session-end-discovery.md` §3–4 and the mission
Checkpoint's decisions (2026-10-11): ends are reset / deregister / reap /
replaced; at-least-once with idempotent ids; the whole last session ends;
marks are non-closing and live here.

## Behaviour

1. **Subscribe and replay.** On `start!` the turn service registers a listener
   with `add-session-end-listener!` and replays unconsumed lines of the outbox
   file: a cursor file `<outbox path>.cursor` beside the outbox holds the last
   consumed transition `id`; replay processes lines after it (or all, when no
   cursor) and advances the cursor after each successful write. Listener and
   replay call one function `consume-session-end!`.
2. **One system-origin record per transition**, idempotent: record id
   `turn-end-<first 12 hex of sha256 of the transition id>` (fits
   `turn-store/record-id?`); if it exists, do nothing. Fields: `origin
   "system"`, `analysis_status "declared"`, `agent_id` the agent, `session_id`
   the old session, `created_at` the transition `at`, `turn_id`
   `<agent>-session-end-<at with : and - removed>`, `source_text` "session
   ended: <reason>", `session_end {:id :reason :new_session}`, and `:acts`:
   - the envelope act `{:id "<turn_id>-n-0" :kind :notice :author "system"
     :at at :to agent :agent :session :turn :record :port? false :standing?
     false :reason <reason> :marked_reply_count n}` where n counts marked
     paragraphs (`turn-record/reply-marks`) over `reply_text` of the session's
     records;
   - one `{:id "<turn_id>-l-<i>" :kind :lapse :author "system" :at at :target
     <act id> :port? false :standing? false}` per entry of the session's last
     ported record's `still_open` whose act author is not "operator"
     (agent-authored reply ports and bell asks); operator-authored ports are
     never targeted.
3. **The kernel reads it.** `session-acts`/`turn->acts`: a record with origin
   "system" and stored `:acts` contributes exactly those acts (no reading, no
   reply parsing). `compute-ports!`'s settled filter admits origin "system"
   with status "declared". After writing the record, call `compute-ports!` on
   it (off-path as for bells): the lapses close the agent ports (`:lapse` is a
   closing kind; an explicit target closes when the table is silent).
4. **Marks.** In `compute-ports!`, when the session has a session-end record at
   or before the computed turn's time, each `still_open` entry whose act
   author is "operator" gains `:mark "orphaned"` when the envelope's
   `marked_reply_count` > 0, else `"unread"`. Nothing else about `still_open`
   changes; marks never close anything.

## Tests (`turn_service_test.clj` with the ports fixture; `turn_acts_test.clj` for 3)

1. `consume-session-end!` on a transition for a seeded session writes the
   record with the fields above and the right act count; calling it again
   writes nothing (idempotent); the cursor advances.
2. Replay on `start!` with a two-line outbox and a cursor at line 1 consumes
   only line 2.
3. Adapter: a system record's acts are taken as stored; an agent ㊸/🈸 port in
   an earlier turn is closed by the lapse (`:closed-this-turn` of the system
   record names it); an operator ask in the same session stays open.
4. Service: after the session end, recomputing the session's last operator
   record shows its operator ports with `:mark "orphaned"` when the session
   had marked replies, `"unread"` for a session whose replies carry no marks.
5. A transition whose session has no records writes the record with only the
   envelope act and refuses nothing.

## Gates

clj-kondo, check-parens.el, `clojure -M:test` for the xiang namespaces (and
registry, unchanged); the test fixture from 0a82894f binds the outbox to a
temp file: use it, never the default path. Touched files only; never `-a`.
Bell claude-16 back with the sha and test output.
