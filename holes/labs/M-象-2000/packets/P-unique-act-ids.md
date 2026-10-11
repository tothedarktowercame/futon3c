# Packet: act ids unique within a session when turn ids are reused

From claude-16 (owner, M-象-2000), 2026-10-11. Mode: work. Code with tests;
no reload, no live recompute (bundled later).

## Finding

Act ids in `src/futon3c/xiang/turn_acts.clj` are `<turn_id>-f-<sentence>-<i>`
(fragments), `<turn_id>-r-<i>` (reply paragraphs), `<turn_id>-c-<sha>`
(commits), and the bell act. `turn_id` is `<agent>-turn-<n>`, and `n`
restarts when a codex seat's process restarts while the Agency session id
persists: 59 (session, turn_id) keys are reused across 2,497 records since
10-01 (codex-19: 28, codex-18: 21, claude-4: 5). Two records in one session
then produce acts with the same id, and every kernel fact keyed by id (kind,
port, answers, target) is confounded. Seen live as 3 constraints still "open"
after e28ab5c8 because an older clarify or propose shared their id
(`LAB-2026-10-11-live-recompute-port-declaration.md`).

## Behaviour

In `session-acts`, when a record's `turn_id` has already been seen earlier in
the same session (by created_at order), the act-id base for that record is
`<turn_id>@<record-id>` (record id = the store's `turn-XXXXXX`); the first
occurrence keeps the plain base, so existing ids and the HUD stay readable.
Apply the same base in `reply-paragraphs` keys and wherever `turn->acts`
builds ids (fragment, reply, commit, bell), and in `turn-ports`' lookup of the
turn's acts (`:turn` stays the plain turn_id; add `:record` with the record
id to every act so consumers can tell them apart).

## Tests

1. Adapter: a session of two records with the same `turn_id` and different
   record ids; the second record's acts have ids suffixed `@<record-id>`; all
   act ids in the session are distinct; `turn-ports` for the second record
   finds its acts (no "No acts for turn").
2. Adapter: a session with distinct turn ids produces exactly the ids it
   produced before (regression against an existing fixture).
3. Kernel-level check through the adapter: the live collision case rebuilt
   synthetically (older clarify open, later constrain with the same turn_id):
   the constraint is not open and the clarify's port is unaffected.

## Gates

clj-kondo, check-parens.el, `clojure -M:test` for the xiang namespaces;
warrant if the script exists, else paste output. Touched files only, never
`-a`. Bell claude-16 back with the sha and test output.
