# Packet: the reply's own time on the record (HOLE 16 of minus40 SPEC-open-items)

From claude-16 (owner, M-象-2000), 2026-10-11. Mode: work. Code with tests; no
reload, no live recompute. Follows your 24bd91f2. Bump
`turn-acts/rules-version` to "2026-10-11.2" (act times change).

## Why

Every act of a record takes `:at` from the record's `created_at`, the operator
turn's time (`turn_acts.clj` lines ~117, 145, 170, 243). So a reply paragraph
or a commit is dated before the agent wrote it, by the length of the agent's
turn. The kernel reads `openo`/`closes-porto` "as of" those times, and the
inbox shows "since". The reply's time is known at the happened call; store it.

## Behaviour

1. `attach-happened!` (`turn_service.clj`) stores `:reply_at` on the record
   when the happened payload carries a reply (map form): the payload's
   `:reply-at` if present (ISO instant), else the service clock at the call
   (`(iso (now-ms svc))`). The http handler passes `:reply-at` through from
   the JSON body when present. A string-summary happened call (files-era)
   stores no `:reply_at`.
2. In `turn->acts`: reply acts and commit acts take `:at` from the record's
   `:reply_at` when present, else `created_at` as today. Operator fragment
   acts and the bell act keep `created_at`. `turn-ports` keeps using the
   turn's operator time as its "as of" anchor (the record's `created_at`),
   so a port the turn itself opened and closed still counts as closed this
   turn (the existing before/after computation; check the existing test
   `turn-ports: a port opened and closed within the turn…` still passes
   unchanged).

## Tests

1. Service: after a map-form happened call the record has `:reply_at`
   parseable as an instant, equal to the payload's `:reply-at` when given.
2. Adapter: with `:reply_at` on the record, reply acts and commit acts have
   `:at` = `:reply_at`; operator fragments have `:at` = `created_at`.
3. Adapter: a record without `:reply_at` yields exactly the acts it yields
   today (regression against the existing fixture: ids and `:at` unchanged).
4. Adapter: a sole-port inferred close inside one turn (operator clarify,
   ㊢ reply with `:reply_at` one minute later) is still reported in
   `:closed-this-turn` for that turn.

## Gates

clj-kondo, check-parens.el, `clojure -M:test` for xiang; warrant or pasted
output; touched files only; never `-a`. Bell claude-16 back with the sha and
test output.
