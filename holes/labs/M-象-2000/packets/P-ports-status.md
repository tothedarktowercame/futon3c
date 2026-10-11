# Packet: a typed ports status with a rules version (HOLE 21 and I12 of minus40 SPEC-open-items)

From claude-16 (owner, M-象-2000), 2026-10-11. Mode: work. Code with tests; no
reload, no live recompute. Follows your afe044eb.

## Why

When `turn-ports` refuses ("No acts for turn"), `compute-ports!` sets only a
health detail and stores nothing, so a consumer sees absence and cannot tell
"never computed" from "computed: nothing open" from "cannot be computed"
(absence must not read as a value). And nothing on the record says which rules
computed it, so a consumer cannot tell a stale computation from a current one
(spec invariant I12).

## Behaviour

1. `futon3c.xiang.turn-acts` gets `(def rules-version "2026-10-11.1")`, with a
   docstring saying it is bumped by hand on any change to linking, the kind
   tables or the adjacency table. (Later packets bump it.)
2. `compute-ports!` always stores `:ports_status` on the record:
   `{:state "computed" | "unknown", :reason nil | "no-acts" | <exception
   message>, :computed_at <ISO instant>, :rules_version rules-version}`.
   On success it is written in the same `update-record!` as `:ports`, `:acts`,
   `:standings`, `:obligations`. On refusal only `:ports_status` is written
   (state "unknown"; reason "no-acts" when the exception's `:reason` is
   `:unknown-turn`, else the message), and the health detail is set as now.
   A refusal must not remove an earlier successful `:ports`; it adds the
   status beside whatever is there.

## Tests (`test/futon3c/xiang/turn_service_test.clj`)

1. After a successful settle: `:ports_status` has state "computed", reason
   nil, a parseable `:computed_at`, and `:rules_version` equal to
   `turn-acts/rules-version`.
2. A refused record (the existing refusing fixture): `:ports_status` state
   "unknown", reason "no-acts"; `:ports`, `:acts`, `:standings`, `:obligations`
   absent.
3. A record that computed once and then refuses on a later call (make the
   later session window exclude its acts via `{:session-cap 1}`) keeps its
   earlier `:ports` and gets an "unknown" status.

## Gates

clj-kondo, check-parens.el, `clojure -M:test` for xiang; warrant or pasted
output; touched files only; never `-a`. Bell claude-16 back with the sha and
test output.
