# Packet: sole-port inferred links, typed by the adjacency table

From claude-16 (owner, M-象-2000), 2026-10-11. Mode: work. Implementation with
tests. Shared tree `/home/joe/code/futon3c`, branch master. Never `git commit -a`.

## Behaviour

When a settled operator turn has exactly one operator fragment of an opening
kind (`lx/opening-kinds`) and a reply paragraph act of that turn has no
`:target`, the pair [reply-act opener] is an *inferred* answer. Its effect is
whatever the adjacency table says for (opener kind, reply kind); if the table
has no entry, the inferred pair does nothing (the port stays open). Explicit
links (`:target`, `:carries-out`) keep today's behaviour. The table itself is
not changed.

Why: `holes/labs/M-象-2000/LAB-2026-10-11-label-linking-census.md` (addendum)
and the mission Checkpoint of 2026-10-11 (codex-68's read): an inferred link
cannot establish relatedness beyond the table, and must carry a witness.

## Where

- `src/futon3c/xiang/turn_acts.clj`, `turn-ports`: compute the inferred pairs
  for the acts of the session (per turn: one opener, reply acts with nil
  `:target`), and build the db as `(lx/db acts inferred)` instead of
  `(lx/db acts)`. Add to the return map `:inferred [{:answer id :opener id
  :rule :sole-port :effect :closes|:keeps|nil}]` so the record's ports carry
  the witness. `compute-ports!` in `turn_service.clj` stores `:ports` from
  this map; include `:inferred` there too (closed_this_turn / still_open
  unchanged in shape).
- `src/futon3c/logic/xiang.clj`, `answerso`: split the two cases. Explicit
  `answers`: effect = table or `:closes` (as now). Inferred
  `answers-inferred`: effect = table only; when nil, the goal fails.
  Docstring updated to say so and why.

## Tests (the review reads these first)

1. Kernel fixture (`test/futon3c/logic/`): a history with one propose opener
   and an inferred explain answer stays open (table has no propose->explain);
   the same with an inferred approve closes; an explicit explain `:target` on
   a propose still closes (authority of explicit links unchanged); an inferred
   pair on an ask-action by report closes; an inferred qualify on an
   ask-action keeps.
2. Adapter test (`test/futon3c/xiang/`): a turn with one `clarify` fragment
   and a ㊢ paragraph with a label bracket (no quote) yields one inferred pair
   with `:rule :sole-port :effect :closes`; the same turn with two opening
   fragments yields none; a turn whose ㊢ paragraph already has a `:target`
   from a quote yields none for it.
3. Regression on the labelled session `ec3a9b31-6656-4530-90db-53a22762c17f`
   (`CHECKPOINT-2026-10-11-false-open-ports.md`): run `session-acts` and
   `turn-ports` over its records in a test JVM (`clojure -M:test` or a REPL
   started in this checkout; never the shared :6768 JVM) and report, for the
   24 rows, which close now. Expected: the quote row plus three sole-port rows
   (02:27 clarify, 02:44 delegate, 03:36 ask-action) = 4; no constraint
   closes. If the count differs, report the rows, do not tune to the number.

## Gates

- `clj-kondo --lint src test` clean on touched files; `futon4/dev/check-parens.el`
  on touched files; `clojure -M:test` green for the logic and xiang namespaces.
- Register a warrant if `scripts/register-warrant.sh` applies; otherwise paste
  the test output in the bellback.
- Do not run `compute-ports!` against live records or reload into the shared
  JVM: the live recompute is a separate step after review.
- Commit only touched files, with a message naming the rule and the table-typing.
- Bell claude-16 back with shas, the 24-row result, and the test output.
