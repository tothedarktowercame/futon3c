# Packet: port or standing as an explicit declaration (kind table + act flag)

From claude-16 (owner, M-象-2000), 2026-10-11. Mode: work. Code with tests only;
no reload into the shared JVM and no live recompute in this packet (codex-70 is
recomputing live ports now; the reload for this change follows after).

## Ruling (Joe, 2026-10-11)

Whether an act kind opens a *port* (a move awaiting a reply) is declared on the
kind table and carried on each act, instead of being inferred from membership
in the adjacency keys. codex-68's reasoning (mission Checkpoint of 2026-10-11):
ports and standings are two relations over one act substrate; `constrain`
creating a standing *and* opening a port, only because it has an adjacency
row, showed the distinction must be explicit in the carrier. Joe chose this
over the smaller fix of deleting the `constrain` row.

## Behaviour

- `futon3c.logic.xiang` declares `port-kinds` explicitly:
  `#{:propose :ask-action :offer :delegate :promise :report-problem :clarify :verify}`.
  `constrain` is not in it. `opening-kinds` becomes an alias of `port-kinds`
  (callers in `turn_acts.clj` lines ~499 and ~520, and `flow` at ~480/485,
  keep working).
- Each act carries `:port? boolean` and `:standing? boolean`. The adapter
  (`turn_acts.clj`: fragment, reply, commit and bell acts) stamps them from
  the tables (`:port?` = kind in `port-kinds`; `:standing?` = kind is
  `:creates` in `kinds`). The kernel's fact loading (`facts-with-links`)
  records a `port` relation from the act's `:port?` when present, else from
  the table; `openo` tests that relation rather than `membero opening-kinds`.
  So a later reading can mark one act as not a port without a table change.
- The `constrain` row leaves the adjacency table (no port, nothing to type).
  Its standing semantics are untouched: `:constrain :creates`, in force when
  promulgated, cancelled by withdraw.
- A consistency check in the tests: every adjacency key is a port kind, and
  every port kind has an adjacency row.

## Tests (the review reads these first)

1. Kernel: a `constrain` act is never open (`open-ports` excludes it) but is
   promulgated and in force; after a `withdraw` targeting it, not in force.
2. Kernel: an act of kind `:propose` with `:port? false` stamped on it is not
   open; the same act without the flag is open (table default).
3. Kernel: the consistency check above.
4. Adapter: `session-acts` output acts carry `:port?` and `:standing?`;
   a constrain fragment has `:port? false :standing? true`; an ask-action
   fragment `:port? true :standing? false`; a propose `true true`.
5. Regression on the labelled session `ec3a9b31-…`
   (`holes/labs/M-象-2000/CHECKPOINT-2026-10-11-false-open-ports.md`) in a
   test JVM, never the shared one: the two constraint rows are absent from
   `still_open`; the closes are the same four as 42379dfa's run. Report
   differences as findings.

## Gates

clj-kondo and `futon4/dev/check-parens.el` on touched files; `clojure -M:test`
for the logic and xiang namespaces; warrant via `scripts/register-warrant.sh`
if it applies, else paste the output. Commit only touched files, message
naming the declaration. Never `git commit -a`. Bell claude-16 back with shas,
test output, and the labelled-session result.
