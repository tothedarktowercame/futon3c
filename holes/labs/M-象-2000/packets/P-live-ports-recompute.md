# Packet: session-cap option on compute-ports!, reload from master, recompute live ports

From claude-16 (owner, M-象-2000), 2026-10-11. Mode: work. Your 42379dfa is
accepted (review: the kernel fixture pins the untabled-inferred case and the
explicit authority; the adapter test has the two-opener negative; 4 of 24 with
no constraint). Two notes for the record, no action: the commit also flattened
an unrelated `let` in `dispatch!`, and it switches off the unresolved-symbol
linter for the whole kernel namespace. Keep future commits to the behaviour
named.

## Goal

Get the sole-port rule into the running service and recompute ports on every
record that has reply text, without `with-redefs` in the shared JVM.

## Steps

1. **Option, with a test.** `compute-ports!` in `src/futon3c/xiang/turn_service.clj`
   takes an optional opts map `{:session-cap n}`, default `ports-session-cap`
   (50), used as the `:limit` of `ts/list-records`. Test in
   `test/futon3c/xiang/turn_service_test.clj`: a session of three settled
   turns; with `:session-cap 1` the oldest turn's recompute sets the health
   detail "No acts for turn" and stores no ports; with `:session-cap 10` it
   stores ports. Gates: clj-kondo, check-parens, `clojure -M:test` for the
   xiang namespaces; warrant if `register-warrant.sh` applies, else paste the
   output. Commit only touched files.
2. **Reload from master into the shared JVM** (policy: reload from master is
   always allowed; a restart is Joe's call). Through `scripts/proof-eval.sh`
   against `/home/joe/code/futon3c` (master, including your two commits):
   `(require 'futon3c.logic.xiang 'futon3c.xiang.turn-acts 'futon3c.xiang.turn-service :reload)`.
   `turn_service.clj` holds no state atoms (only constants), so the reload is
   safe. Verify: evaluate `(:arglists (meta #'futon3c.xiang.turn-service/compute-ports!))`
   shows the opts arity, and `futon3c.xiang.turn-acts/turn-ports` on a
   two-act synthetic session returns an `:inferred` key.
3. **Recompute.** For every analysed operator record with non-empty
   `reply_text` (all sessions; about 452 at the last census), in per-session
   `created_at` order, call `compute-ports!` with `{:session-cap 10000}`
   through proof-eval, at most one per second. Before the first call, write
   the before-state ledger (record id, session, `still_open` count,
   `closed_this_turn` count) and commit it. After: the same columns plus
   `:inferred` count per record; per-session tail totals before and after;
   grand totals. Records that still refuse ("No acts for turn") listed.
4. **The labelled session** `ec3a9b31-…`: the 24 rows with the live result
   (expected the same four closes as your test JVM run; report any difference
   as a finding, do not adjust).
5. Note `holes/labs/M-象-2000/LAB-2026-10-11-live-ports-recompute.md` with
   steps 2 to 4 and the reload receipt (what proof-eval returned).

## Not in scope

Placeholder-session records (`turn-Rbl3PJ` and any other
"(awaiting session)" record): a later packet. The adjacency table: unchanged.
Rule A (label word overlap): not implemented.

Bell claude-16 back with shas, totals before and after, the 24-row table, and
the list of refusing records.
