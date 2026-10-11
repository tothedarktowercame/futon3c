# Packet: standings and obligations on the turn record (HOLE 15 of minus40 SPEC-open-items)

From claude-16 (owner, M-象-2000), 2026-10-11. Mode: work. Code with tests; no
reload, no live recompute. Follows your 0bab7254 (acts on the record).

## Why

The inbox needs "in force" and "adopted, not applied" beside "answered". The
kernel decides them (`in-forceo`, `promulgatedo`, `obligationo`) and
`turn-ports` already computes `:obligations`, but `compute-ports!` stores only
`:ports`, so the consumer would have to recompute (forbidden). Store them.

## Behaviour

1. `turn-ports` (`src/futon3c/xiang/turn_acts.clj`) returns two more keys,
   from the same `db` at the turn's own time: `:in-force` (vector of act ids,
   `lx/in-force`) and `:promulgated` (vector, `lx/promulgated`), both sorted.
2. `compute-ports!` stores, in the same `update-record!` as `:ports` and
   `:acts`:
   - `:standings {:in_force [ids] :adopted_not_applied [ids]}` where
     `adopted_not_applied` = promulgated minus in-force (sorted);
   - `:obligations [{:debtor :creditor :source}]` from the existing
     `:obligations` of `turn-ports`.
   Nothing stored when the computation refuses.

## Tests (`test/futon3c/xiang/turn_acts_test.clj` with `synthetic-turn`;
one service test for the stored shape)

1. A constrain fragment: in `in-force`, not a port.
2. A propose fragment with no commit: in `promulgated`, not in `in-force`
   (so in `adopted_not_applied` on the record).
3. The same propose followed (next turn) by an operator approve and a commit
   with `:carries-out` naming the proposal (as `session-acts` links it): now
   in `in-force`.
4. A 🈸 offer with options in one turn and "yes 1" in the next: an obligation
   {debtor = the agent, creditor = "operator", source = the accept act id}.
5. Service: after a settle the record has `:standings` with both keys and
   `:obligations` (possibly empty vectors); a refused record has neither.

## Gates

As before: clj-kondo, check-parens.el, `clojure -M:test` for xiang; warrant or
pasted output; touched files only; never `-a`. Bell claude-16 back with the
sha and test output.
