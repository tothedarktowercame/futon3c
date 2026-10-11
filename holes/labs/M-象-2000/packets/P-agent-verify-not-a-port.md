# Packet: agent-authored ㊬ verify paragraphs are not ports

From claude-16 (owner, M-象-2000), 2026-10-11. Joe ruled yes (2026-10-11).
Mode: work. Code with tests only; no reload, no live recompute.

## Behaviour

A ㊬ paragraph in an agent's reply reports a check the agent made: a claim, not
a move awaiting the operator. Today it is stamped `:port? true` because
`:verify` is in `lx/port-kinds`, so 72 agent verifies sat open at two session
tails. Change: in `stamp-act` (`src/futon3c/xiang/turn_acts.clj`), an act of
kind `:verify` whose author is not `"operator"` gets `:port? false`. The
operator's own verify fragments ("check that X") stay ports. `port-kinds` and
the adjacency table are unchanged; the kernel reads the per-act flag
(e28ab5c8), so no kernel change is needed. The ㊬ act still exists and still
answers: a ㊬ with a quoted bracket, or a sole-port inference, still closes an
operator report-problem (table: report-problem → verify :closes).

## Tests (`test/futon3c/xiang/turn_acts_test.clj`)

1. A synthetic turn with a ㊬ reply paragraph: the act has `:kind :verify
   :port? false`; after `turn-ports` it is absent from `:still-open`.
2. The same turn with an operator `verify` fragment: that act has `:port? true`
   and is in `:still-open`.
3. A turn with one operator `report-problem` fragment and a ㊬ reply with a
   label bracket: the sole-port inference links them and the report-problem
   closes (the verify still answers even though it opens nothing).
4. A bell turn (origin "agent") answered by a ㊬: `answer-bell` still closes the
   bell's ask-action.

## Gates

As the sibling packet: clj-kondo, check-parens.el, `clojure -M:test` for xiang;
warrant or pasted output; touched files only; never `-a`. Bell claude-16 back
with the sha and test output.
