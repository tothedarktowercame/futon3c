# E-argument-game-search: self-play search on argument games (idea)
**Status:** IDEA — written 2026-10-30 by claude-16 with Joe. Not on the critical path.
Joe: "an interesting project but it isn't yet clear that it is on my critical path. But
it could become my next obsession after AIF, maybe." Do not start without Joe's go.

## Question

Can AlphaZero-style search (self-play, Monte Carlo tree search, a learned policy and
value) help build or judge argument structures, cascades, or s-expression
evaluations? And can a language model sit inside such a game as the move proposer, with
a game engine holding the moves and the win condition?

## Where it came from (2026-10-29 conversation)

- The large Lisp machine daydream: an embedded tree completed to its evaluation, costed
  at about $500k of compute (`~/notes/argument-graph/apex.edn`, node
  `large-lisp-machine-2026`).
- Rob: AlphaGo/AlphaZero/Stockfish-type search on argument graphs, pattern graphs or
  cascades, but "it wouldn't be the first thing to do".
- Game-theoretic games (Snatch, repeated games, Joe's friend Tim); argument games over
  AIF² files; Pease et al. 2017's dialogue game.
- Joe's Simondonian point: "LLMs for everything" is a false problem with a false
  solution. Wrap the LLM in a game engine, or the engine in the LLM: neural eval and
  neural apply (SICP), each half doing what it is suited to.

## Why beware

- **Rob's caution** (above): search needs a cheap, exact terminal reward. Most of our
  argument work has a judged reward, not a computed one, and search on a judged reward
  optimises the judge.
- **Zone has no GPU.** 32 cores, 251 GB RAM (checked 2026-10-29). CPU AlphaZero suits
  small games only; anything Go- or Lean-sized does not fit.
- **Our own GPU trial went badly.** The 2026-09-22 Linode Leanstral trial: local serving
  was generation-bound, nothing was proved, the box was deleted (memory
  `leanstral-local-serving-is-generation-bound`).
- **Lean checks are slow** compared with an arithmetic `eval`, so Lean is a poor first
  domain for self-play even though it is the obvious rich one.
- Prior literature on learned search for program synthesis and theorem proving exists
  and should be read before building anything *(not yet surveyed; from memory only)*.

## Cheap first steps (if taken up)

OpenSpiel 2.0.2 installs from pip on Python 3.12; venv under `~/notes/`, training as a
`systemd-run --user` unit outside the futon3c cgroup.

1. **Arithmetic s-expression game** (apex `wp-alphazero-sexpr`). State: a partial tree
   with holes plus a target number. Move: fill the leftmost hole with `+ - *` or a small
   constant. Reward: 1 if `eval` hits the target. Measure: solve rate on held-out
   targets against plain MCTS and random play, at increasing depth. If the learned value
   does not beat plain search, stop there.
2. **Pure Dung game**: add attacks or supports from a fixed menu until your argument is
   in the grounded extension. Computed win condition, no language model.
3. **Hybrid** (apex `wp-argument-game-hybrid`): a language model proposes the moves,
   the engine checks them and scores. The step towards argument games on AIF² /
   flexiarg graphs.

## Experts think in patterns (Joe, 2026-10-30)

From memory, not yet checked against the sources:

- *Learning to Execute* (Zaremba & Sutskever, 2014): an LSTM trained to predict the
  output of small programs. Accuracy fell as nesting and length grew; curriculum
  training helped but did not remove the limit. A fixed-size network cannot run an
  arbitrarily deep evaluation in fixed compute.
- de Groot, and Chase & Simon: chess masters do not search deeper than weaker players.
  They consider better candidate moves first. They recall real positions far better,
  but random positions hardly better; the advantage is a store of chunks (Simon's
  estimate was tens of thousands), and it applies only to positions that look like real
  play. Gobet's templates; Klein's recognition-primed decision.
- AlphaZero has the same two parts: the policy network is the fast pattern step and
  MCTS is the slow check. The policy alone already plays well.

What follows for this excursion:

1. **Do not learn to execute; learn to recognise.** Neural eval should name a subtree
   as a known chunk with a known value, and hand everything else to the exact
   evaluator. The hybrid is pattern step plus exact check, not a network that runs code.
2. **Random positions defeat chunks.** Randomly generated targets for the arithmetic game
   give a pattern step nothing to learn. A natural distribution, such as expressions
   from real Emacs Lisp history, would. This is a change to step 1 above.
3. **The measure is search saved.** Experts search less, not deeper. Compare nodes
   expanded to solve, with a prior from a pattern library against a uniform prior. For
   argument graphs, the flexiarg library is already that chunk store; the cascade is its
   fast step.
4. **The WM has this shape too.** The cascade proposes; G scores. Whether the pattern
   step saves search there could be measured with no new game at all.

## What would make it critical-path

- The AIF work (WM active inference) needs a policy search it cannot do by enumeration.
- The argument graph (`apex.edn`) grows to where choosing which sorry to discharge next
  is itself a search problem with a computable payoff.
- A venue (Mozilla or another funder) wants the argument-technology line specifically.
