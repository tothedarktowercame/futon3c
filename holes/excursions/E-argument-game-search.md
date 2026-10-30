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

## How would a living thing compute G? (Joe, 2026-10-30)

If organisms minimise expected free energy as routinely as Friston claims, they cannot be
enumerating policies; the cost grows exponentially with the horizon. From memory, not yet
checked: the active inference literature gives three ways out.

- **Hierarchy.** Deep temporal models: each level chooses among a few short policies,
  and its choice sets the context for the level below.
- **Pruning.** Sophisticated inference (Friston et al. 2021) is a recursive tree search
  that drops branches of low probability.
- **Amortisation through habit.** The habit prior E (Friston et al. 2016) is learned by
  counting the policies actually chosen, so with time it mirrors what G would pick.
  Deep active inference work amortises further: Fountas et al. 2020 run MCTS over G
  with a habitual network that prunes it, which is AlphaZero's policy network in active
  inference terms; Millidge 2020 learns a bootstrapped estimate of G, like a Q-function.

This is the chess-chunk story in biological terms: E is the fast pattern step and G is
the slow check, run only where E is uncertain. Optimising a policy network against G
is the route the literature already takes, so it is not new; what we might add is
*where the training data comes from*.

The WM has the ingredients. `DarkTower/AIF/Terms.lean` defines E (`Habit`) and the
tempered posterior σ(ln E − F − γG); futon2 `aif/strategic_habit.clj` accumulates E as
counts of strategic selections, behind an opt-in. That is the 2016 counting habit, not
an amortised G. A measurable question with no new game: on recorded clicks, how often
does the habit's top policy agree with G's, and when they agree, how much scoring could
have been skipped? If agreement rises with experience, the WM is amortising, as an
organism would.

## What would make it critical-path

- The AIF work (WM active inference) needs a policy search it cannot do by enumeration.
- The argument graph (`apex.edn`) grows to where choosing which sorry to discharge next
  is itself a search problem with a computable payoff.
- A venue (Mozilla or another funder) wants the argument-technology line specifically.

## Literature check (2026-10-30)

The survey the "not yet surveyed" notes above asked for has been run:
`M-argument-game-search-lit` → `deep-research-argument-game-search.md` (report, with a
verdict table per memory-claim and a verification legend) and
`deep-research-argument-game-search.notes/` (six thread notes). Headline corrections to
the from-memory paragraphs above, each cited in the report:

- **"Experts search less, not deeper" overstates de Groot.** Campitelli & Gobet 2004:
  experts search *more and deeper* on hard positions, recognition on easy ones. Measure
  nodes-to-solve split by familiar vs unfamiliar, not averaged.
- **Friston 2016's habit E accumulates the policy *posterior*** (`e ← e + η·π` in SPM),
  not a count of chosen policies. futon2's `strategic_habit.clj` selection counter is a
  hard-count variant, not the 2016 rule; they agree only where the posterior is sharp.
- **Fountas 2020's habitual network is a PUCT prior, not a pruner.** Stopping is a
  separate threshold (T_dec = 0.8); leaves use explicit EFE terms, no learned value.
- **"Name a subtree as a known chunk with a known value" has no source.** Chess chunks
  suggest moves; search evaluates them. Present as our own synthesis.
- Deep temporal models is 2017 (NBR 77), not 2018.

What already exists: the computed-win Dung game (step 2's game is off the shelf: grounded
game, Dung 1995 / Modgil & Caminada 2009, polynomial win check; ICCMA solvers; stable
semantics is a degenerate-reward trap). What does not: the arithmetic hole-filling game as
a benchmark (Countdown / Stream of Search and symbolic regression cover its halves), any
self-play that *builds* a framework until a semantics accepts the claim, any argumentation
semantics as RL reward for an LLM proposer, and any habit-vs-G agreement curve. Rob's
caution is the best-supported claim in the file, and it moves: with an LLM proposing
attack edges, the judged reward returns at each edge. No peer-reviewed CPU-only AlphaZero
budget exists; Connect Four is the practical edge (≈ a day on one desktop + GPU, [BLOG]).
