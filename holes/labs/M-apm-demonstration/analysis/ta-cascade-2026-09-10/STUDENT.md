# Student-facing teaching card: a93A01

For f:(0,1)→ℝ, prove uniform continuity is equivalent to:
for every ε>0 there exists N>0 such that, for distinct x,y,
|f(x)−f(y)|/|x−y|>N implies |f(x)−f(y)|<ε.

Start by writing the quantifiers of uniform continuity. Set A=|f(x)−f(y)| and
d=|x−y|. Plan the two implications separately. For each proposed method, state
its missing hypotheses before attempting to apply it.

The following are progressive hints. In this Markdown prototype all levels
are visible; a future tool should reveal one level at a time. This is not a
claim that a staged-delivery mechanism exists.

**Level 1 — identify the intermediate questions.**

Forward: how could a large ratio A/d force x and y to be close? Which factor
would need a bound? Reverse: what does the hypothesis already tell you when
the ratio is large, and what inequality holds in the remaining case?

Relevant library entries: `math-informal-CA/estimate-by-bounding`,
`math-informal/unfold-the-definition`, `math-informal/split-into-cases`.
A method name does not establish its applicability.

**Level 2 — inspect the conditions.**

Forward: uniform continuity gives one distance tolerance for oscillation <1
at every point. Can finitely many points cover the domain at that tolerance?
Explain why this is possible on (0,1) without claiming that (0,1) is compact.
Use `math-informal/local-to-global` only after supplying finiteness.

Reverse: handle x=y separately. For x≠y, compare A/d with the positive N supplied
for this very ε. What distance tolerance makes the low-ratio branch small?

**Level 3 — construction hints.**

Forward: choose a sufficiently fine finite midpoint mesh in (0,1); compare
f(x) with f at a nearby midpoint and take a finite maximum of their values.
Then use the ε-specific uniform-continuity tolerance with that global bound.

Reverse: multiply A/d≤N by the positive d. Choose δ to make Nδ=ε, and check
where the strict inequality comes from.

**Checkpoint before the full worked proof.**

Explain why boundedness of a general metric domain is insufficient, why the
forward proof needs more than continuity, and why the reverse proof does not
use total boundedness. Record the node where you are stuck, the patterns you
actually read, and your attempted step. “Could not use this pattern” alone
is not enough to distinguish a search problem from a missing premise.
