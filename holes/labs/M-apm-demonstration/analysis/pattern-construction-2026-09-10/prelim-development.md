# Pattern-directed development: a93A01

## Entry 1 — statement, exposure and first plan (before writing the proof below)

The task is the equivalence between uniform continuity of f:(0,1)→ℝ and:
for every ε>0 there is N>0 such that for distinct x,y in (0,1),
|f(x)-f(y)|/|x-y|>N implies |f(x)-f(y)|<ε.

This is a deliberately familiar development example, not a fresh-worker test.
I had inspected this statement and its bounded-image pattern in the earlier
packet. At this turn's start I read the existing Main's definitions and first
bridge lemma (first 45 lines), but not its remaining solution. No claim of
independent discovery or causal assistance follows. I will finish this outline
and its paper proof before comparing with the complete retained solution.

Library consultation actually performed: list candidate paths matching bound,
continuity, epsilon, contradiction, definition, cases and inequalities, then
read `math-informal/unfold-the-definition`, `math-informal/split-into-cases`,
`math-informal-CA/estimate-by-bounding`,
`math-informal-CA/show-both-inequalities`, and
`math-formalization-CA/uniform-continuity-boundedness`. The prior turn read
`math-strategy/plan-first-attempt` and `math-strategy/proof-architecture`.

Top-level obligation P: prove both implications, preserving the quantifiers.
Method selection: `proof-architecture` and `plan-first-attempt`; the two
implications are independent statements and can have different methods.
The equivalence introduction itself is an ordinary logical rule, not a
new pattern. `show-both-inequalities` is not selected: this is logical
equivalence, not an order comparison, and inventing an order encoding would
not help this proof.

P.1 (forward): use the UC definition to make nearby values close. To use the
large-quotient assumption to force nearness, need a global numerator bound.
Candidate bounded-image pattern points at that missing intermediate fact.
Its literal prose is not accepted unchanged: bounded metric domain alone is
insufficient, and this problem needs a numerator bound, not a bound below on
a denominator. Record this as a correction need; prove the needed boundedness
from the actual interval and UC hypotheses using the ordinary finite-cover
argument. `estimate-by-bounding` warrants the choice to bound rather than
compute the oscillation. Need: a finite cover by balls small enough for UC.

P.2 (reverse): unpack UC and the quotient condition; `unfold-the-definition`
fits because the required δ must be chosen from ε and the given N. Select
`split-into-cases` on whether the quotient exceeds N: the given condition
handles the high branch; elementary multiplication may handle the low branch.
Also isolate x=y because the quotient is undefined in ordinary real analysis.
Need: check an explicit δ works and every case has a strict ε bound.

Unknown at this plan boundary: exact witness constants; finite-cover witness;
whether a more specific existing pattern expresses the reverse threshold
argument. These must not be labelled supplied by the selected broad patterns.

## Entry 2 — expanding the missing steps through another library consultation

Searched the canonical mathematical families for `finite (cover|net)`,
`totally bounded`, `large.*small`, `threshold` and `quotient`, and read
`math-informal/local-to-global` and
`math-informal-CA/optimise-a-free-parameter` in full.

`local-to-global` helps structure P.1.1: bound on each small member of a
finite cover, then take the maximum. Its warning that not every property is
local matters: an arbitrary infinite family of bounded images need not have
a common bound. Finiteness and a single fixed UC tolerance supply what this
application needs. Compatibility is automatic because all restrictions come
from the same function. The finite net and maximum remain explicit proof work.

No specific reverse-threshold pattern was located in these bounded searches.
This is a search limitation, not an exhaustive theorem of library absence.
`optimise-a-free-parameter` is not selected: no optimum or balancing argument
is needed. Choosing δ=ε/N is ordinary witness construction after obtaining
the inequality A≤Nd. A proposed more specific pattern could later describe
this two-regime argument, but is not counted as existing-library reuse here.

## Entry 3 — complete structured proof

This proof was written after Entries 1–2 and before reading the retained full
solution. Notation: I=(0,1), A=|f(x)−f(y)|, d=|x−y|. For x≠y, d>0 and
|[f(x)−f(y)]/(x−y)|=A/d. All denominators below are explicitly positive.

**〈1〉1. ASSUME f is uniformly continuous on I. PROVE the quotient condition.**

Method: `proof-architecture`; first obtain one global bound, then use the
ε-specific modulus. The challenged bounded-image pattern suggested this
separation but is not accepted as the mathematical warrant. Dependencies are
〈2〉1 and 〈2〉2 below.

**〈2〉1. PROVE there is M>0 such that |f(x)|<M for every x∈I.**

Methods: `unfold-the-definition`, `local-to-global`, `estimate-by-bounding`.
Prerequisites: a uniform modulus at tolerance 1; a finite net in this actual
interval. These are discharged rather than assumed:

- **〈3〉1.** By uniform continuity with ε=1, choose η>0 such that
  x,y∈I and |x−y|<η imply |f(x)−f(y)|<1.
- **〈3〉2.** By the Archimedean property choose an integer n≥1 with
  1/n<η. For 0≤i<n define cᵢ=(i+1/2)/n∈I. Every x∈I belongs to one
  interval [i/n,(i+1)/n], and hence |x−cᵢ|≤1/(2n)<η. This constructs
  the finite net, without assuming the open interval is compact.
- **〈3〉3.** Let M=1+max₀≤ᵢ<n |f(cᵢ)|. This finite nonempty maximum
  exists and M≥1>0. For the i supplied by 〈3〉2, the triangle inequality
  gives |f(x)|≤|f(x)−f(cᵢ)|+|f(cᵢ)|<1+|f(cᵢ)|≤M.
- **〈3〉4. QED 〈2〉1**, by 〈3〉1–〈3〉3.

**〈2〉2. PROVE the required N exists for each ε>0.**

Method: `estimate-by-bounding` to infer a distance bound from a quotient.
Prerequisites: numerator bounded by 2M and a positive modulus δ.

- **〈3〉1.** Fix ε>0. By uniform continuity choose δ>0 such that
  x,y∈I and |x−y|<δ imply |f(x)−f(y)|<ε.
- **〈3〉2.** Set N=2M/δ>0. Fix distinct x,y∈I and assume A/d>N.
  From 〈2〉1, A≤|f(x)|+|f(y)|<2M.
- **〈3〉3.** If d≥δ, then A/d≤2M/d≤2M/δ=N, a contradiction.
  Therefore d<δ and 〈3〉1 gives A<ε.
- **〈3〉4. QED 〈2〉2**, with the witness N from 〈3〉2.

**〈2〉3. QED 〈1〉1**, by 〈2〉1–〈2〉2 and the identity A/d above.

**〈1〉2. ASSUME the quotient condition. PROVE uniform continuity on I.**

Methods: `unfold-the-definition`, `split-into-cases`, `estimate-by-bounding`.
The case split is exhaustive and supplies genuinely different arguments.
No boundedness or total-boundedness premise is needed in this direction.

- **〈2〉1.** Fix ε>0 and choose N>0 from the quotient condition at this
  ε. Set δ=ε/N>0. Fix x,y∈I with d<δ.
- **〈2〉2.** If x=y, then A=0<ε.
- **〈2〉3.** Otherwise d>0. If A/d>N, the given quotient condition
  gives A<ε.
- **〈2〉4.** In the remaining case A/d≤N, multiplication by d>0 gives
  A≤Nd<Nδ=ε. The strict inequality uses N>0 and d<δ.
- **〈2〉5. QED 〈1〉2**, by 〈2〉1–〈2〉4 and the UC definition.

**〈1〉3. QED**, by the two implications 〈1〉1 and 〈1〉2.

## What was reused and what was constructed here

The selected patterns determined a revisable decomposition: expose quantifiers,
separate the global bound from the local modulus, assemble finitely many local
bounds, and split the reverse implication at its existing threshold. The
mesh, constants M,N,δ and algebra were supplied by the agent as ordinary
mathematics. They were not retrieved as memory implementations. This session
used no memory body as an implementation and makes no memory-use claim.

The source pattern's criticism has two exact witnesses: bounded discrete ℕ
with d(m,n)=1 for m≠n admits the uniformly continuous unbounded map n↦n;
and in this proof it is A, the numerator, that needs the global bound. A future
revision should say “totally bounded domain” (or explicitly restrict to suitable
Euclidean domains) and identify which quotient factor is being controlled.

Candidate middle-level descriptions exposed by this development, not yet
published patterns:

1. **Finite-net oscillation bound:** one uniform local oscillation bound plus
   a finite net yields a global value bound by a finite maximum. Without
   finiteness or a uniform local tolerance the assembly is not justified.
2. **Threshold dichotomy constructs a modulus:** if the high-ratio regime
   already makes A small, use A≤Nd in the complementary regime and choose
   d<ε/N. Requires positive N and a separately handled zero denominator.

These are candidate abstractions of standard mathematics, not claims of novel
mathematical discovery. Their generality and overlap with existing patterns
need cross-example review before library promotion.
