# Canned CT problem — commissioning trial, PRIMARY

**Authored 2026-08-13 by claude-2 for the pre-go-live commissioning test.
NOT a corpus problem. Its purpose is to exercise the full chain: pattern
retrieval → scribe capture → deposit → recall → demonstrated effect.**

Difficulty is a dial we control (Fable): if Run 0 solves this competently
cold, the problem has no headroom and must be re-authored, not
reinterpreted.

---

## Statement

Let $R$ be a ring (associative, unital, not assumed commutative), and let
$\mathrm{Mod}\text{-}R$ denote the category of right $R$-modules.

**(a)** Let $M$ be a right $R$-module. Show that the functor
$M \otimes_R - \colon R\text{-}\mathrm{Mod} \to \mathbf{Ab}$ preserves all
small colimits.

**(b)** Conversely, let $T \colon R\text{-}\mathrm{Mod} \to \mathbf{Ab}$ be
an additive functor that preserves all small colimits. Show that there is a
right $R$-module $M$ and a natural isomorphism
$T \cong M \otimes_R -$.

**(c)** Show that the $M$ of part (b) is determined up to isomorphism, and
identify it explicitly in terms of $T$.

---

## Expected retrieval set

The six `math-informal-CT` patterns, of which these three should be
load-bearing:

| Pattern | Where it bites |
|---|---|
| `transpose-across-an-adjunction` | (a) is cleanest via tensor–hom: $M \otimes_R -$ is a left adjoint, and left adjoints preserve colimits. Transposing turns a colimit statement into a hom statement. |
| `check-it-on-generators` | (b) turns on $R$ generating $R\text{-}\mathrm{Mod}$: define $M := T(R)$, check the natural transformation on $R$, extend by colimits. |
| `compare-universal-properties` | (c) is uniqueness — the two constructions solve the same universal problem rather than agreeing term by term. |

The remaining three (`chase-the-diagram`, `factor-and-lift`,
`strictify-via-coherence`) are the **in-domain distractors**: same family,
plausibly adjacent, but not the route. A retrieval that cannot separate the
three load-bearing patterns from their three siblings has not demonstrated
precision.

## Why this problem, and the self-match discipline

Fable's warning is the design constraint: *"You authored the problem and
know the six target patterns; shared vocabulary can make retrieval pass by
construction."*

The mitigation is **not** to engineer vocabulary disjointness. A runner
facing an adjunction problem *should* retrieve an adjunction pattern —
suppressing that would test nothing. The mitigation is that the statement is
written in the vocabulary of its own subject (ring, module, tensor product,
colimit, additive functor) and **not** in the vocabulary of the patterns
(transpose, adjunct, generator, universal property, coherence, lifting).
None of those six words appears above.

Eilenberg–Watts was chosen because:

1. It is a genuine prelim question with a known correct route, so Run 0's
   competence is scorable against ground truth rather than by taste.
2. Its natural solution needs **three** patterns, not one — a single-pattern
   problem cannot distinguish retrieval from luck.
3. Part (b) has real headroom: the "define $M := T(R)$ and check on
   generators" move is exactly the step a weak solver stalls on, which is
   the assay lesson from case-1 (assay on the step that stalled, never on
   the step that succeeded).
4. The three non-load-bearing CT patterns are genuinely plausible here,
   which makes the precision test honest rather than a straw distractor.

## Ground truth (for scoring; NOT to be shown to any run)

(a) $M \otimes_R -$ is left adjoint to $\mathrm{Hom}_{\mathbf{Ab}}(M, -)$;
left adjoints preserve colimits. (Or directly: colimits in both categories
are computed on underlying abelian groups, and $\otimes$ commutes with them.)

(b) Set $M := T(R)$, a right $R$-module via the left action of $R$ on itself
by multiplication, which $T$ carries to endomorphisms of $T(R)$. Build
$\eta_N \colon T(R) \otimes_R N \to T(N)$ natural in $N$. It is an
isomorphism at $N = R$; both sides preserve colimits; every module is a
colimit of copies of $R$ (a free presentation suffices); conclude by
extending along that presentation.

(c) $M \cong T(R)$. Uniqueness: any $M'$ with $M' \otimes_R - \cong T$ gives
$M' \cong M' \otimes_R R \cong T(R)$.

## Pass conditions (see the pre-registration for falsifiers)

- **Retrieval**: the three load-bearing patterns surface, by name, ranked
  above the three in-domain distractors.
- **Precision**: the negative probe (`problem-analysis-negative.md`) does
  **not** retrieve any of the six.
- **Stopword check**: rerun each retrieval with stopwords stripped from the
  query. If the ranking collapses, the match was noise, not meaning.
