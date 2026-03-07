# FM-001/F1 — ArSE Question Drafts

Context: Issue #45 asks us to falsify (or gain high confidence in) the claim
that $R(B_{n-1}, B_n) \ge 4n-1$ by probing small-$n$ cases and checking for
structural obstructions. The following Stack Exchange–style prompts are ready to
feed into the ArSE pipeline so that we can gather clarifying answers before
committing to a construction program.

## Q1 — Reproducing the small-$n$ enumerations

**Working title:** “How were the $R(B_{n-1}, B_n) = 4n-1$ cases for $n \le 20$
verified computationally?”

**Motivation:** Wesley (2025) reports a SAT/SMS workflow that proves
$R(B_{n-1}, B_n) = 4n-1$ for all $n \le 20$ and for every $n$ with $2n-1$ a
prime power $\equiv 1 \pmod 4$, using 2-block-circulant colorings plus DRAT
certificates.citeturn1search0 To judge how much falsification space
remains, we need to know whether those enumerations already cover the specific
$n$ that FM-001 cares about and whether the certificates can be reproduced or
extended.

**Question text (draft):**

> The recent “Lower bounds for book Ramsey numbers” paper proves
> $R(B_{n-1}, B_n)=4n-1$ for every $n \le 20$ via 2-block-circulant graphs and
> SAT/DRAT certificates. Could someone outline the precise encoding for, say,
> $n=8$ or $n=10$, and explain how the certificates certify that no coloring on
> $4n-2$ vertices avoids both $B_{n-1}$ and $B_n$? I would especially appreciate
> data or code that lets us rerun the SMS search for other small $n$.

**What we hope to learn:** whether recomputing the small cases is tractable for
us, and if there are edge cases (e.g., $n=11$) where the computation was fragile.

## Q2 — Number-theoretic scope of block-circulant constructions

**Working title:** “When does the Paley-style block-circulant coloring on
$4n-2$ vertices exist?”

**Motivation:** The same paper shows that if $2n-1$ is a prime power congruent
to $1 \pmod 4$ then $R(B_{n-1}, B_n)=4n-1$ via a Paley-type template.citeturn1search0
If we could extend that construction to composite moduli or other congruence
classes, we might find counterexamples to $4n-1$ for new $n$ or verify more
cases automatically.

**Question text (draft):**

> Paley-type 2-block-circulant graphs on $4n-2$ vertices give extremal examples
> for $R(B_{n-1}, B_n)$ whenever $2n-1$ is a prime power $\equiv 1 \pmod 4$.
> Are there known generalizations (e.g., to Singer difference sets, cyclotomic
> classes, or circulants with more blocks) that work for composite $2n-1$?
> More concretely, can one write down an explicit coloring on $4n-2$ vertices
> that avoids $B_{n-1}$ in red and $B_n$ in blue when $2n-1=45$ or $53$?

**What we hope to learn:** whether number-theoretic constructions already cover
the $n$ we care about, or whether new circulant templates are worth exploring.

## Q3 — Stability/obstruction phenomena

**Working title:** “Is there a stability theorem for near-extremal book Ramsey
colorings?”

**Motivation:** Off-diagonal book Ramsey results suggest that random or
$k$-partite constructions transition depending on $n$, and Yuriy Wigderson’s
recent work asks whether quasi-randomness forces the $4n-1$ upper bound.citeturn1search5
Knowing whether every $K_{4n-2}$ coloring that avoids both $B_{n-1}$ and $B_n$
must resemble a specific template would help direct the falsification search.

**Question text (draft):**

> Suppose we 2-color $K_{4n-2}$ without creating a red $B_{n-1}$ or a blue
> $B_n$. Must the coloring be “close” (in edit distance or cut distance) to a
> known block-circulant or quasi-random template? Are there stability theorems
> analogous to those for $r(K_{s}, K_{t})$ that would force a large bipartite or
> Paley-like structure in every near-extremal example?

**What we hope to learn:** whether structural theorems already rule out novel
counterexamples, or whether FM-001 should focus on explicit small-$n$
enumerations.

---

Posting order suggestion:

1. **Q1** establishes the status of computational certificates (essential for
   deciding whether falsification is still open for small $n$).
2. **Q2** probes for new constructions that could produce counterexamples.
3. **Q3** asks about structural obstructions; useful once we know which $n$
   remain unresolved.

Capturing the answers (or the lack thereof) will determine whether FM-001 moves
forward with additional computation, literature search, or a pivot back to
construction mode.

## References

1. Wesley, *Lower bounds for book Ramsey numbers*, Discrete Mathematics (2025).citeturn1search0
2. Wigderson, *Book Ramsey numbers I*, arXiv:2208.01630 (2023).citeturn1search5
