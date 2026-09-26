# Canned analysis problem — commissioning trial, NEGATIVE PROBE

**Authored 2026-08-13 by claude-2. Its job is to FAIL to retrieve.**

Fable: *"Add a negative probe: a second canned problem from a different
domain that must NOT retrieve the CT six."* Retrieval has to demonstrate
precision and honest refusal, not just recall — this test's t91A05.

---

## Statement

Let $(f_n)_{n \ge 1}$ be a sequence in $L^1(\mathbb{R}, \lambda)$ with
$f_n \to f$ in $L^1$ norm.

**(a)** Show that there is a subsequence $(f_{n_k})$ with $f_{n_k} \to f$
almost everywhere.

**(b)** Show by explicit counterexample that the full sequence need not
converge almost everywhere.

**(c)** Suppose additionally that $f_n \ge 0$ and
$\int f_n \to \int f$. Show that $f_n \to f$ in measure on every set of
finite measure.

---

## Expected result: NOTHING

**None of the six `math-informal-CT` patterns may surface.** Nor should any
of the four mined `math-formalization` / `math-strategy` content patterns —
they are Lean formalization and Galois-theoretic, not measure theory.

An honest-empty result here is a **pass**. A near-miss that surfaces
`chase-the-diagram` because both texts contain "show that" is a **fail**,
and precisely the failure mode the stopword standard exists to catch.

## Why this problem

1. **Genuinely different subject.** Measure theory and real analysis; no
   categorical content at any depth, so there is no defensible reading under
   which a CT method pattern is the right route.
2. **Same surface register.** It is a prelim problem of comparable length,
   difficulty and phrasing, with the same connective tissue — "let", "show
   that", "suppose", "sequence", "converges". If retrieval is riding on
   register or function words rather than meaning, this is where it shows.
3. **It has its own honest answer.** Riesz–Fischer for (a), the typewriter
   sequence for (b), Scheffé for (c). If the store later grows analysis
   patterns, this problem stops being a pure negative probe and becomes a
   positive one — record that transition rather than quietly keeping it as a
   negative.

## The relevance floor this instantiates

The batch era's `t91A05` was the guard that a well-formed query with nothing
relevant in the store must return **honest-empty** rather than a confident
wrong match. That guard has never been applied to the pattern channel. This
problem is its pattern-side equivalent, and it must be run in the same pass
as the primary — a retrieval system evaluated only on queries it should
answer has been evaluated on half its job.

## Scoring

| Outcome | Verdict |
|---|---|
| No CT pattern surfaces | **PASS** |
| A CT pattern surfaces below the relevance floor but is reported as such | pass, with the floor's behaviour recorded |
| Any CT pattern surfaces as a candidate | **FAIL** — precision failure, per the pre-registration |
