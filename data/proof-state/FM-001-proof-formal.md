# FM-001: R(B_{n-1}, B_n) = 4n-1 for Prime Power q — Formal Proof

## Problem

Let B_k = K_2 + K̄_k denote the book graph: k triangles sharing a common
edge (the "spine"). For n ≥ 2, determine R(B_{n-1}, B_n).

**Claim.** If q = 2n−1 is a prime power with q ≡ 1 (mod 4), then
R(B_{n-1}, B_n) = 4n−1.

The upper bound R(B_{n-1}, B_n) ≤ 4n−1 is due to Rousseau and Sheehan
[RS78]. We prove the matching lower bound by constructing an explicit
witness graph on 4n−2 vertices.

## Preliminaries

**Book-freeness criterion.** G contains B_k if and only if there exists an
edge uv with |Γ(u) ∩ Γ(v)| ≥ k. So G is B_k-free iff every edge has
fewer than k common neighbors.

**2-block-circulant graph.** Given an abelian group (G, +) and subsets
D₁₁, D₁₂, D₂₂ ⊆ G, define the graph Γ_G(D₁₁, D₁₂, D₂₂) on vertex
set V₁ ⊔ V₂ (two copies of G) with adjacency:

- x, y ∈ V₁: xy is an edge ⟺ y − x ∈ D₁₁
- x, y ∈ V₂: xy is an edge ⟺ y − x ∈ D₂₂
- x ∈ V₁, y ∈ V₂: xy is an edge ⟺ y − x ∈ D₁₂

**Difference counts.** For X, Y ⊆ G and d ∈ G:

    Δ(X, Y, d) = |{(x, y) ∈ X × Y : x − y = d}|

Note Δ(X, Y, 0) = |X ∩ Y|.

## QR/NR Intersection Lemma

Let q be a prime power with q ≡ 1 (mod 4). Let Q ⊂ F_q^× be the nonzero
quadratic residues and N = F_q^× \ Q the non-residues. Then |Q| = |N| =
(q−1)/2, and since q ≡ 1 (mod 4), we have −1 ∈ Q, so Q = −Q and N = −N.

The difference counts are:

| Δ(X, Y, d)   | d ∈ Q     | d ∈ N     |
|---------------|-----------|-----------|
| Δ(Q, Q, d)   | (q−5)/4   | (q−1)/4   |
| Δ(N, N, d)   | (q−1)/4   | (q−5)/4   |
| Δ(Q, N, d)   | (q−1)/4   | (q−1)/4   |

*Proof.* Standard character sum computation using the quadratic character
χ of F_q. See [W25, Lemma 10] or any treatment of Paley tournaments. □

## Construction

Let q = 2n−1 be a prime power with q ≡ 1 (mod 4). Define:

    G = Γ_{F_q}(Q, Q, N)

That is: D₁₁ = Q, D₁₂ = Q, D₂₂ = N.

G has 2q = 4n−2 vertices. We must show G is B_{n-1}-free and its
complement Ḡ is B_n-free.

**Complement structure.** Since −1 ∈ Q:
- D̄₁₁ = F_q^× \ Q = N
- D̄₂₂ = F_q^× \ N = Q
- D̄₁₂ = F_q \ Q = N ∪ {0}

## Theorem. G is B_{n-1}-free.

We show max_{uv edge} |Γ_G(u) ∩ Γ_G(v)| = n−2 < n−1.

**Case 1** (u, v ∈ V₁, d = v−u ∈ Q).
Common neighbors come from V₁ (via D₁₁) and V₂ (via D₁₂):

    |Γ(u,v)| = Δ(Q, Q, d) + Δ(Q, Q, d) = 2 · (q−5)/4 = (q−5)/2 = n−3.

**Case 2** (u, v ∈ V₂, d = v−u ∈ N).
Common neighbors from V₂ (via D₂₂) and V₁ (via D₁₂):

    |Γ(u,v)| = Δ(N, N, d) + Δ(Q, Q, d) = (q−5)/4 + (q−1)/4 = (q−3)/2 = n−2.

**Case 3** (u ∈ V₁, v ∈ V₂, d = v−u ∈ Q).
Common neighbors from V₁ (via D₁₁ and D₁₂^T) and V₂ (via D₁₂ and D₂₂).
Since −1 ∈ Q, the cross-block sum Σ reduces to a difference count:

    |Γ(u,v)| = Δ(Q, Q, d) + Δ(Q, N, d) = (q−5)/4 + (q−1)/4 = (q−3)/2 = n−2.

Maximum: max(n−3, n−2, n−2) = n−2 < n−1. □

## Theorem. Ḡ is B_n-free.

We show max_{uv edge in Ḡ} |Γ_Ḡ(u) ∩ Γ_Ḡ(v)| = n−1 < n.

**Case 4** (u, v ∈ V₁, d = v−u ∈ N).

    |Γ̄(u,v)| = Δ(N, N, d) + Δ(N∪{0}, N∪{0}, d)

For d ∈ N: Δ(N∪{0}, N∪{0}, d) = Δ(N,N,d) + [d ∈ N] + [−d ∈ N] =
(q−5)/4 + 1 + 1 = (q−5)/4 + 2.

(The extra terms arise because 0 ∈ N∪{0} and d−0 = d ∈ N, 0−(−d) gives
−d ∈ N since −1 ∈ Q.)

    |Γ̄(u,v)| = (q−5)/4 + (q−5)/4 + 2 = (q−5)/2 + 2 = n−1.

**Case 5** (u, v ∈ V₂, d = v−u ∈ Q).

    |Γ̄(u,v)| = Δ(Q, Q, d) + Δ(N∪{0}, N∪{0}, d)

For d ∈ Q: Δ(N∪{0}, N∪{0}, d) = Δ(N,N,d) + [d ∈ N∪{0}] + [−d ∈ N∪{0}] =
(q−1)/4 + 0 + 0 = (q−1)/4.

    |Γ̄(u,v)| = (q−5)/4 + (q−1)/4 = (q−3)/2 = n−2.

**Case 6** (u ∈ V₁, v ∈ V₂, d = v−u ∈ N∪{0}).

*Sub-case 6a* (d = 0):

    |Γ̄(u,v)| = |N ∩ (N∪{0})| + |(N∪{0}) ∩ Q| = |N| + 0 = (q−1)/2 = n−1.

*Sub-case 6b* (d ∈ N, d ≠ 0):

    |Γ̄(u,v)| = (Δ(N, N, d) + [d ∈ N]) + (Δ(N, Q, d) + [d ∈ Q])
              = ((q−5)/4 + 1) + ((q−1)/4 + 0)
              = (q−1)/2 = n−1.

Maximum: max(n−1, n−2, n−1, n−1) = n−1 < n. □

## Main Result

**Theorem.** If q = 2n−1 is a prime power with q ≡ 1 (mod 4), then
R(B_{n-1}, B_n) = 4n−1.

*Proof.* The graph G = Γ_{F_q}(Q, Q, N) on 2q = 4n−2 vertices is
B_{n-1}-free (max common neighbors n−2) and has B_n-free complement
(max common neighbors n−1). Therefore R(B_{n-1}, B_n) ≥ 2q+1 = 4n−1.
Combined with the Rousseau-Sheehan upper bound [RS78], equality holds. □

## Computational Verification (n = 25, q = 49 = 7²)

Field: GF(49) = F_7[x]/(x²+1). Verified all 6 edge cases:

| Case | Edge type          | Difference class | Max |Γ| | Bound  | Status |
|------|--------------------|------------------|---------|--------|--------|
| 1    | V₁–V₁ in G        | d ∈ Q            | 22      | < 24   | ✓      |
| 2    | V₂–V₂ in G        | d ∈ N            | 23      | < 24   | ✓      |
| 3    | V₁–V₂ in G        | d ∈ Q            | 23      | < 24   | ✓      |
| 4    | V₁–V₁ in Ḡ        | d ∈ N            | 24      | < 25   | ✓      |
| 5    | V₂–V₂ in Ḡ        | d ∈ Q            | 23      | < 25   | ✓      |
| 6    | V₁–V₂ in Ḡ        | d ∈ N∪{0}        | 24      | < 25   | ✓      |

All values match the algebraic predictions exactly: n−3 = 22, n−2 = 23,
n−1 = 24.

## Applicability

The condition "q = 2n−1 is prime power, q ≡ 1 (mod 4)" is satisfied for
infinitely many n. For n ≤ 50, the qualifying values are:

    n ∈ {3, 5, 7, 9, 13, 15, 19, 21, 25, 27, 31, 37, 41, 45, 49}

(15 of 49 cases.)

For n ≤ 20, Wesley [W25] verified R(B_{n-1}, B_n) = 4n−1 computationally
via SAT/IP solvers, covering cases where q is not a prime power.

The conjecture R(B_{n-1}, B_n) = 4n−1 for ALL n ≥ 2 remains open. The
34 cases with n ≤ 50 where q is not a prime power are the subject of
FM-001b.

## Heuristic Trail

The proof emerged through the following sequence of attempts and dead ends:

1. **SAT encoding** (n = 3, 4): Confirmed R(B₂,B₃) = 11 and R(B₃,B₄) = 15
   via kissat. Timed out for n ≥ 5. Established the pattern 4n−1 but could
   not scale.

2. **Paley(q) direct** (n = 25, q = 97): Paley(97) has 97 vertices but
   we need 98. One vertex short.

3. **Extended Paley** (Paley(97) + ∞): Adding vertex ∞ adjacent to all QRs
   creates a twin obstruction — vertices 0 and ∞ have identical neighborhoods,
   giving 48 common complement-neighbors (need < 25). Refuted.

4. **Cayley on Z₉₈**: SA search over connection sets, 20 restarts × 50K
   steps. Best max common neighbors = 26 (need ≤ 23). SRG(98,k,≤23,μ)
   proved impossible (97 prime forces degenerate parameters). Refuted.

5. **Wesley 2-block-circulant** [W25]: Literature discovery. The key insight
   is using *two* copies of F_q with *asymmetric* block structure (D₁₁ = Q,
   D₁₂ = Q, D₂₂ = N) instead of a single circulant. The asymmetry trades
   G-freeness headroom in one block for complement-freeness headroom in the
   other. Verified computationally for n = 25 (GF(49)), then proved
   algebraically for all qualifying n.

**Dead ends catalogued**: Paley extension (twin obstruction), Cayley Z₉₈
(SA + SRG impossibility), extended Paley with perturbed S (uniform Paley
pressure).

## References

- [RS78] C.C. Rousseau and J. Sheehan, "On Ramsey numbers for books,"
  *Journal of Graph Theory* 2 (1978), 77–87.
- [W25] W.J. Wesley, "Lower Bounds for Book Ramsey Numbers,"
  arXiv:2410.03625v2, September 2025.

## Mission Record

- **Problem**: FM-001 (FrontierMath pilot)
- **Mode**: MAP (completed SPEC → FALSIFY → CONSTRUCT → VERIFY → MAP)
- **Key conjecture**: H-C2-wesley-2block (CONFIRMED)
- **Proof ledger**: FM-001.edn v63
- **Verification**: scripts/fm001/verify_witness.py
- **Commits**: futon3c 47b6508, futon6 f2209eb
