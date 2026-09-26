# S1 result — futon3c git-history topology

Corpus pin: `d722f772ede949719948aec76839d4d5e83586b0` (1,828 commits).
Signal: `git-history-topology`, structurally separate from APM agent-attribution
and WM algorithmic-selection evidence.  This lab emits no receipt-shaped data.

## Preregistered test

The graph has 1,828 commit vertices, 3,014 typed incidence hyperedges and
14,876 incidences.  Hyperedges by relation are: parent 1,827; same-file 646;
same-subsystem 99; co-change 271; temporal 97; references 74.  A many-commit
file is one hyperedge, never a clique.

The null consists of 200 bipartite configuration-model rewires.  Double-edge
swaps preserve every commit degree and every hyperedge degree exactly.  Each
null performs three successful swaps per incidence from the original graph.

| operator | real lambda2 | null mean (sd) | null 95% interval | result |
|---|---:|---:|---:|---|
| unnormalised `Dv - H De^-1 H^T` | 0.271811 | 1.173410 (0.198407) | [0.835495, 1.554879] | outside, below |
| Zhou degree-normalised | 0.035995 | 0.371910 (0.022643) | [0.324192, 0.409725] | outside, below |

This **disconfirms the preregistered expectation** that the real value would
fall inside the null interval.  Both operators detect wiring structure beyond
the incidence-degree sequences.  Under the preregistered decision rule, the
spectral criterion is potentially rehabilitable.  The direction matters: the
real graph is much less connected than its degree-preserving nulls; this is not
evidence that larger lambda2 is intrinsically better.

## Positive controls

- Co-change passed. `src/futon3c/transport/http.clj` (149 commits) and
  `test/futon3c/transport/http_test.clj` (52 commits) co-change 47 times,
  Jaccard 0.3052, and rank second among 271 qualifying pairs.
- Alias control passed. Both `Joe Corneli` and `Joseph Corneli` occur, while
  all six relation derivations ignore author identity. The test swaps those
  labels throughout the corpus and asserts that the derived edge set remains
  exactly equal.

## Determinism

The harness uses seed 20260731 and a fixed ARPACK start vector. Two complete,
independent 200-null reruns produced byte-identical artifacts:

- `s1-corpus.json`: `777e23768791d8c98bd8ae48102c4184098662ea9c6baa716fd5ccc032df732b`
- `s1-results.json`: `9636fcd7b786484c37a21c82a119bed7ca6d5f07ea243ef7075ff68aae8f15e0`

Runtime versions are Python 3.12.3, NumPy 1.26.4 and SciPy 1.11.4, also
recorded in the results artifact.

## Scope

This is one heavily single-author software project. It tests a topology
operator on code history; it does not show that memory improves proof work or
solver performance. S2 fix-to-cause retrieval was not built.
