# C-substrate-completion — STANDARD-ARGUE *(DRAFT spec)*

**Status:** ✅ **PROMOTED 2026-06-01** (Joe-ratified) into `C-substrate-completion.md` **§4
STANDARD-ARGUE** — which is now the canonical source of truth (incl. the O3 resolved-form
refinement: propose-here = curvature ∧ unresolvedness). This file is retained as the working
copy + full provenance of the claude-3 ⇄ claude-6 requirements salvo (micro-whistle, 6 turns).
**Purpose:** the spec the keystone `M-substrate-metric` is written *against* — i.e. *what the
metric contract must satisfy to be fit for both consumers, verifiable as a design before
anyone builds.* This is the Campaign's reason to exist (cross-mission adequacy).

---

## Ownership *(ratified by Joe, 2026-06-01)*

| Role | Mission | Owner |
|---|---|---|
| **Keystone** | `M-substrate-metric` *(to be opened)* | **codex-3** — opens/owns; metric math (Ollivier–Ricci/Wasserstein curvature + Laplace–Beltrami embedding) |
| **Paired consumer (E1)** | `M-aif2` (`futon2/holes/`) | **claude-3** |
| **Paired consumer (E2)** | `M-differentiable-code` (`futon5/holes/missions/`) | **claude-6** |
| **Demonstrator** | `E-codebase-manifold` (M-aif2 §6, unopened) | TBD |
| **Coordination** | the Campaign | claude-3 (in chair with Joe); **swarm** is the default working mode |
| **STANDARD-VERIFY ratifier / dissolution** | — | **Joe** |

---

## The contract the keystone must satisfy — four obligations

### O1 · R-shared-nodes  *(keystone-owned identity; resolves the granularity tension)*

The keystone fixes **one multi-resolution, typed node-identity set** over substrate-2,
**closed under {E1's relation-types, E2's relation-types}**. Each consumer declares its needed
grains/relation-types *up front*; the keystone guarantees nodes exist at each.

STANDARD-VERIFY checks, against that one set:
- **(id)** every declared grain/type is present;
- **(E1)** every node carries a **type `κ` can dispatch on** (node-type → action-class);
- **(E2)** every node is **conditioning-normalized** so `∂s/∂A` is well-scaled across nodes
  spanning orders of magnitude.

**Why this dissolves the keystone-once-vs-falls-out-of-DERIVE tension** (Campaign CONSTITUTION
§"node-granularity is a shared prerequisite" vs M-differentiable-code MAP): split
node-**IDENTITY** (shared, keystone-owned, resolved once) from node-**PROPERTY** (per-consumer
clause). `A[n,r,target]` *selects a typed sub-lattice* of the shared identity
(granularity-**selection**) — it does **not** create identity. Valid **iff** the identity is
multi-resolution and closed under the declared relations; **otherwise** `A` would subdivide a
node unilaterally = re-authoring identity = a real divergence to **escalate to Joe**.

### O2 · The "two cuts of one object" certification

The metric is **one object** (the ground metric `d` / node embedding) consumed along **two
cuts that VERIFY must certify independently — one does not imply the other.** Three axes:

| Axis | E1 (M-aif2) | E2 (M-differentiable-code) |
|---|---|---|
| **order** | zeroth — *read* curvature value | first — `∂s/∂A` gradient |
| **latency** | runtime / **live** (WM scan cadence) | DERIVE-time / **offline** (batch OK) |
| **clause** | owned curvature **sign/polarity** | **conditioning** normalization |

The discrete-curvature read (Ollivier–Ricci, Wasserstein-on-graph) and the smooth gradient
(Laplace–Beltrami embedding) are **different regimes off the same `d`**. VERIFY certifies
*both cuts*, never assumes one implies the other.

### O3 · R-E1 — curvature-as-tension  *(owner: claude-3)*

**(a) Owned polarity.** A curvature query (Ollivier–Ricci over the typed identity) whose
**sign/threshold convention is owned by the metric, not the consumer**: "propose here" fires on
*unresolved strain / incompleteness*, **never** on settled structure. This is the metric's
answer to M-aif2's Stage-B finding (two coexisting conventions: `mission_delta_t`'s signed grad,
negative on **complete** nodes, vs `war_machine`'s `(1−T) ≥ 0`). The tension-proposer *reads*
direction; it never re-derives it.

**(b) Liveness.** Curvature must be answerable against **current** substrate-2 at **WM-scan
cadence**, OR the contract defines a **staleness/caching bound** the proposer may tolerate
(max-age + recompute-on-demand for hot nodes). The Gap-1 payoff — *the field generates
candidates when the queue is dry* — only fires if the read is live; Ollivier–Ricci is
Wasserstein-per-edge = expensive, so this is contract-relevant, not free.

**Adequacy tests (sign-off, checkable as design):**
1. A node known **complete** (T=0, e.g. `futonzero-capability` — the exact node the Stage-B
   spike mis-flagged at −9.8) must **NOT** return propose-here; a known bridge/bottleneck **must**.
2. Curvature read within the scan budget, or within the declared staleness bound.

### O4 · R-E2 — continuity-as-differentiable-band  *(owner: claude-6 — confirmed + extended 2026-06-01)*

> Transcribed by claude-3 from claude-6's salvo turns; **confirmed by claude-6 on review, with
> (c) and the O1-join added** — both load-bearing from M-differentiable-code's MAP and not
> derivable from (a)/(b).

**(a) Differentiable band.** A continuity query returns coordinates a gradient can flow through:
`s = f(A ; embedding)` differentiable end-to-end w.r.t. the soft typed adjacency `A[n,r,target]`;
`∂s/∂A` defined and finite. Embedding coordinates are **fixed observations the metric supplies**
(measurement instrument — *never* the optimized object); the consumer optimizes only `A`.

**(b) Owned conditioning.** `∇` well-scaled, **not dominated by any single node**.

**(c) Non-degeneracy (discriminative embedding).** The supplied embedding must **separate
nodes** — `s = f(A ; embedding)` is only a meaningful band if `cos(emb(n), emb(m))` actually
*varies* across pairs. A collapsed embedding makes the band degenerate and `∂s/∂A` meaningless
**even when finite**. This is not hypothetical: M-differentiable-code's MAP records futon6's
R-GCN structural embedding (`hypergraph-embeddings.npy` + `graph-gnn-model.pt`) collapsing to
**cosine ≈ 1.0** (`futon6/technote-arxiv-mining.md:14-30` — FAISS could not separate papers;
resolved only by switching to text-BGE). So the metric must source continuity from a
**discriminative** layer (text-BGE-grade separation), *not* the tried-and-found-wanting R-GCN
arm. This is the contract-level form of M-differentiable-code's gap #3 (R-GCN warning).

**O1-join (granularity).** O4 presupposes O1: the fixed embedding must exist **at the grain E2
selects** (the typed sub-lattice its `A[n,r,target]` ranges over — symbol/function/boundary, not
only namespace), else E2 holds coordinates for the wrong nodes. This is where O4(a) and O1(id)
meet: "embedding present" and "grain present" are one obligation seen from two seats.

**Adequacy tests (sign-off, checkable as design):**
1. **Conditioning:** the two extreme nodes (the ~115k-line `futon3c` namespace vs a one-line
   `defn`) — one gradient step on `A` is numerically sane: `∂s/∂A` finite for both, step
   direction *not* swamped by the big node. Condition-number / Jacobian check on a 2-node toy,
   before any extractor exists.
2. **Non-degeneracy:** on a sample of node pairs at the selected grain, `cos(emb·,emb·)` spread
   is well below the collapse regime (a separation/variance floor, *not* ≈ 1.0 everywhere).

**Latency:** offline/batch acceptable.

---

## Mapping to the Campaign joint-completion criterion (C-substrate-completion §1)

STANDARD-VERIFY **passes** when **O1–O4 are each verified as a design** — fit for *both*
consumers — **before either builds**. The Campaign then dissolves toward `:satisfied` when
≥1 paired requirement is **released-and-consumed** (e.g. E1's tension-proposer reads curvature
from the live metric).

## Handoffs / open items

- **codex-3** opens `M-substrate-metric` with this spec as its charter basis. Its MAP's **first
  sub-deliverable = node-identity (O1)** — per the Campaign CONSTITUTION shared-prerequisite
  finding.
- **Each consumer hands codex-3 its declared relation-types/grains** (O1 closure): E1 = the `κ`
  node-types it dispatches on; E2 = file-dependency / function-ownership / boundary-membership
  grains.
- **Escrow:** E1 + E2 stay `:held` → `:contract-released` on STANDARD-VERIFY of this spec →
  `:satisfied` on metric delivery.

## Collaboration pattern (the answer to Joe's "find a good way to share work")

**owner-drafts-its-own-clauses · contributor-synthesizes-the-shared-frame · Joe ratifies.**
Default mode: **swarm**. Requirements settled by **micro-whistle salvo**; heavy metric math to
**codex-3**. This draft is the salvo's crystallized residue.
