# Mission: M-substrate-metric

**Date:** 2026-06-01
**Status:** DELIVERED (v1) - with named residue. O1-O4 design-fit accepted by Joe; E1 + E2 escrow entries are `:contract-released`; E1 curvature query delivered; E2 continuity cut passed O4(b)/O4(c). Campaign remains open until a paired consumer consumes live.
**Campaign:** `futon3c/holes/campaigns/C-substrate-completion.md`
**Charter spec:** `futon3c/holes/campaigns/C-substrate-completion.STANDARD-ARGUE.draft.md`
**Owner:** codex-3, ratified by Joe via claude-3 handoff, 2026-06-01.

---

## HEAD

### Operator shape

`C-substrate-completion` needs a keystone Mission that defines the shared substrate-2 metric contract before paired consumers build against it. The Campaign's two live consumers need one ground metric `d` over one shared, multi-resolution typed node-identity set, with two separately-certified cuts:

- **E1 / M-aif2:** Ollivier-Ricci curvature over current substrate-2 at WM-scan cadence, with metric-owned polarity for "propose here."
- **E2 / M-differentiable-code:** a Laplace-Beltrami / continuous embedding band that supports offline differentiation through `A[n,r,target]`.

This Mission owns the metric contract and its first precondition: node identity. It does not let either consumer unilaterally create its own node lattice.

### The question

What ground metric contract over substrate-2 is sufficient for both curvature-as-tension and differentiable-code continuity, and what node-identity schema must be fixed first so those two cuts are readings of one object rather than two incompatible shadows?

### Anti-glibness discipline

- Do not define `d` before O1 node identity is fixed.
- Do not collapse E1 curvature substrate relations into E2 `A[n,r,target]` optimization candidates. A relation may feed both, but only by explicit schema flag.
- Do not treat graph curvature alone as the metric-owned "propose here" sign. O3 requires complete nodes not to fire.
- Do not claim STANDARD-VERIFY has passed until O1-O4 are checkable as a design against both consumers.

---

## 1. IDENTIFY

### Invariance claim

The shared standard is not "some embedding" or "some curvature." It is a metric contract over stable substrate-2 identity. If node identity forks by consumer, then curvature, continuity, and future manifold reconstruction are no longer derived objects of one metric. The Campaign exists precisely to prevent that fork.

### First sub-deliverable: O1 node identity

O1 fixes one multi-resolution, typed node-identity set, closed under both consumers' declared grains and relation types. Node identity is keystone-owned; consumer-specific clauses may add properties, flags, or typed relation uses, but they do not create new identity after the fact.

Schema distinction to preserve:

| Relation use | Meaning |
|---|---|
| `feeds-mu?` | Relation participates in the Ollivier-Ricci neighborhood measure `mu_x` for E1 curvature reads. |
| `feeds-A?` | Relation is a typed adjacency candidate in `A[n,r,target]` for E2 gradient optimization. |

A relation can have both flags. Neither flag is inferred from the other.

### E1 O1 closure set received

Source: claude-3 whistle, 2026-06-01, for `M-aif2`.

E1 node grains that `kappa` dispatches on:

| Node type | E1 action class |
|---|---|
| `:sorry` | `:address-sorry` |
| `:mission` | `:open-mission` |
| `:pattern` | `:fire-pattern` |
| `:file` | context-routed to `:open-mission` / `:address-sorry` through file-to-mission tension |

E1 curvature-substrate relation families, all `feeds-mu? = true`:

| Relation type | Shape |
|---|---|
| `related-mission` | mission <-> mission |
| `mission-cross-ref` | mission-doc <-> mission-doc |
| `file->mission` | file <-> mission |
| `sorry->related-missions` | sorry <-> mission |

E1 does not optimize `A`; these relations are not automatically `feeds-A?`.

### Metric-owned polarity answer for E1

Completeness / resolution state is a node feature owned by the metric contract, with per-grain providers or rollups. Curvature may identify bridge or bottleneck structure, but "propose here" requires unresolvedness as well. A complete `:mission` node can remain geometrically interesting while still not firing as actionable tension. For node grains without native `T`, the metric must define a provider, rollup, or explicit unknown/non-actionable state; the consumer must not invent polarity.

### E2 O1 closure set received

Source: claude-6 via claude-3 whistle, 2026-06-01, for `M-differentiable-code`.

E2 node grains that must exist so `A[n,r,target]` selects rather than re-authors identity:

| Node type | E2 use |
|---|---|
| `:file` | file-level dependency and boundary membership |
| `:namespace` | namespace-level requires/provides and symbol ownership |
| `:symbol` | function / `defn`-level optimization target |
| `:boundary` | responsibility / cluster membership target |
| `:claim` | wiring-contract or responsibility claim linked to code |

E2 typed-adjacency relation families, all `feeds-A? = true`:

| Relation type | Shape |
|---|---|
| `file-dependency` | `:requires` / `:provides` between `:file` / `:namespace` |
| `function-ownership` | `:defines`, `:symbol -> :namespace` |
| `boundary-membership` | `:symbol` / `:file -> :boundary` |
| `claim-to-code` | `:claims` / `:supports` / `:violates` between `:claim` and code nodes |

E2 extraction constraints:

- **Canonical trees only:** exclude worktrees, origin checkouts, and `.state`; target the living-stack population rather than stale sandbox duplicates.
- **Non-degenerate continuity layer:** do not use the collapsed R-GCN arm as the continuity embedding. `futon6/technote-arxiv-mining.md` records pairwise cosines clustering near 1.0 for the R-GCN graph embeddings, while BGE text embeddings retained discriminative spread.

**IDENTIFY exit:** both consumers' O1 closure sets are now recorded. The blocker is lifted. O1 can move to MAP schema proposal.

---

## 2. MAP - O1 node-identity schema proposal

### 2.1 Node type set

The initial shared node-identity set is the union of E1 and E2 grains:

| Node type | Required by | Notes |
|---|---|---|
| `:sorry` | E1 | Must carry resolution/completeness feature and action class `:address-sorry`. |
| `:mission` | E1 | Must carry resolution/completeness feature; complete missions must not fire propose-here. |
| `:pattern` | E1 | Must carry resolution/completeness feature or explicit unknown/non-actionable state. |
| `:file` | E1 + E2 | Shared grain; may participate in curvature substrate and E2 adjacency selection. |
| `:namespace` | E2 | Namespace-level code node; needed for dependency and ownership relations. |
| `:symbol` | E2 | Function / `defn`-level code node; needed for normalized gradients. |
| `:boundary` | E2 | Responsibility / cluster node selected by boundary membership. |
| `:claim` | E2 | Contract / responsibility claim node linked to code. |

Identity is multi-resolution: code may appear as `:file`, `:namespace`, and `:symbol`, but those are distinct typed nodes joined by explicit relations. Selection over a lower grain never creates identity; it selects an existing node.

### 2.2 Relation-use table

| Relation type | Shape | `feeds-mu?` | `feeds-A?` | Owner / use |
|---|---|---:|---:|---|
| `related-mission` | mission <-> mission | true | false | E1 curvature substrate |
| `mission-cross-ref` | mission-doc <-> mission-doc | true | false | E1 curvature substrate |
| `file->mission` | file <-> mission | true | false | E1 curvature substrate; shared `:file` grain with E2 |
| `sorry->related-missions` | sorry <-> mission | true | false | E1 curvature substrate |
| `file-dependency` | `:requires` / `:provides` between `:file` / `:namespace` | false | true | E2 typed adjacency |
| `function-ownership` | `:defines`, `:symbol -> :namespace` | false | true | E2 typed adjacency |
| `boundary-membership` | `:symbol` / `:file -> :boundary` | false | true | E2 typed adjacency |
| `claim-to-code` | `:claims` / `:supports` / `:violates` between `:claim` and code nodes | false | true | E2 typed adjacency |

Current schema deliberately records relation-use flags separately. The shared overlap currently proven by both consumers is at the `:file` grain, not yet at a same-named relation. If a later extracted relation instance is declared by both consumers, it becomes `feeds-mu? = true` and `feeds-A? = true`; dual-use is explicit, never inferred from shared endpoints.

### 2.3 Required node properties

Every node carries:

| Property | Meaning |
|---|---|
| `:node/id` | Stable identity, including type and canonical source locator. |
| `:node/type` | One of the O1 node types above. |
| `:canonical-source` | Source path / locator after excluding worktrees, origin checkouts, and `.state`. |
| `:resolution-state` | Metric-owned completeness / unresolvedness feature used by E1 polarity; ratified Campaign §4 O3 contract obligation, not optional metadata. |
| `:conditioning-scale` | Metric-owned audit / optional normalization metadata for E2. Not applied by default in v1; use only if future relations show degree-driven gradient swamping. |

### 2.4 Selection rule

`A[n,r,target]` may select only triples whose `n`, `r`, and `target` already exist in the O1 identity/relation table and where `r.feeds-A? = true`. A missing grain or relation is not silently subdivided by E2; it is an O1 schema escalation.

`mu_x` for Ollivier-Ricci curvature may use only incident relations where `feeds-mu? = true`. E1 reads curvature and polarity; it does not optimize adjacency.

### 2.5 MAP exit check

- E1 closure set represented: yes.
- E2 closure set represented: yes.
- Shared `:file` grain represented once: yes.
- Relation-use split represented: yes.
- E1 same-named dual-use check: **confirmed clear** by claude-3, 2026-06-01. E1's four relations remain `feeds-mu? = true / feeds-A? = false`; overlap with E2 is the shared `:file` grain only, not a same-named relation.
- E2 same-named dual-use check: **confirmed clear** by claude-6 via claude-3, 2026-06-01. E2's four relations select over existing grains; no same-named relation currently needs both flags.

**MAP exit:** closed for O1. Node identity, relation-use flags, required node properties, and select-not-subdivide rules are ready to feed DERIVE.

### 2.6 Contract promotion note

Joe ratified Campaign STANDARD-ARGUE on 2026-06-01, and `C-substrate-completion.md` §4 O3 now adopts this Mission's polarity resolution: `propose-here = geometric strain (curvature) AND unresolvedness`, with `:resolution-state` as a metric-owned node feature. This Mission therefore owns `:resolution-state` as part of the O1/O3 contract surface.

### 2.7 IFR and O4 baseline decision

Accepted keystone IFR, 2026-06-01:

> One ground metric `d` that is a pure function of already-present substrate structure plus already-discriminative text - live for E1, differentiable offline for E2 - with no model training and no identity changes. The only new object is `d` itself.

Owner decision: accept **JAX** as the O4 differentiable-band substrate and accept **`:symbol` + bounded code-text observations + per-source softmax normalization** as the baseline pilot shape. `:conditioning-scale` is retained as metric-owned audit metadata, not applied by default. This is IFR-aligned because futon5 already has a JAX autodiff precedent (`tools/tpg/jax_refine.py`), and the Campaign §4 O4 contract already requires a differentiable offline band, fixed embedding observations, conditioning, and non-degenerate text-grade discrimination.

Fallbacks are costed departures, not defaults:

| Option | Status | Justification threshold |
|---|---|---|
| `:symbol` + bounded BGE/code-text observations + per-source softmax | **baseline** | Already ratified grain; no identity change; no model training; O4(b) confirmed well-conditioned without default degree weighting. |
| `:scope` overlay | fallback | Only if `:symbol` fails O4(b) conditioning or O4(c) discrimination. |
| spec / claim overlay beyond existing `:claim` nodes | fallback | Only if existing `:claim` grain cannot express the loss bands. |
| refactor / new grain | last resort | Only if extraction cannot produce stable, conditioned existing grains. |
| trained embedding / R-GCN arm | rejected for baseline | Violates IFR and conflicts with O4(c) non-degeneracy warning unless separately justified. |

Pilot instruction to E2 / claude-6: test whether `:symbol` identity with bounded code-text observations and per-source softmax gives finite, sane `∂s/∂A` and non-degenerate text-observation separation before proposing any new grain. `:conditioning-scale` may be tested as an optional audit/weighting knob, but is not part of the default v1 path unless evidence shows degree-driven swamping.

### 2.8 O4 Pilot #1 relay

E2 Pilot #1 result, relayed by claude-3 from claude-6, 2026-06-01: **`:symbol` + namespace-context is settled as the baseline embedding shape; no `:scope` escalation.**

- **O4(c) discrimination:** PASS on BGE contextualized symbol windows. `M-differentiable-code` records off-diagonal cosine median about `0.51-0.56`, p99 about `0.71`, and fraction `> 0.95` approximately zero - the inverse of the R-GCN collapse.
- **O4(b) conditioning:** PASS. claude-6's final on-box run found `dS/dA` finite and well-scaled: max/median about `4x`, not the feared `10^4-10^5x`. Sparse `feeds-A?` structure held (`7084` ownership edges + `852` file-dependency edges, never dense). Degree-based `:conditioning-scale` was not load-bearing and slightly worsened max/median (`4.76` vs `3.95`), so it is audit/optional in v1.

### 2.9 O4 operational memory gate

O4 pilots must not run full living-stack JAX / BGE jobs on the serving futon3c box. This is a contract guard, not merely an implementation preference.

Evidence: `futon6/holes/missions/M-differentiable-math.md` records a 2026-05-31 BGE-grounded curvature recompute that OOM'd the machine by reading a 2.3G `entities.json` whole, holding an 805k qid-to-row dictionary, mmap'ing a 3.1G embedding array, and pressuring the serving futon3c JVM into swap. Joe's decision there was to back off and require streaming / slicing / non-serving-machine execution.

Rules for this Mission:

- Full living-stack O4 runs belong off the serving box, ideally on the superpod.
- Local pilots must be hard memory-capped or limited to a bounded slice sufficient to check gradient finiteness and conditioning; a few hundred symbols is enough for the first check.
- Do not read large JSON arrays whole; stream or precompute small sidecars.
- Do not materialize full embedding arrays when mmap row-slices or bounded samples suffice.
- Any STANDARD-VERIFY witness for O4 must report its memory posture: host, slice size, peak memory if known, and whether the futon3c JVM was co-resident.

### 2.10 `:conditioning-scale` provider contract

`:conditioning-scale` is metric-owned, like `:resolution-state`. E2 may consume it, but the consumer does not invent it.

Keystone O4(b) ruling, 2026-06-01: `:conditioning-scale` is **audit / optional metadata in v1, not default-applied weighting**.

Evidence: claude-6's O4(b) run found gradients finite and well-scaled without default degree weighting. The real spread was about `4x` max/median; the feared `10^4-10^5x` spread did not manifest. Conditioning came from bounded BGE/code-text windows and per-source softmax normalization, not from degree weighting. Applying `1/sqrt(total-A-degree)` slightly worsened max/median (`4.76` vs `3.95`), so the v1 default is no degree weighting.

The current provider remains **degree-aware, not raw-size-aware**, for audit and future fallback:

| Field | Meaning |
|---|---|
| `:conditioning-scale/source` | `:metric/o1-degree-provider-v0` or explicit `:unknown`. |
| `:conditioning-scale/a-degree` | Count of candidate `feeds-A?` targets in which the node participates, by relation and total. |
| `:conditioning-scale/loss-weight` | Optional fallback weight, `1 / sqrt(max(1, total-a-degree))`; not applied by default in v1. |
| `:conditioning-scale/raw` | Preserved source counts: relation degree, optional line count, symbol text-window length. |

Contract:

- O4 v1 losses use bounded code-text observations and per-source softmax normalization by default.
- `:conditioning-scale/loss-weight` is applied only if a future relation family demonstrates degree-driven swamping; no such swamping appeared in O4(b).
- Raw line count is recorded for audit but is not the v1 conditioning axis; bounded symbol windows keep source text size out of `dS/dA`.
- If `total-a-degree` is unknown, the audit field is incomplete, but this does not block v1 O4 execution unless a future run chooses to apply degree weighting.

### 2.11 E2 STANDARD-VERIFY design verdict

Keystone verdict for E2, 2026-06-01: **buildable spec, not yet fully witnessed**. The design accounts for E2's O4 obligations as follows:

- **O4(a) differentiable band:** JAX remains the differentiable substrate for the offline Laplace-Beltrami / `A[n,r,target]` band. The O1 identity set and `feeds-A?` relation table are sufficient for `A` to select over existing nodes and relation types.
- **O4(c) non-degeneracy:** the E2 continuity layer must be a direct code-text embedding at the existing `:symbol` / `:namespace` grains, using bounded namespace-context symbol windows. The embedding cache is build-work under this keystone contract because it is not currently on disk: futon6 BGE covers papers, futon3 embeddings cover missions / patterns, and the R-GCN arm is explicitly rejected because it collapses to cosine near 1.0. This does not reopen O1: the source text and node identities already exist; the owed artifact is the fixed observation layer over those identities, not a new grain or trained graph embedding.
- **O4(b) conditioning:** PASS in final E2 run. `dS/dA` was finite and well-scaled with bounded code-text observations and per-source softmax; degree weighting is audit/optional and not applied by default in v1.
- **O1 select-not-subdivide:** confirmed PASS by E2 for all four relation families: `file-dependency`, `function-ownership`, `boundary-membership`, and `claim-to-code`.

SV / DELIVER posture: E2 obligations are satisfied for v1. The direct code embedding is non-degenerate, and O4(b) conditioning passed on-box under sparse `feeds-A?` structure. Broader future runs still obey the §2.9 memory gate.

---

## 3. DERIVE - E1 curvature cut

E1 is the buildable-now cut of the metric: it uses already-ratified O1 grains (`:mission`, `:sorry`, `:pattern`, `:file`) and existing / already-discriminative observations. DERIVE starts by pinning the contract surface that O3 requires before choosing `mu_x` weights or the curvature-cut distance `d`.

### 3.1 `:resolution-state` provider contract

Convention: `:resolvedness` is a scalar in `[0,1]`, where `0.0` means wide-open / maximally actionable and `1.0` means complete / non-actionable. This is deliberately the inverse of the older `T(sorry)` proof-of-concept convention; this field is not the thin scalar `T`, it is the metric-owned O3 polarity feature.

The boolean O3 gate only uses whether resolvedness is numeric and `< 1.0`. The intermediate values below are therefore not extra firing thresholds; they are for candidate ranking after the gate fires:

```
action-intensity(node) =
  max(0, -1 * :curvature/min-incident-kappa)
  * (1 - :resolution-state/resolvedness)
```

If a consumer does not use `action-intensity`, STANDARD-VERIFY should treat only the binary actionable / non-actionable distinction as normative and the intermediate scalar values as provisional.

Every E1 node gets:

| Field | Meaning |
|---|---|
| `:resolution-state/source` | Which provider supplied the value, or `:none`. |
| `:resolution-state/resolvedness` | Number in `[0,1]`, or `:unknown` when the grain has no native provider. |
| `:resolution-state/actionable?` | Whether O3 may allow propose-here after curvature also indicates strain. |
| `:resolution-state/raw` | Source phase/status/state preserved without lossy rewriting. |

Provider table:

| Grain | Provider / rollup | Resolvedness rule | Actionability |
|---|---|---|---|
| `:mission` | Mission lifecycle phase/status. | `HEAD` / `IDENTIFY` / `MAP` / `DERIVE` -> `0.10`; `ARGUE` / `VERIFY` -> `0.35`; `INSTANTIATE` / `DOCUMENT` -> `0.65`; `COMPLETE` / `CLOSED` / `DISSOLVED` -> `1.00`; blocked/stalled keeps its phase value but raw status is preserved. | Actionable iff resolvedness `< 1.0` and curvature indicates strain. |
| `:sorry` | Sorry registry status, preserving raw status. | `:open` / `:reopened` -> `0.00`; `:addressed` -> `0.65`; `:closed` / `:foreclosed` / `:falsified` / `:n-a-by-design` / `:acknowledged-v1-in-force` -> `1.00`; unknown statuses -> `:unknown`. | Actionable iff numeric resolvedness `< 1.0` and curvature indicates strain. |
| `:pattern` | Pattern lifecycle / validation state where present. | validated / applied / in-force -> `1.00`; candidate / unvalidated / proposed -> `0.20`; no native state -> `:unknown`. | Unknown is non-actionable until a provider exists; low resolvedness can be actionable with curvature. |
| `:file` | No native resolution-state provider. | `:unknown` with `:resolution-state/source :none`. | Direct file actionability is false. File tension routes through `file->mission` in `mu_x`; each incident mission keeps its own resolution-state. A many-mission file produces per-mission routed actionability, not one file scalar. |

O3 firing rule:

```
propose-here?(node) =
  curvature-strain?(node)
  AND numeric(:resolution-state/resolvedness)
  AND (:resolution-state/resolvedness < 1.0)
  AND :resolution-state/actionable?
```

This satisfies the Stage-B guard: a complete mission such as `futonzero-capability` can be geometrically central or bridge-like, but it cannot fire propose-here because its resolvedness is `1.0`.

### 3.2 `mu_x` neighborhood measure

Accepted E1 principle: `mu_x` is **structural, not resolution-weighted**. Curvature and resolvedness stay separate conjuncts in O3. Weighting `mu_x` by `:resolution-state` would bake actionability into the geometry and undo the reason O3 splits "strain" from "unresolvedness."

For v0, use a lazy random-walk measure over the `feeds-mu?` relation subgraph:

```
mu_x = alpha * delta_x
       + (1 - alpha) * uniform(incident-feeds-mu-neighbor-endpoints(x))

alpha = 0.5
```

Rules:

- `incident-feeds-mu-neighbor-endpoints(x)` is the multiset of opposite endpoints from incident relations where `feeds-mu? = true`.
- If multiple incident `feeds-mu?` edges point to the same neighbor, their mass sums at that neighbor. Uniformity is over incident structural edges first, then collected as a measure over nodes.
- If `x` has no incident `feeds-mu?` edges, `mu_x = delta_x`.
- `:resolution-state` never enters `mu_x`.
- Edge-type weights are deferred. `related-mission`, `mission-cross-ref`, `file->mission`, and `sorry->related-missions` have equal structural status in v0 until evidence justifies tuning.

This is IFR-aligned: no training, no new identity, no fitted edge-type knobs. It also matches the successful structural precedent from `futon6/resources/differentiable-math/ricci-tag-curvature.json`, which used a structural graph with `alpha = 0.5` and produced meaningful positive/negative curvature ranges.

### 3.3 Node-level `curvature-strain?` rollup

Ollivier-Ricci curvature is edge-defined, but O3's `propose-here?` predicate is node-level. The node rollup is part of the E1 curvature contract:

For each node `x`, compute Ollivier-Ricci curvature `kappa(e)` for every incident `feeds-mu?` edge `e`.

Expose:

| Field | Rule |
|---|---|
| `:curvature/min-incident-kappa` | Minimum incident `kappa(e)`; primary strain read. |
| `:curvature/mean-incident-kappa` | Mean incident `kappa(e)`; audit / smoothing read, not the v0 trigger. |
| `:curvature/strain-edge` | Incident edge attaining the minimum, for traceability. |
| `:curvature/strain?` | `true` iff `min-incident-kappa < 0.0`; false for isolated nodes or all non-negative incident curvature. |

The v0 trigger uses the minimum because E1 wants bridge/bottleneck detection: one sharply negative bridge should survive averaging. Keep `min-incident-kappa` as a magnitude, not only a boolean, so INSTANTIATE can rank strained nodes by intensity if the `< 0.0` threshold is too broad. Later evidence may add quantile or weighted rollups, but v0 keeps the bridge signal legible.

### 3.4 E1 curvature-cut `d`

Decision: **hop-distance is the E1 baseline `d` for the curvature cut.** BGE / embedding distance becomes an O2 convergence check later, not the baseline.

Reason:

- E1's `feeds-mu?` support includes `:file` via `file->mission`.
- Mission and pattern embeddings exist; a file/code embedding is not available without inheriting E2's still-gated code-embedding work.
- Therefore a BGE-baseline `d` would make the E1 cut depend on E2's blocker, defeating the buildable-now reason for doing the curvature cut first.
- Hop-distance covers every E1 grain immediately, requires no training and no new identity, and matches the structural precedent from `futon6/resources/differentiable-math/ricci-tag-curvature.json`, whose structural graph produced both negative bridge-like curvature and positive clustered curvature.

V0 definition:

```
d_E1(x,y) = shortest-path length between x and y
            in the undirected feeds-mu? structural graph
```

Rules:

- The graph uses the same `feeds-mu?` relations as `mu_x`.
- Distance is symmetric and unweighted in v0.
- Disconnected pairs are outside the finite transport component; compute curvature per connected component or define an explicit large sentinel only if an implementation requires a total matrix.
- Edge-type weights remain deferred with the `mu_x` edge-type weighting question.

### 3.5 O2 reconciliation

This does not silently violate O2's "one object" discipline. The keystone contract now treats the shared object as **shared identity plus certified cut-specific realizations**, with an explicit convergence obligation:

- E1 baseline realization: structural hop-distance over the shared `feeds-mu?` graph, because it is buildable now and covers `:file`.
- E2 baseline realization: BGE/contextualized-symbol observation plus JAX differentiable band, because that is the O4 substrate.
- O2 convergence check: once E2's code embedding is available at the relevant grains, compare hop-curvature and embedding-curvature on overlapping E1 grains. Agreement supports the claim that both cuts are reading one underlying object; disagreement is a STANDARD-VERIFY finding, not something hidden inside either consumer.

This is acceptable at DERIVE because Campaign §4 O2 already says the cuts are certified independently and one does not imply the other. The reconciliation is made explicit here so STANDARD-VERIFY can test it.

### 3.6 E1 DERIVE surface

The E1 curvature-cut DERIVE surface now has its four required contract pieces:

- `:resolution-state` provider and O3 firing rule.
- structural `mu_x`.
- node-level `curvature-strain?` rollup.
- hop-distance baseline `d_E1`, with BGE/embedding convergence check deferred to O2.

This is ready for E1 review as the curvature-cut contract before implementation.

### 3.7 E1 review signoff and bounded claim

E1 review verdict, claude-3, 2026-06-01: **signed off**. The tension-proposer reads the E1 contract as `(min-incident-kappa, resolvedness, actionable?)`; the `kappa` node-type to action-class map remains on `M-aif2`'s side.

Bounded claim for STANDARD-VERIFY: this curvature cut is a high-leverage specialist. It fires on unresolved work at structural bridges / bottlenecks. It does **not** claim to cover every unresolved node or universally retire empty-queue symptoms; flat-but-unresolved nodes belong to other proposers. This is a consumer-side consequence, not a keystone defect.

---

## 4. VERIFY staging - E1 curvature cut

Historical staging note: this section records the pre-SV E1 VERIFY plan. At the time, the E1 cut could be verified without claiming full Campaign STANDARD-VERIFY, while O4's full conditioning extreme remained gated. That gate has since passed for v1; see §12 for the terminal closure state.

E1-cut VERIFY should check the contract as a design before implementation:

| Check | Witness shape |
|---|---|
| `:resolution-state` provider | Fixture table covering one mission per lifecycle bucket, one open/addressed/closed sorry, one pattern with state, one pattern without state, and one file. |
| `mu_x` | Toy graph over all four `feeds-mu?` relation families; verify alpha mass at self, uniform edge mass, summed mass for repeated neighbors, and delta measure for isolated nodes. |
| `curvature-strain?` | Synthetic incident-kappa table; verify min, mean, strain-edge, boolean threshold, and magnitude retained for ranking. |
| `d_E1` | Toy structural graph with connected and disconnected components; verify unweighted symmetric hop-distance and component handling. |
| O3 composition | Complete-but-bridge mission does not fire; open sorry on negative bridge fires; open flat sorry does not fire via this specialist cut. |

This VERIFY staging proceeded independently of the O4 ruling because it did not release Campaign escrow by itself. Campaign `:contract-released` still required O1-O4 STANDARD-VERIFY fit-for-all, which later passed.

### 4.1 E1-cut VERIFY result

Implemented as `futon3c.logic.substrate-metric-e1-invariants`, following the house `build-db -> query-violations -> run-verify` idiom over `clojure.core.logic` + `pldb`.

Witness result, 2026-06-01:

- conforming witness: clean, zero violations;
- adversarial traces: 6 / 6 caught by their own invariant;
- `run-verify`: `:verified? true`;
- focused test namespace: `2` tests, `16` assertions, `0` failures, `0` errors.

Invariants witnessed:

| Invariant | Adversarial caught |
|---|---|
| Stage-B guard | complete node on negative bridge cannot propose |
| Conjunct independence | curvature signature changes under resolution permutation |
| Measure validity | `mu_x` sum not equal to 1 |
| Strain soundness | negative incident edge without node strain |
| No-bypass composition | proposal without strain |
| Unknown safety | unknown resolvedness proposed |

This was an E1-cut VERIFY witness only. By itself it did not release Campaign escrow, because full Campaign STANDARD-VERIFY still required O1-O4 fit-for-all. Campaign STANDARD-VERIFY and the O4(b) v1 conditioning proof have since landed; see §7 and §12.

### 4.2 E1 VERIFY review signoff

E1 review verdict, claude-3, 2026-06-01: **signed off after independent code review**. E1 read `futon3c.logic.substrate-metric-e1-invariants` directly and confirmed:

- `q-stage-b-guard` faithfully models the founding Stage-B bug: `:complete-bridge` has `resolvedness = 1.0`, sharp negative bridge curvature, `strain? true`, and still does not propose; the adversarial flip to `propose? true` is caught.
- all 6 invariants are implemented and adversarially caught by their own category;
- scope is honest: this is a design/consistency model over asserted labels, with `propose?` derivation deferred to implementation, matching the M-aif2 Stage-A/Stage-B VERIFY style.

E1 surface state: **complete for the curvature cut** — O1 closed, E1 DERIVE signed, E1-cut VERIFY green and E1-confirmed. Later RUN/DELIVER work carried this through R1/R2 and full OR curvature; see §8-§12.

---

## 5. Keystone STANDARD-VERIFY attestation

Keystone-owner attestation, 2026-06-01, for Campaign STANDARD-VERIFY:

**Verdict:** the `M-substrate-metric` design satisfies O1-O4 as a design, fit for both current consumers, subject to Joe ratification. No obligation is known to be design-inadequate.

Matrix:

| Obligation | Keystone design-fit verdict |
|---|---|
| O1 shared identity | **PASS as design.** One multi-resolution typed identity set is fixed, closed under E1 and E2 declared grains and relation families. `feeds-mu?` and `feeds-A?` are explicit relation-use flags; `A[n,r,target]` selects existing nodes/relations and does not subdivide identity. |
| O2 one object | **PASS as design.** The contract uses shared identity plus independently certified cut-specific realizations: E1 structural hop-distance for live curvature and E2 direct code-text embedding + JAX band for offline gradients. The O2 convergence check is explicit, not hidden. |
| O3 E1 curvature cut | **PASS as design and locally verified.** `:resolution-state`, structural `mu_x`, node-level strain rollup, and `d_E1` are specified; the E1 invariant witness is green and E1 independently signed off. |
| O4 E2 continuity cut | **PASS as design and delivered for v1, proof owned by E2.** JAX is the differentiable substrate; the required continuity layer is a fixed direct code-text embedding at existing `:symbol` / `:namespace` grains; non-degenerate BGE-grade spread is required and has landed; `:conditioning-scale` is metric-owned audit / optional metadata, not default-applied weighting. |

Residue classification:

- **O4(b) full conditioning extreme** is resolved for v1: the final E2 run found finite, well-scaled gradients and sparse `feeds-A?` structure. It is no longer an open RUN/DELIVER gate.
- **O2-third Fisher-Rao latent** remains optional future residue, not a current design or v1 delivery blocker. The Campaign names Fisher-Rao as a latent third bridge; current delivery covers the two active consumers and records convergence checks rather than requiring the latent leg before delivery.
- **Direct code-embedding cache** has landed for the Clojure v1 path as a fixed observation layer over existing `:symbol` / `:namespace` identities. Elisp/Python extractor expansion is follow-on residue, not a new identity grain.

Attestation answer to the SV convening questions:

- (a) **Yes:** the keystone design satisfies O1-O4 design-fit for both consumers.
- (b) **Yes at SV time:** O4(b) full conditioning extreme and the O2-third Fisher-Rao latent were RUN/DELIVER gates, not STANDARD-VERIFY design blockers. O4(b) has since passed for v1; remaining residue is named in §12.
- (c) **No:** I do not see an obligation where the design is currently inadequate enough that STANDARD-VERIFY should wait.

---

## 6. INSTANTIATE - E1 runtime core

Bounded implementation increment, 2026-06-01: `futon3c.metric.e1` landed as the small pure runtime core for the E1 curvature cut.

Scope:

- normalize and select E1 `feeds-mu?` edge families;
- build the undirected multigraph used by `mu_x` and `d_E1`;
- compute the structural lazy random-walk measure, including isolated-node `delta_x` and repeated-edge mass collection;
- compute unweighted hop-distance on the `feeds-mu?` graph;
- roll edge-level `kappa` values into node-level E1 fields: min, mean, strain edge, and `strain?`;
- compose the O3 `propose-here?` gate and `action-intensity` ranking scalar.

Non-scope: this namespace does not compute Ollivier-Ricci curvature / Wasserstein transport. It prepares the contract-fixed graph, measure, distance, and polarity surfaces that the curvature solver consumes.

Witness:

- `clojure -M:test -n futon3c.metric.e1-test`
- result: `5` tests, `26` assertions, `0` failures, `0` errors.
- regression with E1 VERIFY model: `clojure -M:test -n futon3c.metric.e1-test -n futon3c.logic.substrate-metric-e1-invariants-test` -> `7` tests, `42` assertions, `0` failures, `0` errors.

---

## 7. STANDARD-VERIFY ratification

Joe ratified Campaign STANDARD-VERIFY on 2026-06-01, relayed by claude-3 as coordination owner.

Ratified state:

- O1-O4 design-fit accepted.
- Escrow E1 and E2 transition from `:held` to `:contract-released`.
- Keystone and consumers may build to the verified spec.
- Subsequent RUN/DELIVER work completed the E1 curvature query and the E2 v1 continuity / conditioning proof points recorded in §8-§12.

Operational consequence for this Mission: STANDARD-VERIFY released the keystone to build to spec. The v1 build artifacts and remaining residue are recorded in the closure record (§12).

Mission lifecycle status assignment after closure: **DELIVERED (v1)**. Earlier `INSTANTIATE` was the correct post-SV build-to-spec status; O4(b) has since passed and the terminal v1 state is now recorded in §12, with named residue and without Campaign dissolution.

---

## 8. RUN/DELIVER R1 - E1 substrate graph extraction

R1 artifact: `holes/missions/M-substrate-metric.R1-report.md`.

Implementation:

- `futon3c.metric.e1-report` fetches only E1 `feeds-mu?` relation families by hyperedge type.
- It builds the `futon3c.metric.e1` graph and reports node counts, edge counts, component summaries, and structural bridge candidates.
- It does not read the full entity corpus, build embeddings, run JAX, or compute Ollivier-Ricci curvature.

Live bounded run, 2026-06-01:

- per-type cap: `2000`;
- fetched relation counts: `related-mission=23`, `mission-cross-ref=654`, `file→mission=1014`, ASCII `file->mission=0`, `sorry->related-missions=0`;
- E1 graph: `922` nodes, `1691` structural edges, `5` connected components;
- node grains: `719` files, `187` missions, `16` sorries;
- bridge candidates: `576`, top candidate split score `2715`.

R1 state: **complete as graph extraction/report**. Next E1 runtime step is bounded hop-distance Ollivier-Ricci over this graph, sampled/capped first with timing reported before scaling.

---

## 9. RUN/DELIVER R2 - sampled OR curvature + polarity

R2 artifacts:

- `scripts/substrate_metric_e1_curvature.py`
- `holes/missions/M-substrate-metric.R2-curvature-sample.json`
- `holes/missions/M-substrate-metric.R2-curvature-report.md`

Implementation:

- fetch only E1 `feeds-mu?` relation families plus bounded mission/sorry docs for `:resolution-state`;
- exclude mission-adjacent report/sample artifacts from O1 mission nodes;
- compute lazy-walk Ollivier-Ricci curvature on a capped edge sample using exact sparse transport via `scipy.optimize.linprog`;
- apply metric-owned `:resolution-state` and actionability to produce the post-filter propose surface.

Sample run, 2026-06-01:

- graph: `922` nodes, `1691` structural edges, `5` components, largest `908`;
- bridge candidates: `576`;
- OR curvature sample: top `200` bridge candidates;
- timing: `2.079s` total, `1.279ms` mean per edge, `9.377ms` max per edge;
- kappa distribution: min `-0.5556`, p10 `0.0164`, median `0.0417`, p90 `0.0909`, max `0.5000`;
- post-R2 actionable propose candidates: `3`.

Top post-filter propose candidates:

| Rank | Node | Phase | Min kappa | Action intensity |
|---:|---|---|---:|---:|
| 1 | `futon4-d/mission/essays-diachronic-model` | `identify` | `-0.5556` | `0.5000` |
| 2 | `futon4-d/mission/or-training-as-learning-system.v1` | `identify` | `-0.3571` | `0.3214` |
| 3 | `futon6-d/mission/canon-fingerprint-store` | `derive` | `-0.1667` | `0.1500` |

R2 state: **sampled curvature + polarity complete**. The result confirms that bridge-candidate status is too broad, while `min-incident-kappa` magnitude plus `:resolution-state/actionable?` produces a small actionable mission set.

Full-pass confirmation, 2026-06-01:

- artifacts: `holes/missions/M-substrate-metric.R2-curvature-full.json`, `holes/missions/M-substrate-metric.R2-curvature-full-report.md`;
- current live graph after later mission-adjacent writes: `923` nodes, `1692` structural edges, `5` components, largest `909`;
- all `577` structural bridge candidates computed with OR curvature;
- timing: `3.316s` total, `2.497ms` mean per edge, `22.055ms` max per edge;
- kappa distribution: min `-0.5556`, p10 `0.0167`, median `0.0400`, p90 `0.0909`, max `1.0000`;
- post-R2 actionable propose candidates: `4` (`essays-diachronic-model`, `or-training-as-learning-system.v1`, `canon-fingerprint-store`, `bayesian-structure-learning`).

Full-pass verdict: **E1 curvature query delivered** for the current live graph. No broad hidden negative set appeared outside the sampled head; the negative tail remains selective.

---

## 10. INSTANTIATE - R2 resolution-state providers

Implementation: `futon3c.metric.resolution-state`.

Scope:

- mission provider maps lifecycle phase/status to the O3 resolvedness buckets;
- sorry provider maps registry status to open/addressed/closed resolvedness;
- pattern provider maps candidate/validated-style states where present;
- file provider returns explicit `:unknown` / non-actionable direct state, preserving the `file->mission` routing rule;
- unknown grains are explicitly non-actionable.

Focused witness:

- `clojure -M:test -n futon3c.metric.e1-test -n futon3c.metric.e1-report-test -n futon3c.metric.resolution-state-test -n futon3c.logic.substrate-metric-e1-invariants-test`
- result: `14` tests, `73` assertions, `0` failures, `0` errors.

Live bounded sample, 2026-06-01:

- query types: `code/v05/mission-doc`, `code/v05/sorry`, `code/v05/pattern`, cap `2000` per type;
- docs read: `218`;
- providers seen: `201` mission, `17` sorry, `0` pattern;
- resolvedness distribution: `1.0=50`, `0.65=45`, `0.35=15`, `0.1=74`, `0.0=5`, `:unknown=29`;
- directly actionable by O3 unresolvedness/actionability before curvature: `139`;
- unknown/non-actionable: `29`.

R2 provider state: **provider wiring complete for real mission/sorry/file/pattern shapes**. The sampled OR curvature run in §9 now consumes this provider surface.

---

## 11. INSTANTIATE - bounded OR sample probe (superseded)

Implementation: `scripts/substrate_metric_e1_or_sample.py`.

Artifact: `holes/missions/M-substrate-metric.OR-sample.md`.

Status: **superseded by §9**. This first probe was useful for timing, but it ran before the canonical graph filter excluded mission-adjacent report/sample artifacts from O1 nodes.

Scope:

- bounded E1 relation fetches only;
- structural `mu_x` with `alpha=0.5`;
- hop-distance over the E1 graph;
- exact local W1 via SciPy `linprog` for sampled bridge-candidate edges.

Run, 2026-06-01:

- command: `python3 scripts/substrate_metric_e1_or_sample.py --limit 2000 --sample 8`;
- graph at run time: `946` nodes, `1715` edges, `5` components, `600` structural bridge candidates;
- sampled edges: `8`;
- LP solve time: `31.31 ms` total;
- wall time including HTTP fetch: `742.26 ms`;
- kappa range in sample: `-0.8583` to `0.3333`.

Observation: after the R1 report was written, the live substrate immediately reflected that artifact, and `M-substrate-metric -- M-substrate-metric.R1-report` became the top structural bridge in the next sample. This is not a solver failure; it is evidence that the live graph is responsive and that consumer ranking may need an explicit self-artifact exclusion or downweighting policy before M-aif2 consumes top-k bridge suggestions.

OR-sample probe state: **timing evidence retained; ranking superseded**. The canonical-filtered R2 report in §9 is the current curvature/propose surface for E1.

---

## 12. Closure record - DELIVERED (v1)

Final keystone state, 2026-06-01: **DELIVERED (v1) - with named residue**.

This is a keystone terminal state, not a Campaign dissolution state. The Campaign remains open until at least one paired consumer consumes the metric live.

### 12.1 Delivered obligations

| Obligation | Closure verdict |
|---|---|
| O1 shared identity | **Delivered.** One multi-resolution typed identity set is fixed, closed under E1 and E2 relation families. `feeds-mu?` and `feeds-A?` are explicit; E2 selects existing `A[n,r,target]` triples and does not subdivide identity. |
| O2 one object | **Delivered as v1 contract.** The keystone supplies shared identity plus certified cut-realizations: E1 hop-distance / OR curvature and E2 direct code embedding / differentiable band. The hop-curvature vs embedding-curvature convergence check is explicitly deferred as named residue. |
| O3 E1 curvature cut | **Delivered end-to-end.** DERIVE signed, VERIFY green, R1 graph extraction live, R2 resolution-state providers wired, sampled and full OR curvature passes completed, and a stable actionable propose surface produced. |
| O4 E2 continuity cut | **Delivered for v1.** Direct code embedding is non-degenerate (`code-emb.npy`, `7534 x 1024`, cosine median about `0.627`), and O4(b) conditioning passed: `dS/dA` finite and well-scaled, max/median about `4x`, sparse `feeds-A?` structure (`7084` ownership + `852` file-dependency edges). |

### 12.2 Conditioning-scale final ruling

`:conditioning-scale` remains metric-owned metadata, but in v1 it is **audit / optional**, not default-applied weighting.

O4(b) evidence showed degree weighting is not load-bearing: bounded code-text windows keep source size out of `dS/dA`, and per-source softmax normalization keeps gradients well-conditioned. Applying `1/sqrt(total-A-degree)` slightly worsened max/median (`4.76` vs `3.95`), so the v1 default does not apply it. It may be activated later only if a future relation family demonstrates degree-driven swamping.

### 12.3 Ratification and escrow

- STANDARD-VERIFY: Joe ratified PASS-on-design.
- Escrow: E1 and E2 are `:contract-released`.
- E1 RUN/DELIVER: keystone delivered the curvature query; M-aif2 slice-1 owns live consumption.
- E2 RUN/DELIVER: keystone delivered the v1 code embedding / differentiable-band contract surface; M-differentiable-code owns live consumption.

### 12.4 Named residue

Residue is explicit and non-blocking for DELIVERED(v1):

- **Embedding v1 scope:** code embedding is Clojure-only in v1. Elisp/Python extractors are follow-on work, resumable without re-opening O1.
- **O2 convergence check:** hop-curvature vs embedding-curvature on overlapping grains remains deferred until both cuts are live enough to compare. This is a STANDARD-VERIFY-grade follow-on check, not a hidden premise.
- **Consumer wiring:** M-aif2 slice-1 owns consuming `curvature-at` / ranked candidates; M-differentiable-code owns consuming the code embedding. The keystone owns metric surfaces, not live consumer integration.

### 12.5 Campaign boundary

Keystone `DELIVERED (v1)` does **not** dissolve `C-substrate-completion`.

Campaign dissolution still requires at least one paired requirement consumed live, per the Campaign joint completion criterion. M-aif2 slice-1 reading live curvature is the likely first satisfaction path, but that is downstream of this keystone.

### 12.6 Branch / merge posture

Current branch: `codex/m-substrate-metric-runtime`.

Closure commit includes the final O4(b) ruling and this terminal record. Merge-to-master remains Joe's decision.
