# P3 scaled memory-graph D_state sweep

Date: 2026-08-01
Status: frozen, deterministic, descriptive; no retrieval repair applied

## Claim boundary and census

This scales the V1 `damage_state_sweep.bb` operator without changing it.  The
receipt export contains 92 distinct problems with offered receipts, rather than
the inherited estimate of about 73.  The latest recorded query per problem was
selected deterministically by receipt timestamp and evidence id.  Two problems
(`a92J02`, `a93A01`) have only retroactive receipts with no recorded query, so
90 cases entered capture.  All 90 captured successfully.

Only 10 of those 90 cases produced both a non-empty candidate baseline and a
perturbable reviewed edge.  The other 80 produced an empty baseline under the
exact V1 lexical-proposal plus reviewed-pattern-projection operator.  They are
retained in the fixture and result, but excluded from damage fractions: an
empty candidate list cannot identify the effect of deleting an edge.  Thus the
honest sensitivity sample is **n = 10**, not 73, 90, or 92.  The 80 empty
baselines are a separate reachability result, not zero-damage observations.

## Scaled result

**Table 1 caption — CURRENT-GRAPH STRUCTURAL SENSITIVITY, NOT HISTORICAL
REPLAY: without dispatch-time snapshots this sweep measures the reviewed
memory graph at capture time, not the graph as it stood at dispatch.  The
lexical index reported `index-as-of` 2026-07-31T04:44:43.940884560Z for every
case; graph capture occurred 2026-08-01T09:20:02.467126697Z.**

| perturbation | usable problems | forks | changed | changed fraction | mean Jaccard damage | maximum Jaccard damage |
|---|---:|---:|---:|---:|---:|---:|
| remove one reviewed memory edge | 10 | 347 | 50 | 0.1441 | 0.0601 | 1.0000 |
| remove one memory→pattern role | 10 | 347 | 7 | 0.0202 | 0.0152 | 0.8000 |
| remove content arm | 10 | 10 | 10 | 1.0000 | — | — |
| remove pattern arm | 10 | 10 | 10 | 1.0000 | — | — |

The scale result is not null within the reachable subset: 14.4% of individual
edge removals and 2.0% of individual pattern-role removals changed the ordered
top-five candidates.  The large gap between edge and role sensitivity is
consistent with an attachment retaining other paths after one role is removed;
it is descriptive and is not an outcome-lift claim.  Both whole-arm ablations
changed every usable problem.

**Table 2 caption — same mandatory caveat: these are CURRENT-GRAPH structural
sensitivities, not historical dispatch replays.**

| problem | query | edge-removal changed fraction | role-removal changed fraction |
|---|---|---:|---:|
| hard-problems-a01a03-lean-main-lean | convolution identity indicator | 0.1220 | 0.0244 |
| hard-problems-a01a04-lean-main-lean | ball euclideanspace volume integral | 0.1429 | 0.0000 |
| hard-problems-a01a05-lean-main-lean | inner tendsto zero | 0.5556 | 0.0000 |
| hard-problems-a01a07-lean-main-lean | tendstouniformlyon convergence disks on_disks | 0.0820 | 0.0164 |
| hard-problems-a01j01-lean-main-lean | uniform convergence small line | 0.1429 | 0.0286 |
| hard-problems-a02j05-lean-main-lean | dirichlet sinc integral theorem | 0.1429 | 0.0000 |
| hard-problems-a03j03-lean-main-lean | convolution approximation tendsto holder | 0.1429 | 0.0286 |
| hard-problems-a93j06-lean-main-lean | deriv ball norm metric | 0.1429 | 0.0286 |
| hard-problems-a95j03-lean-main-lean | algpoly route degree search | 0.2500 | 0.0500 |
| hard-problems-a97a06-lean-main-lean | analytic complex analyticon line | 0.1220 | 0.0244 |

## Found and deliberately not fixed

1. The programme's inherited `~73` is stale: the frozen export has 92 distinct
   offered-receipt problems, 90 with a usable recorded query.
2. Current-operator reachability is much smaller than the receipt census:
   80/90 captured queries yield no candidate at all.  This is not repaired or
   reclassified as damage.
3. The lexical index snapshot predates graph capture.  No dispatch-time seed
   snapshot exists, and the text index has no temporal replay.  This is the
   already-staged B2 limitation, now visible in the scaled fixture.
4. Known defects in `used-ids`, statement-order term selection, and absent
   lane-scoped rarity ranking remain untouched.  `used-ids` is not used to fit
   this sweep.

## Determinism and frozen inputs

The script uses fixed seed `20260731`, sorted complete enumeration, and no
random sampling.  Capture is write-once; reruns are entirely offline.  An
immediate rerun returned both artifacts as `:existing` and reproduced hashes:

- fixture: `4d684c1a443858735ea26f633f3d40c6c234358fa3c5018fe558378d3403bc1c`
- result: `b82cc571be239a0e253713f4d8fed06cf465b442e549a1c3e561325059d437fa`

All named frozen inputs retained their pre-run SHA-256 hashes, including the
V1 fixture/result, both receipt exports, Ψ-v2 result, rejection coding, and
coding sections.  No existing artifact was edited.
