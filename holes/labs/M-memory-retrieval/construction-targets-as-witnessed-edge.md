# ConstructionTargets witness the edge that memories cannot

Opened 2026-08-04 by claude-12, from Joe's observation: *"the ConstructionTargets
are a little bit like memories (shared objects) and I believe they* were *used."*

He is right, and the comparison is more useful than a loose analogy. It supplies
the **only witnessed instance in the corpus of `retrieval-hit → problem-solved`**
— the edge that `capability-proof-store.md` M4/M5 records as unwitnessed for
memories. All measurements below are from `apm-lean` at HEAD, 2026-08-04.

## 1. They were used — mechanically, not by report

Fifteen problems import a `ConstructionTargets` submodule. In **15 of 15** the
imported declaration is actually *referenced in the proof body*, not merely
imported:

| problem | submodule | declaration used |
|---|---|---|
| a02J05 | Sinc | `dirichlet_integral_improper` |
| a92J05 · a97A08 | Rouche | `zeroCountInClosedBall_add_eq` |
| a94A03 · a96A04 · a94J04 | LpRepresentative / L2Translation | `toLp_integral_eq_integral`, `hasFiniteIntegral_indicator_lift` |
| a94A10 | HerglotzRigidity | `affine_of_entire_pick(_neg)` |
| a95J01 · a95A08 | UnivalentDeriv / SchwarzEquality | `deriv_ne_zero_of_injOn`, `exists_rotation`, `exists_unit_mul` |
| a94A03 · a95J08 | YoungConvolution | `young_convolution_inequality` |
| a95A03 | RadialIntegrationR3 | `integrable_radial_R3_iff`, `radial_integral_R3` |
| a96A08 · a97A06 | Sinc | `tendsto_integral_sinc_pi_div_two` |
| a97A07 | CircleParam | `unitParam_eq_circleIntegral` |
| a95J06 | BanachZarecki | `banachZarecki_monotone` |

**12 of the 15 importing problems are sorry-free.**

**Cross-problem amortization, measured** — the property the memory store claims
and has not witnessed:

    toLp_integral_eq_integral            3 problems
    hasFiniteIntegral_indicator_lift     3 problems
    tendsto_integral_sinc_pi_div_two     2 problems
    zeroCountInClosedBall_add_eq         2 problems
    young_convolution_inequality         2 problems
    deriv_ne_zero_of_injOn               2 problems

## 2. The natural experiment: a92J05 and the module path

`ConstructionTargets.lean`'s own docstring states the failure in the store's
vocabulary exactly:

> *"before this, `ConstructionTargets/` had no `lean_lib` entry and no root
> module, so the directory was not on the module path — `import
> ConstructionTargets.SchwarzEquality` failed with 'unknown module prefix' and
> no `.olean` was ever produced. **The lemmas below were proved but mechanically
> unreachable from the problems they were built for.**"*

That is a **layer-4 index-reach failure**: content correct, present, and
invisible to the retrieval mechanism. Layer 4 is precisely the layer left OPEN
for memories in the four-layer anatomy.

Timeline for a92J05 (git, `apm-lean`):

| date | event |
|---|---|
| 2026-07-18 | a92J05 attempted; *"Rouché condition and no-roots-on-circle proven, **1 sorry for Rouché step**"* — **stalls on exactly one step** |
| 2026-07-29 | `ConstructionTargets/Rouche.lean` created — the lemma for that step now exists |
| **2026-07-30** | **`a270a2a` "Put ConstructionTargets on the module path"** — the lemma becomes reachable |
| 2026-07-30 | same day: *"isolate a92J05 Rouche root-count frontier"*, *"prove a92J05 Rouche boundary homotopy"* |
| 2026-08-01 | `d4198ec` a92J05 closed, 0 sorries |

Content existed for eleven days without moving the problem. It moved the day it
became reachable.

**Warrant: `inductive-n=1 natural experiment`, not `mechanical`.** The 07-30
commits are same-day as the fix, which is suggestive, not controlled — nobody
randomized the module path, and an agent may simply have pushed harder that day.
The honest claim is that the stall, the lemma's existence, the unreachability
window, and the resumption line up on the one step the lemma addresses.

## 3. Why CTs can witness the edge and memories cannot

Four differences, in increasing order of importance:

1. **Use is a compile-time dependency.** `import` + reference is checkable by
   grep and enforced by the compiler. Memory use is *reported* by the runner —
   the `USED`/`IGNORED` line. One is a record; the other is testimony, and this
   programme has now been wrong three times by trusting testimony.
2. **Reachability is binary and mechanical.** On the module path or not. Memory
   reachability is a retrieval score over a bag of ≤3 frequency-ranked terms,
   which E8 showed does not put named targets in the candidate list at all.
3. **The artifact is the content.** A CT *is* the lemma you apply. A memory is
   usually *advice about* content.
4. **Therefore CTs are substitutive; most memories are regulative.** This is
   V2's own distinction, and it explains the asymmetry rather than restating it:
   **a substitutive object can witness its own contribution, because using it
   leaves a mechanical trace in the artifact. A regulative one cannot** — advice
   that changed how someone worked leaves no dependency edge.

## 4. What follows for design

**(a) The unwitnessed edge is not uniformly unwitnessable.** It is witnessed for
substitutive objects and structurally unwitnessable for regulative ones. So
`capability-proof-store.md` M5's refusal should be **split by memory kind**, not
left global: substitutive memories have a mechanical upgrade path; regulative
ones need a different instrument or an honest permanent refusal.

**(b) Prefer memories that name a reusable artifact.** A memory whose content is
"`CT/Sinc.tendsto_integral_sinc_pi_div_two` closes the Dirichlet-tail step"
converts, at the moment of use, into an import — and therefore into a witness.
A memory saying "try Abel regularization here" does not. This is an actionable
scribe-protocol bias, not a philosophical point.

**(c) The candidate witness set already exists.** For every lemma reused by 2–3
problems, the *second and third* uses are cases where a memory pointing at the
lemma would have been useful — same shape as the E8 known-item cases, but with
the outcome already known. Six lemmas, ten second-or-later uses. These are the
records Joe asked for, and they are frozen: the problems are closed, the
dependency edges are in the artifacts, and nothing needs re-running to score
them.

**(d) The recording gap, measured.** I ran the check rather than proposing it.
Of the six declarations reused across 2–3 problems each, **only two are named by
any memory at all**:

    zeroCountInClosedBall_add_eq       1 memory
    young_convolution_inequality       1 memory
    tendsto_integral_sinc_pi_div_two   0
    toLp_integral_eq_integral          0
    hasFiniteIntegral_indicator_lift   0
    deriv_ne_zero_of_injOn             0

(`ConstructionTargets` appears 16 times across the store, so the store is not
blind to them as a category — it is blind to these specific reusable results.)

**So the cross-problem amortization that did happen, happened without the memory
store.** Four of the six lemmas were found and reused a second and third time by
some route other than a recorded memory — most likely the agent reading the
repo, which is exactly M-diagramprover's "the relay runs on repo-memory" stated
as a measurement rather than an impression.

This relocates the failure. It is a **recording gap (M1), not a retrieval gap
(M4)**: retrieval cannot surface what was never written down. The most valuable
memories the corpus could contain — the ones with demonstrated multi-problem
reuse and a mechanical witness available — are largely the ones nobody wrote.
That is a concrete, immediately actionable scribe-protocol target: backfill a
memory per reused ConstructionTarget declaration, and every future use converts
into an import and therefore a witness.

## 5. Open question this raises against the causal spec

The retrieval-stage DAG has no node for **artifact-mediated use**. It models
surfacing, offering and use as agent-side events. The CT evidence says there is
a second path — memory → artifact import → outcome — whose middle step is
observable in a way the direct path is not. That is a candidate delta (v12),
registered here, not applied: I would want claude-10's read on whether it is a
new node or a refinement of the existing use edge.

---

## 6. Backfill alone is a blip — the missed-promotion gap, measured

Joe, 2026-08-04: *"if we only backfill and don't add the capability (and indeed
requirement) to record future constructions of this nature, the backfill will be
a one-off blip."*

Measured against the frozen corpus. Helper declarations proved **independently in
two different problem files** — i.e. the same construction built twice, where one
promotion would have served both:

| helpers | problems | domain |
|---:|---|---|
| **6** | a00J04 ↔ a01A08 | lemniscate machinery (`isClosed_lemniscate`, `connectedComponents_complement_lemniscate_le`, `card_roots_le_degree`, `isPreconnected_subset_superlevel_or_sublevel`, and two superlevel-frontier lemmas) |
| 1 | a92J05 ↔ a97A08 | `zeroCountInClosedBall_aeval_eq_card_filter` |
| 1 | a00J05 ↔ a01A11 | `cauchyTransform_differentiableAt` |

**The a92J05 ↔ a97A08 pair is a controlled comparison and it settles the
question.** Those two problems *already share a ConstructionTarget*: both import
`ConstructionTargets.Rouche` and both use `zeroCountInClosedBall_add_eq`. So the
promotion route was known, available, and demonstrably used by these very
problems — and a **second** shared lemma was still duplicated inline. Promotion
is not gated by ignorance of the mechanism. It is gated by nothing at all: it
happens when someone thinks of it.

The a00J04 ↔ a01A08 pair is the same failure at scale — six lemmas, effectively
an unpromoted ConstructionTargets module.

**This is why backfill alone is a blip, stated as evidence rather than
prediction.** Backfill addresses the 165 declarations already in
ConstructionTargets and the reused ones with no memory. It does nothing about
the *next* duplicate, and the corpus shows duplicates arising even between
problems that were already amortizing successfully.

### What the requirement has to be

Two halves, and the second is the one that lasts:

1. **Capability** — detect it. A declaration in a problem file that is stated
   over general objects (does not mention the problem's specific data) is a
   promotion candidate. Crude proxy today: 707 of 752 problem-file declarations
   are not named after their problem, against 165 already promoted. The
   name-based proxy over-counts badly; the *duplicate-name* signal above does
   not, and it is a floor, not an estimate — it only catches collisions where
   two agents independently chose the same name.

2. **Requirement** — gate it. Same shape as the `USED`/`IGNORED` attribution
   line that already exists in the packet contract: a session that proves a
   general helper must either promote it to `ConstructionTargets/` **or** state
   why it is problem-specific. Untyped silence is what the memory contract
   already refuses elsewhere (`use-receipt` will not accept a surfaced memory
   without an inclusion reason); this applies the same discipline to
   constructions.

### Where the DAG earns its keep (Joe's argument, restated)

The value is **not** documentation. It is that a top-down decomposition of the
proof says *where a construction is called for* — at which node the agent has
reason to build a reusable object rather than an inline one. That converts
promotion from "someone thought of it" into a checkable obligation at a named
site.

And it is testable on frozen data before anything is built: the 15 CT-importing
problems record where constructions actually arose, and the 8 duplicated helpers
record where they should have. **A decomposition that predicts those 23 sites is
doing real work; one that does not is documentation.** That is a cheap
falsification test and it needs no Zai.
