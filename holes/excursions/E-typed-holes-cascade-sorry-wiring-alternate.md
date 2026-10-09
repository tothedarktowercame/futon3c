# E-typed-holes-cascade-sorry-wiring-alternate — M-typed-holes run as a cascade→sorry→wiring fold

**VERDICT (2026-10-09, provisional):** DONE — Counterfactual comparison doc whose stated deliverable is the comparison itself; it concludes with convergence recorded as the finding and only the optional conformance certificate left as a commissionable extra. _(WM status classification by zai-5, medium confidence; not yet confirmed by the author.)_

**Paired with `E-typed-holes-example-self.md` and the sibling of
`E-typed-holes-aif-alternate.md`. A second counterfactual: what M-typed-holes
*would have looked like* had we run it, from HEAD onward, explicitly as a
**cascade → sorry → wiring** fold (the "heart of a mission" / closure-fold
discipline from `E-ground-G` → `futon6/holes/early-closures.md` →
`M-typed-holes-example-first-flights.md`), instead of as undifferentiated
lifecycle phases. The point is the same as the AIF excursion's: the comparison
(§4) — does the fold route reach the same design, and what does it surface that
the "as planned" route left latent? Here the surfaced artifact is a
**conformance certificate**: a machine-checkable proof that M-typed-holes' own
lifecycle instantiates the mission-heart pattern.**

*Honesty: the cascade→sorry→wiring fold is a real, formalised operation
(`FirstFlightsExample.lean`, `first-flights-{cascade,wiring}.edn`). This doc does
not re-run it on the loaded CPU; it **re-reads the phases we already wrote** as
fold-stages, and names the one new artifact (the conformance cert) the fold
discipline would have produced. Nothing here is a fabricated mining run.*

## 0. The fold, in one line

`early-closures.md`: cascade→sorry→wiring is a **graph-transformation** ("poor
man's protein folding"). The **sorry** is a typed hole — disconnected pieces +
an unfilled target. The **cascade** proposes candidate design-patterns (a menu).
**Select** keeps the subset whose rewrite-LHS matches the sorry's topology.
**Wiring** is the *fill*: a graph-rewrite fold of the sorry-graph into a
construction. It is the **fold grain** of `fill` (grain 2; grain 1 is atomic
`PFunctor.comp`).

## 1. The mission re-read as one fold (HEAD → INSTANTIATE)

Had we typed each phase as a fold-stage from the start, the lifecycle would read
as a single cascade→sorry→wiring pass — and every stage is *already present* in
what we wrote, just unnamed:

| phase | fold-stage | what it (already) did | projection it routes through |
|---|---|---|---|
| **HEAD** | the want-port | declared the hungry target: "one fill, six surfaces, asking = proving" | — (the intent) |
| **IDENTIFY** | **sorry** | named the hole-topology: **six disconnected fill-sites + one unfilled shared-fill target** (the gap + 4 exemplars) | `discharge` (the sorry) |
| **MAP** | cascade (opens) | surveyed candidate material — ready/missing, Q1–Q6 — the menu starts assembling | `cascadeFeed` |
| **DERIVE** | **cascade + select** | the menu of candidate fillers (the projections + Poly datatype) **and** the selection (D1–D6 = which patterns wire the topology) | `cascadeFeed` → `answer` (select) |
| **ARGUE** | **select** (the type-check) | justified *why these patterns fold this sorry* — the pattern cross-ref (single-authority-registration, construct-an-explicit-witness, exotype-determines-behaviour) is the rewrite-LHS-matches-topology check; confronting "keep six fills" rejects the unselected | `answer` |
| **VERIFY** | **wiring** | folded the selection into the construction **and certified it** — literally produced `?wiring-cert` | `compose` (the comb/wiring) |
| **INSTANTIATE** | wiring (realised) | the folded construction as runtime: the single `fill` + six adapters (gated, D6) | `compose` → all six |

So the "as planned" run **was already a cascade→sorry→wiring fold** — it just
never said so. The fold is not an alternative path; it is the *type* of the path
we took.

## 2. The reflexive fixpoint (why conformance is special here)

The four fold-stages — **sorry, cascade, select, wiring** — are
`discharge`, `cascadeFeed`, `answer`, `compose`: **four of the six `Projection`
constructors `Coverage.lean` already certifies.** So the mission-heart pattern is
*built out of M-typed-holes' own subject matter*. The consequence:

> M-typed-holes' lifecycle conforms to the mission-heart pattern **because the
> mission-heart pattern is assembled from the very (hole, fill) projections
> M-typed-holes unifies.** Conformance is a **fixpoint**, not a coincidence.

This is the dogfood one level deeper than the self-typing excursion reached.
`E-typed-holes-example-self.md` typed the mission *as* a typed-hole object;
this types the mission's *lifecycle* as a fold built from the mission's *own
projections*. The thing it studies and the way it was made are the same shape.

## 3. What the fold route surfaces that "as planned" left latent

Exactly parallel to the AIF excursion (which surfaced the **death clause**), the
cascade route surfaces a **conformance certificate** — and, like the death
clause, it is not a new risk but a latent fact promoted to something checkable:

- **The latent fact:** the lifecycle-heart routes through `{discharge,
  cascadeFeed, answer, compose}` — four of the certified six. Stated nowhere.
- **The certificate:** a Lean theorem `conformsToMissionHeart` (extending
  `Coverage.lean` + `MissionExample`) mapping the heart's fold-stages to those
  four `fillFacet` images — so "this mission instantiates the mission-heart
  pattern" becomes machine-checked, not asserted. This is precisely the
  falsifiable-mission discipline `C-falsifiable-missions` is after.

The "as planned" route produced **design** + **wiring** certificates and had no
place for a **conformance** certificate — there was no node that asked "is this a
well-formed mission?". The fold route makes that the natural closing artifact.

## 4. The comparison (the payoff)

| | "as planned" route (done) | cascade→sorry→wiring route (this doc) |
|---|---|---|
| how phases are typed | as lifecycle phases (opaque) | as fold-stages (sorry / cascade / select / wiring) |
| conformance to the mission-heart | **latent, unstated** | **manifest** — each phase *is* a named fold-stage |
| where cascade/sorry live | scattered (sorry in IDENTIFY, cascade-menu in ARGUE) | placed as explicit stages of one fold |
| genuinely-new artifact | (design + wiring certs) | **the conformance certificate** |
| verdict on the design | *inevitable* | **same design — reached identically** |

Three findings:

1. **Same design, third independent derivation.** The fold route reaches the same
   thing (one `fill`, six projections). With the AIF route, that is now **three**
   independent arguments for the design — as-planned, AIF, cascade-fold — so
   "inevitable" is triply backed.
2. **The fold route's unique yield is the conformance certificate** — the
   counterpart to the AIF route's death clause. Both are *latent facts of the
   mission promoted to checkable artifacts* (adoption gate; mission-heart
   conformance), and both were invisible to the plain lifecycle run.
3. **The honest wrinkle is real but harmless.** In our committed docs the
   cascade-menu sat in **ARGUE** and the sorry in **IDENTIFY** — not a clean
   DERIVE=cascade / ARGUE=sorry positional mapping. The fold route would have
   placed them as explicit stages; but the *content* is identical, and the
   accurate (distributed) mapping — sorry@IDENTIFY → cascade/select@DERIVE+ARGUE →
   wiring@VERIFY — is what the conformance cert should certify. We prove what we
   did, not a tidier story.

## 5. Verdict — carry back to the real mission

As with the AIF excursion (whose HEAD/sigil/failure-conditions we merged into the
charter), adopt from the fold route into M-typed-holes proper:

1. **State the correspondence** in the charter + `M-typed-holes-VERIFY.md`: the
   lifecycle is a cascade→sorry→wiring fold, with the accurate distributed
   mapping and the four-projection fixpoint (§1–§2).
2. **Commission the conformance certificate** — a `conformsToMissionHeart`
   theorem extending `Coverage.lean`/`MissionExample`, mapping the heart-stages
   to `{discharge, cascadeFeed, answer, compose}` via `fillFacet`. This completes
   the mission's certificate triple: **design · wiring · conformance**.
3. **Do *not* re-run the phases.** All three routes (as-planned, AIF, cascade)
   converge on the same design and the same lifecycle content; re-derivation would
   only re-confirm it. Record the convergence as the finding — that is the result,
   not redone work.

The genuinely new artifact the fold route adds is the **conformance certificate**,
which the as-planned and AIF routes had no place for — just as the AIF route's new
artifact was the death clause.

*Cross-refs:* `E-typed-holes-example-self.md` (the live self-typing),
`E-typed-holes-aif-alternate.md` (the sibling counterfactual — death clause),
`M-typed-holes-VERIFY.md` (the two-certificate scope this adds a third cert to),
`mathlib4/DarkTower/Coverage.lean` (the six `Projection`s the fold-stages route
through), `mathlib4/DarkTower/FirstFlightsExample.lean` (the fold formalised as
`fill` grain-2), `M-typed-holes-example-first-flights.md`,
`futon6/holes/early-closures.md` §"cascade → sorry → wiring as a *process*",
`futon3c/holes/campaigns/C-falsifiable-missions.md` (the falsifiable-conformance
discipline this serves).
