# E-R5-red-ring-fill — the singularity that arrived with no dimension to receive it

**Opened:** 2026-08-27 · claude-13 at Joe's direction. The fifth and last of the
red-ring excursions; the four others opened 2026-08-26/27.

**R5 is the inverse of R14, and together they exhaust how discrimination fails.**

> **R14** — a dimension with **no singularity** on it: τ is computed, reported,
> and no value of it changes the selected action.
> **R5** — a singularity with **no dimension** to receive it: `:warm-customer-pays`
> became satisfied and the machine had no channel on which to register it.

## Status

| | state |
|---|---|
| the ring's claim | **CONFIRMED at source** — and it is the best-evidenced of the five |
| bearer | **external** — *"the one red ring of the five whose cost is borne outside the stack"* |
| theoretical core | the Deleuze/singularity material, which belongs here rather than in R14 |
| relation to module 1 | `foldCompliant` in `GainChain.lean` is R5's property in embryo |

## The claim, checked

R5's pattern says `:warm-customer-pays` — *"the strongest signal the outside
world can send this stack — someone paid"* — arrived **satisfied, uncounted and
unsurfaced**. Verified 2026-08-27:

- `grep -c warm-customer-pays futon2/src/futon2/aif/observation.clj` → **0**.
  It is not one of the fourteen channels the machine steers by.
- `grep -c … futon2/src/futon2/aif/efe.clj` → **0**. It reaches no score.
- In the evidence store (`storage/futon1a/.../evidence.edn`) it occurs **once**,
  and that occurrence is prose: *"`warm-customer-pays` → (sales outcome, not an
  academic demo — **skip** for the fair)"*.

So the rung is not merely uncounted. **Its sole appearance in the evidence store
is a line scoping it out of a selection.** (Fairly: that scoping is to a demo
context, not a general dismissal — but it is the only trace, and it is a skip.)

**This is the first of the five rings whose recorded reason survived checking
without correction.** R8's producer, R14's location, R6's proposer and R2's
nag-inference all needed correcting; R5's does not.

## Why the Deleuze material belongs here

The ring's own salience already states the condition, citing the stack's own
theory:

> *"the one verdict the apparatus cannot issue to itself was received and not
> perceived, so nothing downstream could compose with it (futon-theory/futonic-logic,
> **A7: without salience, generativity remains inert**)."*

A7 and Deleuze's negative condition are the same sentence:

> *"those questions are senseless that inquire after dimensions upon which no
> singularity occurs, but instead produce a line of ordinary points that stupidly
> progress ad infinitum."*

R5 is that condition read from the other side. Not *"we are looking along a
dimension where nothing happens"* (R14, TryHarder) but *"something happened and
we were looking elsewhere."* Both are failures of discrimination; only together
do they cover it.

**And R5's node statement is the general form.** *"The evaluate stage reports what
the criterion set does NOT cover, with the same discipline it applies to a poor
score."* That is: **the criterion set must declare its own boundary**, so that an
event outside it registers as outside rather than as absent. Family 5's
`declaredDomain` and family 2's `typedAbsence`, applied to the criteria
themselves rather than to a producer.

**WR-25's framing is a special case.** *Good news gets the same evidence
discipline as bad* is one asymmetry of a polarity-neutral condition. The general
requirement is that **an event which discriminates must find a dimension**; the
good-news case is where this stack happens to have been caught.

## The requirement, in the family vocabulary

**Family 2 (typed absence) and family 5 (declared domain), at the criterion set.**

> A criterion set states what it does not cover. An event outside it is recorded
> as *outside*, with the same discipline as a poor score — never as nothing.

**❌ The naive fix: add a `:warm-customer-pays` channel.** It recreates the
defect one rung along. The next signal the world sends that the criterion set
does not anticipate is again invisible, and the fix scales by editing a list —
I1, and a list is the paradigm dimension with no singularity on it.

**✅ The requirement-satisfying fix:** the projection reports its **own
coverage** — which rungs it counts, and that there exist satisfied rungs it does
not — so an uncovered satisfaction is a *typed* report rather than a silence.
That is `foldCompliant` one level up: the step must leave a record even when it
cannot produce a value.

**Acceptance.** A satisfied rung outside the counted set produces a record
naming it as uncovered. No rung is silently absent. And, per the singularity
test: adding the fifteenth channel must be a consequence of the coverage
statement, not an edit to it.

## What makes this ring different from the other four

Its cost is **borne outside the stack**. R8's bearer is the apparatus, R14's is
unknown, R6's is *"the War Machine's own"*, R2's is the operator and the learning
loop. R5's is *"the operator's, and through him the paying party's."*

That has a consequence for sequencing: the other four can be repaired on the
stack's own evidence. R5's acceptance needs an event from outside, and those
arrive on the world's schedule. **The instrument must therefore be in place
before the next one arrives** — which is WR-27's *born instrumented* applied to
the one channel we do not control.

## Slices

1. **What the projection actually counts** — enumerate the rungs in the counted
   evidence projection, and whether the projection states its own coverage.
   *(discovery, cheap)*
2. **The coverage statement** — what it would mean for the evaluate stage to
   report non-coverage with the discipline of a poor score. Design, not code.
3. **The formalisation** — likely no new vocabulary: `typedAbsence` and
   `declaredDomain` at criterion grain. Confirm before writing a module; the
   modular-order note predicts R5 is *"largely instantiation"* of module 1.
4. **Do not build a `:warm-customer-pays` counter.** Recorded as a slice so the
   temptation is on the record as refused.

## Related

- `futon3/library/problems/satisfied-rungs-are-counted-and-surfaced.flexiarg` — the ring.
- `p4ng/empirics-futon/NOTE-singularity-and-discrimination.md` — the condition, and the five-into-one unification.
- `p4ng/empirics-futon/NOTE-patterns-as-problems.md` — why an enumerated criterion set has no singularities.
- `E-R14-red-ring-fill.md` — the inverse failure.
- `mathlib4/DarkTower/WarMachine/GainChain.lean` — `foldCompliant`, R5's property in embryo.
