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

## The build: an edge-graph kernel, general across three domains

*Joe, 2026-08-27: aim for a build general enough to work in Snatch, the
mathematics patterns, and general-purpose WM behaviour; Snatch is the easy test
domain.*

### What the corpora actually carry — measured before designing

| corpus | `@why` | `@how` | `@see-also` |
|---|---:|---:|---:|
| `futon3/library` (1227 flexiargs) | 60 | **1 file** | 38 |
| the five maths namespaces (~96) | 14 | **0** | 21 |

Plus a **second edge store**: substrate pattern→pattern edges, which
`TN-APM-cascades-exist-unused.md` confirms *"DO exist, as relations rather than
hyperedges"* (addendum 2026-08-26). And *leaf memories* — what zai is actually
handed — are *"a flat list … sorted by hash"*, carrying no structure at all.

**Design consequence, and it is the main one: the build must not depend on
`@how`.** It is the relation that would give cascades and it is essentially
unpopulated everywhere, including the domain that is closest to live test. The
build takes **typed edges from any source** and is indifferent to which directive
supplied them.

### The five steps

| step | what it does | domain-specific? |
|---|---|---|
| **1 · support** | a `core.logic` relation, run forward against domain facts, returning the bindings under which an edge holds | the *facts* are; the relation shape is not |
| **2 · mass** | a Beta posterior per **edge** — `intrinsic-values/credit-for` generalised from per-class to per-edge, as `aif2/tension.clj` already did for per-entry | no |
| **3 · kernel** | `node → D(node)`; a cascade is Kleisli composition | no |
| **4 · readouts** | `discriminates?` — is the channel constant — and `entropy`, the ambiguity term | no |
| **5 · mirror** | every witness ships a case where it must find **zero mass** | the case is |

Only steps 1 and 5 are per-domain, and only their *facts*. That is what makes one
build serve Snatch, mathematics and the War Machine.

### Why this belongs in R5 rather than elsewhere

R5's requirement is that the evaluate stage **report what its criterion set does
not cover**. Step 4 is that requirement made computable: a channel's entropy *is*
the statement of how much the criterion fails to determine, and `discriminates?`
is the statement that a dimension carries no singularity at all. **R5 is the ring
where a spread becomes reportable rather than merely present.**

### Dispatch slices — drafted, not sent

Sized per the handoff protocol: one behaviour, one acceptance test each. **Send to
codex-22.**

- **S1 · edge-graph reader.** Parse flexiarg directives into a typed edge set
  `{:from :to :kind :attested?}`, with the source pluggable so substrate relations
  can be added without touching the reader's consumers.
  *Acceptance:* over `futon3/library` it reports **60** `@why` files, **38**
  `@see-also`, **1** `@how` — the numbers measured above. A reader that returns
  different counts is wrong, and the counts are the test.

- **S2 · kernel constructor.** Given edges, a support relation and Beta state,
  produce `node → D(node)` with `entropy` and `discriminates?` readouts.
  *Acceptance:* on the snatch edge already witnessed, the support is the same four
  bindings `checks/how_witness_snatch.clj` returns, unattested edges weight at
  Beta(1,1), and the entropy of a uniform four-point channel is `log 4`.

- **S3 · snatch adapter and mirror.** Facts from the game's design diagrams; the
  mirror is the G1-vs-G3 case.
  *Acceptance:* witness has positive mass and positive entropy; **mirror has zero
  mass.** A kernel that puts mass everywhere is the stochastic form of a witness
  that cannot fail.

- **S4 · mathematics adapter.** *Held* until the maths apparatus repair lands, and
  noted now because it is the domain that will actually exercise this. Its facts
  come from the proof apparatus, not from a diagram.

**S1 and S2 are independent** and can go in parallel; S3 needs both.

### The adapter is a GUARANTEE on new flights, not an analysis of old ones

*Joe, 2026-08-27: "the 82 flights are of indicative importance only — what we
really need are new guarantees around new flights."*

**Recorded as a correction, and as a repeat.** This is the second time in one day
I have proposed validating against a historical corpus and been redirected: first
the retro-trip acceptance bar over `data/wm-trace/`, now an adapter over
`data/wm-full-loop/`'s 82 flights. The pull is real — the data is *there* — and
the requirement is about what happens next. **A frequency computed over closed
flights is not falsifiable; a guarantee on the next flight is.**

So the 82 flights keep exactly the role the trace corpus was given: **fixture
design.** They tell us which shapes are real — the fourteen terminal
dispositions, the (π, o) pairing, that 21% of flights lose their π — and nothing
more.

**The guarantee, and it attaches to a seam that already exists.** Every flight
writes `007-closed.edn`. So:

> **Every flight's close carries a coverage statement**: the criterion set
> declared for that flight, and whether the terminal outcome fell inside it. An
> outcome outside the declared coverage is recorded as `:uncovered` — never
> omitted.

That is R5's requirement made **operative per flight** rather than inferred from
a corpus. It is checkable at one point, on a record the runner already writes,
and it applies to the next flight rather than to the previous eighty-two.

**Chain, in Tier-0 shape:**

| link | what |
|---|---|
| Lean | `CoverageReport.lean` states it (dispatched 2026-08-27) |
| emitter | the clause joins the emitted contract |
| Clojure | validates a **new** flight's `007-closed.edn` |
| mutation test | proves a close that omits the coverage statement is **rejected** |

**What it would have said about 2026-07-15.** Those 22 attempts each recorded
*"no addressable entities"* per action class — a form of non-coverage reporting
at **action** grain — and then closed with `:outcome :no-selection` and no
coverage statement at **flight** grain. Under the guarantee the close must say
whether `:no-selection` was inside the declared criterion set. It was; and
*saying so* is what converts twenty-two silent repetitions into twenty-two
records that a reader can see agreeing with each other.

**Acceptance:** the first flight after the clause lands carries a coverage
statement, and a hand-mutated close that drops it is refused by the check. No
claim is made about any flight before it.

### Honest bounds carried into the build

- Beta(1,1) is a **prior**, i.e. a stipulation — `S-G3` requires it declared in
  the artefact, not absorbed into a number.
- Entropy over the wrong coordinates is a number, not a measurement. Whether
  `@holds-at` nodes are the right outcome space is **unresolved**, and this build
  does not settle it — it makes the question askable at one-edge scale.
- The `@how` sparsity means the first real graphs will be `@why`/`@see-also`
  shaped: authority and peerage rather than method decomposition. **Those are not
  the same relation**, and reading a cascade off them would be a category error.

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

## What the Snatch study settled about G(π) — and a fourth clause

**Added 2026-08-27** · after the `futon3/library/snatch/` collection was compiled
into a running policy. This is the section the paper's definition should track.

`NOTE-a-standard-for-G.md` named exactly one gap, and named it well:

> **`G` is earned at action grain and not at policy grain.** The only multi-step
> prediction repeats a single action, so "policy" collapses to "this action,
> sustained" — and a sustained action is not a π.

**S-G2 is now satisfiable by a real artefact.** The pattern-driven policy over
*Snatch or Share* produces, under G4 against a snatcher, the action sequence
`offer → denounce → offer → denounce → offer`, scoring `+3` where grim trigger
scores `−1`. Each action is chosen by a different pattern's `THEN`; the sequence
is heterogeneous because the *situation* changed, not because a schedule said so.
That is a predicted-and-then-realised sequence conditioned on a policy, which is
what S-G2 asks for and what nothing in the stack could previously produce.

### S-G4 — the value must move when the cascade is re-wired

`futon2.aif.cascade_prior` already states that two cascades with the same
patterns wired differently are different policies. Making precedence data made
that runnable: promoting one pattern in the consultation order, changing no
pattern text and no membership, moves the G4 score from `+3` to `−5`.

That gives the standard a fourth clause, in the same declared-domain form as the
other three:

> **S-G4** · a quantity may be emitted as `G(π)` only if it is **sensitive to the
> policy's wiring** — there exists an alternative ordering of the same components
> under which it differs. A score no re-wiring can move is not scoring the policy.

This is the singularity criterion applied one level up. A dimension along which
nothing varies carries no information; the same is true of a policy space in
which every wiring scores alike.

### S-G2 is necessary and not sufficient — the case that shows it

Grim trigger under G1 against a snatcher plays `offer → abstain → abstain →
abstain → abstain`. Two distinct actions, so it **passes S-G2**. It is still not
a policy in the sense we need: it is hardcoded, it has exactly one wiring, and
no re-ordering of anything can move its score. **S-G4 refuses it and S-G2 does
not**, which is the argument for having both.

### The definition, as short as it goes

> **G(π)** is the value a policy earns by being run: accumulated at action grain
> over one realised trajectory, and attributed at policy grain to the pattern
> cascade that produced the actions. A quantity earns the name only if the
> trajectory is not a single action sustained (S-G2) and the value moves when the
> cascade is re-wired (S-G4) — and only if it is computed from a predicted
> outcome distribution and declared preferences in the first place (S-G1), with
> every stipulated component declared (S-G3).

The cascade, not the action chain, is what carries the attribution: one pattern
acts per round, so the acting sequence is linear, while the cascade is the
`@why` sub-graph those patterns stand on. For the G4 run that is seven nodes and
ten edges, four of them standing on two authorities — a semilattice, which is
Alexander's own shape and the reason the attribution is not simply a list.

### Where the Lean stands

Two different R5 modules, and it is worth not conflating them:

| module | states | status |
|---|---|---|
| `DarkTower/WarMachine/CoverageReport.lean` | R5's **coverage** clause — declare the criterion boundary, type the outside | **built and reviewed** |
| `DarkTower/WarMachine/PolicyGrade.lean` | S-G2 and S-G4 as checkable predicates over a finished run, with the three Snatch witnesses | **built and reviewed 2026-08-27** (codex-22 `3677281f8b`, review fix `0de75bc6e6`) |

`CoverageReport` was already in place; what was missing was any statement of the
naming discipline, which is what makes `G(π)` refusable rather than merely
defined.

`PolicyGrade` proves five things, no `sorryAx`, no Mathlib. Two are general and
depend on no axioms at all:

- **S-G4 forces a policy space with more than one point** — a score cannot be
  sensitive to a choice of wiring that does not exist.
- **A singleton-indexed score family therefore fails S-G4**, whatever it scores.

Those carry the grim-trigger refusals, so the only commitment left in those cases
is the modelling one — that a hardcoded policy admits exactly one wiring — and
the refusal is a consequence rather than a second assumption. The first draft had
the refusal resting on the choice of `Unit` as the wiring type, which is true but
puts the argument in the type rather than in a proof.

### Two bounds on S-G4, from codex-22, accepted

**The wiring type does not witness that its values re-order the same
components.** `PatternWiring` has two constructors and nothing forces them to be
two orderings of one collection. That obligation falls on whoever builds the
family; here it is discharged by both scores having been measured on the same
twelve patterns, which is evidence outside the proof.

**A wiring-insensitive score may belong to a robust policy rather than a
degenerate one.** If every ordering of a collection scores alike, that could mean
the collection is good in any order. S-G4 refuses the name in that case too, and
the reason is worth stating precisely: **S-G4 is a condition on the measurement,
not a verdict on the policy.** A number no re-wiring moves supplies no evidence
that the cascade produced it; it may be a fine score, it is just not evidence
about π. The cost is real and should be carried openly — a genuinely robust
collection cannot earn `G(π)` on a scenario set that fails to separate its
wirings, and the remedy is a scenario that does, not a weaker clause.
