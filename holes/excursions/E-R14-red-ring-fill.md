# E-R14-red-ring-fill — commitment temperature, and who bears its cost

**Opened:** 2026-08-26 · claude-13 at Joe's direction, as the successor to
`E-R8-red-ring-fill`. Excursion from `futon2/holes/missions/M-formal-war-machine.md`.

**Read this first: R14 is not red for R8's reason, and this excursion must not
open as a repair.**

## The premise, stated before any work

R8 is red on a **demonstrated defect**. Its problem pattern carries
`+ salience: 香, by duration` with dates, an archive path, and a named bearer:
*"the apparatus's, and latently the operator's, since a learning loop that is
not learning spends his time without repaying it."*

R14 is red on an **unfilled salience hole**, and its own pattern says so:

> `?salience(required)`: No source row supplies a 香 instance for commitment
> temperature specifically. … **No dated observation records commitment
> temperature being wrong, costly, or noticed.** Recorded as an unfilled hole
> rather than argued from the mechanism: hallucinating content into the interval
> produces false salience.
>
> `+ whose problem:` **unknown, and that is the finding.** Of the five rings
> this is the one for which no party has yet been shown to bear a cost.

— `futon3/library/problems/commitment-temperature-is-instrumented-as-gain.flexiarg`

It also **refuses in advance the obvious shortcut**: the nearest salience
instance is WR-27's *"three uninstrumented loops found"*, which the pattern says
is generic to the ruling and *"is carried by R8's dead outer loop rather than by
this node."* Borrowing R8's evidence for R14 is a move the pattern has already
declined.

**Consequence.** Slice 1 is not a repair and not a formalisation. It is the
salience question. A legitimate outcome is *"no bearer found"*, which would make
R14 red on an evidence hole rather than a defect — a result to record, not a
failure to work around.

## The correction this forces to module 1

`NOTE-modular-formalisation-order.md` and `E-R8-red-ring-fill` both state
**module 1 = R8 + R14, one module**, on three signs: WR-27 carries
`@holds-open R8 R14`; commit `b624242` armed both, one flag named *"R14
live-wire migration"*; and one measurement (`:selection-gain` 1.0 in all 65
occurrences) exposed both.

Those signs hold, but they cover **one face of R14**. R14 has two:

| face | what it is | status |
|---|---|---|
| **gain-in** | `:selection-gain`, the terminal consumer of R8's realised-outcome fold. `selection_gain.clj` — a scalar, clamped to [0.5, 2.0], burn-in at exactly 1.0 until `min-history` samples accrue | **modelled**, as `gainAdvances` in `DarkTower/WarMachine/GainChain.lean` |
| **temperature-out** | τ_eff = adaptive-temperature(G-spread) / selection-gain, and its effect on the **next selected action** — the pattern's actual demand | **not modelled, and not yet located in the code** |

Joe, 2026-08-26: *"maybe it's more than one module."* Correct, and the cut does
not fall between R8 and R14 — **it falls inside R14**, between the gain it
consumes and the temperature it emits. `gainAdvances` is the seam: module 1 ends
where it asserts the gain moved, and says nothing about what the movement did.

## Slices

### Slice 1 — the bearer *(first, and blocking)*

**Question.** Does any dated observation record commitment temperature being
wrong, costly, or noticed?

**Constraints.**
- May not cite R8's dead outer loop, per the pattern's own refusal.
- May not argue from the mechanism. The pattern explicitly prefers an unfilled
  hole to a plausible reconstruction.
- The corpus must be named as a path (`T-wm-wrong-corpus-26082026`).

**Outcomes.** Either a 香 instance with a date and a bearer — in which case
slices 2 and 3 have a target — or a recorded null result, in which case R14's
redness is re-described honestly and this excursion closes there.

### Slice 2 — where τ_eff is actually applied

`selection_gain.clj:14` states `τ_eff = adaptive-temperature(G-spread) /
selection gain`. A first grep of `futon2/src/futon2/aif/` finds temperatures in
a softmax at only two sites, and **neither is obviously this one**:
`cascade_prior.clj:173` (a shadow-cascade `:tau`) and `preferences.clj:128`
(`default-c-temperature`, a C-vector channel temperature — a different
quantity). `selection_gain.clj` itself is explicitly *signal-agnostic* and
computes a scalar; it does not apply it.

**Question.** Does the R14 temperature reach policy selection at all, and at
which step? Until that is answered, "commitment temperature governs the flip"
is an architectural claim with no located mechanism — the same shape as the
Figure-4 wiring problem this mission exists to fix.

### Slice 3 — the pairing record

R14's `THEN`, verbatim: *"Record commitment temperature at the flip, link it to
the resulting actuator transition, and measure how it changes the next selected
action."*

That is **family 2 (inhabited handle) applied to the temperature→action edge**:
a step counts only if it left something durable, and here the durable thing is a
*pair* — (τ at the flip, the action subsequently selected). One without the
other is not evidence.

**The naive fix that would recreate a known defect.** Log τ per tick. A stream
of temperatures with no linked transition measures nothing; it is the
before-and-after the pattern's `BECAUSE` asks for, minus the after.

## The formalisation, if slice 1 finds a bearer

`DarkTower/WarMachine/CommitmentTemperature.lean`, at the light standard
(`p4ng/empirics-futon/NOTE-light-formalisation-standard.md`): standalone, one
chain property, one compliance property, refusal theorems named after dated
incidents, one positive witness.

**Not before slice 1.** With no bearer there is no incident to name a theorem
after, and a refusal theorem with no incident behind it is the thing this stack
calls false salience.

## Relation to R8

The two rings share a ruling (WR-27) and a scalar, and differ in kind:

- **R8** — a mechanism ran, then stopped. The work is repair, and the evidence
  is seven weeks of silence.
- **R14** — a mechanism may never have been wired to its consumer, and no one
  has been shown to notice. The work is first to find out whether that costs
  anything.

Treating them the same is what would produce a second excursion whose premise
has to be corrected three times.

## Related

- `futon3/library/problems/commitment-temperature-is-instrumented-as-gain.flexiarg` — the pattern, including the hole.
- `futon3c/holes/excursions/E-R8-red-ring-fill.md` — the predecessor.
- `p4ng/empirics-futon/NOTE-light-formalisation-standard.md` — the module recipe.
- `p4ng/empirics-futon/NOTE-modular-formalisation-order.md` — needs the module-1 refinement above.
- `futon2/src/futon2/aif/selection_gain.clj` — the gain-in face, docstring at lines 7–70.
