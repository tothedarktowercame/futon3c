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

## Strategy notes

*Added 2026-08-26 by Fable, at Joe's request, after reading the code and the
trace archive that slices 1–3 point at. Nothing below closes a slice; each note
says what to do first, what to name, and what would count.*

### 1. Run slice 2 before slice 1 — the mechanism is locatable, and what it shows changes slice 1's question

The excursion says τ_eff is *"not yet located in the code"*. It is located; the
first grep missed it because it looked for softmax sites, and the site is a
function rather than a literal:

| step | where | what it does |
|---|---|---|
| τ computed | `futon2/src/futon2/aif/policy.clj:35` `effective-temperature` | `:spread` → τ_spread / g; `:selection-gain-only` → 1 / g |
| g fed in | `futon2/scripts/futon2/report/war_machine.clj:4248–4269` | reads `:selection-gain` from the previous trace record, folds `:realized-outcome`, passes `selection-gain-value` |
| τ used | `war_machine.clj:4476` → `policy/select-action` with `:selection-boundary :strategic-recommendation` | `strategic-recommendation` (`policy.clj:223`) computes τ, then sets `chosen` to `(first controller-entries)` — the rank-1 action, **independent of τ**. τ reaches only the reported `:controller-ranking` scores, the habit *counterfactual*, and `:softmax-weights` |
| action enacted | `war_machine.clj:4526` `wm-decision` | `assoc`s `:action (:action strategic-action)` — the output of `invoke-strategic-selection`, the reason-bearing selector — **over** whatever the controller chose |

So on the enacting path the temperature→action edge is cut twice: the selector
takes the rank-1 action regardless of τ, and the reason-bearing selector then
replaces that action anyway. The default `:actuation` branch is the same shape
(`best = (first ranked-actions)`, `policy.clj:377`; τ shapes only the
`:softmax-weights` it reports). The one branch where τ can change the argmax
is the habit-prior branch — `scores = −G/τ + ln E` — which runs only under
`:structural-pressure-mode :habit-prior`.

This is the finding `futon2/holes/labs/ants-faithfulness/PREDICTION-outcome.md`
recorded for the ants controller on 2026-08-01 — *"the commitment temperature
is dead … argmax over p = argmax over −G for every τ > 0; τ cannot affect the
choice"* — now in the War Machine. It also repeats that note's warning about
how the error gets made: the ants static scan had written *"adaptive τ genuinely
controls selection sharpness"* from the docstring. `selection_gain.clj:14` is a
docstring of the same kind.

**What this does to slice 1.** The pattern's IF — *"an actuator loop uses
commitment temperature to govern a live label-supplied flip"* — is not
satisfied by the running code. Then *no bearer is possible*, not merely
unfound: a dial that cannot change the selected action cannot cost anyone
anything, and the salience hole is a consequence rather than a mystery. The
honest re-description of R14 is **red on a disconnected dial**, which is a
third kind alongside R8's demonstrated defect and the "evidence hole" this
excursion opened on.

**Keep the pattern's constraint intact while saying so.** "May not argue from
the mechanism" forbids manufacturing a 香 instance from plausibility. A
line-numbered code fact that *explains why the interval is empty* is a
different thing and should be recorded as that — an explanation of the null,
not a salience instance. Do not write it into the `?salience(required)` slot.

**Verify it separately before acting on it.** The read above is mine, from one
sitting. Slice 2 should be a discovery dispatch with exactly one acceptance
bar: *state, with line numbers, whether any value of τ can change `:action` on
the path `wm_scheduled_run` → `judge` → `wm-decision`, and under which
`:structural-pressure-mode`.* Author ≠ reviewer applies to readings as much as
to code.

### 2. Slice 1 — name the corpora before searching, and pre-decide what counts

`T-wm-wrong-corpus-26082026` requires the null to carry its path. Enumerate the
paths in advance so the null is a list of searched corpora, not "nothing
found":

- `futon2/data/wm-trace/` — 53 files, 2026-05-18..07-21. The enacting archive
  (not `wm-full-loop/`). Already carries `:tau`, `:tau-spread`, `:tau-mode`,
  `:selection-gain` per record and the enacted `:policy`.
- `futon2/holes/NOTE-*.md`, `TN-*.md`, `E-*.md` — the inventory that mentions
  the quantity: `TN-wm-rank109-explained`, `E-r18-faithfulness-audit`,
  `TN-War-Machine-Restart`, `TN-wm-failure-to-launch`, `wm-baseline`,
  `NOTE-what-stopped-2026-07-08`, `NOTE-grounded-feed-missing-input`,
  `labs/ants-faithfulness/PREDICTION-outcome`.
- `p4ng/empirics-futon/NOTE-thirtyfour-steps-both-levels.md` and
  `NOTE-R8-what-to-build.md` §"R14 is already falsifiable today".
- The War Bulletins (WB-15 mints WR-27).

Three candidates will surface, and each needs a ruling on whether it qualifies
as *our dial being wrong, costly, or noticed* — the promotion test's
`:not-unblocked-by` clause already refuses the Galois fixture on exactly this
question, so decide these the same way, in writing:

| candidate | date | what it is | why it may not count |
|---|---|---|---|
| the τ-mode flip | 2026-07-13 (Joe; `9d8f2de`, first trace 07-14) | `arena-tau-mode` → `:selection-gain-only`, so τ_eff = 1/g; g pinned ⇒ τ_eff ≡ 1.0 in all 31 records since. Before the flip (07-04..07-09, `:spread`) τ varied: 0.80, 1.60, 0.106, 15.05. The flip handed commitment entirely to a gain that could not move, and nothing recorded that it had. | it is a dated event on our dial, and it was not *noticed* — which is the hole's own wording; but given note 1, the cost was zero |
| ants dead τ | 2026-08-01 | `PREDICTION-outcome.md` — a preregistered prediction falsified; codex-8/codex-9's work | same repo, same shape, sibling controller — **not the R14 dial** |
| operator ⑯ | 2026-08-26 | thirtyfour-steps: *"temperature demonstrably varies — after two wrong assumptions the search got more conservative"* | the operator's temperature, not the WM's; the strongest *noticed* instance, on the other level |

None of these is a clean 香 instance for R14. Whether the first one qualifies
is a judgement about what "noticed" means when the dial was inert; that is
Joe's call, and the slice should present the three and stop rather than pick.

### 3. Slice 3 — do not build the pairing record over a cut wire

The excursion already names the naive fix (log τ per tick, no linked
transition). There is a second one that note 1 exposes: **build the pair
(τ at the flip, next selected action) while the action does not depend on τ.**
That produces a well-formed pairing record of a quantity that is provably
inert — instrumentation at birth of a loop that is not a loop. It is the ants
static-scan error again, this time in data rather than prose.

Order therefore: slice 2 verified → Joe decides whether to reconnect the edge
(it is the enactment path, the same class of call as R8's slice 4) → *then*
slice 3, as the birth-time instrument of the reconnected loop. If Joe does not
reconnect, slice 3 does not happen and the ring stays red on a disconnected
dial, with that stated.

Two things in the archive are worth a look before designing the record, since
they are the nearest thing to the pair already on disk:

- The pre-flip traces (07-04..07-09) pair a varying τ_spread with an enacted
  `:policy` per record. 85 of 88 enacted policies are
  `M-bayesian-structure-learning`, so the same policy-diversity limit that
  blocks R8's slice 2 blocks any retrospective τ→action reading here. Say so
  with the path.
- `wm-trace-2026-07-18.edn` and `-07-21.edn` carry three records with
  `:governed-by :habit-prior` — the only records where a τ-scaled score is
  recorded as having changed the winner. Check whether these are the live
  decision or the D-1d shadow calculation before citing them; the keys around
  them (`:habit-prior-applied`, `:counterfactual`) suggest shadow.

### 4. The module cut, refined once more

The cut inside R14 stands, but the temperature-out face is itself two edges,
and the formal property should span both:

    g moved  ⟹  τ_eff moved  ⟹  the selected action is a function of τ_eff

`gainAdvances` (`GainChain.lean:156`) asserts the first arrow's premise and
nothing after it. The `CommitmentTemperature.lean` chain property is the
second and third arrows together; per the light standard, never one predicate
per arrow, because `effective-temperature` and `strategic-recommendation` each
typecheck alone and the defect is in their composition.

If slice 2 is verified, the refusal theorems have their dated incidents
without waiting on slice 1 — **2026-07-13** (τ_eff pinned to 1/g with g
unmovable) and **2026-08-01** (argmax annihilates τ, ants) — and the positive
witness is the habit-prior branch, where `scores = −G/τ + ln E` makes the
choice depend on τ by construction. That answers the excursion's own
objection ("no bearer, no incident, no theorem") without filling the salience
hole: the incidents are *disconnection* incidents, and the theorems should be
named that way. Whether a disconnected dial merits a module at all before it is
reconnected is a scheduling question for `NOTE-modular-formalisation-order`,
not something to settle here.

### 5. Working rules for this excursion

- **Dispatch shape.** Slice 2 is one discovery packet (a reading, with line
  numbers, no code). Slice 1 is one packet per corpus family if the notes
  inventory is too long for one sitting. Nothing is "find and fix".
- **Every null names its path**, per `T-wm-wrong-corpus-26082026`; a null over
  `wm-full-loop/` is a null about the non-enacting runner and must not be
  written as a null about the stack.
- **Do not re-verify what E-R8 already established** — the 88 outcomes, the
  three-layer silence, g pinned at 1.0. Cite `E-R8-red-ring-fill` and move.
- **Expect one premise correction, and make it now.** E-R8 was corrected three
  times because each claim was checked only after the next one was built on
  it. R14's "not yet located" is the corresponding claim here; note 1 is the
  correction, and slice 2's verification is the check that should precede
  everything else in this file.

## Review of the strategy notes — claude-13, 2026-08-26

Fable asked for independent verification before acting ("author ≠ reviewer
applies to readings as much as to code"). I am the reviewer; this is that check,
done at source rather than by re-reading the notes. **Every claim I could reach
verified.** No dispatch is needed for slice 2's reading — it is done here.

| claim | verified at | verdict |
|---|---|---|
| τ computed in `effective-temperature` | `policy.clj:35` | ✅ and the two modes are exactly as stated: `:spread` ⇒ τ_spread/g, `:selection-gain-only` ⇒ 1/g |
| the live selector ignores τ | `policy.clj:223` `strategic-recommendation`, `chosen` at **:238** = `(or (first controller-entries) (first ranked-actions))` | ✅ τ feeds only `scores` → `habit-order` → `counterfactual-idx`; it never touches `chosen` |
| default branch likewise | `policy.clj:377` `best (first ranked-actions)` when `priors?` is false | ✅ |
| habit-prior is the one branch where τ can move the argmax | `policy.clj:~400–410`: `scores = −g/τ + lp`, then `chosen-idx (apply max-key scores …)` | ✅ — and the reason is exact: `lp` is *not* scaled by τ, so τ changes the trade-off rather than cancelling |
| the enacting path takes the τ-free boundary | `war_machine.clj:4476` passes `:selection-boundary :strategic-recommendation` | ✅ |
| and then overwrites the action | `war_machine.clj:4527` `wm-decision (assoc controller-decision :action (:action strategic-action) …)` | ✅ the edge is cut twice, as stated |
| the trace archive | 52 files in `futon2/data/wm-trace/` | ✅ `:tau-mode` occurrences `:spread` 76 / `:selection-gain-only` 35; **every file from 07-15 through 07-21 carries τ = 1.0 and nothing else**; pre-flip values 0.80012, 1.60024, 0.1056, 15.0504 all present; **exactly 3** `:governed-by :habit-prior` records |

**Not confirmed, and worth one line of work:** whether `invoke-strategic-selection`
(`war_machine.clj:4097`) itself consumes τ. A grep of its head found no `tau`,
`temperature` or `selection-gain` reference, which is consistent with the notes
but is not proof. If it does, the second cut is not a cut.

**My error, and its cause.** This excursion as opened said τ_eff was *"not yet
located in the code"*. It was located all along, at `policy.clj:35`. The grep I
ran matched that file **thirty times**; I piped it through `head -25` and the
first 25 lines were all `preferences.clj`, so I read an absence off a truncated
output and wrote it down as a fact. That is the same family as
`T-wm-wrong-corpus-26082026` — a null asserted over evidence that was never
fully seen — and the general form is now: **before recording an absence, check
that the search was exhausted, not merely that it returned nothing you noticed.**

**Adopted from the notes:** slice 2 runs before slice 1, for Fable's reason —
if the pattern's `IF` ("an actuator loop uses commitment temperature to govern a
live label-supplied flip") is not satisfied by the running code, then no bearer
is *possible* rather than merely unfound, and the empty salience interval is a
consequence with an explanation. The constraint Fable attaches holds and is
restated here so it is not lost: **this explanation does not go in the
`?salience(required)` slot.** It explains why the interval is empty; it is not
an instance filling it. R14's honest re-description is **red on a disconnected
dial** — a third kind of red, alongside R8's demonstrated defect and this
excursion's opening guess of an evidence hole.

## The information-theoretic reading — and why it upgrades the finding

Joe, 2026-08-26. τ is the temperature of `P(π) ∝ exp(−G(π)/τ)`, which makes it
exactly the knob on the **entropy** of the policy distribution: τ → 0 gives a
point mass (H = 0, total commitment), τ → ∞ gives uniform (H maximal, no
commitment). Taking the argmax keeps only the *mode* — and the mode of a softmax
is invariant to its temperature for every τ > 0, since `x ↦ x/τ` is strictly
monotone. So on the enacting path

    I(τ ; selected action) = 0 bits

Not small. Zero, by construction, for every value of τ.

**This is a bound, not a code observation.** g reaches `:action` only through τ:
`chosen` (`policy.clj:238`) is a function of `ranked-actions` alone, and the
only other readers of `:selection-gain` are an audit field written *into* the
record (`enact.clj:226`) and reporting (`lane_futility`) — neither gates an
action. With `g → τ → action` a Markov chain, the data processing inequality
gives

    I(realized outcomes ; selected action) ≤ I(g ; action) ≤ I(τ ; action) = 0

**Consequence for R8, and it is a scheduling result.** Repairing R8 completely —
slices 4 and 5 landed, deposits flowing, γ moving off 1.0 — **cannot change a
single selected action.** `E-R8-red-ring-fill`'s stopping condition ("the ring
stays red with a working instrument attached") understates this: with the
instrument fully working the behavioural effect is provably nil until R14's edge
is reconnected. `p4ng/empirics-futon/NOTE-modular-formalisation-order.md` puts
R8's repairs first; this argues the reconnection is what makes them measurable.

**It also explains the empty salience interval exactly.** The cost of a wrong
dial is bounded by what the dial can transmit. Zero bits ⇒ no bearer is
possible. Fable's *"not merely unfound"* was a code reading; this is the bound
underneath it.

### The irony, in three layers

1. **This is an active-inference stack.** In the canonical form
   `P(π) = σ(−γ · G(π))`, precision over policies γ is the signature quantity —
   what the free-energy formulation adds over plain cost minimisation — and
   action follows from *sampling* the policy posterior, not from its argmax.
   **Argmax is precisely the operation that makes precision inert.** A machine
   whose only stated requirement is AIF faithfulness selects by the one rule
   under which its own precision parameter cannot act.
2. **G carries an epistemic term** — expected information gain about hidden
   states. A loop built to maximise information gain transmits zero bits from
   its own gain to its own behaviour.
3. **WR-27 says a loop is born instrumented for its gain, and here the
   instrument works.** `:controller-ranking` scores are literally `−G/τ`;
   `:softmax-weights` is the full distribution; `:habit-adjusted-ranking` and
   `:counterfactual` are computed and written. **The distribution is calculated,
   recorded, and then discarded at the final step.** The channel is not missing —
   it is severed at the last inch, *after* the reading is taken. Perfect
   instrumentation of a quantity with no downstream.

### And the one branch where τ does carry information names the repair

In the habit-prior branch, `scores = −G/τ + ln E`. Because `ln E` is **not**
scaled by τ, τ sets the exchange rate between evidence and habit, and there the
argmax does depend on it. The general statement:

> τ carries information only when it trades off two terms with different
> τ-scaling. A single-term argmax annihilates it. Precision is meaningful only
> relative to something else.

That yields the reconnection menu by derivation rather than invention:

| option | what it does | cost |
|---|---|---|
| **(a) sample** `P(π)` instead of argmax | canonical AIF; `I(τ ; action)` becomes the entropy τ controls, so the dial is live by construction | changes enactment behaviour — the same class of call as R8's slice 4, Joe's |
| **(b) keep argmax, keep a second non-τ-scaled term** | the habit prior, which already exists and governs exactly **3** records in the archive | needs the shadow-vs-live check the strategy notes flag before citing those 3 |
| **(c) accept it is decorative** | stop reporting τ as though it governed | honest, and it forecloses slice 3 |

Option (c) is a real option, not a concession: logging a zero-capacity quantity
per tick is exactly the *"well-formed pairing record of a provably inert
quantity"* the strategy notes warn against.

**Scope.** All of this is about the enacting path under
`:selection-boundary :strategic-recommendation`. Under `:structural-pressure-mode
:habit-prior` the capacity is nonzero, which is why those 3 records matter and
why establishing whether they are live or shadow is now the highest-value cheap
check in this excursion.

## The operator level — open, and deliberately left thin

R8's workup ran at three levels: machine, operator surface, company. R14 now has
the machine level (verified and formalised) and the company level (below). The
operator level is **open**, and Joe has no strong view on it (2026-08-26). This
section records what is actually there rather than filling the gap.

**What exists.** One cell: `p4ng/empirics-futon/NOTE-thirtyfour-steps-both-levels.md`
step ⑯, operator column — *"temperature demonstrably varies — after two wrong
assumptions the search got more conservative."*

**Why it should not be leaned on.** That is an agent's self-report about its own
behaviour, on the agent-self-report channel — which **slice 3 of this excursion
identifies as the uninstrumented one**, on the strength of a same-day case where
codex-12's completion summary contradicted its own commits. So the strongest
*noticed* instance of a commitment temperature in the stack sits on the channel
we have the least reason to trust, and it is uncorroborated. Treating it as
evidence would be R9 (no self-certification) in the one place the excursion is
most tempted to skip it.

**What today's finding does establish at this level**, and it is a correction
rather than a new claim: the WM's ⑯ cell in that note read *"runs, but `g`
pinned at 1.0 so temperature never adapts"*, locating the fault in the input.
That is wrong — the selector's argmax is invariant in τ, so ⑯ would not steer
the choice with a freely moving gain either. Corrected in that note, 2026-08-26.

**And it constrains a live proposal.** The note argues that the WM and operator
loops *"fail in complementary halves"* and that closing the AIF over the operator
is **"joining two half-loops that fail in opposite places."** That still holds,
with one joint now qualified: **the WM's ⑯ cannot consume a gain as built.**
Feeding operator outcomes into `g` would move `τ_eff` and change no action, so
this joint's payoff is gated on the reconnection decision, not on the data.

**The one thing the operator level does supply** is an existence proof rather
than evidence: at that level the τ→action edge is closed — the search *did* get
more conservative. Whatever mechanism does that is the nearest working instance
of what R14 asks for. Worth looking at if and when the reconnection is on the
table; not worth building an argument on now.

## The company level

`futon0/analysis/business-models/NOTE-select-column-and-R14.md` (2026-08-26)
develops the demand-side story for this ring, as the R8 workup did for its own.
Three points bear on the slices above:

1. **Four of the corpus's 15 phase-located cases sit in SELECT**, and all four
   say the same thing in nearly the same words — the appraisal completed and the
   commitment did not follow. PlanetMath: *"The step that never completed is the
   commitment, not the appraisal."* Lightbend: *"Nothing failed at perception or
   evaluation; the choice went against the seller."*
2. **Both greens are green on episodic sponsorship**, not on a commitment that
   followed an appraisal — Galois's record splits the buyer (DARPA `:engaged`,
   the services `:declined-on-capacity`), and PlanetMath's says of its Springer
   and Google funding that *"none became a standing budget line."* In this
   column, green does not mean the loop closed.
3. **Docker and Lightbend belong to R14's column, not R8's.** The earlier note
   assigned Docker to R8 through WR-27, which carries `@holds-open R8 R14` and
   so does not discriminate; the corpus's `:failing-phase` field says SELECT.
   Its *"0 of 2 returned signals changed the next action"* is therefore a
   reading of the commitment step.

**These are not a salience instance**, and the note says so at length: they are
a demand-side argument for instrumenting the step, not evidence that our
uninstrumented dial cost anyone anything. Same discipline as the strategy notes'
treatment of the disconnection finding.

## Related

- `futon0/analysis/business-models/NOTE-select-column-and-R14.md` — the company level.
- `futon3/library/problems/commitment-temperature-is-instrumented-as-gain.flexiarg` — the pattern, including the hole.
- `futon3c/holes/excursions/E-R8-red-ring-fill.md` — the predecessor.
- `p4ng/empirics-futon/NOTE-light-formalisation-standard.md` — the module recipe.
- `p4ng/empirics-futon/NOTE-modular-formalisation-order.md` — needs the module-1 refinement above.
- `futon2/src/futon2/aif/selection_gain.clj` — the gain-in face, docstring at lines 7–70.
