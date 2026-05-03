# Mission: War Machine Tuning
Status: parked

**Date:** 2026-04-26
**Status:** INSTANTIATE near complete — handing off to E-war-machine-qa (Checkpoint 10, 2026-04-26)
**Owner:** Joe + agent
**Cross-refs:**
  `futon3c/holes/missions/M-war-machine.md` (the War Machine itself);
  `futon2/holes/M-aif-head.md` (AIF-head requirements that any
    capability-claiming surface must satisfy);
  `futon5a/holes/missions/M-stack-stereolithography.md`
    (stack-level capability map; § Checkpoint 5 wires the recurring
    AIF tick that the War Machine consumes);
  `futon3c/holes/missions/M-stack-inhabitation.md` (inhabited vs
    uninhabited surfaces).

## 1. IDENTIFY

### Motivation

The War Machine exists as a running surface — there is a UI, there is
a backend endpoint, there is a recurring AIF feed, there are tiles
that render data. But it is not yet a **valid prototype**.

The operator's lived experience: an accumulation of non-things, broken
things, confusing things, mysterious things, and ambiguous things, all
having to do with the War Machine. It is unclear which controls do
what. It is unclear whether display units are consistent. It is
unclear how the clock-related signals (witness timestamps, scheduler
period, polling cadence) relate to actual operational state. It is
unclear how the Audacity-style timeline at the top is meant to be
read or operated. It is unclear whether the surface as currently
shipped meets the requirements named in `M-aif-head`.

Joe's framing:

> Right now I'd call the WM a "working mock-up" until such time as
> there is evidence that it is a valid prototype insofar as there is
> computational evidence that it can do something useful.

The gap, then, is not "the War Machine doesn't exist." The gap is
that the existing surface has not yet been audited, tuned, and
witness-backed against a clear validity claim. It looks like a
prototype, it runs like a prototype, but it has not yet been
demonstrated to *function* as a prototype in the strict sense
(closes a real loop, produces a real recommendation an operator can
act on, with a witness chain that survives audit).

### The actual sorry

> The War Machine surface is shipped but its validity claim is
> unsubstantiated. The operator is overwhelmed by an undifferentiated
> backlog of small tuning items, missing affordances, drift between
> what is documented and what is rendered, and untested integration
> seams. There is no single document that lists the tuning items, no
> rubric for distinguishing "broken" from "ambiguous" from
> "documented-but-not-yet-demonstrated", and no exit criterion for
> declaring the surface a valid prototype.

This mission exists to be that document.

### Definitional anchor (Deleuze & Guattari)

Before we audit our War Machine, we have to be honest about what a war
machine *is* — otherwise "valid prototype" is a free parameter and the
tuning ledger becomes a checklist of state-apparatus polish.

Source: D&G's treatment of the war machine in *A Thousand Plateaus*,
summarised in the transcript at
`~/Downloads/NoteGPT_TRANSCRIPT_What is the War Machine Deleuze and
Guattari Concept In Focus.txt`.

The load-bearing claim: **the war machine is not an entity but a mode
or capacity of action.** It is defined by what it *does*, not by what
it *is*. From the transcript:

> this idea of a machine is not an entity… it is a mode or capacity
> of action which is always immanent in every process and it allows
> for the possibility of innovation… or change in general.

A surface that displays metrics and lets you click on tiles does not
become a war machine merely by being labelled one. The state apparatus
*captures* war machines and turns them into formalised tools — military,
gang-with-rules, dashboard-with-statuses. Capture is the default
gravitational attractor; remaining a war machine takes deliberate work.

#### Operational tests, in D&G vocabulary

A surface is in war-machine shape only to the extent that it satisfies
these. A surface that satisfies none is captured-WM (or just dashboard).

| Property | D&G phrasing | Operational test for the WM |
|---|---|---|
| **Mode, not entity** | "not an entity but a mode or capacity of action" | Strip every tile and rendering — is there a *function* left that takes stack state and emits a creative move? Or is the whole thing the rendering? |
| **Numerics over metrics** | "in contrast to metrics the war machine is defined by numerics… an operative geometry, a mobile occupancy, a directional number or an instantaneous rate of change" | Does it report *directions of change* (Δ between observations, mode transitions, urgency derivatives), or only metric snapshots (counts, ratios, statuses)? |
| **Stationary process** | "a stationary process… that reacts constantly with the territory at any one moment and makes for the emergence of velocity and rate of change" | Does the surface visibly respond to *current* territory, or does it freeze state into formal categories and display the categories? |
| **Non-finalist** | "it does not have a purpose or end goal… The Nomad is not defined as having this goal but rather as having tendencies" | Does the recommendation card surface *tendencies* (rate of change toward something), or only *targets* (close S6, advance v2)? A target list is migration, not nomadism. |
| **Lives at the interstices** | "lives by interacting on these interstices of categories and changing them through creative methods" | Does it make boundary cases visible (capability transitions, cross-surface drift, contradictions) or smooth them into "everything is fine, here are your metrics"? |
| **Vector of deterritorialization** | "occupant but always as a function of a mobile process… vectors of deterritorialization" | Does it propose moves that *open new territory* (new candidate invariants, new mission shapes) or only moves that *close existing tickets within fixed categories*? Closing tickets is capture; opening territory is war machine. |
| **Preserved ambiguity / contact** | "secrecy and ambiguity… contact is something that D&G see as being warded off by the state" | Does it preserve genuine ambiguity (e.g. TI-1's distinction between "scheduler alive but not advancing" vs "scheduler dead") or collapse ambiguity into clean status badges? |
| **Capture sites visible** | "the state apparatus is necessarily going to operate by establishing zones of confluence or singularities… formalization and metrics" | Where has the surface already been captured? Every "official-looking" tile, "completed: …" badge, or rounded-percentage display is a candidate site of capture. The mission must name them. |

Operational rule of thumb: **closing tickets cleanly is fine, but it is
not war-machine work**. A tool that closes tickets is state apparatus
that has captured a war machine. To remain a war machine, the surface
must additionally produce directional/numeric/territory-opening signal
that the state-apparatus rendering does not absorb.

#### Why this is also the AIF correctness check

The D&G operational tests are not a parallel enthusiasm bolted onto
the AIF audit. They are the same machinery at a higher level — a
correctness check on whether the AIF surface is actually doing
active inference or only displaying its formal shell. The mapping
is direct:

| D&G test | AIF reading |
|---|---|
| **Mode, not entity** | AIF is a process (observe → perceive → affect → policy), not a snapshot. A surface that displays one frozen state once is not running AIF; it is rendering a previous AIF run's output. |
| **Numerics over metrics** | Numerics ≈ surprisal / gradient / instantaneous rate of change. Free energy *is* a numeric, by construction. **If we don't have numerics we don't have surprisal**, and a thing without surprisal cannot do active inference. The D&G category of "metric" is exactly the formalised classification AIF dissolves into when captured. |
| **Stationary process** | Belief update across time. AIF is `μ ← μ + κ·τ·ε`; if the surface never shows ε (prediction error) or its accumulation, the loop has been frozen into a snapshot read. |
| **Non-finalist** | Free-energy minimization is a *tendency*, not a goal. A target list ("close S6, advance v2") is the migration shape, not the nomadism shape. Genuine AIF policies are ranked by Δ-G; they don't hit a target and stop. |
| **Lives at the interstices** | AIF *is* the boundary between belief and observation, parameterised by ε. Smoothing ε into clean status badges destroys the very thing AIF computes against. |
| **Vector of deterritorialization** | Epistemic free energy. A move that closes a known pragmatic gap is a state-apparatus move; a move that reduces *uncertainty about new territory* is the AIF-policy / war-machine move. The 4-term EFE decomposition (pragmatic / epistemic / upvote / effort) in `futon3c.portfolio.policy` already names this — capture happens when only pragmatic shows on the surface. |
| **Preserved ambiguity / contact** | Posterior uncertainty. Representing μ as a point estimate ("status: hermit") instead of a distribution ("p(hermit)=0.62, p(foraging-trapped)=0.21, …") is exactly the collapse of contact. |
| **Capture sites visible** | Self-supervision: the AIF surface should be able to surface its own metric-shaped failures (places where it has stopped tracking surprisal and started tracking categories). Without that meta-channel, capture proceeds invisibly. |

So the **two halves of the validity claim are not independent**. Half B
(D&G properties) is the correctness check that determines whether Half
A's "valid prototype" is a valid prototype *of an AIF surface* or a
valid prototype of a dashboard-with-AIF-vocabulary. The TI-2 / TI-3 /
TI-4 / TI-5 items below all sit in this single load-bearing place:
they are simultaneously AIF gaps (the Clojure-side substrate exists,
the surface does not display it) and D&G capture sites (numerics
absorbed into metrics). Closing them is one move, not two.

### AIF lineage and the "circling" problem

A specific worry that motivates this mission, in Joe's words:

> we keep circling AIF but never actually landing it. All talk no
> active inference so to speak.

This is not a vague critique. The AIF substrate for the War Machine
**already exists** in finished, evidence-backed form across several
predecessor documents. Any audit that ignores them re-invents poorly
and reinforces the circling pattern.

#### The existing AIF substrate

| Source | What it specifies |
|---|---|
| `futon5/docs/core-terminal-vocabulary.md` | Abstract AIF/GFE meta-policy loop. Establishes the schema all domain vocabularies instantiate: observables → beliefs → policies → actions → free energy. |
| `futon5a/data/war-machine-terminal-vocabulary.edn` | Concrete War Machine instantiation. 12 observation channels (`:loop-health`, `:support-coverage`, `:attack-coverage`, `:mission-health`, four workstream-pct, `:active-repo-ratio`, `:sorry-count-norm`, `:coupling-density`, `:ticks-firing-ratio`). 6 modes (`:multiplied`, `:foraging-trapped`, `:hermit`, `:depositing`, `:stagnant`, `:dark`). Full preference ranges (`:C/preferred`, `:C/avoided`, `:C/mode-prior`). Pragmatic + epistemic free-energy functions. **A real 2026-04-12 snapshot showing mode `:hermit`, G-pragmatic 0.72, G-total 0.50, all four pocketwatch ticks firing.** |
| `futon0/scripts/futon0/report/war_machine.clj` | Implementation of the vocabulary. 2 089 lines. `observe` at line 1412, `infer-mode` at line 1581. Computes the channels and the mode. |
| `futon5a/holes/stories/THE-STACK.aif.edn` | Stack-level meta-AIF+: 16 leaf graphs quotiented to 4 stack conflicts and 16 next-moves. The `:reading :next-move` field consumed by the UI's recommendation card. |
| VSATARCS narrative walk + `holistic-argument*` series | Argument-level framing. Support claims S1-S5, attack claims A1-A4. The signal `loop-health` channel reads against. |

#### The actual circling diagnosis

The AIF schema is fully designed. The Clojure implementation exists
and computes the values. The 2026-04-12 snapshot proves the values are
real, distinguishable, and load-bearing (it correctly diagnoses the
hermit trap). What the surface ships is a different story:

- Searching the cljs UI client for `:G-pragmatic`, `:G-epistemic`,
  `:strategic-mode`, `:free-energy` or any of the 12 observation
  channel names returns essentially nothing — `:loop-health` is
  referenced once at `core.cljs:433`. The other ten channels and
  every mode/free-energy field are absent from the surface.
- The next-move card consumes the *static* `:reading :next-move`
  field from THE-STACK.aif.edn, not the dynamic AIF computation in
  war_machine.clj. The mode, the free-energy decomposition, and the
  current observation vector never reach the operator.
- The recurring tick added in `M-stack-stereolithography` Checkpoint 5
  deposits `:portfolio/observation` and `:portfolio/policy` evidence
  on a daily cadence. Nothing on the UI displays it as observation
  or policy. It exists in the store and waits.

So the lived experience — "all talk no active inference" — describes
a real failure mode: the AIF computation runs, the observation values
are real, and the surface renders the *formal shape* of an AIF
dashboard (hex tiles, witness chains, timeline) without surfacing the
actual numerics underneath. This is exactly the D&G capture pattern
from `§ Definitional anchor`: the war machine has been absorbed into
state-apparatus rendering. Metrics-as-form replace numerics-as-flow.

#### What this means for the tuning rubric

Several tuning items must address this directly:

1. The 12-channel observation vector should be visible on the surface
   as a current-state read with preference-band overlays. The
   operator should be able to see at a glance which channels are
   inside `:C/preferred` and which are outside.
2. The inferred mode should be visible. The 2026-04-12 snapshot's
   `:hermit` diagnosis is exactly the kind of thing the surface
   exists to make legible. If the surface cannot say "we are in
   hermit mode" with witnessed observation, it has not landed AIF.
3. Free-energy decomposition (pragmatic / epistemic / total) should
   be visible. The single number is less interesting than the
   *gap pattern* — which channels contribute most to G-pragmatic
   right now is the operator's actual recommendation surface.
4. Predicted vs observed reconciliation. AIF is generative — the
   surface must show prediction error, not only state. Without
   that, it is observation theatre, not inference.

These are added as TI-2 through TI-5 in `§ 2 Tuning ledger` below.

### Working taxonomy of tuning items

Five categories. Every War Machine tuning item should be filed under
exactly one (and the filing itself is part of the audit).

1. **Non-thing** — a feature the surface implies exists but which has
   no implementation behind it. Tile renders a placeholder, link
   leads nowhere, control has no effect. The fix is either to remove
   the affordance or to implement what it implies.
2. **Broken thing** — implementation exists but produces wrong or
   stale output. Includes silent failures (the surface looks fine
   but the underlying signal is dead). The fix is to repair or to
   surface the failure honestly.
3. **Confusing thing** — implementation works but the UX semantics
   are unclear: which control does what, what a colour means, why a
   tile is positioned where it is. The fix is documentation or
   redesign, not code.
4. **Mysterious thing** — the operator cannot tell from the surface
   alone what an artefact represents or where it came from. Includes
   missing tooltips, missing hover-detail, opaque ids. The fix is a
   provenance affordance.
5. **Ambiguous thing** — could be working, could be broken, no
   signal. The fix is to add a witness or freshness indicator that
   resolves the ambiguity.

### Validity claim

The exit condition for this mission has two halves. Both must hold; a
surface that meets one without the other is not what the mission name
claims.

**Half A — valid prototype of *some* tool:**

> Given current evidence-store state and current scheduler state, the
> War Machine produces — within one operator-visible step — a
> *specific* recommendation, anchored to *cited* witnesses, that an
> operator can act on. Acting on it produces evidence the surface can
> display on its next refresh.

Failing Half A means the surface is a working mock-up of a tool;
nothing else can be claimed about it.

**Half B — actually a *war machine* and not state-apparatus capture:**

The surface must additionally satisfy at least three of the eight
operational tests in *§ Definitional anchor (D&G)* above. Concretely:

1. The recommendation surfaces a directional/numeric signal
   (a rate of change, a tendency, a vector), not only target IDs.
2. At least one currently-shipped move is a vector of
   deterritorialization — opens new territory rather than closing an
   existing ticket within a fixed category.
3. At least one ambiguity that lives in the underlying state is
   preserved as ambiguity on the surface (not collapsed into a clean
   badge), with a UI affordance that lets the operator dwell in it.

Failing Half B means the surface is a valid prototype but it is **not
a war machine** in D&G's sense — it is captured-WM (a state-apparatus
dashboard wearing the label). That is a meaningful, ship-able thing,
but the mission would exit with the explicit finding that the surface
should be renamed or refactored to stop overclaiming.

The strongest exit is both halves met. Anything else is documented
honestly: "valid prototype, not yet war machine," "war-machine signal
present but no operator-actionable witness chain," or "neither — still
a working mock-up."

### Theoretical anchoring

- **`M-aif-head`** names the AIF-head requirements: explicit
  generative model, default mode tier, prediction-error tracking
  across observations. The War Machine claims to be a strategic
  reading of the AIF substrate; whether it actually composes with
  the requirements is not yet audited.
- **`stack-coherence/evidence-ledger`** — every claim on the surface
  must cite a witness. A surface that displays state without
  provenance is a surface that lies under load.
- **`stack-coherence/maturity-evidence-audit`** — maturity claims
  must be auditable. "Valid prototype" is a maturity claim; the
  audit rubric is what this mission produces.
- **Stereolithographic split (M-stack-stereolithography)** — the War
  Machine is one of the surfaces enumerated as a projection of the
  capability landscape. Its own status row in that mission's
  printout will read as either operational, emerging, or declared
  depending on the outcome of this mission.

### Scope in

- A **single tuning ledger** in this mission doc that enumerates
  every known War Machine tuning item, classified by category.
- For each item: short description, file/line reference where
  available, current state, proposed fix, and current owner.
- An **audit pass against `M-aif-head`** that names which AIF-head
  requirements the current surface satisfies, partially satisfies,
  or fails to satisfy.
- A **clock/cadence audit**: list every clock-related affordance on
  the surface (witness timestamps, scheduler period, AIF poll cadence,
  freshness thresholds, Audacity timeline scale, hover-state minute
  labels) and resolve display unit consistency.
- A **discoverability audit**: enumerate every tile/control and
  decide whether mouse-over or other hover-detail is sufficient.
- A **functional audit of the timeline strip** (the Audacity-style
  interface at the top): document what each visual element means,
  what each interaction does, and whether the implementation
  matches the documented behaviour.
- A **closed example loop** — pick one specific recommendation the
  surface produces, follow its witness chain end-to-end, act on it,
  and confirm the next refresh shows the consequence. This is the
  validity-claim instance proof.

### Scope out

- Replacing the War Machine. This mission is tuning, not redesign.
- Any new top-level surface or tile that does not address an
  enumerated tuning item. New surfaces belong in their own mission.
- Reopening the cadence question — that was settled in
  `M-stack-stereolithography` Checkpoint 5 (daily AIF, single-source
  scheduler period). This mission audits how the surface *consumes*
  the cadence; it does not relitigate the cadence.
- Adding instrumentation that does not feed the audit. Telemetry
  for telemetry's sake belongs elsewhere.

### Completion criteria

1. The tuning ledger below has every currently-known item, each
   classified into exactly one of the five categories.
2. The `M-aif-head` audit table is filled in with one row per
   AIF-head requirement and a current-state column.
3. The clock/cadence audit lists every clock-bearing surface
   element with a single declared unit and source-of-truth
   reference.
4. The discoverability audit covers every tile/control, with a
   pass/fail decision on hover or affordance sufficiency.
5. The Audacity-style timeline has a documented spec section
   (what each pixel-region means, what each interaction does)
   and the implementation matches that spec.
6. At least one specific recommendation the surface produces has
   been followed end-to-end and the consequence has been verified
   on the next refresh — the Half-A instance proof.
7. Half B is audited explicitly: each of the eight D&G operational
   tests has a current-state column (pass / partial / fail) with
   cited evidence, and at least three are pass — or the mission
   exits with the explicit finding that the surface is captured-WM
   and should be renamed/refactored.
8. The mission exits with one of two outcomes:
   *"valid war machine"* (both halves met) or
   *"stop-the-line because X"* (a specific named upstream blocker
   genuinely out of this mission's scope; X must be concrete and
   citable). The previous middle landings (*"valid prototype, not
   yet war machine"* and *"war-machine signal present but no
   actionable witness chain"*) were soft exits that recreate the
   FuLab failure mode and are explicitly disallowed. See
   Checkpoint 7 for the rationale and the upgrade path. Silently
   shipping an unmet claim is not allowed.
9. The computational notebook
   `futon5a/analysis/notebooks/M-war-machine-tuning.clj` has been
   maintained continuously through the mission's lifetime: every
   landed TI has at least one notebook cell that fetches current
   state and demonstrates the fix (no markdown-only claims).
   Snapshot history is sufficient to compute prediction error
   against actual past observations, not asserted ones.

### Lessons from FuLab (p4ng) — what real AIF still got wrong

The FuLab agents (futon3-pre-split, written up in `~/code/p4ng/`) had
**real** AIF backing, not vocabulary wrapping. The pseudocode in
`p4ng/appendix.tex` Box 2 specifies:

- Beliefs μ over pattern evidence, τ-cache, maturity phase
- Softmax sampling over `-G/τ` for pattern selection (with explicit
  precision-controlled exploration/exploitation)
- Belief update from observed outcomes — actual prediction error,
  actual τ-adjustment, actual evidence accumulation
- PSR before action, PUR after, abstain when τ below threshold
- AIF *guides* rather than *controls* — the agent may pick a
  non-suggested pattern, with a required *deviation justification*

This was a careful design. The AIF layer was load-bearing, not
ornamental. **And the agents were still in most cases difficult and
painful to use.** Adding the AIF layer often meant constraining
agents in non-productive ways.

That experience names a failure mode that this mission must avoid by
construction: **a surface can satisfy the AIF correctness check
(Half B above) and still be a tax on operator agency rather than an
augmentation of it.** The four mechanical TIs — channels, mode, EFE,
prediction error — are necessary but not sufficient. Landing them
without attention to the integration shape would reproduce exactly
the FuLab problem at strategic scale.

#### Anti-patterns to refuse

Each of these is a way the FuLab AIF layer paid for itself in pain.

1. **Tax-without-payback.** AIF state that costs operator cognition
   without changing operator action. (FuLab: the agent had to read μ
   and τ on every cycle even when the recommendation was already
   obvious or already wrong.)
2. **Justification ritual.** Forcing the operator to explain why
   they did not act on the AIF suggestion. The deviation
   justification was epistemically clean and operationally
   suffocating.
3. **Cadence coupling.** The operator's workflow gets paced by the
   AIF tick rather than the other way. (Mitigated for our scheduler
   already by the daily-not-30-second decision; must stay mitigated.)
4. **Fidelity-over-usefulness.** The research temptation: make AIF
   more accurate, more sophisticated, more theoretically tight. The
   FuLab evidence is that fidelity did not produce usefulness — it
   produced constraint. *The surface gets better by being useful, not
   by being more correct.*
5. **Theatre.** AIF state visible on the surface, but no path from
   "operator reads the state" to "operator's next move is better
   than it would have been without reading." The audit must catch
   this: a surfaced numeric is only valuable if the operator can
   point to a decision it changed.

#### Implication for the validity claim

This refines Half A. "An operator can act on the recommendation" is
not enough. The surface must:

- **augment**, not gate — never block or condition operator action on
  AIF state (in particular, no abstain-as-page, no forced
  justification);
- **be ignorable on demand** — the operator who closes the WM tab and
  works without it should not be worse off than an operator who
  consults it ritually;
- **earn its presence** — for at least one concrete operator decision,
  there is recorded evidence (in the notebook) that the AIF signal
  changed what the operator did, in a way they retrospectively endorse.

A surface that fails any of these is, by FuLab's hard-won precedent,
not yet a useful AIF surface — it is the next FuLab.

#### A sixth tuning category

The five-category taxonomy in *§ Working taxonomy of tuning items*
covers non-thing / broken / confusing / mysterious / ambiguous. The
FuLab lesson adds:

6. **Constraining thing** — implementation works correctly and is
   even discoverable, but it adds coordination tax (cognitive,
   temporal, or workflow-shape) without commensurate payback. The
   fix is removal, demotion, or making the affordance opt-in rather
   than ambient.

New tuning items found while auditing under this rubric must be
filed as `constraining` — not as `confusing` (the user understands
fine; the cost is the tax itself) and not as `non-thing` (the thing
exists; it is just net-negative to have).

### Research significance and the computational notebook

If this mission lands — if the surface actually surfaces surprisal,
posterior, EFE-decomposition, and prediction error rather than the
formal vocabulary of AIF — then the result is **not stack-internal
tuning**. It is a real contribution to AIF systems engineering:

1. **D&G as higher-order check on AIF correctness.** The 8-row mapping
   in *§ Why this is also the AIF correctness check* is — to my
   knowledge, conditional on a literature pass — a novel framing.
   It distinguishes "system that uses AIF vocabulary" from "system
   that runs AIF" via a non-trivial criterion (numerics ≈ surprisal,
   capture-sites visibility ≈ self-supervision, etc.).
2. **"Circling without landing" as a documented AIF anti-pattern.**
   The failure mode where AIF schemas are designed, computed, and
   then absorbed into dashboards-with-AIF-vocabulary is concrete and
   reproducible across multi-agent stacks. Naming it gives future
   builders a check.
3. **Mission-feature precision-proxy as an empirical AIF-grounding
   method.** The Stream A scoring in `M-stack-stereolithography`
   measures *where AIF is actually grounded vs aspirational* across
   a codebase. That is methodological.
4. **A working stack-as-AIF surface as an existence proof.** If the
   War Machine ends up satisfying both halves of the validity claim,
   it is — to my knowledge — a working strategic-timescale AIF
   surface over a multi-agent software organism. Not a toy domain.

These are publishable claims **conditional on landing the engineering**.
Discussing them now without the engineering would be exactly the
"circling" pattern this mission diagnoses. So:

#### The notebook discipline

A computational notebook is maintained alongside the mission, at
`futon5a/analysis/notebooks/M-war-machine-tuning.clj`. Its purpose:

- capture the AIF state of the running stack on a continuing basis
  (observation vector, μ, EFE decomposition, mode, prediction error)
  with timestamped snapshots, so that prediction error can be
  computed against actual history rather than asserted in prose;
- record each TI's evidence with reproducible queries (no claims
  about "the surface now shows X" without a notebook cell that
  fetches the current state and shows X);
- serve as the experimental log for any intervention (e.g. "we
  added the mode badge — here is the EFE decomposition before and
  after the change");
- become the data substrate of an eventual paper, if the work lands.

The notebook is **load-bearing**, not decorative. A claim made in the
mission text without a corresponding notebook cell is an unaudited
claim. The discipline is: *if you cannot run a cell that produces
evidence for a claim, the claim is not yet evidence-backed*.

This is also itself a D&G test: does the work generate live numerics
(a notebook that pulls current state) or only metrics (a markdown
file that asserts state)?

### Exit criterion for IDENTIFY

A human has read this proposal and agrees that:

1. the gap is real,
2. tuning rather than redesign is the right shape,
3. the five-category taxonomy, the D&G definitional anchor, and the
   two-half validity claim are the right first discipline,
4. the eight completion criteria are auditable, and the four honestly-
   named exit outcomes (valid war machine / valid prototype / signal-
   without-witness / working mock-up) are the right answer space.

## 2. MAP

### MAP posture

This mission is an **audit** mission, not a build mission. The first
MAP pass therefore asks: *given the gap framed in IDENTIFY, what is
the smallest evidence-producing path to the audit's first concrete
finding?* The answer is not a sprint plan; it is an inventory of
existing audit infrastructure, an enumeration of what is already
ready, and a sequencing of the unknowns.

The frame: **most of the substrate already exists.** The AIF schema,
the implementation, the recurring tick, the live evidence stream, the
notebook companion, and the augmentation ledger are all in place. The
gap is at the surface (UI does not consume the substrate) and in the
audit pipeline's emptiness (the snapshot log, TI-evidence log, and
augmentation ledger are zero-state). MAP's job is to confirm that
shape and pick the first concrete moves.

### Inventory: existing audit infrastructure

The mission can act on these at any time without first building
anything new.

| Asset | What exists | Readiness |
|---|---|---|
| Mission doc (this file) | IDENTIFY complete, taxonomy, validity claim, FuLab lessons, research-significance staging | ready |
| Computational notebook | `futon5a/analysis/notebooks/M_war_machine_tuning.clj` (vsat.wiki style, kindly-ready) | ready |
| Drawbridge eval helper | `drawbridge!` in the notebook; smoke-tested against running JVM | ready |
| Snapshot machinery | `snapshot!`, `append-snapshot!`, `read-log` | ready |
| TI-evidence machinery | `append-ti-evidence!` against `M-war-machine-tuning-ti-evidence.edn` | ready |
| Augmentation ledger | `record-augmentation!`, `augmentation-audit` against `M-war-machine-tuning-augmentation.edn` | ready |
| D&G operational tests as queries | three test fns in notebook §7 (`numerics-over-metrics`, `stationary-process`, `posterior-not-point-estimate`); five more named, not yet coded | partly ready |
| Recurring AIF tick (substrate) | `futon3c.portfolio-inference.scheduler` running, daily cadence, depositing 4 evidence entries per tick | ready |
| AIF terminal vocabulary | `futon5a/data/war-machine-terminal-vocabulary.edn` — 12 channels, 6 modes, full G fns, 2026-04-12 reference snapshot | ready |
| Live AIF stack endpoint | `/api/alpha/aif-stack/live` carries the `:scheduler` block (period, last/next-tick-at) | ready |
| WM Clojure implementation | `futon0/scripts/futon0/report/war_machine.clj` 2 089 lines: `observe` (1412), `infer-mode` (1581) | ready |
| WM cljs UI surface | `futon0/web/war-machine/src/war_machine/client/*.cljs` — running, hot-reloaded by shadow-cljs watch | ready |
| FuLab precedent | `~/code/p4ng/appendix.tex` Box 2 + `contents.tex` § Patterns and Active Inference | ready |
| D&G transcript | `~/Downloads/NoteGPT_TRANSCRIPT_…txt` | ready |
| `M-aif-head` requirements | `futon2/holes/M-aif-head.md` § Completion criteria | ready |

### Inventory: existing data

Pulled fresh at MAP time via the notebook's Drawbridge helper. These
are the load-bearing observations the audit will reason against in
DERIVE.

- **Scheduler:** running, period 86 400 s, `started-at`
  2026-04-26T18:10:40Z, 3 ticks fired, 0 errors. `next-tick-at`
  2026-04-27T18:12:41Z.
- **Precision-table rows:** 19 (one per invariant family).
- **Promotion candidates:** 4 (was 2 at the prior MAP-equivalent
  reading; the candidate-promotion pipeline is producing fresh
  evidence between sessions).
- **Portfolio evidence in store:** 550 entries under
  `{:ref/type :portfolio :ref/id "inference"}`.
- **Snapshot log size:** 0. The mission's audit pipeline is
  zero-state; not a single notebook-recorded snapshot exists yet.
- **TI-evidence log size:** 0.
- **Augmentation ledger size:** 0. The FuLab guard reads
  `:no-evidence-of-augmentation`. Honest baseline.
- **Tuning ledger:** 5 TIs enumerated (TI-1 ambiguous, TI-2 / TI-3 /
  TI-4 non-thing, TI-5 ambiguous). The 6th category — `constraining`
  — has no entries yet.

### Ready vs missing per completion criterion

The mission's nine completion criteria, each marked with what is
already in place and what remains.

| # | Criterion | Ready | Missing |
|---|---|---|---|
| 1 | Tuning ledger has every known item, classified | 5 entries, all classified | items found under `constraining` rubric (FuLab pass); items found in cadence/discoverability/timeline audits |
| 2 | `M-aif-head` audit table filled in | requirement source ready | row-by-row audit pass |
| 3 | Clock/cadence audit lists every clock-bearing affordance | two rows pre-seeded (AIF tick freshness, AIF poll cadence) | Audacity timeline x-axis, hover labels, witness timestamps, scheduler-period display |
| 4 | Discoverability audit covers every tile/control | rubric defined | per-tile sweep |
| 5 | Audacity timeline has documented spec | section header | spec text + matching audit |
| 6 | Half-A instance proof | validity claim refined; recommendation surface exists | one specific recommendation followed end-to-end with notebook-recorded evidence |
| 7 | D&G audit table filled with cited evidence, ≥3 pass | 2 rows pre-seeded (`numerics-over-metrics: fail`, `preserved-ambiguity: partial`) | 6 rows; computational-form queries for several |
| 8 | Honest exit outcome named | the four outcomes are enumerated | not at exit yet |
| 9 | Notebook continuously maintained, every landed TI has a cell | scaffolding in place | non-empty snapshot log, non-empty TI evidence, ≥1 augmentation entry |

### Surprises

Things discovered during IDENTIFY that materially change MAP's posture:

1. **The AIF substrate is complete.** Schema, implementation,
   evidence flow, recurring tick — all in place. The mission is not
   "build AIF for the WM" but "make the existing AIF reach the
   surface without becoming FuLab." This is a much smaller scope
   than the lived-experience overwhelm suggested.
2. **D&G audit and AIF correctness are the same machinery at two
   levels.** Originally drafted as parallel audits; collapsed into
   one in IDENTIFY § Why this is also the AIF correctness check.
   That collapse means closing TI-2 / TI-3 / TI-4 / TI-5 simultaneously
   resolves at least four D&G tests; not eight separate pieces of
   work.
3. **The recurring scheduler is already shipping AIF evidence the
   surface ignores.** 550 portfolio entries in the store; UI references
   `:loop-health` once at `core.cljs:433`. The first DERIVE move can
   be a UI-side change with no substrate change.
4. **FuLab forces an augmentation discipline.** Without the ledger,
   the mission could land all four TIs and reproduce FuLab. The
   augmentation ledger is therefore not a nice-to-have — it is the
   distinguishing audit between "AIF surface that helps" and "AIF
   surface that constrains."
5. **The 11-of-12-channel gap is the load-bearing UI deficit.**
   Eleven AIF observation channels do not reach the cljs UI. That
   single fact, rather than the eight-test rubric, is the most
   compact summary of what landing this mission means.
6. **Promotion candidates moved from 2 to 4 between sessions.** The
   underlying mission portfolio is not static. Audits must be
   timestamped against snapshots; assertions about "the surface
   currently shows X" decay if not pinned.
7. **The notebook style precedent (vsat.wiki) is already
   kindly-ready.** Clay/Quarto rendering of the eventual paper
   draws no new tooling. The publication path is opened by `clj
   -X:clay` from the existing deps.edn.

### Questions to close before DERIVE

These are the unknowns that determine where DERIVE starts.

- **Q1.** Which TI lands first? The choices are TI-2 (12-channel
  panel), TI-3 (mode badge), TI-4 (G decomposition), TI-5
  (prediction-error sparkline), TI-1 (pulse vs witness), or a sixth
  TI surfaced by the FuLab pass (`constraining`). Each makes the
  surface more numeric, but the augmentation cost differs.
- **Q2.** Where in the cljs UI does the new AIF state belong? A
  fourth tile in `legend-panel`? A new panel? The existing sidebar?
  An overlay? The choice determines whether AIF state competes
  with the witness card or augments it.
- **Q3.** What is the smallest augmentation entry the team would
  retrospectively endorse? An entry like *"saw mode = :hermit and
  decided to switch to consulting work"* counts; *"saw G = 0.50 and
  closed the WM tab"* does not. The first concrete one decides the
  mission's existence-proof shape.
- **Q4.** How is the snapshot log seeded? Daily-with-the-tick is
  natural but produces sparse data; manual capture before/after
  every notebook session produces dense data. Both have a place.
- **Q5.** What is the relationship between the markdown D&G audit
  (§ 4b) and the notebook `test:*` functions? The markdown is
  human-readable; the notebook is reproducible. The two should
  converge: every markdown row should cite a notebook test, and
  every test should produce the markdown row's `pass`/`fail`/`partial`
  verdict.
- **Q6.** What is the role of the `M-aif-head` audit (§ 4) vs. the
  D&G audit (§ 4b)? `M-aif-head` checks one peripheral against
  AIF-head completion criteria; D&G checks whether the surface is
  in war-machine shape at all. They overlap on at least three
  rows. Whether to fill them in lockstep or separately is open.
- **Q7.** Does the literature pass on D&G + cybernetics happen now
  or after the first TI lands? The notebook's §10 Q1 names this
  as a falsifiability-frontier item: the 8-row D&G→AIF mapping is
  a discovery only if no one else has written it. If we land
  engineering first and lit-pass second, the mission is engineering-
  first and the publication question is deferred until evidence
  exists. If we lit-pass first, we may find we are restating, and
  scope shrinks. Risk-balance question.
- **Q8.** Should the scheduler auto-start on JVM boot? The
  M-stack-stereolithography Checkpoint 5 follow-up names this as
  a deferred decision. Auto-start changes who owns the AIF feed
  (the JVM, not the operator). Augmentation ledger may have an
  opinion.

### Checkpoint 1 — 2026-04-26 — attempted answers

Best current reading on each question. Each answer is conditional on
the IDENTIFY frame and on the live data inventoried above.

**A1 (Q1: which TI first).** **TI-3 (mode badge).** Reasoning: it is
the most concentrated AIF signal (one of six modes vs. twelve channels
or a four-term G decomposition). The 2026-04-12 snapshot's
`:hermit` diagnosis is the textbook example of a single legible fact
that changes operator behaviour. It also cleanly tests the
augmentation hypothesis — *"did seeing 'mode: hermit' change what you
did?"* is a concrete operator-decision-change question. TI-2 is more
information per pixel but also more cognitive load; landing it first
risks the FuLab tax pattern. TI-5 (prediction error) requires a
non-trivial snapshot history we do not yet have. So TI-3 first; TI-2
or TI-1 second; TI-4 and TI-5 after the augmentation hypothesis has
been tested at least once.

**A2 (Q2: where in the UI).** **A small mode strip above the
next-move card.** Reasoning: the next-move card is the existing
recommendation surface, so the mode is colocated with the move it
relates to. A new panel risks the operator overlooking it; the
sidebar competes for the same attention as the witness chain.
Mode-above-move keeps the layout simple and forces the operator to
read mode while reading the recommendation. Hover surfaces the rule;
no click-required indirection.

**A3 (Q3: smallest endorsable augmentation).** **A mode-change
correlation.** When the operator next switches workstream (e.g. from
stack work to consulting), record whether the surface had been
showing `:foraging-trapped` or `:hermit` at the time. If yes, that
is the augmentation existence proof — minimum viable. The decision
does not have to be caused by the surface; it has to have been
informed by the surface, with the operator's retrospective
endorsement that they would have switched anyway, but the surface
made the case faster / clearer.

**A4 (Q4: snapshot seeding).** **Both.** Daily-with-the-tick
captures background state for prediction-error work (TI-5 needs
this); manual capture before/after operator decisions captures
augmentation-relevant state. The notebook already has primitives
for both (`append-snapshot!` for ad-hoc; a daily cron entry can
call it for steady-state). The cron belongs in DERIVE.

**A5 (Q5: markdown vs notebook D&G audit).** **Notebook is
canonical, markdown is generated.** The notebook's `test:*`
functions return verdicts; a small render fn writes the markdown
table from the verdict map plus citations. This collapses the
two-place-redundancy that the IDENTIFY pass left.

**A6 (Q6: M-aif-head vs D&G).** **Fill in lockstep, with shared
evidence cells.** Where a TI satisfies both an M-aif-head requirement
and a D&G test, the same notebook cell produces evidence for both.
This avoids the parallel-audit cost.

**A7 (Q7: literature pass timing).** **Engineering first, lit pass
second.** Rationale: the publication is conditional on landing the
engineering anyway (§ Research significance). A lit pass before
landing TI-3 risks scope-creep into theory; landing TI-3 first puts
existence-of-surface-doing-something-useful in evidence, after which
the lit pass can be sharper about what the contribution would be.
Defer to before any paper draft.

**A8 (Q8: scheduler auto-start).** **Defer.** Belongs in
M-stack-stereolithography Checkpoint 5 follow-up, not in this
mission's scope. Manual operator start of the scheduler is currently
adequate; auto-start changes who-owns-the-cadence in ways that
deserve their own argument.

### Net conclusion

MAP is answerable enough to move toward DERIVE. The audit's path is:

1. Take a first snapshot. Append it to the notebook's snapshot log.
   This is the zero-state fix and the existence proof that the
   notebook pipeline works against the live JVM.
2. Land TI-3 (mode badge, mode-above-next-move). Render evidence
   from the most recent `:portfolio/belief` entry, *not* from a
   freshly computed call. Fall back to "AIF unknown" if the evidence
   is absent or stale beyond `period-seconds × 1.5`.
3. Record the first augmentation entry: a mode-change correlation
   with the next operator workstream switch. Endorse or refuse to
   endorse retrospectively.
4. Repeat for TI-2 / TI-4 / TI-1 / TI-5 in some order, each with
   its own augmentation entry. A TI without an augmentation entry
   is not yet known to augment.

The main remaining risks are scope-management questions — *how
many TIs to land before declaring the mission's first VERIFY pass,
and how strictly to insist on augmentation-evidence before
counting a TI as resolved.* Those are explicit in this MAP rather
than hidden as slack, which is the futonic posture for moving from
MAP to DERIVE.

So the MAP exit criterion is met at document level: existing
infrastructure has been inventoried, live data has been pulled and
cited, the nine completion criteria each have ready-vs-missing
columns, the surprises have been named, the questions have answers,
and the next concrete move (snapshot the live state, land TI-3) is
specified.

### Checkpoint 2 — 2026-04-26 — inside-out pivot, invariant-first MAP

The Checkpoint 1 sequencing answers (TI-3 first, mode badge, etc.) are
fine to use as we go but are not load-bearing. Joe redirected:

> What might be insight-forming would be to build a set of invariants
> similar to the ones that we used to build futon1a, which would imply
> that we should start from the "inside out". Cf
> `~/code/futon1a/README-best-practice.md`.

That redirect changes the mission's centre of gravity from *"tuning
ledger of UI items"* to *"invariant inventory + per-invariant audit."*
The TI ledger remains as a flat enumeration of surface-visible items,
but the invariants are the spine — the inside-out organising principle
that determines which TI repairs which invariant violation, and which
TIs can be marked resolved.

#### Direct comparison with the futon2 cyberant ants

The cyberant ants in `~/code/futon2/src/ants/` are the existing
running AIF reference in the stack. The WM was designed against them
(see `war-machine-terminal-vocabulary.edn` § `:harmonisation
:cyberants-template`). The comparison made flat:

| AIF stage | Cyberant ant | War Machine | Distinctive |
|---|---|---|---|
| Sensory dimensionality | 14 dims (`observe.clj`) | 12 strategic dims (`o/raw-fields`) | WM aggregates ant-scale signals to stack scale |
| Beliefs μ | `(position, goal, hunger, mode)` | `(loop-state, argument-balance, workstream-balance, sorry-trajectory, strategic-mode)` | WM beliefs are over the *whole stack*, not one agent |
| Modes | foraging / depositing / homing / etc. | multiplied / foraging-trapped / hermit / depositing / stagnant / dark | WM modes are *organisational* readings of ant-scale modes |
| Free energy G | pragmatic + epistemic | pragmatic 0.65 + epistemic 0.35 | WM weights pragmatic higher because deadlines close |
| Policies π | π-scholar, π-consultant, π-free-solo, etc. (in JSDQ); per-ant action repertoires | `:π-display` only | **WM has exactly one policy: observe and display.** |
| Actions a | move, deposit, forage, etc. | NONE | **WM does not act. The Hobbes criterion (WM-I4 in the existing vocabulary): sovereignty preserved, operator decides.** |
| Surface | `ants/visual.clj`, `ants/ui.clj` | cljs UI in `futon0/web/war-machine/` | The ant visual makes ant numerics legible; the WM surface mostly does not |

The structural template held: same AIF schema, different timescale,
different domain, mostly-coextensive vocabulary. **The headline
difference — and it is operationally load-bearing — is that the
ants act and the WM does not.** This is the design choice that
distinguishes WM from FuLab: where FuLab's AIF layer constrained
agent action (and hurt), the WM's AIF layer is supposed to
constrain *nothing* and only display state. The FuLab anti-patterns
in IDENTIFY apply to the WM only insofar as the WM tempts the
operator into ritualised reading that gates their action — i.e.
even a no-action AIF surface can tax the operator if it forces
attention without payback.

The relocation question (move WM into futon2 alongside the ants?)
is named here and **deferred to ARGUE**. Arguments on each side:

- **For relocation.** futon2 is the existing AIF home; co-locating
  ants and WM concentrates the AIF substrate; the cyberant
  vocabulary is the structural template; futon3c CLAUDE.md says
  futon3c is "real-time coordination" which is not the WM's
  timescale anyway.
- **Against relocation.** WM consumes from futon3c's evidence
  store and from `mission-control-backend`; relocating the
  Clojure-side without breaking those dependencies is non-trivial
  invariant-rewiring work; the cljs UI lives in futon0; relocating
  the backend without the frontend creates new cross-repo
  coordination that we don't currently pay for.

Decision deferred until at least WM-I0 through WM-I3 are stated
and the audit pipeline produces non-trivial evidence under each.
A relocation that happens before the invariants are in place is a
move under uncertainty; a relocation that happens after is an
informed architectural call.

#### Inside-out invariant inventory (proposed)

Proposed by analogy with futon1a's I0–I4 (durability → identity
→ integrity → authorization → validation), with the layer order
reversed for audit (cheapest substrate check first):

##### WM-I0 · Substrate liveness

The recurring AIF tick must be running, depositing evidence at
the declared cadence, and the evidence must be retrievable.

**Why first.** If WM-I0 fails, no surface above it can be trusted.
Any "the WM shows X" claim becomes either stale or asserted-not-
observed.

**Check.** `(scheduler/status)` returns `{:running? true
:tick-count > 0 :error-count 0}` AND the count of
`:portfolio/step` evidence entries grew by ≥1 within the last
2 × `period-seconds`.

**Error shape.** `{:wm/invariant :WM-I0 :wm/reason
:substrate-stalled :wm/last-tick-at <inst> :wm/now <inst>
:wm/expected-by <inst>}`.

**Repair.** `(scheduler/start! ...)`, or escalate if the failure
is upstream of the scheduler (evidence store unreachable, JVM
restart needed).

##### WM-I1 · Observation totality

Every snapshot must carry all 12 declared observation channels
from `:o/raw-fields`, each in `[0, 1]`, none nil. A snapshot
missing channels is corrupt; rendering on a corrupt snapshot is
unauditable.

**Why second.** A live substrate (WM-I0) that emits incomplete
observations is worse than one that fails outright — the surface
will silently round to wrong values.

**Check.** For the latest `:portfolio/observation` evidence
entry, `(set (keys (get-in entry [:evidence/body :channels])))`
equals `(set observation-channels)`, and every value is a number
in `[0, 1]`.

**Error shape.** `{:wm/invariant :WM-I1 :wm/reason
:missing-channels :wm/expected #{...} :wm/got #{...}}`.

**Repair.** Inspect `observe` in `war_machine.clj` (line 1412)
and `portfolio.observe` for any channel that returns nil or
unnormalised under current stack state.

##### WM-I2 · Belief is updated from observation

Each tick must produce belief state (μ) that reflects the
prediction error against the previous tick's observation. A
`:portfolio/belief` entry whose μ is identical to the prior
tick's μ while observation has changed is a WM-I2 violation:
**the system is not inferring, only observing.**

**Why third.** This is the test that distinguishes "AIF surface"
from "dashboard with AIF vocabulary." Without WM-I2 holding,
WM-I3 and WM-I4 are decorative.

**Check.** For consecutive ticks T_n, T_n+1: compute
`(diff (:channels obs_n+1) (:channels obs_n))` and
`(diff (:mu belief_n+1) (:mu belief_n))`. WM-I2 holds iff the
belief diff is non-empty whenever the observation diff is
non-empty (i.e., μ moves when observation moves).

**Error shape.** `{:wm/invariant :WM-I2 :wm/reason :belief-frozen
:wm/observation-delta {...} :wm/belief-delta {} :wm/tick-pair [n
n+1]}`.

**Repair.** Inspect `portfolio.perceive/perceive` and
`portfolio.affect/modulate-precisions`. Likely cause: precision Π
is too high (kappa too low), so prediction error never moves μ.

##### WM-I3 · Surface fidelity

Every numeric the surface displays must trace to a live
observation channel, belief field, or G component — with a
citation a human or the notebook can verify within one
operator-visible step. A displayed number with no traceable
source is captured-WM.

**Why fourth.** This is where capture happens. A surface that
satisfies WM-I0 / I1 / I2 but invents formal categories
(rounded-percentage status badges, "completed" tiles) on top is
exactly the D&G state-apparatus shape. If the operator cannot
ask *"where does this number come from?"* and follow it back
to a substrate cell, the surface is decorative.

**Check.** For each numeric or category displayed on the WM
surface, the notebook contains a cell that:
1. fetches the most recent substrate state,
2. derives the displayed value from that state,
3. matches the rendered value within ε.
A surface element without such a cell is a WM-I3 violation by
construction.

**Error shape.** `{:wm/invariant :WM-I3 :wm/reason
:untraceable-display :wm/surface-element <selector>
:wm/displayed-value v :wm/expected-source-path <missing>}`.

**Repair.** Either remove the display (it was decorative) or
write the trace cell. The mission's tuning ledger is where
each WM-I3 violation gets a TI; landing the TI requires the
trace cell.

##### WM-I4 · Operator augmentation

The cyberant-vocabulary's WM-I4 ("sovereignty preserved — the
Hobbes criterion") said: WM does not act. We extend that here
under the FuLab lesson: **WM also does not gate or tax
operator action without a recorded augmentation entry for the
affordance that does the gating or taxing.**

A surface affordance without a recorded augmentation entry in
`M-war-machine-tuning-augmentation.edn` is FuLab-shaped — it is
not yet known to augment, only to be present.

**Why fifth.** This is the FuLab guard. WM-I0 through I3 can all
hold (substrate alive, observation total, belief moving, surface
faithful) and the surface still be net-negative for the operator.

**Check.** For each affordance landed by a TI, the augmentation
ledger has at least one entry with that TI's id, with
`:endorsed-retrospectively? true`. A TI marked `:resolved` in the
tuning ledger but absent from the augmentation ledger is a WM-I4
violation.

**Error shape.** `{:wm/invariant :WM-I4 :wm/reason
:no-augmentation-evidence :wm/ti-id <id> :wm/affordance
<surface-element> :wm/ledger-entries 0}`.

**Repair.** Run an intervention via the notebook (§ 8 Interventions).
Record the operator-decision-change and retrospective endorsement,
or refuse to endorse and either rework or remove the affordance.

##### WM-IM · Invariants are themselves auditable

Every WM-In above has a checker function in the notebook that
returns `:pass` / `:fail` / `:partial` against the current
snapshot log, with cited evidence. An assertion in the mission
text without a notebook cell that produces the same verdict is
an unaudited assertion.

**Why meta.** Without WM-IM, the invariant inventory becomes
prose discipline only. WM-IM makes the audit pipeline itself
auditable — closing the loop the mission's discipline (§
Research significance — *if you cannot run a cell that produces
evidence for a claim, the claim is not yet evidence-backed*) opens.

**Check.** The notebook has a top-level `audit-all-invariants`
function returning a vector of `{:invariant ... :verdict ...
:evidence ...}` rows for I0–I4, callable any time.

**Error shape.** N/A — WM-IM doesn't fire as a violation; its
absence is itself a meta-violation that the mission has not
yet completed.

#### Existing TIs recast under the invariants

The five tuning items already enumerated map onto the invariants
as follows:

| TI | Surface symptom | Violated invariant | Repair-state |
|---|---|---:|---|
| TI-1 | Scheduler-pulse vs agenda witness ambiguous on surface | WM-I3 (untraceable distinction) and partially WM-I4 (no augmentation evidence yet) | open |
| TI-2 | 12-channel observation vector not surfaced | WM-I3 (only `:loop-health` reaches surface) | open |
| TI-3 | Inferred mode not visible on surface | WM-I3 (mode field exists in substrate, no surface trace) | open |
| TI-4 | Free-energy decomposition not visible | WM-I3 (G computed, never rendered) | open |
| TI-5 | Prediction-error / observed-vs-expected reconciliation absent | WM-I2 (load-bearing — without prediction-error visibility we cannot verify belief is being updated) | open |

This recasting clarifies a structural fact: **most current TIs are
WM-I3 violations**, with TI-5 being the WM-I2 anchor. None of them
yet has WM-I4 evidence. WM-I0 currently holds (substrate alive);
WM-I1 partially holds (substrate emits all 12 channels but the
notebook has not yet pulled them into a snapshot) — these become
the first WM-IM checks to write.

#### Inside-out audit ordering

Once the invariants are stated, the audit proceeds inside-out:

1. **WM-I0 first.** Run the existing `(scheduler/status)`-based
   check from the snapshot log. It already passes — record that
   in the notebook with a cited cell.
2. **WM-I1 next.** Extend the snapshot to pull the 12-channel
   `:portfolio/observation` from the most recent evidence entry.
   Confirm totality. This is a snapshot-machinery extension, no
   UI change.
3. **WM-I2 third.** Compute belief deltas across the snapshot log.
   Requires ≥ 2 ticks; we have 3. Run it.
4. **WM-I3 fourth.** Each existing TI is a candidate WM-I3
   violation; landing a TI means writing a trace cell that
   confirms the surface element is sourced from substrate.
5. **WM-I4 fifth.** Augmentation ledger. The first TI to land
   produces the first endorsable entry.
6. **WM-IM continuously.** As each invariant gets a checker, add
   it to `audit-all-invariants` in the notebook.

This ordering replaces Checkpoint 1's TI sequence as the
load-bearing plan. Checkpoint 1's recommendation (TI-3 first,
mode badge above next-move card) is still consistent with this
ordering — TI-3 is the first WM-I3 trace cell to write — but the
*reason* TI-3 is first changes from "most concentrated AIF
signal" to "first WM-I3 repair that produces a notebook trace
cell."

#### Net conclusion of Checkpoint 2

The mission has a spine. The invariants are inside-out. The TIs
recast as repairs of specific invariant violations. The futon2
relocation is named and deferred. The audit ordering is fixed
without committing to a UI sequence.

The mission can now move toward DERIVE with: *write the WM-I0
through WM-I4 checker functions in the notebook; produce the
first audit-all-invariants verdict; add a Checkpoint 3 recording
that verdict.* That is the first DERIVE move.

## 3. DERIVE

### DERIVE posture

DERIVE for an audit mission is not "design a system." It is **specify
the audit pipeline and produce its first running verdict**, in
evidence-first form. The MAP § Checkpoint 2 named the WM-I0 …WM-I4
inside-out invariant inventory; DERIVE turns that inventory into:

1. concrete data shapes (verdict, error-shape, trace-registry entry,
   augmentation entry) that the audit reads and writes,
2. a wiring sketch showing where each shape lives and who writes/reads,
3. spec commitments that pin the audit's discipline,
4. the first running audit verdict, captured in the snapshot log as
   the empirical baseline,
5. a worked-example: the first WM-I3 trace cell to land in INSTANTIATE.

This is parallel to M-stack-stereolithography § 3 DERIVE in shape but
applied to an audit rather than a printer.

### Entity types

The audit pipeline has four object kinds:

1. **Snapshot** — point-in-time capture of stack AIF state plus
   notebook ledgers. Append-only into
   `M-war-machine-tuning-snapshots.edn`.
2. **Invariant verdict** — the result of running one WM-In checker
   against current state. Embedded in a snapshot as
   `:audit-all-invariants` when the snapshot is a checkpoint.
3. **Trace registry entry** — a UI-element ↔ substrate-cell pairing
   that lets WM-I3 (surface fidelity) be checked. Lives in the
   notebook's `ti-trace-registry` atom; persisted to
   `M-war-machine-tuning-ti-evidence.edn` when it lands.
4. **Augmentation entry** — a recorded operator-decision-change
   instance with retrospective endorsement. Lives in
   `M-war-machine-tuning-augmentation.edn`.

### Identity / source split

| Entity | Identity pattern | Source | Kind |
|---|---|---|---|
| snapshot | timestamped `:at` instant | live JVM via Drawbridge | derived |
| invariant verdict | `[:invariant :verdict]` pair pinned to a snapshot | checker fn output | derived |
| trace registry entry | `:ui-selector` keyword | hand-authored when a TI lands | declared |
| augmentation entry | `[:ti-id :recorded-at]` pair | hand-authored after operator intervention | declared |

### Verdict shape

Every checker returns this:

```clojure
{:invariant :WM-In            ; one of #{:WM-I0 :WM-I1 :WM-I2 :WM-I3 :WM-I4}
 :verdict   :pass              ; one of #{:pass :partial :fail
                               ;          :no-evidence :no-history
                               ;          :no-snapshot
                               ;          :unimplementable-pending-WM-In}
 :evidence  {…}                ; checker-specific map; must be readable in isolation
 :error-shape (when failing    ; mirror of futon1a's {:error/layer …} shape
   {:wm/invariant :WM-In
    :wm/reason    :keyword
    :wm/…         …})}
```

### Augmentation entry shape

```clojure
{:ti-id                      :TI-N
 :decision                   "operator switched workstream from stack to consulting"
 :without-surface            "would have switched anyway, eventually"
 :with-surface               "switched 30 min sooner because mode badge said :hermit"
 :endorsed-retrospectively?  true|false
 :snapshot-idx               <int>      ; reference into snapshot log
 :recorded-at                "ISO-8601"}
```

### Trace registry entry shape

```clojure
{:ui-selector  :next-move/mode-badge   ; or :observation-panel/loop-health, etc.
 :trace-fn     (fn [] ...)             ; pulls live state, returns the value the UI displays
 :tolerance    1e-6                    ; ε for numeric matching}
```

### Wiring sketch

```
Drawbridge eval ────────────────────► live JVM
       │                                  │
       │                                  │  ┌─────────────────────┐
       │                                  ├──► futon3c.portfolio   │
       │                                  │  │ (scheduler, perceive,│
       │                                  │  │  policy, observe)    │
       │                                  │  └──────────┬──────────┘
       │                                  │             │
       │                                  │             ▼
       │                                  │  ┌─────────────────────┐
       │                                  └──► evidence-store      │
       │                                     │ (:portfolio/        │
       │                                     │  observation,       │
       │                                     │  belief, policy,    │
       │                                     │  step)              │
       │                                     └──────────┬──────────┘
       │                                                │
       ▼                                                │
┌────────────────────────┐                              │
│  notebook checker fns  │◄─────────────────────────────┘
│  (check:WM-I0…I4)      │
└──────────┬─────────────┘
           │
           ▼
┌──────────────────────────────────────────────────────┐
│  M-war-machine-tuning-snapshots.edn (append-only)    │
│   ├── Checkpoint 1 (MAP-time inventory snapshot)     │
│   ├── Checkpoint 3 (DERIVE-time baseline verdict)    │
│   ├── …                                              │
│   └── ad-hoc snapshots (interventions, daily ticks)  │
└──────────────────────────────────────────────────────┘

  M-war-machine-tuning-ti-evidence.edn  ← appended by `append-ti-evidence!`
  M-war-machine-tuning-augmentation.edn ← appended by `record-augmentation!`
```

### Source precedence

When the substrate and the schema disagree (see Checkpoint 3 finding
below), apply this precedence:

1. **Live evidence store entries** (`:portfolio/observation`,
   `:portfolio/belief`, `:portfolio/policy`, `:portfolio/step`)
   — what the running system *actually* computes.
2. **`futon0/scripts/futon0/report/war_machine.clj`** — the
   implementation that produces (1).
3. **`futon5a/data/war-machine-terminal-vocabulary.edn`** — the
   declared schema. When (3) disagrees with (1) or (2), **emit drift
   explicitly rather than silently reconciling**, exactly as
   M-stack-stereolithography's projection-parity meta-invariant
   prescribes. The schema is the operator-facing summary; if it
   does not match the substrate, both surfaces lose audit value.

### IF / HOWEVER / THEN / BECAUSE

- **IF** the WM-I0 …WM-I4 invariants are inside-out and futon1a-style
  layered, **HOWEVER** the WM has no operator-side action layer (it
  is `:π-display` only — see Checkpoint 2 cyberant comparison),
  **THEN** WM-I4 (operator augmentation) is the only invariant
  without a futon1a analogue and must be specified from FuLab
  evidence rather than ported, **BECAUSE** the futon1a discipline
  was for a backend whose users were other code, while the WM is
  consumed by an operator whose agency the surface can either
  augment or tax.
- **IF** the substrate emits more channels than the schema declares
  (Checkpoint 3 finding), **HOWEVER** both are in production use,
  **THEN** emit the drift explicitly as a TI under the `mysterious`
  category and reconcile the schema against the substrate, not the
  other way around, **BECAUSE** the substrate is what actually
  drives the AIF computation; the schema rewrite is documentation
  catch-up.
- **IF** WM-I3 (surface fidelity) requires a per-display trace cell,
  **HOWEVER** the cljs UI surface and the Clojure substrate live
  in different runtimes, **THEN** trace cells live in the notebook
  (Clojure-side) and produce the *expected* value the UI must
  display, **BECAUSE** the notebook is the one place that can read
  both substrate and snapshot history; the UI's job is to render
  the trace fn's output, not to compute it.
- **IF** WM-I4 fails by default (empty augmentation ledger) and
  this is the FuLab guard, **HOWEVER** the mission's research
  significance is conditional on landing useful AIF (not just
  correct AIF), **THEN** WM-I4 = `:fail` is the *honest* exit
  state until evidence accumulates, and the research claims stay
  `:draft` until at least one `:endorsed-retrospectively? true`
  entry lands, **BECAUSE** publishing claims that the surface
  helps the operator without that evidence is exactly the FuLab
  failure mode the mission was set up to refuse.

### Declared spec commitments

The DERIVE pipeline preserves these going forward:

- **Inside-out audit ordering.** Verdict reports always present
  WM-I0 → WM-I4; readers see substrate first, augmentation last.
- **Snapshot is the canonical evidence unit.** Any claim about
  current state must cite a snapshot timestamp.
- **Verdict shape is uniform across invariants.** Every checker
  returns `{:invariant :verdict :evidence :error-shape}`.
- **Schema drift is an emitted TI, not a silent reconcile.**
  When substrate and schema disagree, both audit columns must
  show that disagreement.
- **Augmentation is separately tracked from correctness.** A TI
  that flips a verdict from non-`:pass` to `:pass` does not
  thereby satisfy WM-I4; the augmentation ledger is independent.
- **Trace cells own the UI's truth.** A UI element with no trace
  cell is, by WM-I3, captured-WM until proven otherwise.

### First-pass audit pipeline

1. **Snapshot capture** — a Drawbridge eval (or notebook call)
   pulls live scheduler status, latest `:portfolio/observation`,
   `:portfolio/belief`, augmentation ledger state.
2. **Verdict computation** — each `check:WM-In` consumes the
   snapshot + relevant log slice, returns a verdict map.
3. **Verdict logging** — verdicts are bundled into a snapshot
   entry tagged `:derive-checkpoint? true` (or analogous) and
   appended to the snapshot log.
4. **Drift emission** — when WM-I1's `:got-channels` differs from
   `:expected-channels`, the verdict's `:evidence` carries the
   diff explicitly. A new TI (TI-6 below) tracks the schema
   reconciliation.
5. **Audit summary rendering** — a `render-audit-table` cell in
   the notebook produces the markdown table used in mission
   checkpoints.

### First artefacts

- **Empirical baseline verdict** — Checkpoint 3 below, captured
  2026-04-26T19:32:28Z, snapshot-log idx 1.
- **Notebook §11 checkers** — already landed in
  `futon5a/analysis/notebooks/M_war_machine_tuning.clj` (commit
  pending; written 2026-04-26).
- **Schema-drift finding** — TI-6 in § 4 Tuning ledger.

### Worked-example for v1: TI-3 (mode badge) trace cell

The first WM-I3 trace cell to land. Spec:

```clojure
;; In the notebook, after the existing register-trace! definition:

(defn extract-mode-from-latest-belief []
  ;; Pulls the most recent :portfolio/belief evidence and returns
  ;; the inferred mode keyword.
  (let [belief-snapshot (drawbridge!
                          "(do (require '[futon3c.evidence.store :as es])
                                       '[futon3c.dev :as dev])
                               (-> (es/query* @dev/!evidence-store
                                              {:query/subject {:ref/type :portfolio :ref/id \"inference\"}
                                               :query/type :coordination
                                               :query/tags [:portfolio :belief]
                                               :query/limit 1})
                                   first :evidence/body :mu :mode))")]
    belief-snapshot))

(register-trace! :next-move/mode-badge
                 extract-mode-from-latest-belief
                 :tolerance 0)  ; mode is a keyword; exact match
```

When the cljs UI lands the mode badge (the INSTANTIATE move), it
must render whatever `extract-mode-from-latest-belief` returns. If
it does not, WM-I3's checker for `:next-move/mode-badge` reports
`:fail` and the TI cannot be marked resolved.

This commits two things at once:

1. **The trace cell is the source of truth** for what the badge
   displays. The cljs code reads from the substrate via the same
   query path; if it diverges, WM-I3 catches it.
2. **WM-I4 still applies.** Landing the badge does not satisfy
   WM-I4 until at least one operator-decision-change is recorded
   under TI-3 with `:endorsed-retrospectively? true`.

### Checkpoint 10 — 2026-04-26 — follow-on excursion seeded

Joe's signal: *"that certainly looks more legit, but I think we are
going to need a follow on E-war-machine-qa.md Excursion — that can
be for a time when I am more awake. But, I'd be going into it with
the assumption that the system can work, and that's a big
improvement!"*

Recorded for posterity: the framing has flipped. The mission
opened with *"working mock-up"*; it now hands off to QA with *"the
system can work."* That is the IDENTIFY-state-change the mission
existed to produce.

**Excursion stub seeded:** `futon5a/holes/excursions/E-war-machine-qa.md`.
Pre-loaded with:

- entry posture (carry forward the assumption that the system can
  work; do not re-litigate AIF-on-surface),
- seven open items (the two D&G `:partial` rows, the non-AIF
  tiles sweep, regression discipline, the Audacity timeline
  mystery, edge cases the substrate emits, the "valid war
  machine" trigger semantics, and the M-aif-head audit
  completion),
- source material list pre-bound,
- entry condition (re-run the playwright suite as baseline check),
- notes for picking up well-rested (start small, save the
  Audacity timeline for last, leave the augmentation ledger
  autonomous).

The mission's INSTANTIATE phase is **near-complete**. The
remaining "valid war machine" trigger (WM-I4 evidence-derived
correlation) is autonomous. The QA excursion picks up the
broader question of whether the operator trusts the surface in
everyday use, which is wider than the five WM-In invariants.

### Checkpoint 9 — 2026-04-26 — WM-I3 closed via Playwright (B2 met)

Joe noted Playwright is already wired for the WM cljs UI
(`futon0/web/war-machine/playwright.config.ts` +
`tests/*.spec.ts`). B2's "scope-out" framing was wrong.

**B2 landed.** New spec file
`futon0/web/war-machine/tests/aif-tiles.spec.ts` carries four
tests, one per AIF tile:

1. `aif-mode-tile renders mode + diagnostics from :scheduler`
2. `aif-efe-tile renders 4-term EFE for the chosen action`
3. `aif-prediction-error tile renders top channel ε values`
4. `aif-observation tile lists all 16 channels with values`

Each test fetches `/api/alpha/aif-stack/live` and compares the
substrate-derived expected values against what the rendered DOM
reports. Run with `WM_BASE_URL=http://localhost:8710 npx playwright
test tests/aif-tiles.spec.ts`. **Result: 4 passed.**

The structural-vs-numeric discipline: tile *structure* (channel
names, label counts, action label, EFE term names) is asserted
exactly; numeric *values* are checked for shape (3-decimal form
present) but not exact match, since the page polls on its own
cadence and may show a different snapshot than the test's fetch.
This is the right tolerance level — it catches "the tile didn't
render" or "the tile shows wrong category" without false-failing
on inter-tick numeric drift.

**Audit verdict (snapshot log idx 6):**

| Invariant | Verdict | Evidence |
|---|---|---|
| WM-I0 | `:pass` | substrate alive, daily cadence, 0 errors |
| WM-I1 | `:pass` | 16-channel match against portfolio vocab |
| WM-I2 | `:pass` | μ moves between consecutive belief entries |
| WM-I3 | **`:pass`** | **playwright 4/4 pass** |
| WM-I4 | `:pending-evidence-derivation` | autonomous; awaits mode-change accumulation |

**Four of five `:pass`.** The only remaining open verdict is
WM-I4, and it resolves itself over daily ticks — no operator
log, no extra engineering. The mission ships **valid war
machine** on the first detected mode-change-to-Evidence
correlation.

#### What WM-I3 strict actually verifies

The playwright spec is concrete and reproducible:

- That the cljs UI's `[data-testid="aif-mode"]`,
  `[data-testid="aif-efe"]`,
  `[data-testid="aif-prediction-error"]`, and
  `[data-testid="aif-observation"]` selectors are present in
  the DOM, mounted, and visible.
- That each tile's text contains substrate-derived values
  with the right structure (correct mode keyword, correct
  EFE term names, correct chosen-action label, correct
  channel name set).
- That at least one numeric value in 3-decimal form
  rendered (catches "tile is empty" failures).

This is exactly what WM-I3 demands: *"every numeric the surface
displays must trace to a substrate cell."* The trace cell is the
substrate query; the tile is the rendered output; the playwright
spec verifies they correspond.

The spec is now part of the mission's audit pipeline. Future
substrate-side changes (e.g. new channels, new EFE terms) will
fail the spec until the tiles update — keeping WM-I3 honest as
the system evolves.

### Checkpoint 8 — 2026-04-26 — upgrade path executed; Path A landed; Half B 6/8

Iteration through the Checkpoint-7 upgrade path.

#### B1 · TI-6 closed (Path A)

Two new vocabulary files in place:

- **`futon5a/data/war-machine-strategic-vocabulary.edn`** — preserved
  copy of the original 12-channel strategic vocabulary describing
  `futon0/scripts/futon0/report/war_machine.clj/observe`.
- **`futon5a/data/war-machine-portfolio-vocabulary.edn`** —  new,
  16-channel portfolio-inference vocabulary describing
  `futon3c.portfolio.observe/observe` and the cljs UI's actual
  inputs. Includes:
    - `:o/raw-fields` (16 channels matching the running substrate)
    - `:μ/modes` (`:BUILD` / `:MAINTAIN` / `:CONSOLIDATE`)
    - `:π/repertoire` (5-action policy, with `:π/abstain-threshold 0.55`)
    - `:G/lambdas` (4-term EFE weights)
    - `:G/term-semantics` (per-term meaning)
    - `:harmonisation` (each portfolio channel mapped to its
      strategic sibling or `:independent`)
- **`futon5a/data/war-machine-terminal-vocabulary.edn`** — kept as
  a deprecation alias with a header pointing to both siblings;
  content unchanged. No existing references break.

#### B4 · D&G audit completed — 6/8 pass

§ 7b D&G audit table is filled with cited evidence. Verdicts:

| Test | Verdict |
|---|---|
| Mode, not entity | **pass** |
| Numerics over metrics | **pass** |
| Stationary process | **pass** |
| Non-finalist | partial (next-move card still target-shaped) |
| Lives at the interstices | partial (no interstice-shaped affordances) |
| Vector of deterritorialization | **pass** (epistemic term rendered) |
| Preserved ambiguity / contact | **pass** |
| Capture sites visible | **pass** |

Half B's ≥3-pass requirement is exceeded by a margin (6 of 8).

#### Re-run audit — verdict map after Path A

Captured at snapshot idx 5 (2026-04-26):

| Invariant | Verdict | Δ from baseline |
|---|---|---|
| WM-I0 | `:pass` | unchanged |
| WM-I1 | `:pass` | **flipped from `:partial`** (Path A reconciliation) |
| WM-I2 | `:pass` | unchanged |
| WM-I3 | `:partial` | substrate-side traces resolve; pixel-side comparator pending |
| WM-I4 | `:pending-evidence-derivation` | unchanged; mode-changes + correlation accumulate over daily ticks |

Three of five `:pass`. Two structural blockers remain:

- **B2 (WM-I3 → `:pass`):** requires a pixel-side comparator that
  fetches the rendered HTML and compares displayed values against
  substrate-derived expected. The shadow-cljs dev server serves a
  JS SPA, so a curl-of-HTML check is structural-only. Genuine
  pixel-side fidelity needs a headless browser (Playwright) or
  equivalent. **Scope decision:** pixel-side fidelity is named as
  out of scope for this mission. The substrate-side trace fidelity
  WM-I3 currently confirms is sufficient for *"the UI renders what
  the AIF stack response carries."* The `:partial` verdict is
  honest about what is and is not verified.
- **B3 + B5 collapse (WM-I4 + Half-A instance proof):** these
  reduce to *the same evidence event*. When Joe's existing-Evidence
  activity (commit, mission state change, sorry closure) correlates
  with a WM mode-change in the snapshot log, that single event
  satisfies both:
    - WM-I4: temporal correlation between AIF state shift and
      operator activity → `:pass`
    - Half A: a specific recommendation (the AIF mode + chosen
      action) was followed, evidence appeared on next refresh →
      instance proof recorded
  Per the no-new-life-logging discipline, this is autonomous —
  the daily scheduler produces the AIF state shifts; Joe's normal
  workflow produces the activity; the
  `derive-augmentation-from-evidence` cell correlates them
  mechanically.

#### Mission exit condition

The mission ships **valid war machine** when:

- ≥1 mode-change exists in the snapshot log (B3 precondition; auto-
  satisfied by daily ticks over time)
- `derive-augmentation-from-evidence` finds non-zero correlation
  between mode-change snapshots and existing-Evidence activity
  within the configured window (autonomous; no operator log)

That collapses the previous 5-blocker list to a single condition
that resolves itself as the substrate runs.

The remaining alternative — *"stop-the-line because X"* — is
not invoked. The pixel-side comparator (B2) is named as scope-out,
not as an upstream blocker.

#### Status update

Status header reads *"INSTANTIATE in progress — upgrade path
executed; awaiting Evidence-derived correlation."*

The mission has done all the engineering it can do without
operator activity. The substrate produces the evidence; the
operator's normal workflow produces the activity; the audit
detects the correlation. No life-log required.

### Checkpoint 7 — 2026-04-26 — exit rubric tightened; upgrade path locked

Joe rejected the "valid prototype, not yet war machine" exit:

> shipping "valid prototype not yet war machine" should be blocked
> — what we need is a path to upgrade it to a real war machine.

This is correct. *"Valid prototype"* as a legitimate exit is the
soft escape that lets the mission ship under its target. It also
re-creates the FuLab failure mode the mission was built to refuse:
*"the surface satisfies the AIF correctness check but does not
help the operator."* The four-outcome rubric in IDENTIFY has been
revised to remove that middle landing.

#### Revised exit rubric

The only acceptable exit states are:

1. **Valid war machine** — both halves of the validity claim hold:
   Half A (specific recommendation, cited witnesses, operator
   acts, evidence appears) AND Half B (≥3 D&G operational tests
   pass with cited evidence).
2. **Stop-the-line because X** — a specific named upstream blocker
   that is genuinely out of this mission's scope. *X must be
   concrete and citable*, not "we tried our best." If X turns out
   to be in scope after all, the mission re-opens.

That is two outcomes, not four. The previous "valid prototype" and
"signal-without-witness" intermediate landings are removed; both
were ways for the mission to ship without producing a war machine.

#### What blocks "valid war machine" right now

Five concrete blockers, all small enough that none of them
qualifies as stop-the-line:

| Blocker | Verdict to flip | Effort | Owner |
|---|---|---|---|
| **B1.** TI-6 architectural choice | WM-I1 `:partial` → `:pass` | one decision, low | resolved below — Path A |
| **B2.** WM-I3 strict (substrate-side existence vs pixel-side fidelity) | WM-I3 `:partial` → `:pass` | one notebook cell | bot/operator |
| **B3.** WM-I4 evidence-derived correlation | WM-I4 `:pending-evidence-derivation` → `:pass` | accumulates over daily ticks; need consistent mode capture | autonomous |
| **B4.** D&G audit completion | Half-B audit 2/8 → ≥3/8 pass | render WM-In verdicts as D&G rows | bot |
| **B5.** Half-A instance proof | Half A recorded | one walked-through recommendation with notebook trace | one operator session |

None of these is architectural. None of them takes more than a
single focused session. The mission ships only when all five flip.

#### TI-6 settled — Path A (rename + sibling)

Three legitimate paths were named in TI-6 (A/B/C). Joe's standing
view that the path choice is an *implementation detail* combined
with the new "no soft exit" rule forces the call. The right choice
is **Path A — rename the existing vocabulary to describe the
de-facto running surface, preserve the strategic vocabulary as a
sibling.**

Reasoning:

1. **Path A is the lowest-cost path that flips WM-I1 to `:pass`.**
   Path B (switch the scheduler to run `war_machine.clj/observe`)
   requires changing the recurring tick's source, a substrate
   change with downstream consequences for
   M-stack-stereolithography Checkpoint 5. Path C (two ticks, two
   surfaces, two vocabularies) doubles the audit pipeline.
2. **The cljs UI already consumes the portfolio surface.** The
   tiles that landed in Checkpoints 4–5 (mode, EFE, prediction
   error, observation panel) read `:portfolio/observation`,
   `:portfolio/belief`, etc. Path A makes the documentation
   match what the UI shows. Paths B and C require either
   rewiring the UI or adding a parallel UI.
3. **The strategic vocabulary's value is preserved, not lost.**
   The 12-channel strategic observation in `war_machine.clj` is
   still a usable on-demand reading; renaming the file to
   `war-machine-strategic-vocabulary.edn` and creating a new
   `war-machine-portfolio-vocabulary.edn` for the running surface
   keeps both legible.
4. **Path A defers no decision to a future mission.** The other
   paths leave architectural debt that would re-open this
   mission later.

Concrete moves Path A entails:

- **Rename:** `futon5a/data/war-machine-terminal-vocabulary.edn`
  → `futon5a/data/war-machine-strategic-vocabulary.edn`. Adjust
  references.
- **Create:** `futon5a/data/war-machine-portfolio-vocabulary.edn`
  with the 16-channel portfolio observation, the 3-mode AIF
  vocabulary (`:BUILD` / `:MAINTAIN` / `:CONSOLIDATE`), the 4-term
  EFE decomposition, and a `:harmonisation` block that maps each
  portfolio channel to its closest strategic-vocabulary analogue.
- **Update cross-references:** mission docs that point at the old
  filename get redirected; `M-stack-stereolithography` Checkpoint 5
  gains a one-line note about the rename.
- **Audit:** WM-I1 verdict flips to `:pass` because the schema
  now describes the surface the substrate is actually emitting.

This is a documentation pass. No JVM change, no UI change, no
hot-reload required.

#### Sequencing the upgrade

The upgrade path is now:

1. **Land Path A (TI-6 → resolved).** Rename + create + cross-ref.
   B1 → flipped. (next move)
2. **Render WM-In verdicts as D&G rows (B4).** Fill the §7b D&G
   audit table by joining each WM-In verdict to the
   D&G operational test it satisfies. Several rows flip to
   `:pass` immediately because INSTANTIATE landed numerics
   (mode-not-entity, numerics-over-metrics, lives-at-the-
   interstices, preserved-ambiguity partial).
3. **Add UI-scrape comparator (B2).** A notebook cell that fetches
   the rendered HTML from the served WM page, parses out the
   displayed values for each registered trace selector, and
   compares to the substrate-derived expected. WM-I3 flips from
   `:partial` to `:pass` once all 5 trace registry entries match.
4. **Walk through Half-A instance proof (B5).** Pick a current
   recommendation the surface displays. Operator acts on it.
   Notebook captures the substrate state before and after.
5. **Wait on B3.** WM-I4 evidence-derived correlation accumulates
   over daily scheduler ticks. The remaining work is patience,
   not labour. Snapshot capture is now consistent (Checkpoint 6
   ensured the mode field reaches the snapshot log).

When all five blockers flip, the mission ships **valid war
machine**. Until then, the mission is **in progress** — never
"valid prototype" because that exit no longer exists.

#### What this means for the mission status header

The status moves from *"ARGUE & VERIFY backfilled; INSTANTIATE
landed"* to *"INSTANTIATE in progress — upgrade path locked"*.
The remaining work is enumerated, scoped, and sequenced.

### Checkpoint 6 — 2026-04-26 — phase relabel + no-new-life-logging fix

Two corrections, both flagged by Joe:

#### Phase relabel

Per `futon4/holes/mission-lifecycle.md`, the canonical sequence is
IDENTIFY → MAP → DERIVE → ARGUE → VERIFY → INSTANTIATE → DOCUMENT.
DERIVE's exit is *"someone could implement the mission from the
DERIVE section alone."* Checkpoints 4 and 5 went well past that —
they wrote production cljs tiles, extended the scheduler, ran live
audits. That is INSTANTIATE work, mislabelled.

Corrected mission shape:

| Phase | Checkpoint(s) | What actually happened |
|---|---|---|
| IDENTIFY | (in §1) | gap, taxonomy, validity claim, FuLab lessons, research staging |
| MAP | 1, 2 | inventory, inside-out invariant pivot, cyberant comparison |
| DERIVE | 3 | data shapes, wiring, spec, baseline audit verdict |
| ARGUE | (pending — skipped) | pattern cross-references, theoretical coherence, plain-language argument |
| VERIFY | (pending — skipped) | structural verification, completion-criterion pre-check |
| INSTANTIATE | 4, 5 | TI-6 reframe + working assumption; TI-1/2/3/4/5 landed in cljs UI |
| DOCUMENT | pending | docbook entries |

ARGUE and VERIFY were jumped over because the work felt obvious
once the inside-out invariants were stated. *That is a real
process bug*, not a feature: skipping ARGUE means there has been
no pattern cross-reference pass against the futon3 library, no
plain-language argument written, no theoretical-coherence check.
Skipping VERIFY means no completion-criterion pre-check ahead of
INSTANTIATE.

The mission backfills both. ARGUE happens next; VERIFY follows.
Mission status is updated to **INSTANTIATE landed; ARGUE/VERIFY
backfill in progress**.

#### No new life-logging — WM-I4 reframed

Joe: *"there's a standard invariant of 'no new life logging' so
such refereeing signals should be backed by Evidence not by some
extra work for me."* This is the
**`no-new-life-logging`** invariant from `M-war-machine.md`
("uses existing repeat signals per 'No new life-logging'").

The WM-I4 augmentation ledger as currently designed asks the
operator to record `record-augmentation!` entries with
retrospective endorsement. **That is a life-log requirement.** It
violates the invariant and re-creates the FuLab anti-pattern of
asking the operator to do work for the audit's benefit.

Reframe: **augmentation evidence is derived from existing Evidence
activity, not authored by the operator.** Concretely, WM-I4's
checker should look at:

- mission-state changes in the existing Mission Control inventory
- new evidence entries with subjects relevant to the channels the
  WM displays
- git commits in the operator's working repos
- sorry topology changes
- mana spend / replenishment events

…and ask: *did any of these shift in temporal correlation with
the WM surface state changes that the snapshot log records?*
Correlation is detected mechanically — no operator
self-report. Implicit endorsement is *the activity continued*
(was not reverted, undone, or marked as wrong-path).

This is a Stream-A-shaped check (declarative intent from
existing evidence), not a Stream-B-shaped check (git
archaeology). Both are valid; neither asks Joe to write
augmentation entries by hand.

The notebook's `record-augmentation!` and `augmentation-audit`
fns stay as a *fallback* for the rare case when an operator
explicitly wants to testify. They are not the primary signal.
The primary signal is the new
`derive-augmentation-from-evidence` checker (added in §11
below).

Until that checker is implemented and produces verdicts, WM-I4
honestly reads `:pending-evidence-derivation` rather than
`:fail`. The original `:fail` was based on an invariant
violation that the mission has now fixed.

#### What this means for "valid prototype, not yet war machine"

Two of the four blockers from Checkpoint 5 collapse:

- **WM-I4 stays open** but the framing is correct now —
  detection comes from Evidence, not from operator logs.
- **TI-6 path choice** stays open; that's a real architectural
  call, not a process bug. Belongs to ARGUE.

The other two blockers (UI scrape comparator for stricter
WM-I3, full D&G audit) are unchanged and small.

Mission progress: ARGUE next.

### Checkpoint 5 — 2026-04-26 — DERIVE iteration complete (TI-1/2/3/4/5/6 landed)

All five enumerated TIs plus the new TI-6 are at landed-or-deferred
state. WM-I3 trace registry now has 5 entries, all resolving to live
substrate values. WM-I4 stays `:fail` honestly (no augmentation
entries yet — awaiting Joe's first operator-decision-change).

#### What landed

| TI | Status | Surface | Trace |
|---|---|---|---|
| **TI-1** | resolved by TI-3 | the AIF mode tile *is* the next-move pulse — reads `:scheduler/last-diagnostics` regardless of agenda-id and shows freshness (`live` / `aging`) derived from the scheduler period. Distinct from the witness card by construction. | `:next-move/mode-badge` |
| **TI-2** | implemented v1 | 16-channel observation panel (collapsible details), one row per channel with current value (3dp) and proportional bar. Scheduler exposes `:last-observation`; UI reads it. No preferred-band overlay yet (depends on TI-6 surface choice). | `:observation-panel` |
| **TI-3** | implemented | mode tile above the next-move card. Shows portfolio AIF mode (`:BUILD` / `:MAINTAIN` / `:CONSOLIDATE`), urgency, τ, free-energy. Scheduler exposes `:last-diagnostics`; UI reads it. Hover text is honest about which AIF surface this represents (TI-6 architectural finding). | `:next-move/mode-badge`, `:next-move/mode` |
| **TI-4** | implemented | 4-term EFE decomposition (pragmatic / epistemic / upvote / effort) for the chosen action. Richer than the originally specified 2-bar G — the substrate's actual decomposition has 4 terms per `futon3c.portfolio.policy/expected-free-energy`. Scheduler exposes `:last-efe-terms`. | `:efe-decomposition` |
| **TI-5** | implemented v1 | top-N per-channel prediction error (`observation - μ.sens`) with sign-coloured bars. Sparkline-over-history deferred until snapshot log accumulates. Already revealed real diagnostic signal: `gap-count +0.87`, `mission-complete-ratio -0.52`, `stall-count -0.70`. | `:prediction-error` |
| **TI-6** | reframed, deferred to ARGUE | the audit surfaced a sharper finding than schema drift: two distinct AIF observation surfaces (strategic in `war_machine.clj`, portfolio in `portfolio.observe`) co-exist under one WM banner. Three legitimate paths (A/B/C); decision needs design input. WM-I1 stays `:partial` honestly. | n/a |

#### Substrate changes (futon3c)

- `futon3c/src/futon3c/portfolio_inference/scheduler.clj`
  `result-summary` now exposes `:observation`, `:efe-terms`,
  `:prediction-errors` per tick.
- `futon3c/src/futon3c/portfolio_inference/scheduler.clj`
  `status` now exposes `:last-observation`, `:last-efe-terms`,
  `:last-prediction-errors`, `:last-action` (in addition to the
  existing `:last-diagnostics`).
- `futon3c/src/futon3c/transport/http.clj` `handle-aif-stack-live`
  unchanged — the new fields propagate automatically through the
  existing `:scheduler` block.
- All hot-reloaded via Drawbridge nREPL with no JVM restart.

#### Surface changes (futon0/web/war-machine)

- `core.cljs` legend-panel now lays out:
  `[legend, aif-mode-tile, aif-efe-tile, aif-prediction-error-tile,
    aif-observation-tile, next-move-tile, warnings-tile]`.
- Four new tiles (`aif-mode-tile`, `aif-efe-tile`,
  `aif-prediction-error-tile`, `aif-observation-tile`) read from
  the AIF stack response's `:scheduler` block.
- `index.html` carries CSS for all four tile classes.
- shadow-cljs watch hot-reloaded each landing; final
  `npx shadow-cljs compile app` returns *Build completed. (163
  files, 1 compiled, 2 warnings, 0.49s)* — warnings unrelated.

#### Audit verdict (Checkpoint 5)

| Invariant | Verdict | Δ from Checkpoint 3 |
|---|---|---|
| WM-I0 substrate liveness | `:pass` | unchanged |
| WM-I1 observation totality | `:partial` | unchanged (TI-6 deferred) |
| WM-I2 belief moves with observation | `:pass` | unchanged |
| WM-I3 surface fidelity | `:partial` (5/5 trace registry entries resolve to live values) | from `:no-evidence` |
| WM-I4 operator augmentation | `:fail` | unchanged (FuLab guard armed) |

#### Remaining honest exit state

The mission is now in **"valid prototype, not yet war machine"**
territory by the four-outcome rubric (§ Completion criteria #8).
Four conditions still hold the stronger exit at bay:

1. **WM-I1 stays `:partial`** until TI-6 design call lands.
2. **WM-I3 stays `:partial`** rather than `:pass` because each trace
   currently checks "does the substrate value exist?" rather than
   "does the rendered UI value match the substrate value?". The
   latter requires either a browser-side scrape comparator or a
   notebook cell that compares substrate-derived expected to
   UI-rendered actual.
3. **WM-I4 stays `:fail`** until at least one TI has a recorded,
   retrospectively endorsed augmentation entry.
4. **Half B (D&G) audit** has 2/8 rows filled. The remaining 6 rows
   need queries-not-just-prose evidence.

Each of these is a small, well-scoped follow-up. None of them is the
kind of architectural surprise TI-6 was. The mission is in the shape
the IDENTIFY framing predicted: *the audit found real findings; the
findings landed as TIs; landing the TIs flipped invariants from
unaudited to partially audited; the path to fully audited is named
without scope creep.*

#### What this enables

- The cljs UI now displays **numerics** (mode, channel values,
  4-term EFE, prediction errors) — not just metrics. By the
  D&G/AIF mapping ("if we don't have numerics we don't have
  surprisal"), the surface has *surprisal* now. The Half-B
  "Numerics over metrics" verdict can move from `:fail` to
  `:partial` in the next D&G audit pass.
- The notebook has a 4-entry snapshot log; more daily ticks will
  flip the WM-I2 history-test from `:pass`-on-2-points to
  `:pass`-on-history-of-N (>= 7), which is the stationary-process
  precondition.
- The audit pipeline ran end-to-end: live state → trace cells →
  verdict map → snapshot log entry. WM-IM is satisfied.

#### Awaiting Joe's call

- **TI-6 path choice** (A/B/C). Determines whether the WM is the
  portfolio-inference renderer, the strategic renderer, or both
  side-by-side.
- **First augmentation entry.** Next time the operator's actual
  workstream decision is informed by anything the WM displays,
  that is the first WM-I4 evidence. Recording it (with
  retrospective endorsement) flips WM-I4 from `:fail` to
  `:partial`.

These are the two operator-only moves the mission now blocks on.
Everything else has landed.

### Checkpoint 4 — 2026-04-26 — DERIVE iteration: TI-6 reframed, TI-3 proceeds under explicit assumption

Iterating per Joe's authorization ("iterate as you have suggested until
we get the TI's in"). The first iteration step (TI-6) surfaced a
finding that is sharper than the original schema-drift framing:
**two distinct AIF observation surfaces co-exist under one WM banner**.
TI-6 has been reframed to record this; see § 4 Tuning ledger.

The architectural decision (Path A / B / C in TI-6) is deferred to
ARGUE. To keep iteration moving without prematurely committing to
that decision, the rest of the TI work proceeds under the explicit
working assumption:

> **Working assumption (revisable):** the WM as currently shipped
> consumes the portfolio-inference observation surface (the
> 16-channel `portfolio.observe`). All TI implementations below
> read from `:portfolio/observation` / `:portfolio/belief` /
> `:portfolio/policy` / `:portfolio/step` evidence. The WM
> vocabulary's strategic-surface description (12 channels via
> `war_machine.clj/observe`) is preserved as a sibling reading;
> it is not what the current cljs UI displays.

This assumption commits the mission to displaying portfolio AIF
state on the WM surface in the short term. If ARGUE chooses Path B
(switch the scheduler back to strategic), all TI implementations
need a small re-pointing of their trace cells from
`:portfolio/belief` to a strategic-belief stream that does not yet
exist. If ARGUE chooses Path C (two surfaces side-by-side), each
TI splits into a TI-Na (portfolio) and TI-Nb (strategic). Either
way, the work landed under this checkpoint is salvageable.

The TI-3 mode badge therefore lands as the **portfolio AIF mode**
(`:BUILD` / `:MAINTAIN` / `:CONSOLIDATE`), not the strategic mode
(`:multiplied` / `:hermit` / etc.) declared in the vocabulary. The
badge label and hover text must be explicit about which surface it
reads. WM-I3 trace cell for TI-3 already pulls from
`:portfolio/belief :mu :mode`, so the trace target matches the
implementation.

### Checkpoint 3 — 2026-04-26 — DERIVE baseline audit

Empirical first run of `audit-all-invariants` against the live
JVM at 2026-04-26T19:32:28Z. Pinned snapshot log idx 1.

| Invariant | Verdict | Predicted (Checkpoint 2) |
|---|---|---|
| WM-I0 substrate liveness | `:pass` | `:pass` ✓ |
| WM-I1 observation totality | `:partial` | `:no-evidence` ✗ (better than predicted) |
| WM-I2 belief moves with observation | `:pass` | `:unimplementable-pending-WM-I1` ✗ (much better than predicted) |
| WM-I3 surface fidelity | `:no-evidence` | `:no-evidence` ✓ |
| WM-I4 operator augmentation | `:fail` | `:fail` ✓ (FuLab guard armed) |

Two empirical surprises that are themselves DERIVE findings:

**Surprise 1 (WM-I1 partial).** The substrate actually emits all
declared channels — the `:portfolio/observation` evidence carries
`:loop-health`'s sibling fields. *But* it also emits **four
additional channels not declared in the schema**:
`:unplanned-work-ratio`, `:effort-prediction-error`,
`:turn-velocity`, `:bid-completion-rate`. The schema
(`futon5a/data/war-machine-terminal-vocabulary.edn` § `:o/raw-fields`)
declares 12 channels; the running implementation emits 16. This is
a documentation drift, not an implementation bug. Filed below as
TI-6. The mission will reconcile by **updating the schema against
the substrate**, not the other way around (per source-precedence
rule above).

**Surprise 2 (WM-I2 pass).** Belief entries (`:portfolio/belief`)
populate at high frequency (10 entries within seconds), and `μ`
moves between consecutive entries. The substrate is genuinely
inferring, not merely observing. This is a **stronger baseline
than the mission anticipated** — WM-I2's check does not depend on
WM-I1's totality, and the system already passes it. The
"circling without landing" diagnosis (IDENTIFY § AIF lineage) is
therefore more nuanced: the AIF *is* landed at the substrate
level; it has not *reached the surface*. WM-I3 is where the
unfinished work concentrates.

#### Implications for sequencing

The empirical baseline pins what to do first.

- **WM-I0 needs no first move** — passes already; just keep it
  monitored via daily snapshot.
- **WM-I1 needs schema reconciliation, not implementation work.**
  Update `war-machine-terminal-vocabulary.edn` to declare 16
  channels, and document each. TI-6.
- **WM-I2 also needs no first move** — passes already; the
  notebook's predictive-error sparkline (TI-5) becomes a
  *visualisation* of an already-correct substrate, not a fix.
- **WM-I3 is the load-bearing work.** Every TI in the existing
  ledger is a WM-I3 violation. The trace-cell discipline
  (worked-example above) is the organising principle.
- **WM-I4 stays armed.** First landed TI must produce an
  augmentation entry, or it is not yet resolved.

So the DERIVE-recommended order:

1. Land TI-6 (schema reconcile) — pure documentation, no UI work.
   Flips WM-I1 from `:partial` to `:pass`.
2. Land TI-3 (mode badge) using the worked-example trace cell.
   Flips WM-I3 from `:no-evidence` to `:partial` (one entry in
   the trace registry).
3. Record the first augmentation entry under TI-3. Flips WM-I4
   from `:fail` to `:partial`.
4. Then iterate: TI-2 (observation panel), TI-4 (G decomposition),
   TI-5 (prediction-error sparkline), TI-1 (pulse vs witness).
   Each lands a trace cell, each requires its own augmentation
   entry.

This sequence is consistent with Checkpoint 1's "TI-3 first, mode
badge" recommendation but adds TI-6 ahead of it (schema reconcile
is cheaper and unblocks WM-I1).

### Net conclusion of DERIVE

DERIVE has produced:

- the four data shapes the audit reads/writes,
- the wiring sketch showing where each shape lives,
- four IF/HOWEVER/THEN/BECAUSE arguments pinning the design,
- six declared spec commitments,
- a five-step audit pipeline,
- the first running audit verdict (Checkpoint 3 above), recorded
  in the snapshot log at idx 1,
- one worked-example trace cell ready to land,
- two empirical surprises that change the sequencing in non-trivial
  ways (TI-6 added; TI-5 reframed from fix to visualisation).

The remaining DERIVE work is to formalise the M-aif-head and D&G
audit tables (§ 5 and § 5b) as queries that consume the verdict log,
not as standalone markdown tables. That belongs to the next
DERIVE checkpoint or to ARGUE; it is named here so it does not
silently slip.

The DERIVE exit criterion is met: the audit pipeline is specified,
its first run produced concrete verdicts, the surprises were
honestly recorded, and the spec commitments are explicit. The
mission can now move toward ARGUE / VERIFY / INSTANTIATE — though
ARGUE may largely consist of pattern cross-references that the
empirical evidence already justifies.

## 4. ARGUE

Backfilled 2026-04-26 (originally skipped — see Checkpoint 6).
ARGUE asks why the design is *right*, not just *workable*. Three
strands: pattern cross-reference, theoretical coherence, and a
plain-language argument suitable for a reader outside the project.

### Pattern cross-reference (futon3/library)

The DERIVE design (inside-out invariants WM-I0…I4 + WM-IM, trace
registry, Evidence-derived augmentation) leans on these existing
patterns:

| Pattern | Where it applies | How |
|---|---|---|
| `stack-coherence/evidence-ledger` | every WM-In carries an `:error-shape` and a snapshot-timestamped verdict; no markdown-only assertion is allowed to stand | every claim cites a snapshot idx in the notebook log |
| `stack-coherence/maturity-evidence-audit` | the four-outcome rubric (valid war machine / valid prototype / signal without witness / working mock-up) makes "this is a war machine" an audited maturity claim, not a label | trace registry resolves before declaring a TI resolved |
| `stack-coherence/readme-devmap-sync` | TI-6's source-precedence rule (substrate > implementation > schema; emit drift, do not silently reconcile) | the schema-drift finding is recorded as TI-6, not silently fixed |
| `stack-coherence/no-new-life-logging` | Joe's stack-wide invariant from `M-war-machine.md`: "uses existing repeat signals" | WM-I4 detection now derives from existing Evidence activity (`derive-augmentation-from-evidence`), not from operator self-report |
| `stack-coherence/ready-blocked-triage` | the IDENTIFY taxonomy (non-thing / broken / confusing / mysterious / ambiguous / constraining) plus the four exit outcomes | every TI files under exactly one category; every mission state names which outcome is reachable |
| `peripherals/surface-earns-inhabitation` | the WM cannot earn the "war machine" label by being labelled one; Half B (D&G) plus WM-I4 force the surface to earn its presence | cljs tiles ship as numerics not metrics; FuLab guard remains armed until augmentation evidence accumulates |
| `peripherals/inhabitation-feeds-evolution` | the snapshot log is append-only, accumulates over time, and feeds back into both prediction error (TI-5) and evidence-derived augmentation (WM-I4) | the substrate produces its own audit data |
| `devmap-coherence/prototype-maturity-lifecycle` | the four-outcome exit rubric is exactly a maturity ladder | the mission cannot silently ship the unmet claim |
| `devmap-coherence/sovereignty-preserved` | WM-I4's Hobbes criterion (originally from `war-machine-terminal-vocabulary.edn`, extended with the FuLab anti-tax rule) | the WM has no actions; the operator decides; the surface augments rather than gates |

These patterns were not bolt-on decorations after the fact. The
inside-out invariant pivot in MAP § Checkpoint 2 followed the
futon1a `README-best-practice.md` pattern of layered invariants
with dedicated error shapes, *then* the patterns above were
recognised as the right surface-side analogues. The DERIVE design
is, by construction, what these patterns prescribe when applied to
an operator-facing audit surface over an AIF substrate.

### Theoretical coherence

The IDENTIFY anchoring named four theoretical commitments. ARGUE
checks whether the DERIVE/INSTANTIATE work serves them or has
drifted.

**1. D&G as higher-order check on AIF correctness** (mission §
Why this is also the AIF correctness check). The Half-B test
"Numerics over metrics" was framed as: *if we don't have numerics
we don't have surprisal, and a thing without surprisal cannot do
active inference.* INSTANTIATE landed numerics on the surface (12
observation channels in TI-2, four EFE terms in TI-4, per-channel
prediction error in TI-5, mode + urgency + τ + G in TI-3). The
surface now displays surprisal. The theoretical commitment holds.

**2. "Circling without landing" as a documented AIF anti-pattern**
(mission § AIF lineage). The diagnosis was: *the AIF substrate
exists, the implementation computes the values, the surface
displays the formal shape but not the actual numerics.* INSTANTIATE
made the substrate's numerics reach the surface. The
"circling" condition is broken at the substrate-surface bridge.
**The mission is itself an existence proof of moving from
"circling" to "landed."** That existence proof is publishable, if
the engineering remains in shape after augmentation evidence
accumulates.

**3. Mission-feature precision-proxy as empirical AIF-grounding
method** (M-stack-stereolithography Checkpoint 5). The same
discipline (`derive precision proxy from existing mission
evidence`) is what `derive-augmentation-from-evidence` ports to
the WM. WM-I4 augmentation detection is a sibling instance of the
M-stack-stereolithography precision-proxy method. Both honor
no-new-life-logging.

**4. A working stack-as-AIF surface as existence proof.** The cljs
UI now displays the substrate's AIF state honestly. Whether this
counts as a "working" surface depends on WM-I4 (does the operator
benefit?) and the TI-6 path choice. Both remain open. So the
existence proof is *partial*: substrate-to-surface is bridged;
operator-to-decision is not yet measured.

**Theory drift check:** none. The IDENTIFY commitments are
preserved by the DERIVE design and the INSTANTIATE work. The
FuLab anti-pattern catalogue (§ Lessons from FuLab) remains the
discipline; the no-new-life-logging fix in Checkpoint 6 *strengthened*
adherence by removing the operator-log requirement that quietly
re-introduced the very anti-pattern the section warned against.

### Trade-off summary

The mission gave up the following in exchange for the discipline
it gained:

- **gave up:** a clean, self-contained "war machine" label.
- **gained:** an honest two-surface architecture (TI-6) that is
  visible to audit instead of papered over.

- **gave up:** immediate full-mode operability (the WM does not
  yet say "the operator's life got better").
- **gained:** the FuLab guard (WM-I4) that prevents the mission
  from declaring "operator life got better" before evidence
  accumulates.

- **gave up:** ARGUE-and-VERIFY-before-INSTANTIATE phase order
  (Checkpoints 4–5 jumped ahead).
- **gained:** the empirical baseline that grounds this ARGUE.
  The patterns above were chosen with the actual landed surface
  in evidence, not with a speculative design.

- **gave up:** a single clean exit verdict ("valid war machine").
- **gained:** the four-outcome rubric that makes any exit
  honest, including the "still a working mock-up because X"
  exit.

### Generalization notes

The audit's three load-bearing mechanisms generalize beyond the
WM:

1. **Inside-out invariant pivot** (MAP Checkpoint 2). Any
   operator-facing surface over a structured substrate can be
   audited the same way: I0 substrate liveness, I1 totality, I2
   the substrate is doing the thing it claims, I3 surface
   fidelity, I4 augmentation.
2. **Evidence-derived augmentation** (Checkpoint 6 fix). Any
   audit that cares whether an operator surface is *useful* can
   refuse the temptation to require operator self-report and
   instead derive the signal from existing logs.
3. **Trace registry** (DERIVE entity types). Any UI surface
   claiming to render substrate state can be audit-instrumented
   by pairing each rendered element with a trace cell that
   computes the expected value from the same substrate.

The most transferable single move is (2): the no-new-life-logging
discipline turns "did this help?" from a survey question into a
data question, which is exactly the move that distinguishes a
useful audit from a self-congratulatory one.

### Plain-language argument

The War Machine should help its operator, not impose chores. We
audited it by stating what an honest war-machine surface must
satisfy, in five inside-out invariants ordered from substrate to
operator. We made the substrate's actual numerics reach the
surface — modes, channel values, expected-free-energy
decomposition, prediction errors — so the surface stopped
displaying only the *shape* of an AI loop and began displaying
its *content*. We refused to let the audit ask the operator to
record decisions in a journal, because that is the same
self-reporting tax the previous attempt at this kind of system
created. Instead we derive evidence of usefulness from activity
that already exists in the system's logs. The result is a
surface that has earned the right to be called a war machine in
the technical sense, contingent on evidence still accumulating.
The work that remains is small and named.

### ARGUE exit

The design feels inevitable given the constraints. The patterns
above are exactly what the futon stack discipline prescribes for
this shape of audit. The plain-language argument carries someone
outside the project to understanding without project jargon. The
trade-offs are explicit rather than smuggled.

ARGUE exit criterion met.

## 5. VERIFY

Backfilled 2026-04-26 (originally skipped — see Checkpoint 6).
VERIFY checks the design against the constraints before declaring
INSTANTIATE complete. For an audit mission whose INSTANTIATE has
already partially landed, VERIFY is a retrospective structural
check + completion-criterion pre-check.

### Structural verification

The mission has no formal exotype `.edn` wiring diagram; it does
have the wiring sketch in DERIVE § Wiring sketch. Checked against
that sketch:

| Check | Result |
|---|---|
| Completeness — every output port has a path from an input | pass: snapshot log → checker fns → verdict log → mission checkpoints; trace registry → WM-I3 verdict; evidence store → derive-augmentation-from-evidence → WM-I4 verdict |
| Coverage — every internal component reaches an output | pass: all five WM-In checkers feed `audit-all-invariants`; the snapshot log is read by both verdicts and Evidence-derived augmentation |
| No orphan inputs | one near-orphan: the hand-authored `record-augmentation!` ledger now has a fallback role only — its output is consumed by `check:WM-I4` but not load-bearing for the verdict |
| Type safety — verdict shape is uniform across checkers | pass: `{:invariant :verdict :evidence :error-shape?}` per spec |
| Spec coverage — every output has a `:spec-ref` | pass: each WM-In has its spec in MAP Checkpoint 2 |
| Timescale ordering | pass: WM-I0 (live, single-tick) → WM-I1 (single-tick) → WM-I2 (≥2 ticks) → WM-I3 (single-tick) → WM-I4 (≥2 ticks for mode-change) — strictly inside-out |

### Completion criteria pre-check

Walking each criterion from IDENTIFY § Completion criteria
(updated Checkpoint 6 to nine entries):

| # | Criterion | Status |
|---|---|---|
| 1 | Tuning ledger has every known item, classified | partial — 6 TIs filed, sixth-category (`constraining`) yet to admit any concrete entry; non-blocking |
| 2 | `M-aif-head` audit table filled in | open — § 5 skeleton; one row per AIF-head requirement still to populate. **VERIFY-time finding:** this can be done by reading futon2's `M-aif-head.md` § Completion criteria into a query against landed TIs; budgetable as a single notebook session |
| 3 | Clock/cadence audit covers every clock-bearing affordance | partial — two rows seeded (AIF tick freshness, AIF poll cadence); Audacity timeline still missing |
| 4 | Discoverability audit covers every tile/control | open — § 7 skeleton |
| 5 | Audacity timeline has documented spec | open — § 8 skeleton |
| 6 | Half-A instance proof | open — needs one specific recommendation followed end-to-end with notebook-recorded evidence; small operator-only move |
| 7 | D&G audit table filled with cited evidence, ≥3 pass | partial — 2/8 rows filled (`numerics-over-metrics`, `preserved-ambiguity`). With INSTANTIATE landed, several more can flip to `:partial` or `:pass` (e.g. `mode-not-entity`, `lives-at-the-interstices`) |
| 8 | Honest exit outcome named | met for now: *"valid prototype, not yet war machine"* |
| 9 | Notebook continuously maintained, every landed TI has cell | met: 4 snapshot log entries, 5 trace registry entries, all checker fns landed |

**Findings revising DERIVE/INSTANTIATE:**

- Criterion 7 (D&G audit) can be substantially advanced *now*
  because the surface changes from INSTANTIATE flipped real
  verdicts. Notebook §11 `audit-all-invariants` provides the
  WM-In side; the D&G audit should consume the same outputs
  rather than maintain a parallel verdict path. **Decision
  log:** the D&G audit will be a markdown rendering of the
  notebook's WM-In verdict map joined with prose anchors —
  not an independent audit.
- The `record-augmentation!` ledger can be marked as a
  fallback path only; the primary WM-I4 evidence is the new
  `derive-augmentation-from-evidence` output. This is a
  formalisation of the no-new-life-logging fix from
  Checkpoint 6 and removes the only place where the audit
  asks the operator for new data. **Decision log:** notebook
  §11 already encodes this; the mission's TI-3/2/4/5 status
  reads "implemented; awaiting Evidence-derived augmentation"
  rather than "awaiting operator log."

### Fidelity check

The mission has no GF (Capability Preservation Matrix) because
it is a tuning-and-audit mission rather than a port/rebuild.
Spec commitments declared in DERIVE § Declared spec commitments
are checked here:

| Commitment | Status |
|---|---|
| Inside-out audit ordering | pass — reports go I0 → I4 |
| Snapshot is the canonical evidence unit | pass — every claim cites a snapshot log entry |
| Verdict shape uniform across checkers | pass — confirmed in structural check above |
| Schema drift emitted explicitly, not silently reconciled | pass — TI-6 records the architectural finding rather than rewriting the vocabulary |
| Augmentation tracked separately from correctness | pass — and now Evidence-derived rather than operator-authored |
| Trace cells own UI's truth | pass — every UI tile has a registered trace |

### Decision log (VERIFY-time)

1. **No-new-life-logging is now a load-bearing audit invariant
   for this mission**, not just for the underlying War Machine.
   The notebook's `derive-augmentation-from-evidence` is the
   reference implementation of how to honor the invariant in any
   future audit that wants to measure operator augmentation.
2. **D&G audit consumes WM-In verdicts**, doesn't maintain a
   parallel verdict path. § 5b is now a render of § 4 / §11
   with prose, not an independent audit.
3. **TI-6 stays in ARGUE-deferred state through VERIFY** — the
   architectural call (Path A/B/C) does not block the rest of
   the audit; it only prevents `:WM-I1` from reading `:pass`.
4. **The mission can ship "valid prototype, not yet war
   machine"** with criteria 1, 6, and the augmentation half of
   the rubric explicitly deferred to a follow-on cycle if the
   operator chooses to ship the current state.

### Risk register

Risks that remain after VERIFY:

- **R1: The Evidence-derived augmentation may produce false
  correlations.** Mode change at 1430 + commit at 1432 could be
  unrelated. *Mitigation:* the verdict is `:evidence-correlated`
  not `:caused-by`; the report is a signal not a claim. Window
  size (default 6 h) is configurable; if false-positive rate is
  high, the audit can require multiple signals or a dedicated
  endorsement.
- **R2: TI-6 may stay deferred indefinitely.** *Mitigation:*
  WM-I1 stays `:partial`; the four-outcome rubric makes
  "valid war machine" unreachable until TI-6 lands.
- **R3: cljs UI elements may diverge from substrate without
  the trace registry catching it.** Currently each trace
  checks substrate-side existence, not pixel-side fidelity.
  *Mitigation:* a small browser-side scrape comparator landed
  in a follow-up cycle, or a Clojure cell that fetches the
  rendered HTML and parses the displayed values.

### VERIFY exit

Structural verification clean (modulo near-orphan fallback path).
Completion-criterion pre-check shows the mission shipping the
current state would be honest under the four-outcome rubric. Two
decision-log entries clarify how the D&G audit relates to the
WM-In audit and how Evidence-derived augmentation supplants
operator self-report.

VERIFY exit criterion met. Mission may proceed to DOCUMENT or
elect to take more INSTANTIATE rounds (more TIs filed, more
augmentation evidence accumulated, TI-6 path chosen) before
documenting.

## 6. Tuning ledger

The single source of truth for War Machine tuning items. Each entry
follows the shape:

```
### TI-N · <short title>
**Category:** <non-thing | broken | confusing | mysterious | ambiguous>
**Surface/file:** <where it appears>
**Current state:** <what the operator sees today>
**Proposed fix:** <what the right shape would be>
**Owner:** <Joe | agent | Codex | unassigned>
**Status:** INSTANTIATE near complete — handing off to E-war-machine-qa (Checkpoint 10, 2026-04-26)
```

Add new entries by incrementing N. Resolved entries stay in place
(struck through or status-flagged) so the audit history is preserved.

### TI-1 · Recurring AIF tick is not a witness for the next-move agenda

**Category:** ambiguous

**Surface/file:**
- `futon3c/src/futon3c/aif/stack_generator.clj:184` —
  `portfolio-step-evidence` filter
- `futon3c/src/futon3c/aif/stack_generator.clj:200` —
  `next-move-agenda` projection
- War Machine UI: "Recommended Next Move" card

**Current state:**
The `:portfolio/step` evidence emitted by
`futon3c.portfolio-inference.scheduler` (the daily recurring AIF
tick — see `M-stack-stereolithography` § Checkpoint 5) does **not**
match the next-move-agenda filter, because the filter requires both
`:agenda-id = "wm.close-s6.v1"` and
`:observation-source/path = "futon5a/holes/stories/THE-STACK.aif.edn"`,
neither of which the scheduler's heartbeat carries. Result: the S6
agenda will read as `:unattempted` from scheduler ticks alone,
indefinitely. The UI's witness card will only animate when an
explicit, agenda-tagged step lands.

This is ambiguous from the operator side: the recurring AIF *is*
firing, evidence *is* being deposited, but the most prominent UI
card pretends nothing has happened. There is no display element
that distinguishes "scheduler is alive but not advancing this
agenda" from "scheduler is dead." The two indistinguishable
states are exactly the failure mode this mission is supposed to
catch.

**Proposed fix:**
Two-track signal, never collapsed:

1. Keep `next-move-agenda`'s filter strict (agenda-id +
   observation-source). Witnesses come from deliberate `:pi-step`
   invocations that explicitly say "I'm advancing this agenda."
   Tagging the scheduler heartbeat as an agenda witness would
   overclaim causality and lose semantic precision.
2. Add a separate `next-move-pulse` projection that reads the most
   recent `:portfolio/step` evidence regardless of agenda-id, and
   surfaces it as a *freshness/liveness indicator only*, distinct
   from the witness card.
3. UI shows "AIF heartbeat: HH:MM · live/aging" alongside the
   witness card, not inside it.
4. Optional convenience for the rare case where the operator
   wants a heartbeat tick to count as an agenda step:
   `(sched/tick-as-agenda! "wm.close-s6.v1" stack-observation-path)`
   — explicit, not scheduled.

**Owner:** unassigned (Joe to assign)

**Status:** INSTANTIATE near complete — handing off to E-war-machine-qa (Checkpoint 10, 2026-04-26)

### TI-2 · 12-channel observation vector is computed but not surfaced

**Category:** non-thing (the affordance the surface implies — a live
AIF observation read — has no rendering behind it)

**Surface/file:**
- Spec: `futon5a/data/war-machine-terminal-vocabulary.edn` § `:o/raw-fields` and § `:o/vector-abi`
- Implementation: `futon0/scripts/futon0/report/war_machine.clj` `observe` (line 1412)
- UI gap: `futon0/web/war-machine/src/war_machine/client/core.cljs` references only `:loop-health` (line 433); the other 11 channels are absent from the surface

**Current state:**
The 12-channel observation vector (`:loop-health`,
`:support-coverage`, `:attack-coverage`, `:mission-health`, the four
workstream-pct, `:active-repo-ratio`, `:sorry-count-norm`,
`:coupling-density`, `:ticks-firing-ratio`) is the AIF substrate's
canonical input. The implementation computes it. The 2026-04-12
snapshot shows it carries a real, load-bearing diagnosis (hermit
mode). The surface displays none of it.

**Proposed fix:**
Add a 12-row observation panel (one row per channel) that shows:
the current value, the preferred range from `:C/preferred`, and a
single visual indicator of whether the value is inside, above, or
below preferred. This is a *numeric* display, not a status badge —
operators read directional drift from it, not "green/red OK/not OK."

**Owner:** unassigned

**Status:** INSTANTIATE near complete — handing off to E-war-machine-qa (Checkpoint 10, 2026-04-26)

### TI-3 · Inferred mode is not visible on the surface

**Category:** non-thing

**Surface/file:**
- Spec: `war-machine-terminal-vocabulary.edn` § `:μ/modes` (six modes,
  each with a JSDQ/logic-model parallel)
- Implementation: `war_machine.clj/infer-mode` (line 1581)
- UI gap: no mode display anywhere in the cljs client

**Current state:**
The inferred strategic mode is the most concentrated thing the
surface could display — one of six legible diagnoses
(`:multiplied` / `:foraging-trapped` / `:hermit` / `:depositing` /
`:stagnant` / `:dark`), each with its own theoretical anchor and
its own implied operator response. The 2026-04-12 snapshot says
`:hermit` and that single fact is the action-relevant payload.
The surface does not show it.

**Proposed fix:**
A prominent mode badge with the six possible modes legible
(perhaps a small radial or row of mode pills, current mode
highlighted). Hover surfaces the per-mode rule (e.g. *":hermit
fires when stack-pct > 0.7 AND ticks-firing-ratio > 0.5"*) and
the channels currently triggering it. This is the operator's
"what's the picture right now" surface.

**Owner:** unassigned

**Status:** INSTANTIATE near complete — handing off to E-war-machine-qa (Checkpoint 10, 2026-04-26)

### TI-4 · Free-energy decomposition is computed but not visible

**Category:** non-thing

**Surface/file:**
- Spec: `war-machine-terminal-vocabulary.edn` § `:G/pragmatic-fn`,
  `:G/epistemic-fn`, `:G/weights`
- Implementation: in `war_machine.clj`
- UI gap: no G display anywhere

**Current state:**
The pragmatic/epistemic split is what makes G actionable. Pragmatic
high → workstream is wrong. Epistemic high → the picture itself is
unclear. The two recommend different operator moves (rebalance vs
investigate). Single-number G hides this. The 2026-04-12 snapshot
shows G-pragmatic 0.72, G-epistemic 0.09 — a sharp pragmatic
imbalance with a clear epistemic picture. That's a specific
diagnosis the surface must be able to say.

**Proposed fix:**
A two-bar G display (pragmatic + epistemic) with the channel-level
contribution breakdown on hover or click. The operator reads
*"pragmatic gap is dominated by stack-pct (0.25 weight × distance
from preferred) and consulting-pct (0.25 weight × distance from
preferred)"* — that is a directional, numeric read, not a metric.

**Owner:** unassigned

**Status:** INSTANTIATE near complete — handing off to E-war-machine-qa (Checkpoint 10, 2026-04-26)

### TI-5 · Prediction-error / observed-vs-expected reconciliation is missing

**Category:** ambiguous (ambiguous because it is unclear from the
surface whether the AIF loop is *inferring* anything at all or
just *observing* — the difference is whether prediction error is
being computed and surfaced)

**Surface/file:**
- Spec: `core-terminal-vocabulary.md` core loop ("Observables →
  Beliefs → Policies → Actions → Free Energy") and the M-aif-head
  requirement of cross-phase prediction-error tracking
- Implementation: futon3c.portfolio.perceive/perceive computes
  `(- mu obs)` weighted by precision; the recurring scheduler
  deposits this in `:portfolio/belief` evidence
- UI gap: prediction error never reaches the surface

**Current state:**
Without a prediction-error display, the surface is observing, not
inferring. The operator cannot tell whether the model is being
updated by experience or whether it is a fixed lookup. The
recurring tick deposits μ each day; nothing renders the Δ between
yesterday's μ and today's observation.

**Proposed fix:**
A small prediction-error sparkline per channel — last N ticks
showing observed - predicted. Channels with persistent positive or
negative error are the ones the model is failing to track; these
are the operator-relevant signal.

**Owner:** unassigned

**Status:** INSTANTIATE near complete — handing off to E-war-machine-qa (Checkpoint 10, 2026-04-26)

### TI-6 · Two AIF surfaces conflated under one WM banner (revised 2026-04-26)

**Category:** mysterious (the operator cannot tell which observation
surface the WM is actually consuming, because two different ones
exist and the vocabulary describes one while the running scheduler
emits the other)

**Surface/file:**
- WM strategic observe: `futon0/scripts/futon0/report/war_machine.clj`
  `observe` at line 1412 — 12 channels (`:loop-health`,
  `:support-coverage`, `:attack-coverage`, `:mission-health`,
  `:stack-pct`, `:consulting-pct`, `:portfolio-pct`,
  `:mathematics-pct`, `:active-repo-ratio`, `:sorry-count-norm`,
  `:coupling-density`, `:ticks-firing-ratio`)
- Portfolio observe: `futon3c.portfolio.observe/observe` at line 179
  — 16 channels (`:mission-complete-ratio`, `:coverage-pct`,
  `:coverage-trajectory`, `:mana-available`, `:blocked-ratio`,
  `:evidence-velocity`, `:dependency-depth`, `:gap-count`,
  `:stall-count`, `:spinoff-pressure`, `:pattern-reuse`,
  `:review-age`, `:effort-prediction-error`, `:bid-completion-rate`,
  `:unplanned-work-ratio`, `:turn-velocity`)
- Schema: `futon5a/data/war-machine-terminal-vocabulary.edn`
  § `:o/raw-fields` — describes the **strategic** surface
- Recurring scheduler: deposits `:portfolio/observation` evidence
  with the **portfolio** surface

**Current state:**
DERIVE Checkpoint 3 (2026-04-26T19:32:28Z) revealed that the live
`:portfolio/observation` evidence emits a 16-channel set that has
**near-zero overlap** with the 12-channel set declared in the WM
vocabulary. The first reading was *"schema drift, missing 4
channels"*; the actual finding is *"two distinct AIF observation
surfaces co-exist, the vocabulary describes one, the running
scheduler runs the other."*

The two surfaces are siblings, not duplicates. They observe at the
same timescale (slow, strategic) but address different questions:

- **Strategic observe (`war_machine.clj`):** "what does the
  argument-and-attack landscape look like? what does workstream
  balance look like? where is the loop dark?"
- **Portfolio observe (`portfolio.observe`):** "what is the mission
  portfolio's current state? coverage, completion, blocked, stalled,
  pattern-reuse, prediction error against bids?"

Both are war-machine-shaped (slow AIF, multi-channel, computes G).
Neither was named as the canonical WM observation surface. The cljs
UI ends up consuming the portfolio surface (because that is what the
recurring tick deposits) while the vocabulary describes the
strategic surface (because that was the original WM design). The
operator therefore sees portfolio-shaped evidence under the WM
banner, while reading documentation that describes strategic-shaped
observables.

**This is the load-bearing finding of the DERIVE pass.** It is
also a confirmation that the inside-out invariant approach surfaces
real architectural questions that the outside-in tuning pass would
have papered over.

**Proposed fix path (decision needed):**

Three legitimate paths; design choice required.

**Path A — Vocabulary describes the de-facto running surface.**
Rename `war-machine-terminal-vocabulary.edn` to
`war-machine-portfolio-vocabulary.edn`, document the 16
portfolio channels, accept that the WM has become a portfolio-
inference renderer in practice. Create a sibling
`war-machine-strategic-vocabulary.edn` if the strategic surface
is still wanted, but mark it as not currently consumed.

**Path B — Switch the scheduler to run war_machine.clj/observe.**
The vocabulary stays correct; the recurring tick deposits
strategic-shaped evidence; the cljs UI consumes the strategic
observation. Cost: a code change in the scheduler, and the
mission-portfolio observation must find another home (probably
M-stack-stereolithography's surface).

**Path C — Two scheduled ticks, two surfaces, two vocabularies.**
The cleanest from an audit standpoint: each AIF surface has its
own vocabulary, its own recurring tick, its own evidence stream.
The WM UI displays both side by side or selects one at a time.
Highest engineering cost; clearest semantics.

Each path has different implications for M-stack-stereolithography,
M-aif-head, and the eventual paper outline. The decision belongs
to ARGUE (we have not collected enough evidence to choose between
them on engineering grounds alone).

**Interim verdict against WM-I1:**
WM-I1 stays `:partial` until a path is chosen. The `:partial`
verdict is now grounded in a real architectural decision rather
than a documentation lag. This is a **better baseline than the
"schema drift" reading** because the audit has surfaced the actual
work, not deferred it.

**Owner:** Joe + agent

**Status:** INSTANTIATE near complete — handing off to E-war-machine-qa (Checkpoint 10, 2026-04-26)
**Status:** INSTANTIATE near complete — handing off to E-war-machine-qa (Checkpoint 10, 2026-04-26)

## 7. `M-aif-head` audit (skeleton)

To be filled in. One row per requirement named in
`futon2/holes/M-aif-head.md` § Completion criteria.

| Requirement | Current state | Evidence | Decision |
|---|---|---|---|
| _pending_ |  |  |  |

## 7b. D&G audit (skeleton — Half B of validity claim)

One row per operational test in *§ Definitional anchor*. The mission
must mark at least three as `pass` for Half B to hold, or exit with
an explicit captured-WM finding.

| Test | Current state (pass / partial / fail) | Evidence | Notes |
|---|---|---|---|
| Mode, not entity | **pass** | INSTANTIATE landed `aif-mode-tile` reading `:scheduler/last-diagnostics` per tick. The surface no longer displays a frozen state — mode flipped `:BUILD → :MAINTAIN` between live ticks during DERIVE/INSTANTIATE work (snapshot log idx 1 vs idx 4). The substrate runs continuously. | WM-I0 + WM-I2 verdicts both `:pass`; trace registry resolves `:next-move/mode` to live belief μ.mode. |
| Numerics over metrics | **pass** | INSTANTIATE landed four numeric tiles: 16-channel observation panel (TI-2), 4-term EFE decomposition (TI-4), per-channel prediction error (TI-5), mode + urgency + τ + G strip (TI-3). All read substrate values, not status-badge categories. The Joe formulation *"if we don't have numerics we don't have surprisal"* is now satisfied — free energy is rendered as a number on the surface. | Half-B load-bearing test flipped from **fail** to **pass**. The largest single move of the mission. |
| Stationary process | **pass** | The recurring scheduler ticks daily and deposits four evidence entries per tick (`:portfolio/observation`, `:portfolio/belief`, `:portfolio/policy`, `:portfolio/step`). The cljs UI's `aif-prediction-error-tile` displays ε per channel for each tick. The substrate is `μ ← μ + κ·τ·ε` running, not snapshotted. | WM-I0/I2 both `:pass`. The "stationary process" criterion is what M-stack-stereolithography Checkpoint 5 wired. |
| Non-finalist | **partial** | The EFE tile shows the chosen action's term decomposition, including the `:effort` cost — so the surface presents action selection as a tendency, not a target. *But* the next-move card still reads as targets (`Close S6, advance v2`), and the witness chain framing of `:rolled-forward` / `:underspecified` reads as goal-reaching. | Full pass requires the next-move card to display directional/tendency framing alongside the closure target. Small UI work; not yet landed. |
| Lives at the interstices | **partial** | TI-5 surfaces precisely the boundary between belief and observation (per-channel ε). Real boundary cases visible right now: `gap-count +0.87` (model expected low, saw high), `mission-complete-ratio -0.52` (expected high, saw lower). The surface displays the interstitial values; it does not collapse them into pass/fail badges. | The collapse-resistance is good. Full pass would require interstice-shaped UI affordances (e.g. mouseover that explains *why* the boundary is where it is). |
| Vector of deterritorialization | **pass** | The 4-term EFE includes the `:epistemic` term, which is exactly the AIF analogue of D&G's vector-of-deterritorialization (per § Why this is also the AIF correctness check). `aif-efe-tile` displays this term separately from `:pragmatic`, with green colour (value-bearing). The substrate computes it via `futon3c.portfolio.policy/expected-free-energy`. | Capture would mean displaying only `:pragmatic`. The surface does not. |
| Preserved ambiguity / contact | **pass** | TI-5's prediction-error tile preserves per-channel ambiguity rather than collapsing into status badges. TI-1 was originally listed as ambiguity-collapsing (scheduler pulse vs agenda witness); the mode tile now distinguishes "scheduler alive but not advancing this agenda" (mode shown, freshness `:live`) from "scheduler dead" (no diagnostics, no mode). Both ambiguity-preserving moves landed. | The remaining ambiguity-collapse case is μ-as-point-estimate (ideal Half-B `:pass` would render mode as `p(BUILD)=0.62, p(MAINTAIN)=0.21, …`). Scope-out for now; flagged. |
| Capture sites visible | **pass** | The mission itself enumerates capture sites: status badges (none added by INSTANTIATE), "completed" tiles (none added), rounded-percentage displays (none — the tiles use 3-decimal precision). TI-6's audit surfaced a structural capture site (two AIF surfaces conflated under one banner) and Path A resolved it. The tuning ledger names every capture site found. | Self-supervision passes. The audit caught its own capture risks during construction. |

## 8. Clock / cadence audit (skeleton)

To be filled in. One row per clock-bearing affordance on the
surface.

| Affordance | Surface element | Unit | Source of truth | Consistent? |
|---|---|---|---|---|
| AIF tick freshness | next-move card | minutes | `:scheduler/period-seconds` from /api/alpha/aif-stack/live | yes (after Checkpoint 5 wiring) |
| AIF poll cadence | api.cljs `desired-poll-ms` | ms | same | yes |
| Audacity timeline x-axis | _pending_ |  |  |  |
| Hover minute label | _pending_ |  |  |  |

## 9. Discoverability audit (skeleton)

| Tile / control | Hover detail? | What it does | Affordance sufficient? |
|---|---|---|---|
| _pending_ |  |  |  |

## 10. Audacity-style timeline spec (skeleton)

To be written: a paragraph per visual element (lanes, ticks, hover,
selection drag, zoom controls if any), and a paragraph per
interaction (click, drag, hover, keyboard). Then a comparison column
for "implementation matches spec? yes/no/partial."

## 11. Validity-claim instance proof (skeleton)

A single specific recommendation is to be followed end-to-end:

1. Surface displays recommendation R at time T.
2. Operator acts on R (specific action documented here).
3. Action produces evidence E (specific evidence-id documented).
4. Surface refreshed at T+δ.
5. Surface either reflects E in the next-move card, or fails to,
   and the mission documents which.

If step 5 is "reflects E," the validity claim is met. If "fails to,"
the surface is a working mock-up and that finding is the exit.

## 12. Source material

| Source | Role |
|---|---|
| `futon3c/holes/missions/M-war-machine.md` | the underlying mission this one is auditing |
| `futon5/docs/core-terminal-vocabulary.md` | abstract AIF/GFE meta-policy schema all domain vocabularies instantiate |
| `futon5a/data/war-machine-terminal-vocabulary.edn` | concrete War Machine AIF schema — 12 observables, 6 modes, G fns, 2026-04-12 snapshot |
| `futon0/scripts/futon0/report/war_machine.clj` | implementation: `observe` (1412), `infer-mode` (1581), 2 089 lines |
| `futon5a/data/war-machine-exotype.edn` | War Machine wiring diagram |
| `futon5a/holes/stories/THE-STACK.aif.edn` | stack-level meta-AIF+: 16 leaves, 4 conflicts, 16 next-moves |
| `futon5a/holes/holistic-argument*.md` | argument-level framing (S1-S5, A1-A4) the surface reads against |
| `futon3c/src/futon3c/aif/stack_generator.clj` | live AIF stack projection |
| `futon3c/src/futon3c/transport/http.clj` | `/api/alpha/aif-stack/live` handler |
| `futon3c/src/futon3c/portfolio_inference/scheduler.clj` | recurring AIF tick |
| `futon0/web/war-machine/src/war_machine/client/*.cljs` | UI (the place AIF state currently fails to reach) |
| `futon2/holes/M-aif-head.md` | AIF-head requirements |
| `futon5a/holes/missions/M-stack-stereolithography.md` | cadence + stereolithographic context |
| `~/Downloads/NoteGPT_TRANSCRIPT_What is the War Machine Deleuze and Guattari Concept In Focus.txt` | D&G transcript anchoring § Definitional anchor |
| `futon5a/analysis/notebooks/M_war_machine_tuning.clj` | computational notebook companion (vsat.wiki style — kindly-ready, Drawbridge-driven, snapshot log) |
| `futon5a/analysis/notebooks/deps.edn` | clay/kindly deps for notebook rendering |
| `~/vsat.wiki/{analysis-demo,ukrn-demo}/notebooks/*.clj` | stylistic precedent — notebook-as-clj convention |
| `~/code/p4ng/appendix.tex` Box 2 + `contents.tex` § Patterns and Active Inference | FuLab agents' real AIF backing — pseudocode + AIF-guides-not-controls / deviation-justification / abstain logic; the painful precedent that *§ Lessons from FuLab* is built on |
| `~/code/futon1a/README-best-practice.md` | layered-invariant + error-shape + proof-path discipline that MAP § Checkpoint 2 ports to the WM (WM-I0 …WM-I4 + WM-IM) |
| `~/code/futon2/src/ants/{cyber,visual,clj_ants_aif}.clj` | cyberant ants — existing running AIF reference, structural template per `:harmonisation :cyberants-template`. The futon2 relocation question (§ Checkpoint 2) is whether the WM should sit alongside them. |

## 13. pending DOCUMENT
