# E-pipeline-pipecleaner — is the invariant-queue pipeline tracer still useful?

Status: PIPE-CLEANED / ROADMAP-DEFERRED
Parent: `futon7/holes/M-futon-forward-model.md`
Related: `futon7/holes/missions/M-value-creation-loop.md`
Opened: 2026-06-16

## Question

The `pipeline-tracer` mechanism was introduced as a week-one self-test for
`M-invariant-queue-extend`: non-invariant work items were recorded as evidence
and expected either to close or to expose a clogged pipeline. On 2026-06-16 the
boot check still reported four overdue 2026-05-06 tracks. Those tracks were
closed as superseded, not because the mechanism is worthless, but because the
dataset was stale.

This excursion asks whether the pipeline tracer is still useful for us, and if
so what dataset it should track now.

## Initial Finding

The current tracer dataset is not useful as a boot-pressure source:

- It is hard-coded in `futon3c.logic.tracer/default-tracers`.
- Its six items were specific to the 2026-04-29 invariant-queue self-test.
- Four stale-open tracks had been overtaken by later mission structures
  (VSATARCS docs/contracts, War Machine tuning/pilot/first-outing,
  substrate/mission-scope work, typed-bells/typed-holes).
- Closing them made the check pass without changing the apparatus.

That suggests the apparatus may still be good, but the watched population needs
to become a live projection rather than a frozen historical list.

## Candidate Datasets

1. Forward-model watched backlog items:
   read `M-futon-forward-model` artifacts and track the subset that is
   structurally central, active, and expected to move within a declared window.

2. War Machine operator-lane brief/nag items:
   track items routed to Brief or Nag, and close them when acknowledged,
   discharged, parked, or promoted to a mission.

3. Mission-doc missing-status / stale-status repairs:
   track remediation obligations emitted by archaeology checks, not arbitrary
   mission count pressure.

4. Typed coordination residues:
   track projection adapters or typed-hole fills that are explicitly waiting on
   runtime evidence.

## Useful-If Criteria

The tracer is worth keeping if it satisfies all of these:

- The watched set is generated from a current source of truth.
- Each item has a clear close condition.
- The warning tells the operator exactly what to do next.
- Closing an item writes evidence, not just edits a local list.
- The tracer helps the forward model distinguish live pressure from historical
  sediment.

## Original Next Slice

Compare `default-tracers` with one generated watched set from
`M-futon-forward-model.mint.edn` or the operator-lane bulletin. If the generated
set produces better operator action than the hard-coded list, replace or
supplement `default-tracers` with that projection.

## Current Resolution

The first goal is complete: the stale invariant-queue pipe has been cleaned.
The old tracer rows were preserved as `pipeline-prototype.edn`, the boot warning
is no longer driven by a frozen historical queue, and the excursion now records
what a useful replacement pipe would need to watch.

The larger question remains open: turning the whole backlog into something like
a linear roadmap is not solved here. The working conclusion is narrower and more
stable: a linear pipeline is only valid as a projection from a cascade /
semilattice, with scan/proof witnesses back to the sources that certify each
card.

## Checkpoint 1 — Read with M-value-creation-loop

`M-value-creation-loop` sharpens the dataset question. The useful pipeline is
not "old work items that should move someday"; it is the prospective value loop:

```text
internal gap
  -> prospective pattern-cascade build strategy
  -> external close-candidate / supplier scan
  -> pheromone PR
  -> resonance / affinity
  -> grounded EOI
  -> consent-gated send
  -> collaborator / supplier / revenue / gap filled
```

That loop is exactly an `M-futon-forward-model` exercise:

- It keeps the forward model descriptive-first: the trace records current loop
  state and witnessed transitions, not a churn prediction.
- It supplies the generative turn from `M-futon-forward-model` §7.3: a missing
  feature becomes a prospective build strategy, then a public value-creating
  action.
- It gives pipeline-tracer a live close condition at each stage instead of a
  frozen target date.

So the tracer should not primarily watch the whole backlog. It should watch
**value-loop candidates**, with one tracer per candidate gap/strategy pair.

### Candidate Tracer Record

```clojure
{:track-id :value-loop/<gap-id>
 :source :M-value-creation-loop
 :gap {:kind :sorry|:devmap|:capability-star|:held-kit|:mission-backlog
       :id ...
       :forward-model-region ...
       :why-this-gap ...}
 :stage :gap|:strategy|:close-scan|:pr|:resonance|:eoi-staged|:sent|:converted|:parked
 :expected-next {:stage ...
                 :by "YYYY-MM-DD"
                 :operator-action "..."}
 :close-condition "what evidence closes or advances this stage"
 :evidence-refs [...]}
```

This changes the warning semantics:

- stale at `:gap` means "no prospective strategy was produced";
- stale at `:strategy` means "no external close-candidate scan ran";
- stale at `:close-scan` means "candidate found but no PR/strawman decision";
- stale at `:pr` means "PR not submitted/merged/parked";
- stale at `:eoi-staged` means "operator has not reviewed/sent/parked";
- stale at `:sent` means "awaiting outcome-window result";
- `:converted` and `:parked` are terminal closures.

### Better First Dataset

Use `M-value-creation-loop` Checkpoint 2's `kit-cadence` hand-fold as the first
pipe-cleaner item. It has all ingredients a useful tracer needs:

- a real held gap (`:kit-cadence`);
- a typed `have -> want` arrow;
- a prospective cascade output;
- a hand-folded build strategy;
- known policy holes;
- a clear next transition: encode the arrow/rules machine-readably or park the
  policy holes.

That is better than using the old invariant-queue tracks because it exercises
the current forward-model concepts directly: gap inventory, prospective cascade,
stage transition, and evidence-backed closure.

## Checkpoint 2 — Pipeline is only valid as a cascade projection

Joe's sharper constraint: a linear pipeline/roadmap is only honest when attached
to a cascade/semilattice that represents the actual backlog as defined by real
scans.

So the tracer must not own the ontology. It is a **1-dimensional projection** of
a richer backlog structure:

```text
real scans
  -> backlog semilattice / cascade
  -> selected value-loop path
  -> linear pipeline trace for that path
```

The old `default-tracers` violated this discipline: they were a hand-written
roadmap list with no current semilattice behind them. That made their warning
age badly. The fixed design is:

1. Build or read a scanned backlog structure:
   `M-futon-forward-model.semilattice.edn`, `M-capability-star-map`, held
   pudding-prover kits, `sorrys.edn`, devmaps, and other real inventories.
2. Select a path through that structure using the prospective cascade/value-loop
   criteria.
3. Emit a pipeline tracer only for the selected path, carrying a pointer back to
   the semilattice nodes and scan witnesses that justify it.
4. When the underlying scan changes, regenerate or retire the projection.

This gives the linear pipeline its proper status: not the roadmap itself, but a
temporarily useful reading order through a real, branching backlog. A boot
warning is then meaningful because it says, "this projected path through the
scanned backlog is stalled at stage X," not "an old list item missed a date."

## Checkpoint 3 — make sense of what was in the queue

Before using `pipeline-prototype.edn` or the Arxana Candidate Invariant queue as
feature-scan sources, separate the layers that were previously conflated.

### 1. The pipeline prototype was a self-test, not a general roadmap

`holes/excursions/pipeline-prototype.edn` preserves six historical tracer items
from the 2026-04-29 invariant-queue self-test:

| Track | Queue meaning | Feature-scan class |
| --- | --- | --- |
| `track-4-2-snapshot-as-evidence` | Decide how probe runs should leave a durable state snapshot. | witness/schema: what operational state needs first-class evidence? |
| `track-4-3-arxana-view-columns` | Make probe state visible in the operator surface. | operator-readout: what stack state is invisible or hard to compare? |
| `track-3-write-class-scoping` | Generalize the single-boundary/write-class pattern to more event classes. | boundary-generalization: which recurring writes still lack mission stubs, gates, or a ratchet? |
| `track-1-substrate-2-lift` | Convert substrate-2 deferred invariant stubs into real checks. | deferred-stub lift: which intended laws are still only placeholders? |
| `track-2-war-machine-aif-lift` | Convert War-Machine AIF deferred invariant stubs or record a principled deferral. | deferred-stub lift / principled deferral: which conceptual laws are not yet executable? |
| `track-5-vsatarcs` | Decide whether the VSATARCS narrative is parked or registerable as a contract. | ontology/narrative registration: which parked concepts need re-park, mission, or invariant shape? |

Read this as a **taxonomy of missing-feature pressures** produced by the old
queue, not as live work. It asks:

1. What state needs to become durable evidence?
2. What state needs an operator readout?
3. What write patterns need mission/gate/ratchet scoping?
4. What deferred laws need executable checks?
5. What deferred laws need explicit non-action criteria?
6. What parked narratives need a real registry status?

Those questions are good scan prompts over the current stack. The specific six
2026-05-06 items are historical.

### 2. The Arxana Candidate Invariant view is a mixed projection

The Emacs surface in `futon4/dev/arxana-browser-lab.el` currently builds a
Candidate Invariant view from at least three populations:

1. candidate invariants from the structural-law inventory / audited projection;
2. pipeline-tracer overlays;
3. deferred-stub overlays for substrate-2 and War-Machine AIF families.

The hard-coded tracer overlay in that file has already diverged from the old
runtime seed: four stale I0 tracers were removed from the Emacs view on
2026-06-01 after being verified done, while two genuinely pending-looking
tracers were retained and re-dated:

- `track-3-write-class-scoping`;
- `track-5-vsatarcs`.

It also carries twelve deferred-stub rows:

- six `substrate-2/*` phase-1 families;
- six `war-machine/*` AIF families.

So "the queue" should not be treated as a single list. It is a **projection
surface** combining:

- promotion candidates: candidate laws close enough to become operational;
- watchlist candidates: law-shaped pressures that should stay visible;
- tracer overlays: concrete work/path items being watched for flow;
- deferred-stub overlays: executable-invariant holes waiting for a real check or
  a principled deferral.

### 3. First scan shape

The first useful pipe-cleaner scan should probably not ask "which queue row is
next?" It should ask:

```text
for each queue/prototype item:
  what missing-feature pressure does this item represent?
  what current source of truth would prove that pressure is still live?
  what semilattice/cascade node should it attach to?
  what linear tracer stage, if any, would be a useful projection?
```

That converts stale queue rows into feature probes. For example:

- `snapshot-as-evidence` scans for stateful runtime facts that only exist as
  local memory, logs, or operator recollection.
- `arxana-view-columns` scans for facts that exist durably but are not visible
  where decisions are made.
- `write-class-scoping` scans for recurring append/write event families without
  a mission stub, single-boundary, or anti-bypass check.
- `deferred-stub lift` scans `probe_taps`, structural-law inventory entries, and
  mission docs for laws whose prose exists but executable checks do not.
- `VSATARCS` scans parked concepts for the next honest state: re-park,
  register, promote, or delete.

This is the bridge from the old queue to `M-value-creation-loop`: a row becomes
useful only after it is reinterpreted as a missing-feature pressure grounded in
a current scan source. A linear pipeline can then be emitted for one selected
pressure/path, but the durable source is the scanned cascade.

## Checkpoint 4 — exploration per feature-scan class

This pass reads the prototype classes as scan probes over the current stack. The
point is not to reopen the old six tracks; it is to ask which kinds of missing
features the old queue was sensitive to.

### witness/schema

**Prototype source:** `track-4-2-snapshot-as-evidence`.

**What landed:** `src/futon3c/logic/snapshot.clj` now implements
`I-state-snapshot-witness` and emits boot/on-demand snapshot evidence for:

- structural-law inventory state;
- live family-check registry state, including deferred status;
- repo refs;
- the most recent HUD render.

The structural-law inventory also records these as operational siblings under
`state-snapshot-witness/{inventory,registry,repo-refs,hud-render}`.

**Remaining pressure:** this class is no longer about the invariant queue
specifically. It has become a general question:

```text
which runtime facts are still only visible as process memory, logs, REPL
knowledge, or operator recollection, and therefore need a state-snapshot witness?
```

Candidates surfaced by grep include agency runtime registries, mission-control
mana/portfolio snapshots, War-Machine scheduler snapshots, blackboard snapshots,
and peripheral state snapshots. Some already emit evidence; a scan should
distinguish "snapshot exists but not queried" from "no witness exists."

**Scan probe:** enumerate functions whose names include `snapshot`, classify
them as `:durable-evidence`, `:api-only`, `:local-memory`, or `:test-only`, and
ask whether each one has a close condition in the evidence store.

### operator-readout

**Prototype source:** `track-4-3-arxana-view-columns`.

**What landed:** `futon4/dev/arxana-browser-lab.el` now joins recent
`:family-fired` evidence into both the Candidate Invariant and
Operational Families views, rendering `LastFire`, `LastViol`, and `Inactive`
columns. `futon4/dev/arxana-dramaturge.el` has assertions for those columns.

**Remaining pressure:** the class generalizes from "add these three columns" to:

```text
which facts already exist durably but are not visible at the decision surface
where Joe/operator chooses the next move?
```

The Candidate Invariant surface is especially useful because it mixes structural
law candidates, tracer overlays, and deferred-stub overlays. It is a readout
surface for a cascade, not just a table.

**Scan probe:** compare durable evidence/snapshot event types against Arxana/HUD
view columns. A missing readout is a feature candidate when the fact exists in
evidence but no operator surface renders it near the relevant decision.

### boundary-generalization

**Prototype source:** `track-3-write-class-scoping`.

**Evidence found:** `M-invariant-queue-extend` names four follow-on write
classes that would benefit from the single-boundary recipe:

- bell receipts;
- agent registration;
- mission state transitions;
- gate traversals.

The single-boundary precedent is now real for evidence appends:
`futon3c.evidence.boundary/append!` owns evidence writes, and current application
call sites mostly route through it. The old mission text expected separate
mission stubs per write class, each with a boundary + ratchet + canary triple.

**Remaining pressure:** this is still a live-looking class. It is not asking
"are there direct `append*` calls?" only. It asks:

```text
which recurring writes change stack state but do not yet have one owner, one
shape, one anti-bypass check, and one read-back/ratchet witness?
```

That is broader than evidence. Candidate write classes now include agent roster
persistence, mission-state saves, consent-gate events, staged outbox deposits,
substrate ingestion, ArSE ask/answer writes, and bridge/feeder writes such as
VSATARCS ingestion.

**Scan probe:** search for state-changing verbs (`append`, `save`, `persist`,
`register`, `deposit`, `ingest`, `send`, `answer`, `ask`) and classify each
write class by whether it has:

1. one boundary namespace/function;
2. a declared shape;
3. a no-bypass static check or runtime guard;
4. a durable witness/read-back check;
5. an operator-visible failure mode.

### deferred-stub lift

**Prototype sources:** `track-1-substrate-2-lift` and part of
`track-2-war-machine-aif-lift`.

**Evidence found:** `src/futon3c/logic/probe_taps.clj` still registers deferred
taps for two six-family populations:

- `substrate-2-phase-1-invariants`;
- `war-machine-aif-invariants`.

The deferred tap is honest: probe sweeps show `:outcome :inactive` with
`:deferred? true` and detail explaining the source and follow-on. This is better
than invisible absence.

**Remaining pressure:** the live question is now:

```text
which deferred probe families are still only placeholders, and for each, is the
next honest move to lift it, defer it with criteria, or retire it as superseded?
```

Substrate-2 may have moved since the original six-family list; War-Machine has
also grown into scheduler/operator-lane machinery. A lift scan should not assume
the 2026-04-29 family list is still the best one.

**Scan probe:** read the registry snapshot and probe-taps source, then join each
`:deferred? true` family to current mission docs and tests. Classify as
`:lift-now`, `:needs-source-update`, `:defer-with-criteria`, or `:retire`.

### principled deferral

**Prototype source:** the "OR documented deferral" branch of
`track-2-war-machine-aif-lift`.

**Evidence found:** the prototype itself already encodes an important
alternative to "build it": a deferred invariant may be closed by a recorded
decision to wait for the owning mission, but only with criteria for revisit.
`M-invariant-queue-extend` repeats that standard in its completion criteria.

**Remaining pressure:** this is a separate feature class from deferred-stub
lift. The missing feature is not a check-fn; it is a deferral protocol:

```text
how does the stack record that a live-looking obligation is intentionally not
being acted on, and what event wakes it back up?
```

Without revisit criteria, "deferred" becomes indistinguishable from forgotten.
With criteria, it can be a valid terminal state for a tracer stage.

**Scan probe:** find `Status: DEFERRED`, `parked`, `held`, `blocked`, and
`deferred-stub` records, then require a reason, owner/source, and revisit
trigger. Rows missing one of those become feature candidates for a deferral
schema/readout.

### ontology/narrative registration

**Prototype source:** `track-5-vsatarcs`.

**Evidence found:** VSATARCS is no longer merely a parked narrative. There are
current artifacts around:

- `futon7/holes/vsatarcs-doc-integrity.md`, which identifies stale registry
  statuses and undocumented capabilities, with the action "regenerate VSATARCS";
- `src/futon3c/vsatarcs/feeder.clj`, which stages/ingests substantive pilot
  events into the VSATARCS alignment evidence;
- multiple `M-war-machine-aif-last-mile` checkpoints coordinating WM/VSATARCS
  alignment and porting.

So the old "parked -> re-park or register" binary is stale. The current class is
more like:

```text
which narrative/ontology surfaces lag the real substrate, and what machinery
keeps them synchronized without pretending they are executable checks?
```

**Remaining pressure:** VSATARCS-like artifacts are important because they
compress the stack into a readable narrative/ontology. They can drift even when
the code is correct. The feature gap is a registration/freshness contract for
prose/ontology surfaces.

**Scan probe:** enumerate narrative/ontology artifacts that claim stack state
(`VSATARCS`, holistic arguments, stack essays, forward-model mints), compare
their cited statuses/capabilities against current mission docs and substrate
edges, and classify each mismatch as `:regenerate`, `:cross-link`,
`:schema-extension`, or `:retire-claim`.

### Summary

The classes split into two groups:

1. **Already partly operational, now reusable as scan templates:**
   witness/schema, operator-readout, deferred-stub lift.
2. **Still conceptually live as missing feature families:**
   boundary-generalization, principled deferral, ontology/narrative
   registration.

The next bounded prototype should probably pick one class and produce an EDN
scan, not another prose list. The best candidate is `boundary-generalization`:
it is current, cross-stack, and naturally yields rows of the form
`write-class -> boundary? -> shape? -> guard? -> witness? -> readout?`.

## Checkpoint 5 — library-pattern characterization

`holes/excursions/pipeline-pattern-map.edn` maps the six scan-template classes
onto existing `~/code/futon3/library` patterns. This changed the read of several
classes:

- `witness/schema` is broader than state snapshots. `state-snapshot-witness` is
  one technical subcase, but the larger pattern is
  `agent/evidence-over-assertion` + `futon-theory/curry-howard-operational`:
  mission claims want checkable witnesses/proof paths. Pudding Prover and
  M-typed-holes are the current stronger anchors here.
- `operator-readout` is mostly covered by existing operator surfaces (War
  Machine's operator view, Arxana/HUD). The distinct question Joe raised about
  reading his behaviour from logged turns belongs more to
  `peripherals/inhabitation-feeds-evolution` and
  `aif/structured-observation-vector`: behaviour readout as learning-loop data,
  not just a dashboard.
- `deferred-stub lift` is specialist. The pattern is
  `invariant-coherence/subsumption-witness`: a deferred stub is a latent artifact
  A that should be retired when a stronger canonical record P exists, or carried
  honestly when P does not.
- `boundary-generalization` remains a live missing-feature family, characterized
  by `agency/single-routing-authority`,
  `invariant-coherence/protocol-family-naming`, and
  `peripherals/canonical-typed-event-vs-side-channel`.
- `principled-deferral` is not merely "wait"; it wants
  `agent/provisional-claims-ledger`, `agent/pause-is-not-failure`, and gated
  examples such as `correspondence-coherence/mission-unlocks-eoi`.
- `ontology/narrative-registration` wants timeline/canonicalization patterns:
  `sidecar/per-id-audit-timeline-linkage`,
  `futon-theory/retroactive-canonicalization`, and
  `futon-theory/single-source-of-truth`.

The EDN map is the better seed for the next scan than the original
`pipeline-prototype.edn`: it preserves the queue's pressures but grounds each
pressure in library vocabulary before any generated scan rows are attempted.

## Checkpoint 6 — Moran-style graphical prototype

`holes/excursions/pipeline-pattern-cascade.html` is a first static graphical
display populated from `pipeline-pattern-map.edn`.

The visual grammar follows the Moran/MSC cascade discussed in
`futon5a/essays/moran-1971-agent-cascades/reading-moran-1971.md`: pattern nodes
are wired by the contexts they create for one another, and the tangle is a
feature because it exposes the control problem. In this prototype:

- filled dark nodes are existing `~/code/futon3/library` patterns;
- pale nodes are apparatus that partly exists but does not exhaust the larger
  question;
- hollow nodes are expected holes where the current pattern vocabulary suggests
  a new scan/pattern should exist;
- dashed edges are speculative/missing dependencies.

The six initial holes shown are:

1. mission witness inventory;
2. behaviour readout vector;
3. write-class scan;
4. lift/retire classifier;
5. deferral schema;
6. narrative freshness scan.

This display is deliberately not a roadmap. It is an adviser-facing control
diagram: current patterns create contexts where the stack should either discover
a new pattern, run a scan over current artifacts, or decide that the apparent
hole is already satisfied elsewhere.

Revision: the display now separates three layers:

1. library patterns;
2. concrete apparatus warranted by those patterns;
3. remaining holes where no common apparatus/pattern exists yet.

`pipeline-pattern-map.edn` now records the same relation explicitly with
`:apparatus` entries and `:warrant-patterns`, so the SVG layer has a data-backed
interpretation rather than being only a drawing convention.

Revision 2: the display now also reads **upward** toward
`M-stack-stereolithography`. `pipeline-pattern-map.edn` has a
`:built-capabilities` layer with four small capability-state cards, and a
`:feature-warrant-layer` that makes the pattern warrants visible above those
cards:

1. self-describing stack documentation (`:exists-already`, sourced from
   `futon7a/vsatarcs.html`);
2. operator-visible stack state (`:exists-already`, sourced from War
   Machine/Arxana-style readouts);
3. evidence-backed claim flow (`:emerging`, sourced from Pudding Prover,
   typed holes, and evidence boundaries);
4. capability stereolithography printout (`:declared`, sourced from
   `M-stack-stereolithography` itself).

This is deliberately only a slice of VSATARCS, not a full import. The point is
to show the same semilattice in both directions:

- downward: current patterns and apparatus imply missing scan/pattern holes;
- upward: current patterns and apparatus also warrant existing or emerging
  stack capabilities.

That makes the diagram a hand-built prototype of the stereolithography
printout: capability cards carry status, source, support classes, and
pattern-backed warrants, while the lower layer still exposes the holes that
would be needed to generate the printout rather than maintain it by hand.

Revision 3: the HTML now draws those warrant patterns *above* the four attested
features. The visual stack is:

1. pattern warrants for each attested feature;
2. attested/emerging/declared capability cards;
3. scan classes that support or pressure those capabilities;
4. apparatus cards and holes below each class.

Revision 4: the diagram now adds a one-story **forward-model** strip below the
`held/deferred schema` hole. This strip is not claimed as existing apparatus.
It says what building the schema would buy:

- bounded deferral memory;
- operator-safe re-entry;
- mission pressure signal;
- a pattern-seeding loop.

The corresponding EDN entry is `:held-deferred-schema-story` under
`:forward-model-takes`. It is deliberately marked `:prospective`, with
`:attested-by-existing-patterns []`, because this is the case where the diagram
is most clearly informed by expected/non-existent patterns. The candidate
patterns seeded by the story are:

- `agent/held-item-wake-trigger`;
- `agent/deferral-ledger-as-operator-memory`;
- `mission/deferred-work-reentry-protocol`.

Revision 5: the HTML now includes a prose version of this forward-model story
at the bottom of the page, below the diagram. The prose block states the
argument in words, lists the four gains, and gives one-line definitions for the
three candidate patterns. This keeps the graphic from being the only carrier of
the forward-model claim.

## Checkpoint 7 — Toward a Real Capability Semilattice

Joe's observation after the forward-model story: the stack has hundreds of
patterns and hundreds of missions, so the demo is not intrinsically limited to
six hand-picked scan classes. A model like this could plausibly be built for
the whole stack.

The important correction is that `futon3/library/...` is not itself a cascade.
It is currently a flat collection of pattern namespaces and flexiarg files. A
quick count on 2026-06-16 found:

- 74 top-level pattern-library directories under `futon3/library`;
- 1,010 `.flexiarg` pattern files;
- 52 `.aif.edn` story graphs under `futon5a/holes/stories`;
- 101 story markdown files under `futon5a/holes/stories`.

The observed pattern cross-references are still valuable, but they mainly give
**phylogeny**: which patterns cite, reuse, descend from, or were lifted through
which earlier patterns. That is not the same thing as logical structure.

For the stereolithography/VSATARCS problem, the more useful structure is a
capability semilattice:

- what capability exists or is emerging;
- which story claims support it;
- which apparatus realizes it;
- which patterns warrant the claim;
- which holes pressure it;
- which missions or invariants close, instantiate, or generalise it.

The VSATARCS AIF stories already contain part of this logical material. For
example, `leaf-6-4-4.aif.edn` uses edges such as `:supports`, `:closes`,
`:generalises-shape-to`, `:extends-coverage-of`, and `:feeds-crystallisation-of`.
`leaf-cycle.md` gives a prose chain from work to proof paths, proof paths to
patterns, patterns to coordination, coordination to understanding, and
understanding back to argument. Those are semilattice edges, not just pattern
lineage.

So the next substrate should not be "draw every pattern as a node." It should
derive capability rows from multiple surfaces:

1. VSATARCS/AIF stories for logical support and story claims;
2. the pattern library for warrants and lineage;
3. invariant surfaces for operational/candidate status;
4. mission docs and apparatus paths for realized-by / pressured-by evidence.

Cross-reference: this is adjacent to `M-capability-star-map`
(`futon0/holes/missions/M-capability-star-map.md`) and should not rediscover
that mission's structure-view from scratch. `C-pudding-prover` §8.2 records the
useful split:

- Pudding Prover registry = **status-view** over the capability set;
- `M-capability-star-map` = **structure-view** over the same capabilities:
  `:produces`, `:requires`, toposort, graph shape, and EFE-over-graph scheduler.

This excursion's graphic is therefore best read as a tiny explanatory slice
that bridges VSATARCS prose, pattern warrants, and capability-star-map
structure.

`pipeline-pattern-map.edn` now records this as `:semilattice-next-plan`, with a
candidate node vocabulary:

- `:capability`;
- `:pattern`;
- `:mission`;
- `:story-claim`;
- `:apparatus`;
- `:hole`;
- `:invariant`;
- `:source-surface`.

and a candidate edge vocabulary:

- `:supports`;
- `:instantiates`;
- `:realized-by`;
- `:warranted-by`;
- `:pressured-by`;
- `:closes`;
- `:generalises-shape-to`;
- `:depends-on`;
- `:exposes-hole`;
- `:seeds-pattern`.

The right next bounded build is therefore a tiny derived semilattice EDN from
3-5 VSATARCS AIF stories plus their cited patterns, rendered with the same
visual grammar as this prototype. That would test whether the demo's layers can
be generated from real stack evidence rather than authored by hand.

### Clustering Note

The 1,010 flexiarg files are large enough that an embedding-based clustering
pass would help make sense of the pattern library, but the clustering should be
split by role:

- **Pattern-only clustering** should discover warrant vocabularies and latent
  pattern families. This helps explain what the library can say, but does not
  by itself say what features exist in the stack.
- **Mission-only clustering** should discover feature regions. Missions are
  closer to "what is actually in the stack?" because they bundle apparatus,
  goals, status, and evidence.
- **Mission→pattern cross-references** should then join the two clusterings.
  These edges are explicit warrants: a mission or feature region is backed by
  these patterns, not merely close to them in embedding space.

So the proposed analysis shape is:

1. cluster missions independently to get feature basins;
2. cluster patterns independently to get warrant constellations;
3. overlay explicit mission→pattern citations and high-confidence embedding
   neighbours;
4. compare the resulting feature basins with `M-capability-star-map.graph.edn`.

The risk to avoid is a single undifferentiated embedding plot where missions
and patterns blur together. That would be good for retrieval, but weaker as a
capability semilattice.

## Checkpoint 8 — Two Clusterings Plus Warrant Links

Built the first derived two-space cluster model:

- `scripts/stack_semilattice_clusters.py`;
- `holes/excursions/pipeline-semilattice-clusters.edn`;
- `holes/excursions/pipeline-semilattice-clusters.md`.

Inputs:

- missions: `futon3a/resources/notions/bge_mission_embeddings.json`
  (229 embedded missions, BGE);
- patterns: `futon3/library/**/*.flexiarg` (1,010 pattern files);
- warrant links: `futon6/data/mission-pattern-scopes.edn`
  (`:applied` mission→pattern links).

Method:

- missions are clustered in the existing BGE embedding space;
- patterns are clustered separately using TF-IDF over flexiarg text followed by
  truncated SVD, because no current pattern BGE embedding file was found in the
  workspace;
- mission→pattern warrant links use explicit `:applied` pattern references, not
  embedding proximity;
- each side is hierarchical: coarse clusters plus within-cluster subclusters.

Output counts:

- 12 mission clusters over 229 missions;
- 18 pattern clusters over 1,010 patterns;
- 550 resolved applied-pattern warrant links.
- 12 temporal levels over mission clusters, using the same coarse anchor rule as
  `piano_roll.py`: latest dated status stamp, falling back to `**Date:**`.

Reading for `M-stack-stereolithography`:

- mission clusters are candidate **feature basins**;
- pattern clusters are candidate **warrant constellations**;
- applied mission→pattern links are first-pass `:warranted-by` edges between
  those two spaces.

This is not yet the canonical capability star-map. It is a derived comparison
surface: if a mission cluster aligns with a capability star, and its explicit
pattern links concentrate in a few warrant constellations, that gives a
candidate semilattice row of the form:

```clojure
{:capability ...
 :feature-basin <mission-cluster>
 :warranted-by <pattern-clusters>
 :missions [...]
 :patterns [...]
 :status <from star-map / pudding / stereolithography>}
```

The next useful move is to join this cluster model against
`M-capability-star-map.graph.edn`: for each capability star, list which mission
clusters contain its `:minted-by` missions and which pattern clusters warrant
those missions.

Temporal-level note: the piano-roll direction is a useful constraint on the
semilattice. A mission or feature may be **elaborated** later by a later
mission, but it is suspicious for its **completion** to depend on that later
mission. So temporal levels should not replace semantic dependencies, but they
should flag impossible or suspicious prerequisite edges:

```clojure
{:edge-kind :completion-prerequisite
 :from later-mission
 :to earlier-completed-mission
 :verdict :suspicious-unless-reclassified-as-elaboration}
```

This gives the cascade a direction without flattening it into a tape: the
result is still a graph, but one whose completion-dependency edges should
mostly respect the piano-roll order.

### Checkpoint 8a — Temporal Edge Audit

Added a derived data-quality audit to
`scripts/stack_semilattice_clusters.py` and regenerated
`pipeline-semilattice-clusters.{edn,md}`. The audit now uses Git history as a
fallback anchor when a mission lacks an in-document date: explicit status/date
stamps still win, but otherwise the mission file's first commit date is recorded
as `:git-first-commit`.

The audit reads explicit `M-*` references in mission markdown files as
candidate mission→mission edges, classifies the local wording conservatively,
and applies one hard temporal sanity check:

```clojure
{:edge-kind :completion-prerequisite
 :source earlier-mission
 :target later-mission
 :verdict :suspicious-forward-prerequisite}
```

Current result:

- 824 candidate mission-reference edges;
- temporal anchor sources:
  - `:date-field` 105;
  - `:early-text-date` 53;
  - `:status-stamp` 39;
  - `:git-first-commit` 29;
  - `:missing` 3;
- edge kinds:
  - `:completion-prerequisite` 13;
  - `:retrospective-warrant` 128;
  - `:elaboration` 45;
  - `:cross-reference` 638;
- verdicts:
  - `:ok` 729;
  - `:ok-as-elaboration-or-reference` 91;
  - `:needs-dates` 3;
  - `:suspicious-forward-prerequisite` 1;
- suspicious forward prerequisites: 1.

The Git fallback reduces the date gap from 96 edges to 3. It also surfaces one
candidate temporal contradiction:

```clojure
{:source-mission "M-peripheral-model"
 :source-date "2026-02-10"
 :source-date-source :git-first-commit
 :target-mission "M-forum-refactor"
 :target-date "2026-03-08"
 :target-date-source :status-stamp
 :context "S-validate: Coordination outcome validation — needs forum (M-forum-refactor)"}
```

This should be reviewed as data quality rather than treated as a proven
logical error. It may mean `M-peripheral-model` was committed before its
validation dependency was actually ready, or it may mean "needs forum" was an
elaboration/future-validation note rather than a completion prerequisite.

This is a useful precondition for extending the Pipeline Pattern Cascade
upward: we can use temporal levels as a weak ordering constraint while treating
later references as elaboration, retrospective warrant, or cross-reference
unless the prose explicitly says "depends on / requires / made possible by".

### Checkpoint 8b — Direct Pattern Warrants Per Mission Cluster

The upward direction of the Pipeline Pattern Cascade does not need pattern
clusters as its first support layer. Pattern clusters may still help name
warrant constellations, but the stronger immediate edge is:

```clojure
mission-cluster -> concrete patterns actually cited by missions in that cluster
```

`pipeline-semilattice-clusters.edn` now records this under
`[:warrant-links :by-mission-cluster]`, and
`pipeline-semilattice-clusters.md` renders it as "Direct Pattern Warrants By
Mission Cluster".

Current direct-warrant surface:

- 6 of 12 mission clusters have explicit applied-pattern citations;
- M0 is the dense general stack basin:
  - 211 cited patterns;
  - 76 citing missions;
  - top examples include `futon-theory/stop-the-line`,
    `invariant-coherence/reachable-from-boot`,
    `math-informal/reduce-to-known-result`,
    `futon-theory/structural-tension-as-observation`, and
    `peripherals/surface-earns-inhabitation`;
- smaller directly warranted basins include:
  - M1 peripheral/essay/parity work: 29 cited patterns across 6 missions;
  - M5 training/research-system work: 23 cited patterns across 2 missions;
  - M6 Codex/IRC/Agency work: 5 cited patterns across 4 missions;
  - M11 WebArxana/UI work: 4 cited patterns across 1 mission.

This gives the upward cascade a simpler rule:

1. cluster missions into feature basins;
2. attach concrete cited patterns as warrants;
3. only then optionally cluster patterns to name higher-level warrant
   vocabularies.

Clusters without direct pattern citations should be rendered as feature basins
with weak or missing pattern warrants, not hidden. That absence is itself a
useful hole signal for `M-stack-stereolithography`: either the missions need
explicit pattern back-links, or the current feature basin is not yet backed by
library-pattern discipline.

### Checkpoint 8c — M0 Split By Dominant Cited Pattern Cluster

M0 is the dense general stack basin, so the pattern clusters become useful as a
secondary internal split. The current derivation assigns each citing M0 mission
to the pattern cluster it cites most often, then summarizes those groups in
`[:warrant-links :dominant-pattern-cluster-breakdown]`.

M0 should be treated as a family of refined basin nodes, not as one large
feature:

- M0P1: 15 missions — coordination, stop-the-line, all-or-nothing,
  source-of-truth patterns;
- M0P2: 20 missions — AIF/operator/surface-inhabitation patterns;
- M0P13: 11 missions — math proof strategy and rational reconstruction
  patterns;
- M0P6: 8 missions — runtime invariants, boot reachability, snapshots;
- M0P0: 7 missions — writing, pattern discipline, categorical objects.

This is the right role for pattern clustering in the upward cascade: split a
too-large mission basin according to the basin's own cited-pattern vocabulary,
without replacing the concrete mission→pattern citation layer.

The next semilattice-rendering algorithm should be **inductive**:

1. start from already-attested capability nodes and scan classes;
2. attach the next mission basin where its concrete cited-pattern warrants
   overlap an existing node's warrant vocabulary;
3. if a basin has several maximally informative cited-pattern vocabularies,
   split it into refined nodes like `M0P1`/`M0P2` before attaching;
4. leave basins with no warrant overlap as visible holes rather than forcing a
   link.

This keeps the cascade from becoming a flat cluster plot. Each new upward node
has to earn its attachment from an existing edge: cited pattern overlap first,
then temporal sanity, then embedding proximity only as a weaker suggestion.

### Checkpoint 8d — First Promoted Blocks In The Semilattice Slice

`pipeline-pattern-cascade.html` now starts moving blocks out of the "Upward
Structure" holding area and into the main "Capability semilattice slice".

The first promoted blocks are:

- `M0P1` → coordination law;
- `M0P2` → AIF / operator;
- `M0P13` → proof strategy;
- `M0P6` → runtime witness;
- `M0P0` → pattern discipline;
- `M1` → peripheral parity;
- `M2` → readiness pilot;
- `M5` → training research;
- `M6` → agency realtime;
- `M11` → WebArxana graph.

They are drawn as first-class blocks above the existing capability cards, with
explicit dashed attachment edges into the current semilattice:

- `M0P1` attaches toward self-describing docs;
- `M0P2` attaches toward operator-visible state;
- `M0P13` and `M0P6` attach toward evidence-backed claims;
- `M0P0` attaches toward capability printout / pattern discipline.

Each promoted block now also has concrete pattern warrants above it, preserving
the visual grammar:

- `M0P1`: `stop-the-line`, `par-as-obligation`,
  `single-source-of-truth`;
- `M0P2`: `expected-free-energy-scorecard`,
  `surface-earns-inhabitation`, `term-to-channel-traceability`;
- `M0P13`: `reduce-to-known-result`, `construct-an-explicit-witness`,
  `compose-independent-lemmas`;
- `M0P6`: `reachable-from-boot`, `state-snapshot-witness`,
  `drain-channel-shape`;
- `M0P0`: `patterns-as-categorical-objects`,
  `meet-the-reader-where-they-are`, `name-what-you-drop`.
- `M1`: `pattern-as-strategy`, `single-routing-authority`,
  `evidence-over-assertion`;
- `M2`: `sense-deliberate-act`, `expected-free-energy-scorecard`,
  `predictive-coding-belief-update`;
- `M5`: `argue-empirically-not-persuasively`,
  `belief-state-operational-hypotheses`, `candidate-pattern-action-space`;
- `M6`: `structured-events-only`, `all-or-nothing`,
  `single-source-of-truth`;
- `M11`: `live-sync-source-truth`, `deep-storage-to-active-graph`,
  `present-graph-topology-not-adjacency-lists`.

This is still a hand-built first promotion, but it establishes the visual
grammar for the next algorithmic step: repeatedly promote one warranted basin,
attach it to the closest already-attested semilattice node, and split it first
if its cited-pattern vocabulary is mixed.

These attachment edges are deliberately "best guess" for now. A later pass
should refine them against substrate-2 attested links: code dependency,
hypergraph support, or other substrate edges should confirm which promoted
blocks actually depend on which existing blocks. Until then, the rule is to
populate the semilattice visibly while keeping the edges soft and revisable.

### Checkpoint 8e — Weak Blocks Promoted As Visible Holes

The remaining mission clusters with no direct applied-pattern citations are
now also present in the main semilattice slice as weak promoted blocks:

- `M3` — maintenance/probe;
- `M4` — forum organization;
- `M7` — perfect-crime singleton;
- `M8` — stack HUD;
- `M9` — pattern discovery;
- `M10` — reader evaluation.

They use hollow `?` pattern nodes above them instead of concrete pattern
warrants. This preserves the cascade rule while making the absence of warrants
visible. These nodes are not absent features; they are semilattice frontier
nodes whose next useful work is either:

- add explicit mission→pattern back-links if the warrant already exists;
- seed new patterns if the work has recurring shape but no library pattern;
- or demote/merge the block if substrate-2 later shows it is not a distinct
  dependency.

### Checkpoint 9 — First Downward Forward-Model Layer

Started growing the semilattice downward with reference to
`M-futon-forward-model`. The visual now has a separate downward projection panel
below the main semilattice, and `pipeline-pattern-map.edn` records the same
layer as `:downward-forward-model-layer`.

The first downward chain is:

```text
backlog → basin
  → valuable path
  → market × interest
  → ROI / cash gate
  → piano-roll trace
  → selected pipeline
```

This follows `M-futon-forward-model`'s discipline:

- the semilattice/backlog map is the ontology;
- the piano roll is a time projection over that graph;
- a linear pipeline is emitted only for one selected pressure/path;
- each pipeline row should carry a pointer back to the semilattice node and
  scan witness that justify it.

This is still a best-guess projection layer, but it starts to connect the
current cascade to the concrete forward-model artifacts:

- `M-futon-forward-model.semilattice.edn`;
- `M-futon-forward-model.roadmap.edn`;
- `M-futon-forward-model.roi-results.edn`;
- `M-futon-forward-model.projected-piano-roll.svg`.

### Checkpoint 9a — Lower Staging Area Populated

The downward panel now includes "Roadmap Deliverables Staged From Below",
populated from `M-futon-forward-model.roadmap.edn` and joined mentally to the
ROI pass in `M-futon-forward-model.roi-results.edn`.

`pipeline-pattern-map.edn` records the same pass as
`:downward-roadmap-staging`, with a first-pass warrant status:

- `:strong` — several concrete current-library patterns already warrant the
  feature shape;
- `:partial` — adjacent patterns exist, but the product/feature shape is not
  fully covered;
- `:missing` — the feature likely needs a new library pattern.

Current first-pass mix:

- strong: story-as-service, proof-pipeline SaaS, living docs, repro/research
  QA, pattern atlas API, evidence audit, agent orchestrator;
- partial: formalization audit, invariant-as-service, manifold dashboard,
  audience intelligence, Mission Control PM, consent autopen, frontier
  detector, living KG, AIF eval harness, learning diagnostic, REPL cockpit;
- missing: cyborg HUD / operator-augmentation interface.

This is useful because it shows that the future-feature layer is not uniformly
unsupported. Some features are already strongly warranted by the current
pattern library; others are real frontier pressure for pattern seeding.

### Checkpoint 9b — Certification Feeds For Realized And Future Cards

The lower staging area now records two additional certification sources besides
`M-futon-forward-model.roadmap.edn`:

- capability stars from `futon0/holes/missions/M-capability-star-map.graph.edn`;
- Pudding levels from `futon7/holes/pudding-prover-registry.edn`.

Joe's correction is important: these resources do not only supply future
pressure. They can certify both directions in the cascade:

- satisfied capability stars and cleared Pudding levels can warrant past or
  realized cards;
- held/active capability stars, held sorries, and open observables can seed
  future cards and missing-pattern work.

Current measured source shapes:

- capability star map: 37 capability nodes; 23 `:satisfied`, 13 `:held`, 1
  `:active`;
- Pudding registry: 47 sorries; 24 `:satisfied`, 22 `:held`, 1
  `:contract-released`; plus 8 observables, 7 of them `:open`.

The HTML now shows these as "Certification Feeds: Realized And Future Cards"
below the roadmap staging grid. They are deliberately represented as source
feeds rather than full semilattice cards:

- the star map names capability-frontier stars such as distributed proofreaders,
  cold EOI send/response/conversion, full ArXiv mining, symbol grounding, AI
  prelims, and overnight War Machine trust;
- the Pudding registry names unchecked proof levels such as T2.2--T2.5,
  T3.3--T3.4, T4.3--T4.4, T5.1--T5.2, and open observables such as
  cross-mission unlocking and semilattice rollout reconciliation.

The next refinement would be to turn each repeated certification family into a
proper cascade attachment with:

- a semilattice attachment point;
- current pattern warrants, if they exist;
- missing-pattern seeds, if they do not;
- and a scan/proof witness back to the star map or Pudding registry.

### Checkpoint 10 — Pipe Cleaned; Whole-Backlog Roadmap Deferred

This excursion has achieved its first goal: the pipe was cleaned.

Concretely:

- the stale `pipeline-tracer` dataset was identified as historical sediment, not
  live operational pressure;
- the obsolete queue contents were preserved as `pipeline-prototype.edn` instead
  of remaining wired as a warning source;
- the scan-template classes were read against the pattern library and turned
  into a visual cascade prototype;
- the cascade was grown upward from mission clusters and concrete pattern
  warrants;
- the cascade was grown downward from `M-futon-forward-model`, roadmap/ROI
  artifacts, capability-star states, and Pudding Prover states.

What remains deliberately unresolved is the stronger roadmap question: whether
the whole backlog can or should be collapsed into a linear plan. The current
answer is no stronger than this:

- a linear roadmap can be useful for a selected path;
- the selected path must point back to a cascade/semilattice attachment point;
- the attachment point should be certified by scans, pattern warrants,
  capability stars, or Pudding proof states;
- otherwise the line is just a projection with no visible ontology underneath.

So the follow-up is not "make the whole backlog linear." It is: refine the
cascade attachment algorithm, then emit linear traces only for selected,
certified paths.

## Checkpoint 11 — The lifecycle made visual: meme → arrow (sorry) → wiring, and cascades as applied chaos

A worked, pattern-backed visual was built for one discrete, valuable, buildable
backlog item — `M-symbol-grounding` (a held capability star) — to answer a
question `M-typed-holes` left open: it gave *computational* confirmation that
holes-and-fills are produced, but there was no *visual* confirmation that Joe's
specific ordering `cascade → sorry → wiring diagram` is actually followed. In
building the picture (Joe ↔ claude-6, 2026-06-16) the lifecycle itself got
sharpened, and the sharper version **supersedes
`futon-theory/derive-exits-on-a-minted-sorry`**.

### The artifact

`holes/excursions/cascade-sorry-wiring-symbol-grounding.html` — a standalone
SVG sketch in the Moran-1971 cascade grammar. It is pattern-backed, not mocked:

- cascade nodes are `M-symbol-grounding`'s **8 real applied patterns**
  (`futon6/data/mission-pattern-scopes.edn`) on an inner ring, plus real
  grounding-cluster **neighbour patterns** (co-cited by `M-diagramprover`,
  `M-stack-inhabitation`, `M-categorical-code`, `M-structure-seed-promotion`)
  on an outer ring;
- the sorry's `have/want` is the mission's IDENTIFY example,
  `$T:\mathcal{C}\to\mathcal{C}$` opaque → grounded markup;
- the wiring diagram uses the mission's own `nlab-wiring` vocabulary
  (`bind/let`, `constrain/such-that`, typed `input_ports`/`output_ports`).

It is explicitly a **sketch**. A *real* sorry would have to be evidenced by
entities in **substrate-2**, which the cascade's concepts effectively **bind**.
That binding is the line between the drawing and a live sorry.

### The refined lifecycle (supersedes `derive-exits-on-a-minted-sorry`)

The `(have → want)` object runs through three states — exactly the meme store's
`proposal → arrow → promote-to-fact` machinery:

| Phase | Object | State |
| --- | --- | --- |
| IDENTIFY | `(have, want)` **meme** — the felt gap, a *proposal* | proposal |
| MAP | chart the field that exists | — |
| DERIVE | the meme is a **query-magnet**; the **cascade** condenses *around* it | proposal (+ correlation halo) |
| ARGUE | fit the cascade to actual system conditions → **upgrade meme → arrow = the sorry** | **arrow** |
| VERIFY | turn the sorry into the **wiring diagram** (the construction) | **fact** |

Phase by phase:

- **IDENTIFY** names a `(have, want)` pair. This is a **meme**, *not yet a
  sorry* — a proposal that has not been upgraded into an arrow.
- **DERIVE** uses the meme as a **magnet**: querying the pattern field with it
  makes a **cascade** condense *around* it. The cascade is **correlation**, not
  construction; it **expresses** the already-identified gap — it does **not
  propose** it. (The old reading — cascade proposes/originates the gap — is the
  documented anti-pattern: a back-filled problem node.)
- **ARGUE** does the **upgrade `meme → arrow = sorry`** by fitting the cascade
  to actual system conditions. ARGUE argues the gap is *real*, has *observable
  aspects*, and that *we have the right ideas to fill it*. The result is a sorry
  with **ports identified + a reasonable structural sketch**, but whose **exact
  construction is not yet known** — i.e. still a Kolmogorov problem.
- **VERIFY** turns the sorry into a **wiring diagram** — a *"simple matter"*
  once the ports are known. The wiring diagram is the **BHK construction /
  code-object witness**: evidence that an executable realizer exists that
  transforms `cascade —via the sorry→ a real working solution`, and runs.

**What changed from the old pattern.** `derive-exits-on-a-minted-sorry` had the
sorry minted at DERIVE's exit and the cascade *assembled against it* in ARGUE.
The update: (1) the DERIVE object is a **meme**, not yet a sorry; (2) the sorry
is a **retrieval magnet** the cascade condenses *around*, not a target the
cascade is built against; (3) the **meme→arrow upgrade is ARGUE's job** and is
done by *fitting to a real system-hole* (substrate-2), not by assembling
patterns; (4) the wiring diagram is a **VERIFY** witness, not the DERIVE/ARGUE
product.

### Grounding discipline

A real sorry is **substrate-2 evidence bound by cascade concepts**. So the
differentiable/embedding machinery below lives on the **DERIVE side** (propose
candidate cascades fast), and **substrate-2 binding is the ARGUE gate** that
promotes meme → arrow. The gradient field gives *proposals*; substrate-2
evidence is what makes a sorry real.

### Why it matters — cascades as applied chaos

Joe's framing: building cascades should get *easier and easier* if we treat them
as **applied chaos**. "Cascades are always skiing *down* Mount Analogue while we
climb *up* it." Reference: **moguls of chaos**
(`https://cpierard.github.io/projects/moguls-of-chaos/`) — a board on a
periodically bumpy slope is a low-dimensional ODE with sensitive dependence, a
horseshoe-map strange attractor, and a **bifurcation to chaos at a critical bump
height**.

The mapping is tighter than metaphor, because most legs already exist:

- **the mogul field is already measured.** `M-differentiable-math` computes
  **Ollivier–Ricci curvature on the BGE embeddings** and confirmed it *marks
  bottlenecks*. That curvature field is the moguls.
- **retrieval is downhill, development is uphill** — same mountain, opposite
  directions. The piano-roll is the climb; the cascade is the ski.
- **the new bridge: a time-skewed embedding.** Skew the pattern embedding so it
  follows the FUTON stack's temporal order (the `piano_roll.py` levels). Joe's
  cleanest version: **embed the dated git commits and decorate them multiply
  with the patterns they touch** — and the temporal anchor is already wired
  (Checkpoint 8a used `:git-first-commit` dates). That warps the static cosine
  magnet into a *flow* down a developmentally-oriented slope.
- **the substrate is differentiable.** `M-differentiable-substrate` is at
  INSTANTIATE; JAX is already running (`futon6/scripts/code_diff_jax_pilot.py`,
  `diffsub_emit.py`; `futon5/tools/tensor/jax_step.py`, `tpg/jax_refine.py`).
  So cascade assembly can be a **JAX descent** on the mogul field, not top-k
  cosine.
- **chaos as a sorry-quality diagnostic.** Cascade-formation *sensitivity* is a
  criterion: a meme whose cascade sits in a **stable basin** (small magnet
  perturbation → same cascade) is well-posed/buildable; one in the **chaotic
  regime** (tiny perturbation → wildly different cascade) is ill-posed. The
  Pierard bifurcation threshold becomes the line.

### The keystone claim — the lifecycle is learnable

Joe's claim that makes "build cascades by JAX" more than a slogan: **we can
embed the cascade, embed the arrow (sorry), and embed the wiring diagram** — all
three lifecycle objects are points/structures in the same differentiable space.
Therefore the transitions `meme → arrow → wiring` are **learnable maps** in
embedding space, not hand-authored steps. That is the mechanism by which cascade
construction "gets easier and easier": each completed `meme → arrow → fact`
trajectory is a training example for the maps that produce the next one. Reason
about the resulting objects with **graphical logic** (the wiring diagram already
*is* a string diagram).

### Smallest real probe (deferred — "figure out how it all works later")

Not yet built. The minimal experiment that would test the whole thesis, using
only existing pieces: take the pattern/mission embedding + the
already-confirmed Ollivier–Ricci curvature, warp it along the
piano-roll/git-commit time axis, drop the `M-symbol-grounding` meme-magnet on
it, integrate the descent in JAX, then **perturb the magnet** and watch whether
the cascade is a stable basin or bifurcates. Natural home: an excursion
alongside `M-differentiable-math`.

Cross-refs: `M-typed-holes`, `M-memes-arrows-patterns-diagrams` (+ its
BHK-research note: cascade = correlation, sorry = Kolmogorov problem, BHK
arrow = wiring diagram = construction), `M-differentiable-math`,
`M-differentiable-substrate`, `futon5a/scripts/piano_roll.py`,
`holes/excursions/pipeline-pattern-cascade.html`.

Status: sketch artifact done and reviewed; the differentiable/learnable
mechanism is **optative**, recorded here as the next direction, not yet
demonstrated.
