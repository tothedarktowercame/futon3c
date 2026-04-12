# Mission: War Machine

**Date:** 2026-04-10
**Status:** INSTANTIATE (IDENTIFY→MAP→DERIVE→ARGUE→VERIFY complete 2026-04-11)
**Owner:** Joe + Claude (cross-stack)
**Cross-ref:** M-stack-inhabitation (umbrella), M-a-sorry-enterprise (sorry closure model),
  holistic-argument-sketch.md (the thesis), war-room.md (WR-1 through WR-4)
**Repos:** futon3c (evidence, Agency), futon3 (war room, pattern library, holistic argument),
  futon4 (Arxana browser), futon5a (logic model, sorry topology), futon0 (joe-hud)

## 1. IDENTIFY

### Motivation

The war room exists to coordinate across missions and across futons. Six war
bulletins have been written (2026-02-14 through 2026-04-10), each capturing
findings from a significant effort. But the bulletins are manually authored
snapshots — there is no mechanism that continuously synthesizes the ground-level
state (141 missions, 199 evidence entries, 853 patterns, 65 candidate
invariants, 8 sorrys) with the strategic frame (holistic argument, WR decisions,
support/attack relations).

The result: Joe has difficulty understanding the full complexity of what's going
on. The war room document is updated manually and drifts from reality. The
holistic argument sketch describes a self-arguing system, but the argument
itself is not self-maintaining.

The arc across the six bulletins tells a story:

**Bulletin 1 (Feb 14): First Proof** — Wiring diagrams are operational, not
decorative. The evidence discipline works (S1). But the system can't see itself.

**Bulletin 2 (Feb 18): Evidence Landscape** — Evidence store operational. Mission
control starts scanning repos. But observation surface is half-blind (S2).

**Bulletin 3 (Feb 22): Self-Representing Stack** — Ideal/actual self-images
identified. System needs to compare what it aspires to be with what it actually
is. But the comparison is manual, not computed (S4, A2).

**Bulletin 4 (Feb 27): Portfolio Legibility** — Portfolio inference operational.
First recommendation: "review." Following it exposed broken sensors. 73 missions
triaged. But 29 remained unclassified, and the inference loop was half-blind (A1).

**Bulletin 5 (Mar 1): Tickle Experiment** — Multi-agent coordination works under
load. Quality feedback loop validated. But infrastructure bottlenecks (merge,
orchestration, liveness) caused 40% rework (S3, S5).

**Bulletin 6 (Apr 10): Inhabitation Threshold** — Entry ceremony fixed. Evidence
pipeline immediately operational. Context retrieval, sorry topology, HUD operator
panel all wired in a single session. But 66 "active" missions, most abandoned.
The system consumes its operator (A1, A2, WR-4).

**The thread:** Each bulletin adds a capability, but the integration is always
manual. The holistic argument's loop —
`work → proof → patterns → coordination → self-representation → better work` —
has been demonstrated piecewise across the bulletins, but never runs
autonomously. Each step requires a human (Joe) or a prompted agent (Claude) to
manually connect the outputs of one step to the inputs of the next.

The War Machine is the peripheral that closes this gap: it reads the evidence
landscape, the mission inventory, the sorry topology, the pattern activation
history, and the holistic argument, and produces an updated strategic assessment
— not as a one-off document, but as a continuously available synthesis.

### What the War Machine produces

**War Room Status** (available on demand, e.g. `M-x war-machine`):

1. **Loop health:** For each arrow in the holistic argument's loop
   (`work → proof → patterns → coordination → self-representation → better work`),
   is evidence flowing? When was the last observed event for each arrow?

2. **Support/attack update:** For each S1-S5 and A1-A4, what's the current
   evidence? Which bulletins addressed it? Has new evidence accumulated since
   the last bulletin?

3. **Sorry proximity:** For each critical sorry, what's the current closure
   trajectory? (From M-a-sorry-enterprise affinity scoring.)

4. **Mission triage signal:** How many missions are genuinely active vs
   abandoned-in-progress? What's the inventory debt?

5. **Pocketwatch signal:** Current workstream commit ratios, evidence topic
   counts, firing ticks. (From joe-hud operator panel.)

6. **WR compliance:** Is WR-4 being respected? (Stack hours vs depositing hours.)

### Theoretical anchoring

- **Third-order self-representation (holistic argument):** The system argues
  for its own next steps. The War Machine is the mechanism that produces the
  argument from the evidence.
- **Mission Control as physics engine (Bulletin 3):** Statics (what exists),
  dynamics (what's changing), empirics (what the evidence says). The War Machine
  adds a fourth: *strategy* (what the evidence implies about what to do next).
- **AIF loop at the strategic level:** Observe (evidence landscape), infer
  (support/attack balance), act (WR decisions, mission triage), evaluate
  (next bulletin). The War Machine is the observe+infer step.

### Scope in

- Read evidence store for loop health signals
- Read mission inventory for triage signals
- Read sorry topology for closure trajectory signals
- Read joe-hud signals (commit ratios, evidence topics, firing ticks)
- Synthesize into war room status (markdown or HUD panel)
- Accessible via `M-x war-machine` or `arxana://view/war-room`

### Scope out

- Autonomous WR decision-making (the machine reports, Joe decides)
- Writing war bulletins (those remain manual — the machine provides the data)
- Modifying missions or sorrys (read-only observer, like the joe-hud)
- New evidence collection (uses existing repeat signals per "No new life-logging")

### Completion criteria

1. War Machine produces a readable strategic status from live data
2. Each arrow in the holistic loop has a "last seen" timestamp
3. Each S1-S5 and A1-A4 has a current evidence summary
4. Accessible from Emacs (command or Arxana browser)
5. Updates in < 30 seconds (no long scans)

### Relationship to other missions

| Mission | Relationship |
|---------|-------------|
| M-stack-inhabitation | Parent — the War Machine is an inhabitation surface |
| M-a-sorry-enterprise | Sibling — sorry proximity is one of the War Machine's signals |
| M-self-representing-stack | Predecessor — the War Machine IS the self-representing stack applied to strategy |
| M-portfolio-inference | Related — portfolio inference was the first attempt at automated strategy |
| holistic-argument-sketch | Source — the support/attack relations are the strategic frame |

### Source material

| Source | Role |
|--------|------|
| futon3/holes/war-room.md | Strategic frame, WR decisions |
| futon3/holes/holistic-argument-sketch.md | Support/attack relations |
| futon3/holes/war-bulletin-{1..6}.md | Historical strategic assessments |
| futon3c GET /api/alpha/evidence | Evidence landscape |
| futon3c GET /api/alpha/missions | Mission inventory |
| futon5a/data/alignment.edn | Sorry topology |
| futon5a/data/stack-logic-model.edn | Logic model, pocketwatch |
| futon0 joe-hud scan functions | Signals (commits, topics, ticks) |

### Theoretical grounding: Deleuze & Guattari's War Machine

The name references Plateau 12 of *A Thousand Plateaus* (Nomadology: The War
Machine). The D&G war machine is not about war — it is about a form of
organization that is *exterior to the State apparatus*. The key concepts map
directly onto the futon stack's situation:

**Axiom I: The war machine is exterior to the State apparatus.**
The futon stack is not an institution. It has no org chart, no department, no
budget line. It exists as a nomadic practice that Joe carries between
institutional contexts (university, consulting, Hyperreal Enterprises). Like
D&G's war machine, it "exists only in its own metamorphoses" — it is the
practice of building, not an organisation that builds.

**Go vs Chess.** D&G contrast chess (State: coded pieces, regulated moves,
institutional war) with Go (nomadic: anonymous pieces, situational relations,
smooth space). The futon stack operates in Go-space: patterns have no
intrinsic rank, they gain relevance situationally. The sorry-pattern affinity
model (M-a-sorry-enterprise) is exactly this: patterns become relevant not by
their position in a hierarchy but by their proximity to the current situation.

**Smooth space vs striated space.** Striated space is measured, gridded,
controlled — the mission inventory with 141 entries, the candidate invariant
queue with 65 items. Smooth space is navigated by feel, by proximity, by
the clinamen (minimum deviation). The sorry topology is smooth space — the
sorrys are not ranked in a backlog, they exert gravitational pull based on
proximity and urgency. The pocketwatch ticks are the clinamen: the smallest
deviation that redirects movement.

**Nomad science vs royal science.** Royal science reproduces: theorems, proofs,
constants. Nomad science follows: flows, affects, singularities. The futon
stack's pattern library is nomad science — 853 patterns that are not theorems
but *tendencies*, each a "force of thrust" (puissance) rather than a fixed
law. The spreading activation model treats patterns as flows through a
neural substrate, not as rules to apply.

**The ambulant scientist.** D&G describe scientists who "follow the flow of
matter" — they inhabit the material rather than theorising about it from
outside. This is the inhabitation thesis (surface-earns-inhabitation): the
War Machine works only when its operator inhabits the surfaces, following
the evidence flow rather than designing from above.

**The danger.** D&G warn that the war machine can be *captured* by the State —
turned into a military organ that serves institutional reproduction rather
than nomadic creation. The futon stack's version of this danger is the
hermit trap (WR-4): stack work that serves only stack work, infrastructure
that reproduces itself without closing external sorrys. The War Machine
peripheral must detect this capture — when the nomadic practice ossifies
into institutional self-maintenance.

### The name

"War Machine" references Deleuze & Guattari's concept of a form of
organization exterior to the State apparatus. The Dhammapada epigraphs in
war-room.md set the companion tone: "happily the peaceful live, discarding
both victory and defeat." The machine is nomadic, not bellicose. It observes
flows, detects capture, and reports. The human follows or redirects.

### Exit criterion for IDENTIFY

A human has read this proposal and agrees the gap is real and the scope is
right. The gap: strategic synthesis is manual and drifts from reality. The
scope: a read-only peripheral that synthesises evidence, missions, sorrys,
and the holistic argument into a continuously available strategic status.

### Exit criterion for the mission as a whole

The War Machine is complete when it can produce a principled response to
Hobbes's foundational claim for State sovereignty:

> *"ostendo primo conditionem hominum extra societatem civilem, quam
> conditionem appellare liceat statum naturae, aliam non esse quam bellum
> omnium contra omnes; atque in eo bello jus esse omnibus in omnia."*
>
> (I show first that the condition of men outside civil society — which
> condition may be called the state of nature — is nothing other than a war
> of all against all; and that in such a war, all have a right to all things.)

The War Machine's response: coordination without sovereignty is possible
when the coordination mechanism is evidence-based pattern selection rather
than command hierarchy. Specifically:

1. **The sorry topology replaces the sovereign's decree.** Typed holes exert
   gravitational pull proportional to their severity and proximity. No one
   commands which sorry to close — the evidence shows which are closing and
   which are not. The "right to all things" is replaced by the affinity
   between patterns and sorrys: not all actions are equally relevant to all
   holes.

2. **The pattern library replaces law with tendency.** Patterns are not rules
   (striated, State-form) but tendencies (smooth, nomadic). They activate
   situationally, not by decree. The spreading activation across the pattern
   network is coordination without a coordinator.

3. **The pocketwatch replaces discipline with awareness.** The ticks do not
   command ("do math from 6-8am") but observe ("85% stack commits, 0%
   consulting — is this what you intend?"). The operator remains sovereign
   over their own time; the machine provides interoception, not instruction.

4. **The evidence landscape replaces the social contract.** Hobbes's *bellum*
   arises from the absence of trust: without a sovereign guarantee, no one
   can cooperate. The evidence landscape provides the trust substrate: every
   turn is logged, every pattern retrieval is certified, every sorry closure
   is observable. Trust is not granted by authority but accumulated from
   evidence.

The response is complete when the War Machine can demonstrate, from its own
evidence base, that coordination is occurring without command — that sorrys
are closing, patterns are activating, and the operator is navigating by
awareness rather than by discipline. The mission succeeds when the response
to Hobbes is not a philosophical argument but a running system with an
evidence trail.

The stance is pacifist military (War Resisters International): "War is a
crime against humanity. I am therefore determined not to support any kind
of war, and to strive for the removal of all causes of war." The War Machine
removes causes of war by making coordination possible without sovereignty.
The *bellum omnium contra omnes* is not the natural state — it is the State's
story about what happens without the State. The War Machine is the counter-
story, backed by evidence.

## 2. MAP

### Q1: Performance budget — all sources under 300ms

| Source | Latency | Notes |
|--------|---------|-------|
| Evidence API (200 entries) | 14ms | Fast. Can fetch 500+ comfortably |
| Missions API (141 missions) | 100ms | Fast. Full cross-repo scan |
| Agents API | 16ms | Fast |
| Joe HUD full scan | 253ms | Includes git walks + evidence query |
| Sorry topology (EDN) | 6ms | Local file read |
| Logic model (EDN) | 6ms | Local file read |

**Verdict:** All sources fit within a 1-second budget. The War Machine
can query everything on every invocation.

### Q2: Loop health — the holistic argument's arrows

The loop `work → proof → patterns → coordination → self-rep → better work`
can be observed via evidence events:

| Arrow | Signal | Count | Last seen |
|-------|--------|-------|-----------|
| work → proof | chat-turn events | 129 | 2026-04-11 (today) |
| proof → patterns | context-retrieval events | 0* | NEVER* |
| patterns → coordination | session-start events | 2 | 2026-04-09 |
| coordination → self-rep | turns discussing sorrys/missions/war room | 88 | today |
| self-rep → better work | turns containing fix/correct/resolve language | 58 | today |

*Note: context-retrieval events are logged by the invoke-fn but under a
different evidence key (emitted directly via estore/append*, not through
the standard emit-invoke-evidence! path). The evidence exists but the
loop-health query needs to check both paths.

**Granularity:** Per-arrow event count + last-seen timestamp in a rolling
window (7 days default). An arrow is "healthy" if it has fired within the
window. An arrow that hasn't fired is "starved" — the Baldwin loop is
broken at that point.

### Q3: Reuse — joe-hud scan functions + 3 new scans

**Reuse from joe-hud:**
- `scan-operator` — sorry topology, pocketwatch ticks, constraint signals
- `scan-work-schedule` — commit hour distribution
- `evidence-topic-counts` (inside `sorry-signals`) — turn topic extraction

**New scans needed:**
1. `scan-loop-health` — query evidence API for per-arrow timestamps and counts
2. `scan-support-attack` — compare S1-S5 / A1-A4 against current evidence
   (could be a table of claim + latest supporting evidence date)
3. `scan-mission-triage` — query missions API for status distribution,
   flag abandoned-in-progress (in-progress with no commits in 30 days)

### Q4: Output surface — all three, sharing one scan

The War Machine scan function returns data. Four renderers consume it:

1. **`M-x war-machine`** — standalone markdown buffer (primary surface).
   Like joe-hud but strategic, not behavioral. Shows loop health, S/A
   update, sorry proximity, mission triage, WR compliance.

2. **`arxana://view/war-room`** — Arxana browser view. Same data, navigable.
   Links to evidence sessions, mission docs, bulletin entries.

3. **Joe HUD addendum** — optional one-line summary in the Operator panel:
   "Loop: 4/5 arrows healthy. WR-4: ⚠ stack 85%. Sorrys: 3 critical."

4. **VSAT Planetarium adapter** — export the strategic state as a VSAT
   constellation. Missions as stories, pattern activations as scenes,
   sorry-pattern affinities as links, workstreams as constellations.
   The same data that drives the markdown and Arxana views, but rendered
   spatially in the planetarium. This is the "bus driver's holiday" —
   the consulting tool applied to the consultant's own practice. Exercises
   VSAT on real data (the Eric/VSAT consulting stream, not Bristol/UKRN-S
   which is a different edge entirely).
   
   Implementation: JSON export compatible with VSAT's import schema
   (stories, scenes, links). Could be static (snapshot) or live (API
   endpoint that VSAT reads). The minimum viable version: a babashka
   script that reads the War Machine scan data and writes VSAT-compatible
   JSON to a file that VSAT can import.

### Q5: Coordination without command — Hobbes operationalisation

Observable signals from the evidence base:

| Signal | Count | What it means |
|--------|-------|---------------|
| Sorry discussed | 14 | Sorrys exert pull without assignment |
| Pattern referenced | 56 | Patterns activate situationally, not by command |
| Workstream shift | 56 | Turns discuss transitions between workstreams |
| Self-correction | 82 | System detects and corrects its own errors |

**The Hobbes test:** Coordination is occurring without command when:
1. Sorrys are closing (or advancing) without explicit task assignment
2. Pattern retrieval influences subsequent turns (activation → action,
   observable as topic shifts correlated with retrieval results)
3. Pocketwatch ticks are acknowledged in evidence (the operator responds
   to awareness signals, not to orders)
4. The evidence base itself is the trust substrate — verifiable by anyone,
   not granted by authority

**Ready vs missing (MAP exit table):**

| Component | Status | Work needed |
|-----------|--------|-------------|
| Evidence API query | Ready | None |
| Missions API query | Ready | None |
| Sorry topology read | Ready | None |
| Logic model read | Ready | None |
| Loop health scan | Missing | Query evidence for per-arrow events |
| Support/attack scan | Missing | Table of S1-S5/A1-A4 with latest evidence |
| Mission triage scan | Missing | Flag abandoned-in-progress missions |
| Standalone buffer renderer | Missing | M-x war-machine command |
| Arxana browser view | Missing | arxana-browser-war-machine.el |
| Joe HUD one-liner | Missing | One line in Operator panel |
| VSAT planetarium adapter | Missing | JSON export script for VSAT import |

**No surprises discovered.** The data is all available and fast. The work
is synthesis — combining existing scans into a strategic view.

## 3. DERIVE

### Core design: the War Machine is a dynamic visualisation

The War Machine is not a report. It is a spatial, animated view of the
stack's strategic state — as legible as the futon2 ant war grid, as
information-dense as a WWII operations room, as natively spatial as a
petri dish or a static analysis call graph.

Prior art within the stack:
- **futon2 ant war** (`ants/visual.clj`): Swing-based, cell-by-cell rendering,
  tick-by-tick animation, colour-coded state. Proves the technology.
- **M-futon-enrichment** (futon4): Rational reconstruction of codebase as
  layered hyperedges. "Code as a Crime Scene" (Tornhill) — git history as
  behavioural signal, temporal coupling, hotspots. The enrichment graph
  is the War Machine's data model.
- **average-day.svg** (futon5a): The pocketwatch as spatial layout. Static
  but proves SVG rendering of temporal data.

### The surface: a graph that lives and breathes

**Nodes** are the entities the War Machine tracks:
- Repos (grouped by workstream: stack, portfolio, consulting, mathematics)
- Sorrys (gravitational wells — sized by severity, coloured by status)
- Missions (positioned by status: active near centre, complete at periphery)
- The operator (Joe — a node whose edges show where attention flows)

**Edges** are the relations:
- Commit flow: repo → repo temporal coupling (files that change together)
- Evidence flow: evidence turns linking sessions to sorrys/missions/patterns
- Sorry-pattern affinity: which pattern activations are near which sorrys
- Loop arrows: the holistic argument cycle (work → proof → patterns → ...)

**Dynamics** (what changes tick by tick):
- Each tick replays one day (or one batch of commits/evidence turns)
- Nodes pulse when they receive activity (commit, evidence turn)
- Edges glow when flow passes through them (evidence links two nodes)
- Sorrys intensify or dim as proximity changes
- Dead zones (repos with no activity) fade to grey
- The hermit trap is visible: a cluster of bright stack nodes with dim
  portfolio/consulting nodes

**Layout algorithm:**
- Force-directed graph with workstream clusters as gravity wells
- Sorrys positioned by affinity to their relevant repos/missions
- Time flows left-to-right (replay) or the layout is atemporal (current state)
- Petri-dish mode: multiple dishes for different workstreams, each showing
  its internal dynamics, connected by cross-workstream edges

### Data model

The scan function produces a graph, not a table:

```clojure
{:nodes [{:id :futon3c :type :repo :workstream :stack :activity 96}
         {:id :SORRY-market-interface :type :sorry :severity :critical}
         {:id :M-war-machine :type :mission :status :identify}
         ...]
 :edges [{:from :futon3c :to :futon4 :type :temporal-coupling :strength 0.7}
         {:id :SORRY-market-interface :to :vsat.wiki :type :sorry-affinity :score 0.4}
         ...]
 :dynamics [{:tick 1 :date "2026-03-28" :events [...]}
            {:tick 2 :date "2026-03-29" :events [...]}
            ...]}
```

Sources (all under 300ms):
- `GET /api/alpha/evidence` → evidence turn events + topics
- `GET /api/alpha/missions` → mission inventory
- Git log across all repos → commit events, temporal coupling
- `alignment.edn` → sorry topology
- `stack-logic-model.edn` → workstream definitions, pocketwatch ticks
- Joe HUD scan functions → commit ratios, topic counts, artifact existence

### Renderer: Swing visualiser (primary)

Following the `ants/visual.clj` model:
- `JPanel` with force-directed graph layout
- Stats panel alongside (like ant war stats): loop health, WR-4 compliance,
  firing ticks, sorry count
- Replay controls: play/pause, step forward/back, speed slider
- Colour legend: workstream clusters, sorry severity, activity intensity
- Click interaction: click a node to see its detail (missions, evidence, sorrys)

Implementation: `futon0/scripts/war_machine_visual.clj` (Clojure or babashka
with Swing). Can reuse `ants/visual.clj` patterns directly.

### Fallback renderers

The scan data also feeds lighter renderers for when the visualiser isn't
running:

1. **`M-x war-machine`** — markdown buffer (proven working, spike already built)
2. **`arxana://view/war-room`** — Arxana browser view (navigable)
3. **Joe HUD one-liner** — "Loop 4/5 | WR-4 ⚠ | Sorrys 3"
4. **VSAT adapter** — JSON export for planetarium (deferred)

### Scan function

Location: `futon0/scripts/futon0/report/war_machine.clj`.

Reuses joe-hud scan functions + adds:
- `scan-graph` — builds the node/edge graph from all sources
- `scan-dynamics` — replays git/evidence history as tick sequence
- `scan-temporal-coupling` — files/repos that change together (Tornhill)

IF the evidence API is available, include evidence turns in the dynamics.
HOWEVER the API may be down.
THEN degrade to git-only dynamics (commits only, no evidence turns).
BECAUSE the nomadic property: the machine works in whatever space it finds.

### Invariant rules

1. **Read-only observer.** The War Machine never modifies missions, evidence,
   sorrys, or the pattern library. It reads and synthesises. This is
   invariant D-I5 from joe-hud.

2. **Graceful degradation.** If any data source is unavailable, the scan
   reports the gap rather than failing. Half-blind observation with
   explicit gap reporting (per the pattern `war-machine/half-blind-observation`).

3. **No new life-logging.** All signals come from existing repeat signals:
   git commits, evidence turns, file existence, API queries.

4. **Sovereignty remains with the operator.** The War Machine advises, it
   does not command. Ticks fire notifications; the human decides. This is
   the Hobbes response operationalised: coordination by awareness, not by
   authority.

### IF/HOWEVER/THEN/BECAUSE

**IF** the War Machine scan reuses joe-hud functions and adds three new scans
**HOWEVER** this creates a dependency between futon0 (public) and the evidence
API (futon3c, running locally)
**THEN** the scan function queries the API when available and degrades to
git-only signals when not
**BECAUSE** the nomadic property: the machine works in whatever space it
finds itself, striated or smooth. The War Machine on a plane with no
server still shows commit ratios and file existence signals.

**IF** the standalone buffer is the primary surface
**HOWEVER** the Arxana view offers navigation and the VSAT adapter offers
spatial layout
**THEN** build the standalone buffer first, then wire Arxana and VSAT as
additional renderers consuming the same scan data
**BECAUSE** surface-earns-inhabitation: the simplest surface gets inhabited
first. If `M-x war-machine` earns inhabitation, the other renderers follow.
If it doesn't, building Arxana and VSAT views is wasted infrastructure.

## 4. ARGUE

### Pattern cross-reference

Six patterns in `futon3/library/war-machine/` ground the DERIVE design.
Each was extracted from a specific bulletin's tension:

| Pattern | Where it applies in the design | How |
|---------|-------------------------------|-----|
| `operational-not-decorative` | Loop health arrows | Each arrow is operational (has a measurable signal) not decorative (a box in a diagram). An arrow that hasn't fired is flagged "starved." |
| `half-blind-observation` | Graceful degradation invariant | The scan explicitly reports what it cannot see. "Evidence API unavailable" is a finding, not a failure. Coverage gaps are first-class data. |
| `ideal-actual-gap` | Support/attack table | S1-S5 are the ideal (what the system claims). A1-A4 are the actual (what threatens the claims). The table computes the discrepancy. |
| `coordination-bottleneck` | Mission triage signal | 66 "active" missions with most abandoned is a coordination bottleneck — the system can initiate work but cannot track its own abandonment. The triage count surfaces this. |
| `inhabitation-threshold` | Surface-first build order | `M-x war-machine` first, then Arxana, then VSAT. The simplest surface gets inhabited first. If it doesn't earn inhabitation, the others aren't built. |
| `state-capture` | WR-4 compliance check | The pocketwatch commit ratios detect when the nomadic practice (following evidence flow) is captured by institutional self-maintenance (stack work for stack work's sake). |

Also relevant from the existing library:
- `peripherals/surface-earns-inhabitation` — the War Machine is itself a surface that must earn inhabitation. If `M-x war-machine` is too slow, too noisy, or too complex, Joe won't use it and the Baldwin loop starves.
- `peripherals/inhabitation-feeds-evolution` — using the War Machine generates evidence about what Joe looks at, which informs what the War Machine should surface next. The observation layer learns from being observed.

### Theoretical coherence

The IDENTIFY phase anchored the War Machine in D&G's Nomadology and the
holistic argument's self-representation thesis. Does the DERIVE design serve
these?

**D&G coherence:** The design is nomadic in structure:
- Smooth space: sorrys exert gravitational pull (proximity, not rank). The
  sorry table is not a prioritised backlog — it's a topology.
- Go not chess: patterns activate situationally. The loop health arrows
  are not a pipeline to execute but a flow to follow.
- State capture detection: WR-4 compliance is the operational test for
  whether the nomadic practice has been captured.
- Ambulant science: the scan follows the data (evidence turns, git commits,
  file existence) rather than imposing a schema.

**Holistic argument coherence:** The design directly addresses the "missing
piece" identified in the holistic argument sketch:

> "Understanding produces argument — *this is the missing piece.*"

The War Machine IS the argument-producing mechanism. Loop health shows
whether the system's claimed virtues (S1-S5) are actually operating.
Support/attack update shows whether the threats (A1-A4) are growing or
shrinking. Sorry proximity shows whether the system is moving toward its
goals. The synthesis is not a philosophical argument but a running report
backed by evidence. The system argues for its own next steps by reporting
what is and isn't working.

### Trade-off summary

| Gave up | In exchange for |
|---------|-----------------|
| Static report as primary | Dynamic visualisation as primary, markdown as fallback. Rationale: the ant war proves that dynamic spatial views earn inhabitation; tables become wallpaper. Churchill didn't read a spreadsheet. |
| Custom rendering engine | Swing (reusing ant war patterns). Rationale: Swing is ugly but proven in futon2. The ants live in Swing. The War Machine lives in Swing. Browser-based can come later. |
| Automated WR decisions | Advisory-only ticks. Rationale: sovereignty remains with the operator (Hobbes criterion). The machine that commands is a State apparatus, not a war machine. |
| VSAT integration | Deferred. Rationale: inhabitation-threshold pattern. The Swing visualiser earns inhabitation first; VSAT is a future renderer consuming the same graph data. |

### Generalisation notes

The War Machine design is not specific to the futon stack. Any system with:
- A set of goals (sorrys / typed holes)
- Observable work signals (commits, turns, file changes)
- A knowledge base (pattern library)
- A claimed self-model (devmaps, holistic argument)

...could instantiate the same scan-and-synthesise pattern. The DERIVE
structure (one scan function, multiple renderers, graceful degradation,
read-only observation) is reusable.

The sorry enterprise thesis: every organisation has sorrys. The War Machine
is the peripheral that makes them visible and tracks their closure
trajectories. The Hobbes response: this coordination does not require a
sovereign — it requires transparency.

### Plain-language argument

The futon stack has a lot going on — 141 missions, 853 patterns, 8 layers,
evidence flowing, sorrys accumulating. Joe can't hold it all in his head,
and a table of numbers doesn't help — it requires reading every line and
mentally synthesising. What works: a spatial view where the state is obvious
at a glance. The ant war proved this — you can see whether ants are busy or
stupid, you can see when colonies die, without reading a single number.

The War Machine is the ant war applied to the strategic state. Repos are
nodes, sorrys are gravitational wells, evidence turns are signals flowing
through edges, temporal coupling reveals hidden dependencies. You watch the
last two weeks replay and see where the work went, where the gaps are, and
where the sorrys sit. The hermit trap is visible as a bright cluster of
stack nodes with dim portfolio/consulting nodes. No one needs to tell you
the balance is wrong — you can see it.

The technology is proven (Swing, force-directed graphs, tick-by-tick replay).
The data sources are fast (all under 300ms). The prior art is in-house
(ant war, enrichment mission, pocketwatch). The core bet: if the strategic
state is spatially legible, coordination happens without command — the Hobbes
response demonstrated by a running system.

## 5. VERIFY

### Wiring diagram

The War Machine exotype is at `futon5a/data/war-machine-exotype.edn`.
It defines a two-cycle AIF observation loop: fast (per-turn pattern
retrieval) and slow (strategic synthesis). 8 inputs, 6 outputs, 7
components, 4 invariants (WM-I1 through WM-I4). The only feedback path
is O-context-evidence → I-evidence (the Baldwin loop). All constraint
inputs are read-only.

### Structural verification: tracing the War Machine through the Placemat

The wiring diagram grounds the verification. The Grand Unified
Placemat (futon5a/data/) provides the structural frame. The verification
question: does the War Machine design fit into the reality the Placemat
describes, or is it floating in developer-tool space with no connection to
the sustainability model?

**The Placemat has three vertices:**
- V-people (UKRN-S, participatory sustainability)
- V-money (VSAT, fiscal sustainability)
- V-orgs (FUTON stack, organizational sustainability)

**The War Machine sits at V-orgs** — it is a component of the stack's
self-governance. But it must serve V-people and V-money or it is hermit
infrastructure (WR-4 / state-capture pattern).

**Tracing the Krowne flows through the War Machine:**

| Flow | Direction | What the War Machine carries |
|------|-----------|------|
| F-d (content/services) | V-orgs → V-people | The sorry topology surfaces gaps in UKRN-S delivery. SORRY-market-interface is a V-people sorry tracked by a V-orgs tool. |
| F-f (content purchase) | V-orgs → V-money | If VSAT is a renderer, the tool demonstrates itself — V-orgs producing a V-money artifact. |
| F-e (sponsorship) | V-money → V-orgs | Revenue from consulting sustains the stack. The War Machine tracks whether this flow is happening (consulting commit %, sorry closure rate). |

**If the War Machine only serves V-orgs → V-orgs, it is captured.**
The VERIFY test: does the design include signals from all three vertices?

| Vertex | Signals in the design | Present? |
|--------|----------------------|----------|
| V-orgs (stack) | Git commits, evidence turns, loop health, mission inventory | Yes |
| V-people (UKRN-S) | Evidence turns mentioning Bristol/UKRN, Working Paper existence | Yes |
| V-money (VSAT/consulting) | Consulting commit %, sorry closure for SORRY-vsat-revenue | Partially |

**Verdict: passes with one gap.** V-money signals are partially present
(commit ratios) but revenue is external and manual. Acceptable for V1.

### Tracing through the JSDQ adapter

| JSDQ component | War Machine equivalent |
|--------|------|
| Observables (o) | The scan data: commit ratios, topic counts, sorry status, loop health |
| Beliefs (μ) | Inferred mode: which workstream is dominant, which sorrys are closing |
| Preferences (C) | Pocketwatch targets + WR-4 constraint |
| Policies (π) | Workstream balance detectable from commit ratios |
| Actions (a) | Not prescribed — the War Machine advises, does not command |
| Constraints | WR-4, cargo-implies-depositing, hermit-warning — implemented as ticks |
| Free energy (G) | The gap between preferred and actual state, made visible |

**The War Machine IS the JSDQ observation function rendered visually.**

### Tracing through the logic model edges

Five of 10 logic model edges interact with the War Machine:

| Edge | Interaction |
|------|------------|
| click-1 | Displays Click 1 output as signal |
| context-retrieval-edge | Shows pattern activation frequency (the MRI) |
| sorry-topology-edge | Shows sorry status and closure trajectory |
| hermit-trap | Detects and displays the capture loop |
| sorry-pattern-affinity | Shows which patterns are near which sorrys (future) |

**No orphan inputs.** Every data source connects to a named edge in the
logic model.

### Completion criteria pre-check

| Criterion | Addressed? |
|-----------|-----------|
| 1. Strategic status from live data | Yes — scan function + renderers |
| 2. Loop arrow timestamps | Yes — loop health scan |
| 3. S1-S5/A1-A4 evidence summaries | Yes — support/attack scan |
| 4. Accessible from Emacs | Yes — `M-x war-machine` + Arxana |
| 5. Updates in < 30 seconds | Yes — all sources under 300ms |
| 6. Hobbes response | Partially — evidence exists, animated display needed to make "coordination without command" visually obvious |

### Decision log

- **DERIVE revision (2026-04-11):** Primary surface changed from markdown
  report to dynamic animated visualiser. Rationale: spatial-over-tabular
  and dynamic-over-static patterns.
- **ARGUE addition:** Three new flexiargs written to back revised design.
- **VERIFY finding:** Design passes Placemat structural verification.
  V-money signals weakest. Acceptable for V1.

## 6. INSTANTIATE

### Checkpoint 1 (2026-04-12): Visualiser V1 operational

**What was built:**

1. **Responsive hex layout** — `fit-hex-size` and `layout-offset` functions
   compute hex size dynamically from panel dimensions. The grid scales to
   any window size; no fixed pixel dimensions. Centering offset keeps the
   grid centered after resize.

2. **Mission nodes in graph** — `mission-nodes` function queries
   `GET /api/alpha/missions`, filters to active/blocked/ready/testing,
   and adds them to the scan graph's `:nodes` map. Visualiser places
   missions in the top two rows (row 0-1), color-coded by status:
   red=blocked, blue=active, green=ready, purple=testing.

3. **Click interaction** — `hex-hit-test` does distance-based collision
   detection against hex centers. Click any hex → detail panel (JTextArea
   at SOUTH) shows node info (repo: workstream/commits, sorry: severity/
   layer/closes-by, mission: status/repo, tick: fired status).

4. **Test suite** — 13 tests, 71 assertions, 0 failures.
   `war_machine_test.clj`: arrow-health math, observation vector
   normalization, render output shape, claim/arrow coverage contracts.
   `war_machine_visual_test.clj`: hex geometry, layout bounds, responsive
   sizing, centering, layout assignment, hit-testing, detail text.
   Test alias added to `deps.edn` (`:test` with cognitect test-runner).

5. **End-to-end verified** — text renderer produces full strategic
   synthesis from live futon3c APIs. All 4 scans operational (loop health,
   support/attack, mission triage, strategic graph). Evidence API and
   missions API both responding.

**Files changed (futon0):**

| File | Change |
|------|--------|
| `scripts/futon0/report/war_machine.clj` | Added `mission-nodes` fn, included missions in `scan-graph` `:nodes` |
| `scripts/futon0/report/war_machine_visual.clj` | Responsive layout (`fit-hex-size`, `layout-offset`, `layout-bounds`), mission sprite rendering, click interaction (`hex-hit-test`, `node-detail-text`, `MouseAdapter`), detail panel, updated legend |
| `deps.edn` | Added `:test` alias |
| `test/futon0/report/war_machine_test.clj` | New — scan function tests |
| `test/futon0/report/war_machine_visual_test.clj` | New — layout/geometry tests |

**Bugs fixed from initial observation:**

- Green hexagon (mathematics workstream) was highly connected due to
  temporal coupling — this is correct behavior (co-change correlation),
  not a bug. Now clickable for detail.
- Pink hexagons (portfolio/consulting) were clipped off-screen due to
  fixed 520×420 panel and hardcoded hex positions at q=7-8. Fixed by
  dynamic sizing that fits all cells within the panel bounds.
- Refresh now triggers repaint which recomputes hex size for current
  window dimensions.

**What remains for INSTANTIATE completion:**

- [ ] Sessions-as-ants model (sessions traverse the hex landscape,
      the conceptual mapping from the user's observation)
- [ ] Dynamic replay (tick-by-tick animation, play/pause/step controls)
- [ ] `M-x war-machine` Emacs command wired to standalone buffer
- [ ] Arxana browser view (`arxana://view/war-room`)
- [ ] VSAT planetarium adapter (deferred per inhabitation-threshold)
- [ ] Hobbes response demonstrated by running system

### Checkpoint 2 (2026-04-12): Scan layer + hex Petri dish + session replay + portfolio wiring

**Commit:** `futon0@1654516`

**What was built:**

1. **8 scan functions** — loop-health (6 arrows), support-attack (S1-S5/A1-A4),
   mission-triage, sessions (with per-step repo AND mission detection),
   scan-graph (temporal coupling), scan-portfolio (live AIF state),
   scan-mission-detail, observe/sense->vector (12-channel normalised vector).

2. **Hex Petri dish visualiser** — tight cluster layout (greedy adjacency,
   most-connected repos central), two view modes (Stack and Missions toggle),
   multi-session replay with coloured ants and checkboxes, stats sidebar
   with portfolio inference state, click-to-detail.

3. **Terminal vocabulary** — `futon5a/data/war-machine-terminal-vocabulary.edn`
   harmonising 6 source vocabularies (cyberants, joe-terminal, JSDQ,
   peripheral-aIF, logic model, sorry topology) into 12 observation channels.

4. **Portfolio Inference wired** — scan-portfolio reads live AIF state
   (mode, urgency, tau, 16 sensory channels) from GET /api/alpha/portfolio/state.

5. **Session ants detect specific missions** — text matching finds mission IDs
   in evidence body text, so the ant lights up "war-machine" not all of futon3c.

**Honest gaps:**

- Portfolio Inference says "BUILD, urgency 0.74" but this is a label without
  direction. The adjacent-possible set and EFE-ranked mission recommendations
  are computed by the AIF loop but not surfaced in the war machine. The operator
  sees a mode, not a plan. This must be fixed before portfolio wiring is "done."

- Pattern-reuse channel in portfolio inference is placeholder (always 0.0).
  Context retrieval generates pattern activation data every turn but it's not
  fed back into the portfolio observation channels. This is the key missing
  connection for the Baldwin loop.

- Session replay has no slider/scrubber for temporal position.

- The visualiser is display-only. The AIF loop that would make sessions
  accumulate knowledge (pattern priming, PSR/PUR→persistent links) is designed
  (M-pattern-inference-engine) but not built.

**What remains for INSTANTIATE completion:**

- [ ] Surface portfolio adjacent-possible set + EFE-ranked missions in war machine
- [ ] Connect context retrieval → pattern-reuse observation channel
- [ ] PSR/PUR priming: sessions start with relevant pattern context
- [ ] Replay slider for temporal position
- [ ] `M-x war-machine` Emacs command
- [ ] Hobbes response: coordination without command demonstrated by running system
- [ ] Clojure REPL against the JVM (needed to launch visualiser from Emacs)

### Checkpoint 3 (2026-04-12): Judgement layer + four-view visualiser + Arxana invariant browser

**Commits:** `futon0@df612b8`, `futon4@d200f04`, `futon3c@6de0fdf`

**Design shift:** Mid-course redesign from dashboard to strategic engine. The
previous session built scans and a hex grid; this session asked "what can we
actually learn from this?" and concluded the war machine needs a judgement
layer — the bridge between raw observations and actionable priorities. The
engine is separate from the visualiser (same pattern as cyberants: the ant
colony runs independently, the viz is a debugger).

**What was built:**

1. **Judgement layer** (war_machine.clj) — the missing inference step between
   observe and render. Computes free energy (G = 0.45, pragmatic + epistemic),
   infers strategic mode (currently: hermit, 10% prior), ranks 15 priorities
   by type (missing AIF heads, channel gaps, unwired invariant families,
   critical path missions). Detects 5 active losses (hermit mode, stack at 91%,
   consulting at 0%, all ticks firing, sorrys accumulating).

2. **AIF head integration** — queries portfolio inference (MAINTAIN, urgency
   0.74, tau 0.93), detects 2 missing heads (mission-aif-head has code but no
   HTTP endpoint; session-aif-head doesn't exist yet per M-aif-head).

3. **Live invariant runner** — queries all 6 domain checks (agency, tickle,
   proof, mission, codex, portfolio) via Drawbridge /eval endpoint. Currently:
   5 domains active, all clean, 0 violations, 0 obligations.

4. **Support/attack enrichment** — overlays invariant and AIF head health onto
   holistic argument claims. S1 gets "9/18 families operational", A1 gets "9
   candidate families unwired", A2 gets "2 AIF heads missing". Support coverage
   jumps from 60% to 80% with invariant evidence; attack from 0% to 50%.

5. **Four-view visualiser** — toggle cycles Stack → Missions → Invariants →
   Patterns. Invariants view: concentric hex rings (9 operational green core,
   9 candidate amber ring, 30 individual candidates dim outer ring). Patterns
   view: 56 collections spatialized by MiniLM embedding similarity
   (top-2-variance dimensions from 384-dim centroids), semantic clusters
   visible (math collections near each other, agency/coordination near each
   other, etc).

6. **Pattern scan** — loads 954 patterns from TSV index, groups into 56
   collections, computes embedding coordinates from MiniLM centroid projection.

7. **Arxana Browser** — Invariants menu restructured: Operational Families
   (18 families grouped by I0-I4 layers with headers), Live Violations,
   Candidate Queue, Invariant Guide (explains layers, lifecycle, ownership).

8. **futon3c endpoint** — GET /api/alpha/invariants ready (activates on next
   server restart). War machine uses Drawbridge /eval fallback in the meantime.

**Key architectural insight:** The war machine integrates AIF heads — it reads
their judgements rather than replacing them. "Sessions are ants" (each with a
trail and a momentary existence). The war machine is the Leviathan response:
coordination without sovereignty via invariant enforcement, pattern activation,
and AIF head composition. New heads are added as they're built; the system
reports where heads are missing (structural gap = priority).

**Honest gaps:**

- Free energy uses the terminal vocabulary preferences (C/preferred from the
  exotype) but the observation vector's support/attack channels read 0% from
  evidence text — the claim-matching regexes may be too narrow. Invariant
  enrichment partially compensates.

- The 4 views are display-only debuggers for the engine. The engine's priority
  list is the actionable output (rendered in markdown). The hex views verify
  the engine isn't hallucinating.

- Pattern embedding coordinates use raw top-2-variance dimensions, not true
  PCA. Semantic clustering is visible but could be tighter with proper
  dimensionality reduction.

- No Clojure REPL wired in Emacs for launching the visualiser — currently
  requires `clojure -M:war-machine` from terminal.

**Critical bug fixed this session:** `emit-context-evidence!` had shape
violations (string tags/ref-type where keywords required). The evidence store
silently rejected every context-retrieval entry. Notifications fired (visible
to Joe), evidence writes failed (invisible). Hot-fixed via Drawbridge and
patched in source. The Baldwin loop (fast cycle → evidence store → slow cycle)
is now connected. Context-retrieval entries are flowing into the evidence store.

**Forward integration point: structural graph + PSR-as-graph-rewrite.**
cf. github.com/tirth8205/code-review-graph — AST-based structural graph
(functions, classes, imports as nodes; calls, inheritance, test coverage as
edges) queried via MCP at review time. The war machine could consume such a
graph as another scan source (`scan-structural-graph`). PSR/PUR would then
operate as typed annotations on graph edges — PSR predicts a graph
transformation, PUR records the delta between predicted and actual. This
connects M-self-representing-stack (the system describes itself structurally)
to M-war-machine (the system observes itself strategically). Not in scope for
this mission but the engine architecture supports it: new scan functions and
AIF heads slot in without redesign.

**What remains for INSTANTIATE completion:**

- [x] Connect context retrieval → pattern-reuse observation channel (bug fix:
  shape validation, evidence now flowing)
- [ ] Surface portfolio adjacent-possible set + EFE-ranked missions
- [ ] PSR/PUR priming: sessions start with relevant pattern context
- [ ] `M-x war-machine` Emacs command (needs JVM REPL)
- [ ] Hobbes response: demonstrated by the four views + judgement layer
  showing coordination without command — invariants enforced, patterns
  activating, priorities emerging from composed head states

## 7. DOCUMENT — _pending_
