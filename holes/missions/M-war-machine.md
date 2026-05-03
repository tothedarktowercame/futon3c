# Mission: War Machine
Status: parked

**Date:** 2026-04-10
**Status:** IDENTIFY
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

The War Machine scan function returns data. Three renderers consume it:

1. **`M-x war-machine`** — standalone markdown buffer (primary surface).
   Like joe-hud but strategic, not behavioral. Shows loop health, S/A
   update, sorry proximity, mission triage, WR compliance.

2. **`arxana://view/war-room`** — Arxana browser view. Same data, navigable.
   Links to evidence sessions, mission docs, bulletin entries.

3. **Joe HUD addendum** — optional one-line summary in the Operator panel:
   "Loop: 4/5 arrows healthy. WR-4: ⚠ stack 85%. Sorrys: 3 critical."

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

**No surprises discovered.** The data is all available and fast. The work
is synthesis — combining existing scans into a strategic view.

### Update 2026-04-26 — recurring AIF substrate is now live

One of IDENTIFY's framings is that the holistic argument's loop has been
demonstrated piecewise across the bulletins but never runs autonomously.
The Portfolio Inference half of that loop is no longer manual.

`futon3c/src/futon3c/portfolio_inference/scheduler.clj` runs
`portfolio-step!` on a fixed cadence (default 1 h) inside the futon3c
JVM and deposits four evidence entries per tick at
`{:ref/type :portfolio :ref/id "inference"}`:

- `:portfolio/observation` — 12 normalized channels
- `:portfolio/belief` — current `μ` and precision
- `:portfolio/policy` — EFE-ranked actions + abstain flag
- `:portfolio/step` — chosen action + diagnostics

The War Machine no longer has to invoke any AIF code to read current
mode, urgency, free energy, abstain status, or policy ranking. The
strategic synthesis can now be a pure read over the evidence store.

Effect on this mission:

- the "loop health" arrow `coordination → self-representation` has a
  continuously-updated trace
- the "self-representing stack" claim now has hourly belief snapshots,
  not manual ones
- War Machine derivation can join AIF mode/urgency directly into the
  bulletin synthesizer

Operational details: see
`futon5a/holes/missions/M-stack-stereolithography.md` § Checkpoint 5.

## 3. DERIVE — _pending_

## 4–7. _Pending DERIVE_
