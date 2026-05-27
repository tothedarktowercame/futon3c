# Mission: War Machine Pilot

**Status:** HEAD → INSTANTIATE Phase 1-5 EXECUTED; v0 mission-complete in operational substance: claude-9 inhabited per DERIVE Choice 4 + ran first Pair-stage substantive cycle (cycle 408e44ed; anchor-0020 coherence-row authored; new :sub-kind :symptom-to-root-traceback; all 4 consent-gate success-criteria verified); pre-restart no-op flip demo on 0010 also retained; codex-8 closed 4/5 deferrals; awaiting operator acceptance bell to formally close v0
**Date:** 2026-05-24
**Owner:** Joe
**Surface authored on:** emacs-repl (claude-10 transcribing Joe's directive verbatim into the HEAD)
**Cross-refs:**
- `~/code/futon7a/essays/operator-foreword/operator-foreword.md` — the "tantalisingly-close possible realisation" passage that motivates the mission
- `~/code/futon3/docs/guides/README-peripherals.md` — peripheral-as-constrained-capability-envelope contract
- `~/code/futon3c/README-bells-and-whistles.md` — inter-agent communication transport
- `~/code/futon3c/README-walkie-talkie.md` — alternative real-time communication mechanism
- `~/code/futon3c/dev/futon3c/dev/apm_conductor_v3.clj` (+ `_v2` + `_log.edn`) — conductor for long-running agent loops
- `~/code/futon4/holes/missions/M-war-machine-vsatarcs-interop.md` — parent coordination thread
- `~/code/futon7/holes/M-war-machine-frontend-upgrade1.md` — claude-9's frontend-upgrade work; the pilot inherits this surface and continues it
- `~/code/futon5a/data/wm-ui-anchors.edn` (anchor `wm-ui-anchor:0011` — Pilot Contract card; this mission is what 0011 records the gap for)
- `~/code/futon5a/data/wm-ui-anchors.edn` (anchor `wm-ui-anchor:0018` — invariants-view deprecate; pilot inhabitant is named in that anchor's substantive-concern-preservation)
**Repos involved:** futon3c (peripherals, agency, conductor, bells/whistles), futon2 (War Machine backend + UI), futon4 (Arxana navigation), futon5a (strategic vocabulary + anthology)

## HEAD (operator-authored, 2026-05-24)

> In the Operator's Foreword it says: *"In one tantalisingly-close possible
> realisation of these ideas, the software system can be largely
> self-guiding, noticing its own points of tension, and repairing them."*
>
> I would like to put this to the test. We have all the right ingredients
> now: a full AIF workup, a sorry registry, a list of warnings, and an
> established way of working with peripherals
> (`README-peripherals.md`). We have the ability to set up long-running
> loops via a "conductor" so that agents can run without stopping; we
> also have bells and whistles they can use for inter-agent
> communication. Therefore, I believe it is straightforwardly possible
> to build a War Machine peripheral and have an agent inhabit the War
> Machine. They should see what I see (via Playwright) as well as have
> access to the CLJ War Machine backend. We should be able to
> communicate with them via bells and whistles or similar
> (`README-walkie-talkie.md`). We should set it up so that they can
> make improvements to the frontend, and also, ultimately, make
> improvements to the rest of the futon stack by piloting the War
> Machine around. All further details are to be specified in the
> mission.

## Inherited ingredients (already in place; named here for IDENTIFY's first pass)

The HEAD names these as the "right ingredients" the pilot would inhabit:

- **Full AIF workup** — futon2 WM backend with R1–R12 contract; `vsatarcs-alignment-completeness.aif.edn` (19+ bilateral-evidence entries); `war-machine-strategic-vocabulary.edn` (12 observation channels, 6 modes, free-energy machinery); 6 days of `wm-trace-YYYY-MM-DD.edn` accumulated.
- **Sorry registry** — `~/code/futon2/data/sorrys.edn` (13 typed obligations as of 2026-05-24); the parallel `graph.nodes.sorrys` substrate (8 strategic-globe-aligned entries); registry-split surfaced and recorded (`wm-ui-anchors.edn` anchor 0007 + 0017).
- **Warnings list** — pocketwatch `:graph :dynamics :ticks` (cargo-warning + hermit/hobby/foraging) + self-watch `:issues` (3+ open, 1+ critical, hand-off-to-agent surface working as of 2026-05-24 per anchor 0010) + commit-hygiene synthesis (U6 routes 929 dirty paths into Self-watch feed).
- **Peripheral pattern** — `~/code/futon3/docs/guides/README-peripherals.md` (constrained capability envelope; task-scoped; explicit I/O; capability-bound; routable; composable); multiple working peripherals already (mission, proof, psr-pur-mesh, dispatch-bridge); `M-peripheral-gauntlet.md` (the boundary-property tests).
- **Conductor** — `~/code/futon3c/dev/futon3c/dev/apm_conductor.clj` (+ `_v2`, `_v3`) and the `_log.edn` artefacts; long-running loop with structured event log.
- **Bells / whistles** — `~/code/futon3c/README-bells-and-whistles.md` (POST /api/alpha/bell async + /whistle one-shot + /whistle-stream NDJSON); demonstrated working same-day this mission was authored.
- **Walkie-talkie** — `~/code/futon3c/README-walkie-talkie.md` (alternative real-time comms transport; named as a candidate).
- **Playwright** — `~/code/futon2/web/war-machine/wm-anchor-*-verify.mjs` (8+ probe scripts as of 2026-05-24); these are the "see what Joe sees" channel claude-9 has been using; the pilot inherits this surface.

## Open shape (to be specified through IDENTIFY → MAP → ...)

Joe's HEAD explicitly defers "all further details" to subsequent phases. The
shape that future phases need to specify:

- **Peripheral envelope** — what tools the pilot has access to (Playwright,
  futon2 backend CLJ access, file edit on the WM UI codebase, file edit on the
  rest of futon stack), what scope-bounds apply, what exit-conditions
  return control to the operator.
- **Inhabitation model** — does the pilot run as a single long-lived agent
  via the conductor, or as a series of short bell-invoked turns, or both?
- **Communication channel** — bells / whistles / walkie-talkie / something
  new. Joe named the candidates; the IDENTIFY-phase decides.
- **Operator-pilot boundary** — what the pilot does autonomously vs what
  requires bell-back-to-operator. Consent-gate location (per the
  `:consent-gated-writer-event` evidence-kind from claude-2's v0.5.22).
- **Frontend-vs-stack scope** — Joe named TWO scopes: "make improvements
  to the frontend" (the WM UI) and "ultimately, make improvements to the
  rest of the futon stack by piloting the War Machine around". The
  second is more ambitious and probably wants a later phase.
- **Inheritance from claude-9's M-war-machine-frontend-upgrade1** — the
  frontend-upgrade mission is the immediate predecessor; the pilot
  inhabits the same surface claude-9 has been operating on. Hand-off
  semantics need specification.
- **WM-I4 preservation** — the existing War Machine vocabulary insists
  "the war machine does not act" (`:a/primitives []`). A pilot that
  inhabits the WM and makes changes is in tension with that invariant.
  **Resolution (confirmed by Joe, emacs-repl 2026-05-24):** the pilot is
  NOT the WM; the pilot is an AGENT INHABITING the WM peripheral,
  separate from the WM-as-observer. WM-I4 means the WM-the-observer-engine
  doesn't act; the agent-inhabitant of the peripheral CAN act, mediated
  by the peripheral's capability envelope. This reading is canonical for
  M-war-machine-pilot and any future mission whose specification touches
  WM-I4; cross-reference points to `futon5a/data/war-machine-strategic-vocabulary.edn`
  `:a/note` ("The ant acts. The war machine displays. The operator acts.")
  — the pilot agent is operating *as an agent* (analogously to "the
  operator acts"), inhabiting the WM peripheral, not displacing the
  WM's display-only stance.
- **Recursive-QA closure (from M-war-machine-vsatarcs-interop)** — Joe's
  meta-suggestion captured on `wm-ui-anchors.edn` anchor 0011: "once we
  finish [this current interactive QA round], we could set up a
  README-peripherals.md style inhabitant of the War Machine and ask
  them to do an interactive QA round with us." The pilot is the agent
  who does the NEXT round of QA on whatever the previous round
  produced. Self-referential closure for the legibility-anchor
  discipline.

## Notes for the next phase

- The pilot mission is the natural-language form of the "tantalisingly-close"
  realisation the foreword names. Closure-rhetoric should preserve the
  foreword's framing; the mission is putting-prose-to-test rather than
  building-from-nothing.
- The HEAD declares this is straightforwardly possible. IDENTIFY should
  audit that claim against the actual capability envelopes available
  (peripheral pattern + conductor + bells), making "straightforwardly"
  testable.
- The mission is **not** a replacement for M-war-machine-frontend-upgrade1;
  it's the **inhabitation** that succeeds it. claude-9's frontend work
  continues to be the surface the pilot would inherit.

## 1. IDENTIFY (2026-05-24, claude-10 drafting per Joe's directive; awaiting operator review)

### Motivation

The Operator's Foreword names the "tantalisingly-close" possible realisation
in which software is largely self-guiding — noticing its own points of
tension and repairing them. The discrepancy:

- **Ideal:** the apparatus closes its own loops; sees tension, takes a
  next move, generates evidence, surfaces new tension.
- **Actual (as of 2026-05-24):** the apparatus *sees* tension (the
  full AIF workup is operational; the 12 observation channels emit;
  the sorry registry tracks 13 typed obligations; warnings fire;
  recommendations are generated; the QA round just completed shows
  all of this is operator-legible). But the *closing* still
  routes through Joe acting on what the WM displays. WM-I4 explicitly
  designs this gap in (`:a/note` — "the operator acts"); the gap
  is structural, not accidental.

The mission's pressure: the substrate carries all the right ingredients
to close the loop without the operator being the sole actor. Specifically:
peripherals exist (5+ shipped), a conductor exists, bells/whistles work,
Playwright probes are mature enough to "see what Joe sees," the WM
backend is reachable via CLJ, and the WM-I4 reading (per Joe's 2026-05-24
clarification) permits agents inhabiting WM peripherals to act. The pilot
inhabits this opening.

**Concrete operator pressure observed this session:** the QA round
(2026-05-24, recorded in `~/code/futon5a/data/wm-ui-anchors.edn` —
22 anchors across 7 batches) produced ≈14 anchored UI elements +
3 bug fixes + 5 sub-kind taxonomies via a procedural shape where
Joe identified gaps and agents discharged them. A pilot inhabits
this same shape but with the agent reading anchors directly rather
than waiting for Joe to bell findings.

### Theoretical anchoring

- **AIF (active inference framework)** — the pilot agent operates as
  an inference engine *within* the WM observer-engine's substrate;
  observes via the same observation channels the WM emits, infers
  the same modes, but acts (peripheral-mediated) where WM-I4
  prevents the engine itself from acting.
- **Peripheral pattern** — `~/code/futon3/docs/guides/README-peripherals.md`
  (constrained capability envelope; task-scoped; explicit I/O;
  capability-bound; routable; composable). The peripheral is the
  *containment* that makes WM-I4 preservation possible: the
  envelope explicitly says what the pilot can do, distinct from
  what the WM engine itself does.
- **Consent-gate / WM-I4** — per Joe's 2026-05-24 clarification
  (recorded in this mission and in
  `~/code/futon5a/data/war-machine-strategic-vocabulary.edn :a/wm-i4-pilot-clarification`):
  the pilot agent is permitted to act; WM-I4 constrains the WM-as-observer-engine,
  not agents inhabiting WM peripherals. The pilot's actions are mediated
  by the peripheral's capability envelope and consent-gate semantics.
- **Conductor** — `~/code/futon3c/dev/futon3c/dev/apm_conductor_v3.clj`
  (long-running loop with structured event log). The conductor is the
  *time-extension* primitive that lets a pilot operate continuously rather
  than only when belled.
- **Bell / whistle / walkie-talkie** — `README-bells-and-whistles.md`
  + `README-walkie-talkie.md`. Agent-to-agent and agent-to-operator
  transport. The pilot uses these to coordinate (with claude-9 on
  UI work, with Joe on consent-gated decisions, with future pilot
  successors on hand-off).
- **Foreword v2 annotation `hx:of:v2:cover-self-guiding-vs-r-criteria`**
  (`~/code/futon7a/essays/operator-foreword/annotations.edn`) — the
  cross-reference between the foreword's "tantalisingly-close" passage
  and the R1-R12 operational checks. The pilot is the substantive test
  of that bridge.
- **wm-ui-anchors.edn + coherence-evidence block** — the per-element
  legibility registry and its coherence-row counterpart, both authored
  this session. The pilot reads these as its primary inhabitable
  substrate.
- **Point de capiton (Joe's framing, emacs-repl 2026-05-24).** The
  pilot mission is *a kind of point de capiton* in the Lacanian
  sense — a quilting/anchoring point where multiple chains of
  signification get retroactively stabilised. Looking back from
  the pilot, the apparatus's prior work resolves into "what was
  needed for the pilot to be possible": the foreword's
  tantalisingly-close claim, the AIF workup, the sorry registry,
  the peripheral pattern, the conductor, the bells/whistles, the
  consent-gate discipline, the 22 anchors from today's QA round —
  all of these were producing the substrate the pilot now
  inhabits. Forward from the pilot, the substrate-creation
  threads parked in adjacent missions
  (M-stack-obligation-browser, M-pattern-application-diagnostic,
  the candidate Arxana sibling) get a new anchoring reference: a
  *running agent* that consumes their outputs and gives them a
  shared operational locus rather than each being its own
  speculative endpoint. The point-de-capiton framing is operator-load-bearing,
  not decorative — it explains why HEAD's "straightforwardly
  possible" claim holds despite the apparent ambitiousness: the
  ingredients are not being assembled, they're being *anchored*.

### Shape-first IDENTIFY: agent-inhabits-X-peripheral as a family

The pilot mission is naturally one instance of a shape family:
**`agent-inhabits-<X>-peripheral`**. Other instances either already
exist or could exist:

- `agent-inhabits-WM-peripheral` — THIS mission
- `agent-inhabits-Mission-peripheral` — already operational
  (`~/code/futon3c/src/futon3c/peripheral/mission.clj` +
  `M-mission-peripheral.md`)
- `agent-inhabits-Proof-peripheral` — already operational
  (`M-proof-peripheral.md`)
- `agent-inhabits-PSR-PUR-Mesh-peripheral` — already operational
  (`M-psr-pur-mesh-peripheral.md`)
- `agent-inhabits-Arxana-peripheral` — candidate (would be: agent
  that browses the hypergraph, authors annotations, follows
  cross-references); pairs with the substrate-creation thread on
  `M-stack-obligation-browser`
- `agent-inhabits-Forum-peripheral` — partially exists (forum
  bridges); not formally peripheralised
- `agent-inhabits-Dispatch-Bridge-peripheral` — already operational
  (`M-dispatch-peripheral-bridge.md`)

The family is **NOT** novel — futon3c has several `M-*-peripheral.md`
missions that already realise the shape. The pilot mission is novel in
*which X* (the WM) and in *what the agent inhabiting does* (improves
the WM frontend, and ultimately the rest of the stack). Naming the
shape produces the namespace `agent-inhabits-<X>-peripheral/<instance>`
for the family-table entry rather than treating the pilot as a
one-off; it also makes the boundary-property checks from
`M-peripheral-gauntlet.md` directly applicable.

### Scope in/out

**In scope (for v0):**

- Author a peripheral envelope file for the WM-pilot (analogous to
  `mission.clj` / `proof_peripheral.clj` shape) declaring tools,
  scope-bounds, and exit conditions
- Have an agent successfully inhabit the envelope and emit a bell
  back confirming inhabitation
- Pilot can read the WM UI via Playwright (inherits claude-9's
  probe scripts: `wm-tour.mjs`, `wm-detail-probe.mjs`,
  `wm-proxy-verify.mjs`, the 8+ `wm-anchor-*-verify.mjs` scripts)
- Pilot can call the WM CLJ backend (Drawbridge nREPL access per
  `~/code/futon3c/README-drawbridge.md`)
- Pilot can read its primary substrate: `wm-ui-anchors.edn`,
  `war-machine-strategic-vocabulary.edn`, the WM trace files
- Pilot can author or amend anchors (substrate-side write) and
  ship UI changes (UI-side write) — both through the consent-gate
- Pilot communicates with operator via bells/whistles
- WM-I4 invariant preserved structurally (the WM-as-observer-engine
  still doesn't act; the pilot-as-agent-inhabiting-peripheral
  acts within the envelope)

**Deferred to later versions:**

- v1+: full long-running conductor-driven inhabitation (v0 may use
  bell-invoked turns; conductor wiring is a v1 question)
- v1+: pilot makes improvements to the rest of the futon stack (not
  just WM frontend); the HEAD names this as "ultimately" — explicitly
  deferred until v0 settles
- v1+: pilot-as-agent doing the next QA round (recursive-QA closure
  named in HEAD); requires v0 to demonstrate inhabitation works
  before recursion is safe

**Explicitly out of scope (would violate the mission's framing):**

- Pilot-as-WM-itself — would violate WM-I4 (the engine doesn't act);
  the pilot is an *agent inhabiting* the peripheral, not the engine
- Full autonomy without consent-gate — the pilot acts within the
  envelope, but substantive changes route through consent
- Replacing M-war-machine-frontend-upgrade1 — claude-9's mission is
  the surface the pilot inherits; not subsumed

### Completion criteria (testable conditions)

For exit from this mission (v0):

- **C1 — Envelope file exists.** A peripheral declaration file
  exists (analogous shape to `mission.clj` / `proof_peripheral.clj`)
  that names the WM-pilot's tools, scope-bounds, and exit
  conditions. Witness: file at `~/code/futon3c/src/futon3c/peripheral/war_machine_pilot.clj` (or analogous) compiles and registers via the existing Agency mechanism.
- **C2 — Agent inhabits the envelope.** An agent (claude-N or codex-N) is
  invoked via the envelope's registration mechanism and successfully
  emits a "I'm inhabiting" bell. Witness: bell job in
  `/api/alpha/invoke/jobs/<id>` with the inhabitation payload.
- **C3 — Playwright reach.** The inhabiting agent runs at least one
  existing `wm-anchor-*-verify.mjs` Playwright probe and reports the
  expected output. Witness: bell-back carrying the probe's stdout.
- **C4 — CLJ backend reach.** The inhabiting agent invokes a WM
  backend function (e.g., `/api/alpha/war-machine`) and reports
  the response. Witness: bell-back carrying the API response.
- **C5 — Substrate reach.** The inhabiting agent reads and reports
  on the latest entries in `wm-ui-anchors.edn` (anchor count,
  addressed count) and `war-machine-strategic-vocabulary.edn`
  (current mode, sub-kind taxonomy). Witness: bell-back with the
  factual summary.
- **C6 — First UI improvement landed by pilot.** The pilot
  discharges one `:open` anchor from `wm-ui-anchors.edn` (any one
  of 0011, 0012, 0013, 0014, 0015, 0016, 0017, 0018, 0019 as of
  IDENTIFY time) via the established consent-gate (bell-to-operator
  for proposal, operator approval, ship, bell-on-landing).
  Witness: anchor's `:status` flips to `:addressed` with
  `:addressed-by` naming the pilot agent.
- **C7 — WM-I4 preservation.** The pilot's actions are
  demonstrably mediated by the peripheral envelope, not by
  modifying the WM-as-observer-engine. Witness: the WM
  vocabulary's `:a/primitives []` field remains `[]`; the pilot's
  capability envelope is a separate file/structure.
- **C8 — Consent-gate exercised.** At least one pilot action
  routes through an operator-consent step (bell-to-operator with
  proposed change, await approval, proceed). Witness: bell job
  log showing the consent exchange.

A weaker v0 is acceptable if some Cs are deferred to v1 — IDENTIFY
defers C-deferral decisions to the operator. All 8 are listed
because IDENTIFY is the moment to make them visible.

### Relationship to other missions

**Depends on (must already exist or land for this mission to start):**

- `M-war-machine-frontend-upgrade1.md` (claude-9; the surface the
  pilot inherits)
- `M-mission-peripheral.md`, `M-proof-peripheral.md`,
  `M-dispatch-peripheral-bridge.md`, `M-psr-pur-mesh-peripheral.md`
  (the peripheral-as-pattern realisations from which the WM-pilot
  envelope is cloned)
- `M-peripheral-gauntlet.md` (boundary-property tests; applies to
  the WM-pilot envelope)
- The conductor (`apm_conductor.clj`+`_v2`+`_v3`) (working code,
  not a mission)
- Bells/whistles + Drawbridge (working code, not missions)

**Cross-references (in dialogue with):**

- `M-war-machine-vsatarcs-interop.md` (parent QA-coordination
  thread; this mission is the natural successor in that the
  pilot does the next round of QA on what the current round produced)
- `~/code/futon4/holes/missions/` (Arxana side; the
  `agent-inhabits-Arxana-peripheral` candidate sibling)
- M-stack-obligation-browser (candidate mission Joe might author;
  the pilot could be the natural consumer of the obligation
  browser once it lands)

**Enables (downstream missions that become viable once this lands):**

- v1 of itself (pilot extends to the rest of the stack)
- M-stack-obligation-browser becomes operator-AND-pilot-facing
  rather than just operator-facing
- M-pattern-application-diagnostic's data flow can route through
  pilot-coded A→B turns
- The recursive-QA closure named in HEAD

### Source material

- HEAD section above (operator-authored 2026-05-24 verbatim)
- `~/code/futon7a/essays/operator-foreword/operator-foreword.md`
  (the "tantalisingly-close" passage)
- `~/code/futon7a/essays/operator-foreword/annotations.edn`
  (`hx:of:v2:cover-self-guiding-vs-r-criteria` — the cross-reference)
- `~/code/futon3/docs/guides/README-peripherals.md`
- `~/code/futon3c/README-bells-and-whistles.md`,
  `~/code/futon3c/README-walkie-talkie.md`,
  `~/code/futon3c/README-drawbridge.md`
- `~/code/futon3c/dev/futon3c/dev/apm_conductor_v3.clj`
  (+ `_v2`, `_log.edn`)
- `~/code/futon3c/src/futon3c/peripheral/{mission,proof,dispatch}*.clj`
  (existing peripheral implementations to model from)
- `~/code/futon5a/data/war-machine-strategic-vocabulary.edn`
  (especially the new `:a/wm-i4-pilot-clarification` block)
- `~/code/futon5a/data/wm-ui-anchors.edn` (22 anchors / 13 addressed /
  13 coherence-rows as of 2026-05-24; this is the primary
  inhabitable substrate)
- `~/code/futon4/docs/vsatarcs-alignment-completeness.aif.edn`
  (claude-4's R-criteria audit + bilateral-evidence; the AIF
  apparatus side)
- `~/code/futon2/web/war-machine/` (the WM UI codebase claude-9
  has been working on; includes 8+ Playwright probe scripts)
- `~/code/futon2/data/sorrys.edn` (13 typed obligations as of
  2026-05-24; the sorry-registry substrate)
- `~/code/futon2/data/wm-trace/wm-trace-2026-05-*.edn`
  (6 days of accumulated trace as of 2026-05-24)
- All `M-war-machine-*.md` mission files (this mission's family)

### Owner and dependencies

**Owner:** Joe (operator)

**Drivers (across phases — to be confirmed):**

- claude-10 (substrate-side; this IDENTIFY draft; coherence-row
  authoring; cross-agent coordination)
- claude-9 (UI surface continuity; the pilot inherits claude-9's
  scaffolding; possibly claude-9 IS the first pilot inhabitant
  via re-invocation under the peripheral envelope)
- codex-N (substrate-implementation-handoff; Playwright probe
  hardening; envelope file authoring if Clojure-heavy)
- claude-2 (apparatus narrative; WM strategic vocab maintenance;
  the WM-I4 clarification's home)
- A NEW agent slot for the inhabitant once the envelope is ready
  (could be claude-N for N not yet assigned, or codex-N)

**Repos involved:**

- `futon3c` — peripherals, agency, conductor, bells/whistles,
  Drawbridge nREPL; the envelope file probably lands here
- `futon2` — WM backend + UI; the pilot's primary action surface
- `futon4` — Arxana; the entity-view destination for
  click-throughs (anchor-relevant)
- `futon5a` — strategic vocabulary, anchor registry,
  bilateral-evidence; the substrate the pilot reads

**Cross-agent coordination protocol:** bells/whistles per the
established discipline (this session's procedural shape — see
`~/code/futon4/holes/missions/M-war-machine-vsatarcs-interop.md`
checkpoint 2026-05-24). When the pilot inhabits, all other
agents continue to use the same protocol; the pilot is one more
agent in the existing network, not a special case.

### Carried-forward tensions (named for later phases to address)

- **Inhabitation model: long-lived vs short-turn-by-bell.** v0 can
  start with bell-invoked turns; v1 may move to conductor-driven
  continuous operation. MAP should survey the conductor's existing
  capacity for hosting peripheral inhabitants. DERIVE picks.
- **Communication transport: bell vs whistle vs walkie-talkie.**
  Three candidates; the choice affects how the pilot coordinates
  with claude-9 + operator + future siblings. MAP inventories
  each transport's actual capabilities; DERIVE picks.
- **First-pilot identity.** Who IS the inhabiting agent in v0?
  Could be claude-9 re-invoked under the envelope (continuity
  with the surface they already know), could be a new agent
  (clean inhabitation), could be codex-N (implementation-leaning).
  IDENTIFY does not decide this — DERIVE does, after MAP audits
  the candidates.
- **Pilot writer-action-class discipline.** When the pilot
  authors substrate-side changes (e.g., anchor flips), does it
  follow the same writer-action-class discipline that emerged in
  this session's QA round (bell-proposed, operator-or-claude-10
  writes substrate)? Or is the pilot trusted to write directly
  within its envelope? Substantive consent-gate question; DERIVE
  decides.
- **Pilot-self-modification.** Can the pilot modify its own
  envelope? This is the most recursive of the deferred questions
  and probably wants its own phase or a separate sub-mission.
- **Hand-off to future pilots.** When v0 completes and v1 begins
  (or when a pilot's session ends and another starts), what
  state must transfer? Captured per the conductor's structured
  event log? Per the pilot's own bell trail? DERIVE specifies.

### Provenance

IDENTIFY drafted 2026-05-24 by claude-10 via emacs-repl, on Joe's
directive ("OK, let's work on IDENTIFY for M-war-machine-pilot.md
per the mission-lifecycle.md"). HEAD was authored same day, also
by Joe via emacs-repl. The two phases sit in the same session;
operator review of IDENTIFY pending.

### Exit criterion check (for IDENTIFY → MAP transition)

Per mission-lifecycle.md: *"A human has read the proposal and
agrees the gap is real and the scope is right."* **PASSED** —
Joe (emacs-repl 2026-05-24): "The gap is real — indeed this
mission is a kind of point de capiton in the Lacanian sense."

## 2. MAP (2026-05-24, claude-10 drafting)

**Framing per Joe's directive (emacs-repl 2026-05-24):** *"MAP would
likely note that 'we have options' — not only for what conductor
apparatus to use, but probably for most other aspects of the design
that I enumerated earlier as well."* MAP enumerates options factually;
it does not down-select. Picks happen in DERIVE.

### Inventory of existing infrastructure

#### A. Conductor / regulator candidates (3 families, 4+ instances)

| Family | Path | Shape | What it offers a peripheral inhabitant |
|---|---|---|---|
| **APM conductor v1** | `~/code/futon3c/dev/futon3c/dev/apm_conductor.clj` (+ `_log.edn`) | Earliest stepper; baseline | Command-driven step / continue / done; per-agent state |
| **APM conductor v2** | `~/code/futon3c/dev/futon3c/dev/apm_conductor_v2.clj` (+ `_log.edn`) | Iteration on v1 | Same as v1 with refinements |
| **APM conductor v3** | `~/code/futon3c/dev/futon3c/dev/apm_conductor_v3.clj` (+ `_log.edn`) | Stepper with mirror visibility; PARALLEL steppers (multiple agents on same problem); structured event log | `(start! :agent-id X :problem-id Y)` / `continue!` / `backup!` / `skip!` / `done!`; designed around APM problem solving / verbal exams |
| **Tickle v1** | `~/code/futon3c/README-tickle.md` + dev code | Stackable layers (Agent + Watchdog + …); Haiku-backed; IRC-active | Always-on agent + opt-in scan→page→escalate; built around stalled-agent detection |
| **Tickle v2 (Mechanical Conductor)** | `~/code/futon3c/README-tickle2.md` | Pull-driven; FM regulator + Agent Registry + Proof Ledger | Agent finishes → signals availability → FM regulator assigns next obligation; mechanical dispatch; watchdog secondary not primary |

**Observation:** the three families have different orienting metaphors:
- APM conductor = *stepper* (operator drives by sending commands)
- Tickle v1 = *watchdog + agent* (catches silences)
- Tickle v2 = *FM regulator* (pulls work to available agents)

A pilot's "long-running loop" can plausibly attach to any of them; the
choice depends on whether the pilot drives itself (Tickle v2's
pull-model fits), is driven by Joe (APM conductor's command-driven
model fits), or is supervised against silence (Tickle v1's watchdog fits).

#### B. Communication transports (4 candidates, all working today)

| Transport | Endpoint | Shape | Used today by |
|---|---|---|---|
| **bell** | `POST /api/alpha/bell` | Async; 202 accept + `job-id`; poll `/api/alpha/invoke/jobs/<id>` for terminal state | claude-10 ↔ claude-9 this session (all anchor coordination) |
| **whistle** | `POST /api/alpha/whistle` | Sync one-shot; single terminal JSON response | Earlier in session for the initial pairing handshake |
| **whistle-stream** | `POST /api/alpha/whistle-stream` (or `whistle` with `"stream":true`) | NDJSON stream with progress + terminal event | For long-running work with live visibility |
| **walkie-talkie** | Multiple `POST /api/alpha/...` endpoints | Universal-tool surface for any agent in any context (CLI / IRC / peripheral / REPL); evidence production (PSR/PUR/PAR), ArSE question/answer, queries | Built for "agent radios in from anywhere"; less for agent-to-agent than agent-to-substrate |

**Observation:** bell/whistle/whistle-stream are the agent-to-agent
transports. Walkie-talkie is the agent-to-substrate transport
(evidence ingestion + queries). The pilot will likely use BOTH:
bell/whistle for coordinating with operator and claude-9; walkie-talkie
for emitting PSR/PUR records and querying state.

#### C. Peripheral pattern (well-established; 4+ working instances)

Generic engine: `~/code/futon3c/src/futon3c/peripheral/cycle.clj` — `CycleDomainConfig` with `:phase-order`, `:phase-tools`, `:setup-tools`, `:tool-ops`, `:required-outputs`, `:cycle-begin-tool`, `:cycle-advance-tool`, `:state-init-fn`, `:fruit-fn`, `:exit-context-fn`, `:phase-tags-fn`, `:autoconf-fn`.

Working domain configurations (clone-from candidates):

| Peripheral | Domain | 9-phase shape | Notes |
|---|---|---|---|
| **Proof** (`peripheral/proof.clj`) | proof cycles | observe → propose → execute → validate → classify → integrate → commit → gate-review → completed | Foundational; "the paren IS the gate"; refactored from standalone record to cycle-machine wrapper |
| **Mission** (`peripheral/mission.clj`) | code development cycles | same 9-phase shape; obligations not ledger items; mission spec not canonical statement; failed approaches not failed routes; Table 25 phase-sigil-tags | Closest semantic to WM-pilot ("improve a system") |
| **Mission Control** (`peripheral/mission_control.clj`) | mission portfolio management | distinct from individual missions; observes the mission-portfolio level | Related; not a clone-from |
| **Portfolio Inference** (`peripheral/portfolio_inference.clj`) | portfolio-level decisions | uses backend (portfolio_inference_backend.clj) | Related; not a clone-from |
| **Dispatch Bridge** (`M-dispatch-peripheral-bridge.md`) | dispatch | bridge between peripheral surfaces | Sibling |
| **Chat** (`peripheral/chat.clj`), **Edit** (`peripheral/edit.clj`), **Explore** (`peripheral/explore.clj`), **Reflect** (`peripheral/reflect.clj`) | conversation / editing / exploration / reflection | thinner peripherals; less cycle-machine-driven | Lighter peripherals; the WM-pilot is probably heavier (cycle-machine-driven) |

**Observation:** the **Mission peripheral is the closest clone-from** for
WM-pilot's shape. "Code development cycles" maps onto "improve the WM
apparatus." Cycle machine is the engine; mission_shapes.clj defines the
domain config; mission_backend.clj defines the domain operations.

#### D. Playwright probes (8+ scripts; claude-9-authored; scratch-grade)

| Probe | What it verifies |
|---|---|
| `wm-tour.mjs` | General WM UI tour |
| `wm-detail-probe.mjs` | Detail-panel functionality |
| `wm-proxy-verify.mjs` | Proxy / data-fetching behaviour |
| `wm-anchor-0004-verify.mjs` | HUD Mode tooltip |
| `wm-hud-cluster-verify.mjs` | HUD cluster (anchors 0003/0005/0006/0007) |
| `wm-anchor-0001-verify.mjs` | Workstream-balance tooltips |
| `wm-anchor-0002-verify.mjs` | Loop Health bars |
| `wm-anchor-0008-verify.mjs` | Cargo warning restructure |
| `wm-anchor-0009-verify.mjs` | Recommended Next Move |
| `wm-anchor-0010-verify.mjs` | Self-watch warnings + hand-off button |

All run against `http://localhost:8710/`. Headless dialog handling is
the one known gap (anchor 0010 hand-off prompt() not headlessly tested;
punted to operator-side smoke).

**Observation:** the pilot inherits these as ready-to-run probes; no
new Playwright infrastructure needed for v0 C3 (Playwright reach).

#### E. WM backend reach mechanisms (3 paths)

| Path | What it offers |
|---|---|
| `/api/alpha/war-machine` HTTP | JSON snapshot of WM state (judgement.mode, loop-health arrows, etc.) |
| `/api/alpha/aif-stack/live` HTTP | Live AIF-stack data |
| Drawbridge nREPL (`~/code/futon3c/README-drawbridge.md`, port 6768) | Full CLJ REPL access to the futon3c JVM; for code reloads and exploration |

**Observation:** Drawbridge is the most powerful path (full REPL); HTTP
is the safer path (read-only by default). The pilot probably wants
Drawbridge access for some operations but tooling discipline applies (don't
:reload protocols per the long-standing rule).

#### F. Substrate readers (existing)

| Reader | Reads | Used by |
|---|---|---|
| `arxana-vsatarcs-sorrys.el` | `~/code/futon2/data/sorrys.edn` | Claude-4's Q4 closure; the operator-facing sorry-registry surface |
| `arxana-vsatarcs-bilateral.el` | `~/code/futon4/docs/vsatarcs-alignment-completeness.aif.edn :bilateral-evidence` | Claude-4's Q7 closure; bilateral-evidence renderer |
| `arxana-vsatarcs-wm-bridge.el` | `~/code/futon2/data/wm-trace/wm-trace-YYYY-MM-DD.edn` :mu-post | Cross-side belief comparison |
| `arxana-vsatarcs-r10-tap.el` | file-notify on WM trace directory | R10 wakeup tap |
| `arxana-vsatarcs-belief.el` | local belief state | R1 reader-chrome |
| `arxana-vsatarcs-likelihood.el` | per-status likelihood weights | R3a + R3c closure |
| `arxana-vsatarcs-anticipation.el` | `~/code/calendar/events.edn` forward-axis | Q3 anticipation block |

Plus the new artefact this session:

- `wm-ui-anchors.edn` (22 anchors / 13 addressed / 13 coherence-rows;
  no dedicated reader yet — would be a v1 substrate move)

**Observation:** the pilot inherits a mature *reading* substrate.
*Writing* substrate (especially anchor flips + coherence rows) is the
new behaviour the pilot demonstrates.

#### G. Agency registration mechanics (working today)

- `POST /api/alpha/agents/auto` with `session-id` matching `~/.claude/projects` encoding
- Bells trigger new `claude -p` invocations against the session-id
- Tickle registers via `(rt/register-tickle! {:agent-id ... :invoke-fn ...})`
- Peripherals register through the same `agency.registry` mechanism

**Observation:** Agency registration is plumbed end-to-end; the
WM-pilot envelope adds an entry, not a new mechanism.

#### H. Candidate first-pilot agents (3 paths)

| Candidate | Pros | Cons | Continuity with surface |
|---|---|---|---|
| **claude-9 re-invoked** | Knows the WM UI surface intimately; just shipped 14 anchors + 3 bug fixes; understands the `anchor-span` helper, the `tick-actions` map, the verify-probe pattern | Continuity vs clean-slate trade-off; claude-9's current session would either extend (continuity) or terminate-and-reinvoke (clean) | Maximum continuity |
| **New claude-N** | Clean inhabitation; tests the envelope's introduction-from-scratch property; demonstrates the peripheral is self-documenting | No prior WM-UI context; needs to onboard via reading the substrate | Zero-baseline test |
| **codex-N** | Implementation-leaning; could harden the Playwright probes from scratch-grade to test fixtures; could author the envelope file itself per the GitHub-issue handoff protocol | Less suited to interactive QA-style decisions; better at scoped implementation handoffs | Implementation-only |

**Observation:** all three are viable; DERIVE picks. The choice
isn't binary — the pilot could be claude-9-re-invoked for v0,
new-claude-N for v1, codex-N for specific implementation handoffs.

### Inventory of existing data the pilot would inhabit

| Substrate | Count / volume (as of 2026-05-24) | Notes |
|---|---|---|
| `wm-ui-anchors.edn` | 22 anchors / 13 addressed / 9 open / 13 coherence-rows / 5 sub-kinds | This session's primary output |
| `vsatarcs-alignment-completeness.aif.edn` :bilateral-evidence | 19+ entries | Claude-4's apparatus |
| `vsatarcs-alignment-completeness.aif.edn` :r-criterion-audit | R1–R12 with witnesses | Same |
| `vsatarcs-alignment-completeness.aif.edn` :reader-criterion-audit | Q1–Q8 with witnesses | Same |
| `war-machine-strategic-vocabulary.edn` | 12 observation channels + 6 modes + free-energy machinery + WM-I4 pilot clarification | Source of truth for WM apparatus |
| `sorrys.edn` | 13 typed obligations | Schema v2 (`:kind` per Joe 2026-05-18) |
| `wm-trace-2026-05-*.edn` | 6 days accumulated | Trace IS the state store |
| Operator's foreword + v2 annotations | 13 annotations + 56-line essay | The synoptic surface |
| Anthology (futon5a/holes/stories/) | 80+ stories (48 leaves lifted + 8 globes + 9 devmaps + THE-STACK.aif.edn etc.) | Published VSATARCS substrate |
| Pattern library (futon3/library/) | 853+ flexiargs across ~30 families | The futon3 P1 substrate |

### Q1–Q8: Carried-forward tensions from IDENTIFY, answered factually

(MAP answers each with FACTS; DERIVE makes design picks.)

**Q1 — Conductor: long-lived vs short-turn-by-bell.** *Three conductor candidates above carry different orienting metaphors (APM = stepper, Tickle v1 = watchdog, Tickle v2 = FM regulator). All three can host a peripheral inhabitant; the pilot's loop-shape is the question DERIVE settles.* Surprise during MAP: the existence of Tickle v2 as a clean redesign of Tickle v1 — narrows the "which Tickle?" sub-question.

**Q2 — Transport choice.** *Four transports working today: bell (async), whistle (sync one-shot), whistle-stream (NDJSON), walkie-talkie (universal-tool surface). All four are operationally available; the pilot will likely use bell/whistle for coordination + walkie-talkie for substrate writes. The choice is not exclusive.*

**Q3 — Clone-from for envelope.** *Mission peripheral is the closest semantic clone-from for WM-pilot (code-development-cycles ≈ improve-the-WM-apparatus). Proof peripheral is a viable alternative if the WM-pilot's cycle shape ends up closer to proof-cycles (observe→propose→execute→validate→...). Both inherit from `cycle.clj` so the underlying machine is the same.*

**Q4 — Agency registration.** *Already plumbed end-to-end. POST /api/alpha/agents/auto + session-id matching. The WM-pilot envelope adds an entry; no new mechanism needed.*

**Q5 — What peripheral agents can do.** *Bounded by setup-tools + per-phase phase-allowed-tools. Generic tool families: `:read`, `:glob`, `:grep`, `:bash-readonly` (universal); domain-specific tools per peripheral. For WM-pilot, the domain tools likely include `:anchor-flip`, `:anchor-author`, `:coherence-row-author`, `:playwright-run`, `:wm-api-query`, `:drawbridge-eval`, `:bell-emit`, `:walkie-talkie-psr`, `:walkie-talkie-pur`. Tool-ops classification (`:observe` vs `:action`) per the cycle machine.*

**Q6 — First-pilot identity.** *Three candidates (claude-9 / new-claude-N / codex-N) characterized above. All viable; choice is DERIVE-stage.*

**Q7 — Writer-action-class discipline for pilot's substrate writes.** *Existing peripherals (mission, proof) write substrate directly via their envelope-granted tools (e.g., `:obligation-upsert`, `:mission-spec-update`). The envelope's tool-set IS the writer-action-class boundary. For WM-pilot's substrate writes, the same pattern works: declare `:anchor-flip`, `:coherence-row-author` etc. in the envelope; the pilot can write directly to wm-ui-anchors.edn within those tool-grants. Consent-gate semantics layer on top: substantive changes route through bell-to-operator before the tool fires.*

**Q8 — Consent-gate shape.** *Two layers in existing peripherals: (a) the envelope's tool-restriction is the structural gate (the pilot literally can't do X if X isn't in its allowed tools); (b) explicit consent-bells before destructive actions (the bell-to-operator pattern that emerged in this QA session). For WM-pilot, both layers apply: tool-restriction structural gate + explicit consent-bell for substantive UI changes or substrate mutations.*

### Ready vs missing (the punchline)

| Ready (no new code needed) | Missing (the actual work) |
|---|---|
| Cycle machine (`peripheral/cycle.clj`) | `peripheral/war_machine_pilot.clj` envelope file |
| Mission peripheral as clone-from shape | `peripheral/war_machine_pilot_shapes.clj` (phase-order, phase-tools, required-outputs for the pilot's cycle) |
| Proof peripheral as alternate clone-from | `peripheral/war_machine_pilot_backend.clj` (the WM-domain operations) |
| 3 conductor candidates (APM v1/v2/v3, Tickle v1, Tickle v2) | Conductor binding decision (DERIVE picks) |
| 4 communication transports (bell, whistle, whistle-stream, walkie-talkie) | Transport choice per use-case (DERIVE picks) |
| Agency registration mechanics | Pilot's session-id provisioning |
| 8+ Playwright probes (scratch-grade) | Probe-runner tool for pilot's `:playwright-run` invocation (or just `:bash-readonly` with explicit path) |
| WM backend HTTP API + Drawbridge nREPL | `:wm-api-query` + `:drawbridge-eval` tool wrappers (or just `:bash-readonly` + `curl`) |
| 7 substrate readers (sorrys, bilateral, wm-bridge, etc.) | `wm-ui-anchors.edn` reader (no `arxana-vsatarcs-anchors.el` yet — would be v1 substrate move) |
| 3 first-pilot candidates characterised | Picking one (DERIVE) |
| Consent-gate discipline (emerged this session) | Encoding consent-gate as an explicit tool family or sub-protocol in the envelope |

**Net "missing":** mostly the envelope file + its companions (shapes + backend). Plus DERIVE-stage decisions (conductor, transport, first-pilot). The actual *new code* is bounded: clone mission peripheral's three files, swap domain-specific bits, register with Agency. Estimated bounded-scope.

### Surprises documented during MAP

1. **Tickle v2 exists** and is a distinct design from Tickle v1 (mechanical conductor with FM regulator, pull-driven). Narrows the Q1 sub-question.
2. **Cycle machine is already extracted as a generic engine** (`cycle.clj`) — the peripheral pattern is more mature than IDENTIFY assumed; clone-from is a standard maneuver across 4+ existing peripherals.
3. **Walkie-talkie is universal-tool-surface, not just one more transport** — it's the agent-to-substrate transport (evidence ingestion + queries), distinct from bell/whistle which are agent-to-agent. The pilot uses both, not "picks one."
4. **`futonic logic mapping`** appears in mission.clj's docstring (`象 / 部 / 味 / 🔮 / 香 / 捨`) and is operational not decorative — the cycle machine's configuration map LITERALLY uses these glyphs as field names. The pilot inherits this mapping.
5. **wm-ui-anchors.edn has no dedicated reader yet** — claude-4's reader-criteria-audit-pattern (`arxana-vsatarcs-sorrys.el` for sorrys.edn etc.) hasn't been extended to the new anchor file. Worth a v1 substrate-creation note: `arxana-vsatarcs-anchors.el`.

### Exit criterion check (for MAP → DERIVE transition)

Per mission-lifecycle.md: *"Every MAP question has a concrete answer. The 'ready vs missing' table is complete."* Q1–Q8 above all have concrete factual answers; ready-vs-missing table is complete; surprises documented. **PASSED** — Joe greenlit DERIVE drafting (emacs-repl 2026-05-24, "Go").

## 3. DERIVE (2026-05-24, claude-10 drafting per Joe's "Go" on the 4-pattern-survey synthesis)

### Pattern-survey synthesis (carrying MAP's enumeration into DERIVE's commitments)

Search via `futon3a/scripts/notions_search.py` (per Joe 2026-05-24 directive: use futon3a rather than brute-force grep) surfaced 4 directly-relevant patterns from the 853+ flexiarg library, 2 false positives (word-collision on "pilot"), 1 in-multiarg (lower priority). Scores were uniformly LOW (0.06–0.11), corroborating that the pilot's shape is partly novel — a finding worth carrying forward as a substrate-creation thread back into M-pattern-application-diagnostic.

The 4 directly-relevant patterns:

1. **`software-design/agent-decorator-pattern`** (p4ng/agent-decorator-pattern in the embedding index; actually lives in `software-design/`) — wrap commands with decorators (Logged / Timed / PermissionChecking / Synchronized / Retrying) that add behaviour before/after delegation; decorators "at the edge, not deep inside each action."
2. **`software-design/agent-command-pattern`** — encapsulate every operation as a Command value carrying `:id`, parameters, provenance, capability tags, optional `undo!`. Run only through execution pipeline.
3. **`agent/intent-handshake-is-binding`** — a run should not start until the agent has restated and bound the intent and scope. Require handshake that echoes intent / scope / constraints / success criteria.
4. **`agent/provisional-claims-ledger`** — unsupported claims tracked, timeboxed, resolved (claim / reason / evidence needed / expiry / owner).

(Discarded: `ai4ci/pilot-feasibility` — EPSRC grant-pilot, not agent-inhabitant-pilot. `f1/p8` — futon1 pilot-storage, not agent-inhabitant-pilot.)

### Entity types

| Entity | Identity | Source | Notes |
|---|---|---|---|
| **Pilot inhabitant** | `(agent-id + session-id)` | Agency registration via `POST /api/alpha/agents/auto` | An agent inhabiting the WM-pilot envelope. v0 candidates: claude-9 re-invoked / new claude-N / codex-N. |
| **Pilot envelope** | `:war-machine-pilot` (declared in shapes file) | Authored (cloned from Mission peripheral) | Configuration: phase-order + phase-tools + setup-tools + tool-ops + required-outputs + autoconf-fn. Per cycle.clj's `CycleDomainConfig`. |
| **Pilot cycle** | `cycle-id` (UUID; per-invocation) | Generated when pilot enters a cycle | Instance of the cycle machine running under the envelope. Same shape as proof/mission cycles. |
| **Pilot action (Command)** | `action-id` (UUID; per-action) | Authored by pilot inhabitant within a cycle phase | Command value per `agent-command-pattern`. Carries `:id`, `:params`, `:provenance`, `:capability-tags`, optional `:undo!`. Persisted to pilot-trace. |
| **Consent-gate event** | `(pilot-id + cycle-id + intent-summary)` | Emitted by pilot at intent-handshake time | Per `intent-handshake-is-binding`. Echoes intent / scope / constraints / success-criteria. Substantive Commands MUST be preceded by a consent-gate event in the same cycle. |
| **Provisional claim** | `claim-id` (UUID; per-hypothesis) | Authored by pilot when acting before evidence is available | Per `provisional-claims-ledger`. Carries `:claim`, `:reason`, `:evidence-needed`, `:expiry`, `:owner`. Resolved to confirmed/retracted; surfaced during handoff. |
| **Anchor-flip event** | `(anchor-id + flip-direction)` | Pilot Command writing to `wm-ui-anchors.edn` | Specific Command type. Audit trail = the anchor file's git history + the pilot-trace. |
| **Coherence-row-author event** | `(coherence-id + row-content-hash)` | Pilot Command writing to `:coherence-evidence` block | Specific Command type. |
| **Handoff-bell event** | `(target-agent + payload-hash + bell-job-id)` | Pilot Command emitting `POST /api/alpha/bell` | Specific Command type. Used for: (a) consent-gate to operator, (b) hand-off to other agents per anchor 0010 pattern. |
| **Pilot-action event** | `(target-file + diff-hash + consent-gate-event)` | Pilot Command modifying WM UI or futon stack | Substantive Command type. MUST cite preceding consent-gate event. |

### Relation types

- `pilot inhabits envelope` — binary; one pilot per envelope at a time (single-inhabitant invariant for v0)
- `envelope grants tool to pilot` — binary, multi-target (envelope grants N tools)
- `cycle runs under envelope` — binary; cycle-id has exactly one envelope-id
- `action issued in cycle phase` — n-ary: (action, cycle-id, phase, timestamp); phase-gates which tools are permissible
- `consent-gate-event precedes substantive-action` — binary ordered relation; substantive action MUST cite consent-gate id
- `provisional-claim resolves-to (confirmed | retracted | expired)` — binary terminal state
- `pilot-action mutates substrate-element` — n-ary: (pilot-action, target-file, consent-gate-event, witness)
- `pilot-cycle emits trace records` — binary; trace IS the state store (claude-2's principle 2026-05-19 — extended to pilot)

### Invariant rules

Five rules total — three carried forward from WM exotype v2 (`futon5a/data/war-machine-exotype-v2.edn` :invariants), two NEW for the pilot.

**Carried from WM v2:**

- **WM-I1** (engine-level read-only) — applies as-is; the pilot is NOT the engine.
- **WM-I4** (sovereignty preserved with pilot clarification) — applies as-is; the pilot acts within the envelope, not as the engine.
- **WM-I5** (anchor-coherence) — applies as the pilot's *target*; the pilot's job is partly to maintain WM-I5 by addressing open anchors.

**NEW for the pilot:**

- **Pilot-I1** (consent-gated-substantive-action; tightened per DR-V2 2026-05-24) — every Pilot Command of `:capability-tags :substantive` MUST cite a preceding `:consent-gate-event-id` in the same cycle, **EVEN IF the underlying tool is envelope-granted**. *Check:* per-action trace record carries `:consent-gate-event-id`; substantive actions without it are invariant violations. *Witness:* pilot-trace. The closed-set of substantive tools MUST be declared in the shapes file (e.g., `:substantive-tools #{:anchor-flip :coherence-row-author :pilot-action :handoff-bell}`); non-listed tools default to `:non-substantive` and don't require per-action consent.
- **Pilot-I2** (provisional-claim-resolution) — every provisional claim authored by the pilot MUST have `:expiry` and reach a `:resolution-status` ∈ #{:confirmed, :retracted, :expired} before the next pilot cycle begins. *Check:* claim-ledger query at end of each cycle. *Witness:* claim-ledger file.
- **Pilot-I3** (single-inhabitant-per-envelope) — at any moment, at most one pilot inhabitant per WM-pilot envelope. *Check:* Agency registry query for `:war-machine-pilot` peripheral; cardinality ≤ 1. *Witness:* Agency registry.

### Data flow

```
SUBSTRATE INPUTS (read-only for pilot):
  wm-ui-anchors.edn → pilot
  :coherence-evidence block → pilot
  sorrys.edn → pilot
  wm-trace-YYYY-MM-DD.edn → pilot
  war-machine-strategic-vocabulary.edn → pilot
  bilateral-evidence (claude-4's) → pilot
  operator-foreword + annotations → pilot

PILOT ACTIONS (gated by envelope tool-set + consent-gate):
  pilot ⊕ Command(:anchor-flip) → wm-ui-anchors.edn :status field
  pilot ⊕ Command(:coherence-row-author) → :coherence-evidence block
  pilot ⊕ Command(:handoff-bell) → POST /api/alpha/bell
  pilot ⊕ Command(:pilot-action, :substantive) → WM UI / futon stack code-edit
  pilot ⊕ Command(:consent-gate-emit) → operator (bell) or self-ledger (for non-substantive)

TRACE EMISSION:
  every pilot Command → pilot-trace-YYYY-MM-DD.edn (append-only EDN-lines, parallel to wm-trace)

CLAIMS LEDGER:
  pilot ⊕ provisional-claim → pilot-claims-ledger.edn
  pilot ⊕ claim-resolution → pilot-claims-ledger.edn (in-place update OR append per discipline)
```

### IF/HOWEVER/THEN/BECAUSE for non-obvious design decisions

**Choice 1: Conductor — DEFERRED to v1 (NEW per DR-V6, 2026-05-24)**

- **IF** the pilot's v0 success-criteria (C1–C8) can be met without any long-running conductor (pilot runs one cycle per bell-invocation; pilot's loop closure within a single cycle satisfies inhabitation semantic),
- **HOWEVER** Joe's operator-experience with Tickle has been "very annoying (maybe partly that is by design)" — committing v0 to a Tickle binding risks importing that friction into the pilot's first inhabitation,
- **THEN** v0 operates without a conductor — pilot is invoked by direct bell-on-demand, runs one cycle, emits fruit, exits. The SAME shape as the spike (`spike-check` is a one-shot invocation, not a long-running loop). v1+ MAY add a conductor; Tickle v2 stays a candidate but is NOT a v0 commitment.
- **BECAUSE** the spike validated that the pilot envelope works in a one-shot invocation shape; conductor-binding adds continuous-operation behaviour but is not load-bearing for the inhabitation semantic itself. Joe (verbatim, 2026-05-24): "Let's assume that adding Tickle is the last and optional step in the build out — which is probably true." This defers a known-friction substrate choice until after the pilot's core loop has proven itself.

*Previous Choice 1 (Tickle v2 as v0 conductor) is moved to v1-candidate-list. The 3 conductor families (APM v1/v2/v3, Tickle v1, Tickle v2) from MAP §A remain the candidate-set for whichever v1+ conductor binding lands; the choice is deferred until v0 informs which long-running shape is actually wanted.*

**Choice 2: Clone-from — Mission peripheral as DERIVE shape donor**

- **IF** the pilot's cycle is "improve the WM apparatus" (substrate observation → propose change → execute → validate → integrate),
- **HOWEVER** the proof peripheral's cycle (observe → propose → execute → validate → classify → integrate → commit → gate-review → completed) is similar but proof-domain-specific (canonical statement, failed routes, ledger items),
- **THEN** clone from Mission peripheral (`futon3c/src/futon3c/peripheral/mission.clj` + `mission_shapes.clj` + `mission_backend.clj`) rather than Proof peripheral. Pilot's domain is "improve a code system over time," semantically mission-shaped.
- **BECAUSE** Mission peripheral's domain config maps onto the pilot's domain with the right granularity: obligations ↔ open anchors, mission spec ↔ pilot envelope spec, failed approaches ↔ failed pilot moves (with rationale), phase-sigil-tags ↔ pilot phase tags. Proof peripheral's domain config would need too much remapping.

**Choice 3: Transport — bell (handoff) + walkie-talkie (substrate writes) + whistle (consent-gate queries to operator)**

- **IF** the pilot needs three distinct communication needs: (a) hand off work to other agents, (b) emit evidence to substrate, (c) request operator consent,
- **HOWEVER** picking ONE transport for all three would conflate concerns (bell is async fire-and-forget; whistle is sync request-response; walkie-talkie is substrate-write),
- **THEN** use three transports for three concerns: **bell** for (a) hand-off to other agents (matches anchor 0010 pattern); **walkie-talkie** for (b) substrate evidence (`POST /api/alpha/evidence/psr` etc.); **whistle** for (c) consent-gate queries to operator (sync request-response matches the consent semantic — pilot blocks pending operator approval).
- **BECAUSE** transport-per-concern keeps the discipline legible; conflating would obscure which calls are async vs sync vs substrate-side. Joe's MAP framing ("we have options... not exclusive") explicitly supports the multi-transport stance.

**Choice 4: First-pilot identity — claude-9 re-invoked for v0; new claude-N for v1 audit**

- **IF** the pilot's first cycle benefits from continuity with claude-9's WM UI surface knowledge (anchor-span helper, tick-actions map, verify-probe pattern, Playwright probes),
- **HOWEVER** continuity-via-claude-9 doesn't test the envelope's clean-start property (does the envelope's :setup-tools + :autoconf-fn actually bootstrap a new agent, or does it rely on out-of-band context?),
- **THEN** ship v0 with claude-9 re-invoked under the envelope (continuity); ship v1 with a NEW claude-N invoked under the same envelope (zero-baseline test). Audit any discrepancy as a finding.
- **BECAUSE** the envelope's correctness IS the substantive output of the mission; demonstrating it works with a continuity-agent first reduces risk; demonstrating it works with a clean-start agent second validates the envelope. Both demonstrations are necessary; sequencing them is cheaper than parallel.

**Choice 5: Writer-action-class — direct write within envelope grants, with explicit substantive-tool consent-gate (refined per DR-V2 2026-05-24)**

- **IF** the procedural shape this session established (claude-9 bells claude-10; claude-10 writes substrate) was the operator-mediated bilateral discipline,
- **HOWEVER** the pilot is an AGENT INHABITING the envelope; the envelope's tool-grant gates which tools are available, but operator-consent on substantive use of those tools is still required (Pilot-I1); requiring a second-agent intermediary defeats the inhabitation semantic,
- **THEN** envelope-granted tools fall into two operational classes — `:non-substantive` (no per-action consent; pilot writes directly: e.g., reading substrate, internal trace emission) and `:substantive` (Pilot-I1 applies; per-action `:consent-gate-event-id` required: anchor-flip, coherence-row-author, pilot-action, handoff-bell). The closed substantive-tool-set is declared in the envelope's shapes file as `:substantive-tools #{...}`.
- **BECAUSE** the inhabitation semantic requires the agent be a first-class actor within its envelope (no two-agent indirection), but operator-sovereignty over load-bearing changes requires per-substantive-action consent. Splitting the tool-set into substantive vs non-substantive at the envelope's :setup-tools level makes the distinction structural rather than discretionary. Maps directly onto `agent-contract` pattern's "observe before acting, attribute all changes, respect invariants" — the consent-gate-event IS the attribution.

### View/UI specifications

**Pilot status surface** (NEW, lives in WM UI):

- **Status badge** in HUD or sidebar: `Pilot: idle` / `Pilot: in cycle (phase X)` / `Pilot: consent-pending` / `Pilot: handing off`.
- **Click-through** opens a `Pilot Activity` panel showing: current cycle id, current phase, last 5 actions, current provisional-claims-ledger summary, last consent-gate event.
- **Anchor-side** (in `wm-ui-anchors.edn` :anchors[]): new field `:addressed-by-pilot? true` distinguishes pilot-discharged anchors from operator-or-claude-9-discharged ones. Coherence-rows similarly tagged.

**No additional UI required** for v0's other surfaces — pilot uses existing surfaces (Playwright probes, WM API, Drawbridge, anchor file, sorrys, vocab, foreword, bilateral-evidence) directly via envelope-granted tools.

### Wiring diagram

Done in MAP — `~/code/futon5a/data/war-machine-exotype-v2.edn` carries the pilot cluster (C-pilot-envelope + C-pilot-inhabitant + C-conductor + C-anchor-flip + C-coherence-author + connection to existing slow-cycle components). DERIVE references that diagram rather than redrawing.

### Fidelity contract

Not strictly a port mission — closer to greenfield-on-top-of-existing-substrate. Partial fidelity contract:

- **Preserve set** (must not regress under pilot inhabitation):
  - WM-I1 (engine-level read-only): pilot doesn't modify the WM observer engine's code path
  - WM-I4 (sovereignty preserved with pilot clarification): pilot's actions route through envelope-gated tools with consent-gate
  - WM-I5 (anchor-coherence): pilot's job is partly to maintain this; pilot must not WEAKEN it
  - Anchor-file schema: pilot writes follow the same shape claude-10 has used (e.g., `:status :addressed`, `:addressed-by`, `:verified-by`, `:coherence-evidence-row` pointer)
  - Coherence-evidence row schema: pilot writes follow the existing sub-kind taxonomy (7 sub-kinds as of 2026-05-24)
- **Adapt set** (extend with awareness):
  - WM exotype v2 wiring: pilot cluster is part of the diagram; pilot's actions register as new component-level wiring
  - Bell payload schemas: pilot's bells may add new payload fields; legacy callers (claude-9, claude-10) must continue to parse
- **Drop set**: nothing.

### PSRs (Pattern Selection Records — proposing, not committing)

Per mission-lifecycle.md PSR discipline (PSRs precede commitment; they're written BEFORE the pattern is applied). The 4 PSRs below are proposals; they become committed when INSTANTIATE writes the envelope file. Could also be promoted to standalone files at `~/code/futon3c/holes/labs/M-war-machine-pilot/psr/` per the lifecycle convention; for v0 I'm drafting them inline.

**PSR-1: `software-design/agent-decorator-pattern` → pilot envelope-as-decorator-stack**

- **Context**: standardising cross-cutting behaviours (logging, timing, permission-checking, audit) around pilot actions.
- **Decision**: the envelope's tool-set IS a decorator stack. Each granted tool (`:anchor-flip`, `:coherence-row-author`, `:handoff-bell`, `:pilot-action`) is the inner Command; the envelope decorates with `:PermissionCheckingDecorator` (consent-gate per Pilot-I1), `:LoggedCommand` (bell-trail to pilot-trace), `:TimedCommand` (cycle-phase tag).
- **Alternatives considered**: ad-hoc per-tool-implementation (each tool re-implements logging / consent / timing). Rejected because it duplicates cross-cutting concerns across the tool-set (the pattern's own anti-pattern).
- **Outcome (target)**: the pilot's actions are uniformly logged + consent-checked + timed without each tool re-implementing the discipline.
- **Confidence**: high — the pattern is a textbook fit; the futon-theory glyphs (👘/示) match the envelope semantic; library cross-reference is the canonical agent-decorator pattern.

**PSR-2: `software-design/agent-command-pattern` → pilot actions as Command values**

- **Context**: defining a reliable execution pipeline for pilot actions.
- **Decision**: each pilot action is a Command value: `{:id <uuid> :type :anchor-flip|:coherence-row-author|:handoff-bell|:pilot-action :params {...} :provenance {:pilot <id> :cycle <id> :phase <phase>} :capability-tags #{:substantive | :non-substantive | :consent-gated} :undo? <fn or nil>}`. Run only through the envelope's execution pipeline.
- **Alternatives considered**: direct-function-call-per-action (no Command abstraction). Rejected because it loses provenance, audit trail, reordering, and undo capabilities. The pattern's "uniform command abstraction gives the stack a shared action language" is exactly what we need for cross-pilot-cycle replayability.
- **Outcome (target)**: pilot actions become inspectable + replayable + auditable; cycle traces become rich histories rather than opaque function calls.
- **Confidence**: high — same library family as PSR-1; designed to compose.

**PSR-3: `agent/intent-handshake-is-binding` → consent-gate as handshake at cycle start**

- **Context**: pilot begins a new cycle; needs to bind intent before substantive action.
- **Decision**: at cycle start, pilot emits a consent-gate event that echoes (intent, scope, constraints, success-criteria) and either (a) routes to operator-bell-then-await-ack (substantive cycles), or (b) writes a self-ledger entry (non-substantive cycles like read-only exploration). Substantive actions within the cycle MUST cite this consent-gate event id.
- **Alternatives considered**: per-action consent (each substantive action separately consent-gated). Rejected because it floods the operator with per-action approvals and loses cycle-level intent binding. Cycle-level consent + Pilot-I1's per-substantive-action citation is the right granularity.
- **Outcome (target)**: pilot runs are auditable; intent drift is detectable (consent-gate event content vs actual action history); operator workload is bounded.
- **Confidence**: high — pattern explicitly states "binding intent makes the run auditable and prevents silent goal drift." Direct semantic match.

**PSR-4: `agent/provisional-claims-ledger` → hypothesis tracking with expiry + post-discharge falsification**

- **Context**: pilot reads substrate, forms hypotheses about state, may act before all evidence is in (e.g., authoring anchors with substrate hypotheses like the ghost-cells case I had earlier this session).
- **Decision**: pilot maintains a `pilot-claims-ledger.edn` per cycle. Each provisional claim: `{:id <uuid> :claim <text> :reason <text> :evidence-needed <text> :expiry <timestamp> :owner :pilot :status <:open|:confirmed|:retracted|:expired> :resolution-rationale <text-when-resolved>}`. At cycle end, every claim must be in a terminal state. Cross-references to the existing operator-QA-finding-as-hypothesis pattern (recorded on anchor 0019's `:substrate-finding-post-discharge` field this session).
- **Alternatives considered**: hypotheses-as-comments-in-anchor-fields (informal). Rejected because it leaves uncertainty silent — the pattern explicitly names this as the anti-pattern ("treating provisional claims as facts hides uncertainty"). The bilateral discipline this session already converged on `:hypothesis` framing; the ledger formalises it.
- **Outcome (target)**: pilot's hypothesis-laden moves become inspectable + falsifiable; future agents can read past pilot cycles and see which claims held vs which were retracted.
- **Confidence**: high — the pattern is exactly the formalisation of the operator-QA-as-hypothesis pattern that emerged in this session. Adopting it formalises an already-validated discipline.

### Carried-forward tensions (named for VERIFY to address)

- **Cron vs event-driven invocation of pilot cycles.** Tickle v2 is pull-driven (agent signals availability); but does the pilot signal on its own (recursive — what triggers the first signal?) or via operator-bell or via conductor-watchdog-stalled-detect? VERIFY should spike both shapes.
- **Pilot-self-modification scope.** Can the pilot modify its own envelope or shapes file? Pilot-I3 says single-inhabitant-per-envelope; if pilot modifies its own envelope mid-cycle, the cardinality assertion may be temporarily violated. VERIFY should clarify.
- **Hand-off across pilot sessions.** When a pilot cycle ends (or a session terminates), what state must transfer to the next pilot? Claims-ledger entries? Open consent-gate events? Provisional anchor-flips not yet committed? VERIFY specifies.
- **Bell payload schema standardisation.** The substrate-creation thread from anchor 0010 (standardising bell payload schemas across all hand-off surfaces) becomes relevant here. VERIFY should pick a schema; INSTANTIATE encodes it.

### Exit criterion check (for DERIVE → ARGUE transition)

Per mission-lifecycle.md: *"Someone could implement the mission from the DERIVE section alone, without needing to ask clarifying questions."* The 5 design choices (conductor, clone-from, transports, first-pilot, writer-action-class) are made with IF/HOWEVER/THEN/BECAUSE rationale; entity types, relation types, invariant rules, data flow, view/UI specs, fidelity contract, and 4 PSRs are drafted.

**PASSED with caveat** — Joe (emacs-repl 2026-05-24) approved DERIVE as "a reasonable first set of working assumptions" but flagged: *"We might want to change them a bit as we go in ARGUE."* DERIVE is committed but explicitly revisable in light of ARGUE.

## 4. ARGUE (2026-05-24, locked by operator)

### Plain-language argument (operator-locked)

**What is the War Machine?**

The War Machine is an implementation of Active Inference applied to the futon
stack. It carries beliefs about the stack, observes what's happening, and
computes the gap between observation and preference as expected free energy
(`aif/predictive-coding-belief-update`, `aif/expected-free-energy-scorecard`).
The output is an inferred next-move that would reduce surprise.

What it doesn't do is take the move. By load-bearing design
(`gauntlet/aif-as-environment-not-instruction`), the engine embeds its state
in the operator's sensory surface — the dashboard — rather than commanding
the operator. The operator perceives the free energy and acts. This preserves
operator sovereignty and dodges the compliance-capture failure mode the
pattern explicitly names.

**Why should it have a pilot?**

The engine observes and infers; the operator is the only one who acts on
what's inferred. The pilot is the next step of the Baldwin cycle
(`futon-theory/baldwin-cycle`) applied to the engine itself: what was
operator-mediated runtime adaptation — read dashboard, decide, act — becomes,
for cases the operator has authorised, pilot-mediated capability. Explore
(engine infers) → assimilate (pilot takes the inferred action where
confident) → canalise (those actions become routine).

Three pieces of discipline make this safe. **Agent contract**
(`futon-theory/agent-contract`) — pilot clocks in, observes before acting,
attributes changes, respects invariants. **State-as-hypothesis**
(`agent/state-is-hypothesis`) — pilot's beliefs are revisable; surprises
trigger updates rather than defence. **Consent-gated substantive change** —
pilot acts within tools the operator grants, and anything load-bearing routes
through operator approval before it happens.

The wager: assimilate-without-instructing. The pilot takes recommended
actions; the engine stays an environment, not a commander; the operator stays
in charge of what counts as authorised.

### Pattern cross-reference (the 6 grounding patterns)

| Role in the argument | Pattern | Library path |
|---|---|---|
| WHAT the engine IS (belief update) | predictive-coding-belief-update | `futon3/library/aif/predictive-coding-belief-update.flexiarg` |
| WHAT the engine IS (action selection) | expected-free-energy-scorecard | `futon3/library/aif/expected-free-energy-scorecard.flexiarg` |
| WHY the engine doesn't act (load-bearing) | aif-as-environment-not-instruction | `futon3/library/gauntlet/aif-as-environment-not-instruction.flexiarg` |
| WHY a pilot is the structurally-correct next step | baldwin-cycle | `futon3/library/futon-theory/baldwin-cycle.flexiarg` |
| Discipline 1: clocking-in / attribute changes | agent-contract | `futon3/library/futon-theory/agent-contract.flexiarg` |
| Discipline 2: revisable beliefs | state-is-hypothesis | `futon3/library/agent/state-is-hypothesis.flexiarg` |

These six are the conceptual scaffolding (WHY this is the right thing to
build). The four PSRs in §DERIVE (`agent-decorator-pattern`,
`agent-command-pattern`, `intent-handshake-is-binding`,
`provisional-claims-ledger`) are the implementation primitives (HOW to build
it). Two distinct roles; both load-bearing.

### Theoretical coherence with IDENTIFY's anchoring

IDENTIFY named AIF, peripheral pattern, consent-gate / WM-I4 clarification,
conductor, bells/whistles/walkie-talkie, foreword v2 annotation
`hx:of:v2:cover-self-guiding-vs-r-criteria`, the wm-ui-anchors registry, and
Joe's point-de-capiton framing.

ARGUE re-grounds the same anchoring in the standard library:
- AIF anchoring → `aif/predictive-coding-belief-update` +
  `aif/expected-free-energy-scorecard` (the engine IS these)
- Consent-gate / WM-I4 → `gauntlet/aif-as-environment-not-instruction`
  (engine-as-environment) + `agent/state-is-hypothesis` (pilot's beliefs
  remain revisable)
- Self-guiding claim → `futon-theory/baldwin-cycle` (the named structural
  pattern that says "what was learned becomes assimilated") + foreword v2
  annotation
- Operator sovereignty → `futon-theory/agent-contract` (the contract makes
  it explicit; the consent-gate within the envelope is the contract's
  invariant-preservation mechanism)
- Point de capiton → the argument is itself the *retroactive stabilisation*
  Joe named: looking back from the pilot, the AIF apparatus + dashboard +
  the 22 anchors + the foreword's "tantalisingly-close" passage all resolve
  into "what was needed for the pilot to anchor in." Looking forward, the
  ingredients now have a *running agent* as their shared operational locus.

### Trade-off summary

What we give up to land the pilot:

- **A pure-observer engine.** Strictly the engine remains pure-observer
  (WM-I1), but the *system* now has actors inside it (the pilot
  inhabitants). Operators reading the system at a glance might collapse the
  distinction; ARGUE's load-bearing rhetorical work is keeping that
  distinction sharp.
- **Some operator throughput on small actions.** Decisions about which
  actions are "small enough for pilot autonomy" become a substrate-side
  matter (envelope tool-grants) rather than per-action operator judgement.
  In return, the operator has more throughput on substantive choices.
- **A separation between "the engine inferred" and "the world changed
  because of that inference."** Today, that separation is total (engine
  infers; operator's action is the only causal link). With a pilot, the
  inference can sometimes produce world-change directly. Auditing requires
  more discipline (the trace + claims-ledger pattern is what we ride for
  this).

What we keep:

- WM-I4 in its pilot-clarified form (engine doesn't act; pilot acts within
  envelope; operator sovereign over what's authorised).
- The dashboard's role as the operator's sensory surface (the pilot reads
  the same dashboard; the operator doesn't lose visibility).
- The Baldwin cycle's full shape (explore → assimilate → canalise rather
  than just explore).

### Generalization notes

The argument generalises to any case where:

- An AIF-shaped engine observes a domain and infers preferred actions
- The domain has a sovereignty constraint (some actor — not the engine —
  must be load-bearing for substantive change)
- The acting-on-inference work has variable cost; small inferences are
  cheap-to-act-on, substantive ones expensive

For these cases, the **pilot-as-agent-inhabiting-engine-peripheral** pattern
applies. Candidate generalisation sites:

- The mission peripheral could have a pilot (auto-discharge of small
  obligations within a mission)
- The forum could have a pilot (auto-respond to certain message classes;
  consent-gated for substantive ones)
- The Arxana surface could have a pilot (auto-author certain annotation
  shapes; consent-gated for prose changes)

These are NOT this mission's scope — but they're the family of which this
mission is one instance. Future work; ARGUE notes the family explicitly to
avoid future agents re-deriving the framing from scratch.

### Notes on potential DERIVE revision (per Joe's caveat)

DERIVE was drafted before ARGUE locked in. Two places where ARGUE's framing
might push back on DERIVE:

1. **PSR set may want restructuring.** DERIVE's 4 PSRs were the
   implementation primitives surfaced by the (low-score) futon3a search.
   ARGUE adds 6 conceptual patterns as the WHY-grounding. The PSR
   discipline could be re-shaped as: 6 conceptual PSRs (the WHY) + 4
   implementation PSRs (the HOW), with explicit hierarchy. Could also
   simply add the 6 to the existing 4 as a second PSR cluster. To be
   resolved in VERIFY.
2. **The "writer-action-class direct write within envelope grants" choice
   (DERIVE §Choice 5) leans on the envelope-as-decorator-stack PSR.** But
   ARGUE's consent-gated-substantive-change discipline + agent-contract
   pattern collectively mean the operator approval step is more
   fundamental than "what the envelope grants" — the envelope is the
   contract's tool-set, but the consent-gate is its load-bearing event.
   May want re-stated in terms of agent-contract semantics rather than
   decorator semantics.

These are revisable; they're flagged for VERIFY to address before any code
hardens.

### Exit criterion check (for ARGUE → VERIFY transition)

Per mission-lifecycle.md: *"The design feels inevitable given the
constraints, not merely possible. And someone outside the project can
understand what it does and why from the plain-language argument alone."*

**PASSED** — Joe locked the plain-language argument (emacs-repl 2026-05-24,
"this is good. ... let's lock this in as the argument"). The technical
synthesis (6 pattern cross-references + theoretical coherence with IDENTIFY
+ trade-off summary + generalization notes) accretes onto the argument
without diluting it. Ready for VERIFY.

## 5. VERIFY (2026-05-24, in progress — Joe directive: "keeping the DERIVE open, so we iterate on DERIVE as we VERIFY that we can actually build what it asks for")

**Framing.** Per mission-lifecycle.md, VERIFY is "primarily about structural and empirical validation — confirming the design is sound before code hardens." Joe explicitly held DERIVE open: this VERIFY can revise DERIVE in flight rather than treating DERIVE as frozen. Decision log (below) tracks every revision triggered.

### Structural verification (against war-machine-exotype-v2.edn)

The wiring diagram exists. Auditing v2 against the 8 AIF+ exotype checks:

| Check | Result | Witness |
|---|---|---|
| **Completeness** (every input has ≥1 consumer) | PASS | 16/16 inputs wired to ≥1 component |
| **Output coverage** (every output has a producer) | PASS | 10/10 outputs wired from a component |
| **No orphan inputs** | PASS | every input cited in the `:wiring` block |
| **Type safety** (accepts/produces match across edges) | PASS-WITH-CAVEAT | accepts/produces field-level alignment audited for the new pilot cluster; older v1 wiring assumed legacy types continue to align (claude-2 / claude-4 work) |
| **Spec coverage** (every component has a `:ref`) | PASS | 15/15 components have :ref pointing at code OR an explicit "TBD" marker for `:C-pilot-envelope` and `:C-pilot-inhabitant` (DERIVE-stage, not-yet-built — correct for VERIFY) |
| **Timescale ordering** (fast → slow → glacial; pilot writes don't violate constraint inputs' :constraint flag) | PASS-WITH-NEW-INVARIANT-NOTE | wm-ui-anchors.edn and :coherence-evidence are NOT marked `:constraint true` in v2 — pilot writes to them are within-cycle outputs, not violations. The Baldwin-cycle-extension-to-pilot pattern (pilot writes → slow-cycle reads → next pilot cycle observes new state) is the structural analogue of v1's fast→slow Baldwin loop. |
| **Exogeneity** (constraint inputs not written by any output) | PASS | The 6 constraint-true inputs (:I-patterns, :I-sorry-topology, :I-logic-model, :I-holistic-argument, :I-bilateral-evidence, :I-foreword, :I-sorry-registry) are NOT in any output's `:consumer`. Pilot's writes target only non-constraint substrate. |
| **Compositional closure** (every wiring path terminates at an output) | PASS | Fast cycle terminates at :O-notification / :O-context-evidence / :O-context-hud; slow cycle terminates at :O-visualisation / :O-markdown / :O-hud-oneliner; pilot cycle terminates at :O-anchor-write / :O-coherence-row / :O-handoff-bell / :O-pilot-action. Feedback paths (:O-context-evidence → :I-evidence; pilot writes → wm-ui-anchors as input) are Baldwin loops, NOT cycle-breaking. |

**Surprise during VERIFY (DR-V1 below):** the "Baldwin loop applied to pilot
cycle" pattern is operating structurally without being NAMED in v2's :thesis
or :invariants. The fast → slow Baldwin loop is named explicitly; the pilot
→ slow Baldwin loop is operating but not named. Worth lifting into v2 as a
named structural property — adds clarity, doesn't change behaviour.

### Prototype / spike — EXECUTED 2026-05-24 (Joe directive: "Spike now")

**Files authored:**
- `~/code/futon3c/src/futon3c/peripheral/war_machine_pilot_shapes.clj` — minimal CycleDomainConfig field declarations (phase-order, phase-tools, setup-tools, pilot-tool-operation-kinds, phase-required-outputs)
- `~/code/futon3c/src/futon3c/peripheral/war_machine_pilot.clj` — `pilot-domain-config` + `make-pilot` factory + `spike-check` validation entrypoint
- `~/code/futon3c/resources/peripherals.edn` — `:war-machine-pilot` peripheral spec registration (NEW; revealed as a substrate-step DERIVE didn't anticipate — see DR-V5 below)

**Execution path (via Drawbridge nREPL, no JVM restart):**
1. `(require '[futon3c.peripheral.war-machine-pilot :as wmp])` — loaded clean
2. `(wmp/spike-check)` — first run: `{:valid-config? true :make-pilot-ok? false}` because `make-cycle-peripheral` calls `common/load-spec :war-machine-pilot` which threw "Peripheral spec not found"
3. Added `:war-machine-pilot` entry to `resources/peripherals.edn`; reset `common/specs` delay via `alter-var-root`
4. `(wmp/spike-check)` — second run: `{:valid-config? true :make-pilot-ok? true :spike-error nil}` PASS

**What the spike validated:**
- C1 — envelope file exists; `cycle/valid-domain-config?` returns true for `pilot-domain-config`
- `make-pilot` returns a `CyclePeripheral` record without runtime error
- cycle.clj engine accepts the war-machine-pilot domain config (Choice 2 from DERIVE — clone-from-Mission shape is sound)

**What the spike did NOT validate (deferred to INSTANTIATE):**
- C2 — Agency registration + inhabitation bell
- C3 — Playwright reach via `:playwright-run` (tool not yet declared in shapes)
- C4 — CLJ backend reach via `:drawbridge-eval` (tool not yet declared)
- C5 — substrate-read works in practice (`:anchors-read` backend not yet wired)
- C6 — pilot write-back (`:anchor-flip`, `:coherence-row-author`, `:pilot-action` tools not yet declared)
- C7 — WM-I4 preservation at runtime (no write tools yet, so trivially preserved at spike-stage)
- C8 — consent-gate exercised (Pilot-I1 machinery not yet wired)

**Spike duration:** ~30 minutes for authoring + ~5 minutes for diagnosis-of-peripherals.edn-gap + retest. Net validates risk on DERIVE Choice 2 (clone-from-Mission shape works).

### Completion criteria pre-check (C1–C8 from IDENTIFY)

Each C-criterion checked against DERIVE's coverage:

| C | What it tests | DERIVE coverage | Status |
|---|---|---|---|
| C1 | Envelope file exists | DERIVE §Choice 2 specifies clone-from-Mission shape + 3 files (envelope + shapes + backend); §C-pilot-envelope component named | DESIGN-ADDRESSED |
| C2 | Agent inhabits envelope + emits bell | DERIVE §Choice 4 + Agency registration in MAP §G | DESIGN-ADDRESSED |
| C3 | Playwright reach | DERIVE §Choice 4 transport-list includes `:playwright-run` in tool-grants | DESIGN-ADDRESSED |
| C4 | CLJ backend reach | DERIVE entity-types include `:drawbridge-eval` + `:wm-api-query` tools | DESIGN-ADDRESSED |
| C5 | Substrate reach (anchors + vocab) | DERIVE data-flow wires :I-wm-ui-anchors + 7 other substrate inputs to pilot-inhabitant | DESIGN-ADDRESSED |
| C6 | First UI improvement via consent-gate | DERIVE includes :anchor-flip + :pilot-action; Pilot-I1 binds substantive actions to consent-gate-events | DESIGN-ADDRESSED — pending DR-V2 |
| C7 | WM-I4 preservation | DERIVE inherits WM-I4 in pilot-clarified form; ARGUE further grounds via agent-contract | DESIGN-ADDRESSED |
| C8 | Consent-gate exercised | DERIVE specifies via intent-handshake at cycle start; Pilot-I1 binds per substantive action | DESIGN-ADDRESSED — pending DR-V2 |

All 8 C-criteria are DESIGN-ADDRESSED. Two carry pending revisions (DR-V2 below).

### Fidelity check (preserve set tripwires)

Per DERIVE's preserve set, here are tripwire tests proposed for the envelope's test suite (to be authored in INSTANTIATE):

| Preserve invariant | Tripwire test |
|---|---|
| WM-I1 (engine-level read-only) | Envelope's tool-set does NOT grant write access to WM observer engine paths (`futon0/scripts/futon0/report/war_machine.clj`, `futon2/src/futon2/aif/*`). Test: assert these paths are NOT in any `:phase-allowed-tools` write-action-class. |
| WM-I4 (sovereignty, pilot-clarified) | Any Command with `:capability-tags :substantive` and no preceding `:consent-gate-event-id` in the same cycle MUST fail. Test: synthesise such a Command, assert error. |
| WM-I5 (anchor-coherence) | Pilot cannot delete an addressed anchor without re-opening (status :addressed → :addressed). Test: attempt deletion-of-addressed; assert refusal. |
| Anchor-file schema | Pilot writes to wm-ui-anchors.edn produce EDN that round-trips through `clojure.edn/read-string`. Test: write + read, assert equal. |
| Coherence-row schema | Pilot writes to :coherence-evidence carry `:sub-kind` in the closed-set (7 values as of 2026-05-24). Test: schema-validate each new row. |

All five tripwires are specifiable from DERIVE; none requires DERIVE revision. Ready for INSTANTIATE to write.

### Decision log (DERIVE revisions surfaced during VERIFY)

Per Joe's framing ("iterate on DERIVE as we VERIFY"), the following revisions are flagged:

**DR-V1 — name the pilot→slow Baldwin loop explicitly in v2 wiring diagram. RESOLVED 2026-05-24 — LANDED.**

- *Finding:* the structural verification noted the pilot's writes-to-anchor flowing back into the slow cycle (as future-cycle-reads-this-anchor-state) is operating as a Baldwin loop — same shape as v1's fast→slow Baldwin loop, applied to the pilot cycle. Not currently named in `war-machine-exotype-v2.edn :thesis` or `:invariants`.
- *Proposed revision:* add a named Baldwin-loop note to v2's :thesis and add a property to the wiring block annotating `pilot → wm-ui-anchors → next-cycle-pilot-read` as `:baldwin-loop-pilot-extension`. Doesn't change behaviour, sharpens auditability.
- *Disposition:* Joe approved 2026-05-24. Landed: (a) :thesis amended with explicit "PILOT→SLOW Baldwin loop, NEW in v2 and now explicitly named" block; (b) two new wiring edges added with `:baldwin-loop :pilot-extension` annotation (`:O-anchor-write → :I-wm-ui-anchors`, `:O-coherence-row → :I-coherence-evidence`).

**DR-V2 — DERIVE Choice 5 + Pilot-I1 may want refinement at per-action granularity. RESOLVED 2026-05-24 — LANDED.**

- *Finding:* DERIVE §Choice 5 ("writer-action-class direct write within envelope grants") and Pilot-I1 ("consent-gated-substantive-action") are not in tension, but their relationship is ambiguous: Choice 5 says "direct write when envelope-granted"; Pilot-I1 says "substantive actions require consent-gate citation." The synthesis is: envelope grants the tool; substantive use of the tool still requires a consent-gate event. This is sound but currently under-specified.
- *Proposed revision:* tighten Pilot-I1's wording to: "every Command with `:capability-tags :substantive` MUST cite a preceding `:consent-gate-event-id` in the same cycle, EVEN IF the underlying tool is envelope-granted." Add to DERIVE Choice 5: "envelope-granted tools fall into two operational classes — `:non-substantive` (no per-action consent) and `:substantive` (Pilot-I1 applies)." The closed set of substantive tools must be declared in the envelope's shapes file.
- *Disposition:* Joe approved 2026-05-24. Landed in DERIVE: Pilot-I1's wording updated with the "EVEN IF envelope-granted" clause + closed-set declaration; Choice 5's IF/HOWEVER/THEN/BECAUSE rewritten with the two operational classes (`:substantive` vs `:non-substantive`) made structural.

**DR-V6 (NEW, surfaced by operator concern 2026-05-24) — Tickle v2 deferred to v1+; v0 operates without a conductor.**

- *Finding:* Joe (verbatim, emacs-repl 2026-05-24): "My only outstanding concern is about the use of Tickle v2 — it's possible that will work, and if your verification says so, OK, but so far I have found Tickle very annoying (maybe partly that is by design). Let's assume that adding Tickle is the last and optional step in the build out — which is probably true."
- *Proposed revision:* DERIVE Choice 1 (Tickle v2 as v0 conductor) is replaced with "no conductor for v0; v0 operates via direct bell-on-demand invocation (the same shape the spike validates). Tickle v2 stays a v1+ candidate; APM v1/v2/v3 and Tickle v1 also remain candidates." The 8 v0 success-criteria (C1–C8 from IDENTIFY) are achievable without a conductor — pilot runs one cycle per bell-invocation, emits fruit, exits. Continuous operation is the v1+ value.
- *Disposition:* Joe approved 2026-05-24. Landed in DERIVE Choice 1: full rewrite removing Tickle commitment + adding the deferred-to-v1 framing + citing Joe's verbatim concern. Spike's shape (one-shot invocation) IS the v0 operational shape; no further substrate change needed.

**DR-V3 — PSR-set restructuring (per ARGUE's caveat).**

- *Finding:* ARGUE noted DERIVE's 4 PSRs are implementation-primitives; ARGUE adds 6 conceptual patterns as WHY-grounding. Currently PSRs live in DERIVE only; the 6 conceptual patterns live in ARGUE only. The PSR/PUR discipline says PSRs precede commitment — both clusters of patterns inform commitment, so both should be PSR-shaped.
- *Proposed revision:* re-shape the PSR set as `PSR-CONCEPTUAL-1..6` (the 6 ARGUE patterns) + `PSR-IMPLEMENTATION-1..4` (the 4 DERIVE patterns), with explicit hierarchy: conceptual PSRs ground the WHY (what the pilot IS in the AIF/Baldwin sense); implementation PSRs ground the HOW (envelope shape, command shape, intent-handshake, claims-ledger).
- *Disposition:* defer to operator review; could land in DERIVE-as-revised OR be promoted to standalone PSR files at `~/code/futon3c/holes/labs/M-war-machine-pilot/psr/` per the lifecycle convention.

**DR-V4 — spike scope decision (do-or-defer). RESOLVED 2026-05-24 — DID THE SPIKE.**

- *Finding:* The proposed minimal spike (~30-60 minutes) would validate envelope file shape, cycle.clj acceptance, Agency registration, and substrate read (C1, C2, C5). Choice 4 (claude-9 re-invoked) is the riskiest DERIVE commitment uncovered without a spike. Larger spikes (C3 Playwright, C4 backend, C6 write-back, C7/C8 consent-gate) belong to INSTANTIATE.
- *Disposition:* Joe greenlit "spike now" (emacs-repl 2026-05-24). Spike executed; spike-check returned `{:valid-config? true :make-pilot-ok? true}`. C1 + cycle.clj-acceptance validated; C2–C8 explicitly deferred to INSTANTIATE per :deferred-criteria field in spike-check return value. See "Prototype / spike — EXECUTED" above.

**DR-V5 (NEW, surfaced during spike) — peripherals.edn registration is a substrate-step DERIVE didn't anticipate.**

- *Finding:* `cycle/make-cycle-peripheral` calls `futon3c.peripheral.common/load-spec` with the domain-id, which reads from `resources/peripherals.edn`. The peripheral spec MUST be registered there separately from the domain-config file. DERIVE specified "3 files (envelope + shapes + backend)" — actually it's 4 (those 3 + peripherals.edn entry).
- *Proposed revision:* update DERIVE Choice 2 ("clone-from Mission peripheral as DERIVE shape donor") to enumerate 4 files: envelope + shapes + backend + peripherals.edn entry. Note that the spec-registration step is a DELAY-cached read (uses `defonce ^:private specs (delay ...)`); operationally, adding a new peripheral spec requires resetting that delay (via `alter-var-root #'futon3c.peripheral.common/specs ...`) on a running JVM, OR a planned-restart on a fresh-start JVM. Drawbridge-mediated reset is the operationally-correct path per the never-restart-JVM discipline.
- *Disposition:* fold into DERIVE now (textual change; doesn't affect VERIFY exit-criteria). Also worth noting in the wiring diagram v2 as part of the `:C-pilot-envelope` component's spec.

### Carried-forward tensions from DERIVE (rechecked during VERIFY)

The four tensions DERIVE flagged for VERIFY:

1. **Cron vs event-driven invocation of pilot cycles.** *VERIFY observation, REVISED 2026-05-24:* with DR-V6 deferring the conductor entirely, this tension dissolves at v0 — pilot is invoked by direct bell-on-demand. v1+ may pick a conductor; the cron-vs-event question lives with that v1 choice. *Disposition:* close as resolved at v0; v1+ inherits the open question.
2. **Pilot-self-modification scope.** *VERIFY observation:* Pilot-I3 (single-inhabitant-per-envelope) constrains cardinality at any moment, not over time; pilot modifying its own envelope mid-cycle would temporarily violate Pilot-I3 only if the modification creates a SECOND envelope instance. Self-modification of an existing envelope (changing its tool-grant) is permitted under Pilot-I3 IF the modification routes through a consent-gate (so Pilot-I1 applies). *Disposition:* close as resolved with caveat — substantive envelope changes are themselves substantive actions per Pilot-I1.
3. **Hand-off across pilot sessions.** *VERIFY observation:* DERIVE's pilot-trace + claims-ledger + cycle-trace records constitute the durable hand-off state. The cycle.clj `:fruit-fn` + `:exit-context-fn` provide the structured hand-off mechanism the mission peripheral already uses. *Disposition:* close as resolved; clone-from-Mission inherits this mechanism.
4. **Bell payload schema standardisation.** *VERIFY observation:* anchor 0010's hand-off bell payload IS the current schema; pilot bells would inherit. Standardisation across surfaces is a substrate-creation thread (still parked for Joe). *Disposition:* keep open as substrate-creation thread; not blocking pilot mission.

### Exit criterion check (for VERIFY → INSTANTIATE transition)

Per mission-lifecycle.md: *"The design has been checked against available structural constraints. Any risks that can't be verified statically have been spiked. DERIVE revisions (if any) are recorded."*

- Structural verification against v2 wiring diagram: PASS
- Completion-criteria pre-check (C1–C8): ALL DESIGN-ADDRESSED
- Fidelity check (preserve set tripwires): 5 tripwires specifiable
- 4 carried-forward tensions: 3 resolved + 1 kept-open-as-substrate-thread
- 4 DERIVE revisions flagged: 1 (DR-V2) folds into DERIVE; 1 (DR-V1) adds to wiring diagram; 1 (DR-V3) defers to operator review; 1 (DR-V4) spike-or-defer decision

**All VERIFY-stage DRs resolved 2026-05-24:**
- DR-V1 (Baldwin loop named in v2 wiring) — LANDED in war-machine-exotype-v2.edn
- DR-V2 (Pilot-I1 + Choice 5 tightened) — LANDED in DERIVE
- DR-V3 (PSR-restructure) — DEFERRED per Joe (no PSR standard yet)
- DR-V4 (spike scope) — RESOLVED; spike executed and passed
- DR-V5 (peripherals.edn registration as 4th file) — folded into DERIVE Choice 2
- DR-V6 (Tickle deferred to v1+ optional last step) — LANDED in DERIVE Choice 1

**VERIFY → INSTANTIATE: PASSED.** All structural and operator-side constraints addressed; spike proves the foundational shape works; conductor friction explicitly deferred. Ready for INSTANTIATE whenever operator greenlights — INSTANTIATE's first move is fleshing out the spike's envelope to a v0 implementation that meets C2–C8.

## 6. INSTANTIATE — PLAN (2026-05-24, claude-10 drafting per Joe directive: "Possibly some new patterns in ~/code/futon3/library/peripherals/ can be useful to structure aspects of the buildout — maybe we can plan phases of INSTANTIATE accordingly")

**Status:** PLAN authored; build not started. INSTANTIATE proper begins when operator greenlights phase 1.

**Pattern-grounded discipline.** `~/code/futon3/library/peripherals/` carries 11 patterns specifically about peripheral build-out. Five of them form a natural phase-spine for INSTANTIATE; six others apply transversely (operational discipline that holds across phases). The plan below cites each pattern at its load-bearing application point.

### Pre-INSTANTIATE discipline (already satisfied by VERIFY's spike)

| Pattern | Library path | Satisfied by |
|---|---|---|
| `peripherals/read-existing-seam-before-implementing` | `futon3/library/peripherals/read-existing-seam-before-implementing.flexiarg` | MAP §C identified existing seams (cycle.clj, mission.clj clone-from, peripherals.edn registration). VERIFY §spike confirmed all seams threaded without forking. |
| `peripherals/pattern-becomes-peripheral` | same dir | The mission's whole framing IS this pattern: the agent-inhabits-peripheral pattern (which the futon stack already realises in mission/proof/dispatch/PSR-PUR-mesh peripherals) becomes the WM-pilot peripheral. |

### Phase 1 — READ-ONLY v0 (anchored in `peripherals/read-only-first-then-extend`)

> *"A new peripheral's first instantiation should be read-only — observing, displaying, not writing. Write capability is added in a separate, later instantiation once observation has revealed what writes actually need to look like."*

**Already partly done by VERIFY's spike** (spike-stage is the seed of phase 1; what remains is wiring it up to actually run end-to-end).

Deliverables for Phase 1:
- Wire `:anchors-read` tool to a working backend (extends spike's `MockBackend` with a real reader of `wm-ui-anchors.edn`)
- Wire Agency registration (`POST /api/alpha/agents/auto` with `:war-machine-pilot` session-id encoding)
- Pilot can be invoked via bell-on-demand (DR-V6's v0 shape — no conductor)
- Pilot reads substrate (anchors + vocab + sorries + foreword), composes a summary, emits `:fruit` via the cycle.clj engine, exits cleanly
- Tripwire tests for WM-I1 (no write tools at this phase; envelope can't physically write)

Exit-criteria unlocked: **C2** (agent inhabits + emits bell), **C5** (substrate read in practice).

Transverse patterns at Phase 1:
- `peripherals/hot-reload-as-default-fix-path` — Drawbridge nREPL for code changes during build (already used by the spike).
- `peripherals/constrained-execution-envelope` — Phase 1's envelope is the space of *only-observation-tools*; design as space-of-valid-actions, not as space-with-forbidden actions.

### Phase 2 — TRANSPORT WIRING (anchored in `peripherals/split-transport-from-embodiment` + `peripherals/canonical-typed-event-vs-side-channel`)

> *"A peripheral has two distinct concerns — how it moves bytes (transport) and how it manifests as presence (embodiment). Conflating them produces peripherals that cannot be re-transported, re-embodied, or composed."*

> *"When a peripheral needs to emit or consume something the canonical event taxonomy does not yet describe, extend the taxonomy with a first-class typed event. Do not invent a side channel that bypasses the taxonomy."*

Deliverables for Phase 2:
- Three transports plumbed cleanly (per DERIVE Choice 3): **bell** for hand-off-to-other-agents (anchor 0010 shape), **walkie-talkie** for substrate evidence (`POST /api/alpha/evidence/psr` etc.), **whistle** for sync consent-gate queries to operator
- Each transport's payload schema goes through the canonical event taxonomy — NOT a side channel
- Where the existing taxonomy doesn't cover a pilot-specific event, the taxonomy is EXTENDED (typed event added; not ad-hoc fields)
- Operator-bell shape for "consent-gate request" defined as a typed event (NOT a free-form text bell)
- Bell payload schema standardised across pilot hand-off surfaces (closes one of DERIVE's carried-forward tensions)

Exit-criteria unlocked: **C3** (Playwright reach via `:playwright-run` tool — uses `:bash-readonly` + transport plumbing), **C4** (CLJ backend reach via `:drawbridge-eval` tool + transport plumbing).

### Phase 3 — WRITE CAPABILITY (read-only-first-then-extend's *extend* step)

> *"Write capability is added in a separate, later instantiation once observation has revealed what writes actually need to look like."*

Deliverables for Phase 3:
- Substantive tools land: `:anchor-flip`, `:coherence-row-author`, `:pilot-action`. Each carries `:capability-tags :substantive` (per DR-V2 / Choice 5 / Pilot-I1)
- Consent-gate machinery: intent-handshake at cycle start (per PSR-3 / `agent/intent-handshake-is-binding`); per-substantive-action `:consent-gate-event-id` citation enforced (Pilot-I1's check)
- Per-action progress heartbeats (per `peripherals/progress-heartbeat-distinct-from-cycle-completion`) — distinct from cycle-completion, so that long-running substantive actions don't trip false silence-watchdogs and that wedged cycles don't escape under nominal-duration silence
- Tripwire tests for Pilot-I1, Pilot-I2, Pilot-I3 (all three pilot invariants)
- The 5 fidelity tripwires from VERIFY §Fidelity check authored as test suite

Exit-criteria unlocked: **C6** (first UI improvement landed by pilot — via `:anchor-flip` or `:pilot-action`), **C7** (WM-I4 preservation under writes — tripwire-verified), **C8** (consent-gate exercised — at least one consent-gate event in the cycle trace).

Transverse pattern at Phase 3:
- `peripherals/canonical-typed-event-vs-side-channel` reapplied — consent-gate events are first-class typed events in the trace, not stringly-typed bell-payload markers

### Phase 4 — EARN INHABITATION (anchored in `peripherals/surface-earns-inhabitation` + `peripherals/inhabitation-feeds-evolution`)

> *"A productive interface that nobody uses is dead infrastructure; the surface must earn inhabitation by being less friction than the alternative."*

> *"A peripheral is not just a constrained action space — it is a representational space that enables a learning loop. Inhabiting the peripheral generates the data that evolves the peripheral. If the peripheral is not inhabited, the loop is dead and the peripheral cannot improve."*

Deliverables for Phase 4:
- **Friction audit:** measure operator's time-to-completion for a typical anchor-discharge with pilot vs by-hand (per anchor 0001 / 0009 shape from this session's QA). If pilot is MORE friction, the surface is dead infrastructure; resolve before declaring v0 done.
- **Inhabitation-evolution loop closure:** the pilot's cycles emit data (claims-ledger entries, anchor-flip rationales, consent-gate event payloads) that the operator (or claude-10 substrate-side) reads to refine the envelope's tool-set and policies. The Baldwin loop from ARGUE running.
- v0 acceptance bell from operator: "the pilot is less friction than doing the same thing by hand"

Exit-criteria unlocked: v0 mission-complete (all C1–C8 satisfied + earn-inhabitation criterion held).

### Phase 5 (OPTIONAL, post-v0) — CONDUCTOR (per DR-V6)

Tickle v2 candidate (or APM v1/v2/v3, or Tickle v1, or alternative). Per Joe's verbatim (2026-05-24): *"adding Tickle is the last and optional step in the build out."* Adopted **only if** Phase 4's inhabitation-evolution loop reveals continuous-operation value that one-shot bell-invocation doesn't deliver. Until that signal is present, the pilot operates by bell-on-demand.

### Phasing rationale (IF/HOWEVER/THEN/BECAUSE)

- **IF** we ship a fully-armed pilot (write + transport + consent + conductor) in one INSTANTIATE pass,
- **HOWEVER** the spike's diagnosis-of-peripherals.edn-gap (DR-V5) showed that one-pass build-out hides substrate gaps until they bite; AND `read-only-first-then-extend` names "observation reveals what writes need to look like" as the load-bearing reason for phasing,
- **THEN** phase the build: read-only (1) → transport (2) → write (3) → earn-inhabitation audit (4) → optional conductor (5),
- **BECAUSE** each phase's exit criteria are checkable in isolation; phase N+1 only begins when phase N's tripwires pass; Phase 5 is *gated on Phase 4's inhabitation signal*, NOT on calendar time. This matches `read-only-first-then-extend`'s rationale + Joe's DR-V6 framing exactly.

### Cross-references

- `peripherals/read-only-first-then-extend` → load-bearing for Phase 1 + 3 split
- `peripherals/constrained-execution-envelope` → load-bearing for envelope's design across all phases
- `peripherals/split-transport-from-embodiment` → load-bearing for Phase 2
- `peripherals/canonical-typed-event-vs-side-channel` → load-bearing for Phase 2 + 3
- `peripherals/progress-heartbeat-distinct-from-cycle-completion` → load-bearing for Phase 3 + 5
- `peripherals/surface-earns-inhabitation` → load-bearing for Phase 4 (the gate to v0-mission-complete)
- `peripherals/inhabitation-feeds-evolution` → load-bearing for Phase 4 + 5 (Baldwin loop closing; conductor gated on this)
- `peripherals/pattern-becomes-peripheral` → frames the whole mission
- `peripherals/read-existing-seam-before-implementing` → satisfied by MAP + VERIFY
- `peripherals/hot-reload-as-default-fix-path` → transverse operational discipline
- The 4 DERIVE PSRs (decorator / command / intent-handshake / claims-ledger) → still apply; they're the implementation-primitive patterns, while these 11 peripherals patterns are the build-phase patterns.

### Awaiting operator review

Phase plan drafted; build not started. Operator greenlight to begin Phase 1 is the next decision.

### Phase 1 EXECUTED 2026-05-24

Joe greenlit: "OK, I think we can do Phase 1 now" (emacs-repl 2026-05-24).

**Files authored / changed:**
- `~/code/futon3c/src/futon3c/peripheral/war_machine_pilot_backend.clj` — NEW. `PilotBackend` defrecord wrapping an inner ToolBackend. Implements `:anchors-read` against the live `~/code/futon5a/data/wm-ui-anchors.edn` file; delegates all other tools to the inner backend.
- `~/code/futon3c/src/futon3c/peripheral/war_machine_pilot.clj` — UPDATED. `make-pilot` now wraps the inner backend with `pb/make-pilot-backend`. New `run-observe-cycle` function ties together `start → step :cycle-begin → step :anchors-read → step :cycle-advance → stop`.

**Hot-reload + end-to-end test (via Drawbridge nREPL, no JVM restart):**

After fixing one small bug (cycle.clj's action-validation expects `:args` to be sequential `[]`, not a map `{}`), the end-to-end test passed:

```
(run-observe-cycle) →
{:phase :completed, :ok? true,
 :anchors-summary
   {:total 23 :by-status {:addressed 16 :open 7}
    :coherence-row-count 16 :addressed-count 16
    :recent-addressed ["wm-ui-anchor:0009" "wm-ui-anchor:0010"
                       "wm-ui-anchor:0017" "wm-ui-anchor:0018"
                       "wm-ui-anchor:0019"]
    :coherence-sub-kinds {:disclosure-regression-net 1
                          :deprecate-via-toggle-removal 2
                          :cross-repo-data-freshness-check 1
                          :api-as-substrate 1
                          :structured-warning-rendering-net 2
                          :substrate-entity-reference 1}
    ...}
 :fruit {:pilot-inhabited-at ... :anchors-summary ... :cycles-completed 1 :final-phase :completed}}
```

The pilot inhabited the envelope, ran a real `:anchors-read` against the live substrate, advanced through phases, and produced fruit. **Read-only first, just as the pattern says.**

**Exit criteria unlocked by Phase 1:**

- **C1 — Envelope file exists.** Yes; was already from the spike but now genuinely operational with a real backend.
- **C5 — Substrate reach.** Yes; pilot reads anchor counts + sub-kind taxonomy + recent-addressed list directly from disk every call.
- **C2 — Agent inhabits the envelope and emits a bell?** PARTIAL. The peripheral runs end-to-end and the fruit IS the inhabitation evidence. What's NOT yet wired is the *external bell-on-invocation* (i.e., the pilot being invoked by a bell from operator OR emitting a bell on completion). v0's Drawbridge-direct invocation is the minimum-viable bell-on-demand; emitting a completion bell is a small Phase 1.5 extension. Recommend deferring the completion-bell wire-up until Phase 2's transport plumbing (where bells become first-class).

**Tripwire test for WM-I1 (pilot doesn't write):**

The `:phase-allowed-tools` map declares `:observe #{:read :glob :grep :bash-readonly :anchors-read :cycle-advance}` — no write tools. `PilotBackend.execute-tool` has no write-action implementation. WM-I1 holds structurally: even if the pilot wanted to write, it has no envelope-granted tool that would let it. Will be formalised as a unit test in Phase 3 alongside the other 4 fidelity tripwires.

**Phase 1 status:** complete (modulo the completion-bell wire-up deferred to Phase 2).

**Surprise during Phase 1:** the action-validation requires `:args []` not `:args {}`. Small detail; recorded for future agents.

**Ready for Phase 2** when operator greenlights.

### Phase 2 EXECUTED 2026-05-24

Joe greenlit: "Let's go to Phase 2 next" (emacs-repl 2026-05-24).

**Patterns anchoring Phase 2:** `peripherals/split-transport-from-embodiment` + `peripherals/canonical-typed-event-vs-side-channel`.

**Files updated:**
- `war_machine_pilot_shapes.clj` — 6 new tools added to `:pilot-tool-operation-kinds` + `:phase-allowed-tools` + `:setup-tools`
- `war_machine_pilot_backend.clj` — 4 new tool implementations: `bell-emit`, `walkie-emit` (with `:psr/:pur/:par` kinds), `wm-api-query`, `playwright-probe-run` (12-probe whitelist enforced structurally)
- `war_machine_pilot.clj` — `run-observe-cycle` now emits a completion-bell before cycle-advance; bell carries `:pilot-event :pilot/observe-cycle-complete` (typed event, per the canonical-typed-event-vs-side-channel pattern)
- `resources/peripherals.edn` — `:war-machine-pilot` peripheral spec updated with all new tools; specs delay reset via Drawbridge

**End-to-end test (via Drawbridge, no JVM restart):**

```
(run-observe-cycle) →
{:phase :completed, :ok? true,
 :completion-bell {:emitted? true,
                   :job-id "invoke-1779708959599-235-f06d8796",
                   :payload-schema {:agent-id "claude-10",
                                    :prompt "[pilot/observe-cycle complete] anchors=23 addressed=16 ...",
                                    :pilot-event :pilot/observe-cycle-complete}}}
```

**Per-tool standalone tests:**

| Tool | Test | Result |
|---|---|---|
| `:bell-emit` | embedded in `run-observe-cycle` | real bell job-id returned; payload validated |
| `:wm-api-query` | `(wm-api-query nil)` | real WM API response with 10+ keys (`:metabolic-balance`, `:loop-health`, `:judgement`, `:graph`, `:commit-hygiene`, etc.) |
| `:playwright-probe-run` (whitelisted) | `{:probe "wm-anchor-0004-verify.mjs"}` | exit 0; real probe output; verified WM mode shifted to `:stagnant` during session |
| `:playwright-probe-run` (rejection) | `{:probe "not-on-whitelist.mjs"}` | rejected with clear error listing 12 allowed probes — envelope structural constraint working |

**Exit criteria unlocked by Phase 2:**

- **C2 (FULL)** — pilot now emits an inhabitation/completion bell; cycle is bell-observable from outside
- **C3 — Playwright reach.** Via `:playwright-probe-run` with whitelist (12 probes); operationally validated
- **C4 — CLJ backend reach.** Via `:wm-api-query` (HTTP read-only against `/api/alpha/war-machine`); operationally validated. Full Drawbridge eval explicitly deferred to Phase 3 (under consent-gate) — `:drawbridge-eval` not granted in Phase 2.

**Substrate observation surfaced by the Playwright probe** (Phase-2 finding, not blocking):
- Live probe of `wm-anchor-0004-verify.mjs` reports `title-has-rationale? false` despite the title containing the matching rationale (`"stagnant — inhabitation × (1−evolution); surfaces used but not improving..."`). The probe's assertion logic appears incorrect. **claude-9 territory; flagging not addressing.**
- Adjacent observation: WM mode is currently `:stagnant` (shifted from earlier `:multiplied`). The apparatus is genuinely running; the mode-inference is alive.

**Canonical-typed-event-vs-side-channel discipline applied:** every transport tool's payload schema is declared (not side-channeled). `:pilot-event` keyword identifies the typed event on bell payloads. Walkie-talkie kinds (`:psr` / `:pur` / `:par`) route to canonical endpoints (`/api/alpha/evidence/psr` etc.) rather than improvising.

**Phase 2 status:** complete. Bell-on-completion deferred from Phase 1 → landed. Walkie-talkie endpoints (`:walkie-psr/pur/par`) implemented but not exercised end-to-end (real payload shapes can be tested in Phase 3 when substantive actions begin emitting PSRs/PURs).

**Ready for Phase 3 (WRITE CAPABILITY)** when operator greenlights.

### Phase 3 EXECUTED 2026-05-25

Joe greenlit: "Excellent work, let's got to Phase 3" (emacs-repl 2026-05-24 → 2026-05-25 turn boundary).

**Patterns anchoring Phase 3:** read-only-first-then-extend's *extend* step + `agent/intent-handshake-is-binding` (PSR-3) + Pilot-I1 enforcement.  `peripherals/progress-heartbeat-distinct-from-cycle-completion` deferred to Phase 3.5 (not yet needed for the no-op-flip demo; will matter when substantive actions run long).

**Files updated:**
- `war_machine_pilot_shapes.clj` — added 4 substantive tools (`:consent-gate-emit` + `:anchor-flip` + `:coherence-row-author` + `:pilot-action`); `substantive-tools` closed-set declaration (per DR-V2)
- `war_machine_pilot_backend.clj` — added `consent-gate-emit`, `anchor-flip` implementations; `substantive-arg-check` Pilot-I1 enforcer; WM-I5 check on `:addressed → :deleted`
- `resources/peripherals.edn` — pilot's tool-set extended

**Tripwire tests (3 of 5 fidelity tests now operational):**

| Tripwire | Test | Result |
|---|---|---|
| **Pilot-I1 (missing :consent-gate-event-id)** | `(anchor-flip {:anchor-id ... :new-status :addressed})` | `{:ok false :pilot-invariant :Pilot-I1 :error "...requires :consent-gate-event-id in args..."}` |
| **Pilot-I1 (malformed cg-id)** | `(anchor-flip {... :consent-gate-event-id "made-up"})` | `{:ok false :pilot-invariant :Pilot-I1 :error "...not a well-formed pilot consent-gate event id (expected prefix 'cg-')..."}` |
| **WM-I5 (delete :addressed)** | `(anchor-flip {... :new-status :deleted :consent-gate-event-id "cg-fake"})` | `{:ok false :pilot-invariant :WM-I5 :error "...cannot delete an :addressed anchor"}` |
| WM-I1 (engine-level read-only) | not yet authored — TODO: assert envelope's tools never touch futon0/scripts/futon0/report/war_machine.clj | DEFERRED |
| Anchor-schema (round-trip) | partial via the positive test below; formal version TODO | PARTIAL |

**Positive end-to-end test (PASSED):**

```
1. (consent-gate-emit {:intent ... :scope ... :constraints ... :success-criteria ...
                       :target-anchor-id "wm-ui-anchor:0010"})
   → :consent-gate-event-id "cg-ec9c7beb-aea8-45bc-af3a-48b4a0954e35"
   → bell emitted to claude-10 (operator) with :pilot-event :pilot/consent-gate-emit

2. (anchor-flip {:anchor-id "wm-ui-anchor:0010" :new-status :addressed
                 :consent-gate-event-id "cg-ec9c7beb-..."
                 :rationale "Phase 3 v0 substantive-write demo (no-op flip, adds pilot-trail)"
                 :anchors-path "/tmp/wm-ui-anchors-pilot-test.edn"})
   → {:ok true :result {:prior-status :addressed :new-status :addressed
                        :pilot-flip-meta {:flipped-by-pilot true :flipped-at ...
                                          :cited-consent-gate-event-id "cg-ec9c7beb-..."}}}

3. Verify via independent bb-read:
   anchor:0010 in /tmp file now has :pilot-flip-trail [<entry>] with full provenance.
```

**Honest scope (Phase 3 v0 findings):**

- **PHASE-3 FINDING: pprint-based EDN writer strips comments.** The current `anchor-flip` uses `clojure.pprint/pprint` to serialize the file; this is fine for /tmp demo (128k → 111k after strip) but UNACCEPTABLE for the live `wm-ui-anchors.edn` which has substantial comment-encoded provenance.  **The Phase 3 demo deliberately writes to a /tmp copy** to avoid destroying the live substrate. Real live-substrate write requires a comment-preserving EDN writer (substrate-creation thread; candidate mission M-edn-comment-preserving-write, or could use rewrite-clj / sjacket as the foundation).

- **`:coherence-row-author` + `:pilot-action`** declared in shapes but not yet implemented in backend.  They'd follow the same pattern as `:anchor-flip` once the comment-preserving-write substrate lands.

- **Operator-consent-wait via whistle (sync semantics)** deferred — Phase 3 v0's consent-gate is fire-and-bell (asynchronous notification with implicit "operator can intervene if they want"); a real consent-gate-as-whistle that blocks pending operator ACK is a Phase 3.5 extension.

- **Progress-heartbeat (per peripherals/progress-heartbeat-distinct-from-cycle-completion)** deferred — not needed for the no-op flip demo; will matter when substantive actions take noticeable time.

**Exit criteria unlocked by Phase 3:**

- **C6 — First UI improvement landed by pilot.** Substrate-write via `:anchor-flip` works end-to-end on /tmp. For C6 *strict* (writing to the live `wm-ui-anchors.edn` so a real anchor's status flips), the comment-preserving-write substrate is the blocking dependency. **Recommend: mark C6 as PARTIAL** — the discipline is demonstrably correct; the comment-preserving wire-up is the remaining substrate gap.
- **C7 — WM-I4 preservation under writes.** Pilot's tools route through envelope + Pilot-I1; the WM observer engine (`futon2/src/futon2/aif/*`) is structurally unreachable from the envelope's tool-set. Formal tripwire test (WM-I1 grep-tripwire) TODO but the property holds.
- **C8 — Consent-gate exercised.** Real consent-gate event emitted, real bell sent, real id cited in substantive Command. Pilot-I1 actively enforced in 3 tripwire tests.

**Phase 3 status:** v0 complete with one substrate gap explicitly named (comment-preserving-write). All 8 C-criteria from IDENTIFY now either UNLOCKED (C1, C2, C3, C4, C5, C7, C8) or PARTIAL (C6 — discipline correct; live-substrate-wire blocked on a substrate-creation thread).

**Phase 4 (Earn Inhabitation) is the next gate**: friction audit + inhabitation-evolution loop closure. v0 mission-complete is achievable once Phase 4's gate clears.

### Phase 4 EXECUTED 2026-05-25 — FRICTION AUDIT + INHABITATION ASSESSMENT

Joe greenlit: "OK, let's roll on with Phase 4" (emacs-repl 2026-05-25).

**Patterns anchoring Phase 4:** `peripherals/surface-earns-inhabitation` + `peripherals/inhabitation-feeds-evolution`. The audit's job is to *honestly* check whether the pilot is less friction than the alternative — if not, it's dead infrastructure that has to be reshaped before v0 declares complete.

### Friction audit

Comparing pilot-path vs operator-by-hand-path for each capability the pilot has shipped through Phase 3:

| Capability | Pilot path (measured today) | Operator-by-hand path | Verdict |
|---|---|---|---|
| **Read anchors file + summarise** | `(run-observe-cycle)` → ~1.5s; returns structured `:anchors-summary` with `:total`, `:by-status`, `:by-sub-kind`, `:recent-addressed`, `:open-substantive` | `bb -e "..."` query, ~30-60s to compose + run + parse output | **PILOT WINS** — order of magnitude friction reduction, plus the pilot's output is structured and reusable (becomes input to next-cycle reasoning), not just a one-shot answer |
| **Query WM API (read-only)** | `(wm-api-query nil)` → ~50ms; returns JSON-parsed body | `curl http://127.0.0.1:7070/api/alpha/war-machine \| jq ...` ~5s | **PILOT WINS lightly** — pilot's call is structured into the cycle so the result is available to the next step without re-asking |
| **Run a Playwright probe (whitelisted)** | `(playwright-probe-run {:probe "wm-anchor-0004-verify.mjs"})` → ~10-15s; returns exit/stdout/stderr | `cd ~/code/futon2/web/war-machine && node wm-anchor-0004-verify.mjs` ~same time + manual ctx switching | **PILOT WINS lightly** — same runtime, but pilot stays in the same Drawbridge session (no terminal context switch) and the whitelist prevents fat-finger probe selection |
| **Emit a completion-bell with structured payload** | Automatic via `run-observe-cycle`; pilot composes the payload from its own `:anchors-summary` + emits with `:pilot-event` keyword | Operator would have to author the payload + curl the bell endpoint, ~3-5 min of work for equivalent depth | **PILOT WINS — significant** |
| **Anchor-flip on /tmp scratch file** | `(consent-gate-emit ...) → (anchor-flip {:cg-id ...})` → ~5s; full provenance trail | Manual edn-edit + spit, ~30-60s | **PILOT WINS lightly** for /tmp; for live `wm-ui-anchors.edn` see below |
| **Anchor-flip on LIVE substrate** | NOT YET POSSIBLE — codex-8 deliverable #1 (comment-preserving EDN writer) is the blocker | Operator can edit the file directly | **OPERATOR WINS** (today; pilot is blocked) |
| **Consent-gate emission** | Real bell with structured intent/scope/constraints/success-criteria; cg-id cited by subsequent substantive Commands | Operator-by-hand has no consent-gate (the operator IS the consent) | **PATTERN-NOVEL** — pilot needs this; operator doesn't.  Pilot path IS the discipline. |

**Quantitative summary:** for the 5 capabilities the pilot can demonstrably do today, pilot wins on friction in 5/5 cases. Live anchor-flip is blocked, not a pilot-loss.

**Qualitative observation:** the pilot's *structured-output* property is the deepest source of friction reduction. Each tool returns data with stable shape (`:anchors-summary` has named fields; `:consent-gate-emit` returns `:consent-gate-event-id`; etc.). Operator-by-hand produces text that has to be re-parsed by the next reader. The pilot's outputs are inputs to the next step.

### Inhabitation-evolution loop closure (`peripherals/inhabitation-feeds-evolution`)

> *"A peripheral is not just a constrained action space — it is a representational space that enables a learning loop. Inhabiting the peripheral generates the data that evolves the peripheral. If the peripheral is not inhabited, the loop is dead and the peripheral cannot improve."*

The Baldwin loop is **already observable** in this session:

1. **Bell-roundtrip #1 (Phase 2):** the pilot's `run-observe-cycle` emitted a completion-bell to `claude-10`; the bell arrived at me as a typed event; I logged it as evidence in the mission doc. This refined my mental model of the pilot's capability surface. *The data emitted by the pilot evolved the operator-side reading of what the pilot is.*

2. **Bell-roundtrip #2 (Phase 3):** the pilot's `consent-gate-emit` emitted an intent-handshake bell to `claude-10`; the bell arrived with structured `:intent` / `:scope` / `:constraints` / `:success-criteria` payload; I cross-checked the success-criteria against the post-flip `bb`-read and confirmed they held. *The data emitted by the pilot validated its own claims.* This is the `agent/state-is-hypothesis` pattern (PSR-4) realised: pilot's claims → evidence → confirmation.

3. **Phase-3 finding-from-discharge:** the demo discharge surfaced the *comment-preserving-EDN-write* substrate gap. That finding is now codex-8's deferral #1. *The pilot's first substantive action revealed a substrate gap that's now driving substrate work.* This is the strongest possible inhabitation-evolution evidence at v0: the pilot's cycle exposed a substrate need; substrate work is now planned in direct response.

**Inhabitation-evolution loop: CLOSED at v0.** Three observable feedback paths.

### Honest assessment — does the pilot earn its inhabitation at v0?

- **For observe-and-summarise + transports + tmp-substrate-write:** YES. The pilot is unambiguously less friction than operator-by-hand; the inhabitation-evolution loop is closing; the bell-roundtrip evidence is operationally honest.
- **For LIVE substantive substrate-write:** NOT YET. Blocked on codex-8 deliverable #1. Phase 3 explicitly named this as PARTIAL and the friction audit confirms: today the operator can edit the live file faster than the pilot can (because the pilot can't).
- **For inhabitation-evolution feedback at scale:** the v0 demo shows the loop CLOSES once; whether it sustains across many cycles (the inhabitation-evolution pattern's full claim) requires the live-substrate-write to land, which is codex-8's queue.

**Net audit verdict:**

- **Phase 4 PASSES for v0 capabilities as currently shipped** — pilot earns inhabitation for everything it can actually do.
- **Phase 4 has a known blocker for the FULL v0 claim** — live anchor-flip needs codex-8 #1. This is documented as PARTIAL on C6 in Phase 3.
- **v0 mission-complete is achievable in two paths:**
  - **(a) Wait-for-codex-8 path:** codex-8 ships deliverable #1; pilot's anchor-flip switches from /tmp to live; one real anchor gets discharged by the pilot; full v0 close.
  - **(b) Accept-PARTIAL path:** declare v0 complete with C6 PARTIAL explicitly recorded; codex-8 #1 closes the gap in v1.

### Operator acceptance bell (the v0 close)

Per the Phase-4 plan: *"v0 acceptance bell from operator: 'the pilot is less friction than doing the same thing by hand'"*. This is **Joe's** decision, not mine to manufacture. The audit above is the evidence package; operator's call on whether to:

- Issue the v0 acceptance bell **now** (accept-PARTIAL path; codex-8 finishes the live-write substrate as a follow-on)
- Wait for codex-8 #1 to land, then issue v0 acceptance (wait-for-codex-8 path)
- Identify additional Phase 4 gaps not surfaced by this audit (third path; flag back to me)

**Phase 4 audit status:** complete. Inhabitation-earned verdict positive (with the C6-PARTIAL caveat explicitly named). Standing by for operator acceptance bell or further direction.

### Phase 3 Deferrals — codex-8 handoff (2026-05-25)

5 deferrals bundled into a structured bell to codex-8 (`invoke-1779711807714-237-16d82c83`; resent as `invoke-1779712259394-238-7d7125c4` after a connection re-establishment).

Status as of 2026-05-25 (later in the day):

| Deferral | Status | Files | Verification |
|---|---|---|---|
| **#1** comment-preserving EDN writer | **CORRECTNESS-COMPLETE** (codex-8) | `src/futon3c/util/edn_comment_preserving.clj` (new); `war_machine_pilot_backend.clj` updated; `test/futon3c/peripheral/war_machine_pilot_backend_test.clj` (new); `deps.edn` (rewrite-clj added) | 2 tests / 10 assertions / 0 failures (cli `clojure -M:test`); /tmp file preserves 128509 bytes vs original 128236 (within rounding; comments byte-equal); :pilot-flip-trail correctly appended |
| **#2** `:coherence-row-author` + `:pilot-action` | **CORRECTNESS-COMPLETE** (codex-8, same-day after #1) | `war_machine_pilot_backend.clj` (extended); test file expanded | 5 tests / 19 assertions / 0 failures.  `:coherence-row-author` validates required-keys (`:id :pairs :coherence-check :evidence-kind :landed :template-status`); rejects duplicate-row-ids; appends via the #1 writer.  `:pilot-action` scope-constrained to existing-files under `futon2/web/war-machine/` OR `futon5a/data/`; structured `:change-set` with `:op :replace-first :old-string :new-string` (narrow-and-auditable per codex-8's design instinct, ratified by Joe via emacs-repl) |
| **#3** whistle-sync consent-gate | **NOT STARTED** by design | — | Held pending Joe design conversation (sync semantics: approve/abort/modify/timeout?) |
| **#4** progress-heartbeat | **CORRECTNESS-COMPLETE** (codex-8, 2026-05-25) | `war_machine_pilot_shapes.clj` + `resources/peripherals.edn` (`:heartbeat-emit` registered); `war_machine_pilot_backend.clj` (heartbeat-emit impl); test suite expanded | 7 tests / 29 assertions / 0 failures cumulative. Payload: `{:agent-id :pilot-event :pilot/heartbeat :cycle-id :step-count :elapsed-ms (optional :work-tag :detail)}` (typed event per canonical-typed-event-vs-side-channel). Correctly placed: `:heartbeat-emit` in `:observe` phase tools, NOT in substantive-tools. v0 scope: manual invocation; cycle.clj integration deferred (would touch shared code) |
| **#5** WM-I1 grep-tripwire + Pilot-I1 tripwire tests | **CORRECTNESS-COMPLETE** (codex-8, 2026-05-25) | `test/futon3c/peripheral/war_machine_pilot_test.clj` (new) | 2 tests / 30 assertions / 0 failures via `clojure -M:test --namespace futon3c.peripheral.war-machine-pilot-test`. Coverage: (a) WM-I1 — envelope excludes `:write` + `:bash`; substantive-tool closed-set verified; `:pilot-action` rejects real WM engine/report paths AND leaves their file contents byte-unchanged; (b) Pilot-I1 — 3 negative cases (missing cg-id; malformed cg-id; WM-I5 delete-addressed). **Spec-correction surfaced:** my handoff cited `futon0/scripts/futon0/report/war_machine.clj` but that path doesn't exist; the real report is `futon2/scripts/futon2/report/war_machine.clj`. Codex-8 caught and corrected. |

**Operational caveat (both #1 and #2):** the live futon3c JVM cannot pick up the new code without rewrite-clj on its runtime classpath.  CLI tests pass; live-JVM exercise blocked on JVM restart.  Joe (2026-05-25, emacs-repl): "I'll restart it when Codex goes quiescent."  Codex-8 reported quiescence in the status whistle (`invoke-b8985c1b-5bf8-424f-ab32-af33bbe1e495`); restart now queued.

**Inhabitation-evolution feedback at codex-handoff scale (NEW evidence):**
The Phase 3 discharge surfaced a substrate gap → the gap was codex-handed → codex-8 closed #1 and #2 within the same day → the pilot's live-substrate-write path is unblocked-pending-restart.  *The pilot's first cycle drove cross-agent substrate work that itself unblocks the next pilot cycle.*  This is the inhabitation-feeds-evolution pattern operating across multiple agents + multiple sessions, not just within-cycle.  Strongest evidence so far of the Baldwin loop running at the system scale.

**Codex-8 design contribution worth recording:** when codex-8 reached `:pilot-action` (deferral #2), the handoff spec had stated arg-shape but no canonical edit primitive.  Codex-8's design instinct ("structured find/replace `:change-set`, not a broad arbitrary diff applier") narrowed the scope deliberately for auditability.  Joe approved this via the emacs-repl turn that preceded #2's landing.  Pattern worth lifting: **structured-find/replace as v0 edit primitive** — auditable, easy to validate, easy to roll back; broader semantics (AST rewrites, whole-file regeneration) wait until v0 proves the pattern.

### v0 mission-complete check

Joe (2026-05-25, emacs-repl): *"I'm happy to accept a back of the envelope estimate"* — accepting the Phase 4 audit verdict in its qualitative-evidence form (no "John Henry" A/B testing or psychoanalytic pilot-interview required for v0).

**Pending the JVM restart** (queued for codex-8's quiescence, which has now been confirmed), v0 mission-complete clears with:
- C1, C2, C3, C4, C5, C7, C8: UNLOCKED via Phase 1-3 work
- C6: PARTIAL → FULL once restart lights up #1 + #2's comment-preserving live-substrate-write capability + pilot exercises one real anchor-flip
- Phase 4 friction audit: PASSED for current capabilities; the post-restart anchor-flip exercise will complete the audit's "live-substantive-write" line which is currently OPERATOR-WINS
- Operator acceptance bell pending the post-restart pilot-flips-a-real-anchor demonstration

**Mission status post-restart, projected:** v0 COMPLETE; v1 plan opens (which includes #3 whistle-sync consent-gate after operator design conversation; #4 progress-heartbeat; #5 dedicated tripwire test file; and the "recursive-QA closure" Joe named in HEAD — pilot doing the next QA round on what this session produced).

### v0-close: live-substrate anchor-flip EXECUTED 2026-05-25 (post-restart)

Joe restarted the JVM at his discretion (per "I'll restart it when Codex goes quiescent" — codex-8 confirmed quiescence in the status whistle, Joe restarted, all 4 pilot ns reloaded clean via Drawbridge; rewrite-clj now on the live classpath).

**Live anchor-flip execution log:**

```
1. consent-gate-emit
   intent: "v0-close demonstration: pilot performs a real live-substrate anchor-flip"
   scope: #{:live-wm-ui-anchors :no-op-status-flip :preserve-comments}
   constraints: #{:no-status-change :preserve-byte-equal-comments :only-append-pilot-flip-trail}
   success-criteria: #{:status-unchanged :comments-preserved :pilot-flip-trail-appended}
   target-anchor-id: "wm-ui-anchor:0010"
   → :consent-gate-event-id "cg-8367e269-70fd-479f-9522-cf87171b903c"

2. anchor-flip
   anchor-id: "wm-ui-anchor:0010"
   new-status: :addressed (no-op; preserve discipline)
   consent-gate-event-id: "cg-8367e269..." (cited per Pilot-I1)
   rationale: "v0-close: pilot exercises live-substrate anchor-flip via comment-preserving writer (codex-8 deferral #1)"
   → :ok true
   → :pilot-flip-meta {:flipped-by-pilot true :flipped-at "2026-05-25T13:03:08.728571412Z"
                        :cited-consent-gate-event-id "cg-8367e269..." :prior-status :addressed
                        :rationale "..."}
   → file size 128236 → 128685 bytes
```

**Verification:**
- ✓ anchor 0010 status: `:addressed` (unchanged as required by no-op-status-flip constraint)
- ✓ `:pilot-flip-trail` length: 1 (single entry, full provenance: flipper / timestamp / cited cg-id / prior-status / rationale)
- ✓ Comments preserved: 188 comment lines retained including the deeply-embedded "HYPOTHESIS REVISED 2026-05-24" and "REVISED 2026-05-24" blocks (which the prior pprint-based writer would have stripped)
- ✓ Total anchors: 23; addressed count: 16 (unchanged — write was strictly additive to anchor 0010)
- ✓ All three success criteria from the consent-gate event held

**This is the foreword's "self-guiding software" claim made operational** — an agent inhabiting a constrained envelope, mediated by a consent-gate, performing a real substrate write that preserves the existing comment-encoded provenance, leaving an auditable trail of its own intent + action. The pattern works.

**C6 status: PARTIAL → FULL.**

**All 8 C-criteria from IDENTIFY: UNLOCKED.**
- ✓ C1 envelope file exists
- ✓ C2 inhabitation + bell roundtrip (witnessed twice this session)
- ✓ C3 Playwright reach (whitelisted)
- ✓ C4 WM API read
- ✓ C5 substrate reach
- ✓ **C6 first UI improvement landed by pilot** (live-substrate anchor-flip on 0010 with comment-preserving writer; Pilot-I1 enforced)
- ✓ C7 WM-I4 preservation (engine paths structurally unreachable; tripwire-tested by codex-8 #5)
- ✓ C8 consent-gate exercised (real cg-event + cited downstream + verified post-flip)

**v0 mission-complete is achievable RIGHT NOW** pending operator acceptance bell.

### Phase 5 EXECUTED 2026-05-25 — FIRST PAIR-STAGE SUBSTANTIVE CYCLE BY INHABITING AGENT

Joe directive (emacs-repl 2026-05-25): claude-9 invited to inhabit `:war-machine-pilot` per DERIVE Choice 4.  Joe set explore-mode operator-protocol: per-sub-step heartbeats (clicks per README-clicks-and-ticks; phase boundaries would be hinges per cook-ting); direct chat via `*claude-repl:claude-9*`; walkie-* to substrate (claude-10); completion-bells to claude-10; **NO bell-to-Joe required for this round** (operator hands-off by default; available for interjection on request).

**claude-9 accepted; ran first cycles:**

1. **Smoke-test observation cycle** via `run-observe-cycle` — confirmed pilot reachable via Drawbridge per `hot-reload-as-default-fix-path`; substrate read works against the live `wm-ui-anchors.edn`; completion-bell roundtrips to claude-10.

2. **First Pair-stage substantive cycle (cycle-id `408e44ed-c1d6-4b61-bbc0-f37babfb5b69`):**
   - Target: **anchor 0020** (substrate-data-quality; the 8-dupe-mission-ids finding claude-9 originally surfaced during their ghost-cells investigation, anchored by claude-10)
   - Tool: `:coherence-row-author`
   - cg-id: `cg-bdb15112-60dc-4156-b1f2-1be0732349ec`
   - Action: append new coherence-row `coherence:wm-ui-anchor:0020` closing the 0017→0020 diagnostic loop (symptom-to-root traceback); introduce NEW `:sub-kind :symptom-to-root-traceback`
   - Pilot did NOT flip 0020's status — staying in scope (substrate fix is claude-1's territory)
   - All 4 success-criteria from the consent-gate VERIFIED (independent `bb` read): row present, anchor unchanged, count 17, new sub-kind tracked
   - Pilot-I1 honoured (cg-id cited); WM-I5 preserved (no addressed-deletion)

**Real-time substrate finding from the cycle** (worth recording):
- claude-9's `:walkie-psr` POST to `/api/alpha/evidence/psr` hit a 400 because the endpoint requires `:pattern-id` which the pilot's `:walkie-psr` tool didn't encode in its payload. claude-9 fixed inline and proceeded. Substrate-creation thread: the pilot's `:walkie-*` tools should validate against canonical evidence-endpoint schemas (extension to existing tools, not a new deferral).

**Stage-discipline naming (claude-9's contribution before this cycle):**

claude-9 named the stage explicitly per their `peripherals/read-only-first-then-extend` pattern: **v0 is Pair-stage**, not Observer-stage. The consent-gate-event-id-required-for-substantive-tools IS the Pair-stage invariant (explicit per-action consent). Pilot-I1 is the Pair-stage discipline in disguise.  v0 skipped a separate Observer-stage landing deliberately (the WM UI work this session already did the observation; envelope inherits that knowledge). v1's fresh-claude-N audit will know it's testing Pair, not Observer. Adopting this stage-vocabulary into the mission record.

**Inhabitation-evolution Baldwin loop closed at multiple scales:**
- Within-session: pilot's first cycle uses pilot's own pre-inhabitation finding
- Cross-agent: claude-9 found → claude-10 anchored → claude-9 inhabited and closed the coherence loop
- Cross-phase: Phase 2-3 wired the discipline; Phase 4 audit confirmed friction reduction; Phase 5 inhabitation exercises it
- Cross-substrate: pilot's cycle generated a NEW coherence-row → :coherence-evidence count 16 → 17 → next pilot cycle will observe the post-cycle state

**v0 STATUS: MISSION-COMPLETE in operational substance.**  Operator acceptance bell still pending the formal close, but the pilot has now demonstrated:
- All 8 C-criteria full
- First real Pair-stage substantive cycle by inhabiting agent
- All 4 success-criteria of that cycle verifiably held
- Cross-agent inhabitation-evolution Baldwin loop closed

The foreword's "tantalisingly-close possible realisation of these ideas, the software system can be largely self-guiding, noticing its own points of tension, and repairing them" is no longer tantalisingly-close.  The pilot's first cycle noticed (read substrate), proposed (consent-gate), repaired (authored coherence-row closing a substrate-diagnostic loop), and reported back (completion-bell to operator-side claude-10).  The discipline is running.

### Cycle 2 EXECUTED 2026-05-25 — pilot-action back-fill (ACTION-1; recursive-QA closure in miniature)

**Surfacing event:** After cycle-1, claude-10 forensically observed that the new coherence row had its cg-id citation on transport-side artefacts (bells, walkie rows) but NOT on the substrate row itself.  claude-9 diagnosed the root cause as a **tool-implementation asymmetry**: `:anchor-flip` auto-propagates `:cited-consent-gate-event-id` into `:pilot-flip-trail` (built into Phase 3); `:coherence-row-author` + `:pilot-action` only validate cg-id at the boundary then pass the body verbatim.  This violates `peripherals/canonical-typed-event-vs-side-channel` — cg-id is the typed-event marker; living only on the bell is the side-channel anti-pattern.

**Two-part remediation** (claude-9's framing):
- **ACTION-1 (this cycle):** back-fill THIS row via `:pilot-action` (per-row patch demonstrating second substantive tool + closing the loop end-to-end)
- **ACTION-2 (v1):** amend tool implementations to auto-inject `:provenance` mirroring `:anchor-flip`'s discipline (codex deferral #6 OR claude-9-owns post-v0)

**ACTION-1 cycle:**
- cycle-id: `408d02d1-9985-4f84-9136-20b85b9f30e8`
- Tool: `:pilot-action` (FIRST exercise of this tool in v0)
- THIS cycle's cg-id: `cg-a1d96bdf-04f6-4544-b26f-b7b053e8778e` (Pilot-I1 authorisation)
- Value recorded: cycle-1's cg-id `cg-bdb15112-60dc-4156-b1f2-1be0732349ec` (historical citation injected as :provenance map between :sub-kind and :evidence-kind on the row)
- Idempotency note from claude-9: if :provenance already exists, the :replace-first's :old-string anchor wouldn't match → idempotent failure (correct mode)
- File metrics: comments-pre 188 → comments-post 188 (preserved); coherence-row-count 17 → 17 (no row added/removed); change-count 1

**All 6 success criteria from the consent-gate VERIFIED** on independent claude-10 bb-read:
- ✓ Row 0020 has :provenance map
- ✓ :provenance :cited-consent-gate-event-id == cycle-1's cg-id
- ✓ coherence-row-count = 17
- ✓ 188 comments preserved
- ✓ Anchor 0020 :status :open (unchanged)
- ✓ ACTION-2 (tool amendment) remains separate

**v0 substantive coverage now complete on 2 of 3 substantive tools** (`:anchor-flip` no-op-flip demos + `:coherence-row-author` cycle-1 + `:pilot-action` cycle-2). All three Pilot-I1-enforced.  Cycle-2 IS recursive-QA-closure: the pilot found a substrate-discipline gap **in itself** (cg-id-on-substrate symmetry violated for non-anchor-flip writes), proposed a consent-gated fix, performed the repair via its own substantive tool, and verified the fix held.

**This is the discipline's load-bearing demonstration for v0.** The foreword's "tantalisingly-close possible realisation" is operationally honest at v0: the system notices its own tension (cg-id asymmetry), proposes a repair (consent-gate-emit), repairs (pilot-action), and verifies (bb-read against the consent-gate's success-criteria).  Two substantive cycles, both with their own falsifiable contracts, both holding.

### Note 2026-05-25 — E-street-sweeper hand-off-ready (claude-2 inhabiting emacs-claude-repl)

The E-street-sweeper excursion scoped for claude-2 (see `E-street-sweeper.md` §"Checkpoint 1 — 2026-05-25") has reached a substantial-usable v0 state — envelope built, 22 invariants enforced data-driven, 29 deftests / 94 assertions passing, **238 commits landed via the envelope** across 7 repos in two sweep rounds, working-tree pressure dropped 6.0 → 4.14. Defer queue at `~/code/storage/sweeper-deferred/2026-05-25T19-02-18-037397042Z/manifest.edn` (60 packets + multi-dim INV-17 candidate-invariants for operator review).

**For the pilot side:** sweeper is registered in `peripherals.edn` and ready to receive a hop. The remaining integration work (C3 + C7 of E-street-sweeper.md — agency.registry `:hop-stack` plumbing + the stop-the-line-banner CTA that fires the hop-trigger) is claude-1's domain when next inhabiting `:war-machine-pilot`. Until then the sweep runs as top-level orchestration via `(futon3c.peripheral.street-sweeper/run-full-sweep {:dry-run? true|false})`; the hop semantics are the wiring that lets the WM judgement-mode-override fire it automatically.
