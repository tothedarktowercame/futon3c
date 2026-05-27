# Mission: M-pilot-appearance

**Date:** 2026-05-25
**Status:** HEAD + IDENTIFY complete; MAP complete; DERIVE prototype-complete; ARGUE next
**Timebox:** 3 weeks soft (extends through the M-war-machine-pilot Phase 4 friction-audit window, since Phase 4 outcomes feed this mission's MAP)
**Predecessor:** `M-war-machine-pilot.md` — when Phase 3 lands, the pilot is operational; this mission picks up the question *what is it for the pilot to show up well across all four levels*
**Sibling-of:** `M-interim-director.md` — both missions are facets of the operator-on-Hyperreal instrumentation gap identified 2026-05-25; this one is artefact-shaped, M-interim-director is commercial-shaped
**Owner:** Joe (futon7)

## 0. Provenance

This HEAD distils a conversation on 2026-05-25 that moved through:
- the Jim Morrison / Jim Henson counterfactual as a meditation on virtual attractors and infrastructural conditions for non-viable-but-real artefacts
- the Adult Swim observation: virtual attractors find expression across distributed sites when the infrastructure tolerates high mutation rates
- the diagnostic finding that the futon stack has more introspective instrumentation than Hyperreal-the-commercial-entity does
- the recognition that the four-frame correspondence (pheno / geno / exo / xeno ↔ rūpa / saṅkhāra / citta / dhammas) gives the storyboard a *traversal mechanic* analogous to Braid's time-reversal
- the third-jhāna refinement: each frame is a register at which movement becomes detectable when the register is sufficiently quiet; field-cleaning at each depth is itself the game mechanic
- the LLM-Markov-boundary point: drawing the boundary around user+LLM+conversation dissolves the spurious duality between user-mind and LLM-mind and makes the *whole-system movement* the phenomenon
- the homoiconic-artefact decision: scenes as .cljc files that can be inhabited by the war-machine pilot, played as browser game-levels, rendered as slides, and read as REPL documents — same source, four readers
- the prototype: three skeleton files (`protocols.cljc`, `scene_04_pilot_phase_3.cljc`, `repl_printer.cljc`) authored as HtDP skeletons for Claude Code to fill in against the live stack

The longer conversational substrate for this mission is preserved in `~/code/futon3c/holes/missions/pilot-handoff/TN-appearance.md`. That note records the conceptual path from virtual-attractor / mutant-infrastructure discussion, through the commercial-instrumentation asymmetry, into the homoiconic storyboard proposal and the eventual naming/bounding of `M-pilot-appearance` as distinct from `M-war-machine-pilot`.

## 1. IDENTIFY — the gap

### 1.1 Right View — invariance claim

The pilot is an inhabitant of the war-machine peripheral. It now exists (post-Phase-3) as a capable agent that can read and write. But "showing up well" means more than "can act" — it means *appearing coherently across the four canonical futon5 registers at which inhabitants are observable*. The invariance claim:

> Across every artefact produced by the pilot, every mission it participates in, every interaction with the operator, every external demonstration: the pilot's appearance across the four canonical depths — pheno, geno, exo, xeno — must be mutually coherent. A pilot that is well-behaved at one depth but incoherent at another is not yet a pilot, regardless of what its phenotype suggests.

Out of scope: further capability-extension of the pilot (that's M-war-machine-pilot Phases 4-5); commercial pitching that uses the pilot as a demo (that's M-interim-director and successor EoI-drafting work).

### 1.2 Right Intention — predecessor / what it grows from

The gap this mission addresses is *the appearance asymmetry* identified across the session: the pilot was built to *act on* the futon stack but the apparatus for *understanding how the pilot itself appears* is younger, sparser, and not yet load-bearing. The closest predecessor work is:

- the four-frame correspondence already drafted (TODO: locate canonical file; provisional ref `~/code/futon3/docs/four-frame-correspondence.md`)
- the third-jhāna phenomenology described in the Operator's Foreword
- the Grand Unified Placemat's substrate-layer (which names *what rejects incoherence at each medium* but does not yet name *what makes an inhabitant legible at each depth*)

What's new here: extending the substrate-rejection framework from *artefacts* (code, prose, missions, strategy) to *inhabitants* (agents that act through the apparatus). The pilot is the first inhabitant complex enough to require this extension; the framework will generalize to operator-as-inhabitant and Hyperreal-as-inhabitant.

### 1.3 Right Speech — register / how outputs read as evidence

Mission outputs read as evidence when they *show* the pilot at one of the four canonical depths in a way that can be cross-checked against the other three. In this mission, the current working hypothesis is: `pheno = code`, `geno = AIF layer + war machine`, `exo = VSATARCS`, `xeno = the outward-facing output of this mission as a third-order reader of the stack`. The honesty test: an outside reader should be able to point at a specific output and say *this is the pilot appearing at the exo register via VSATARCS* without further explanation. If the depths are not distinguishable in the output, the output is not yet doing its work.

### 1.4 Right Mindfulness — observable indicators

Week-to-week, the mission is making progress when:
- new scenes are authored against the storyboard protocols and the pilot's appearance at each depth is *visible* in each scene
- substrate-checks at each depth start firing (rejecting candidate moves that are incoherent at that depth)
- the four renderers begin producing outputs that an outside reader recognizes as mode-appropriate
- the pilot's behaviour across missions shows the appearance-coherence improving — not "more capable" but "more legibly itself across registers"

Anti-indicators: scene authorship slows because the scene-fn body keeps wanting to do work that doesn't fit any of the depths (signals a missing distinction or a wrong-bounded mapping); rendering outputs feel arbitrary or interchangeable across modes (signals that the modes are not yet differentiated enough).

### 1.5 Right Livelihood — honest cost-test, scope, timebox, exit criterion

Timebox: 3 weeks soft. Scope: the *scaffolding for pilot-appearance instrumentation*, not its completion. The exit criterion is *a working storyboard with at least four scenes (one per world), each rendered in at least two of the four modes, with the appearance-at-each-depth visible across the scenes as a coherent characterization of the pilot*.

This mission is paid-work-adjacent but not directly paid-work: it produces the artefact the commercial offer points at (via the VSAT EoIs and any successor outreach). If the storyboard does not produce a demonstrable artefact by end of window, that's evidence the artefact-shape is wrong and the next mission should adjust shape rather than extending time.

### 1.6 Source material, repos, and dependencies

This mission's IDENTIFY stage is grounded in the following concrete source material:

- `~/code/futon3c/holes/missions/M-war-machine-pilot.md` — predecessor mission; supplies the pilot's capability-side shape, especially Phase 3 write-capability and the forthcoming Phase 4 friction-audit window that this mission explicitly depends on.
- `~/code/futon3c/holes/missions/pilot-handoff/protocols.cljc` — prototype storyboard DSL skeleton; records the current guess at the scene vocabulary.
- `~/code/futon3c/holes/missions/pilot-handoff/scene_04_pilot_phase_3.cljc` — prototype scene for the "pilot lands Phase 3" world-2 exemplar.
- `~/code/futon3c/holes/missions/pilot-handoff/repl_printer.cljc` — prototype Mode D renderer skeleton; the cheapest current proof path for multi-mode rendering.
- `~/code/futon3c/holes/missions/pilot-handoff/TN-appearance.md` — long-form conversational derivation of the mission; source for why the storyboard form, pilot-appearance framing, and mission boundary exist in the first place.
- `~/code/futon5a/data/grand-unified-placemat.edn` — substrate-layer donor; informs the "what rejects incoherence at each medium" side that this mission extends from artefacts to inhabitants.
- `~/code/futon4/holes/mission-lifecycle.md` — lifecycle contract this mission is being validated against.

Provisional / still-to-be-nailed-down source material, to be resolved in MAP rather than silently assumed:

- the four-frame correspondence file currently cited provisionally as `~/code/futon3/docs/four-frame-correspondence.md`
- the exact Operator's Foreword file/section carrying the third-jhāna phenomenology
- the exact canonical AIF event taxonomy file that the storyboard scene/event vocabulary should align with

Repos involved and current dependency shape:

- `futon3c` — this mission doc and the current `pilot-handoff` prototype artefacts
- `futon5a` — placemat / substrate-layer inputs
- `futon4` — mission lifecycle contract
- `futon7a` — intended long-term home of the storyboard namespaces implied by the prototype ns names

Dependency stance for IDENTIFY:

- hard dependency: `M-war-machine-pilot` continuing through Phase 4, because this mission's first real friction test is intentionally parallel to that earn-inhabitation window
- sibling coordination dependency: `M-interim-director`, because this mission's outputs are artefact-side inputs to the commercial frame but are not themselves commercial-first

### 1.7 Prototype status of the handoff artefacts

The three `pilot-handoff/*.cljc` files are **prototype artefacts**, not yet settled design:

- they are HtDP-style skeletons authored to make the storyboard shape concrete enough to inspect
- they are admissible at IDENTIFY because they function as source material and as a candidate first artefact
- they do **not** mean DERIVE is already complete; their records, protocols, helpers, and renderer stubs remain provisional and are expected to be revised by MAP/DERIVE once the source-material questions above are resolved

Operationally: this mission's IDENTIFY owns the gap statement, candidate mission-specific depth-mapping, and candidate scene list; the `pilot-handoff` code is a prototype substrate that subsequent phases may keep, revise, or discard.

## 2. The mission-specific reading of the four-level frame

The current operator clarification is that this mission does **not** introduce any new traversal levels. The canon remains the futon5 four-depth descent/ascent:

`pheno → geno → exo → xeno`

The actual design question is not cardinality but manifestation: **how do those generic futon5 levels show up in this mission?**

Current working mapping (operator clarification, 2026-05-25; to be tested in MAP):

| Depth     | Frame correspondence | In this mission, what it names | Pilot appearance |
|-----------|----------------------|-------------------------------|------------------|
| Pheno     | pheno / rūpa | The code substrate: processes, files, tests, tool calls, bytes moved, and the concrete write-capability landing in code. | The pilot has a body in the narrow futonic sense: runnable code, filesystem traces, process-space occupancy, resource-cost. |
| Geno      | geno / saṅkhāra | The AIF layer plus the War Machine as the inference/display apparatus the pilot inhabits: dispositions, signals, peripheral envelope, consent-gate shape. | The pilot has tendencies: a characteristic active-inference profile and a bounded action-shape inside the WM apparatus. |
| Exo       | exo / citta | VSATARCS: the textual/articulated distillation layer through which the stack becomes legible to readers within the system. | The pilot has a voice in the stack's own exo-medium: readable articulation, narration, and structured explanation. |
| Xeno      | xeno / dhammas | The output of this mission itself: a third-order reader of the stack that also serves as an interface to the outside world. | The pilot appears as pattern-bearing outward artefact: not just saying what happened, but instantiating a reusable outside-facing form. |

Implication: terms like `bio`, `cyber`, `semiotic`, `socio`, and `psycho` may still be useful as *overlay descriptors* for aspects of appearance, but they are **not** new traversal-depths unless earned by much stronger argument than currently exists.

MAP therefore has a narrower but sharper job:
- preserve the traversal canon `pheno → geno → exo → xeno`
- test the specific manifestation claim `pheno = code`, `geno = AIF layer + WM`, `exo = VSATARCS`, `xeno = this mission's outward-facing output`
- distinguish what is truly depth-structural from what is merely an overlay vocabulary about role, embodiment, semiotics, or standing
- reject any mapping that sounds elegant but fails to make actual scene-authorship or rendering more legible

## 3. Scene list — annex

The mission's IDENTIFY phase generates a candidate scene list as its first concrete artefact. The list below is *the question, not the answer* — these are the scenes that the mission will need to author and render to demonstrate pilot-appearance coherence; the actual authorship is the work of subsequent phases.

Scenes are organized by world (reader-stack level) and by where they fall in the methodological play-loop.

### World 1 — Futon stack

| Scene | Title | Depths active | Pilot-appearance focus |
|-------|-------|--------------|-----------------------|
| 01 | First compile | pheno only | (pre-pilot — establishes the substrate the pilot will later inhabit) |
| 02 | First test | pheno + geno | (pre-pilot — the geno-register first appearing as the test-failure signal) |
| 03 | First mission lifecycle | pheno + geno + exo | The futonic-mission format first appearing as an exo-articulation that operates over geno-state |

### World 2 — War machine

| Scene | Title | Depths active | Pilot-appearance focus |
|-------|-------|--------------|-----------------------|
| 04 | The pilot lands Phase 3 | all four | **PROTOTYPE — drafted in skeleton form 2026-05-25.** Pheno: pilot's body acquires write capability in code. Geno: consent-gate becomes part of the pilot's dispositional structure inside the WM apparatus. Exo: pilot, operator, and other agents converge on shared articulation. Xeno: substrate-provision is recognized as the outwardly reusable pattern. |
| 05 | Phase 4 — earn inhabitation | all four | The pilot acquires enough standing within the workflow to be recognized as less-friction than by-hand. The friction-audit produces the first cross-depth coherence check. |
| 06 | The pilot's first refusal | all four (esp. geno + exo) | The pilot declines a write that would violate WM-I4; the refusal must be coherent across all four depths (pheno: action not taken; geno: consent-gate fires; exo: refusal articulated; xeno: refusal instantiates a recognizable outside-facing pattern of bounded agency). |

### World 3 — VSATARCS

| Scene | Title | Depths active | Pilot-appearance focus |
|-------|-------|--------------|-----------------------|
| 07 | The documentation maintains itself | exo + xeno + (feedback to pheno) | The pilot appearing as an *author of documentation*; the exo articulation and xeno pattern-output become load-bearing. The textual distillation acquires its own existence as patterns. |
| 08 | A first reader encounters the stack | all four; new operator role | The pilot's appearance is now mediated by *another reader's* citta-frame. The Markov-boundary widens. |

### World 4 — Placemat / Hyperreal scale

| Scene | Title | Depths active | Pilot-appearance focus |
|-------|-------|--------------|-----------------------|
| 09 | A buyer encounters the apparatus | all four + commercial register | The pilot appears in a context where it is *being purchased* — the outside-facing xeno artefact acquires economic content, while the exo articulation picks up sales-pitch register without collapsing into it. |
| 10 | The Hyperreal apex updates | all four at meta-scale | Cross-project evidence flow: pilot-as-instance becomes pilot-as-pattern. The xeno-attractor for "agent inhabitant of a constrained capability envelope" generalizes from the war-machine to other peripherals. |
| 11 | The recursion is exhibited | meta-meta | The pilot recognizing *itself* as an instance of the meta-pattern. The dhamma-level appearance closing on itself. (NB: this scene is structurally risky and may turn out to be unwritable; that is informative.) |

### Cross-cutting scenes (not in a single world)

| Scene | Title | What it does |
|-------|-------|--------------|
| C1 | The third-jhāna interlude | A scene with no action — only field-cleaning at every depth. The play-loop slows; the player learns the field-cleaning mechanic directly. |
| C2 | A substrate rejection | The player makes a candidate move that the substrate rejects at one depth. The rejection becomes the lesson. |
| C3 | The cross-face flow | A pattern matured at one face (e.g., the substrate-provision meta-pattern at code-scale) becomes available at another face (the same pattern at commercial-scale, via the Hyperreal apex). |

## 4. Tensions to be addressed in subsequent phases

### 4.1 T1 — Whether the mission-specific four-depth mapping is actually right

The session first generated a five-level vocabulary extemporaneously, but the operator clarification is that the mission should use the existing futon5 four-depth canon rather than introduce new depths. The actual risk is now different: the proposed manifestation mapping (`pheno = code`, `geno = AIF + WM`, `exo = VSATARCS`, `xeno = this mission's outward output`) may still turn out to be wrong, incomplete, or too neat. The MAP phase needs to test that mapping against actual scene-authorship.

**Tag:** `address-in-map`

### 4.2 T2 — Whether scenes are authored top-down or bottom-up

Top-down: the level structure is fixed and scenes are commissioned to fill it. Bottom-up: scenes are authored as they arise and the level structure is recovered post-hoc. The HtDP skeleton-first approach in the prototype is *neither* — it's contracts-first, which gives top-down structure for the *vocabulary* but leaves bottom-up freedom for the *content*. Whether this is the right discipline for the full storyboard is a MAP-phase question.

**Tag:** `address-in-map`

### 4.3 T3 — The relationship to M-war-machine-pilot

M-war-machine-pilot owns the pilot's *capability*; this mission owns the pilot's *appearance*. The two missions will produce artefacts that need to compose cleanly. The risk: this mission may end up wanting to specify pilot-capability changes that should properly belong to M-war-machine-pilot's later phases. A coordination thread between the two missions needs maintaining.

**Tag:** `address-now`. **Content:** the coordination thread lives in this mission's `:cross-refs` field; updates that touch pilot-capability are routed to M-war-machine-pilot's checkpoint stream rather than implemented in this mission directly.

### 4.4 T4 — The relationship to M-interim-director

M-interim-director is the commercial-instrumentation mission. The storyboard *is* a commercial artefact (the scenes are demonstration objects) but its primary work is not commercial. The risk: this mission's outputs get conscripted into commercial demonstrations before they are coherent enough to bear the weight, producing scenes that look like sales-deck slides rather than play-able levels.

**Tag:** `address-now`. **Content:** scenes are not promoted to commercial use until they pass the cross-mode rendering test (a scene must render successfully in at least two modes before it is shown to any external party). This is the storyboard's analogue of M-war-machine-pilot's earn-inhabitation criterion.

### 4.5 T5 — Whether the overlay vocabulary is useful or merely decorative

The session adopted `psycho/bio/socio/cyber/semiotic` from the operator's own framing without testing whether it actually improves observation once the mission is grounded back into the four canonical depths. Alternative vocabularies exist: the Buddhist khandhas (already partly invoked via the four-frame correspondence); the Pearce/Friston/Clark active-inference vocabulary; the Latourian actor-network vocabulary; the Goffman dramaturgical vocabulary. The current overlay vocabulary's virtue is that it is the operator's native idiom; its risk is that it is *only* the operator's native idiom and may not travel.

**Tag:** `decision-debt-defer`. **Decision point:** mission close. **Content:** revisit after first four scenes are authored; if the vocabulary has earned its keep through use, retain; if it has felt forced at any point, propose a successor vocabulary.

## 5. Seeded checkpoints

### Checkpoint 0 — prototype-three-files lands

**Trigger:** the three skeleton files from session 2026-05-25 (`protocols.cljc`, `scene_04_pilot_phase_3.cljc`, `repl_printer.cljc`) are filled in by Claude Code and evaluate end-to-end.

**Callback:** unblocks MAP phase; the prototype is the substrate that MAP works against.

### Checkpoint 1 — first cross-world scene

**Trigger:** at least one scene from each of the four worlds (W1, W2, W3, W4) has been drafted at skeleton level.

**Callback:** unblocks DERIVE phase; the cross-world scene-set is the substrate the level-structure can be designed against.

### Checkpoint 2 — first cross-mode render

**Trigger:** at least one scene renders successfully in at least two of the four modes (most likely Mode D = REPL and Mode C = slide-deck, since they are the cheapest to implement).

**Callback:** unblocks ARGUE phase; the cross-mode render is what makes the multi-platform claim demonstrable.

### Checkpoint 3 — the friction-test

**Trigger:** the storyboard is used to author at least one scene that documents an *actual* mission-round in real time (not retrospectively). The friction of doing so is measured.

**Callback:** parallel to M-war-machine-pilot Phase 4 — same earn-inhabitation discipline. If the storyboard is more friction than by-hand mission-documentation, the storyboard is dead infrastructure and the mission either revises the approach or closes with that finding.

### Checkpoint close — preregistered scene list

**Trigger:** the scene list in §3 has been refined enough that an outside reader could pick up any scene and know what it is supposed to do without further conversation.

**Callback:** mission closes; the work of *authoring* the full scene list transitions to a successor mission (or to ongoing operator practice).

## 6. Open shape — to be specified in MAP onward

- The relationship between scenes and missions: is each scene the documentation of a mission-round, or are scenes generic templates that any mission-round can instantiate? Or both?
- The relationship between scenes and the AIF event taxonomy: scenes emit events, scenes are *organized* by events; what's the discipline?
- The relationship between scenes and the Foreword: the Foreword can refer to scenes as companion pieces, but scenes may also reference the Foreword as their philosophical preamble. How to avoid circular-reference becoming circular-dependency?
- The economics of cross-mode rendering: which renderers are cheap, which are expensive, which produce artefacts that have standalone value vs which only serve the storyboard internally?

## 7. MAP — initial survey (2026-05-25)

This section records the first concrete survey pass, driven by the search
clues in `pilot-handoff/TN-appearance.md`. It is intentionally factual:
what already exists, where it lives, and what remains missing.

### 7.1 Note-derived search targets

The long-form note suggested the following search targets for MAP:

- the canonical four-depth framework (`pheno/geno/exo/xeno`)
- the existing VSATARCS reader surface and its story/scene format
- the exact Operator's Foreword passage tying self-guiding software to VSATARCS
- the current typed event / evidence vocabulary already in the stack
- the substrate-layer donor that the storyboard wants to extend
- any existing storyboard / deck / renderer infrastructure that would make the prototype less greenfield than it looks

### 7.2 Survey questions answered

**Q1. Where is the strongest currently-located canonical source for the four-depth framework?**

Current best answer: `~/code/futon3/library/futon-theory/four-types.flexiarg`.
It is not written in the exact `pheno → geno → exo → xeno` vocabulary, but it
*is* the strongest currently-located canonical articulation of the same
four-part structure: genotype / phenotype / exotype / xenotype, with explicit
applications to futon (`genotype = pattern definitions / DSL specs`,
`phenotype = system behavior`, `exotype = interfaces + event protocol shapes`,
`xenotype = portable higher-order pattern`).

Corollary finding: the provisional reference `~/code/futon3/docs/four-frame-correspondence.md`
was **not** located in this first survey pass. The mission should therefore stop
treating that path as likely-canonical and instead treat it as unresolved until
a stronger home is found.

Supporting-but-weaker reference: `~/code/futon5/docs/M-diagram-composition.md`
mentions `pheno-geno-exo-xeno (four-level stacking)` but does not itself define
the framework.

**Q2. What existing reader/rendering infrastructure already exists that the storyboard could lean on?**

VSATARCS is already an Arxana-native reader for scene-structured Markdown:
`~/code/futon4/README-vsatarcs.md` and `~/code/futon4/dev/arxana-browser-vsatarcs.el`.
Relevant facts:

- VSATARCS already reads `*.md` stories in explicit scene form (`## Scene: Title | anchor`)
- it already has a story/scenes object model in embryo (`vsat-story` / `vsat-scene`)
- it is explicitly a **reader, not a writer**
- it already has a test surface (`futon4/test/arxana-browser-vsatarcs-test.el`)

Implication: the storyboard is not greenfield at the reader-surface level.
There is already a mode that can consume scene-structured narrative artefacts.
What does *not* exist yet is a bridge from the proposed `.cljc` storyboard
artefact to VSATARCS's current Markdown scene-form.

**Q3. Where is the exact Foreword material this mission depends on?**

The active Foreword file is `~/code/futon7a/essays/operator-foreword/operator-foreword.md`.
The key passages for this mission are:

- the "tantalisingly-close possible realisation" claim about self-guiding software
- VSATARCS as a reader of the FUTON stack, and its readers as second-order readers
- the lifecycle paragraph naming MAP as "surveying what already exists"

This resolves the earlier IDENTIFY TODO about the Foreword's exact home.

**Q4. What existing substrate-layer donor already names coherence-rejection media?**

`~/code/futon5a/data/grand-unified-placemat.edn` already names the operative
substrate layer. The relevant elements for this mission are:

- `:S-arxana-essay`
- `:S-flexiarg`
- `:S-futonic-mission`
- `:S-placemat-itself`

This matters because the prototype scene's `substrate-check` concept is not an
invention from nowhere; it already has a donor vocabulary and donor semantics.

**Q5. What is the current strongest candidate for the storyboard's event vocabulary?**

No dedicated storyboard event vocabulary exists yet. The strongest current
candidates are two adjacent vocabularies:

- `~/code/futon4/holes/missions/M-interest-network-coupling.md` §2.1, which
  defines a neutral `state/*` + `link/asserted` event vocabulary
- `~/code/futon3c/docs/evidence-facets.md`, which documents the current
  evidence-entry schema (`evidence/type`, `evidence/claim-type`,
  `evidence/subject`, `evidence/tags`, `evidence/in-reply-to`)

MAP finding: the prototype scene DSL should not silently invent a third,
competing event ontology. It needs either:

- a clean adaptation of `state/*` / `link/asserted` for storyboard-emitted
  evidence, or
- an explicit declaration that scene-internal trace moves (`clean-register`,
  `descend`, `substrate-check`, `recognize-pattern`) are *not* event-log
  entries and only the scene's outward `:evidence-emitted` payload aligns with
  the stack's existing event vocabulary.

**Q6. What existing home already exists for the storyboard artefact?**

The current implementation substrate is still only the prototype under
`~/code/futon3c/holes/missions/pilot-handoff/`.

The intended long-term home suggested in the note
(`~/code/futon7a/essays/placemat-storyboard/`) does **not** currently exist in
the repo. Therefore the current namespace choice (`futon7a.placemat.storyboard.*`)
is an anticipatory design choice, not an already-backed file layout.

### 7.3 Ready vs missing

| Ready (no new code needed to inspect/use) | Missing (actual work still to do) |
|---|---|
| Canonical four-type donor in `futon3/library/futon-theory/four-types.flexiarg` | Confirm or replace the missing `four-frame-correspondence.md` reference with a truly canonical home |
| Mission lifecycle contract in `futon4/holes/mission-lifecycle.md` | Decide and document the exact relation between the mission's `pheno/geno/exo/xeno` language and the genotype/phenotype/exotype/xenotype donor |
| VSATARCS reader surface for scene-structured Markdown (`README-vsatarcs.md`, `arxana-browser-vsatarcs.el`) | Bridge `.cljc` storyboard scenes to an existing reader format, or explicitly choose not to |
| Operator's Foreword in `futon7a/essays/operator-foreword/operator-foreword.md` | Extract the exact companion relation between Foreword and storyboard without creating a circular dependency |
| Placemat substrate layer in `futon5a/data/grand-unified-placemat.edn` | Implement real substrate-check behavior against those donor substrate categories |
| Existing typed evidence vocabulary in `evidence-facets.md` | Choose the storyboard's event-alignment discipline; the prototype still carries placeholder `:evidence-emitted` maps |
| Existing neutral `state/*` event vocabulary in `M-interest-network-coupling.md` | Decide whether scene traces emit `state/*` events, evidence entries, both, or neither |
| Prototype artefacts in `pilot-handoff/` (`protocols.cljc`, `scene_04_pilot_phase_3.cljc`, `repl_printer.cljc`, `TN-appearance.md`) | Fill the stubs, get Scene 04 evaluating, and prove at least one renderer end-to-end |
| Existing VSATARCS story corpus / scene-form conventions | No long-term `futon7a` storyboard directory or assembly namespace exists yet |

### 7.4 Surprises / scope corrections

- The biggest correction from this MAP pass is that the mission's hard problem is **not** inventing new levels; it is relating a mission-specific four-depth reading to an already-existing four-type donor framework without hand-waving.
- The second correction is that the storyboard is less isolated than the prototype initially made it seem: reader infrastructure, typed evidence vocabulary, and substrate vocabulary all already exist nearby.
- The main unresolved source-material gap after this pass is the not-yet-located canonical four-frame correspondence file. That remains a real MAP task, not a prose TODO to ignore.

### 7.5 MAP pass 2 — eightfold-path pairing as a bridge hypothesis

Prompt for this pass: Joe suggested that the four-part structure may be
recoverable by **bracketing the eightfold path into pairs** (for example
View+Intention), and pointed to two concrete sources:

- `~/code/futon3/holes/missions/M-weird-modernism.md`
- `~/code/futon3c/holes/missions/pilot-handoff/TN-repertoire.md`

First source check: `M-weird-modernism.md` does indeed contain an explicit,
already-authored one-to-one correspondence between the eightfold path and the
eight-phase mission lifecycle:

- Right View ↔ HEAD
- Right Intention ↔ IDENTIFY
- Right Speech ↔ MAP
- Right Action ↔ DERIVE
- Right Livelihood ↔ ARGUE
- Right Effort ↔ VERIFY
- Right Mindfulness ↔ INSTANTIATE
- Right Concentration ↔ DOCUMENT

It also states two further things that matter here:

- the correspondence was **recognised, not engineered**
- the path is a **loop**, not eight independent slots; specifically, the last
  factors prepare the ground for the first ones on the next turn

Second source check: `TN-repertoire.md` gives the missing four-part bridge
material. It uses Acaster's Repertoire as a four-episode scaffold and aligns
that scaffold to the four frames of reference / four satipaṭṭhānā. More
importantly, it explicitly names the loop-logic Joe is using in practice:

- Right Mindfulness + Right Concentration are both the "last two links" in the
  path **and** the foundation for Right View + Right Intention
- the four-part teaching frame is therefore not a rival to the eightfold path;
  it is one way of *taking a turn through it*

#### MAP finding

This pairing hypothesis is **structurally plausible** and is now grounded in
actual local source material rather than only conversation memory.

The strongest currently-supported recovery looks like this:

| Eightfold-path pair | Lifecycle pair | Four-part recovery role |
|---|---|---|
| View + Intention | HEAD + IDENTIFY | seeing and naming the gap |
| Speech + Action | MAP + DERIVE | truthful articulation and compositional response |
| Livelihood + Effort | ARGUE + VERIFY | earning and sustaining a viable path |
| Mindfulness + Concentration | INSTANTIATE + DOCUMENT | clear enactment plus durable crystallisation; also the loop-preparation for the next turn |

#### What this does help with

- It gives a **real local bridge** from an eight-phase discipline to a
  four-part pedagogical / observational structure.
- It supports the idea that the storyboard can be authored as **four-part
  turns of practice** without denying the underlying eightfold discipline.
- It strengthens the case that the mission's scene work can borrow from
  Buddhist frames without pretending the mission itself is a Buddhist text.
- It opens an explicit **deck-granularity option** for the pilot artefact:
  the same underlying methodological turn can be rendered either as
  **4 macro scenes / cards / dashboard elements / user interactions** or as
  **8 finer-grained scenes / cards / dashboard elements / user interactions**.

#### Deck-granularity implication

The current best reading is more specific than a simple paired-factor
compression:

- **8-element deck** = one element per lifecycle / path factor
  (`HEAD`, `IDENTIFY`, `MAP`, `DERIVE`, `ARGUE`, `VERIFY`, `INSTANTIATE`,
  `DOCUMENT`), i.e. one full **futonic mission** rendered card-by-card
- **4-element deck** = one element per turn of a distinct **Pilot's Cycle**,
  not merely a presentation-layer pairing of the same mission lifecycle

Operationally, this means the "game" interaction can be designed at either of
two legitimate resolutions:

- an **8-interaction loop**, where the user engages the full mission-shaped
  discipline factor-by-factor
- a **4-interaction loop**, where the user engages a pilot-native cycle whose
  stages are still to be named exactly but which is evidenced already by the
  Pair-stage / inhabitation practice around the War Machine pilot

This is useful because it gives the pilot's deck a principled scalability
without collapsing two different things into one:

- the 8-part version is the mission as such
- the 4-part version is the pilot's operative cycle through that mission-space

One practical clue here is the now-replaced **Pilot Contract** card on the War
Machine surface (`wm-ui-anchor:0011`). The current repo state still preserves
the anchor, the replacement site, and the reframe to the Inhabitation Log, but
does **not** preserve the exact old three-bullet copy. That means the card is a
real precedent for a compact pilot-facing interaction surface, but its exact
bullet structure cannot yet be treated as canon from source alone.

MAP consequence: deck/card/scene/dashboard design should be evaluated with this
as an explicit question rather than decided ad hoc:

- which surfaces want the **4-part Pilot's Cycle**?
- which surfaces want the **8-part futonic mission loop**?
- can the old Pilot Contract / new Inhabitation Log slot become the place where
  the 4-part cycle is made operator-visible?
- if so, what are the four stages, and are they best rendered as bullets,
  cards, scenes, or dashboard actions?

#### Composite donor problem: Contract + REPL + four foundations

`REPL` by itself is not sufficient. The real design problem is to align several
different donors that are each carrying a different kind of truth:

- **Pilot Contract** — card-shape / commitment-shape / compact operator-facing
  articulation
- **REPL** — operational turn logic of inhabitation (`Read`, `Eval`, `Print`,
  `Loop`)
- **kāya / vedanā / citta / dhammā** — the contemplative or attentional content
  of the four turns
- **R1-R12** — the operational AIF completeness / self-guiding-software
  walkthrough already made explicit in the War Machine and VSATARCS material

These should not be collapsed into a single vocabulary too early. A better
working hypothesis is:

- the **Contract** gives the brevity and card discipline
- the **REPL** gives the machine / inhabitant action grammar
- the **four foundations** give the humanly meaningful register of what each
  turn is noticing
- the **R-criteria** give the validation grammar for whether the cycle is
  actually load-bearing as an apparatus rather than merely evocative in prose

So the `Pilot's Cycle` likely wants to be a **composite alignment**, not a
single donor imported wholesale.

Important distinction: `R1-R12` is *not* another candidate visible card
vocabulary in the same way as `REPL` or `kāya/vedanā/citta/dhammā`. In the
local sources it is the operationalisation of the foreword's
"self-guiding software" claim — twelve checks for whether the apparatus is
really doing what the discipline says it should. So it belongs here as a
**backing audit lattice**, not as the face text of the deck.

Current best synthesis sketch:

| Pilot turn | REPL logic | foundation register | R-criteria backing cluster | card question |
|------------|------------|---------------------|---------------------------|---------------|
| 1 | `Read` | `kāya` | `R1-R3` belief / observation / prediction-error | what is happening here, concretely, in the surface/body of the stack? |
| 2 | `Evaluate` | `vedanā` | `R4-R7` forward model / EFE / policy / adaptive precision | what has salience, pressure, pull, or aversion right now? |
| 3 | `Print` / `Propose` | `citta` | `R8-R9` trace / validation / named properties | what reading, story, or coordinated move is mind producing from that? |
| 4 | `Loop` / `Log` | `dhammā` | `R10-R12` live operation / composition / outer loop | what pattern does this instance belong to, what artefact gets emitted, and what seeds the next turn? |

This is stronger than `REPL alone` because it explains why the four turns are
not just interpreter verbs:

- `Read` becomes embodied / surface-sensitive rather than generic input
- `Evaluate` becomes attunement to felt significance rather than abstract
  scoring
- `Print` becomes the stage where an inner reading becomes a sharable
  structured artefact
- `Loop` becomes not just repetition but recap / categorisation / reseeding of
  the next cycle, which is exactly the dhammā/Recap load-bearing move in
  `TN-repertoire.md`

It is also stronger than using `R1-R12` directly as the deck's visible
structure:

- `R1-R12` is too fine-grained and too apparatus-specific to be the face of the
  pilot's deck
- but it is exactly the right thing to sit *behind* the deck as the check that
  the four turns are not empty theatre
- in that sense, the deck can be four-part while the audit behind it remains
  twelve-part

It also explains what the old **Pilot Contract** card may have been doing,
even though its exact three-bullet wording is not recoverable from current
source: compactly exposing a pilot-facing discipline rather than merely naming a
capability.

Working consequence: the visible deck labels may need to be hybrid rather than
purely technical. For example, the structural spine can stay REPL-shaped while
the card titles or prompts speak in the more experiential/foundational
register.

#### What this does **not** yet settle

It does **not** by itself determine the mission's earlier four-depth claim
`pheno = code`, `geno = AIF + WM`, `exo = VSATARCS`, `xeno = outward-facing
mission output`.

Reason: the path-pairing material is primarily about **practice sequencing**
and **pedagogical grouping**, whereas the `pheno/geno/exo/xeno` material is
about **what kind of thing is being observed** at each depth. Those are
related, but not identical questions.

So the honest MAP stance after pass 2 is:

- the eightfold-path pairing is a credible secondary bridge into a four-part
  frame
- it may help with scene design, pacing, and methodological exposition
- it is **not yet sufficient** to replace the existing donor problem of how
  the mission-specific four-depth reading relates to the four-type / four-frame
  canon

### 7.6 Breadcrumb — second 8-fold structure surfaced (2026-05-27)

A second 8-fold structure appears in the stack and may share DNA with the
eightfold-path ↔ mission-lifecycle correspondence recorded in §7.5.

**M-interest-network-coupling step (b)**, committed 2026-05-27 as
`~/code/futon3/library/structure/interest-event-vocabulary.flexiarg`, declares
**eight typed state-transition events** that posterior-update interest-network
entities (EoIs, institution-objects, basins, and per the flexiarg's downstream-
consumer list also sorries and missions):

- `state/spawned` — entity newly proposed
- `state/refined` — non-truth-changing revision
- `state/strengthened` — confirmed by lived evidence
- `state/addressed` — closed or fulfilled
- `state/falsified` — empirically disproven
- `state/foreclosed` — declared no-longer-viable
- `state/reopened` — re-entered after closure
- `link/asserted` — cross-entity relation declared

That is eight slots, with seven `state/*` events plus one cross-entity
`link/asserted`. The mission-lifecycle is also eight slots
(HEAD/IDENTIFY/MAP/DERIVE/ARGUE/VERIFY/INSTANTIATE/DOCUMENT), with seven
authoring phases plus one cross-mission DOCUMENT-as-handoff.

The candidate DNA: **the eight slots are the lifecycle of work whose object
is symbolic — whether the object is a mission (work-in-itself) or an
interest-network entity (work-tracked-by-missions)**. Mapping candidates
(seeds for IDENTIFY-of-this-question, not commitments):

| Mission lifecycle (work-self) | Interest-event (work-tracked) | Note |
|---|---|---|
| HEAD | `state/spawned` | First articulation; entity exists |
| IDENTIFY | `state/refined` | Sharpening without truth-claim shift |
| MAP | `link/asserted` | Cross-entity dependency surfaced |
| DERIVE | `state/strengthened` | Position taken on positive evidence |
| ARGUE | (no obvious dual?) | Argument-as-act is not in the event vocab |
| VERIFY | `state/falsified` (refusal) or `state/addressed` (confirmation) | The fork-point |
| INSTANTIATE | `state/addressed` | Closure as evidence |
| DOCUMENT | `state/foreclosed` or `state/reopened` | Either retire or re-enter |

The table is **deliberately imperfect** — `state/foreclosed` and
`state/reopened` form a duality the lifecycle treats as DOCUMENT-then-next-HEAD
(loop closure per the §7.5 note that "the path is a loop, not eight
independent slots"); and ARGUE has no obvious event-dual which itself is
informative — argue-as-act may be the *meta-event-emitting* moment rather
than an event in the vocabulary.

What this breadcrumb does **not** claim: that the two 8-folds are isomorphic.
What it does claim: they are not coincidentally 8-fold. Both are decompositions
of the lifecycle of symbolic work, and the partial mapping is dense enough that
recognition is preferable to engineering — same diagnostic as §7.5 said about
the eightfold-path correspondence.

Provenance: surfaced 2026-05-27 emacs-repl session (claude-1 + Joe) during
M-action-cost-modelling T2-closure work that arrived at this 8-fold parallel
via the sorry-typing → ΔT(mission) → mission-phase → eight-phase ordinal
chain. Cross-references:

- `~/code/futon3/library/structure/interest-event-vocabulary.flexiarg`
  — the eight-event vocabulary itself (landed 2026-05-27 10:53)
- `~/code/futon3/holes/missions/M-weird-modernism.md` §"Eightfold Path ↔
  mission lifecycle" — the eightfold-path mapping that §7.5 references
- `~/code/futon3c/holes/missions/M-action-cost-modelling.md` §3.8 — the
  aliveness-synthesis projection-apparatus from which this parallel surfaced
- `~/code/futon4/holes/missions/M-interest-network-coupling.md` — the
  upstream mission whose step (b) committed the event vocabulary

### 7.7 Breadcrumb — aliveness-substance four-fold as candidate fifth donor (2026-05-27)

§7.5's donor synthesis table assembled four donors for the Pilot's Cycle deck:
`REPL` (turn logic), `four foundations` (`kāya/vedanā/citta/dhammā` —
attentional register), `R-criteria` (validation grammar), and `Contract`
(card discipline). Today's M-action-cost-modelling §3.8 work surfaced a
fifth candidate donor whose alignment with the existing table is dense
enough to flag.

The donor: **the aliveness-substance four-fold**.

Joe's three-term articulation 2026-05-27: *"Mana flows ↔ anamnesis
discharges ↔ niche gets constructed; the three are the same loop."*
The fourth term to round the loop is **anamnesis disclosure** — same
substance as anamnesis-discharge, but in its visible-but-not-yet-acted-on
moment. The split is natural: tension first becomes legible in the
substrate (high-ΔT region surfaces, false-floor names a missing edge
family), then gets acted on by the felt-pull, then leaves a typed-edge
trace, then registers as renewed-aliveness ready for the next cycle.

Aligning with §7.5's existing donor table:

| Turn | REPL | foundation | R-criteria | aliveness substance | what surfaces |
|------|------|------------|-----------|--------------------|--------------|
| 1 | `Read` | `kāya` | `R1-R3` belief / observation / PE | **anamnesis discloses** | un-discharged tension becomes legible in the substrate — high-ΔT region surfaces, false-floor names a missing edge family |
| 2 | `Eval` | `vedanā` | `R4-R7` EFE / policy / precision | **anamnesis discharges** | felt salience selects the candidate; action taken |
| 3 | `Print` | `citta` | `R8-R9` trace / validation | **niche constructed** | typed-edge trace lands in substrate-2 — the substrate is enriched for the next agent's read |
| 4 | `Loop` | `dhammā` | `R10-R12` live operation / outer loop | **mana flows** | pattern crystallises as artefact; felt-aliveness seeds the next disclosure |

**Substance singularity claim**: where REPL/foundations/R-criteria each
carry a *different kind of truth* (turn logic / attentional register /
validation grammar respectively), the aliveness substance carries **one
substance through four moments**. This is structurally distinct from the
other donors and partially explains why it aligns so densely with all
three of them at once. The loop-closure is exact: mana-flow at turn 4
seeds anamnesis-disclosure of turn 1 of the next cycle, completing the
recurrence that the §7.5 note about "the path is a loop, not eight
independent slots" already insisted on.

**Connection to niche construction (library pattern)**: the
`aif/niche-construction` flexiarg landed today
(`~/code/futon3/library/aif/niche-construction.flexiarg`) names false-floor
readings as the surface form of turn 1 disclosure. The pattern's NEXT-STEPS
list — "surface false-floor readings as such" — is operationally a turn-1
discipline: don't let the agent silently zero a signal; let the absence
register as anamnesis disclosed.

**What this breadcrumb does not claim**: that the aliveness-substance is
a *primary* donor on the same level as REPL or the four foundations. It
is a *secondary alignment* that happens to be substantively dense. Whether
it gets a card-text appearance, lives only in the agent's internal
discipline, or merely backs the other donors via §3.8's synthesis is an
operator/ARGUE-stage question, not settled here.

Provenance: surfaced 2026-05-27 emacs-repl session (claude-1 + Joe) during
M-action-cost-modelling §5.1 T2/T4/T6/T9 closure work. The four-term loop
emerged from Joe's three-term articulation + the alignment question with
this mission's existing donor table. Cross-references:

- `~/code/futon3c/holes/missions/M-action-cost-modelling.md` §3.8 — the
  aliveness synthesis (Alexander / Salingaros / Mana / Anamnesis / T / EOI)
- `~/code/futon3/library/aif/niche-construction.flexiarg` — the
  library pattern naming false-floors as the disclosure-phase signal
- `~/code/.claude/projects/-home-joe-code/memory/project_aliveness_synthesis.md`
  + `feedback_niche_construction_as_wm_principle.md` — memory anchors

## 8. ARGUE — seeded argument substrate

The mission can now move into ARGUE on the basis that two things are true:

- **MAP is complete enough** to bound the problem honestly. The donor set is no
  longer "whatever sounded right in conversation"; it is now constrained by
  actual local sources: futon5 four-depth canon, `TN-appearance.md`,
  `TN-repertoire.md`, the `R1-R12` walkthroughs, and the prototype scene /
  renderer substrate.
- **DERIVE is prototype-complete enough** to argue against. The
  `pilot-handoff` prototype is no longer only HtDP scaffolding; it now carries
  a real scene trace, a REPL rendering, and a concrete 4-card `Pilot's Cycle`
  mockup. ARGUE can therefore treat the prototype as an exhibited artefact
  rather than only a design intention.

ARGUE should not pretend to derive the whole mission from one flexiarg. The
right shape is a **bundle of patterns**, each grounding a different clause of
the claim. In particular, the eightfold-path material should not be treated as
only a remembered analogy from notes: the stack already contains a dedicated
`liberation/` pattern family that formalises the noble-path logic directly.

### 8.1 Eightfold-path donor family

#### `liberation/noble/right-*`

`~/code/futon3/library/liberation/noble/right-view.flexiarg`

`~/code/futon3/library/liberation/noble/right-intention.flexiarg`

`~/code/futon3/library/liberation/noble/right-speech.flexiarg`

`~/code/futon3/library/liberation/noble/right-action.flexiarg`

`~/code/futon3/library/liberation/noble/right-livelihood.flexiarg`

`~/code/futon3/library/liberation/noble/right-effort.flexiarg`

`~/code/futon3/library/liberation/noble/right-mindfulness.flexiarg`

`~/code/futon3/library/liberation/noble/right-concentration.flexiarg`

Load-bearing use in this mission:

- grounds the claim that the futonic 8-phase loop is not an ornamental
  Buddhist gloss but already has a local formalisation in the library
- supplies the strongest prior writeup for the mission-lifecycle / eightfold
  correspondence previously surfaced through `M-weird-modernism.md`
- makes the `Pilot's Cycle` problem sharper: the 4-card frame is not the whole
  path, but a compressed operational face laid over an 8-factor discipline
- explains why `right view`, `right effort`, and `right mindfulness` are
  structurally special: they "run and circle around" the other factors, which
  matches the need for a deck that stays inspectable and revisitable rather
  than firing once
- gives ARGUE a better account of what the mockup is trying to preserve:
  visible turns for the inhabitant, backed by a deeper faculty-grammar in
  which seeing, directing, abstaining, striving, monitoring, and immersion are
  path-functions rather than decorative labels

This is the foundational donor family for the mission's path / lifecycle
argument.

#### `liberation/mundane/right-*`

The mundane counterparts should stay in view as contrast material rather than
the primary argumentative home.

Load-bearing use in this mission:

- help explain why the visible pilot deck may need practical, user-facing
  prompts instead of exposing the noble vocabulary directly
- preserve the distinction between a card that instructs an operator and a
  deeper claim about faculties operating cleanly
- provide a fallback language for ARGUE when the noble tier would otherwise
  read as overcompressed or overclaimed

These are secondary to the noble patterns, but important for tone and
translatability.

### 8.2 Stack-structure and pilot-discipline donors

#### `futon-theory/four-types`

`~/code/futon3/library/futon-theory/four-types.flexiarg`

Load-bearing use in this mission:

- grounds the claim that `pheno / geno / exo / xeno` are distinct but
  interrelated observational lenses rather than arbitrary labels
- supports the mission-specific manifestation map
  (`pheno = code`, `geno = AIF+WM`, `exo = VSATARCS`, `xeno = outward-facing
  pilot artefact`) as a **typed projection**, not a poetic analogy
- gives ARGUE its best answer to "why four depths at all?"

This is the foundational donor for the mission's level-structure argument.

#### `futon-theory/agent-contract`

`~/code/futon3/library/futon-theory/agent-contract.flexiarg`

Load-bearing use in this mission:

- grounds the claim that the pilot is not just a theme or mascot but an
  accountable inhabitant that clocks in, observes before acting, attributes
  changes, respects invariants, and leaves a proof trail
- supports the `Pilot's Cycle` claim that the visible 4-card frame must remain
  operationally honest rather than becoming decorative interaction design
- gives ARGUE a way to tie `Print / Propose` and `Loop / Log` back to actual
  evidence obligations

This is the foundational donor for the mission's pilot-specific discipline.

#### `peripherals/read-only-first-then-extend`

`~/code/futon3/library/peripherals/read-only-first-then-extend.flexiarg`

Load-bearing use in this mission:

- grounds the Phase-3 exemplar scene already chosen as the prototype
- supports the claim that the pilot's appearance is staged, not all-at-once:
  observer -> pair -> collaborate is an epistemic order, not just a product
  roadmap
- gives the 4-card frame a strong candidate historical substrate: the mockup is
  not floating free of the War Machine pilot; it is narrating the `extend`
  phase after observation has already done its work

This is the foundational donor for the mission's temporal / staged argument.

#### `peripherals/surface-earns-inhabitation`

`~/code/futon3/library/peripherals/surface-earns-inhabitation.flexiarg`

Load-bearing use in this mission:

- grounds the claim that the storyboard / deck / REPL rendering must be less
  friction than the primitive alternative or it is dead infrastructure
- supports treating the 4-card frame as an **operational stack frame** rather
  than merely explanatory chrome
- gives ARGUE a criterion for judging the prototype: if `scene_04` plus the
  REPL renderer is not easier to inhabit than by-hand articulation, the form
  has not yet earned its place

This is the foundational donor for the mission's inhabitability argument.

#### `peripherals/inhabitation-feeds-evolution`

`~/code/futon3/library/peripherals/inhabitation-feeds-evolution.flexiarg`

Load-bearing use in this mission:

- grounds the claim that using the storyboard is itself how the storyboard gets
  better
- supports the planned feedback relation between ARGUE and DERIVE: argument does
  not merely justify the prototype after the fact; it points at where the next
  prototype iteration should sharpen
- ties the pilot-appearance mission back to the Baldwin-loop logic already live
  in the wider stack

This is the foundational donor for the mission's evolution-through-use
argument.

#### `t4r/*`

`~/code/futon3/library/t4r/rationale.flexiarg`

`~/code/futon3/library/t4r/dtp-fit.flexiarg`

`~/code/futon3/library/t4r/training.flexiarg`

`~/code/futon3/library/t4r/legacy.flexiarg`

`~/code/futon3/library/t4r/supplement.flexiarg`

Load-bearing use in this mission:

- grounds a prior in-stack discourse where `Read / Evaluate / Print / Loop`
  is already treated as capability-development infrastructure rather than only
  as interface choreography
- connects the pilot deck to a stronger developmental claim: the visible
  4-card frame can be argued as a compact training / inhabitation apparatus
  that helps an operator or successor inhabitant learn how to read, judge,
  articulate, and continue a stack situation
- supports the claim that the prototype is not only a renderer but a small
  learning system: the user engages the deck, leaves inspectable artefacts,
  and becomes more capable through repeated turns
- provides language for collective rather than purely individual operation:
  "distributed research operating system", "portable connective tissue",
  "agents of collective intelligence", which fits the handoff / succession
  nature of the pilot problem
- strengthens the bridge from `Pilot Contract` to `Inhabitation Log`: the log
  is not just archival residue but part of the capability substrate by which
  later inhabitants inherit and extend the method

This is the strongest donor family for the mission's capability-development
and succession argument.

### 8.3 Secondary donors

These look useful but should probably stay secondary in ARGUE unless the writing
proves otherwise:

- `~/code/futon3/library/aif/structured-observation-vector.flexiarg`
  because the 4-card frame may want to be defended as a structured observation
  surface rather than only a prompt set
- `~/code/futon3/library/realtime/learn-as-you-go.flexiarg`
  because the pilot-cycle may turn out to be one specific four-channel learning
  loop
- `~/code/futon3/library/enrichment/rational-reconstruction.flexiarg`
  because the mission is reconstructing a live prototype from note-material and
  practice, not designing purely top-down
- `~/code/futon3/library/t4r/exec-summary.flexiarg`
  because it may help with compressed exposition once the main argument is
  written, but does not add much beyond the more specific `t4r/*` donors above

### 8.4 Provisional ARGUE thesis

The current best thesis for ARGUE is:

> The pilot's appearance should be argued as a four-depth, four-turn,
> pattern-backed apparatus whose visible simplicity is backed by a fuller
> eightfold discipline. The `liberation/noble/right-*` family explains why a
> futonic mission can legitimately be treated as an 8-factor loop and why a
> compressed `Pilot's Cycle` can still be answerable to that deeper grammar;
> `four-types` explains why the appearance has four observational depths;
> `agent-contract` explains why the pilot is a real inhabitant rather than a
> metaphor; `read-only-first-then-extend` explains why the exemplar scene is
> staged around the Phase-3 landing; `t4r/*` explains why the 4-card frame can
> be treated as capability-development infrastructure rather than only a UI
> convenience; and `surface-earns-inhabitation` plus
> `inhabitation-feeds-evolution` explain why the storyboard prototype must
> itself be inhabitable and evolve through use.

### 8.5 ARGUE output target

The next artefact should not yet be more code. It should be a short argument
section or companion note that:

- states the thesis above in plain language
- cites the concrete flexiarg donors explicitly
- names the `liberation/noble/right-*` family as the prior in-stack writeup of
  the eightfold-path logic, rather than relying only on note-memory or the
  mission-lifecycle table
- uses `t4r/*` to explain why the deck should be read as a compact training /
  capability-development surface, especially in relation to handoff and
  successor inhabitation
- explains the 4-card `Pilot's Cycle` as a hybrid:
  Contract-shape + REPL turn-logic + four-foundations attention grammar +
  `R1-R12` audit backing
- names what the current `scene_04` mockup proves and what it still leaves open

This then becomes the substrate against which the next DERIVE iteration can be
judged, instead of refining the prototype in a vacuum.

This pass now supplies both:

- a plain-text argument draft immediately below
- a mission-local AIF+ companion at
  `pilot-handoff/pilot-appearance-argument.aif.edn`

### 8.6 Plain-text argument draft

The pilot's appearance should not be argued as a fifth stack level and not as
decorative user-interface chrome. It is the outward articulation of an already
given four-depth stack. At `pheno` depth there is concrete code; at `geno`
depth there is the AIF layer plus the War Machine; at `exo` depth there is
VSATARCS as readable scene-form and survey surface; at `xeno` depth there is
the pilot-facing artefact by which one inhabitant presents the stack to another
inhabitant and, when needed, to the outside world. The task of appearance is
therefore not to invent a new ontology but to let an operator move across these
depths without confusing them.

The stack already has a prior writeup for the stronger practice claim.
`liberation/noble/right-*` formalises the eightfold-path logic as a family of
faculties and disciplines rather than as a loose metaphor. That matters here
because a futonic mission is already intelligible as an 8-factor loop. The
proposed 4-card `Pilot's Cycle` does not replace that loop; it compresses it
into a visible operational face. `Read / Evaluate / Print / Loop` is the most
useful structural spine because it names what a pilot actually does turn by
turn, while `kāya / vedanā / citta / dhammā` and `R1-R12` keep those visible
turns answerable to a deeper attentional and validation grammar. The deck is
therefore justified only if each simple visible move stays backed by a more
serious discipline.

`talk4real` sharpens why that compression is worth making. In that prior art,
`REPL` is not merely an interaction metaphor; it is capability-development
infrastructure. That is exactly the missing bridge here. The pilot problem is a
succession problem: one inhabitant must leave a trace another inhabitant can
read, judge, extend, and contest. On that reading, the old `Pilot Contract`
slot and the new `Inhabitation Log` slot are not minor card swaps. They are
the local place where the stack teaches later inhabitants how to inhabit it.
The four-card frame therefore belongs on the side of training, handoff, and
collective intelligence, not on the side of pitch collateral.

`agent-contract` and `read-only-first-then-extend` keep the argument honest.
The pilot is not a mascot but an accountable inhabitant who observes before
acting, attributes changes, respects consent and boundary gates, and leaves an
inspectable proof trail. Phase 3 is the right exemplar because it already
presupposes a read-only landing and asks how extension becomes legitimate and
legible. The current `scene_04` mockup demonstrates that this can be rendered
as an operational stack frame: a scene trace plus a 4-card cycle that can step
the protocols and the REPL printer together. What it does not yet prove is
that the chosen card labels are final, or that this surface has already earned
routine use.

That final criterion comes from `surface-earns-inhabitation` and
`inhabitation-feeds-evolution`. The pilot deck is justified only if it is
easier and truer than ad hoc articulation, and only if repeated use improves
the next version of the deck. So the present prototype should be read as an
operational hypothesis. If later inhabitants can use it to produce cleaner
logs, sharper judgments, more explicit consent gates, and more continuous
succession across handoffs, then the argument holds. If not, the form must be
revised rather than defended.

### 8.7 Practical review after `E-street-sweeper` / `E-night-shift`

The session's practical excursions sharpen the 4-turn cycle beyond the earlier
mockup. The cycle is no longer only a card vocabulary. It is beginning to show
up as a real capability-routing discipline inside the pilot substrate.

The first clarification is that **`Read` is already more concrete than the
prototype made it sound**. In practice, the pilot's read phase is not generic
"input"; it is the War Machine / AIF sensing layer:

- ranked actions
- stop-the-line and metabolic-balance signals
- open substrate threads
- current inhabitation-log state
- VSATARCS drift or completeness gaps

So the pilot's `Read` phase is increasingly well-grounded at the mission's
`geno` layer. The operator's observation is right: we have not built a
dedicated `Read` excursion because much of that work is already what the
AIF-bearing War Machine substrate is for.

The second clarification is that **`Evaluate` is not just interior judgement;
it is capability classification and envelope choice**. The key practical move
in this session was repeatedly asking not only "what is wrong?" but "what
class of action does this require, and therefore what peripheral is
legitimate?" The excursions operationalise exactly that:

- `E-street-sweeper` classifies a problem as working-tree hygiene pressure
- `E-night-shift` classifies a problem as code-modification / branch-isolated
  implementation work

This means the pilot's `Evaluate` turn should now be understood as including a
**hop judgement**: given the current reading, which constrained capability
envelope is the truthful next apparatus?

The third clarification is that **`Print` is more operational than the word
alone suggests**. In this session, the pilot did not merely "print" a reading
as prose. It emitted typed external artefacts:

- excursion notes
- hop decisions
- branch names
- frame/worktree receipts
- commit / PR deliverables
- VSATARCS bilateral-evidence entries
- bells back to sibling inhabitants

So `Print / Propose` should be argued not only as "state a reading" but as
"emit the next inspectable artefact by which the stack can coordinate action".
On this reading, `E-night-shift` is especially close to the `Print` turn,
because its terminal deliverable is a branch + PR rather than a private
decision.

The fourth clarification is that **`Loop / Log` remains the least-complete turn
in the current prototype, but its shape is now easier to name**. It does not
only mean "repeat". It means:

- re-read pressure after action
- update or seed the inhabitation log
- feed new evidence into VSATARCS
- note what remains open
- support successor or sibling inhabitants through a cleaner handoff
- perform a reflection round in the `talk4real` sense, so capability is
  increased rather than merely spent

This is where the reflection suggestion becomes load-bearing. Without a real
reflection / recap move, the pilot risks becoming a task router that emits
artefacts without metabolising what it learned.

One new systems consequence follows from the pair of excursions: the
peripherals are **not independent islands**. `E-street-sweeper` and
`E-night-shift` will need some mutual intelligibility. In particular,
street-sweeper should not treat night-shift's isolated frames, worktrees, and
feature branches as undifferentiated dirt. The pilot stack therefore likely
needs a shared substrate notion of:

- active frame / workspace roots
- branch ownership and branch purpose
- which dirty paths are "pressure to be swept" versus "live shift state"

That looks less like a new pilot level and more like a cross-turn coherence
requirement between `Evaluate`, `Print`, and `Loop`.

Practical consequence for the mission argument:

- `Read` is increasingly identified with WM/AIF signal intake
- `Evaluate` is increasingly identified with capability classification and
  hop/peripheral choice
- `Print` is increasingly identified with typed artefact emission
  (`bell`, `log`, `VSATARCS`, `branch`, `PR`, `excursion note`)
- `Loop` is increasingly identified with reflection, substrate update, and
  cross-inhabitant continuity

This does not invalidate the earlier composite alignment
(`Contract + REPL + four foundations + R1-R12`). It strengthens it by giving
each visible turn a more operational substrate witness.

### 8.8 Machine-readable companion

A mission-local AIF+ companion now lives at
`pilot-handoff/pilot-appearance-argument.aif.edn`.

Its job is not to replace the prose above, but to expose the argument in a
shape that can be inspected by the futon5 AIF+ / AIF² machinery:

- thesis node: four-depth, four-turn, capability-developing pilot apparatus
- support nodes: four-depth typing, eightfold-discipline compression,
  succession/training claim, Phase-3 prototype witness, inhabitability
  criterion
- conflict nodes: decorative-compression risk, provisional-card-language risk
- falsifiability node: repeated use fails to improve handoff quality

This gives the mission a better validation path than prose-only ARGUE: the
claims, attacks, and refutation condition are now explicit enough to test,
tighten, or remap as DERIVE iterates.

### 8.9 Caveat — AIF covers the whole cycle, not just Read

Joe (emacs-repl 2026-05-25):

> *"Although the mapping `Read → AIF` is accurate 'enough', in fact AIF
> covers the whole cycle.  Right now we have thought of WM as a first-order
> 'reader' of FUTON but in fact it is both a reader *and* 'writer' of futon
> (insofar as it has the affordance of setting up 'hops' into night-shift)."*

The four-step REPL alignment in §8.7 articulates **one operator-visible
slicing** of the apparatus' cycle into discrete turns.  AIF is the **larger
frame** that produces those turns: observation → belief update → policy /
action selection → action → re-observation.  Each AIF cycle position can
participate in both reading and writing; the REPL decomposition keeps
those slices nameable for operator narrative, but it doesn't carve the
underlying AIF into mutually exclusive Read / Eval / Print / Loop layers.

**Specific consequence for the WM/pilot apparatus:**

- **WM is both reader AND writer of futon**, not a first-order reader.
- WM's *reader-side* affordances are the visible ones: observation channels,
  mode classification, agenda surfacing, the patterns view, the
  Inhabitation Log card, etc.
- WM's *writer-side* affordances are also real: setting up the **hops** into
  `:night-shift`, `:street-sweeper`, and successor peripherals.  WM doesn't
  perform the writes itself (WM-I4 holds: the WM-as-observer-engine
  doesn't act) — but WM-as-affordance-surface enables and routes the writes
  that inhabiting agents then execute.
- The pilot inhabits this **dual affordance**.  Its READ is AIF; its EVAL
  is the hop-into-peripheral that WM staged; its PRINT is the typed artefact
  the peripheral emits; its LOOP feeds substrate update back into WM's
  reader-side.  All four steps are inside one AIF cycle.

**Generalisation for sibling apparatus:** any AIF cycle position can both
READ (sense / belief-update) and WRITE (via action selection that
propagates through subordinate machinery).  The REPL family's four-step
decomposition is useful for naming and routing; the AIF frame is what
produces it.  When designing new excursions / peripherals, the
read/write affordance should be considered separately from the
operator-facing REPL slice they implement.

#### Scope correction after liberation-source recovery

The path-pairing material looks most immediately useful for:

- the **teaching / traversal mechanic** of the storyboard
- cross-cutting scenes like the third-jhāna interlude or recap / loop scenes
- explaining why a four-part play-loop can be faithful to an eight-phase
  methodology

It looks less immediately useful as a direct source for the stack-facing
`code / AIF+WM / VSATARCS / outward artefact` depth map. That map still needs
to be argued at the level of stack entities and interfaces, not only by appeal
to contemplative structure.

What changes after locating the `liberation/` family is not the boundary above
but the confidence of the path-side argument: the mission no longer has to
gesture vaguely at an eightfold background, because the background is already
patterned in the library.

## 8. Engine metadata

- **Predecessor mission(s):** `M-war-machine-pilot.md` (capability), `M-interim-director.md` (commercial frame)
- **Sibling mission(s):** `M-signal-roll-up.md`, `M-daily-scan-multi-axis-queue.md` (both also addressing operator-on-Hyperreal instrumentation)
- **Authored from session:** 2026-05-25 (Jim Morrison ↔ Jim Henson ↔ Adult Swim ↔ virtual attractors ↔ third jhāna ↔ Markov boundary ↔ homoiconic artefact ↔ HtDP prototype)
- **Artefacts produced in IDENTIFY:** the three skeleton files; this mission document; the scene-list annex
- **Awaiting:** next ARGUE/DERIVE iteration, especially whether the 4-turn mockup should grow explicit substrate support for capability classification, peripheral-hop choice, shared workspace/frame awareness, and reflection / loop logging across `:street-sweeper`, `:night-shift`, and successor excursions
