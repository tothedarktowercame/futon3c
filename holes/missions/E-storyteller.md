# Excursion: Storyteller

**Type:** Excursion (E-prefix) — a **follow-on**, not needed for the first
demo. Sibling of `E-cheesemonger.md`. Bounded scope-out owned end-to-end by a
single agent when picked up.

**Status:** SCOPED (vision recorded 2026-05-29; not started)
**Date:** 2026-05-29
**Author:** claude-2 (emacs-repl, paired with Joe)
**Parent:** `M-pilot-appearance.md` (the xeno-layer appearance question) + the REPL LOOP exo-doc-currency requirement (`holes/specs/repl.spec.edn`)
**Surface that authored this:** emacs-repl 2026-05-29 between claude-2 and Joe

## Why this exists (Joe's vision, emacs-repl 2026-05-29)

> "we'd probably want to write E-storyteller.md that would give more elaborate
> guidance on how the WM→VSATARCS interface will work. I can imagine coming in
> to the office in the morning, putting on a Steam Frame, and navigating a new
> set of constellations that tell me the story of what was built overnight,
> deciding on operator-level merges with a hair-trigger VR pointer … that would
> be fun. Not needed for a first demo."

The first-demo home for the WM→VSATARCS update is a single scene in
`futon5a/holes/stories/war-machine-lucid-scenes.md` ("The loop closes for
real") — fine to watch, but flat. E-storyteller is the elaborated interface.

## What it is

The **xeno layer fully realized as an immersive, actionable operator surface.**
An overnight autonomous run emits a sequence of γ-frames (`data/repl-traces/`),
each carrying its turn-trace + `:learning` + the candidate branch it explored
(the integral curve γᵢ). E-storyteller renders that night's run as **navigable
constellations** the operator walks through in VR:

- **Story, not hairball.** The render preserves the *time axis* — it is the
  **stereolithographic FUTON view** (`project_stereolithographic_view`: the
  termite-mound semilattice, not the time-collapsed VSAT hairball), so the
  operator sees *what happened in what order overnight*, scene by scene.
- **Operator-level merges in VR.** Each explored branch is a candidate the
  operator can replay-to-master or not — the **gate-at-merge** consent step
  (`project_consent_gate`), now a hair-trigger VR gesture. Supervised→autonomous
  migration is the same swap as everywhere: replace the operator's VR pointer
  with an autopen at that one location.
- **Reads the LOOP's own output.** Constellations are sourced from the frames +
  the cheesemonger's hole-budget (what was mined vs closed overnight) + the
  fork-warrants (why the loop chose each direction) — so the story explains
  *itself*, not just narrates.

## Layer placement

pheno (code) → geno (AIF/WM) → exo (VSATARCS readable scene-form, kept current
by the LOOP doc-currency requirement) → **xeno (this: the operator inhabits the
exo as an immersive story and acts on it)**. The LOOP keeps the docs *true*;
E-storyteller makes the xeno surface *walkable and decision-bearing*.

## Scope

**In (when picked up):** the constellation render from γ-frames + cheesemonger +
fork-warrants; the VR navigation + merge-gesture binding to the consent-gate;
the stereolithographic time-axis layout.

**Out:** the first demo (the flat lucid scene suffices); the LOOP doc-currency
*regen* itself (that's the spec's LOOP requirement, separate); building the
autonomous overnight runner (separate). E-storyteller assumes those exist and
makes their output inhabitable.

## Cross-references

- `futon3c/holes/specs/repl.spec.edn` — LOOP `:autonomy` exo-doc-currency (this is its xeno elaboration)
- `futon3c/holes/missions/E-cheesemonger.md` — the hole-budget the story surfaces
- `futon3c/README-pilot.md` — the role this surface is for
- `futon5a/holes/stories/war-machine-lucid-scenes.md` — the first-demo seed scene
- `project_stereolithographic_view`, `project_stack_geometry_anthology` — the VSATARCS-native viewer + time-axis-preserving render this builds on
- `project_consent_gate` — the gate-at-merge, here a VR gesture

## Provenance

Coined by Joe (emacs-repl, 2026-05-29) while closing M-pilot-appearance, as the
fun-but-not-yet elaboration of the WM→VSATARCS interface: walk into the office,
Steam Frame on, navigate the overnight story, merge with a flick.
