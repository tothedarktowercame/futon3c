# Excursion: Campaign-Spec Grounding (IF/HOWEVER/THEN per `structure/what-problem-is-this-actually-solving`)

**Type:** Excursion (E-prefix; bounded scope-out from M-action-cost-modelling VERIFY T10).
**Status:** SCOPED for handoff to codex-5 via Agency bell.
**Date:** 2026-05-26
**Author:** claude-1 (scoping); end-to-end owner: codex-5 (authoring per the bell).
**Parent mission:** `futon3c/holes/missions/M-action-cost-modelling.md` §3.5 + §5.1 T10.

## Why this exists

M-action-cost-modelling §3.5 introduced **Campaign** as a candidate new structural element (one tier above Mission, for months-scale efforts comprising multiple Missions). The shape-first IDENTIFY test passed with 3 plausible instances (`Campaign-R3-honesty`, `M-the-futon-stack` retroactively, `Campaign-substrate-completion`), which cleared the namespace-adoption threshold.

But §3.5 names the Campaign concept without **arguing why Mission-shape can't absorb the 3 instances** — i.e. without grounding the introduction in `structure/what-problem-is-this-actually-solving`'s IF/HOWEVER/THEN discipline. VERIFY T10 named this as a **pre-commit gate**: Campaign-spec must be re-authored with proper pattern-grounded argument BEFORE INSTANTIATE commits any Campaign-scaffolded work.

This excursion lands that re-authoring as a discrete artefact, so M-action-cost-modelling §3.5 can be replaced (or supplemented) with the grounded version + cross-reference.

## Scope (codex-5's task)

### (1) Read the pattern

`futon3/library/structure/what-problem-is-this-actually-solving.flexiarg` — Hinge-pool question protecting against proxy-problem substitution. The pattern's `:claim / :ground / :warrant / :backing` structure should shape the Campaign-spec re-authoring.

### (2) Re-author M-action-cost-modelling §3.5 Campaign-spec with explicit IF/HOWEVER/THEN

Required structure:

- **IF**: when does a candidate piece of work need a Campaign-shaped container rather than a Mission?
- **HOWEVER**: what failure modes does NOT having a Campaign produce? (work that spreads across too many missions; coordination overhead that no single mission owns; lost trail when constituent missions complete piecemeal)
- **THEN**: define Campaign with: name, constituent Missions, coordination shape (sequential / parallel / convergent), joint completion criterion. Lifecycle: HEAD → IDENTIFY → MAP layer at the Campaign level; constituent Missions follow their own lifecycles independently; Campaign §COMPLETE fires when joint completion criterion is met.
- **BECAUSE**: argument grounded in `structure/what-problem-is-this-actually-solving` — without Campaign-shape, multi-mission work is mis-shaped either as "a Mission that grew too big" (loses Mission discipline) or as "a tangle of Missions without organising frame" (proxy-problem substitution; each Mission appears successful while the joint goal slips).

### (3) Re-verify the 3 worked-example instances

For each: produce explicit IF/HOWEVER/THEN justifying Campaign-shape over absorbed-Mission-shape:

- `Campaign-R3-honesty` (M-INC step (b) + R3d-rewiring + multi-channel verification)
- `Campaign-the-futon-stack` (the existing `M-the-futon-stack-Q1..Q7` cluster retroactively)
- `Campaign-substrate-completion` (substrate-1 → substrate-2 → substrate-3 progression)

If any of the 3 fails the IF/HOWEVER/THEN test (i.e. could be absorbed back into Mission-shape without loss), record that explicitly. shape-first IDENTIFY allows `:special-case true` outcomes; Campaign-as-shape is real only if at least 2 of 3 pass.

### (4) Output

A new section to be inserted into M-action-cost-modelling.md replacing or supplementing §3.5. Two acceptable shapes:

- **Option A (in-place replacement)**: write the grounded §3.5 directly as a doc-edit to M-action-cost-modelling.md.
- **Option B (sibling artefact)**: author this excursion file fully (E-campaign-spec-grounding.md) with the grounded spec inside; M-action-cost-modelling.md §3.5 becomes a one-paragraph cross-reference pointing here.

Codex-5 picks A or B; both are defensible. B keeps M-action-cost-modelling lean; A keeps the spec where readers will look first.

## Constraints

- Per `[[feedback_we_do_discipline]]`: do not claim Campaign is "validated" or "real" — claim only that the 3 instances pass shape-first IDENTIFY's protocol per `mission-lifecycle.md` §"shape-first IDENTIFY".
- Per `[[feedback_argue_strategic_verify_operational]]`: stay in spec / IF-HOWEVER-THEN territory; don't pre-load operational hooks for Campaign management (that's separate, future-Joe work).
- Per `[[project_e_prefix_excursions]]`: end-to-end owner is codex-5; sub-handoff requires operator-authority.

## Success criteria

- IF/HOWEVER/THEN/BECAUSE structure present for the Campaign-shape introduction
- 3 worked-example instances each get explicit Campaign-vs-Mission justification
- Pattern `structure/what-problem-is-this-actually-solving` cited with at least the `:claim` quoted accurately
- Output option (A or B) clearly named in the closing notes
- VERIFY T10's pre-commit gate is closeable: a human reading the new spec can answer "is this Campaign-shape thing real?" with evidence rather than handwaving

## Closure marker

When done, codex-5 should reply via the standard whistle-completion mechanism with:
- `DONE: <file-path>` where the spec landed (either M-action-cost-modelling.md §3.5 or E-campaign-spec-grounding.md)
- A one-line summary of the option taken (A or B)

## Cross-references

- M-action-cost-modelling.md §3.5 (current Campaign-spec; replacement target)
- M-action-cost-modelling.md §5.1 T10 (the pre-commit gate VERIFY entry)
- `structure/what-problem-is-this-actually-solving.flexiarg` (the grounding pattern)
- `mission-lifecycle.md` §"shape-first IDENTIFY" (the protocol Campaign passed earlier)
- `[[feedback_argue_strategic_verify_operational]]`
- `[[feedback_we_do_discipline]]`

## Provenance

- Scoped: claude-1, 2026-05-26 (emacs-repl, paired with Joe)
- Handoff: Agency bell to codex-5 (job-id recorded in the bell-emission)
