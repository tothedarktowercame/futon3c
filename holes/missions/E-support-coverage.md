# Excursion: R3a Support-Coverage Likelihood (entity-tag classification in the bootstrap)

**Type:** Excursion (E-prefix; bounded scope-out from a mission; multi-cycle pilot work; see [[project_e_prefix_excursions]] for the convention).
**Status:** SCOPED — multi-cycle plan; first cycle to start as a regular operator-paced inhabitation (NOT a candidate for `[[project_claude_autorunner]]` v1 because the design choices in cycle 1 need operator judgement).
**Date:** 2026-05-26
**Author + end-to-end owner:** claude-1 (`:war-machine-pilot` peripheral, emacs-repl surface paired with Joe).
**Parent mission:** `futon3c/holes/missions/M-war-machine-pilot.md` (this is the live recommendation that surfaced after the 6-sorry R3a reclassification cycle on `cg-18b7831b`).
**Trigger:** WM live next-move-live ranked-actions[0] post-reclassification: `address-sorry sorry/r3a-likelihood-support-coverage` (G=-4.426).  Joe's emacs-repl directive 2026-05-26: "write up the task that we discussed — 'support-coverage genuinely needs entity-tag classification in the bootstrap — that's substantive substrate work, not a one-cycle pilot move' — as a new E-support-coverage excursion."

## Why this exists

After the 6-sorry reclassification cycle (`cg-18b7831b`, 2026-05-26), the WM's live ranked-actions surfaced `sorry/r3a-likelihood-support-coverage` as top-1.  Unlike the 6 sorries just reclassified to `:n-a-by-design`, this one is genuinely belief-derivable — its rationale says so:

> "S1-S5 support claim coverage [0,1] from holistic argument.  **Could derive from belief mass on entities tagged as supporting evidence; needs entity-tag classification in the bootstrap.**"

The sibling sorry `:sorry/r3a-likelihood-attack-coverage` has the SAME structural prerequisite:

> "A1-A4 attack claim coverage [0,1] from holistic argument.  **Sibling to :support-coverage — same entity-tag prerequisite.**"

So this excursion bookkeeps two channels' worth of R3a coverage AND lands the substrate (entity-tag classification at bootstrap) that several future channels will likely also need.  Closing this advances R3 coverage from 4-of-14 toward 6-of-14 in honest belief-derivation terms.

## The substrate gap

The four existing R3a likelihoods (`:annotation-health`, `:sorry-count-norm`, `:mission-health`, `:active-repo-ratio`) all operate over `belief` — a map of entity-id → posterior over the discrete status set.  None of them needs the entity TYPED beyond its presence in the belief map.

Support-coverage is different: it needs to count belief mass NOT over all entities, but over the SUBSET of entities tagged as supporting-evidence (S1-S5 in holistic-argument vocabulary).  Without an entity-tag classification scheme, `predict-support-coverage` has nothing to filter on.

The same gap applies to `:attack-coverage` (filter on attacking-evidence entities, A1-A4).

## Scope (multi-cycle subplan)

### Cycle 1 — Design entity-tag schema + identify S1-S5/A1-A4 entities

**RESOLVED UP-FRONT 2026-05-26 (operator-paired emacs-repl).** Decision: option **(b)** parallel `entity-tags` map `entity-id → #{:tag-set}` that lives alongside `belief`.

Reasoning (operator-approved):
- Entity-tags are STATIC — derived at bootstrap from the holistic-argument graph and never updated per-tick.  No drift surface; the "two structures to keep in sync" theoretical concern collapses because tags are read-only after bootstrap.
- Option (a) would have required refactoring all 4 existing predictors to `get-in [id :posterior]`; that's real work optimising for an integrity property already-true-by-construction.
- Trace serialization: (b) keeps the trace file smaller; tags can be reconstructed at any time from bootstrap.
- Tag schema can evolve forward without belief-shape churn.

Outstanding sub-question for Cycle 2 to answer (not blocking): where exactly do S1-S5 + A1-A4 entities live in the futon substrate?  Best candidates from the cached structural prior:
- `futon5a/holes/stories/leaf-argument.aif.edn` — Pillar I (the "holistic argument" pillar) defines S1-S5 + A1-A4 as named claim/attack roles
- Leaf nodes have `:role :S<N>` or `:role :A<N>` or similar role-keyed metadata
- Mapping: bootstrap reads leaf-argument.aif.edn (and possibly sibling leaves), enumerates nodes with role-keyed S/A markers, and emits `entity-tags` keyed on the same entity-id the belief map uses

Cycle 2 confirms the substrate location during the implementation.

### Cycle 2 — Implement tagging at bootstrap

- **Owner**: operator-paced after cycle 1 approval; *candidate for autorunner once schema is fixed* (mechanical work).
- **Outputs**:
  - New fn in `futon2.aif.belief` (or sibling) that classifies entities at bootstrap time and produces the tags map (or extended belief, per cycle 1's choice).
  - Tags applied to live belief used by `judge`.
  - Tests asserting: known S-entities get `:supports-S<N>` tags; known A-entities get `:attacks-A<N>` tags; entities with neither stay untagged.
- **Done when**: tests green; running JVM's belief carries the tag overlay; cycle event logged with `:cooking-time-seconds`.

### Cycle 3 — Implement `predict-support-coverage` + `predict-attack-coverage`

- **Owner**: operator-paced or autorunner; mechanical port of the 4-channel pattern.
- **Outputs**:
  - `predict-support-coverage` in `futon2.aif.belief`: returns `{:mean :variance}` where mean = (sum of healthy-mass on supports-tagged entities) / (count of supports-tagged entities).
  - `predict-attack-coverage` analogous.
  - Added to `channels-with-likelihood`.
  - `predict-observation` returns both.
- **Done when**: tests green; `(predict-observation belief)` includes both new channels.

### Cycle 4 — Verify R3a iteration picks up new channels + live WM shift

- **Owner**: operator-paced.
- **Outputs**:
  - `judge`'s R3 inner-loop computes prediction-errors for the new channels.
  - WM scheduler tick refreshes; live next-move-live's top-N rotates off `sorry/r3a-likelihood-support-coverage` AND `sorry/r3a-likelihood-attack-coverage`.
  - Both sorries flipped from `:status :open` → `:status :addressed` + `:resolution` citing the cg-chain of cycles 1-3.
  - Inhabitation event logged with full live-shift evidence.
- **Done when**: live shift demonstrated; sorries marked addressed; R3 coverage moves from 4-of-14 to 6-of-14 in the docs.

## Cooking-time logging

Per Joe's directive 2026-05-26: log `:cooking-time-seconds` per inhabitation event.  Over many cycles this becomes calibration data for effort estimates.

**Schema addition** (sibling thread, applies to all future inhabitation events):

```clojure
{:id "inhab/..."
 :at "..."
 :event :cycle-complete
 :cooking-time-seconds 221     ; <-- new field
 :cooking-display "3m 41s"     ; <-- new field, human-legible
 ...}
```

Where does the timer come from?  The emacs-repl surface already displays "Cooked for 3m 41s" between turns.  Pilot can read this from the buffer surface at cycle-end (or operator can paste it).  Substrate-side, the wall-clock between `:at` of two adjacent cycle-events for the same pilot is a usable proxy.

**This is itself a Cycle 0** (substrate-schema addition) and can land before or during Cycle 1.

## Success criteria

1. Cycle 1 produces a design doc with explicit operator-decision-request appended to this excursion.
2. Cycle 2 lands entity-tag classification at bootstrap with green tests.
3. Cycle 3 ships both predictors + adds them to `channels-with-likelihood`.
4. Cycle 4 demonstrates live WM shift: both `sorry/r3a-likelihood-support-coverage` and `sorry/r3a-likelihood-attack-coverage` disappear from ranked-actions.
5. R3 coverage advances from 4/14 to 6/14 in `futon2/docs/futon-aif-completeness.md`.
6. Each cycle logs `:cooking-time-seconds` per the schema addition.
7. The remaining 2 :prototyping-forward sorries (`:coupling-density`, `:ticks-firing-ratio`) surface as the new top-2 — proof the WM is correctly distinguishing remaining work.

## What this excursion does NOT own

- **Other :prototyping-forward sorries** (`:coupling-density`, `:ticks-firing-ratio`) — they have different prerequisites (edge-entities, tick-entity-typing).  Separate excursions.
- **Refactoring the four existing R3a likelihoods** — their no-tagging-needed pattern is fine.
- **Multi-channel R3d sign-aggregation refinement** beyond what naturally falls out of adding two new channels.  `:sorry/r3d-per-entity-attribution` is its own sorry; partially-closed per v0.16.
- **Backfilling Cooking-time on past cycle events** — schema addition applies forward only.

## Multi-cycle cadence + autorunner suitability

| Cycle | Autorunner suitable? | Why |
|---|---|---|
| 0 (Cooking-time schema) | YES | Mechanical schema addition + tooling update |
| 1 (Design) | NO | Operator-judgement needed on tag schema choice (a) vs (b) |
| 2 (Implement tagging) | YES once schema fixed | Mechanical; tests assert correctness |
| 3 (Implement predictors) | YES | Pattern port from 4 existing predictors |
| 4 (Verify + reclassify sorries) | YES | Tick + verify + log; checklist work |

If [[project_claude_autorunner]] lands first, cycles 2/3/4 are good candidates for its first real-world use (after the 60-min hard-cap + AUTORUNNER-OFF sentinel mechanics are in place).

## Cross-references

- `futon2/data/sorrys.edn` — `:sorry/r3a-likelihood-support-coverage` + `:sorry/r3a-likelihood-attack-coverage`
- `futon2/src/futon2/aif/belief.clj` — site for `predict-support-coverage` + `predict-attack-coverage`; existing 4 predictors as templates
- `futon2/docs/futon-aif-completeness.md` §R3 — R3 row's "4 of 14 channels" status updates to "6 of 14" after cycle 4
- `futon5a/data/pilot-inhabitations.edn` — where cycle events log
- `futon3c/holes/missions/E-wm-live-recommendation.md` — surface that pointed at this work
- `futon3c/holes/missions/M-war-machine-pilot.md` — parent
- [[project_claude_autorunner]] — would consume this excursion's bounded-mechanical cycles 2-4
- [[project_tickle_as_operator_model]] — full Tickle would deflection-challenge if pilot drifts mid-cycle
- [[feedback_use_playwright_for_ui_verify]] — verify cycle 4's live shift via Playwright

## Provenance

- WM live next-move-live ranked-actions[0] post-`cg-18b7831b`: `address-sorry sorry/r3a-likelihood-support-coverage` (G=-4.426, 2026-05-26T08:31:04Z)
- Joe's directive: "write up the task that we discussed... as a new E-support-coverage excursion.  Probably we would do aspects of it in multiple WM inhabitations so that we get the work logged properly and the queue advanced.  In principle we could start to get better at effort estimates if we logged the Cooking time per inhabitation event." (emacs-repl 2026-05-26)
- This excursion file authored: claude-1, 2026-05-26
