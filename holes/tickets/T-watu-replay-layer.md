# T-watu-replay-layer — the click-through half needs a review, not a link

Raised by joe + claude-13, 2026-08-24. Fourth in the series with
`T-forward-model-vs-active-work.md`, `T-exogenous-evidence-update-rule.md`,
`T-devmap-forward-model-calibration.md`.

**Status: SCOPED, NOT STARTED.**

## The correction this ticket exists to record

The WIP board's draw pile was described as the WATU half. It is not. Joe,
2026-08-24: *"those are pending items, but the WATU half would need a big review
of e.g. superpod work etc."*

- `holes/tickets/` is the **queue** — what is pending. That is still Bentley
  Priory: current state, on the table, readable at a glance.
- **WATU is the past** — what ran, what happened, and why. A different room with
  different staff, because it is a different job.

From futon-2026 §1: *"Bentley Priory is a model to see now… WATU is a model to
learn how: a replay instrument, restricted by construction, in which a
hypothesis about the world can be tested before it costs anything. Neither
substitutes for the other."*

## The design constraint, which is the whole difficulty

WATU was **deliberately kept poorer in information than reality**: players were
screened so each saw only what the officer they represented could have seen.

So the WATU layer is **not a log dump**. Piping every superpod run into a page
would be the opposite of the instrument — display instead of interrogation. What
it must support is: replay an action that was lost, infer from the wreckage what
must have been happening, and test a counter-move against the reconstruction
before anyone pays for it.

That implies the hard rule, which is the same one Part III imposes on its
corpus: **record what was knowable at decision time**, not what we know now.
Hindsight-encoded history teaches nothing and reads as competence.

## The corpus that exists

Cascade rung `m22` "structure mining" (`M-distributed-frontiermath +
M-superpod-mark2/3`) is the anchor. Material already written, none of it indexed
or reachable from the board:

| artefact | shape |
|---|---|
| `futon6/SUPERPOD-RERUN-NOTES.md` | already WATU-shaped: "What Changed and Why", with a measured failure (9.6% parse success on local Llama-3-8B; model wrote prose then truncated JSON) and the counter-move |
| `futon6/technote-superpod-mining-yields.md` | yields, 187 lines |
| `futon6/superpod-1a-technote.md`, `README-superpod.md`, `HANDOFF-superpod.md` | context |
| `futon6/holes/mark7-superpod-run-playbook.md` | the procedure |
| `futon6/holes/superpod-dag-contract.md`, `superpod-one-shot-plan.md`, `warp-superpod-parallel-runner.md` | design |
| `futon6/holes/pre-superpod-pipeline-readiness.html` | a readiness board — one of the two artefacts Joe named as the form to aim at |
| `storage/superpod-{math,mo}-processed.tar.gz` | outputs |

**Hazard:** every one of these also exists under `futon6-old-copy/`. Establish
which tree is authoritative BEFORE reviewing, or the review grades the wrong
history.

## Why this is expensive, and should not be pretended otherwise

This is a **research task, not a rendering task**. Someone has to read the run
history and reconstruct what was known at each decision. That is why Uxbridge
and WATU were separate rooms: the live picture is cheap to maintain and the
replay instrument is not. Any plan that treats the WATU layer as "add a link"
has mistaken the expensive half for the cheap one.

`T-devmap-forward-model-calibration.md` is already a WATU exercise in miniature
— replay a frozen forward model, grade against outcomes, blind to hindsight. Its
rubric is the template to reuse here rather than inventing a second one.

## Cheap thing available now (do this first, it is small)

Add a `watu` field to the WIP cards, null today, exactly as `promotion_test` is
null. The board then reports two countable absences per card: *nobody has said
what shipped would look like*, and *nobody has reconstructed what happened*. Both
are findings; hiding either makes the board prettier and less true.

## Related

- `T-forward-model-vs-active-work.md` — the board this attaches to.
- `T-devmap-forward-model-calibration.md` — the rubric template.
- Paper: `p4ng/sec-observation-futon.tex` (the two rooms),
  `p4ng/sec-operator.tex` (registers, Figure 5).
