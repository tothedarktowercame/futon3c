# Excursion: E-ticks-firing-ratio-likelihood

**Date:** 2026-05-30
**Status:** IDENTIFY
**Move:** DECOMPOSE, not discharge
**Authored-by:** codex-2 as War Machine pilot, run `live-a138864a-c398-427f-9ebd-6b0eab1b253d`, cg `cg-bf2f9b00-f3a3-47cc-9859-e3c7c0e68671`
**Parent sorry:** `sorry/r3a-likelihood-ticks-firing-ratio`

## Why This Exists

The WM ranked `sorry/r3a-likelihood-ticks-firing-ratio` at top-1 after the
coupling-density discharge. The apparent twin pattern does not currently hold.
Coupling-density had a clean bridge:

- Observation data: repo-level temporal-coupling edges `{:from :to :strength}`.
- Belief entities: stack-annotation sections.
- Bridge: each section has provenance/source path; source path derives a repo.

Ticks-firing-ratio does not yet have the analogous bridge. The logic model
defines four pocketwatch tick ids under `:pocketwatch :ticks`:

- `:hermit-warning`
- `:hobby-warning`
- `:foraging-warning`
- `:cargo-warning`

`futon2.report.war-machine/scan-graph` evaluates those tick ids into
`[:graph :dynamics :ticks]` records carrying `:id`, `:fired?`, `:observed`,
`:proxy-note`, and `:computed?`. Neither the definitions nor the evaluated
results name stack-annotation section ids, entity ids, or a stable tag that can
select a belief cohort.

## Rule-Out

A bounded coupling-style discharge would require one of these to exist:

1. Tick definitions carry explicit entity ids.
2. Evaluated tick results carry explicit entity ids.
3. Stack annotations contain tick ids or tick roles.
4. A canonical vocabulary maps each tick id to entity tags.

Current files checked:

- `futon5a/data/stack-logic-model.edn`
- `futon5a/holes/stack-annotations.edn`
- `futon2/report/war_machine.clj` tick evaluator
- `futon5a/data/war-machine-strategic-vocabulary.edn`

The only stable tick identifiers are the four pocketwatch warning ids. They
describe constraints over workstream ratios, evidence topics, and file existence;
they are not entity-typed. Mapping them directly to arbitrary repos or sections
would be a proxy, not a belief-derived likelihood model.

## Needed Bridge

Add a first-class tick/entity bridge, parallel to
`classify-entity-tags-from-stack-annotations` and
`classify-entity-repos-from-stack-annotations`.

Candidate shape:

```clojure
{:tick-id :hermit-warning
 :entity-tags #{:tick/hermit-warning :workstream/stack :workstream/consulting}
 :source [:logic-model :pocketwatch :ticks]}
```

Then implement:

1. `classify-entity-ticks-from-stack-annotations` or a sibling reader for a
   canonical tick-to-entity map.
2. `predict-ticks-firing-ratio`, filtering belief to entities tagged by tick id
   or tick/workstream role.
3. Add `:ticks-firing-ratio` to `channels-with-likelihood`.
4. Thread the tick/entity bridge through `judge` the same way coupling-density
   now threads entity-repos + coupling-edges.
5. Tests proving that untyped entities do not affect the tick cohort.

## Success Criteria

- A canonical tick-to-entity mapping exists outside the predictor.
- The predictor does not infer tick membership from ad hoc string matching on
  warning prose.
- `:ticks-firing-ratio` appears in `channels-with-likelihood`.
- WM tick refresh rotates off `sorry/r3a-likelihood-ticks-firing-ratio`.

## Non-Goal

Do not hardcode the four warning ids directly inside the predictor as entity
selectors. That would silence the sorry without creating the missing substrate.
