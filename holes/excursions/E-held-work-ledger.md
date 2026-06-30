# E-held-work-ledger — the durable, queryable held/deferred-work ledger

**Status:** IDENTIFY → POC (2026-06-30). **Owner:** claude-8 (end-to-end).
**Member of:** C-cascade-real — the **held/deferred-work ledger** facet (Clause-1
standards 2 *grounded* + 3 *durable/reconstitutable*; the "no sheet of paper" test).

## The gap (evidence-first)

Held/deferred work is **folklore**: it lives as prose in **~445 `holes/` docs**
("Deferred", "follow-on", "STILL GATED ON", "parked") **plus** a handful of
**mutually-incompatible structured registries** — `futon2/resources/sorrys.edn`
(`:open` sorries), `futon0/…/M-capability-star-map.graph.edn` (`:held` stars),
`futon7/holes/pudding-prover-registry.edn` (`:held` sorries) — **plus** the live
archivist **gate-queue**. Five shapes, no common one. So the operator *cannot*
ask the question the cascade exists to answer: **"what held work should wake up
now?"** — the answer today is manual archaeology.

## The schema (named by the cascade itself, not invented)

`pipeline-pattern-cascade.html` §"Held / Deferred Schema": every parked obligation
becomes a **typed object** with **reason · owner · evidence-condition · wake-trigger
· expiry/review-rule · re-entry-path**, so re-entry is *operator-safe* (governed by
evidence/dates/dependency-events). Formalized here as `:held/*`:

```clojure
{:held/id            <stable, source-namespaced>
 :held/reason        <one-line: what is parked / why>
 :held/owner         <agent | "joe" | nil>
 :held/status        :held
 :held/kind          <:external-dependency | :decision-debt | :prototyping-forward | …>
 :held/wake-trigger  <condition/event that should wake it>
 :held/evidence-condition <what evidence would resolve it>
 :held/review-by     <date | nil>
 :held/re-entry      <pointer to resume: file / anchor / grounding>
 :held/source        {:registry <which> :ref <original id> :doc <file>}
 :held/missions      [<canonical <repo>-d/mission/<id> …>]
 :held/raised-at     <date>}
```

## POC (this excursion, v0)

`futon3c/scripts/held_work_ledger.bb` harvests the **structured** registries into
one typed ledger (`held-work-ledger.edn`) and surfaces **governance gaps** — items
missing an owner / wake-trigger / re-entry (the ungoverned folklore that rots
invisibly). One typed adapter (`from-sorrys`) + a generic registry tree-scan
(`from-registry-scan`) prove the unification across heterogeneous schemas.

## Follow-on (named, not silently skipped)

1. **Prose harvest** — the ~445 `holes/` "Deferred/follow-on/gated-on" sections.
2. **Gate-queue adapter** — the live archivist worklist.
3. **substrate-2 landing** — held-items as entities edged to the now-canonical
   `<repo>-d/mission/<id>` nodes (Clause-1 standard 5 *composed*; rides the
   E-futon1a-archivist canonicalisation + claude-2's D4 feeder pattern).
4. **Real wake-trigger evaluator** — "what wakes now?" governed by evidence/dates/
   dependency-events, not just gap-detection.
