# D1 — round-1 conductor cascade offers, 2026-08-26

This is a read-only reconstruction of frames f9–f17.  It answers a narrower
question than a grep of `data/apm-campaigns/`: did the round-1 conductor call
the cascade, and what did it produce?

## Result

The cascade did execute.  It produced expanded, routed offers in f9, f10, f13,
and f15.  The statement “the cascade has never run anywhere” is false.  The
accurate statement is that the campaign/archive directories inspected by
`TN-APM-cascades-exist-unused.md` do not contain the conductor's cycle outputs.
The full operational state was written by `:problem-save` to
`data/problem-state/<cycle-id>/vN.edn`; the evidence store also retained step
evidence whose `advance-problem-phase` arguments contain the output payload.
The separate snapshot evidence contains only cycle id, version, phase and step
counts, so a grep of snapshot summaries cannot find offers.

`conductor/memory-offers` scans every recorded `:dispatch-solver` and
`:dispatch-student-fresh` result, then calls `cascade-receipt-offers` for every
raw receipt.  Therefore an empty result below still distinguishes “the
function ran with no seed” from “the conductor never dispatched.”

## Per-frame reconstruction

| frame | (a) conductor dispatch reached? | (b) cascade/output evidence | (c) solver surfaced ids | (d) explanation |
|---|---|---|---:|---|
| f9 | Yes: one solver and one student dispatch. Solver job `invoke-1787039327262-4733-c4f8c867`. | Yes. The solver-attempt advance carried 101 offers: 1 `:leaf`, 100 `:co-incidence`; 100 carried `:offer/via-pattern`, and all 101 carried `:offer/cascade-expanded-available 115`. | 1 | Cascade ran and persisted in evidence step arguments, but not in the campaign frame stub. |
| f10 | Yes: one solver and one student dispatch. Solver job `invoke-1787046147915-4756-43c6bd77`. | Yes. 102 offers: 2 `:leaf`, 48 `:why-hop`, 52 `:co-incidence`; 100 carried `:offer/via-pattern`, and all 102 carried expanded-available = 131. | 2 | Cascade ran and persisted outside the archive directories searched by the TN. |
| f11 | Yes: one solver and one student dispatch. Solver job `invoke-1787069359429-4886-b2adf4db`. | `cascade-receipt-offers` was reached when the solver attempt was recorded, but the resulting `:memory-offers` vector was empty. | 0 | The raw solver receipt had an empty `:memory-use/surfaced-ids`; there was no seed to expand. |
| f12 | Yes: three solver dispatches and one student dispatch. Solver jobs `invoke-1787120302749-4925-3b3082f9`, `invoke-1787122732677-4942-94c8a70f`, and `invoke-1787123632359-4949-f2b97971`. | The solver-attempt advance persisted an empty `:memory-offers` vector. | 0 on all three | The cascade computation ran over empty surfaced-id vectors and returned no offers. |
| f13 | Yes: one solver dispatch, job `invoke-1787138965830-5047-ae5ec610`. | Yes. 105 offers: 5 `:leaf`, 100 `:co-incidence`; 100 carried `:offer/via-pattern`, and all 105 carried expanded-available = 128. | 5 | Cascade ran. Both the final local state (`v21.edn`) and evidence step payload agree. |
| f14 | No conductor cycle or frame dispatch found. There is no frame-14 file and no problem-state registration. | No. | n/a | The only `f14-solver` text-search hits explicitly describe a separate probe sent to `codex-8`, “not `f14-solver`.” Registration existed, but no f14 conductor dispatch occurred. |
| f15 | Yes: three solver and one student dispatch. Solver jobs `invoke-1787153737431-5077-eb8f0615`, `invoke-1787154576940-5079-19a9b984`, and `invoke-1787155612296-5082-1844680c`. | Yes. Final cycle output has 315 offers across the three solver-attempt records: 15 `:leaf`, 144 `:why-hop`, 156 `:co-incidence`; 300 carry `:offer/via-pattern`, all 315 carry expanded-available = 132. This is three 105-offer expansions, not 315 distinct offers from one receipt. | 5 on each solver dispatch | Cascade ran three times and was persisted in `data/problem-state/.../v356.edn`. |
| f16 | No conductor dispatch. The solver did work, but the retained transcript shows direct bells from `f16-guide` (`invoke-1787217051964-5153-35304c82`, `invoke-1787217212303-5158-abfd6e0a`) rather than a `:dispatch-solver` problem step. | No conductor cycle output. | n/a | Direct Agency work bypassed `conductor/memory-offers`; the frame was later voided as defective. |
| f17 | Yes: five solver and one student dispatch. First solver job `invoke-1787231385995-5240-44213fe0`; the remaining solver job ids are retained in `v65.edn`. | The recorded solver attempt has an empty cycle-output `:memory-offers` vector. | 0 on all five | The cascade computation ran over empty surfaced-id vectors and returned no offers. |

The f9/f10/f13/f15 values are not inferred from registration prose.  They are
counts over maps carrying the actual `:offer/route`, `:offer/via-pattern`, and
`:offer/cascade-expanded-available` keys.  Likewise, f11/f12/f17 are not
classified as “did not run”: each has conductor dispatch steps followed by a
recorded solver-attempt advance, which is the call site that invokes
`memory-offers`; the inputs simply had no surfaced ids.

## Persistence path and why the archive grep missed it

- `src/futon3c/apm/conductor.clj:450-457` calls
  `cascade-receipt-offers` for the raw memory receipts attached to solver and
  fresh-student dispatch steps.
- `record-solver-attempt!` places that result in the advance payload as
  `:memory-offers` (`conductor.clj:661-670`).
- The problem backend saves the complete state with `pr-str` under
  `data/problem-state/<cycle-id>/vN.edn`
  (`src/futon3c/peripheral/problem.clj:1029-1050`).
- The evidence snapshot deliberately stores only a summary
  (`problem.clj:163-173`).  Full step evidence is more useful here because an
  `advance-problem-phase` entry retains the advance arguments.
- The Agency ledger no longer retains these old invoke jobs: listing 1,529
  current jobs returned no f9–f17 seat jobs, and direct GETs for six known job
  ids returned 404.  The job ids in the table therefore come from the persisted
  problem-step evidence/state, not a currently live ledger entry.

## Exact HTTP queries run

All substrate requests were GET-only.  No deep-health request was made.

Initial surface and schema checks:

```text
curl -fsS --max-time 20 'http://localhost:7073/api/alpha/evidence?type=problem&limit=10&include-ephemeral%3F=true'
curl -fsS --max-time 20 'http://localhost:7070/api/alpha/invoke/jobs?limit=5'
curl -fsS --max-time 30 'http://localhost:7073/api/alpha/evidence/text-search?q=problem-save&limit=100&hydrate=true'
```

The first query returned zero because problem-peripheral evidence is typed
`:coordination`, not `:problem`; reading `peripheral/evidence.clj` established
the correct schema before the remaining queries.

Problem searches used to recover cycle/session ids and dispatch evidence:

```text
curl -fsS --max-time 60 'http://localhost:7073/api/alpha/evidence/text-search?q=a01J06&limit=500&hydrate=true'
curl -fsS --max-time 60 'http://localhost:7073/api/alpha/evidence/text-search?q=m93J02&limit=500&hydrate=true'
curl -fsS --max-time 60 'http://localhost:7073/api/alpha/evidence/text-search?q=t01A05&limit=500&hydrate=true'
curl -fsS --max-time 60 'http://localhost:7073/api/alpha/evidence/text-search?q=m03J01&limit=500&hydrate=true'
curl -fsS --max-time 60 'http://localhost:7073/api/alpha/evidence/text-search?q=m99J06&limit=500&hydrate=true'
curl -fsS --max-time 60 'http://localhost:7073/api/alpha/evidence/text-search?q=m93J06&limit=500&hydrate=true'
curl -fsS --max-time 60 'http://localhost:7073/api/alpha/evidence/text-search?q=m97A06&limit=500&hydrate=true'
curl -fsS --max-time 60 'http://localhost:7073/api/alpha/evidence/text-search?q=f14-solver&limit=500&hydrate=true'
curl -fsS --max-time 60 'http://localhost:7073/api/alpha/evidence/text-search?q=f16-solver&limit=500&hydrate=true'
```

Exact-session evidence queries (each used `limit=1000`):

```text
curl -fsS --max-time 30 'http://localhost:7073/api/alpha/evidence?session-id=5c6114c7-f433-4229-94c9-ced78a0a5fee&limit=1000&include-ephemeral%3F=true'
curl -fsS --max-time 60 'http://localhost:7073/api/alpha/evidence?session-id=dedd116d-d8ab-4d03-af00-fc37920f68c6&limit=1000&include-ephemeral%3F=true'
curl -fsS --max-time 60 'http://localhost:7073/api/alpha/evidence?session-id=524e7110-9eed-4dcc-9943-d541aa31c2f7&limit=1000&include-ephemeral%3F=true'
curl -fsS --max-time 60 'http://localhost:7073/api/alpha/evidence?session-id=c57ea1de-9050-4c98-b29b-7d7b67491329&limit=1000&include-ephemeral%3F=true'
curl -fsS --max-time 60 'http://localhost:7073/api/alpha/evidence?session-id=530f6875-935d-40a1-864b-c61a9680152f&limit=1000&include-ephemeral%3F=true'
curl -fsS --max-time 60 'http://localhost:7073/api/alpha/evidence?session-id=578e66fa-fb69-4c19-9ac7-8f29df30a6e6&limit=1000&include-ephemeral%3F=true'
curl -fsS --max-time 60 'http://localhost:7073/api/alpha/evidence?session-id=zai-ff6cd21d-0e29-4ba6-8e2c-761dd7f24713&limit=1000&include-ephemeral%3F=true'
```

One exploratory batch used `limit=2000` on the same session endpoint and was
rejected with HTTP 400; it was immediately replaced by the supported 1000-row
queries above.

Agency retention checks:

```text
curl -fsS --max-time 30 'http://localhost:7070/api/alpha/invoke/jobs?limit=5000'
curl -sS --max-time 10 'http://localhost:7070/api/alpha/invoke/jobs/invoke-1787039327262-4733-c4f8c867'
curl -sS --max-time 10 'http://localhost:7070/api/alpha/invoke/jobs/invoke-1787046147915-4756-43c6bd77'
curl -sS --max-time 10 'http://localhost:7070/api/alpha/invoke/jobs/invoke-1787069359429-4886-b2adf4db'
curl -sS --max-time 10 'http://localhost:7070/api/alpha/invoke/jobs/invoke-1787120302749-4925-3b3082f9'
curl -sS --max-time 10 'http://localhost:7070/api/alpha/invoke/jobs/invoke-1787138965830-5047-ae5ec610'
curl -sS --max-time 10 'http://localhost:7070/api/alpha/invoke/jobs/invoke-1787231385995-5240-44213fe0'
```

## Answer

“Never run anywhere” is not true.  The round-1 conductor expanded real recall
seeds and persisted routed offers in four frames.  What is true is “nothing was
copied into the campaign/frame archive locations searched by the earlier
technote.”  Three additional conductor frames did call the same path but had
empty solver surfaced-id vectors, so they correctly produced no routed offers.
The two remaining registrations did not test the path: f14 never dispatched a
frame, and f16 worked through direct guide-to-solver Agency bells.
