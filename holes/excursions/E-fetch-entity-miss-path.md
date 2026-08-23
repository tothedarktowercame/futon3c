# E-fetch-entity miss path: 27 s per 404, stringified ends, orphan UUID ends

Measured 2026-08-23 by ams-claude (Fable) against the live futon1b JVM
(`:7073`, `/home/joe/code/futon1b` master). Discovery only — no source or
data changed. Reported symptom (Joe): `/api/alpha/entity/<id>` costs ~24 s
per call and 404s on nearly every pattern ident; 964 pattern-role ends
across 322 distinct idents.

Related: `E-futon1b-latency-inventory.md` (names the `fetch-entity` name-scan
fallback as "known" but does not measure it), `E-futon1b-gc-wedge.md`.

## 1. Hits are fast; misses cost two full `[*]` scans (~27 s)

| request | result | wall |
|---|---|---|
| `GET /entity/agency/bounded-lifecycle` (raw or `%2F`) | 200 | 0.09 s |
| `GET /entity/<slug ident>` × 71 resolvable idents | 200 | 0.08–0.11 s each |
| `GET /entity/0ba32581-f5ff-4df3-9c17-22de247968c4` (absent) | 404 | 26.7 s |
| three more absent UUID idents | 404 | 27.1 / 27.7 / 31.1 s |
| `GET /entities?type=pattern/library&limit=1` (reference scan) | 200 | 13.96 s |

Mechanism — `futon1b/futon1b_graph.clj:124` `fetch-entity`:

1. `xt/id` point lookup → fast, misses;
2. `entities-by-name` (`:118`): `(from :entities [*]) (where (= entity/name p))`
   → unindexed full scan of `:entities` (~5.9k rows, every column) ≈ 13 s;
3. external-id fallback (`:135`): same scan on `entity/external-id` ≈ 13 s.

A miss therefore costs ≈ 2 × the 14 s reference scan. The guard is not
involved (`/health`: `holders []`, `permits/waiters 0`). Slash-bearing ids
are not the problem: `uri-tail` URL-decodes and the raw form works.

## 2. Which idents miss on the live store

Enumerated all `mission-scope/{pattern,psr,pur,nesting}` hyperedges
(178 + 5 + 0 + 14; `pxr` empty) and extracted every `:role :pattern` /
`:role :target-pattern` end: **212 ends, 77 distinct idents** (not 964/322 —
Joe's figure must come from another source; see §3 for why it could be
larger there).

- 73 slug idents: **71 resolve**. Misses: one file endpoint
  (`futon3-d/file/library/structure/interest-event-vocabulary.flexiarg`,
  legitimately not an entity) and a literal `#uuid` token (400).
- 4 UUID idents (8 ends): **all 404, ~27 s each**.

### The UUID ends are orphans of the 08-14 re-ingest

The UUID pattern *entities* were expunged (the re-ingest rewrote pattern ids
to slugs — same event recorded at `futon1b_graph.clj:346-348` for the sigil
join). What survived are `mission-scope/pattern` hyperedges whose
`:target-pattern` end was resolved by `mission_scope_ingest`'s
`resolve-pattern-node` *before* the re-ingest and never rewritten. Each
carries a `:pattern/ref` so remapping is mechanical:

| UUID end | hyperedge | `:pattern/ref` |
|---|---|---|
| 0ba32581-… | (in pattern dump) | futon3/library/futon-theory/task-as-arrow.flexiarg |
| 18fb9e91-… | hx\|mission-scope\|demonstration-foundry/pattern/turn-design-into-checks | system-coherence/turn-design-into-checks |
| 1ddda1fb-… | — | peripherals/surface-earns-inhabitation |
| 59c414ec-… | hx\|mission-scope\|learning-loop/pattern/whose-question-is-this | structure/whose-question-is-this |
| 63e1d83e-… | — | orchestration/pattern-warranted-choice-point |
| 78def5d0-… | hx\|mission-scope\|futon-problems/pattern/stop-the-line | futon-theory/stop-the-line |
| 96499d0b-… | hx\|mission-scope\|demonstration-foundry/pattern/typed-kolmogorov-arrows | sidecar/typed-kolmogorov-arrows |
| 97383400-… | hx\|mission-scope\|zaif-harness/pattern/no-self-certification | aif/no-self-certification |

(`?end=<uuid>` returns 0 because of §3 — the end index holds the
stringified map, not the uuid.)

## 3. Separate bug: ends are stored as pr-str'd maps

`mission_scope_ingest.clj:1332-1336` (and the `mission-scope/pattern`
writer) post `:hx/endpoints` as `{:role … :entity-id …}` maps.
`futon1b_server.clj:81` does `(mapv str endpoints)`, so the store holds

    :hx/ends [{:entity-id "{:role :target-pattern, :entity-id \"orchestration/…\"}"} …]

Consequences: `?end=<ident>` cannot find these hyperedges; any consumer that
reads `:hx/ends` literally and calls `/entity/<that string>` gets a
guaranteed 27 s miss per end. A consumer walking ~1000 such ends would see
exactly "24 s per call, 404 on nearly every one" — the likely origin of the
964/322 figure.

## 4. Plan

Three independent slices, each its own handoff/review:

**P1 — futon1b `fetch-entity` miss cost (small, do directly).**
Collapse the two fallback scans into one
`(where (or (= entity/name p) (= entity/external-id p)))`, project
`[xt/id entity/id]` only, then point-fetch the winner (the `[*]` decode
dominates the scan). Skip the fallback entirely for UUID-shaped ids (never
a name). Acceptance: absent-id 404 ≤ 2 s on the live store; the 71 slug
hits unchanged; `retract-flexiarg!` (shares the scan) re-measured.
Longer-term: a name→id side index so the miss path is a point lookup.

**P2 — end coercion (server side, small).** `hyperedge` route accepts map
ends: extract `:entity-id` into `:hx/endpoints`, keep `:role` in
`:hx/ends`. Acceptance: re-POST one mission-scope hx, `?end=<slug>` finds
it. Then a one-off rewrite of the existing stringified ends.

**P3 — orphan UUID ends (data).** Rewrite the 8 ends in the table above to
their slug (`pattern/ref` → library slug), via `documents/retract` +
re-POST. Acceptance: zero UUID-shaped `:target-pattern` ends; each hx
found by `?end=<slug>`.

Order: P1 first (removes the 27 s tax regardless), P3 next (tiny), P2 last
(touches the write path; needs the ingest re-run or the rewrite script).
