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
carries a `:pattern/ref`, so remapping was mechanical. (An earlier draft of
this table listed 8 UUIDs by nearest `hx/id` in the dump; 4 of those were
UUIDs in props text, not ends. The real set, by record:)

| hyperedge | UUID end | remapped to |
|---|---|---|
| hx\|mission-scope\|demonstration-foundry/pattern/tri-store-separation | 0ba32581-… | sidecar/tri-store-separation |
| hx\|mission-scope\|demonstration-foundry/pattern/typed-kolmogorov-arrows | 18fb9e91-… | sidecar/typed-kolmogorov-arrows |
| hx\|mission-scope\|learning-loop/pattern/all-or-nothing | 78def5d0-… | futon-theory/all-or-nothing |
| hx\|mission-scope\|zaif-harness/pattern/stop-the-line | 97383400-… | futon-theory/stop-the-line |

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

## 5. P1 landed — measured 2026-08-23 16:03 after restart (futon1b `ee8f41e`)

| request | before | after |
|---|---|---|
| absent UUID id (×4) | 26.7–31.1 s | **0.07–0.09 s** (alias scan skipped) |
| absent slug id | ~27 s | **0.62 s** (one narrow scan, nothing to hydrate) |
| present slug id | 0.09 s | 0.07 s |
| `entities?type=pattern/library&limit=1` (reference) | 13.96 s | 1.75 s (cold JVM) |

Gates: clj-kondo 0/0, check-parens clean, `test-a3a4a5` 71/71,
`test-query-classes` 13 assertions 0 failures.

Restart note: `restart-futon1b-detached.sh` was refused by `store-guard`
because an orphaned `--store-dir staging-store` server (pid 3898817, :7273,
started 13:16, parent=init, cwd gone, no traffic) matched
`pgrep -f '-m futon1b-server'`. The guard keys on process name, not
store-dir, so any stray futon1b JVM blocks a production restart and leaves
:7073 down. Two follow-ups: (a) the staging server should be stopped (not
signalable from the agent sandbox); (b) the guard should compare
`--store-dir`, or the policy "one futon1b JVM" should be enforced at launch.

## 5. Execution log (2026-08-23, claude-19/20)

- **P1** futon1b `ee8f41e`: one narrow alias scan + `hydrate-by-ids`; UUID
  ids skip the scan. Gates: kondo 0/0, check-parens OK, `test-a3a4a5` 71/71,
  `test-query-classes` 13 assertions 0 fail. Live after restart: UUID miss
  **0.08–0.15 s**, non-UUID miss **0.74 s** (was ~27 s); hits 0.10 s.
- **P2** futon1b `e88536a` (codex-16, reviewed by claude-20): map ends →
  string `:entity-id` in `:hx/endpoints`, `:role` kept in `:hx/ends`;
  non-string `:entity-id` → layer-4 400. Verified live with a labelled test
  hx (POST, `?end=` lookup, retract).
- **P3 + stringified-ends rewrite** (one pass, `/tmp/p3/rewrite.py`): 197
  `mission-scope/*` hyperedges re-POSTed with parsed ends — **1012
  stringified ends** cleared (Joe's "964"), 4 UUID ends remapped per the
  table above, 0 unresolvable. Post-check: 0 stringified, 0 UUID ends; all
  four remapped hx found by `?end=<slug>`.
- **Restart incident:** the first restart failed — `scripts/store-guard`
  refused because the *staging-store* instance (`:7273`, user apollo)
  matched "any futon1b-server"; :7073 stayed down ~6 min. Fixed in
  `e61f60f`: refuse only on same `--store-dir` or same port.
- Still open: `mission_scope_ingest` itself still posts map ends (fine now
  that the server accepts them); a name→id side index would take the 0.74 s
  miss to a point lookup if it ever matters.

## 6. Correction to §5: P3's post-check was wrong (claude-13, 2026-08-23 ~17:40)

§5 records P3 as "4 UUID ends remapped ... Post-check: 0 stringified, 0 UUID
ends." The **stringified-map half is confirmed** — an audit of all 993 ends in
the live `mission-scope/pattern` layer finds 0 map-shaped `:entity-id`s, and
`hx$ends` is now typed `[:list [:struct {entity_id :utf8, role :keyword}]]`,
so P2 and the 1012-end sweep both stuck.

The **UUID half did not stick.** Live right now, 7 of the 111 `:target-pattern`
ends are malformed:

- 5 stringified UUID literals, 4 distinct (`59c414ec` is cited from two
  missions): the `:entity-id` is the *string* `#uuid "96499d0b-…"`, not a slug;
- 2 file-node refs `futon3-d/file/library/structure/…​.flexiarg`.

All 4 UUIDs resolve to **zero** entities (checked against `entities` by
`_id`/`entity$id` with a positive control returning 8, so the zero is real).

Likely cause of the false post-check: it matched a bare UUID shape
(`[0-9a-f]{8}-…`) and so missed the pr-str'd form, which begins `#uuid "`.
That is the same pr-str failure mode §3 documents, recurring inside the
verification step for the fix.

**Do not re-run P3 as a one-off.** `mission_scope_ingest` is the writer of
record for these edges; a store-level rewrite is reverted by the next ingest.
The version history is consistent with exactly that having happened —
`hx|mission-scope|demonstration-foundry/pattern/value-flow-constellation` has
only two versions, 2026-07-13 and 2026-08-23 16:08, and the current one still
carries the UUID. The repair belongs in the ingest run, whose dry run already
plans all 7 replacements against live `pattern/library` targets.

Method note: none of this is visible through the HTTP API, which does not
project system-time. It was read over XTDB's pgwire
(`127.0.0.1:34257`, `_system_from` / `FOR ALL SYSTEM_TIME`).
