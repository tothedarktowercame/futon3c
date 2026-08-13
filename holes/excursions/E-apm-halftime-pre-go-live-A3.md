# A3 work plan — reconcile duplicate `pattern/library` entities

Scope: A3 only from `E-apm-halftime-pre-go-live.md`. This is a read-only
investigation and a proposed execution plan. No store write, migration, dedupe,
or service restart was performed while preparing it.

## Conclusion and fix order

The duplicate minting is **historical, not still being produced by the current
writer**. Fix order is therefore: freeze and export a reconciliation manifest,
prove its topology, then dedupe. A minting fix is not a prerequisite, although
the dedupe must have an idempotence canary before it is allowed to run.

The original five-copies hypothesis is false. The complete live population has
5,876 current `pattern/library` documents under 1,318 names. Of those names,
260 have one distinct id, 358 have two, and 700 have seven. Thus 1,058 names
are duplicated and there are 4,558 surplus documents. The 5,000-row HTTP page
is censored and cannot establish totals.

## Checks performed

All store access below was read-only.

1. `GET /api/alpha/entities?type=pattern%2Flibrary&limit=5000` returned 5,000
   rows and `:count 5000`. A local extraction from that response found 1,194
   names, 811 duplicated names, and page-local multiplicities
   `{1 383, 2 111, 6 505, 7 195}`. `limit=5001` returned HTTP 400 with maximum
   5,000. JSON returned HTTP 500 on `#xt/zdt` readback.
2. The route does not implement paging. `futon1b_server.clj:523-530` accepts
   only `type` and `limit`; `futon1b_graph.clj:299-314` orders and limits but
   has no cursor/offset predicate. Consequently the requested HTTP pagination
   is impossible without changing code, which is outside this read-only scope.
3. To establish the uncensored totals without opening a second XTDB node, I
   used a read-only JDBC connection to the running node's loopback pgwire port
   and selected `entity$name` and `entity$id` from `entities` where
   `entity$type` is the keyword `pattern/library`. The result was the complete
   5,876/1,318 population and multiplicities stated above.
4. I read current documents for three duplicated names through that same
   connection:

   | name | ids | content comparison |
   |---|---:|---|
   | `math-formalization/ae-integral-zero` | 2 | Same source/title, but only the qualified-name id carries the current `flexiarg-v0` props, slots, source path, sigils, and metadata. |
   | `iiching/exotype-157` | 7 | Not identical. Four UUID rows share older prose and `external-id` `Exotype 157 (0x9D)`; another UUID and the qualified-name row use the title as source; the qualified-name row alone carries current flexiarg props. One migrated id is the literal string `#uuid "0030…"`, distinct from the plain UUID id. |
   | `agency/loud-failure` | 2 | Source/external-id agree, but the qualified-name id carries the complete current flexiarg projection while the UUID row is minimal. |

   The documents therefore require a content/topology rule, not “keep an
   arbitrary copy.”
5. I checked today's actual watcher activity. The journal shows
   `math-formalization/transport-across-an-instance-diamond` ingested twice
   (13:36 and 13:37) and `math-informal-CT/factor-and-lift` ingested twice
   (13:41 and 13:42). Each has exactly one current `pattern/library` id.
   `compare-universal-properties` and the renamed
   `math-informal-CT/verify-universal-property` also each have one current id.
   This is direct evidence that repeated current ingestion is not minting new
   ids.

## Where minting happens

There is no current non-idempotent minting site among the three candidates.

- `futon1a/scripts/ingest_flexiarg_pattern.clj:61-68` sends stable `name` and
  `external-id`; it does not generate an id.
- The watcher added by `0a1971bb`, now
  `futon3c/watcher/file_ingest.clj:1296-1320`, sends the same stable identifiers
  and no generated id.
- The live write boundary is `futon1b_graph.clj:241-267`. In
  `ensure-entity-id`, lines 245-250 select a requested id, otherwise a
  same-name/same-type existing id, and mint `random-uuid` only if neither
  exists. Today's repeat-ingest evidence confirms that lookup works live.

The UUID strata predate the current watcher path. The closest source-level
root-cause record is `futon1a/compat/futon1_write.clj:34-44`: its retained
history says the older compatibility behavior required exactly one name match,
treated multiple matches like none, and therefore generated another UUID on
repeated sync. Commit `746b9da` (2026-02-08) installed the current stable
smallest-id behavior. The literal `#uuid "…"` versus plain UUID pair also
shows that at least one migration changed identifier representation. Therefore
“the same ingest ran five times through today's writer” is not supported by
the data. A3.1 below must reconstruct the exact historical strata before any
deletion; current code alone cannot assign every UUID to a particular run.

## Proposed sub-steps

### A3.1 — produce a frozen, reviewable reconciliation manifest

Build a read-only scanner using bounded keyset pages over pgwire (or first add a
cursor to the HTTP endpoint in a separately reviewed change). Export, for every
current `pattern/library` name:

- every `_id`/`entity$id`, content hash after removing identity and temporal
  fields, full raw document, and system/valid-time coordinates;
- whether the id equals the qualified pattern name;
- whether current props identify `flexiarg-v0` and point to an existing source
  file;
- all incoming/outgoing relations and hyperedges by exact endpoint id;
- all clause entities reachable through `pattern/has-*`, `pattern/includes`,
  and legacy component relations, including their own incoming references;
- the current filesystem pattern and its canonical seven facet texts.

The manifest is an approval artifact, not an execution script. Record a hash
and the source watermark so a changed store invalidates approval.

### A3.2 — classify each name before selecting a survivor

Use these classes:

1. **Canonical complete:** one entity whose id is the current qualified name,
   whose props and seven clauses match the current flexiarg projection. Keep it.
2. **Canonical repairable:** the qualified-name entity exists but a legacy row
   contains information absent from it. Produce an explicit field-level merge
   proposal; do not delete until Joe accepts it.
3. **Legacy-only current pattern:** no qualified-name entity exists, but the
   source file/index still names the pattern. First create/verify the canonical
   1 + 7 + 7 topology in a future execution packet, then retire legacy copies.
4. **Renamed/deleted pattern:** no current source name exists. Resolve through
   git rename history and the approved re-filing manifest; do not accidentally
   resurrect it merely because an old entity exists.
5. **Externally referenced legacy id:** any candidate loser with an incoming
   relation/hyperedge outside its own pattern subgraph. Retargeting or a
   compatibility alias requires explicit review before retirement.

Survivor priority is semantic, not lexicographic: current qualified name with
source-matching `flexiarg-v0` projection; otherwise no automatic survivor.

### A3.3 — prove minting remains stopped

Before dedupe, run an idempotence canary in a disposable store fixture: ingest
one existing pattern twice through the same watcher/HTTP route and assert that
the second transaction changes no entity, clause, or relation id and increases
no document count. Also test a pre-seeded duplicate-by-name fixture, because
the live writer deliberately chooses a stable existing id in that condition.
No production probe is needed.

### A3.4 — construct atomic per-pattern retirement batches

For an approved canonical-complete pattern, retain its pattern entity, its
seven canonical clause entities, and its seven canonical `pattern/has-*`
relations. Retire only a loser subgraph proven private to that loser:

- the surplus `pattern/library` entity;
- clause/component entities reachable only from that loser;
- relations whose source is that loser or a retired private component;
- never an entity/relation with an external incoming reference until that
  reference has an approved retarget rule.

Use `POST /api/alpha/documents/retract` only in the future execution packet,
with one fully enumerated atomic document set per pattern. Do not infer a
fixed “1 + 7 + 7 deleted” count: legacy topologies differ, and the current
shape is the survivor shape, not necessarily the loser shape.

### A3.5 — batch conservatively and verify losslessness

Pilot on three approved names representing the 2-id, 7-id, math, and exotype
cases. After each batch verify:

- exactly one current pattern entity for the name;
- its source/props equal the frozen expected projection;
- exactly the expected canonical facets and relations remain;
- all pre-existing external incoming references resolve;
- global distinct current source names are unchanged except approved
  rename/delete cases;
- retrieval by qualified name returns the survivor.

Only then proceed in small batches with health, indexing-watermark, and
readback gates. Stop on any 503, cached-plan conflict beyond built-in retries,
watermark drift beyond the batch, or mismatch against the manifest.

## Blast radius and reversibility

The measured upper bound is 4,558 surplus pattern entity documents, not 2,800.
Clause entities and relations make the eventual mutation larger; A3.1 must
measure it exactly before approval. The recent 36-minute outage makes a single
bulk transaction or restart unacceptable.

Before execution, take and verify a cold/restorable store backup at a scheduled
maintenance point and export every proposed-to-retire raw document plus
temporal coordinates and a manifest hash. The authoritative rollback for a
wrong dedupe is restoring the pre-run store backup. Reposting exported current
documents is only a logical-content repair: it cannot promise restoration of
the original bitemporal history, so it is not the primary rollback. Each small
batch also gets its own inverse-content bundle and post-batch audit report.

## Sequencing with A1 and A2

- **A1 (1,213 `pattern/library` hyperedges):** separate abandoned re-typing
  output, not the entity UUID/name-matching mechanism. A3 will not remove or
  correct those hyperedges. A1 should remain separately explained and cleaned;
  its id diff can use the frozen A3 manifest, but must not be folded into A3.
- **A2 (284 `code/v05/pattern-slot` records):** legacy watcher projection,
  separate from entity identity minting. A3 will not remove those records.
  Sequence A3 manifest first so A2 cleanup cannot erase evidence needed to map
  legacy clauses, then execute the independently approved A2 cleanup.

Recommended order is: A3.1 manifest and identity/topology classification;
A1 explanation; A2 mapping confirmation; approved A3 dedupe; then independent
A1/A2 executions in their own scopes. Fixing A3 alone changes neither A1 nor
A2 counts.

## Noted, not blocking

The entity list endpoint's lack of cursor and its JSON failure on tagged values
made the original one-page measurement easy to misread. That belongs to the
already locked response/query residuals, not to a new A3 task; this plan does
not expand the locked list or propose working it here.
