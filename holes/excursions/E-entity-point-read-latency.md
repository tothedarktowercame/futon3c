# E — `/api/alpha/entity/<id>` point-read latency (2026-08-23)

**Status:** open. Run INTERRUPTED by Joe pending an external agent's diagnosis —
"something about the 27s findings isn't right." This note preserves what was
measured so the restart does not have to rediscover it.

## The measurement

All against futon1b on :7073, on a freshly restarted, unloaded JVM (metaspace
156 MB, heap 422/4096, G1 Old 0 collections) after the XTDB classloader-leak
repair.

| call | rows returned | bytes | latency |
|---|---|---|---|
| `/health` | — | 444 | **3 ms** |
| `/api/alpha/hyperedges?type=clock/clocked-on&limit=250` | 250 | 60,234 | **1.9 s** |
| `/api/alpha/entities?type=scope/pattern&limit=1` | 1 | 1,488 | **13.7 s** |
| `/api/alpha/entity/<id>` | 0 (404) | — | **~24 s** |

Three consecutive point reads: 25,030 / 23,578 / 24,194 ms. Constant, and
**404 every time** — including for `nonexistent-probe-xyz`, so the cost is not
in fetching or hydrating anything.

**This is the part that looks wrong.** A point read that finds nothing costs
~8x a 250-row hyperedge page that returns 60 KB, and ~13x nothing at all.
Returning no rows is slower than returning sixty kilobytes. Whatever `/entity/`
does, it is not proportional to the work it accomplishes.

Same shape on `/entities`: 13 s whether it matches one row or zero. Both routes
were measured slow BEFORE today's repairs too (`/entities/latest` at 15.34 s
during the deadline-gate review this morning), so this is long-standing and is
NOT a regression from the XTDB parameterisation work.

## Why it blocks the scope ingest

`resolve-pattern-node` (mission_scope_ingest.clj) calls `get-entity` before
falling through to the CACHED `resolve-pattern-library-entity`:

    (or (get-entity client base-url pattern-ident)        ; ~24 s, ~always 404
        (resolve-pattern-library-entity ...)              ; cached, in-memory
        (when (seq (str pattern-ref))
          (get-entity client base-url pattern-ref))       ; another ~24 s
        ...)

Corpus: **964 pattern-role ends across 322 distinct idents** in
`futon6/data/mission-scope-trees` (66% are repeats; `stop-the-line` alone
appears 28 times).

## What was tried, and what it bought

- `28dc7e9f` — made `--dry-run` actually suppress writes (it had been parsed and
  silently ignored on the main ingest path). Unrelated to latency, but it is why
  the interrupted run wrote nothing.
- `8a955dc1` (codex-1) — memoise `get-entity` per run, caching misses.
  Removes the 66% repeats; the 322 *distinct* first lookups remain.
  Floor: 322 x 24 s ~= **2.1 h**, down from ~6.4 h.

Observed: run started 15:40:18Z, at 15:46:31Z the live
`mission-scope/pattern` count was still 178 — unchanged, i.e. no mission had
completed. Interrupted ~15:53.

**Thread dump at interrupt** (`E-entity-point-read-latency.threaddump.txt`):

    "main" ... cpu=1230.15ms elapsed=739.40s ... WAITING (parking)
      jdk.internal.net.http.HttpClientImpl.send
      futon3c.scripts.mission_scope_ingest$http_edn
      ...$get_entity <- ...$resolve_pattern_node <- ...$ingest_scope_tree_ <- $_main

**0.17% CPU over 12 minutes.** The client is not computing; it is parked on the
server. The bottleneck is entirely futon1b response time.

## The option NOT taken (Joe's call, deliberately left open)

Reordering `resolve-pattern-node` to try the cached library resolver FIRST would
cut the run to minutes. It was not done because it changes resolution
PRECEDENCE, not just speed: `get-entity` matches on entity id, while
`resolve-pattern-library-entity` matches on `:external-id`/`:name`, and those can
select different entities. That is a semantic decision.

Note the empirical hint, though: of the 63 edges written by the earlier
accidental run, **62 came out `:linked`** — so resolution is succeeding, and
almost certainly via the cached path, *after* the 24 s miss.

## Artefacts

- `E-entity-point-read-latency.threaddump.txt` — full dump at interrupt.
- `/tmp/mission-scope-pattern-ingest-20260823.log` — codex-1's progress log
  (START 15:40:18Z count 178, CHECK 15:46:31Z count 178). In /tmp; copy it if it
  matters beyond this boot.
- Prior art: `holes/excursions/E-apm-A3-ingest-efficiency.md` (2026-08-14).

## Open question for the diagnosis

Why does a 404 point read cost ~24 s on an idle JVM when a 250-row indexed
hyperedge scan costs 1.9 s? Candidates worth eliminating: a full-table scan on
the entity-id path; a per-request `await-tx`/basis sync; the 2-permit
`with-expensive-read!` admission gate being taken on a route that should not
need it; or an XTQL plan that is not using the id index.
