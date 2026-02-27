# Hypergraph + DOCUMENT Probe (2026-02-27)

## Scope
- Compare current `futon1a` hyperedge surface against `futon4` writer expectations.
- Add first-class DOCUMENT enforcement in mission cycle gates.

## Evidence: futon1a hyperedge surface
- HTTP routes present:
  - `POST /api/alpha/hyperedge`, `POST /api/hyperedge`
  - `GET /api/alpha/hyperedge/:id`
  - `GET /api/alpha/hyperedges?type=...`
  - `GET /api/alpha/hyperedges?end=...`
- Source:
  - `futon1a/src/futon1a/http/app.clj`
  - `futon1a/src/futon1a/api/routes.clj`
  - `futon1a/src/futon1a/compat/futon1_write.clj`
- Integration tests:
  - `futon1a/test/futon1a/integration/hyperedge_http_test.clj`
  - `futon1a/test/futon1a/integration/arxana_compat_write_http_test.clj`
- Verified on this run:
  - `clojure -M:test -n futon1a.integration.hyperedge-http-test -n futon1a.integration.arxana-compat-write-http-test`
  - Result: `Ran 9 tests containing 52 assertions. 0 failures, 0 errors.`

## Evidence: futon4 payload shape expectations
- `futon4` sends `hx/endpoints` through as provided by caller.
- Current test fixture uses endpoint maps with `(role . ...)` and `(entity . "...")`:
  - `futon4/test/arxana-store-test.el` (`arxana-store-post-hyperedge-builds-payload`)
- Payload builder:
  - `futon4/dev/arxana-store.el` (`arxana-store--hyperedge-payload`, `arxana-store--post-hyperedge`)

## Compatibility matrix (current)
- Flat endpoint IDs (`["id-a" "id-b"]`): supported.
- Rich endpoint maps with `:entity-id`: supported (covered by futon1a test).
- Rich endpoint maps with `:entity` string (futon4 test style): supported (added test).
- Query by type and by endpoint ID: supported.
- Idempotent write shape (`hx/id` stable): supported (covered by futon1a test).
- Data-only passage endpoint (`:role ... :data {...}` without entity-id): not covered by futon1a tests.

## New compatibility test added
- `futon1a/test/futon1a/integration/hyperedge_http_test.clj`
  - `hyperedge-futon4-endpoint-shape`
  - posts endpoints as `{:role \":role/source\" :entity \"...\"}` and `{:role \":role/target\" :entity \"...\"}`
  - verifies accepted write + normalized roles and endpoint IDs
- Supporting normalization update:
  - `futon1a/src/futon1a/compat/futon1_write.clj`
  - `normalize-endpoint` now accepts `:entity` as either map or string.
- Verified on this run:
  - `clojure -M:test -n futon1a.integration.hyperedge-http-test -n futon1a.integration.arxana-compat-write-http-test`
  - Result: `Ran 10 tests containing 57 assertions. 0 failures, 0 errors.`

## Why code→doc felt persisted in futon4
- `arxana-links.el` explicitly defines tiers:
  - Tier 0 = ephemeral computed links (no persistence)
  - Tier 1/2 = persisted strategy or anchored links
- `arxana-browser-code.el` computes matches in-memory from docbook/index + symbol scans (`arxana-browser-code--docbook-matches`) and stores runtime maps (`arxana-browser-code--doc-symbol-map` etc.).
- `docs-backlog.org` has an explicit TODO noting these code/docs live-sync structures were non-persisted and needed a persistence manifest.

Interpretation: during the futon1a era, code→doc correspondence could be visibly working in UI because it was derivable at read time, even when persisted hyperedge/link coverage was incomplete.

## Mission discipline changes made
- Mandatory DOCUMENT checkpoint added to mission cycle requirements:
  - `:integrate` now requires `:doc-artifacts` and `:hypergraph-plan`.
- New gate in mission gate-review:
  - `:GD-document` fails when docs are missing, or hypergraph plan is neither wired (`refs`) nor explicitly deferred with reason.
- Source:
  - `futon3c/src/futon3c/peripheral/mission_shapes.clj`
  - `futon3c/src/futon3c/peripheral/mission_backend.clj`
  - `futon3c/test/futon3c/peripheral/mission_backend_test.clj`
- Docs updated:
  - `futon3c/docs/futonic-missions.md` (DOCUMENT phase text + GD gate in pipeline table)

## Verification for mission changes
- Command:
  - `clojure -M:test -n futon3c.peripheral.mission-backend-test -n futon3c.peripheral.mission-test -n futon3c.peripheral.mission-control-test`
- Result:
  - `Ran 71 tests containing 359 assertions. 0 failures, 0 errors.`
