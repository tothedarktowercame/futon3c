# Technote: Portfolio Inference — Sensor Tuning

**Date:** 2026-02-27
**Context:** First live scan showed CONSOLIDATE locked by saturated/broken sensors
**Precedes:** technote-portfolio-inference-live-scan.md
**Tests:** 943 tests, 3352 assertions (0 failures)

## Problem

The portfolio inference live scan (step 7) showed the system stuck in
CONSOLIDATE mode, unable to transition:

- **consolidate→BUILD** requires `spinoff-pressure < 0.3` — was 1.0 (saturated)
- **consolidate→MAINTAIN** requires `coverage-pct > 0.7` — was ~0 (broken sensor)

Three root causes, each independently locking the mode transition.

## Fix 1: Normalization Priors Scaled to Portfolio Size

**File:** `src/futon3c/portfolio/observe.clj:34-40`

The normalization caps were set for a small portfolio (~10 missions). The actual
portfolio has 40 missions across 10 devmaps (~80 components). Two caps were
saturating immediately:

| Prior | Old | New | Rationale |
|-------|-----|-----|-----------|
| gap-cap | 10 | 50 | 80 components across 10 devmaps; 10 uncovered barely registers |
| spinoff-cap | 5 | 15 | 40-mission portfolio legitimately has 10+ spinoff candidates |

**Impact on live observation:**
- `gap-count`: was `79/10 = 1.0` (saturated), now `79/50 = 1.0` still saturated
  but will drop once Fix 2 reduces the false gap count
- `spinoff-pressure`: was `N/5` (saturated above 5 candidates),
  now `N/15` — requires actual high spinoff to saturate

The caps aren't arbitrary — they represent "what would be surprising for a
portfolio this size." A portfolio of 40 missions generating 15+ new spinoff
candidates in a single review would genuinely be surprising.

## Fix 2: Coverage Matching — Devmap→Mission ID Lookup

**File:** `src/futon3c/peripheral/mission_control_backend.clj:261-301`

**Root cause:** The coverage computation used heuristic substring matching
between devmap component IDs and mission IDs. This never worked because the
naming domains are incommensurable:

- Component IDs use architectural prefixes: `G5-spec`, `S-dispatch`, `E-store`
- Mission IDs use project names: `portfolio-inference`, `agency-refactor`

`"S-dispatch"` never substring-matches `"portfolio-inference"` or any other
mission name. Result: 0% coverage across all 10 devmaps, even though most
have active parent missions.

**Fix:** Added a primary matching path: if a devmap's `:devmap/id` matches an
active mission (in-progress or complete), ALL components of that devmap are
considered covered. This is semantically correct — a devmap describes the
architecture of a single mission, and its components are sub-parts being
addressed by that mission's work.

The heuristic substring matching is preserved as a fallback for devmaps
without a matching parent mission.

**Expected impact:**
- Devmaps like `social-exotype`, `futon3-coordination`,
  `evidence-landscape-exotype` (all in-progress missions) will now show
  100% coverage
- Gap count drops from 79 to only components in devmaps whose parent
  missions are unknown/ready (not actively being worked on)
- `coverage-pct` goes from ~0 to a realistic number
- Unlocks the `consolidate→MAINTAIN` transition path once coverage > 0.7

## Fix 3: review-age Subject ID Mismatch

**File:** `src/futon3c/portfolio/observe.clj:114-127`

The review-age sensor queried for evidence with subject `{:ref/id "global"}`
(the mc-review subject), but `portfolio-step!` emits evidence with subject
`{:ref/id "inference"}`. The sensor never found any evidence, always returning
999 days → saturated at 1.0.

**Fix:** Query both subjects and use the most recent:
```clojure
(let [global    (estore/query* ... {:ref/id "global"} ...)
      inference (estore/query* ... {:ref/id "inference"} ...)
      latest    (first (sort-by :evidence/at compare-desc
                                (concat global inference)))]
```

After restarting `make dev` and running a portfolio step, review-age will
drop from 1.0 to ~0 as the step's own evidence emission is now visible
to the sensor.

## Live Scan After Fixes 1+2 (Fix 3 pending restart)

Tested with Fixes 1 and 2 applied, Fix 3 not yet loaded (server restarted
before the review-age query fix):

| Channel | Before | After | Notes |
|---------|--------|-------|-------|
| coverage-pct | ~0 | **0.90** | Coverage matching working |
| gap-count | 1.0 | **0.08** | No longer saturated |
| spinoff-pressure | 1.0 | **0.29** | Sub-0.3 threshold |
| review-age | 1.0 | **1.0** | Still stuck (Fix 3 needed) |
| dependency-depth | 1.0 | **0.99** | Still high (real signal) |

The system transitioned BUILD → CONSOLIDATE on step 2 because `review-age > 0.8`
triggered `build→consolidate?`. Once Fix 3 is loaded and review-age drops,
`consolidate→MAINTAIN` should fire (needs `coverage-pct > 0.7 AND review-age < 0.3`
— coverage is 0.90, review-age will be ~0).

72 missions scanned (was 40 — the expanded repo roots picked up more).
FE converged from 0.0877 to 0.0000 in 5 steps.

## Expected Mode After Full Restart

With all three fixes loaded, the most likely outcome is **MAINTAIN**:
- `coverage-pct` = 0.90 (> 0.7 threshold)
- `review-age` → ~0 after first step (< 0.3 threshold)
- `spinoff-pressure` = 0.29 (< 0.3 — also unlocks consolidate→BUILD)

The BUILD→CONSOLIDATE trigger (`review-age > 0.8`) won't fire because
review-age will be near 0. The system should stabilize in BUILD or MAINTAIN.

## Future: Per-Component Coverage Annotations

The current fix uses devmap-level coverage (active parent mission →
all components covered). This is correct but coarse. True per-component
coverage would require explicit annotations:

```edn
;; In a devmap or mission file:
{:mission/id :social-exotype
 :components-addressed [:S-dispatch :S-validate :S-persist]}
```

This is a future refinement — the devmap-level approach is sufficient for
accurate mode selection and gap identification.

## Post-Fix: Confirmed Mode Transition (all three fixes live)

After restarting `make dev` with all three fixes:

| Channel | Settled Value | Notes |
|---------|-------------|-------|
| coverage-pct | 0.90 | Fix 2 |
| gap-count | 0.075 | Fix 1+2 |
| spinoff-pressure | 0.239 | Fix 1 |
| review-age | **0.000** | **Fix 3 confirmed** |
| evidence-velocity | 0.9996 | High evidence production |
| stall-count | 0.589 | Moderate (42 unknown missions) |
| mission-complete-ratio | 0.192 | 14/72 complete |

**Mode: BUILD** (no longer locked in CONSOLIDATE). FE converged to ~0.0003 in 4 steps.

## Enablers for M-self-representing-stack (E-1/E-2/E-3)

Three endpoints added to prepare the data surface for Arxana hyperedge ingestion.

### E-1: Tag-based evidence query (schema-level)

Moved tag filtering from HTTP post-filter into the store layer. `EvidenceQuery`
now includes `:query/tags` — all backends (Atom, XTDB, HTTP proxy) filter
natively. AND semantics: `?tag=mission,backfill` requires both tags.

Files: `shapes.clj`, `backend.clj`, `http_backend.clj`, `http.clj`

### E-2: Tension export endpoint

`GET /api/alpha/mc/tensions` — returns structured tension data as typed maps
instead of MC's string-based `find-gaps`. Each entry has `:tension/type`
(`:uncovered-component`, `:blocked-mission`, `:structural-invalid`), devmap/
component IDs, and summary.

Live result: 9 tensions (all uncovered-component type, in peripheral-gauntlet
and f3c-grounding-functor devmaps).

Files: `mission_control_shapes.clj` (TensionEntry), `mission_control_backend.clj`
(`build-tension-export`), `http.clj`

### E-3: Backfill endpoint

`POST /api/alpha/mc/backfill` — creates per-mission evidence entries tagged
`[:mission :backfill :snapshot]`. Idempotent: skips duplicates on re-run.

Live result: 73 missions backfilled (created=73, skipped=0). Re-run: created=0,
skipped=73.

### Integration verification

```
curl -X POST localhost:7070/api/alpha/mc/backfill          # 73 created
curl 'localhost:7070/api/alpha/evidence?tag=mission,backfill&limit=3'  # tag query works
curl localhost:7070/api/alpha/mc/tensions                   # 9 structured tensions
```
