# multi-watcher

`futon3c.watcher.multi` is the live-ingestion daemon for substrate-2. It
polls the futon roots on a fixed-delay schedule, dispatches per-file
ingest for changed files, and emits per-cycle heartbeats and
commit-vertex catch-up edges. It is one of the load-bearing components
delivered by **M-live-geometric-stack** (futon3/holes/missions/M-live-geometric-stack.md,
phase 4.5 — the "live ingestion discipline" section) and is what makes
the geometric `(T, ∇, Δ, drift)` field over the futon stack *live*
rather than batch.

## What it's good for

- Keeping substrate-2's typed hypergraph in sync with source on disk
  as files change — namespaces, vars, tests, mission docs, vocab
  terms, cross-refs.
- Detecting renames and deletions via content-hash matching, so
  vertex provenance survives file moves instead of being recreated.
- Catching up commit-vertex edges (`:commit`, `:authored`, `:edits`,
  `:precedes`) per cycle, so the commit DAG that M-live-geometric-stack
  uses as its print schedule stays current without a manual ingest run.
- Emitting `event "hypergraph-update"` heartbeats so the ingestion
  pipeline's own health is observable on the substrate it writes to.

Per-cycle work per root:

1. `walk-root` — enumerate watched files under the root, modulo
   `NOISE-PATTERN`.
2. `incremental-snapshot` — fingerprint (mtime, size) every file;
   SHA-256 only those whose fingerprint moved.
3. `detect-moves-and-deletes` — partition the symmetric difference of
   cache↔snapshot into renames (hash match), deletions, additions.
4. Dispatch per-file ingest (`file-ingest/dispatch!`) for changed
   paths; handle deletions (mark dependent edges
   `:edge/witness-stale true`) and renames (cascade the rename across
   vars).
5. Emit a heartbeat hyperedge with the per-cycle counts.
6. `ingest-new-commits-for-root!` — commit-vertex catch-up via
   `futon3c.watcher.commit-ingest`.

Cross-root moves (a file leaving one repo and appearing in another)
are detected globally before any per-root dispatch runs.

## Where it fits in the stack

- **Producer for:** substrate-2 (futon1a hyperedge store on port 7071).
  Every watched file change becomes one or more hyperedges under the
  parent repo's label.
- **Companion of:** `futon3c.process-watchdog` — a sibling scheduled
  executor that monitors the multi-watcher's own liveness via CYDER
  `:process/last-active` plus the watcher's per-cycle progress fields
  (`:last-cycle-started-at`, `:last-progress-at`, `:last-subtask`).
  Edge-triggered alerts (`:cycle-stuck`, `:stale`, `:state-error`,
  `:not-running`) and recovery notifications go through `notify-send`.
- **Consumed by:** M-live-geometric-stack's geometric layer, the War
  Machine's `/api/alpha/aif-stack/live` endpoint, and any query that
  trusts substrate-2 vertices to reflect what's currently on disk.

## Service shape

Lives in one JVM (futon3c, invariant I-0). Surface:

| Function | Use |
|---|---|
| `(futon3c.watcher.multi/start! opts)` | Start the loop. `opts` includes `:roots`, `:interval-ms` (default 5000), `:cold-scan?` (default false — match the bb watcher's `--no-cold-scan`), `:commit-ingest?` (default true). |
| `(futon3c.watcher.multi/stop!)` | Idempotent stop; clears `!state`. |
| `(futon3c.watcher.multi/status nil)` | Snapshot: cycle counter, last started/finished/progress timestamps, current subtask, n-roots, last error. |
| `(futon3c.watcher.multi/tick!)` | Fire one cycle on demand (interactive debugging). |

State lives in the `!state` atom: `{:executor :run-id :event-n :cycle-n
:per-root-cache :roots :interval-ms :commit-ingest? :last-cycle-*
:last-progress-at :last-error :last-subtask :stopping?}`.

## Configuration

### Watched extensions

```clojure
(def WATCHED-EXTS #{"clj" "cljs" "cljc" "el" "py" "flexiarg" "md"})
```

`.md` files only get ingested if they match the mission-doc pattern
(`/holes/missions/M-<name>.md`). Other markdown is skipped to avoid
ingesting notes, READMEs, etc., as if they were structured artefacts.

### Noise exclusions

```clojure
(def NOISE-PATTERN
  #"/\.(git|cpcache|shadow-cljs|lsp|clj-kondo|pytest_cache|venv|state)/|/node_modules/|/target/|/out/|/__pycache__/")
```

`state` excludes `<repo>/.state/` — the canonical futon convention
for per-frame transient working directories (E-night-shift frames in
futon3c, proof frames in futon6, etc.). These contain sandbox source
checkouts that must **not** be ingested as canonical source — see the
fix note below.

## Operational notes

### Status from a shell

```bash
bash scripts/proof-eval.sh \
  '(do (require (quote futon3c.watcher.multi)) (futon3c.watcher.multi/status nil))'
bash scripts/proof-eval.sh '(futon3c.process-watchdog/status)'
```

### Code reload

Hot-reload via Drawbridge, not by restarting the JVM (invariant I-0):

```bash
bash scripts/proof-eval.sh '(load-file "src/futon3c/watcher/multi.clj")'
```

The new code takes effect on the next scheduled cycle. The atom keeps
its state across reloads.

### Watchdog alert anatomy

When you get a `notify-send` like

> futon3c watchdog · multi-watcher
> cycle-stuck: no watcher progress for 31156ms

it means the current watcher cycle has gone >30s without a
`mark-subtask!` call. The threshold is `max(:min-stale-ms,
:stale-factor × interval-ms)` — currently `max(30000, 3×5000) = 30000`.
If you see these *frequently* (rather than as rare exceptions),
something is wrong: a slow substrate-2 backend, a synchronous-blocking
hot path (the watcher's hot loop POSTs hyperedges to futon1a on
`localhost:7071` — back-pressure there cascades here), or — as in the
2026-05-25 incident below — the watcher is doing too much work per
cycle.

## Fix: exclude `.state/` (2026-05-25)

**Symptom.** `:cycle-stuck` alerts firing every ~30s, paired with
"recovered" notifications shortly after — a cyclic stuck→recovered
pattern instead of the intended exceptional event.

**Root cause.** `NOISE-PATTERN` did not exclude `.state/`. Per-frame
transient checkouts created by autonomous peripherals (E-night-shift
frames under `futon3c/.state/night-shift-frames/<frame-id>/checkout/`,
proof frames under `futon6/.state/proof-frames/<frame-id>/`) include
full source-tree copies. The watcher walked them, fingerprinted them,
SHA-256'd them, and dispatched per-file ingests to substrate-2 under
the *parent* repo's label — both contaminating substrate-2 with
hyperedges for sandbox copies and creating enough HTTP work per cycle
to blow past the 30s watchdog threshold.

Survey at time of fix:

- `/home/joe/code/futon3c/.state` — 377 watched-ext files
- `/home/joe/code/futon6/.state` — 218 watched-ext files

Watcher status caught it red-handed:

```clojure
{:cycle-n 76
 :last-cycle-started-at  "2026-05-25T18:21:09Z"
 :last-cycle-finished-at "2026-05-25T18:21:04Z"   ; previous cycle
 :last-progress-at       "2026-05-25T18:24:00Z"   ; current, 3 min in
 :last-subtask {:phase :file-ingest
                :repo  "futon3c-d"
                :path  ".../.state/night-shift-frames/.../checkout/.../mission_control_backend.clj"}}
```

**Fix.** Added `state` to `NOISE-PATTERN`:

```clojure
;; before
#"/\.(git|cpcache|shadow-cljs|lsp|clj-kondo|pytest_cache|venv)/|..."
;; after
#"/\.(git|cpcache|shadow-cljs|lsp|clj-kondo|pytest_cache|venv|state)/|..."
```

Reloaded via Drawbridge with `(load-file "src/futon3c/watcher/multi.clj")`.

**Cache pruning step.** After tightening the noise pattern, the
existing `per-root-cache` atom still held entries for the now-excluded
`.state/` paths. The next `walk-root` would have produced a snapshot
missing those entries, which `detect-moves-and-deletes` would
classify as deletions — blasting substrate-2 with ~310 deletion
cascades. Mitigation: re-filter the cache through the new
`watched?` predicate before the next cycle:

```clojure
(swap! (:per-root-cache @futon3c.watcher.multi/!state)
       (fn [m] (into {} (for [[r snap] m]
                          [r (into {} (filter (fn [[p _]] (futon3c.watcher.multi/watched? p))
                                              snap))]))))
```

If you ever extend `NOISE-PATTERN` again, prune the cache the same
way to keep the transition silent.

**Open follow-up (not done).** Substrate-2 still contains hyperedges
that were ingested for `.state/` paths during the contamination
window. A retroactive cleanup of those edges is deferred — author an
E-file if it bites.

## Pitfalls if you touch this

- **Don't restart the JVM** to pick up watcher changes — use
  Drawbridge `load-file` (CLAUDE.md invariant I-0).
- **Don't synchronously call heavy diagnostics** via `proof-eval.sh`
  if they hold the eval thread for >shell-timeout — the shell timeout
  cascades into the JVM. Status calls are cheap; full-tree scans are not.
- **Don't `:reload` third-party namespaces** (e.g. `xtdb.api`) over
  Drawbridge — it poisons the JVM and forces a restart. Reload your
  own edits only.
- **Anything you add to `NOISE-PATTERN`** should have a one-line
  comment explaining the convention if the token is non-obvious
  (`state` is futon-specific; `target` isn't).
