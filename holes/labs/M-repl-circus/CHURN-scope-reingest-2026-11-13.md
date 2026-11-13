# CHURN: why E-euclid-tutor and E-starmap-branch-merge re-ingest every few minutes

kimi-2, 2026-11-13 (discovery only; nothing changed, nothing loaded, no
re-ingest triggered). Requisition from claude-13.

## Symptom

In the last hour `futon3c.watcher.scope-reingest` re-ingested
E-euclid-tutor 9 times and E-starmap-branch-merge 8 times, each run holding
the single reingest lane. Over 24 h (`journalctl --user`, `scope-reingest]`
lines): **E-starmap-branch-merge 1037, E-euclid-tutor 867**; the next most
frequent documents have ≤ 7 (M-repl-circus 7, a handful at 1–3 — normal
edit traffic). The two outliers re-run once per watcher cycle, about every
70–90 s, around the clock.

## The documents do not change

- `holes/excursions/E-euclid-tutor.md`: mtime 2026-11-07 07:25, last commit
  futon0 `56c3dc2` (2026-11-07). sha256 `28e419b0…`.
- `holes/E-starmap-branch-merge.md`: mtime 2026-11-08 22:30, last commit
  futon0 `ee516a4` (2026-11-08). sha256 `6e0561a1…`.

No writer touches them. They are re-ingested because they are *dispatched*
every cycle, not because they change.

## The mechanism (futon3c `src/futon3c/watcher/`)

1. Every watcher cycle, `multi.clj` `build-plan` (l. 1333) compares the
   futon0-d snapshot against the in-JVM `per-root-cache`; every cycle for
   days it has found **"86/275 files changed"** (`[cycle N] futon0-d` log
   lines). Those 86 include the two documents — their content differs from
   the frozen cache.
2. Per-file dispatch runs, and each mission/excursion doc dispatch calls
   `scope-reingest/schedule!` (`file_ingest.clj` l. 1493/1500/1506) and
   `enqueue-mission-maintenance!` (`multi.clj` l. 1550) — hence one
   reingest of each doc per cycle. (Note: both the scope-reingest lane and
   the mission-scope-lane fire per dispatch; the journal shows both
   `[scope-reingest]` and `[mission-scope-lane run]` for these docs.)
3. The dispatch batch then reaches the **deletion** doseq
   (`multi.clj` l. 1584). The frozen cache still contains watched `.py`
   files under `/home/joe/code/futon0/.venv-motifs/` — a virtualenv that no
   longer exists on disk. `NOISE-PATTERN` (`multi.clj` l. 55) excludes
   `/.venv/` but not `/.venv-motifs/`, so those files entered the cache
   while the venv existed and are now permanent `deleted` entries.
4. `handle-deletion!` (l. 1209) calls `source-file-vertices` →
   `fetch-of-type` (l. 877) → `GET /api/alpha/hyperedges?type=code/v05/
   namespace&repo=futon0-d&source-file=.../_pytest/__init__.py`. futon1b's
   log shows this exact query every cycle, taking 15–16 s and ending
   `outcome=client-disconnected` — the watcher's per-request timeout
   (`http-get-edn`, `multi.clj` l. 232–235, 5 s) fires while futon1b is
   still scanning (no index on `source-file`).
5. The throw escapes to the cycle-level catch (l. 1601): "file-ingest error
   (cache unsettled; commit-ingest still runs): request timed out" — and
   **the cache settlement at l. 1633 is skipped**. Next cycle the same 86
   files are "changed" again and the same venv deletion times out again.
   Self-sustaining loop, present in the journal since at least
   2026-11-10 00:00 (the retention horizon; likely older). The same
   signature appears intermittently for futon1-d (e.g. cycle 8631), so the
   pattern is not futon0-specific.

The churn is therefore self-aggravating: every cycle's dispatches schedule
two multi-minute reingests that load futon1b, which keeps the deletion
queries slow, which keeps the cache unsettled. Tonight kimi-1's dedup job
adds further futon1b load, but the loop predates it by days.

## Proposed fix (one paragraph)

Two independent changes, either of which breaks the loop; do both. (a)
Stop processing noise deletions: extend `NOISE-PATTERN` (and
`file-ingest/excluded-dir-re`) to cover `/.venv*/` generally, and make
`run-cycle!` drop `deleted`/`renamed` entries whose paths match the noise
pattern *before* calling `handle-deletion!`, so a deleted venv can never
reach a store query; the stale cache entries then simply disappear from
the diff. (b) Make settlement fault-local: a failing `handle-deletion!` /
`handle-rename!` should be caught per entry (as `ingest-event!` already
does per file) so one bad deletion cannot unsettle the whole root's cache;
the root then settles once and the 86-file replay ends. Supporting
hygiene: the `type+repo+source-file` hyperedge query needs an index or a
stricter bound (15 s scans on the serving store), and the watcher's 5 s
timeout on deletion queries could escalate to the expensive-read lane
rather than failing the cycle.

## Evidence pointers

- Cycle churn + error: `journalctl --user | grep "futon0-d"` —
  "86/275 files changed" then "cache unsettled … request timed out" every
  cycle (e.g. cycles 8644–8651, 01:20–01:25).
- Slow query: futon1b log `[futon1b-request] … type=code%2Fv05%2Fnamespace
  … source-file=…2F.venv-motifs%2F… _pytest%2F__init__.py elapsed-ms=15463
  outcome=client-disconnected` (repeats every cycle, e.g. 01:21:01,
  01:22:06).
- Code: `multi.clj` l. 1333 (build-plan), l. 1547–1604 (dispatch try/catch
  and unsettled-cache path), l. 1633 (settlement), l. 1209
  (handle-deletion!), l. 877 (fetch-of-type), l. 55 (NOISE-PATTERN);
  `file_ingest.clj` l. 1493–1506 (schedule! call sites).
- Docs unchanged: git + stat as above.
