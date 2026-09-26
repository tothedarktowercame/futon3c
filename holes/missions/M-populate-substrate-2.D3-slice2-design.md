# M-populate-substrate-2 — D3 Slice 2 design note (historical re-ingest)

**Author:** claude-4 · **Date:** 2026-06-25 · **Status:** DRAFT for claude-2 sign-off (do not code until ack)
**Builds on:** D3 slice 1 (futon1a `a525adb`, futon3c `e916975`) — valid-time plumbing, forward.

---

## 1. Goal

Replay every commit in all 14 watched repos and re-emit the **structural code
graph** (`var` / `contains` / `calls` / `coverage`) at each commit's **valid-time**,
so `db-as-of(t)` recovers the code structure *as it actually stood at commit t* —
across the whole existing history (April→now), not just forward. Removal-accurate:
defs/edges that disappear at a commit must become invisible to `db-as-of` after it.

Slice 1 proved the write path carries valid-time and that var vertices time-travel.
Slice 2 makes it **historically true and complete**.

## 2. Core architectural idea — a stateful forward replay carrying an incremental repo model

The naïve "for each commit, parse the whole repo as-of that commit" is O(commits ×
repo-size) parsing — fatal at 5,423 commits. Instead, walk each repo's commits
**chronologically** carrying a running snapshot of the repo's structure, and apply
each commit's diff:

```
state per repo:  snapshot : {rel-path → collect-file-result}     ;; the file model
                 by-ns    : {ns → #{qnames}}  (derived from snapshot, for calls/coverage)
for each commit c (oldest→newest):
  (A/M path)  blob   := git show c:path           ;; BLOB-ACCURATE — never the working tree
              new    := collect-from-string(blob, path)
              old    := snapshot[path]
              emit   PUTs  for new var/contains/calls/coverage at valid-time=c.ts
              emit   RETRACTs at valid-time=c.ts for (old − new): removed vars + their edges
              snapshot[path] := new ;  update by-ns
  (D path)    emit   RETRACTs at valid-time=c.ts for everything in snapshot[path]
              snapshot dissoc path ; update by-ns
  emit edits (c → new-or-changed vars) at valid-time=c.ts   ;; reuse commit-ingest
```

Per-commit work is **O(changed files)**, not O(repo). Removal falls out as a cheap
**set-diff** of the carried snapshot — so removal-accuracy costs almost nothing
extra (this is why I recommend doing it; see §6). `by-ns` is maintained
incrementally so `calls`/`coverage` symbol resolution at commit c sees the repo
state up to c, with no re-parse.

## 3. Components

### 3a. Blob-accurate `collect-from-string` (futon3c.watcher.file-ingest)
`read-forms` (file_ingest.clj:231) reads from a `java.io.File` via `PushbackReader`.
Add a sibling that reads from a string (`StringReader` → `PushbackReader`), and a
`collect-clj-from-string [content path]` (path only for the `test-file?` heuristic +
ext) reusing `collect-clj-file`'s form-walk. Then `collect-from-string [content path]`
dispatching by ext, mirroring `collect-file` (file_ingest.clj:307).
- **Scope:** clj/cljs/cljc first — that is the entire `var`/`contains`/`calls`/`coverage`
  structural graph. elisp/flexiarg parse from path too and refactor the same way.
  **python shells out** to `python_ast_helper` (reads a file) → blob-accuracy needs
  stdin or a temp blob; **defer python historical structure to a named follow-on**
  (its forward live versioning is unaffected). Flag this gap loudly, don't hide it.

### 3b. Shared structural-emitter (parameterized by valid-time)
Extract the post body of `ingest-one-file!` (file_ingest.clj:1037 — the
namespace/var/test/calls/coverage/contains `post!` calls) into
`emit-structure! {:structure :label :base-props :root-ctx :valid-time-ms :prior-structure}`.
- `ingest-one-file!` calls it with `valid-time-ms nil` + `prior-structure nil` →
  **identical current behaviour** (regression-safe).
- the replay calls it with `valid-time-ms = c.ts` and `prior-structure = snapshot[path]`
  (drives the retract set-diff).
- **Requires** file-ingest's `post-hyperedge!` to honour a valid-time the same way
  commit-ingest's now does. Cleanest: lift the `*valid-time-ms*` dynamic var +
  `hx/valid-time` payload stamping into a shared helper both `post-hyperedge!`s use,
  so there is exactly **one** valid-time write path and **one** id convention
  (kills the two-id-convention drift risk for the directed `dir:` edges).

### 3c. Replay driver — new ns `futon3c.watcher.replay`
Requires `file-ingest` (for collect/emit) + `commit-ingest` (for `list-commits`,
`files-changed`-style status, edits). Per repo: chronological `list-commits`, carry
the snapshot/by-ns state, drive §2. New ns (not multi/commit-ingest) keeps the live
hot path untouched and avoids a circular require (multi→file-ingest already).
Entry: `replay-repo! {:root :label}` and `replay-all! {:roots}`. `ingest-all-commits!`
(commit_ingest.clj backfill) is **subsumed** by this — and it must use blob content,
not the working tree, per the same rule.

### 3d. Removal / retract directive (futon1a)
Add an `hx/op "retract"` directive to `compat-upsert-hyperedge`: when present (with
`hx/valid-time`), emit `[:xtdb.api/delete hx-id valid-time]` instead of a put (hx-id
computed identically from type+endpoints, or explicit `hx/id`). `verify-materialized!`
inspects put-docs only → a delete-only tx has none → passes (the entity is correctly
*absent* at current time after an end-valid-time delete). **Open risk to verify
before building:** `inv/enforce-counter-ratchet!` (pipeline.clj:131) runs on every
tx — confirm it tolerates a delete op (it likely counts puts; a delete may read as a
"drop" it wants to block). If it fights deletes, the retract path needs a ratchet
carve-out (`:allow-drop-classes`) or the ratchet must skip valid-time-ended deletes.

### 3e. Idempotency + resumability
- **Idempotent by result:** re-running re-puts identical docs at the same valid-time
  → identical `db-as-of`. No dedup-guard (per your call).
- **Cursor-resumable:** persist a per-repo `replayed-through-sha` so a re-run skips
  completed repos and resumes mid-history. Store it as a dedicated marker hyperedge
  (`code/v05/replay-cursor` [label] props {sha ts}) — NOT under `.state` (excluded
  from ingest). On start, resume from the cursor; advance it per commit (or per
  batch-of-N for fewer writes).

## 4. Cost / runtime estimate (grounded)

- **5,423** non-merge commits across the 14 repos (live count, matches the spine).
- Single write through the full HTTP→pipeline→put path: **~6 ms** sequential
  (measured: 20 posts / 133 ms).
- Current structural volume: var 115k, contains 63k, calls 74k, edits 184k. The
  replay re-emits structure per commit; total writes ≈ Σ over commits of changed
  structure — order **0.3–1.0 M writes** depending on churn.
- Sequential wall-clock: **~30 min to a few hours**, one-time, resumable.
  Recommend running it **monitored + throttled** (it shares the serving JVM's
  pipeline) so it doesn't starve the live watcher; never pause/stop services. Could
  parallelise per-repo later, but sequential + resumable is the right first cut.

## 5. Slice-2 boundaries

**In:** clj-family blob-accurate `collect-from-string`; shared valid-time emitter;
replay driver with incremental snapshot; removal-accurate retracts; futon1a retract
directive; resumable per-repo cursor; runtime/monitor harness.
**Deferred (named follow-ons, flagged not hidden):** python (and elisp/flexiarg if
they fight the string refactor) historical structure; per-repo parallelism;
auto-trigger of replay from a staleness alarm (D7).

## 6. The removal-scope call (your §2 / Joe-facing)

**Recommendation: do removal-accurate now, do NOT fall back to monotone.** Your
fallback worry was that removal balloons the slice — but the stateful-snapshot
architecture makes removal a **free set-diff** of state we already carry; the only
genuinely new cost is the small futon1a retract directive (§3d) + the counter-ratchet
check. Monotone would *knowingly* leave `db-as-of` over-reporting removed defs —
exactly the historical-inaccuracy defect Joe ruled out for forward-only. So accuracy
and effort point the same way here. (If the counter-ratchet interaction in §3d turns
out nasty, that — not the diff — is the only thing that could justify a monotone
v1 + loud flag; I'll surface it explicitly if it does, for your + Joe's call.)

## 7. Open decisions for your sign-off

1. New ns `futon3c.watcher.replay` vs folding into commit-ingest — I propose new ns.
2. Retract directive on `compat-upsert-hyperedge` (`hx/op "retract"`) vs a sibling
   route vs direct-node deletes for the batch — I propose the directive (keeps the
   run-write! discipline) pending the counter-ratchet check.
3. Removal-accurate now (my recommendation) vs monotone+flag.
4. Resume-cursor as a marker hyperedge vs a tracked edn file — I propose the marker
   hyperedge.
5. Throttle policy / when to run (off-peak, monitored) — Joe's call on timing.
