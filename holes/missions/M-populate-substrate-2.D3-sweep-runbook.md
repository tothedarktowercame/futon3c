# D3 full historical re-ingest — 14-repo sweep RUNBOOK

**STATUS: ✅ COMPLETED 2026-06-25** — all 13 remaining repos replayed & verified
(14/14 cursors match last-non-merge HEAD); D0 stayed healthy throughout; serving
JVM never restarted. See **§9 CHECKPOINT: Run complete** at the bottom for the
per-repo table, what was checked, and three findings.

**For:** a standalone CLI claude (NOT in the agency mesh). Driven by a human (Joe).
**Date:** 2026-06-25 · **Author:** claude-2 (supervisor of the D3 build).
**Mission:** `futon3c/holes/missions/M-populate-substrate-2.md` (read its Checkpoint log
for full context; this runbook is self-contained for execution).

---

## 0. What this is

The D3 machinery (valid-time historical re-ingest of the structural code graph) is
**built, reviewed, and committed**. Slice-1 (forward plumbing) and one repo (futon3b,
33 commits) are done and verified — `db-as-of(t)` recovers code structure as it stood at
commit t. **This runbook performs the remaining 13 repos.**

You drive the **already-running futon3c JVM** via Drawbridge (HTTP eval). You do **not**
start any process, and you do **not** write replay logic — it exists in
`futon3c.watcher.replay`. Your job is to kick each repo's replay, watch it, verify it,
and move on.

## 1. HARD RULES (do not violate)

1. **NEVER restart or kill the JVM** (`pgrep java` — it serves futon1a :7071, futon3c
   :7070, Drawbridge :6768, WebArxana :3100). There is no "restart to fix it" lever.
2. **Drive the live JVM via Drawbridge only.** Do not start a second JVM or run the
   replay in a separate process.
3. **Kick each replay as a background `future`** (commands below). A *synchronous*
   Drawbridge eval of a big repo would run for many minutes and the HTTP call would time
   out (and shell-timeout can cascade) — never do that.
4. **Monitor D0 liveness** (the live commit-ingest watcher) throughout. If it degrades,
   pause between repos (see §6). The replay shares the serving JVM's write pipeline.
5. The replay is **non-destructive** (valid-time puts + end-valid-time retracts, all
   idempotent). Worst case of a botched repo is redoing it — nothing is lost.
6. Anything you create to verify (probe vars) — **evict it**. The replay's real data stays.

## 2. The Drawbridge eval helper

Every JVM interaction is one of these (run from `/home/joe/code`):

```bash
EVAL() { curl -s -H "x-admin-token: $(cat /home/joe/code/futon3c/.admintoken)" \
  -H "Content-Type: text/plain" --data-binary @- "http://127.0.0.1:6768/eval"; }
# usage:  EVAL <<'CLJ'
#           (some clojure)
#         CLJ
```
A reply is `{:ok true, :value …}` or `{:ok false, :error …}`. (Note: the live store's
`/api/alpha/hyperedge/:id` returns **EDN**, not JSON — don't json-parse it.)

## 3. Preconditions check (run once)

```bash
EVAL <<'CLJ'
(do (require '(futon3c.watcher.replay))
    (let [s @futon3c.watcher.multi/!state]
      {:replay-loaded (some? (resolve 'futon3c.watcher.replay/replay-repo!))
       ;; resume-works: read-cursor-sha must return the futon3b HEAD sha (string).
       ;; If it's nil/throws, the LIVE JVM is on a pre-EDN-fix replay.clj — reload
       ;; the committed version (next line) so :resume? doesn't silently re-do repos.
       :resume-works (string? (futon3c.watcher.replay/read-cursor-sha "futon3b-d"))
       :d0 {:commit-ingest? (:commit-ingest? s) :last-error (:last-error s)
            :cycle (some-> (:cycle-n s) deref)}}))
CLJ
```
Expect `:replay-loaded true`, **`:resume-works true`**, and `:d0 {:commit-ingest? true
:last-error nil …}`. If `replay-loaded` is false OR `resume-works` is false, load the
committed (EDN-fix `0afb9c2`+) version into the live JVM (no restart), then re-check:
```bash
EVAL <<<'(load-file "/home/joe/code/futon3c/src/futon3c/watcher/replay.clj")'
```

## 4. The repos (do them in THIS order — smallest first to de-risk)

futon3b is DONE (skip; `:resume?` no-ops it anyway). Counts are non-merge commits;
futon6 is high-count but mostly **python** → structurally skipped (fast).

| order | path | label | commits |
|---|---|---|---|
| 1 | /home/joe/code/futon3a | futon3a-d | 74 |
| 2 | /home/joe/code/futon1a | futon1a-d | 95 |
| 3 | /home/joe/code/futon5a | futon5a-d | 132 |
| 4 | /home/joe/code/futon7  | futon7-d | 143 |
| 5 | /home/joe/code/futon1  | futon1-d | 158 |
| 6 | /home/joe/code/futon0  | futon0-d | 220 |
| 7 | /home/joe/code/futon2  | futon2-d | 237 |
| 8 | /home/joe/code/futon5  | futon5-d2 | 291 |
| 9 | /home/joe/code/futon7a | futon7a-d | 367 |
| 10 | /home/joe/code/futon4 | futon4-elisp-d | 453 |
| 11 | /home/joe/code/futon3 | futon3-d | 919 |
| 12 | /home/joe/code/futon6 | futon6-py-d | 1133 (mostly python → skipped) |
| 13 | /home/joe/code/futon3c | futon3c-d | 1170 |

Rough total ~5,400 commits. Expect minutes for small repos, up to ~30–45 min each for
the two biggest (futon3/futon3c). One-time.

## 5. Per-repo procedure

**Step A — one-time, define the result holder:**
```bash
EVAL <<'CLJ'
(defonce sweep-results (atom {}))
CLJ
```

**Step B — kick the repo (background future).** Substitute PATH + LABEL from §4:
```bash
EVAL <<'CLJ'
(do (swap! sweep-results dissoc "LABEL")
    (future (swap! sweep-results assoc "LABEL"
              (try (futon3c.watcher.replay/replay-repo!
                     {:root "PATH" :label "LABEL"
                      :emit-removals? true :resume? true :verbose? true})
                   (catch Throwable t {:error (str t)}))))
    :kicked)
CLJ
```
Returns `:kicked` immediately.

**Step C — poll until done (every ~60s).** `nil` = still running; a result map = done;
`{:error …}` = failed (stop, report):
```bash
EVAL <<'CLJ'
(let [r (get @sweep-results "LABEL")
      s @futon3c.watcher.multi/!state]
  {:result r
   :d0 {:commit-ingest? (:commit-ingest? s) :last-error (:last-error s)
        :cycle (some-> (:cycle-n s) deref)}})
CLJ
```
A clean done looks like `{:result {:label "LABEL" :n-commits N :n-files M
:n-skipped-nonclj K :n-retracted R :through-sha "…"} :d0 {…last-error nil…}}`. Confirm
`:d0` stays healthy each poll (`commit-ingest? true`, `last-error nil`, `cycle`
increasing). If `cycle` stalls or `last-error` is non-nil, see §6.

**Step D — verify the repo time-travels** (substitute LABEL and two ISO dates spanning
the repo's history — use `git -C PATH log --reverse --format=%ci | head -1` for the
earliest):
```bash
EVAL <<'CLJ'
(require '(xtdb.api :as xt))
(let [node (:node @futon3c.dev/!f1-sys)
      vc (fn [db] (count (xt/q db (quote {:find [e] :in [pfx]
              :where [[e :hx/type :code/v05/var] [e :hx/endpoints ep]
                      [(clojure.string/starts-with? ep pfx)]]}) "LABEL/")))]
  {:vars-as-of-EARLY (vc (xt/db node #inst "YYYY-MM-DD"))
   :vars-now (vc (xt/db node))})
CLJ
```
Sanity: `vars-as-of-EARLY` should be **less than** `vars-now` (structure accreted over
the repo's life). That's the time-travel payoff. (For futon6, expect near-zero clj vars —
it's python; that's fine.)

**Step E — next repo.** Repeat B–D for the next row in §4.

## 6. Monitoring, abort, resume

- **Healthy:** between repos, `:d0` `cycle` keeps advancing and `last-error` is nil.
- **If D0 degrades** (cycle stalls / latency spikes): let the current repo finish (the
  per-commit work is small and the run is non-destructive), then **pause** before the next
  repo until cycle advances again. Do NOT restart the JVM.
- **No clean mid-repo stop.** A running `future` can't be force-killed; let it finish. The
  run is non-destructive, so this is safe — just slow.
- **Resume / idempotency:** `:resume? true` reads the per-repo `code/v05/replay-cursor`
  marker and skips already-replayed commits — but the cursor only advances at **repo
  completion**, so an interrupted repo restarts from its first commit on re-run (idempotent,
  just rework). Re-running this whole runbook is safe: completed repos no-op.

## 7. Final verification (after all 13)

```bash
EVAL <<'CLJ'
(require '(xtdb.api :as xt))
(let [node (:node @futon3c.dev/!f1-sys)]
  (into (sorted-map)
    (for [lbl ["futon0-d" "futon1-d" "futon1a-d" "futon2-d" "futon3-d" "futon3a-d"
               "futon3b-d" "futon3c-d" "futon4-elisp-d" "futon5-d2" "futon5a-d"
               "futon6-py-d" "futon7-d" "futon7a-d"]]
      [lbl (futon3c.watcher.replay/read-cursor-sha lbl)])))
CLJ
```
Every label should return a non-nil cursor sha. Cross-check each against the repo's
**last non-merge commit**: `git -C <path> rev-list --no-merges -1 HEAD` — NOT
`git rev-parse HEAD` (the replay walks non-merge commits, so on a merge-headed repo the
cursor correctly differs from HEAD). Also expect the cursor to **lag HEAD by any commits
landed after the sweep** (the live watcher carries those forward) — confirm the cursor is
an *ancestor* of HEAD (`git merge-base --is-ancestor <cursor> HEAD`), not that it equals
it. Then D0 final check (§3) — `commit-ingest? true`, `last-error nil`.

## 8. Report back

Bell/notify claude-2 (or tell Joe) with: per-repo `{:n-commits :n-files :n-skipped-nonclj
:n-retracted}`, the final cursor table, confirmation D0 stayed healthy, and any repo that
errored. Deferred-by-design (don't worry if you see them): futon6 python structure
(skipped+counted), and `code/v05/edits` is not re-emitted by the replay (already populated).

---

**Provenance:** replay machinery — futon1a `6279703` + futon3c `50866d6`, resume fix
`0afb9c2`. Reviewed/verified by claude-2. The mechanism in §5 (background future +
result atom + EDN cursor read) was tested against the live JVM before this runbook was
written.

---

## 9. CHECKPOINT: Run complete (2026-06-25)

Executed by the standalone CLI claude, driven by Joe. All 13 remaining repos
swept; the store stayed healthy; the serving JVM (PID 57253) was never
restarted. `db-as-of(t)` now reconstructs Clojure/Elisp code structure as it
stood at commit *t* across all 13 repos (~5,400 commits).

**Per-repo results** (commits / files / skipped-nonclj / retracted / cursor==HEAD / time-travel vars early→now):

| label | commits | files | skip | retract | cursor==HEAD | vars early→now |
|---|---|---|---|---|---|---|
| futon3a-d | 74 | 79 | 223 | 37 | ✓ | 0→859 |
| futon1a-d | 95 | 285 | 86 | 195 | ✓ | 0→409 |
| futon5a-d | 132 | 32 | 887 | 3 | ✓ | 0→866 |
| futon7-d | 143 | 19 | 254 | 6 | ✓ | 0→118 |
| futon1-d | 158 | 786 | 738 | 2577 | ✓¹ | 0→1937 |
| futon0-d | 220 | 53 | 416 | 443 | ✓¹ | 0→760 |
| futon2-d | 237 | 266 | 234 | 138 | ✓¹ | 0→1285 |
| futon5-d2 | 291 | 539 | 2173 | 515 | ✓¹ | 0→3426 |
| futon7a-d | 367 | 0 | 521 | 0 | ✓² | 0→0 |
| futon4-elisp-d | 455 | 128 | 1966 | 90 | ✓¹ | 0→2025 |
| futon3-d | 919 | 697 | 4426 | 1048 | ✓ | 0→4135 |
| futon6-py-d | 1133 | 4 | 4352 | 0 | ✓ | see ³ |
| futon3c-d | 1174 | 1613 | 1557 | 1857 | ✓ | 0→6254 |

(futon3b-d was already done before this runbook; its cursor `ad7c824…` is
present and matches HEAD — so the §7 table is 14/14.)

**What was checked (auditable):**
- **§3 preconditions:** `replay-loaded true`, `resume-works true`, D0
  `commit-ingest? true`. No reload needed — the live JVM already had the
  EDN-fix `replay.clj`.
- **Each repo:** kicked as a background `future` (never synchronous), polled to
  completion, then verified `through-sha == git rev-list --no-merges -1 HEAD`
  and `vars-as-of-EARLY < vars-now`.
- **§7 final table:** all 14 cursors non-nil and each cross-checked against its
  repo's actual last-non-merge HEAD — **14/14 OK**.
- **D0 liveness:** monitored every poll; `cycle-n` advanced monotonically
  856 → 1007 across the whole sweep, `commit-ingest?` stayed `true`. The
  recurring `last-error "request timed out"` flickers per-cycle and self-clears
  (live-watcher transient, not degradation — it alternates with `nil` while the
  cycle keeps advancing). The JVM was never restarted/killed.
- **Cleanup:** the only JVM artifact created was the `sweep-results` atom
  (in-memory result maps; no XTDB probe entities to evict).

**Findings:**

1. **¹ Merge-headed repos** (futon1, futon0, futon2, futon5, futon4): their
   `git HEAD` is a merge commit, so the cursor correctly equals the **last
   non-merge commit** (the replay walks non-merge commits). §7's literal
   "cursor == `git rev-parse HEAD`" check therefore needs
   `git rev-list --no-merges -1 HEAD` for these repos — not a bug, but the §7
   check as written gives a false mismatch on merge-headed repos and should be
   amended.
2. **² futon7a** is a docs/assets repo (html/png/edn/pdf — zero `.clj`):
   `n-files 0`, vars 0→0. Correct; structurally empty for code.
3. **³ futon6 — Python is present but NOT time-travelable.** The replay did
   exactly what §4 said: it **skipped Python**, processing only the 4 `.clj`
   test files historically. A prefix query on `futon6-py-d/` nonetheless shows
   **2374 Python vars** (`futon6.grounding/detect_grounded_symbols`, …). Their
   valid-time `== tx-time` at every revision (May–Jun 2026 wall-clock), i.e.
   they were written by the **live D0 commit-ingest watcher at present-time, not
   by the historical replay**. So futon6's Python structure lives in substrate-2
   but `db-as-of(commit-t)` will **not** reconstruct it as it stood at past
   commits (`as-of 2026-03-01 → 0`). This matches the "deferred-by-design"
   note in §8. (Caution for future verifiers: the §5-step-D query's `0→N` for
   futon6 reflects live-watcher accumulation over wall-clock, not historical
   time-travel — don't read it as proof of Python time-travel.)

**Operational note:** one unrelated transient `java` process (a ~30-second
`tools.analyzer` tool, not started by this sweep) briefly appeared mid-run and
self-exited; `pgrep java` returned to the single serving JVM (57253) on its own.
