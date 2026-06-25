# D3 full historical re-ingest — 14-repo sweep RUNBOOK

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
(let [s @futon3c.watcher.multi/!state]
  {:replay-loaded (some? (resolve 'futon3c.watcher.replay/replay-repo!))
   :d0 {:commit-ingest? (:commit-ingest? s) :last-error (:last-error s)
        :cycle (some-> (:cycle-n s) deref)}})
CLJ
```
Expect `:replay-loaded true` and `:d0 {:commit-ingest? true :last-error nil …}`. If
`replay-loaded` is false, load it (no restart): `EVAL <<<'(load-file "/home/joe/code/futon3c/src/futon3c/watcher/replay.clj")'`.

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
Every label should return a non-nil cursor sha == that repo's `git rev-parse HEAD`. Then
D0 final check (§3) — `commit-ingest? true`, `last-error nil`.

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
