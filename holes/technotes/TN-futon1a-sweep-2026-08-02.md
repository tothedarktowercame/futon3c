# TN-futon1a-sweep — what still points at the deprecated store

**Status: measurement + three repairs, 2026-08-02, claude (Opus 5).** Triggered by
Joe's stop-the-line call on "multi_watcher not running". That premise turned out to be
false; the sweep it motivated found three real failures anyway, in a class the premise
named correctly.

## 0. The correcting finding first

**The multi-watcher is running and is correctly pointed at futon1b.** Live status at
08:5x today:

```
:running? true  :cycle-n 57974  :run-id 1785241767309 (started 2026-07-28 13:29)
:last-cycle-finished-at "2026-08-02T07:50:49Z"
:freshness {:outcome :ok :detail {:note "substrate-2 is current with every repo's git HEAD"}}
:n-roots 14  :interval-ms 5000  :last-error nil
```

The daemon is `futon3c.watcher.multi` inside the serving JVM (pid 4009446), **not** the
`futon3/scripts/multi_watcher.clj` babashka script. `multi.clj:37` resolves
`FUTON_SUBSTRATE_URL` *before* `FUTON1A_URL`, and the JVM env carries
`FUTON_SUBSTRATE_URL=http://127.0.0.1:7073` and `FUTON1A_URL=http://127.0.0.1:7073`.
So the switchover was applied here.

**The `baldwin/*` patterns were also ingested and indexed.** All eleven are in
`futon3/resources/sigils/patterns-index.tsv`, which is a symlink into
`storage/futon3/...` refreshed nightly at 04:30 by the `index_patterns.sh` cron. The
patterns were written 2026-08-01; the cron picked them up at 04:30 on 2026-08-02. The
"not indexed" note in `TN-baldwin-experiment-guidance.md` §7 and in
`futon3/library/baldwin/INDEX.md` was true when written and is now stale — the system
self-heals on a daily cadence, it just has a ≤24h lag.

## 1. What the sweep actually found

Three scheduled jobs, all of the same shape: **a job that fails or no-ops silently is
indistinguishable from a job with nothing to do.**

| # | job | trigger | state | cause |
|---|---|---|---|---|
| 1 | `futon6/scripts/mission_efe_scope_dump.py` | daily 05:30 cron | **failing since 2026-07-12** | hardcoded `localhost:7071` |
| 2 | `futon3/scripts/phase_5_signatures.clj` | weekly systemd timer | **failing every run** | `FUTON1A_URL` default `localhost:7071` |
| 3 | `vitality-scanner.service` | hourly systemd timer | **skipped every hour** | `ConditionPathExists` names a script deleted in futon0 `0f67250` |

### 1 — the EFE landscape has been stale for three weeks

`daily_reembed.sh` runs under `set -euo pipefail`. `mission_efe_scope_dump.py` is the
second-to-last step; when it raised `URLError: Connection refused`, the script aborted
**before** `mission_efe_field.py embed`. So the embedded EFE field has not been
regenerated since the switchover. `data/efe-scopes.json` is dated **Jul 12 05:33** —
the switchover day. 22 connection-refused failures are in
`futon2/logs/daily-reembed.log`.

This is exactly the "material regenerated live / on demand that depends on patterns
being up to date" that Joe flagged.

### 2 — phase-5 signatures never ran

`storage/futon3/phase-5-signatures/weekly.log` ends in a babashka stack trace from
`discover-active-labels` → `http-get-edn`. Note that `futon3/CLAUDE.md` says futon3 "is
no longer a running codebase" — but a systemd timer runs a futon3 script weekly. That
contradiction is worth resolving separately.

### 3 — the vitality scanner has been a no-op

`ExecStart` points at `futon0/scripts/vitality_scanner.py`, deleted in commit `0f67250`
("move vitality config to storage and drop python scanner") and replaced by
`scripts/futon0/vitality/scanner.bb`. `ConditionPathExists` fails, systemd records
**`Result=success`**, and the timer has reported success while doing nothing. Not
repaired here: the replacement `scanner.bb:287` also defaults to `:7071`, and
re-enabling it would start new writes — that is an operator decision.

## 2. The two futon1a → futon1b API differences that bite ports

Neither is a URL change. Both were found by fixing the URL and watching what broke next.

**(a) `limit` is now validated and capped.** futon1b `futon1b_server.clj:312-313` sets
`default-result-limit 100` and `max-result-limit 5000`. A request with no `limit`
silently returns 100 rows while reporting the true population in `:count`; a request
above 5000 is rejected with a layer-4 `:invalid-limit`. futon1a had neither behaviour.
So a ported script either **crashes loudly** (limit too high) or — far worse —
**succeeds on a truncated window**.

**(b) There is no cursor, and bounded reads are N+1.** `hyperedges-query` accepts
`:type :end :limit :repo :source-file :valid-as-of :system-as-of :latest?`, but no
offset. With `code/v05/var` at **34,480** rows and a 5000 cap, that population is
**unreachable over HTTP**.

*Correction:* an earlier draft of this note said `?repo=` push-down "does not work",
because `?repo=futon0-d&limit=5000` returned nothing. That was a **client timeout**, not
a server defect — the push-down works, and `?repo=futon0-d&limit=100` returns 100 rows
all carrying `:prop/repo "futon0-d"`. The denormalized `prop/*` columns (H4) are present
on these docs.

What actually makes it look broken is latency. On the bounded path,
`hydrate-hyperedge-window` (`futon1b_graph.clj:531-541`) projects ids inside XTDB and
then issues **one `fxt/q1` per document**, batched only 4 wide:

| limit | time |
|---:|---:|
| 10 | 2.79s |
| 50 | 4.50s |
| 100 | 3.83s |
| 200 | 10.94s |
| 1002 | 92s |
| 5000 | >10 min (abandoned) |

≈50ms marginal per row. futon1b stayed healthy throughout (`{:ok true :node-open? true}`).

## 3. Population gap: the mission-scope surface

futon1b holds roughly an eighth of the mission-scope hyperedges the futon1a-era dump saw:

| binder | futon1a (Jul 12 dump) | futon1b (census today) |
|---|---:|---:|
| loose-section | 7847 | 1002 |
| source-material | 1840 | 160 |
| pattern | 934 | 44 |
| map-item | 799 | 65 |
| mission-scope-in | 682 | 47 |
| capability-scope | 677 | 91 |
| eightfold-phase | 657 | 66 |
| mission-scope-out | 567 | 38 |
| **total** | **14482** | **1545** |

Distinct missions with a `loose-section` surface: **452 → 59**.

This is *not* migration loss and *not* dedup — and, correcting a first reading, it is
*not* the dark `FUTON3C_WATCHER_SCOPE_LANE` gate either.

**There are two scope-reingest implementations, and the live one is ungated.**
`futon3c.watcher.scope-reingest/schedule!` is called unconditionally from
`file_ingest.clj:1281, 1288, 1294` on every mission / excursion / campaign doc land. So
the lane needs no switch: it has been firing all along. The `multi.clj`
mission-maintenance lane behind `FUTON3C_WATCHER_SCOPE_LANE` is a **second, redundant**
implementation of the same mechanics (its own docstring says "the same scope-lane
mechanics as scripts/mission-scope-reingest.sh"). Enabling it would double-write.
Recommendation: delete it rather than enable it.

**The reason nothing landed is the root cause below.** Every reingest has been throwing.

### The root cause: one constant, two rows short

`mission_scope_ingest.clj/hyperedges-by-type` reads a whole type at
`substrate-page-limit` and — correctly — **fails closed on truncation**:

```clojure
(when (and (integer? total) (> total (count hxs)))
  (throw (ex-info "futon1b hyperedge result truncated" …)))
```

`substrate-page-limit` was **1000**. `mission-scope/loose-section` reached **1002**.

Reproduced live: a reingest of `C-falsifiable-missions` returned
`{:status :failed :error "futon1b hyperedge result truncated"}`, and the binder-level
call reported `{:type "mission-scope/loose-section", :returned 1000, :total 1002}`.

So from roughly 2026-07, **every** scope reingest threw, no mission's scope surface could
land, and the futon1b scope surface froze at whatever had landed before the population
crossed 1000. Only 6 of 527 scope-trees have a `loose-section` surface today. That
starves `mission_efe_scope_dump.py`, which aborts `daily_reembed.sh`. **One root cause,
three symptoms** — the guard was right, and it has been shouting into a log nobody read.

**Fix applied:** `substrate-page-limit` 1000 → 5000, env-overridable via
`FUTON3C_SUBSTRATE_PAGE_LIMIT` (clj-kondo 0 errors, check-parens exit 0). Verified
end-to-end: after reloading the ns, the `C-falsifiable-missions` reingest **landed** —
`mission-scope/loose-section` went 1002 → **1021** rows and the campaign's scopes are now
queryable. (The `proof-eval.sh` invocation reported `{:ok false :error "eval timeout"}` at
its 300s client budget; the JVM-side work completed anyway. Do not read a proof-eval
timeout as a failed ingest.)

### Why the 5000 ceiling cannot survive the backfill

This is the decisive constraint on sequencing, and it is arithmetic, not judgement.

One campaign doc contributed **+19** loose-section scopes. The futon1a-era dump gives the
same order independently: 7847 loose-sections across 452 missions ≈ **17.4 per mission**.
There are **527** scope-trees to backfill. So a completed backfill implies roughly
`527 × 17.4 ≈ 9,200` loose-section hyperedges.

**That crosses futon1b's `max-result-limit` of 5000 partway through the backfill**, at
which point `hyperedges-by-type` fails closed again — with no cursor left to reach for.
The backfill would stall around the halfway mark and leave the surface in a worse,
partially-populated state.

**Therefore the cursor is a hard prerequisite for the backfill, not an optimisation.**
Raising the constant bought the ability to land *individual* reingests today; it did not
buy the ability to land all of them.

## 4. Repairs made

1. **`futon6/scripts/mission_efe_scope_dump.py`** — URL now resolves
   `FUTON_SUBSTRATE_URL` → `FUTON1A_URL` → `http://127.0.0.1:7073`; `LIMIT` 8000 → 5000
   (under the cap, above every binder population); per-fetch **truncation assertion**;
   configurable timeout (default 180s — the old 30s no longer suffices); and a
   **shrink guard** that refuses to overwrite the dump when the new one is <75% of the
   old, overridable with `EFE_SCOPE_ALLOW_SHRINK=1`.

   Verified: reaches futon1b, collects 1545 scopes, and **refuses** —
   `"refusing to overwrite efe-scopes.json: 1545 scopes vs 14482 previously (11%)"`,
   exit 1, file untouched (still Jul 12). That is the correct outcome: the fix must not
   silently shrink the EFE landscape by 89% while the scope lane is dark.

2. **`futon3/scripts/phase_5_signatures.clj`** — same URL precedence, plus a
   `fetch-complete!` guard that requests `limit=5000` and **aborts** if `:count` exceeds
   the rows returned. This script *writes* signature hyperedges; computing them from 100
   of 34,480 vars would emit plausible-but-wrong data, which is strictly worse than the
   connection-refused it replaced. Verified: exits 3 with
   `"FATAL: no 200 from …type=code%2Fv05%2Fvar&limit=5000"` before any write.

   Gates: `clj-kondo` **0 errors** (6 warnings, all pre-existing unused bindings in
   untouched code); `futon4/dev/check-parens.el` **exit 0**.

3. **`vitality-scanner.service`** — diagnosed, not repaired (see §1.3).

**Neither repair makes its job work.** Both convert a silent or misleading failure into
a loud, specific one that names the blocker. The blockers (§2, §3) need real work.

## 5. Inventory — everything else still naming futon1a

Counted over `.clj/.bb/.py/.el/.sh`, excluding `futon1a/`, `futon1b/migration*`,
`holes/`, and vendored `site-packages`.

**C — reads `FUTON1A_URL` but defaults to `:7071`** (correct when launched from
`dev-laptop-env`, wrong when run bare): futon3c 26, futon3c-index-check 15, futon3 13,
futon0 3, futon6 2, futon4 2, apm-lean 2, futon3a 1.

**D — hardcoded `:7071`, no env override** (broken however launched):
futon3c-index-check 24, futon5a 16, futon3c 16, futon6 13, futon4 7, futon2 5, futon0 4,
futon5 1, futon3b 1, futon3a 1, powerbi-tui 1.

**E — intentionally historical, leave alone:** `futon1b/migration/*`,
`futon1b/full-backfill-job*.sh`, `futon1b/seed/*`, `futon1b/hx-backfill-per-type.bb`.
These name `:7071` because they *were* the migration; the port jobs stand up a temporary
read-only futon1a on purpose.

Nothing in C or D is on a timer or cron other than the three in §1 — they are all
hand-run. That is why three weeks passed unnoticed.

**Recommended treatment, not applied:** the defaults in C are the cheap sweep — one
mechanical pass replacing `"http://localhost:7071"` with the
`FUTON_SUBSTRATE_URL`/`FUTON1A_URL`/`127.0.0.1:7073` ladder already used in
`multi.clj:37` and `substrate/client.clj:13`. D is the same edit plus adding the env
read. But per §2, a correct default is necessary and **not sufficient** — any script
that fetched an unbounded type window needs a pagination story before it can be trusted,
and there is currently no cursor to write one against. Fixing `?repo=` push-down in
futon1b is probably the smaller lever and unblocks the most callers.

## 6. The generalisable lesson

Every one of these three failed *quietly for weeks* in a different way: an aborted
`set -e` chain whose log nobody read, a systemd unit that logged a stack trace to a file
nobody read, and a unit that reported **success** while skipping. The premise that
started the sweep ("multi_watcher not running") was false, and the class it named was
real. What is missing is not a fix to any one job but a **liveness assertion per
scheduled job** — the watcher has one (`:freshness`, which is how its health was
confirmed in seconds); none of these three does.
