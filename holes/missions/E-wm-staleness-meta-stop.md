# Excursion: WM Staleness Meta-Stop (proper fix)

**Type:** Excursion (E-prefix) — bounded scope-out from M-bounded-in-flight-state §D-03 and M-war-machine §metabolic-balance.
**Status:** SCOPED, awaiting owner assignment (claude-2 authored, will execute unless reassigned)
**Date:** 2026-05-25
**Author:** claude-2 (inhabiting emacs-claude-repl, paired with Joe)
**Trigger:** operator-clear sentinel landed 2026-05-25T17:18Z; this excursion replaces the band-aid with the structural fix before the sentinel TTL (2026-05-27T17:18Z) elapses.

## Why this exists

The War Machine emitted `:stop-the-line` mode override **on stale data** for an unknown window — `~/code/storage/futon0/mana-snapshot.json` had not been refreshed since 2026-05-04 (21 days). Downstream consumers (CLJS banner, hop triggers, E-street-sweeper trigger condition) honored the override as authoritative. The `:stale?` flag was computed (`(> age-min 60.0)` in `scan-metabolic-balance`) and exposed in the payload, but never consulted by the judge step. This is "logging without propagating" per CLAUDE.md §9.

Compounding: **three tier-threshold maps disagree silently**.

| Map | source | silent | advisory | high | stop-the-line |
|---|---|---|---|---|---|
| Producer | `futon0/scripts/mana-snapshot.bb:81-86` | <1 | 1–2 | 2–4 | **≥4** |
| Consumer | `futon2/scripts/futon2/report/war_machine.clj:1324-1333` (docstring claims they match!) | <1 | 1–3 | 3–10 | **≥10** |
| JVM check-fn | `futon3c.logic.metabolic-balance/compute-channel-pressure` (referenced by both as truth) | TBD | TBD | TBD | TBD |

Consumer inherits the producer's pre-computed `:tier` label (line 1437: `(or (some-> (:max-tier snap) keyword) (pressure->tier ...))`), so the divergence is silent — producer always wins. The consumer's docstring lies.

## Current state (after band-aid)

- **Sentinel file** at `~/code/storage/futon0/wm-operator-clear.edn` with `:until 2026-05-27T17:18:36Z`.
- **Payload veneer** in `futon3c/src/futon3c/transport/http.clj` (`apply-wm-operator-clear`) downgrades `:stop-the-line` tiers + judgement mode to `:high` / `:recovery` and embeds the sentinel under `:operator-clear` so the override is visible.
- **Mana snapshot** has been refreshed once manually (2026-05-25T17:09Z) but will decay back to stale without a scheduler.
- **Real picture**: max P=6.00 (futon4 with 120 dirty files, 19d-old max-age, was completely blind-spotted by the stale snapshot). Significant accumulation across the stack but not "drop-everything" pressure.

## Scope (proper fix)

### (1) Staleness gate in judge step

In `futon2/scripts/futon2/report/war_machine.clj` `judge` (~line 2897-2900):

```clojure
;; CURRENT (silently honors stale data):
metabolic-max-tier (get-in scan-data [:metabolic-balance :max-tier])
mode (if (= :stop-the-line metabolic-max-tier)
       :stop-the-line
       base-mode)

;; FIX (honors :stale? as gate):
metabolic-max-tier (get-in scan-data [:metabolic-balance :max-tier])
metabolic-stale?   (get-in scan-data [:metabolic-balance :stale?])
mode (cond
       metabolic-stale?                      base-mode   ;; or :stale-data
       (= :stop-the-line metabolic-max-tier) :stop-the-line
       :else                                 base-mode)
```

Also surface the staleness in the judgement output (`:metabolic-stale? true` and/or `:override-suppressed-reason :stale-data`) so downstream consumers can render appropriate UI.

### (2) Scheduler for mana-snapshot.bb (D-03 closure)

M-bounded-in-flight-state §D-03 asked "how does `mana-snapshot.bb` keep the JSON fresh?" — options were cron / systemd-timer / on-demand. Pick one and land it:

- **Recommended:** systemd-timer at 5-minute cadence (matches the 60-min `:stale?` threshold with 12x headroom). Job file at `~/.config/systemd/user/mana-snapshot.{service,timer}`.
- **Alternative:** crontab `*/5 * * * * bb /home/joe/code/futon0/scripts/mana-snapshot.bb >/dev/null 2>&1`.

Either way: capture stderr to a log file so silent failures are detectable. Add a healthcheck endpoint or HUD widget that surfaces "last successful snapshot N min ago" so a stuck producer is visible without operator-side polling.

### (3) Reconcile producer / consumer / JVM tier thresholds

Three independent tier-map definitions, one source of truth needed. Steps:

1. Read `futon3c.logic.metabolic-balance/compute-channel-pressure` (the JVM "authoritative" reading per producer's comment) and document its tier thresholds.
2. Pick one canonical threshold map (recommendation: the producer's, since it's already what behavior reflects). Document the rationale in `war-machine-strategic-vocabulary.edn`.
3. Update the other two sites to either delegate to the canonical map or carry an identical literal with a cross-reference comment.
4. Update the consumer's `pressure->tier` docstring to match reality (currently lies).
5. Add a property-based test that asserts agreement: feed a range of P values into all three; assert identical tier output.

### (4) Retire the operator-clear sentinel

Once (1)+(2)+(3) land and the proper signal is trustworthy:

- Delete `~/code/storage/futon0/wm-operator-clear.edn`.
- Verify the WM endpoint returns un-veneered data.
- Leave `apply-wm-operator-clear` and the sentinel mechanism IN the code — it's a legitimate operator-side mute that may be needed again (e.g., during planned maintenance, or when sweeping a known-large pile).
- Document the sentinel mechanism in the futon3c README so operators know it exists and how to use it.

## Success criteria

1. **Staleness gate**: `judge` does not propagate `:stop-the-line` mode when `:stale?` is true. Test in `futon2/test/futon2/report/war_machine_test.clj` covers stale + stop-the-line input → mode is base-mode + override-suppressed-reason recorded.
2. **Scheduler running**: `stat ~/code/storage/futon0/mana-snapshot.json` shows mtime < 10 minutes ago at any point of inspection. `systemctl --user status mana-snapshot.timer` reports active.
3. **Tier reconciliation**: a property test asserts all three pressure→tier maps return identical tier for at least 100 sampled P values across [0, 20]. Docstrings reflect actual behavior.
4. **Sentinel retired**: WM endpoint shows `:operator-clear nil` (or field absent) and the data is honest (whatever the real tier is, that's what it shows).
5. **Cycle test**: deliberately stop mana-snapshot.timer for 2 hours, observe WM endpoint show `:metabolic-stale? true` AND `:judgement :mode` NOT `:stop-the-line` regardless of `:max-tier`. Re-enable timer, observe mode resume normal classification within one cadence cycle.

## What this excursion does NOT own

- **Producing the unified threshold map content** — that's an operator-judgement call on thresholds (e.g., "should P=4 really be stop-the-line, or is 6 closer to the felt sense of emergency?"). Surface options to Joe; he decides.
- **Choosing systemd-timer vs cron** — operator-environment preference; recommend systemd-timer for user-scope ergonomics but defer.
- **Backfilling old WM trace data** that was emitted under the miscalibration — past trace is past trace; do not rewrite.
- **The CLJS banner code** itself — its current "mode=stop-the-line → render red banner" logic is correct; the fix is to feed it accurate mode values.

## Cross-references

- `~/code/storage/futon0/wm-operator-clear.edn` — the sentinel currently active (TTL: 2026-05-27T17:18Z)
- `futon3c/src/futon3c/transport/http.clj` — `apply-wm-operator-clear` + helpers (the band-aid implementation)
- `futon2/scripts/futon2/report/war_machine.clj` lines 1324 (`pressure->tier`), 2897-2900 (`judge` override logic), 1391 (`scan-metabolic-balance`)
- `futon0/scripts/mana-snapshot.bb` — the producer, lines 81-86 for tier thresholds
- `futon3c/holes/missions/M-bounded-in-flight-state.md` §D-03 — original D-03 question this excursion closes
- `futon3c/holes/missions/M-war-machine.md` §metabolic-balance — drain-channel-shape spec
- `futon5a/data/war-machine-strategic-vocabulary.edn` — `:μ/override-modes` (`:stop-the-line` entry; semantics)
- `futon2/web/war-machine/src/war_machine/client/core.cljs:1282` — `stop-the-line-banner` (downstream consumer; reads `:judgement :mode`)
- `futon3c/holes/missions/E-street-sweeper.md` — parallel excursion whose trigger condition (`:stop-the-line` mode override on `:working-tree` channel) was the entrypoint that surfaced this meta-stop

## Provenance

- Meta-stop detected: claude-2 in emacs-claude-repl, 2026-05-25T17:13Z, while pre-flighting the E-street-sweeper sweep
- Reframe authored: Joe via emacs-repl, 2026-05-25T17:14Z ("we need to fix the erroneous out-of-date signals in the war machine itself")
- Operator-clear band-aid landed: claude-2 via Drawbridge reload, 2026-05-25T17:24Z
- This excursion file authored: claude-2, 2026-05-25T17:25Z, same session
