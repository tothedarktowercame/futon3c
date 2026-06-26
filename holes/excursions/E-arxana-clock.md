# Excursion: the Arxana Clock — one surface for the system's time-drivers (E-arxana-clock)

**Date:** 2026-06-26 · **Status:** IDENTIFY + MAP grounded in live inventory (2026-06-26) — owned end-to-end by one agent (claude-10).
**Authored by:** claude-10.
**Parent / relates:**
[[README-clicks-and-ticks]] (futon2 — the clicks/ticks conceptual frame this realises) ·
[[M-cyder]] (futon3c — the in-JVM process registry, the keystone source) ·
[[M-autoclock-in]] (the agent clock-in / click concept) ·
[[E-C-vector-live]] (a *consumer*: the serving-JVM belly refresh wants to ride a turn/tick trigger this excursion provides + appear on the clock).
**Repos:** futon3c (`src/futon3c/cyder.clj` — registry; transport/http — endpoint; scripts — the aggregator CLI) · futon2 (`README-clicks-and-ticks.md` — the frame) · system (`crontab -u joe`, `systemctl --user` timers).

---

## HEAD

The system's running is kept alive by a set of **time-drivers** — but they are scattered across **three disjoint mechanisms** with **no unified view**, and Joe can't say how many are installed (2026-06-26). The Arxana Clock is the single surface that answers, at a glance: *what keeps this system ticking, when did each last fire, and when does it fire next?* — and the place a **new** driver (e.g. an every-≈100-turns trigger) registers so it's visible rather than hidden.

### The live inventory (2026-06-26, the gap made concrete)

- **Ticks · wall-clock, external:** 3 cron entries (`wm-scheduled` hourly · `wm-outer-loop` daily 04:00 · futon3a corpus daily 04:30) + 3 futon systemd user timers (`mana-snapshot` every 5 min · `vitality-scanner` ~hourly · `phase-5-signatures-weekly`).
- **Ticks · in-JVM:** `futon3c.cyder` registry holds 13 processes, but only `multi-watcher` + `process-watchdog` are actively periodic; the rest are servers/daemons/state-machines/agent-lanes. **No cyder record carries a cadence/next-fire — all "—".** The wm/portfolio/probe schedulers exist in code but aren't currently started.
- **Clicks · engagement:** invoke-ledger (futon1a), forum posts, mission-edits, agent clock-ins — engagement-driven, not installed as triggers. "Every ≈100 turns" is a *click* trigger (the `:turn` subclass of README-clicks-and-ticks).

So: ~9 active drivers, 3 mechanisms, zero unified visibility, no next-fire tracked anywhere, and no Arxana-Clock surface exists.

### The question

**What is the one read-only surface that normalises cron + systemd + cyder (+ clicks) into a single legible clock — and what is the minimal turn/tick-trigger mechanism a new driver (the belly refresh first) registers on so it both fires and shows up?**

### Discipline this inherits (read first)

- **Read-only aggregator; never fabricate a field.** If a source doesn't expose `last-fired`/`next-fire`, render `—`, don't invent it (the substrate-2 "never assert without evidence" discipline).
- **Never restart the serving JVM** (I-0); read cyder via Drawbridge / in-process, reload via `load-file`.
- **No heavy work in the request path** (`feedback_no_synchronous_heavy_drawbridge_calls`): the clock samples off-cycle / on demand, the turn-trigger is debounced, not a per-request scan.

---

## 1. IDENTIFY — the gap

Three mechanisms, each with its own truth and none aware of the others; nobody computes "next fire" for the in-JVM ones; and there is no home for a turn-counted driver. The cost: drivers freeze silently (the 5-week substrate-2 freeze was exactly an unwatched tick) and new periodic work has nowhere legible to live.

## 2. MAP — sources + what each already exposes

| Source | Read via | Gives for free | Missing |
|---|---|---|---|
| cron | `crontab -l` | cadence (expr), command, log path | last-fired, next-fire (compute from expr) |
| systemd timers | `systemctl --user list-timers --all` | **NEXT + LAST** (next/last-fire), unit, description | what-it-does (read .service ExecStart) |
| cyder (in-JVM) | `futon3c.cyder/list-processes` | id, type, layer, last-active | cadence + next-fire (add to `:metadata`) |
| clicks | futon1a invoke-ledger | last invoke, count | turn-count trigger (build it) |

## 3. DERIVE — the design (framed)

1. **The normalized driver record** (the clock's row):
   `{:name :mechanism (cron|systemd|cyder|click) :what :cadence :last-fired :next-fire :status (live|stale|idle|unknown)}`. `—` for genuinely-absent fields.
2. **The aggregator** — a read-only function that unions the four sources into a sorted list (by next-fire where known). Lives so both a CLI and an HTTP endpoint can call it.
3. **The display** — an Arxana / WebArxana panel reading the aggregator (car 2).
4. **The turn/tick trigger** — a click-counter driver (per README clicks): fires every N clicks/turns; registered in cyder so it appears on the clock. The serving-JVM **belly refresh** is its first rider (E-C-vector-live's "alive in the running system").
5. **Cadence enrichment of cyder** — periodic cyder processes declare `:cadence` (+ optionally `:next-fire-fn`) in `:metadata` so the clock isn't all "—" for the in-JVM layer.

## 4. ARGUE — decisions to ratify

- **Read-only aggregator first, mechanism second** (Joe-ratified plan): the inventory/display is immediate value and risk-free; the turn-trigger is a new mechanism with its own discipline.
- **Aggregate, don't centralise** — cron/systemd stay where they are (OS-owned); cyder stays the in-JVM registry. The clock *reads* all three; it does not become a new scheduler that owns them.
- **The turn-trigger is a click-counter, not a new wall-clock** — it belongs to the clicks substrate; debounced; registered + visible.

## 5. Exit conditions (provisional)

1. A reproducible **aggregator** (not `/tmp`) emits the unified driver list across cron + systemd + cyder, with `—` for absent fields — re-runnable on demand. *(car 1)*
2. An **Arxana Clock display** renders it (mechanism-coloured, sorted by next-fire), updating live. *(car 2)*
3. A **turn/tick-trigger** mechanism fires a registered driver every N clicks, appears on the clock, and the **serving-JVM belly refresh** rides it. *(car 3)*
4. No fabricated `last-fired`/`next-fire` anywhere; absent = `—`.

## 6. Cars (sequence)

1. **✅ Read-only aggregator** — `futon3c/scripts/arxana_clock.bb` (committed `dddb65b`). 21 drivers / 3 mechanisms / 19 futon-relevant; writes `arxana-clock-snapshot.edn`.
2. **✅ The display** — `futon4/dev/arxana-vsatarcs-clock.el` (branch `e-arxana-clock`, commit `c3c8490`). Regular Emacs Arxana (NO WebArxana, per Joe), sibling to `arxana-vsatarcs-ledger.el`; reads the snapshot via the shared EDN reader; grouped by mechanism with cadence·next·last; `g`=refresh (re-runs aggregator), `f`=toggle non-futon. Headless-render verified (19 drivers). `M-x arxana-clock-browse`.
3. **The turn-trigger + belly-refresh rider** + cyder cadence enrichment — the click-counter driver (every ≈N turns), registered in cyder so it shows on the clock; the serving-JVM belly refresh is its first rider. **Next.** Also: cyder records carry no `:cadence`/next-fire today (all "—") — enrich the periodic ones + add a stale-driver alarm (the 5-week-freeze lesson).

## 7. Scope-out (named)

The full perceived-time R7 convolution (clicks-and-ticks → precision) stays with futon2's R7 roadmap; making the WM an inhabitable peripheral (README §"WM as peripheral") is separate; this excursion is only the **clock surface + the turn-trigger that feeds it**.
