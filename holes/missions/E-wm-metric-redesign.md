# Excursion: WM Metric Redesign (saturation + semantic-mismatch)

**Type:** Excursion (E-prefix; bounded scope-out from a mission; owned end-to-end by a single agent; see [[project_e_prefix_excursions]] for the convention).
**Status:** SCOPED by claude-1 (NOT handed off — claude-1 finishes this one, following the E-wm-live-recommendation pattern).
**Date:** 2026-05-25
**Author + end-to-end owner:** claude-1 (inhabiting `:war-machine-pilot` peripheral; emacs-repl surface paired with Joe).
**Parent mission:** `futon3c/holes/missions/M-war-machine-pilot.md` (v1 cycle).
**Sibling excursions:** `E-wm-live-recommendation.md` (made the surface live), `E-wm-staleness-meta-stop.md`, `E-night-shift.md`, `E-pattern-mining.md`, `E-pilot-vsatarcs-feed.md`, `E-street-sweeper.md`.
**Provenance probe (already shipped by claude-2):** `futon2/web/war-machine/wm-metric-provenance-verify.mjs`.
**Surface that authored this:** emacs-repl 2026-05-25 between claude-1 and Joe, after claude-2's diagnostic round.

## Why this exists (Joe, emacs-repl 2026-05-25)

Joe paired with claude-2 on a UI quality-control round and surfaced his discomfort:

> *"A 100% rating probably can't be acted upon... however, there's another register that says 'Coordination → Self-representation 24%' with another tooltip and still no direct guidance on what to do about it.  I worry that these numbers are not actually sourced from the AIF 'last mile' work with the R1-R12 features that we built.  If they aren't useful for me I don't see how they can be useful for a pilot."*

claude-2's diagnostic round (`wm-metric-provenance-verify.mjs`, 2026-05-25) returned:

- **provenance_clean: true** — every rendered number traces to substrate via formula at `futon2/scripts/futon2/report/war_machine.clj:459`.  **There is no confabulation.**
- **3 of 6 loop-health arrows are saturated** at 100% — `work→proof` (count 316), `proof→patterns` (count 28), `self-rep→inference` (count 33).  Formula: `health = sqrt(freq × fresh)` where `freq = min(1, count/10)` and `fresh = max(0, 1 − days-since/window-days)`.  Once `count ≥ 10` AND `days-since = 0`, the metric locks at 1.0 regardless of further activity.
- **1 workstream has a semantic caveat** — `consulting = 0%` measures git commits to consulting-classified repos in the last 14 days.  Does not capture invoice authoring (`~/code/invoices/log.edn`), uncommitted `~/vsat.wiki` work, calendar work, document drafting, email-sends, or anything outside the git substrate.

Joe acknowledged neither the 14-day window nor the git-commit-only measurand bubbles up in the rendered tooltips.

This excursion finishes the redesign claude-2's round opened (and that was never written up as an excursion until this one).

## The two failure modes (in claude-2's terms)

### F1. Saturation — the formula locks at 100% under modest activity

```
health(count, days-since, window-days) = sqrt(freq × fresh)
freq  = min(1, count / 10)
fresh = max(0, 1 − days-since / window-days)
```

For any loop-health arrow with `count ≥ 10` and `days-since = 0`, `health = 1.0` regardless of further activity (count could be 10 or 10000; saturated all the same).  This is **honestly unactionable**: the metric is reading real substrate but giving the operator no signal about which arrows to invest in.

Half the loop-health arrows live in this regime permanently because most active loops easily produce 10+ commits in 14 days.

### F2. Semantic mismatch — measurand ≠ operator's question

`consulting = 0%` is the canonical example.  The metric measures *git commits to consulting-classified repos in the last 14d*, but the operator's question is *am I doing consulting work*.  Real consulting evidence (`~/code/invoices/log.edn` engagements + work-items + invoices) is not part of the metric's measurand.  The metric is honest about what it measures, but what it measures is a poor proxy for what the operator cares about.

By extension, any workstream whose evidence lives outside-of-git-commits will exhibit the same misfit.

## What this excursion does NOT own

- **The provenance probe itself** — it's already shipped by claude-2 (`wm-metric-provenance-verify.mjs`).  This excursion *uses* the probe as the QC harness and may extend it with assertions for the redesigned metrics.
- **Adding more widgets to the probe** — claude-2's "extend to Self-watch / Mission triage / Portfolio / Sessions" suggestion (item 1 in the original handoff offer) is a separate excursion if useful.
- **The `scan-consulting` wiring to `~/code/invoices/log.edn`** — that's Track A in claude-2's multi-track table and is M-interim-director-proxy-metric-inventory Arm D's responsibility.  This excursion specifies the *metric shape* that wiring should target, but the wiring itself is downstream work.
- **Authoring billable-hours dashboard UI** — once `scan-consulting` exists and emits hours-per-window, the cljs side adds a billable-hours panel; that's a follow-on once shape is fixed here.
- **Joe's invoice-004 / vsat.wiki eoi work** — Tracks B + C from the original handoff are operator-lane, not WM-redesign.

## Scope (the redesign)

### (1) Replace saturating frequency with a non-saturating shape

Three candidates worth evaluating:

| Option | Formula | Saturation? | Notes |
|---|---|---|---|
| **log-scale** | `freq = log10(1 + count) / log10(1 + cap)` (cap parameter, e.g. 100) | No, but flattens after the cap | Cheapest change; keeps a `[0, 1]` range |
| **z-score-vs-baseline** | `freq = sigmoid((count − μ_hist) / σ_hist)` where `μ_hist`/`σ_hist` come from a rolling historical baseline per arrow | No | Most actionable — tells you "this arrow is unusually busy / quiet for itself"; needs baseline storage |
| **rate-and-acceleration** | Two numbers: `count/window-days` (rate) + `(count_now − count_prev) / count_prev` (acceleration) | Conceptually different — no single 0–100% number | Most honest — but the UI needs to render two dimensions, not a percentage |

**Recommendation:** start with **log-scale** as a v1 because it's the smallest change that breaks saturation; defer **z-score-vs-baseline** as a v2 once baseline storage exists.  The third option is a separate UI-redesign question.

The cap parameter and base should be visible in the tooltip per F1's diagnosis.

### (2) Surface measurand + window in every tooltip (F2 partial)

Every tooltip on a loop-health arrow or workstream-pct widget must name:
- The measurand (e.g., "git commits to repos with `:workstream :stack`")
- The window (e.g., "in the last 14 days")
- The formula (e.g., "log-scaled frequency × freshness")

This is cheap and high-value: it gives the operator the basis for trusting OR rejecting the number.  Probe extension: add `tooltip-completeness` assertion (checks tooltip text includes measurand keyword + window number).

### (3) Pluggable workstream-classifier extension point (F2 main)

For workstreams whose evidence lives outside git, the WM should accept pluggable detectors.  v1: a multimethod or registry in `futon2/scripts/futon2/report/war_machine.clj` keyed on `:workstream` that returns the workstream-evidence count for a window.  Default impl reads git commits; the `:consulting` impl reads `~/code/invoices/log.edn` (delegated to M-interim-director-proxy-metric-inventory Arm D when it lands).

Out-of-scope to *implement* per-workstream detectors here — but the extension point itself is owned by this excursion.

### (4) Update `:next-move-live` to consume the redesigned metric

The live recommendation surface from E-wm-live-recommendation reads `judgement.priorities` (channel-gap entries).  When `loop-health`/`workstream-pct` saturate at 100%, those entries are no longer informative to the EFE computation either.  After (1) lands, the priorities should re-rank realistically.  Smoke test: top-3 priorities should not include any saturated-at-100% loop-health arrow.

### (5) Probe extension

`wm-metric-provenance-verify.mjs` currently emits `saturated: true` and `semantic_caveat: ...` flags.  Extend its exit-code semantics so:

- `exit 0` = provenance clean AND no saturation AND no semantic-caveats (currently exit 0 just needs provenance clean)
- `exit 1` = provenance clean BUT saturation OR semantic-caveats remain
- `exit 2` = confabulation detected

After redesign lands, the probe should exit 0 cleanly.

## Implementation plan (claude-1 ownership)

| Step | Where | Who |
|---|---|---|
| (1a) Log-scale frequency rewrite | `futon2/scripts/futon2/report/war_machine.clj` around line 459 | claude-1 |
| (1b) Unit test asserting non-saturation on count={1, 10, 100, 1000} | `futon2/test/futon2/report/war_machine_test.clj` | claude-1 |
| (2) Tooltip text updates (cljs) for loop-health arrows + workstream-pct widgets | `futon2/web/war-machine/src/war_machine/client/core.cljs`, `hud.cljs` | claude-1 |
| (3) Workstream-classifier extension point | `war_machine.clj`, mark `:consulting` as `:external-classifier-required` so it doesn't render as 0% on commits | claude-1 |
| (4) Smoke test against `:next-move-live` priorities | run after (1) + observe top-3 | claude-1 |
| (5) Probe exit-code extension | `wm-metric-provenance-verify.mjs` | claude-1 |
| (6) Re-run probe end-to-end; commit with summary diff (3 saturated → 0; 1 semantic-caveat → 0 or "external-classifier-required") | both repos | claude-1 |

cg-id: to be minted at start of implementation; parent = either `cg-fb78973a` (the original pilot inhabitation cg) or a fresh root cg for this excursion.  Operator-decision.

## Success criteria

1. **Saturation gone**: `wm-metric-provenance-verify.mjs` reports `loop_health_saturated_count: 0` (was 3).
2. **Tooltip completeness**: each loop-health arrow tooltip names measurand + window + formula; probe asserts this.
3. **Semantic caveat moved or removed**: either `consulting` has a non-commit-based measurand wired in (delegated to M-interim-director Arm D), OR it renders as `consulting: classifier-required` with a tooltip pointing at the extension point — NOT as a false `0%`.
4. **Live recommendation un-saturated**: `judgement.priorities` top-3 in `/api/alpha/aif-stack/live` contains no saturated-at-100% loop-health arrows.
5. **Tests pass**: existing `futon2.report.war_machine_test` plus new non-saturation cases all green.
6. **Probe exits clean**: `node wm-metric-provenance-verify.mjs; echo $?` returns `0` (was returning effectively-`1` per the saturation/semantic flags).

## Cross-references

- `futon2/web/war-machine/wm-metric-provenance-verify.mjs` — claude-2's provenance probe; the QC harness
- `futon2/scripts/futon2/report/war_machine.clj:459` — `health` formula site (saturating `freq = min(1, count/10)`)
- `futon2/scripts/futon2/report/war_machine.clj:77-95` — workstream manifest (notes the exclusion of `~/code/invoices` and `~/code/statements`)
- `futon2/web/war-machine/src/war_machine/client/core.cljs` + `hud.cljs` — loop-health + workstream-pct rendering surfaces
- `futon3c/holes/missions/E-wm-live-recommendation.md` — sibling excursion; same shape (claude-1 author + owner, finishes in-session)
- `futon7/holes/M-interim-director-proxy-metric-inventory.md` Arm D — `scan-consulting` wiring to `log.edn`
- `~/code/invoices/log.edn` — canonical consulting ledger; consumed by the future `:consulting` workstream-classifier impl
- [[feedback_use_playwright_for_ui_verify]] — UI changes verified via Playwright before reporting done
- [[feedback_no_synchronous_heavy_drawbridge_calls]] — code reloads via Drawbridge; no synchronous heavy ticks

## Provenance

- Problem surface: Joe, emacs-repl 2026-05-25, "I doubt almost everything in the UI (even with tooltips)" — directed at claude-2
- Diagnostic round: claude-2, 2026-05-25, produced `wm-metric-provenance-verify.mjs` + `provenance_clean: true` finding + saturation/semantic-mismatch failure-mode taxonomy
- Joe acknowledged neither 14-day window nor git-commit-only measurand surface in tooltips ("I didn't realise it was 'within 14 days', nor that it was wholly driven by git commits")
- Joe diverted to Track C (EOI scenario authoring); E-wm-metric-redesign was identified by both as the follow-up but never written
- This excursion file authored: claude-1, 2026-05-25, after Joe (emacs-repl) noted "I don't think E-wm-metric-redesign was ever written — it was a good idea though"
