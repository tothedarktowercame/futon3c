# Excursion: WM Live Recommendation (next-move from judgement.ranked-actions)

**Type:** Excursion (E-prefix; bounded scope-out from a mission; owned end-to-end by a single agent; see [[project_e_prefix_excursions]] for the convention).
**Status:** SCOPED + IN PROGRESS by claude-1 (NOT handed off — claude-1 finishes this one).
**Date:** 2026-05-25
**Author + end-to-end owner:** claude-1 (inhabiting `:war-machine-pilot` peripheral; emacs-repl surface paired with Joe).
**Parent mission:** `futon3c/holes/missions/M-war-machine-pilot.md`.
**Sibling excursions (also out-of-band from M-war-machine-pilot):** `E-street-sweeper.md` (claude-2-owned), `E-night-shift.md` (codex-2-owned), `E-pilot-vsatarcs-feed.md` (codex-owned), `E-pattern-mining.md` (codex-owned), `E-wm-staleness-meta-stop.md` (claude-2-authored).
**Surface that authored this:** emacs-repl 2026-05-25 between claude-1 and Joe.

## Why this exists (Joe, emacs-repl 2026-05-25)

After claude-1 attempted three different "12-hour pilot plans" and Joe rejected each as "a plan made by you, not the WM," investigation showed why: the surface the cljs `next-move-tile` renders — `:reading :next-move` — is **a static cached prose field** in `/home/joe/code/futon5a/holes/stories/THE-STACK.aif.edn`, last edited 2026-04-22 (33 days ago at time of writing). It is not recomputed when:

- an agenda is stepped (e.g. cg-fb78973a-... stepped `wm.close-s6.v1` 2026-05-25);
- an action completes;
- the WM scheduler ticks (cadence 300 s);
- mission registry shifts;
- pattern activations update.

Joe's framing (verbatim):

> *"The WM is really not suitable for inhabitation by a pilot yet — I reckon that it should always have a recommended next move but it needs to be up to date. The equivalent for AIF ants of the current model would be if the recommended next move was always :forage — nothing would get done, all ants would die."*

And on ownership:

> *"I'm happy for you to write E-wm-live-recommendation — but don't hand it off, finish it yourself. You are the candidate pilot... overall goal is to make the WM fit for inhabitation by a pilot."*

That is the gap this excursion closes: a War Machine that **actually changes its mind** between ticks based on what the pilot does, so the inhabitation loop becomes iterative rather than the pilot enumerating their own plan.

## Current state (what is broken)

Two endpoints are involved, and the cljs reads from the wrong one for "what should I do next":

| Endpoint | Surface | Freshness | Source |
|---|---|---|---|
| `GET /api/alpha/aif-stack/live` | `:reading :next-move` (rich prose) | **STATIC 33 days** | `THE-STACK.aif.edn` cached prior |
| `GET /api/alpha/war-machine?days=14` | `:judgement :ranked-actions` (219 entries; per-tick) | **LIVE 5-minute cadence** | WM scheduler `tick!` → EFE computation |

The cljs `next-move-tile` (`futon2/web/war-machine/src/war_machine/client/core.cljs:398-593`) reads only `(get-in aif [:reading :next-move])` — the cached prose. The 219 live ranked actions, computed every scheduler tick from `judgement.ranked-actions`, **never reach any UI surface for "what should I do next."**

A sample of what the live surface already knows (curl 2026-05-25T~21:30Z):

```
rank 1  address-sorry  sorry/wm-aif-substrate-addressability        G=-4.99  rationale: meta-sorry registers itself
rank 2  address-sorry  sorry/r3a-likelihood-loop-health             G=-4.39  rationale: R3a likelihood model
rank 3  address-sorry  sorry/r3a-likelihood-support-coverage        G=-4.39
rank 4  address-sorry  sorry/r3a-likelihood-attack-coverage         G=-4.39
rank 5  address-sorry  sorry/r3a-likelihood-stack-pct               G=-4.39
```

Meanwhile the prose says "Close 🐜6" — a recommendation that **was reasonable in April** but does not reflect what has actually happened since (S6 has been stepped; agendas have shifted; sorry pressure dominates).

This is the AIF analogue of an ant colony where `recommended-next-move` is a poster on the wall.

## Scope (the structural fix)

### (1) Add `:next-move-live` to the aif-stack/live response, derived from `judgement.ranked-actions`

In `futon3c/src/futon3c/aif/stack_generator.clj` add a new step in `generate-live` that:

- looks up the latest cached WM snapshot via `(requiring-resolve 'futon3c.wm.scheduler/snapshot-for-days)` with the default window (`14`);
- extracts `[:payload :judgement :ranked-actions]` (top N, default 5);
- extracts `[:payload :judgement :priorities]` (the channel-gap reasoning that drove the ranking);
- extracts `[:payload :judgement :mode]` (so PI's `:wait` / `:work-on` mode is visible);
- projects top-1 ranked-action into the same shape that the cljs `next-move-tile` already understands (`:close`, `:specifically`, `:rationale`, `:alternatives-considered`, `:source`, `:as-of`);
- attaches as `(assoc-in stack [:reading :next-move-live] live-recommendation)`;
- emits a `:freshness-warning` block (with `:cached-prose-mtime`) under the cached `:reading :next-move` so the operator can see when prose is stale.

Helper: `derive-next-move-live` taking the snapshot, returning either a populated `:next-move-live` map or nil (in which case the cljs tile falls back to cached prose with a warning banner).

### (2) Project ranked-action → next-move shape

Top-1 ranked-action is structurally `{:type "address-sorry" :target "sorry/foo" :rationale "..." :G-total <num>}`. Project as:

```clojure
{:close nil                              ; live actions are not 🐜N-indexed; that's a cached-prior thing
 :action (:action top)                   ; the full action map (type, target, intrinsic-value, rationale)
 :G-total (:G-total top)
 :rank 1
 :specifically (action->specifically top) ; e.g. "address sorry/wm-aif-substrate-addressability"
 :rationale (or (-> top :action :rationale)
                "Top of judgement.ranked-actions for this tick")
 :alternatives-considered                ; next 4 ranked actions
   (->> (rest (take 5 ranked-actions))
        (map-indexed (fn [i a]
                       [(keyword (str "rank-" (+ 2 i)))
                        (str (-> a :action :type) " " (-> a :action :target)
                             " (G=" (:G-total a) ")")]))
        (into {}))
 :priorities priorities                   ; full :judgement :priorities for context
 :mode mode                               ; :wait / :work-on / :stop-the-line / etc.
 :source :wm-judgement-ranked-actions
 :as-of <snapshot-as-of>
 :scheduler-period-seconds 300
 :stale? false                            ; computed from (now - as-of) vs period
 :note "Recomputed every WM scheduler tick (default 300s). See E-wm-live-recommendation.md."}
```

### (3) Mark the cached prose as the cached prior

In the same `generate-live`, after projecting `:next-move-live`, transform the existing cached prose:

```clojure
(update-in stack [:reading :next-move]
           (fn [cached]
             (when cached
               (assoc cached
                      :freshness-warning
                        {:source :the-stack-aif-edn-cached-prior
                         :path "futon5a/holes/stories/THE-STACK.aif.edn"
                         :mtime "<file mtime>"
                         :age-days <computed>
                         :note "Static cached prior; not recomputed per tick. See :reading :next-move-live for live recommendation."}))))
```

### (4) Wire the cljs `next-move-tile` to prefer the live surface

In `futon2/web/war-machine/src/war_machine/client/core.cljs:398-593`:

- read `:next-move-live` first; fall back to `:next-move` only when live is absent;
- when both exist, render the live tile with a small "live · last refreshed Nm ago" badge derived from `:as-of`;
- when only cached exists (e.g. WM scheduler not yet warm), render the cached tile with the **freshness-warning banner** at the top in operator-visible chrome (e.g. amber stripe);
- never render the cached prose as if it were live.

### (5) Pilot loop reads live surface only

In `futon3c/src/futon3c/peripheral/war_machine_pilot_backend.clj` `:wm-api-query` (and any pilot-side "what should I do next" tool), prefer `:next-move-live` and refuse to recommend from the cached prose alone — emit `:warning "live-recommendation-unavailable; cached prior is N days stale; will not act"` instead.

### (6) Test: live recommendation shifts on action

Acceptance test (against the running JVM):

1. Capture `:next-move-live :action :target` at T0.
2. Address one sorry (or step one agenda) such that ranked-actions reorders. This can be a concrete pilot action mediated through E-night-shift — e.g. update the sorry-registry entry for `sorry/wm-aif-substrate-addressability` to `:resolved`.
3. Wait for the next scheduler tick (or `(request-tick!)`).
4. Capture `:next-move-live :action :target` at T1.
5. Assert T1.target ≠ T0.target.

Lighter dev-loop variant: stub the wm-snapshot atom in REPL with a manipulated `:ranked-actions`, hit `/api/alpha/aif-stack/live`, observe `:next-move-live` matches.

## Implementation plan (claude-1 ownership)

| Step | Where | Who |
|---|---|---|
| (1) `derive-next-move-live` helper + integration into `generate-live` | `futon3c/src/futon3c/aif/stack_generator.clj` | claude-1 |
| (2) Cached-prose `:freshness-warning` overlay | same file | claude-1 |
| (3) cljs tile: prefer live, fallback to cached with warning | `futon2/web/war-machine/src/war_machine/client/core.cljs` | claude-1 |
| (4) Pilot backend `:wm-api-query` reads live surface only | `futon3c/src/futon3c/peripheral/war_machine_pilot_backend.clj` | claude-1 |
| (5) Tests (clj stack_generator + cljs tile snapshot test if feasible) | `futon3c/test/futon3c/aif/stack_generator_test.clj` | claude-1 |
| (6) Land via night-shift envelope on a dedicated frame | `e-wm-live-recommendation/v0/...` branch | claude-1 |
| (7) Acceptance walkthrough: address a sorry, observe live shift | running JVM + browser | claude-1 + Joe |

Frame-id will be: `night-shift/wm-live-recommendation/v0`
Short-slug: `v0`
cg-id: to be minted; parent = `cg-fb78973a-d2a4-496c-b8ae-5df5a6180676` (the pilot inhabitation cg under which this excursion was authored).

## Success criteria

1. **Live surface exists**: `curl /api/alpha/aif-stack/live | jq .reading.next-move-live` returns a populated object whose `:action` matches `judgement.ranked-actions[0]` in the same response window.
2. **Freshness annotation**: `curl /api/alpha/aif-stack/live | jq .reading.next-move.freshness-warning` returns a non-null block with `:age-days` and `:source :the-stack-aif-edn-cached-prior`.
3. **cljs renders live tile**: open `http://localhost:8710/` → next-move card shows live action (e.g. "address sorry/wm-aif-substrate-addressability") with "live · Nm ago" badge. Verified via Playwright per [[feedback_use_playwright_for_ui_verify]].
4. **Pilot tool prefers live**: `(war-machine-pilot-backend/wm-api-query ...)` returns the live surface; if live is absent, it explicitly refuses to recommend from the cached prose alone.
5. **Live recommendation shifts on action**: per test in §(6).
6. **Tests pass**: `clojure -X:test :nses '[futon3c.aif.stack-generator-test]'` green; existing stack-generator tests still pass.
7. **PR opened, operator-reviewed**: night-shift PR on `e-wm-live-recommendation/v0/...` branch; Joe reviews before merge per [[project_consent_gate]].

## What this excursion does NOT own

- **Improving the EFE computation itself** — `:G-total` semantics, channel-gap weighting, etc. belong to `futon2/scripts/futon2/report/war_machine.clj` `judge` and downstream. This excursion takes ranked-actions as-given.
- **Replacing THE-STACK.aif.edn as a substrate** — the cached prior is a useful documentation artifact (the 🐜N taxonomy, the empirical-bites coalescence) and stays. It just stops pretending to be a live recommendation.
- **PI's policy arena expansion** — the hardcoded 5-action set (`[:work-on :review :consolidate :upvote :wait]`) is owned by `:portfolio.policy` and `E-night-shift` cycle 1's `stack_spine_arena.clj` v0 stub. This excursion routes the existing ranked-actions to the UI, regardless of whether they came from the generic arena or a leaf-registry-driven one.
- **Pilot inhabitation flow itself** — what the pilot does with the live recommendation (mint cg, invoke action, log, repeat) is M-war-machine-pilot territory.

## Cross-references

- `/home/joe/code/futon5a/holes/stories/THE-STACK.aif.edn` — the cached prior (33-day-old; source of the stale `:reading :next-move` prose)
- `futon3c/src/futon3c/aif/stack_generator.clj` — where the projection currently lives (`attach-next-move-agenda`, `generate-live`); where `derive-next-move-live` will be added
- `futon3c/src/futon3c/transport/http.clj` — `handle-aif-stack-live` (no change expected; the generator does the work)
- `futon3c/src/futon3c/wm/scheduler.clj` — `snapshot-for-days`, `!wm-snapshot`, `tick!`, `request-tick!`; the live-source side of the projection
- `futon2/web/war-machine/src/war_machine/client/core.cljs:398-593` — `next-move-tile`; where the cljs reads `:reading :next-move`
- `futon3c/src/futon3c/peripheral/war_machine_pilot_backend.clj` — pilot's `:wm-api-query` tool
- `futon3c/holes/missions/E-night-shift.md` — the branch-isolated landing envelope (this excursion lands through it)
- `futon3c/holes/missions/M-war-machine-pilot.md` — parent mission; this excursion is the unblocker so the pilot loop becomes iterative
- `futon3c/holes/missions/E-wm-staleness-meta-stop.md` — sibling excursion (claude-2-authored) addressing staleness of `:metabolic-balance` signals; structurally similar diagnosis (live-vs-cached confusion), separate scope
- `cg-fb78973a-d2a4-496c-b8ae-5df5a6180676` — the consent-gate event under which the pilot inhabited and authored this excursion
- [[feedback_use_playwright_for_ui_verify]] — verify the cljs change end-to-end before reporting done
- [[feedback_no_synchronous_heavy_drawbridge_calls]] — when verifying live shift via tick refresh, use `request-tick!`, not `tick!`

## Provenance

- Problem detected: claude-1 in emacs-repl, 2026-05-25 ~20:00Z, after three rejected pilot plans surfaced the cached-prose root cause
- Joe's reframe ("WM not suitable for inhabitation by a pilot yet"): emacs-repl 2026-05-25
- Authorship directive ("write it but don't hand it off, finish it yourself"): emacs-repl 2026-05-25
- This excursion file authored: claude-1, 2026-05-25, same session
