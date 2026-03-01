# Technote: Portfolio Inference — Technical Debt and Next Steps

**Date:** 2026-02-27
**Context:** Post-completion of M-portfolio-inference (D-1 through D-11)
**Type:** Retrospective technote (building on session PAR)

## What's Built and Working

The portfolio AIF loop runs live: 8 modules, 937 tests, 3,281 assertions.
It can answer "what should we work on?" backed by prediction error and EFE
computation. The weekly heartbeat shape (D-11) is designed and implemented.
Evidence emission passes Malli validation. The whitepaper documents the
methodology.

## Technical Debt

### T-1: Dynamic require/resolve in portfolio-heartbeat!

**File:** `src/futon3c/portfolio/core.clj:177-183`

`portfolio-heartbeat!` uses `(require 'futon3c.portfolio.heartbeat)` and
`(resolve ...)` at runtime instead of a static `:require` in the ns form.
This was done to avoid a circular dependency, but it's fragile — a typo
in the symbol name fails silently at runtime, not at compile time.

**Fix:** Add `[futon3c.portfolio.heartbeat :as heartbeat]` to the ns
`:require` in core.clj. There is no actual circular dependency — heartbeat
does not depend on core. The dynamic require was unnecessary caution.

**Effort:** trivial

### T-2: futon5 API server not started or tested live

**File:** `futon5/src/nonstarter/api.clj`

The HTTP API exists (GET/POST heartbeat, mana, pool) but has never been
started against a real nonstarter.db. The ring/jetty server code is
standard, but:

- No integration test that starts the server and hits endpoints
- No systemd/supervisor config for running it persistently
- Port 7071 is hardcoded as default — needs to be confirmed as available
- No health check endpoint

**Fix:** Write a smoke test (start server, POST a bid, GET it back, stop).
Add a startup script. Consider whether this should run alongside futon3c's
HTTP transport (port 7070) or separately.

**Effort:** easy

### T-3: No HTTP endpoint in futon3c for portfolio operations

**File:** `src/futon3c/transport/http.clj`

futon3c's HTTP transport has mission-control endpoints (`/api/alpha/mc/*`)
but no portfolio endpoints. There's no way to trigger `portfolio-step!` or
`portfolio-heartbeat!` from Emacs, scripts, or other agents over HTTP.

**Fix:** Add routes:
- `POST /api/alpha/portfolio/step` — run one AIF step, return recommendation
- `POST /api/alpha/portfolio/heartbeat` — run heartbeat with bid/clear data
- `GET /api/alpha/portfolio/state` — return current belief state (μ, τ, mode)

This also needs an Emacs integration — a command in `futon3c-chat.el` or
`futon3c-ui.el` that calls the endpoint and displays the recommendation.

**Effort:** medium (HTTP routes are mechanical; Emacs UI is the real work)

### T-4: portfolio-step! uses unresolved mc-backend reference

**File:** `src/futon3c/portfolio/core.clj:127`

```clojure
(futon3c.peripheral.mission-control-backend/build-portfolio-review)
```

This is a fully-qualified call without a namespace require. It works because
mission-control-backend is loaded by other paths, but it's fragile — if
core.clj is loaded in isolation (e.g., from a test or REPL), this will
throw a ClassNotFoundException.

**Fix:** Add `[futon3c.peripheral.mission-control-backend :as mc-backend]`
to core.clj's ns require and use `(mc-backend/build-portfolio-review)`.

**Effort:** trivial

### T-5: State atom is global, not per-session

**File:** `src/futon3c/portfolio/core.clj:23-28`

```clojure
(defonce !state
  (atom {:mu perceive/default-mu ...}))
```

`!state` is a global defonce. If multiple agents or sessions run portfolio
inference concurrently, they share and clobber the same belief state. For
now this is fine (single-agent portfolio inference), but it violates I-1
(Agent Identity Is Singular) if two agents try to maintain separate
portfolio beliefs.

**Fix (when needed):** Move state into a per-session or per-agent map,
keyed by agent-id. Pass state explicitly rather than mutating a global atom.
Not urgent — the current design is correct for single-agent use.

**Effort:** medium (requires threading state through callers)

### T-6: Heartbeat bid/clear has no UI

The weekly cycle requires: bid on Monday (declare intended actions + effort
bands), clear on Sunday (record actual actions + outcomes). Currently this
can only be done by:
1. Calling `heartbeat/post-bid!` from a Clojure REPL
2. Curling the futon5 API directly

Neither is realistic for weekly use. Needs:
- An Emacs command: `M-x futon3c-portfolio-bid` that prompts for actions
  and effort bands
- An Emacs command: `M-x futon3c-portfolio-clear` for end-of-week
- Possibly an IRC command: `!bid M-foo hard` / `!clear M-foo hard complete`
- A display of current week's bids/clears in the portfolio view

**Effort:** medium-hard (the backend exists; the UX design is the real work)

### T-7: Effort bands not connected to EFE computation ~~DONE~~

Wired heartbeat effort data into the full AIF pipeline:

1. **observe.clj**: Added 3 heartbeat-derived channels (effort-prediction-error,
   bid-completion-rate, unplanned-work-ratio) with neutral defaults when no
   heartbeat data is available. Added `merge-heartbeat-summary` to enrich
   mc-state from action-error output. Channel count: 12 → 15.

2. **perceive.clj**: Added precision entries for 3 new channels. The generic
   perceive loop automatically picks them up via `obs/channel-keys`.

3. **affect.clj**: Added heartbeat channels to mode-precision-boosts. BUILD
   sharpens bid-completion-rate; CONSOLIDATE sharpens effort-prediction-error
   and unplanned-work-ratio.

4. **policy.clj**: Made `effort-cost` observation-aware. When
   effort-prediction-error is high, static effort costs regress toward the
   mean (0.3), meaning the EFE trusts effort estimates less when recent
   heartbeats show they've been wrong.

5. **core.clj**: In `portfolio-heartbeat!`, action errors are computed upfront
   and threaded into the observation pipeline via opts, so the AIF step sees
   the heartbeat data in its observation channels.

**Effort:** easy (the mapping is straightforward; both ends exist)

### T-8: futon5a compression function not written

futon5a is where actual hours live. The compression function that maps
hours to effort bands (for posting to the futon5 public API) doesn't exist
yet. Someone (likely the futon5a maintainer, i.e., Joe) needs to write:

```clojure
(defn hours->effort-band [hours]
  (cond (<= hours 2) :trivial
        (<= hours 4) :easy
        (<= hours 16) :medium
        (<= hours 40) :hard
        :else :epic))
```

Plus a function that reads the week's clears from futon5a, compresses them,
and POSTs to futon5's `/api/heartbeat/clear`.

**Effort:** easy (the function is trivial; the wiring is a small script)

### T-9: Mismatched paren in portfolio-step!

**File:** `src/futon3c/portfolio/core.clj:139-152`

```clojure
    (let [result (assoc result :structure (logic/structural-summary logic-db))]
    ;; Emit evidence
    (when (:emit-evidence? opts true)
      ...)
    result)))
```

The `let` at line 139 opens a binding but the evidence emission block and
the return value `result` aren't clearly inside it. The code works because
Clojure's threading doesn't care about indentation, but the structure is
confusing. The `let` should wrap the evidence emission and return.

**Fix:** Re-indent to make the structure clear, or restructure as a
`doto`-style pipeline.

**Effort:** trivial

## Priority Order

For making the system *usable for a real weekly cycle*:

1. ~~**T-1, T-4, T-9** — Trivial hygiene fixes. Do first. (minutes)~~ DONE (ed8fb6b)
2. ~~**T-3** — HTTP endpoints for portfolio ops. Required for any UI. (hour)~~ DONE
3. ~~**T-2** — Start and smoke-test the futon5 API. (30 min)~~ DONE (3 tests, 16 assertions)
4. ~~**T-6** — Emacs UI for bid/clear. Required for actual weekly use. (hours)~~ DONE (cd8f809 futon5, 37329e9 futon3c)
5. ~~**T-8** — futon5a compression script. Required for privacy masking. (30 min)~~ DONE (integrated into T-6 as nonstarter--hours-to-effort-band)
6. ~~**T-7** — Wire effort bands to EFE computation. Nice-to-have. (hour)~~ DONE (937 tests, 3281 assertions)
7. ~~**T-5** — Per-session state. Only needed for multi-agent. (future)~~ DEFERRED — different mission (MUSN/fulab agent rework)

All technical debt items are resolved or deferred. The system is fully operational
for weekly bid/clear cycles with effort-aware EFE computation.
