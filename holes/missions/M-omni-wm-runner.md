# M-omni-wm-runner — fold the WM full-loop runner into the serving JVM

Chartered 2026-07-26 (Joe, emacs-repl: "let's do the fold-in, *then* restart
the JVM for good measure"). Owner: claude-3 (ground control). Authors: codex
parcels under the coding-handoff protocol; claude-3 reviews as the gate.

## Why (the I-0 ruling)

Every durée click has run as a fresh `clojure -M:wm-full-loop` JVM from
futon2 (`d0be5e6` made the runner a `-m` process entrypoint; the pattern was
canonized in README-ground-control without ever being reconciled against
futon3c I-0). Joe's ruling 2026-07-25: **no third JVM, ever** — the one
allowed exception is the futon1b store on :7073, *because it is the evidence
store*. Costs demonstrated across cohorts 44–45:

- **Classpath divergence:** three vintages of futon2 ran simultaneously in
  one evening; a fix (`e78a728`) could not reach an already-running click.
- **Evidence-env divergence:** each click's read/emit topology is launch-time
  env (`FUTON3C_EVIDENCE_BASE` / `FUTON2_WM_EMIT_BASE`), not shared state.
- **Visibility seam:** the runner reports through external-invoke HTTP posts
  (15s freshness window, transition-only) — the source of every stale
  `*agents*` complaint on 07-25/26.
- **Self-inflicted load:** boot-compile burn + a 100s single-shot HTTP
  selection call lost races against store congestion (attempts 055/057/058)
  that in-process calls do not have.

## Scope

**In:** one durée click (`once` semantics) runs in-process in the futon3c
JVM on a dedicated thread; HTTP trigger + status; registry-direct apparatus
status (no freshness gap); selection patience + typed selection-timeout
kinds (cohort-46 epoch armor); runner code stays in futon2 (the fold-in is
about WHERE it runs, not a rewrite).

**Out (follow-ups):** `continuous` mode; the transient futon0 scan JVM
(note: WM snapshot scans already run in-process via wm/scheduler — the
stray futon0 JVM sighting is a separate cleanup); store heap/indexes;
turn-commits relay fix (separate seam, same close-out arc).

## Parcel A — futon2 (author: codex-2)

`src/futon2/aif/full_loop_runner.clj` (+ tests):

1. **Selection patience:** the strategic-selection call gets an escalating
   retry ladder like the preflight's (`53269bb`): budgets sized ABOVE the
   observed success latency under load (056 selected :ok at 130s; 057/058
   died at a 100s single shot). Ladder 150s/210s/270s, explicit override
   pins, sleeps between; `:readiness/selection-transient` marker on
   late success, mirroring the preflight key.
2. **Typed timeout:** exhaustion closes the attempt with
   `:failure-kind :strategic-selection-unavailable` (the 052 kind — desk
   ledger maps it to infra; 057/058 rendered as untyped crosses).
3. **Injectable selection invoke seam:** `:strategic-selection-invoke-fn`
   in opts — when present, called in place of the HTTP bridge request
   (fn of the same request payload → same response shape). Default
   behavior unchanged (HTTP). This is what parcel B binds in-process.

## Parcel B — futon3c (author: codex-4)

New `src/futon3c/wm/runner_service.clj` (+ route wiring in transport/http.clj
+ tests):

1. **Service:** `click!` runs `futon2.aif.full-loop-runner/run-opportunity!`
   (via `requiring-resolve`; futon2 is already on this classpath — the WM
   scan endpoints prove it) on a dedicated named daemon thread
   ("wm-runner-click"), **single-flight** (an atom guard; a second click
   while one runs → 409 with the running click's id). Opts passed through:
   author/reviewer/repair-reviewer/trigger; plus
   `:strategic-selection-invoke-fn` bound to the in-JVM selection fn the
   scheduler already uses (wm/scheduler's strategic-selection-fn — one
   source of truth, no HTTP hop, no 100s client timeout class).
2. **Endpoints:** `POST /api/alpha/wm/click` {author, reviewer,
   repair-reviewer} → {click-id, started-at}; `GET /api/alpha/wm/click`
   → {running?, click-id, phase, attempt-id, started-at, last-result-ref}.
3. **Registry-direct status:** the service sets the war-machine apparatus
   entry's status/activity/last-active through registry fns directly at
   each phase event (wrap the runner's `:phase-log-fn` seam) — synchronous,
   no 15s freshness lapse, no self-HTTP. The runner's own HTTP status posts
   may remain (harmless duplicates) — do not modify runner reporting in
   this parcel.
4. **Result:** on close, publish {attempt-id, outcome} to the service
   status atom and log one `[wm-click]` summary line. No pprint of the
   full result anywhere (attempt-053/056 pathology).

## Acceptance

- Parcel A: existing runner/cohort test namespaces stay green; new tests:
  ladder timeouts observed by a capturing stub; explicit override pins all
  attempts; exhaustion carries the typed kind; injectable seam used when
  present (HTTP stub NOT called).
- Parcel B: single-flight guard (second click 409s); endpoint contract
  tests with a stubbed runner fn; registry status assertions (invoking →
  idle with last-active stamped); no new JVM/process spawn anywhere
  (`pgrep java` count unchanged during a stubbed click).
- Gates on both: clj-kondo, check-parens, `clojure -X:test` for the
  touched namespaces.

## Cutover plan

1. Both parcels reviewed (claude-3, real gate) and merged.
2. **Joe restarts the futon3c JVM** (his call on timing — "for good
   measure"; this also validates restart-equivalence of all of 07-25/26's
   live-installs).
3. Canary: one in-process click with a fresh cast, watched end-to-end;
   verify no click JVM, synchronous roster visibility, selection via
   in-process seam.
4. Retire the click pattern: README-ground-control click section rewritten
   to the endpoint; `:wm-full-loop` alias marked deprecated-for-clicks in
   futon2 deps.edn comment + AGENTS/CLAUDE notes carry I-0 into futon2.
5. Cohort 46 charter documents the epoch (this mission + selection
   semantics) — separate doc, after cutover.

## Non-negotiables

- I-0 language lands in futon2's instructions as part of this mission (the
  invariant lived only in futon3c's CLAUDE.md while the violation grew in
  futon2 — that gap is the root cause; closing it is in scope).
- No changes to preflight/stopping-rule/revision-round code paths beyond
  the selection seam (all reviewed in the last 24h).
- The attempt/cohort record schema is untouched.
