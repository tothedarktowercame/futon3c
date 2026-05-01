**Status:** MAP → INSTANTIATE iterating per track (2026-04-29). Q-track-priority + Q-merge-with-violations settled by Joe 2026-04-29.
**Predecessor:** [M-invariant-queue-unstuck](M-invariant-queue-unstuck.md) (CLOSED 2026-04-29)

**Settled questions:**
- Q-track-priority **ratified by Joe 2026-04-29**: Track 4 (operator-activation polish) → Track 1 (substrate-2 lift) → Track 3 (write-class scoping) → Track 2 (War-Machine AIF deferral) → Track 5 (VSATARCS re-park). Rationale: Track 4 converts the apparatus from "scaffolded, opt-in" to firing live, which is the cheapest, highest-leverage move and unblocks the live signal the rest can build on.
- Q-merge-with-violations **ratified by Joe 2026-04-29**: stay separate, keep tight cross-link, revisit when V-1..V-5 in M-invariant-violations resolve. Rationale: M-invariant-violations is a *ledger* (V-N entries describing individual violations); M-invariant-queue-extend is *apparatus extension*. Coupling is producer/consumer, not same-mission. Merging now buries the V-N ledger under apparatus work; the natural merger moment is when V-1..V-5 resolve and the violations mission collapses into "the ledger view of probe outcomes."

# M-invariant-queue-extend: Follow-on tracks for the invariant projection apparatus

## 1. IDENTIFY

### Motivation

`M-invariant-queue-unstuck` (closed 2026-04-29) shipped the apparatus:
single boundary, per-turn persistence verification, coverage ratchet,
hourly+on-demand+autoshutter probe, plus three operational core.logic
taps and twelve deferred-stub registrations. The apparatus is in place;
this mission picks up everything the predecessor explicitly deferred.

Per-track:

1. **Substrate-2 phase-1 lift.** Six deferred-stub families currently
   registered for `M-live-geometric-stack`'s phase-1 invariants
   (`L1-stable-id`, `L1-idempotency`, `L2-endpoint-resolution`,
   `L2-vocab-target-resolution`, `L2-counter-ratchet`,
   `regression-vs-bb-v0_5`). Each is encoded in a babashka test script
   at `futon3/holes/labs/M-live-geometric-stack/tests/phase_1_invariants_test.clj`
   that runs against a live futon1a HTTP endpoint. Lift = either invoke
   the bb scripts from the probe or port the assertions to in-process
   Clojure. The latter is more elegant; the former is faster.
2. **War-Machine AIF diagram-invariants lift.** Six deferred-stub
   families currently registered for the AIF invariants enumerated in
   `futon5a/holes/holistic-argument-aif2.edn :formalism :diagram-invariants`
   (`boundary-integrity`, `observation-action-asymmetry`,
   `timescale-separation`, `preference-exogeneity`, `model-adequacy`,
   `compositional-closure`). Check logic exists conceptually in
   `M-war-machine` but is not yet runtime-callable. Lift requires either
   waiting for `M-war-machine` to land runtime checks, or implementing
   minimal in-process versions here.
3. **Write-class generalization (the boundary recipe).** The
   `docs/boundary-pattern.md` recipe enumerates four plausible
   follow-on write-classes that would each benefit from the same
   single-boundary discipline:
   - bell receipts (futon3c)
   - agent registration (futon3c)
   - mission state transitions (futon3c)
   - gate traversals (futon3b)
   Each is its own boundary + ratchet + canary triple. Probably one
   sub-mission per write-class; this mission identifies the population
   and ranks by value.
4. **Operator-activation polish.** Three small items:
   - Coverage-ratchet load-time wiring (currently only the pre-commit
     hook surface is implemented; load-time check is dormant).
   - Snapshot-as-evidence convention (the ratchet's "previous
     baseline" today comes from `git show HEAD:`; a snapshot evidence
     entry on every inventory load gives a richer history surface).
   - Probe arxana-view extension (the operational-families view in
     `arxana-browser-{core,lab}.el` should render `last-fire-at`,
     `last-violation-at`, `inactive-since` columns from the probe's
     evidence; currently the data is queryable but not rendered).
5. **VSATARCS narrative coherence (parked).** Joe deferred this in
   M-invariant-queue-unstuck IDENTIFY because the work isn't far enough
   along to project a stable contract. Track listed for completeness;
   activation depends on VSATARCS landing first.

### Theoretical anchoring

Predecessor mission's anchoring carries: single-boundary discipline,
layered loud-failure, coverage ratchet, projection-not-enumeration,
queue-as-projection of an alive structure. This mission inherits the
apparatus and applies it to the next ring of write-classes and probe
sources.

The new theoretical wrinkle: the predecessor's apparatus assumes a
single repo's worth of write-classes (futon3c). The write-class
generalization track may pierce repo boundaries — e.g. gate traversals
live in futon3b. The boundary discipline still applies per write-class,
but the static check (the I-single-boundary canary) needs to run across
the relevant repo. Cross-repo enforcement is a new concern.

### Scope in

- Substrate-2 phase-1 lift: at least 3 of 6 invariants ported to live
  probe check-fns (the rest deferred again with reasoning).
- War-Machine AIF lift: minimal in-process versions of at least 2 of 6
  invariants (or wait for M-war-machine if Joe directs).
- Write-class generalization: scope two write-classes (probably bell
  receipts + gate traversals) into separate sub-missions; do not
  implement them here.
- Operator-activation polish: all three small items.

### Scope out

- Building boundaries for ALL four write-class generalizations in this
  mission — that's mission-scale per write-class, not a track of one
  mission.
- VSATARCS lift — depends on upstream work.
- Enforcement of I-single-boundary as a CI/clj-kondo rule rather than a
  shell grep — separate, larger.
- Merging with M-invariant-violations — a documentation question more
  than an engineering one; resolve when both missions reach DOCUMENT.

### Completion criteria

1. **Substrate-2 lift partial.** ≥ 3 of 6 substrate-2 phase-1 families
   converted from deferred-stub to a real probe check-fn that returns
   `:ok` or `:violation` with concrete detail.
2. **War-Machine AIF lift partial.** ≥ 2 of 6 War-Machine AIF families
   similarly converted, OR a recorded decision to wait for M-war-machine
   with criteria for when to revisit.
3. **Operator-activation polish complete.** Coverage-ratchet load-time
   wiring lands. Snapshot-as-evidence convention decided + implemented.
   Arxana operational-families view shows `last-fire-at` /
   `last-violation-at` / `inactive-since` columns.
4. **Write-class generalization scoped.** Two sub-mission stubs opened
   (one per write-class), each with IDENTIFY and a sketch of the
   boundary + ratchet + canary triple.
5. **VSATARCS** explicitly marked deferred again (with current reason)
   if still appropriate, OR upgraded to deferred-stub registration if
   VSATARCS has progressed.
6. **No new structural-law inventory entries marked operational that
   are not in fact firing.** I-coverage-ratchet must hold across all
   inventory mutations this mission produces.

### Relationship to other missions

- **Predecessor:** `M-invariant-queue-unstuck` — apparatus this mission
  extends.
- **Source for substrate-2 lift:** `M-live-geometric-stack`
  (futon3/holes/missions/) — the test files that get lifted live in its
  labs directory.
- **Source for War-Machine AIF lift:** `M-war-machine` — the conceptual
  check logic for the diagram-invariants lives there. May require
  coordination with that mission's owner before lift.
- **Sibling:** `M-invariant-violations` — tracks individual violations
  in the operational core.logic layers. The taps + probe make those
  violations live-fire-record-emitting; merging the two missions during
  DOCUMENT is plausible.
- **Generalization targets:** future M-bell-boundary,
  M-gate-traversal-boundary, etc. (one per write-class). This mission
  scopes them but doesn't build them.
- **Reference:** `docs/boundary-pattern.md` — the recipe each
  generalization follows.

### Source material

- `futon3c/src/futon3c/evidence/boundary.clj` — predecessor's boundary
  (the recipe-instance to model from).
- `futon3c/src/futon3c/logic/{ratchet,probe,probe_taps}.clj` —
  predecessor's apparatus.
- `futon3c/docs/structural-law-inventory.sexp` — the registry to extend.
- `futon3c/docs/boundary-pattern.md` — the recipe.
- `futon3/holes/labs/M-live-geometric-stack/tests/phase_1_invariants_test.clj`
  and `phase_2_geometric_test.clj` — substrate-2 source.
- `futon5a/holes/holistic-argument-aif2.edn` — War-Machine AIF source.
- `futon4/dev/arxana-browser-{core,lab}.el` — operational-families
  view to extend.
- `futon4/holes/mission-lifecycle.md` — procedural spec.

### Owner and dependencies

- **Owner:** Joe (architectural authority on track ordering, write-class
  selection, VSATARCS readiness) + claude (substrate-2 lift,
  operator-activation polish, sub-mission scoping).
- **Primary repo:** futon3c (apparatus extensions live here).
- **Secondary repos:** futon3 (substrate-2 source); futon5a (War-Machine
  source); futon4 (arxana view + mission lifecycle).

## 2. MAP

(Pending track selection. Will populate per-track once Joe ratifies
priority order: substrate-2 first / operator-polish first / write-class
scoping first.)

## 3. DERIVE

(Pending MAP per-track.)

## 4. ARGUE

(Pending DERIVE per-track. Will likely be a per-track ARGUE block plus
a synthesis on whether to merge with M-invariant-violations.)

## 5. VERIFY

(Pending DERIVE per-track. The static checks from the predecessor
(I-single-boundary grep, ratchet integrity, probe-tap unit tests)
remain in force; this mission must not regress any of them.)

## 6. INSTANTIATE

(Pending phases above.)

## 7. DOCUMENT

(Pending. Will produce updated docbook entries and possibly a merge
with M-invariant-violations.)

## Checkpoints

### 2026-04-29 — HUD widget: live invariant-queue motion

**Joe's extension of the tracer reframe.** Rather than schedule a +1-week
agent to query tracer movement, surface motion continuously via a Stack
HUD 2 widget. The widget IS the leaderboard — counts of open/closed
tracers, days-to-target color-coded, recent family-canary outcomes.
Stuckness becomes ambient awareness, not a polled report.

**Concrete deliverables**

- `futon0/contrib/stack-hud-widgets.el` — new render function
  `stack-hud-widget--render-invariant-queue`. Three sections:
   1. Header: motion flag (STUCK/MOVING/FLOWING) + open/closed/canary
      counts.
   2. Open tracers: one row per open `:pipeline-tracer-item`, with
      days-until-target color-coded (red past, yellow ≤1d).
   3. Recent canary fires: last N `:family-fired` entries with outcome
      color-coded (ok=green, violation=red, inactive=gray).
- Two new defcustoms: `stack-hud-widget-invariant-queue-window-days`
  (default 7) and `-canary-tail` (default 5).
- Helpers `stack-hud-widget--days-iso-since`, `--has-tag?`,
  `--body-get`, `--days-until`, `--motion-flag`, `--canary-face`,
  `--fetch-coordination`, `--invariant-queue-data`.
- `stack-hud-widget-docs` entry for the new widget.
- Standalone command `stack-hud-widget-render-invariant-queue` for
  iteration outside the HUD.
- Demo-all aggregator extended to include the widget.
- `futon0/contrib/stack-hud-2.el` — registered the renderer in
  `stack-hud-2--block-renderers` and prepended `invariant-queue` to
  the default `stack-hud-2-blocks` list.

**Verified live**

Loaded both files into Joe's running Emacs via `emacsclient`. Standalone
render returns:

```
Invariant Queue — motion
window 7d · since 2026-04-22T17:16:25Z

  STUCK   open: 6   closed: 0   canary fires: 0

  Open tracers (by track-id, days-to-target):
  ─────────────────────────────────────────────────
   +6d  track-5-vsatarcs                  VSATARCS narrative coherence (parked …
   +6d  track-2-war-machine-aif-lift      War-Machine AIF lift (≥2 of 6 OR docu…
   ... (6 total)

  no recent canary fires
```

STUCK is correct: zero closures and zero canary fires, because the
probe loop is still operator-driven (predecessor's
`start-probe-loop!` opt-in). When Joe activates it, the bottom section
starts populating with live family outcomes.

**Net mission state**

- Tracker reframe now has both surfaces: durable evidence entries +
  ambient HUD visibility.
- The pipeline's flow-or-stuckness becomes a property Joe can see at a
  glance instead of a property requiring a query.
- Joe's "Bayesian-HUD-as-cyborg-AIF-interoception" framing
  (project_joe_hud_model in memory) gets one more interoceptive
  channel: the queue's pulse.

### 2026-04-29 — Tracks-as-tracers reframe (Joe)

**Joe's reframe (load-bearing).** Rather than "Tracks 4.2, 4.3, 1, 2, 3, 5 are
TODOs to grind through today," reframe them as **pipeline tracers** added to
the invariant-queue apparatus. The apparatus's first real customer is the
work that built it. If the pipeline moves the tracers from open → closed
over the next ≈ week, the apparatus has passed its own test. If they stay
inert, we learn what's still clogged. Either outcome is informative.

**Two-tier tracer realization:**

- **Invariant-shaped tracks (Track 1: substrate-2 lift; Track 2: War-Machine
  AIF lift)** are *already* tracered via the predecessor's deferred-stub
  registration in `probe-taps`. The 12 `:substrate-2/*` and `:war-machine/*`
  family-ids surface as `:family-fired :outcome :inactive :detail
  {:deferred? true ...}` on every probe sweep. Movement = a stub becoming
  a real check-fn that fires `:ok` or `:violation`.
- **Non-invariant tracks (Track 4.2 snapshot convention; Track 4.3 arxana
  view; Track 3 write-class scoping; Track 5 VSATARCS)** registered as
  `:event :pipeline-tracer-item` evidence entries via the new
  `futon3c.logic.tracer` namespace. Tagged
  `[:invariant-queue :pipeline-tracer :open <track-id>]`; closure = a
  sibling `:event :pipeline-tracer-closed` entry referencing the same
  `:track-id`.

**Concrete deliverables**

- `src/futon3c/logic/tracer.clj` — new namespace with `I-pipeline-tracer`
  canonical statement, `default-tracers` (six entries: tracks 4.2, 4.3, 3,
  1, 2, 5), `emit-tracer!`, `emit-pipeline-tracers!`, `emit-tracer-closed!`.
- `test/futon3c/logic/tracer_test.clj` — 6 deftests / 13 assertions / 0
  failures: emit-shape, per-tracer one-receipt, tag-queryability, close
  emission, canonical statement grep, default-tracers track coverage.
- **Live emission landed:** `tracer/emit-pipeline-tracers!` invoked against
  `@futon3c.dev/!evidence-store` (the running JVM's XtdbBackend) emitted 6
  tracer entries; tag query confirms all six retrievable as
  `[:invariant-queue :pipeline-tracer :open]`. The live system now carries
  the tracer set.

**Mission-state implications**

- Tracks 4.2, 4.3, 1, 2, 3, 5 reframed: not TODOs for this agent's session,
  but tracers being watched. Target date for all six: **2026-05-06** (one
  week from open). At that date the apparatus's behaviour gets reviewed.
- Mission status remains **MAP** (Track 4.1 complete; Tracks 4.2+ on
  tracer status). `Status:` line above unchanged for now.
- Recommended follow-up at +1 week: query
  `[:invariant-queue :pipeline-tracer :open]` and
  `[:invariant-queue :pipeline-tracer :closed]`, count which tracks moved.
  If movement rate is low → the apparatus is surfacing the queue but not
  causing flow; investigate why. If high → the apparatus works; close
  remaining tracks as routine.

**Test sweep**

- Mission suites including new tracer (boundary + invariant + store +
  ratchet + probe + probe-taps + tracer): **75 tests / 217 assertions / 0
  failures**.

### 2026-04-29 — Track 4.1 INSTANTIATE complete: coverage-ratchet load-time wiring lands

**Concrete deliverables**

- `src/futon3c/logic/ratchet.clj` — added `check-on-load!` plus internal
  helper `emit-load-time-fired!`. Runs `check-pre-commit` (working-tree
  vs git HEAD), maps the result to `:ok | :violation | :inactive`,
  emits a `:family-fired :coverage-ratchet :load-time <outcome>`
  evidence entry through the boundary, prints a structured banner on
  violation / OK / inactive (mirroring the existing
  `check-store-backing` boot-banner pattern). Boot continues either
  way — surfacing, not blocking.
- `dev/futon3c/dev/bootstrap.clj` — added `[futon3c.logic.ratchet :as
  ratchet]` to require list; added a try-wrapped `ratchet/check-on-load!`
  call right after the existing I-evidence-per-turn boot check. Both
  load-time invariants now fire on every boot, in order
  (store-backing → coverage-ratchet).
- `test/futon3c/logic/ratchet_test.clj` — two new deftests:
  `check-on-load-emits-family-fired-evidence` and
  `check-on-load-respects-emit-flag`. 12 tests / 37 assertions / 0
  failures total in this file.
- PSR at
  `holes/labs/M-invariant-queue-extend/psr/2026-04-29__derive__ratchet-load-time-wiring.md`
  + PUR at
  `holes/labs/M-invariant-queue-extend/pur/2026-04-29__instantiate__ratchet-load-time-wiring.md`.

**Pre-existing brokenness fixed in passing**

- `src/futon3c/transport/irc.clj`: the predecessor's INSTANTIATE-2-rest
  boundary-require conversion left an orphan `[futon3c.evidence.boundary
  :as boundary]` form *outside* the `(:require ...)` block. The running
  JVM (started 2026-04-24) didn't surface this because it never reloaded
  the file; verifying Track 4.1 required a fresh boot, which exposed it.
  Fix: move the orphan line into the require list. Without this, the
  load-time wiring couldn't be verified end-to-end.

**Tests**

- Mission suites (boundary + invariant + store + ratchet + probe +
  probe-taps): **69 tests / 204 assertions / 0 failures**.
- Wider sweep: 24 failures + 19 errors, all pre-existing brokenness
  in peripherals.edn / shapes mismatches across social.peripheral-test,
  mode-integration-test, peripheral-spec-test, mission-control-test,
  etc. (Joe's other in-flight uncommitted work). The +14 errors vs the
  predecessor's reported sweep are tests previously hidden by the
  irc.clj compile failure.

**Net Track 4.1 mission state**

- ✅ I-coverage-ratchet load-time mode: BINDING. The dormant-mode case
  recorded in M-invariant-queue-unstuck's DOCUMENT phase is now
  non-dormant. Every boot of `dev/bootstrap.clj` emits one
  `:family-fired :coverage-ratchet :load-time <outcome>` entry.
- I-coverage-ratchet status in inventory remains
  `:operational-when-enabled` for now — pre-commit hook is still
  operator-installed (separate activation step). A graduation to
  `:operational` follows once the hook is also wired by the operator.

Track 4.2 (snapshot-as-evidence) and Track 4.3 (arxana view extension)
are independent of one another and can land in any order. Track 4.1's
`:family-fired` emission is the data Track 4.3 will render.

### 2026-04-29 — mission opened

- IDENTIFY phase complete.
- Five tracks identified (substrate-2 lift, War-Machine-AIF lift,
  write-class generalization, operator-activation polish, VSATARCS).
- Two open questions logged for ARGUE: track priority + merger with
  M-invariant-violations.
- Mission status: IDENTIFY complete; pending Joe priority direction
  before MAP / DERIVE on any specific track.
