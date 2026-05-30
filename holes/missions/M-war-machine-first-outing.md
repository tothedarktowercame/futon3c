# Mission: M-war-machine-first-outing

**Date:** 2026-05-30
**Status:** **IDENTIFY** (authored emacs-repl with claude-5, operator Joe present). The pilot works at a basic level — one substantive cycle moved the field 2026-05-29 (`live-2d50834b`), a second on 2026-05-30 (`live-00c07332`, this session). This mission asks: *which open edges must close before we can confidently run the machine for a while without stopping it* — and *how* such a run is actually set up.
**Timebox:** open — gated by the blocking-edge workstream (§3.1), not the calendar.
**Predecessors:**
- `M-war-machine-pilot.md` (capability — v0 complete 2026-05-24)
- `M-pilot-appearance.md` (four-depth appearance — CLOSED 2026-05-29; its "NOT yet done" list seeds this mission's edge inventory)
- `holes/specs/repl.spec.edn` (the REPL-as-differential-operator spec; `:conformance :remaining-gap` and the `:LOOP :autonomy` clause are the contract this mission discharges)
- `futon7/holes/M-war-machine-aif-last-mile.md` (model current), `futon7/holes/M-war-machine-frontend-upgrade1.md` (frontend), `M-war-machine-tuning.md` (audit / numerics)
**Sibling / charters it draws on:** `E-cheesemonger.md` (hole-budget), `E-storyteller.md` (immersive exo), `M-a-sorry-enterprise.md` (sorry-mining), `M-invariant-queue-extend.md` (invariant lifts)
**Owner:** Joe

---

## 0. Provenance

Authored in one emacs-repl session, 2026-05-30, after claude-5 re-inhabited the
War Machine pilot for the first time since `M-pilot-appearance` closed, and ran a
full four-turn cycle "to prime the pump" (run `live-00c07332`: addressed
`sorry/handler-closure-route-rebinding` via an audit + doc-discipline write-up;
top-shift TRUE; frame CONFORMS V1–V4). With the loop demonstrably alive again,
Joe proposed this mission: *now that it works at a basic level, fix up the open
edges so it works better and is ready to run for a while.*

The IDENTIFY-phase reconnaissance below was produced by four parallel read-only
sweeps (predecessor lineage; open-edges inventory; run-mechanisms; the Invariant
Queue + bootstrapping hypothesis). Their conclusions are folded in with
file:line citations; the raw sweeps are in this session's transcript.

---

## 1. IDENTIFY — the gap

### 1.1 Invariance claim

> The War Machine pilot can already complete a single supervised cycle that
> moves the field and emits a conforming frame. "Ready to run for a while"
> is a *different* property: across an unbroken sequence of cycles with **no
> operator at the keyboard between them**, the machine must (a) keep choosing
> defensible directions, (b) earn its discharges honestly, (c) leave a trail an
> operator can audit and *trust* the next morning, and (d) never silently
> corrupt its own substrate. A machine that completes one cycle well but cannot
> be left alone for twenty is not yet ready for a first outing.

### 1.2 The deepest finding (reframes the whole question)

A **mechanical driver alone gives supervised *heartbeats*, not earned
*discharges*.** The `begin/close-live-cycle!` functions
(`src/futon3c/peripheral/war_machine_pilot.clj:229–312`) run headless — they
were driven from Drawbridge this session — but headless they only produce
*supervised-proposals*: no field movement, no real edit (cf. run
`live-021b9a85`, pred-error 0, top-shift false). A **substantive** cycle needs
an **agent in the loop** to make the §8 fork-choice and perform the real PRINT
edit. So "run the WM for a while" is precisely "keep an agent reasoning through
cycles for a while" — and invariant **I-1 (one agent = one session; transport
routes, does not spawn)** forbids cloning a `claude -p` to do it. This is why
the *driver* question (§4) is load-bearing, not incidental.

### 1.3 In scope / out of scope

**In scope:** closing the edges that block a *first* confident unattended run;
choosing and wiring the run driver; the gate (merge) posture for that run; an
informal write-up of what we expect the run to produce (which doubles as the
prediction the run is scored against).

**Out of scope (named follow-ons, deferred — §3.4):** building the autorunner;
doc-currency automation; the E-cheesemonger hole-budget monitor + candidate→
registry promotion; the E-storyteller immersive exo; *sustained* (multi-night)
operation. Each gates sustained operation, not the first outing.

---

## 2. Open-edges inventory (IDENTIFY → MAP boundary)

Go/no-go for a *first* confident unattended run. Citations are to the live
sources read this session.

| Edge | State | Where | Blocks first outing? |
|---|---|---|---|
| **Closure-history ledger + regression-watch** | open; `sorrys.edn` is **gitignored** | `E-cheesemonger.md:87,91`; `futon2/data/sorrys.edn` | **YES — hard blocker.** Unattended `:open→:addressed` flips with no version-controlled audit trail, and no check that a closed sorry re-surfacing is a *regression* not a *new* find = silent corruption invisible for days. |
| **Hole-budget v0 monitor (don't-flood)** | designed, not built | `E-cheesemonger.md:90` | YES *if* candidate→registry promotion is enabled; otherwise deferrable (promotion stays off for the first outing). |
| **Gate / which-γ-merges policy** | per-tool cg enforced; policy deferred | `repl.spec.edn:219–222`; `README-pilot.md:61–63` | **DECISION, not a build** — see §4.2. |
| **LOOP-learning automation** (transcript-mining) | v0 auto-miner only (`:derivation :auto-mined`) | `repl.spec.edn:142–143`; `loop_learning.clj`; `M-a-sorry-enterprise.md` Checkpoint | MAYBE — degraded-but-runnable *with an agent in the loop* (the agent is the learning channel); full automation is a sustained-operation concern. |
| **Doc-currency** (exo/VSATARCS regen on pheno/geno change) | scoped only | `repl.spec.edn:143` (EXO-LAYER clause); `README-pilot.md:75–78` | MAYBE — tolerable for a short outing with a **manual regen checkpoint**; blocks sustained runs. |
| **Scheduler robustness across many cycles** | built, async-safe (`request-tick!`) | `wm/scheduler.clj:184,239,263–268` | NO — sound by design; wants latency telemetry + a soak test (§3.2). |
| **§8 fork-resolution at scale** | built + witnessed once | `repl.spec.edn:225–250` | MAYBE — solid design, untested under many/sparse forks; first outing *is* the test. |
| **Per-frame conformance verification** | built (`repl_spec_verify.clj`), single-cycle witness | `repl.spec.edn:191–207` | NO — but should run **automatically on every emitted frame** during the outing. |

**The one hard blocker is the closure-history ledger + regression-watch.**
Everything else is either a decision, a soak-test, or a sustained-operation
follow-on.

---

## 3. Workstreams

### 3.1 Close the silent-corruption edge (BLOCKING — do first)

- Give sorry-closures a **version-controlled, append-only ledger** — either
  un-gitignore `sorrys.edn` or write a sidecar `sorry-discharge-history.edn`
  (id, prior-status, new-status, cg-id, resolved-by, timestamp, run-id) that
  *is* committed. The ledger is the substrate that makes the next item checkable
  rather than asserted.
- Wire **regression-watch**: when a sorry previously `:addressed` re-surfaces in
  the WM gap-signals, flag it as `:regression-or-false-closure`, a distinct
  category — never silently re-mined as new. (`E-cheesemonger.md:87`.)
- Acceptance: a deliberately re-opened test sorry is caught and surfaced as a
  regression, with the catch recorded in the ledger.

### 3.2 Minimum-viable run harness

- **Driver** wired per §4.1 (recommended: harness `/loop` against this session).
- **Verifier auto-runs** on each emitted frame (`bb scripts/repl_spec_verify.clj`
  on the new `data/repl-traces/<run-id>.edn`); a non-conforming frame halts the
  loop and surfaces.
- **Latency telemetry**: record per-tick wait + per-cycle wall-clock; set a
  simple SLA (e.g. tick lands < 2 min after `request-tick!`).
- **Soak test**: 25–50 cycles unattended in the chosen posture, with the
  verifier gating each, before any claim of "ready."

### 3.3 The expectations write-up (Joe's named follow-on)

An *informal* "what we expect a long run to produce" note — see §5 for the seed.
It is not decoration: it is the **prediction** the run is scored against, so V3
prediction-error has something to bite on at the session scale, not just the
per-cycle scale.

### 3.4 Deferred — named, not silent

Autorunner build; doc-currency automation; E-cheesemonger hole-budget monitor +
candidate→registry promotion; E-storyteller immersive exo; multi-night sustained
operation. **Subsumption discipline:** none of these are claimed "free given the
first outing" — each is live-deferred with its own owner-charter.

---

## 4. How the run is set up (the operator's question)

### 4.1 Driver — the I-1-clean options

- **Tickle** — *wrong tool.* A watchdog/dispatcher that observes via evidence
  timestamps (`agents/tickle.clj`); it would *escalate* a stalled pilot, not
  pace its loop. Good as a **safety net**, not a driver.
- **Bells / Codex handoff** — not drivers; bells are outbound
  (`transport/http.clj` `handle-bell`), Codex is scope-escape (`CLAUDE.md`
  Codex-handoff §). Useful for **handing oversized work out**, not cycling.
- **Autorunner** — *right shape, not built.* "Auto-OK the same in-session agent
  through bounded cycles" is I-1-clean (no clones), but it is a concept today
  (referenced in `E-support-coverage.md`; no `M-claude-autorunner`, no code).
- **Harness `/loop`** — *available now*, I-1-clean: paces a prompt/cycle against
  *this* session at an interval; operator-visible, pausable. The realistic
  first-outing driver.
- **Native pilot-conductor daemon** — only gives supervised heartbeats unless an
  agent is in the loop; a **complement** (field-freshness), not the substantive
  driver.

**Recommendation (operator-pending):** harness `/loop` against this session for
the first outing. Build the autorunner later as the durable driver.

### 4.2 Gate posture — what happens to the pilot's edits

- **Supervised / staging** — edits land on a staging branch (or stay un-merged);
  operator reviews+merges each morning. Lowest risk; gains are *exploration*
  speed, not *decision* speed. This is the spec's own migration story (swap
  operator for autopen at one location later — `repl.spec.edn:219–222`).
- **Autopen full-auto** — commits to master unattended; requires §8 + regression-
  watch to be highly reliable first.
- **Hybrid by action class** — auto-commit low-risk classes (doc, ledgered
  sorry-flip); stage substantive code edits. Needs the action-class split wired.

**Recommendation (operator-pending):** **supervised/staging** for the *first*
outing — the point of a first outing is to *watch what it does* before trusting
it to merge — graduating to hybrid once §3.1's ledger + regression-watch land.

> These two are genuinely operator decisions. They are recorded here as
> recommendations, NOT as ratified, pending Joe's explicit call.

---

## 5. What we expect a long run to produce (seed for §3.3)

Joe's intuition — *most queued items are the WM bootstrapping itself* — is
**confirmed by the registry**: of 5 currently-`:open` sorries, **4 are
apparatus/meta** (HUD-mode rationale, mission-AIF bridge, R3d attribution,
stub-lifts) and only 1 is outward product work (`futon2/data/sorrys.edn`,
read 2026-05-30). So early cycles will be overwhelmingly self-improvement.

The **outward tail** lives in the `:open-mission` ranks (a recent judgement put
these at ranks 7–20): `M-coupling-as-constraint`, `M-fulab-wiring-survey`,
`M-xor-coupling-probe`, `M-futonzero-mvp`, `M-patterns-done-right`,
`M-stack-hud-refactor`, `M-fulab-logic`. Plus the **Invariant Queue**: 6
substrate-2 + 6 WM-AIF diagram-invariant stub families waiting in
`futon3c/docs/structural-law-inventory.sexp` to graduate candidate→operational
(`M-invariant-queue-unstuck.md`, `M-invariant-queue-extend.md`).

**Informal prediction:** ~60–75% of cycles spent on self-bootstrapping
(apparatus sorries, invariant lifts), ~25–40% on the outward mission tail, with
the balance tipping outward only as the apparatus sorries thin. Action-type
meanings (what discharging each costs): `:address-sorry` = resolve a named gap +
ledger the closure; `:open-mission` = commit to a checkpointed scoped mission;
`:fire-pattern` = instantiate a library pattern; `:learn-action-class` = invent a
new capability when nothing else is proposable
(`futon2/src/futon2/aif/forward_model.clj:30`).

This section is the write-up's skeleton; §3.3 fleshes it into the standalone
prediction doc before the soak test runs.

---

## 6. Awaiting

- Operator ratification of the §4.1 driver and §4.2 gate posture (recommendations
  stand pending Joe's call).
- MAP-phase decomposition of §3.1 (ledger shape: un-gitignore vs sidecar) once
  the posture is set.
- (Non-blocking) the lost "metrics" mission — deprioritised per Joe 2026-05-30.

---

## 7. MAP — §3.1 the blocking edge (2026-05-30, claude-5 + Joe)

MAP of the one hard blocker (closure-history ledger + regression-watch). The
other two workstreams (§3.2 harness, §3.3 write-up) are mapped on the next car;
this is the critical path everything else waits behind. All claims below were
read live this session — not carried from prior.

### 7.1 Terrain — critical files (verified)

| File | Role | Verified fact |
|---|---|---|
| `futon2/data/sorrys.edn` | the registry data | gitignored via blanket `data/` (`futon2/.gitignore:28`); **untracked** (`git ls-files` empty). `data/` holds only `sorrys.edn`, a backup, `vitality/`, `wm-trace/` — so un-ignoring one file is surgical. |
| `futon2/src/futon2/aif/sorry_registry.clj` | read API | `load-sorrys` (slurp+edn) / `open-sorrys` (filter `:status :open`). **No writer exists** — every flip is an out-of-band hand-edit (as claude-5 did this session). So a ledger-append has no current write-path to hook; it must be added at the editing site (pilot PRINT) or the harness. |
| `futon2/src/futon2/aif/belief.clj:40,248,265` | belief vocabulary | `:reopened` is already a status in the annotation-belief lifecycle (`#{:spawned :refined :strengthened :addressed :falsified :foreclosed :reopened}`), scored 0.0 ("was-addressed-now-questioned; ambiguous"). **But it tracks per-entity annotation posteriors, not sorry-registry `:status`** — the regression *vocabulary* exists; the *wiring* for sorry re-opens does not. |
| `futon2/scripts/futon2/report/war_machine.clj:3110` (`judge`), `:sorrys` @3258, `:ranked-actions` @3382 | the ranker | where open sorries enter the ranking; the WM only surfaces `:open` sorries, so a closed sorry "re-surfaces" only if its `:status` flips back to `:open`. |
| per-sorry discharge fields | the in-process signal | the convention claude-5 established this session — `:resolved-at`, `:resolved-by-cg`, `:resolved-by-pilot`, `:resolution` — already records closure *on the sorry itself*. |

### 7.2 The blocker splits into two clean sub-pieces

**A — the ledger (durable human audit trail).** Options for DERIVE:
- **A1: un-gitignore `sorrys.edn`** (`!data/sorrys.edn` after the `data/` line; commit). Git history *becomes* the closure ledger — per-flip diffs, timestamps, provenance via commit message. Surgical (bulky/transient `wm-trace/`, `vitality/` stay ignored). Cost: pilot/harness must `git add && commit` after each flip.
- **A2: committed sidecar `sorry-discharge-history.edn`** — append-only transition log `{id from to cg-id resolved-by run-id at}`, living in a tracked dir. Structured + in-process-queryable + decoupled from registry churn. Cost: a new write discipline + a home.
- **A3: rely on the per-sorry `:resolved-*` fields already present** (no new file) — combined with A1 for the human trail.

**B — regression-watch (the check).**
- **Predicate (in-process):** a sorry with `:resolved-at`/`:resolved-by-cg` present but `:status :open` = regression/false-closure (discharged, now open again). Requires a one-line schema discipline: **re-opening a resolved sorry must NOT clear its resolution fields** — add `:reopened-at` and keep the prior resolution, so the predicate fires.
- **Hook options:** **B1** a standalone scan the *run-harness* calls each cycle (`close-live-cycle!` already loads `open-sorrys`; add the regression-scan, halt + surface on a hit) — doesn't touch WM core, right for a first outing. **B2** fold a `:regressions` field into `judge` — more integrated, touches core; defer. Optional later: feed a detected regression as a `:reopened` event into the annotation belief so the WM's own posterior reflects it (reuses the existing `belief.clj` vocabulary).

### 7.3 Recommendation into DERIVE (operator-pending)

**A1 + A3** (un-gitignore `sorrys.edn`; use the already-present `:resolved-*`
fields as the in-process signal — no new file, no new schema beyond the
`:reopened-at` discipline) **+ B1** (harness-level regression scan that halts the
loop). Minimal, surgical, no WM-core change; gives both a human audit trail (git
diffs) and an in-process detector. Acceptance: a deliberately re-opened test
sorry is caught and surfaced, with the catch visible in `git log`.

---

## 8. DERIVE

> **Wiring diagram:** the decided criteria + their interlocks are being captured
> in `M-war-machine-first-outing-wiring.edn` (started 2026-05-30; typed
> ports/components/edges, exotype-flavoured). It grows as DERIVE proceeds.


**DERIVE is NOT closed by §8.1–8.2.** Those resolve a single criterion (closure
integrity). "Confidently run the machine for a while without stopping it" implies
a decision for *each* readiness criterion below; DERIVE stays open until every
row has a disposition (decide-and-build / accept / monitor / defer-named).
Worked one car at a time (car-of-sequence), not ratified en masse.

### 8.0 DERIVE criteria agenda (the full set to work through)

| # | Criterion | What DERIVE must decide | Status |
|---|---|---|---|
| **R-A** | Closure integrity | ledger shape + regression-watch mechanism | **DECIDED** (§8.1–8.2; build pending) |
| **R-B** | Driver | what paces the cycles (`/loop` / autorunner / conductor) — §4.1 | **DECIDED** (§8.4: `/loop`, this session) |
| **R-C** | Gate / merge posture | what happens to the pilot's edits — §4.2 | **DECIDED** (§8.5: staging branch, gate-at-merge) |
| **R-D** | Halt conditions | what *stops* the run (non-conforming frame, regression hit, error, max-cycles, operator sentinel) — the "without stopping it" needs explicit stop criteria | **DECIDED** (§8.8: hard halts + soft stops + stuck detector) |
| **R-E** | Per-cycle gates | verifier auto-run + regression check wired into each cycle; behaviour on fail | **DECIDED** (§8.7: G1 conformance / G2 regression / G3 earned-discharge) |
| **R-F** | LOOP-learning posture | is v0 auto-miner + agent-in-loop enough for the outing, or is more required? | **DECIDED** (§8.10: accept v0; agent is the channel) |
| **R-G** | Hole-budget / don't-flood | is candidate→registry promotion ON or OFF during the outing? (OFF removes flood risk + defers the monitor) | **DECIDED** (§8.11: promotion OFF) |
| **R-H** | Doc-currency | exo/VSATARCS regen — manual checkpoint at run-end, or skip for first outing? | **DECIDED** (§8.12: manual regen at run-end if pheno/geno moved) |
| **R-I** | Fork-resolution guardrails | restrict action classes / cap cycles for the first unattended forks? | **DECIDED** (§8.13: `:address-sorry` allowlist; escalate `:open-mission`/∇-deform) |
| **R-J** | Scheduler / latency robustness | telemetry design + soak-test parameters | **DECIDED** (§8.14: telemetry + 120s SLA; outing = soak) |
| **R-K** | Outing scope / duration | how long is "a while" — N cycles? bounded by what? | **DECIDED** (§8.6: ~10–15 cycles / ~60–90 min) |
| **R-L** | Expectations write-up | shape of the §3.3 prediction doc; authored *before* the soak | **DECIDED** (§8.15: pre-launch scored prediction doc) |
| **R-M** | Observability / morning-review | what the operator sees the next morning (frames, ledger diffs, Arxana violations, learning) | **DECIDED** (§8.9: branch + frames + Arxana + run-summary `.edn`) |
| **R-N** | Safety net | Tickle-as-watchdog escalation if the pilot stalls (recon: its right role) — wire or defer? | **DECIDED** (§8.16: defer for outing; wire before agent-absent run) |

This list itself is a DERIVE artefact to ratify — add/cut/merge rows before we
work them. Many will resolve to a *posture* (accept / monitor / defer-named),
not a build; that is the point of DERIVE here.

### 8.1 R-A.1 — Ledger = relocate `sorrys.edn` to `resources/`, tracked

Not "un-ignore in `data/`" (§7.2-A1) but **move** `futon2/data/sorrys.edn` →
**`futon2/resources/sorrys.edn`**, which is git-tracked. Rationale (Joe):
simpler, and it makes **WM operation reversible at the Git level** — each
discharge is a commit; `git revert`/`git log` is the closure ledger and the
undo. Ties to `E-cheesemonger.md` closure-must-stick + provenance-ledger.

**Synergy discovered:** `futon2/scripts/wm_outer_loop.clj:181-204` *already*
mines `git log -- data/sorrys.edn` for closure-activity evidence-refs — it finds
nothing today because the file is untracked. Retargeting it to the tracked
`resources/sorrys.edn` brings that signal online with no new code.

**Migration reader-list (verified 2026-05-30 — reads, not mentions):**
- `futon2/src/futon2/aif/sorry_registry.clj:37-38` — `default-sorrys-path` (canonical).
- `futon3c/scripts/ingest_sorrys_to_futon1a.clj:32` — hardcoded absolute path.
- **`futon3c` file-watcher** `sorry-registry-path?` / `watched?` (in
  `src/futon3c/watcher/file_ingest.clj` + `watcher/multi.clj`; asserted in
  `test/.../file_ingest_test.clj:20-22`, `multi_test.clj:38`) — **LIVE WIRING.**
  The watcher recognizes the path to ingest edits into substrate-2.
- `futon2/scripts/wm_outer_loop.clj:181-204` — `git log` path (relative `data/…`).
- Name-mentions to refresh for accuracy (non-functional): `war_machine.clj:2815`,
  `hud.cljs`, `core.cljs`, `belief.clj:438`, `intrinsic_values_test.clj`.

**Operational caveat (INSTANTIATE sequencing):** the live futon3c JVM + futon2 WM
scan read this path now. The move must be: (1) update all path constants +
watcher predicates; (2) move the file; (3) Drawbridge-reload the affected
namespaces (sorry-registry, watcher file-ingest/multi, WM scan) — never restart
the JVM (I-0). Until the watcher predicate is reloaded, edits won't ingest, so
order matters. This is a real INSTANTIATE step, not a one-liner.

### 8.2 R-A.2 — Regression-watch = an invariant family on the existing Arxana surface

Not a bespoke harness scan (§7.2-B1) but an **invariant family** on the existing
**Live Invariants / Live Violations** surface — Joe's framing: a regression IS
"an invariant once held, now no longer true." This reuses the probe apparatus
and the Arxana renderer with **no new UI**.

- **Family:** `:sorry-closures-stick`. Register via
  `futon3c.logic.probe/register-family-check!` (tap-factory pattern,
  `logic/probe_taps.clj:74-223`). Add the entry to
  `docs/structural-law-inventory.sexp` (`:candidate` → `:operational-when-enabled`
  once wired; ratchet-governed).
- **Check-fn:** scans the registry; returns `{:outcome :violation :detail
  {:regressions […]}}` when any sorry has `:resolved-at` present but `:status
  :open` (discharged, now open again), else `{:outcome :ok}`.
- **Schema discipline (one line):** re-opening a resolved sorry must NOT clear
  its `:resolved-*` fields — add `:reopened-at` and keep them, so the predicate
  fires. (Optionally feed the hit as a `:reopened` event into the `belief.clj`
  annotation channel — that vocabulary already exists, line 40 — so the WM's own
  posterior reflects it; defer.)
- **Surface (no new UI):** the probe emits `:family-fired` evidence to futon1a
  (`:7071`); the Arxana browser (`futon4/dev/arxana-browser-lab.el:1495-1752`)
  already renders families with Last-Fire / Last-Violation columns. The new
  family shows up automatically.
- **Dual caller for the first outing:** the *same* check-fn is also invoked
  **synchronously by the run-harness each cycle** (halt + surface on a hit) — the
  probe's hourly cadence gives durable evidence + the Arxana surface; the
  per-cycle call gives the immediate gate. One predicate, two callers.
- **Self-consistency:** this makes regression-watch *itself* an Invariant-Queue
  item (cf. `M-invariant-queue-extend.md`) — exactly the kind of work §5 predicts
  a long run produces. The mission's safety mechanism is also a sample of its
  output.

### 8.3 R-A build readiness (does NOT close DERIVE)

R-A is design-decided and *buildable* (the move + reloads; register the family +
check-fn + `:reopened-at` discipline). Whether to build it now or after the rest
of DERIVE is an operator-pacing call — it does not license closing DERIVE.
Impedance note (from recon): probe families are mostly structural/stack laws;
this is a data-content check over a registry — the tap-factory pattern
accommodates it, flagged so it's not a silent category-stretch.

### 8.4 R-B — Driver = harness `/loop` against this session

The substantive cycle needs an agent in the loop (§1.2); I-1 forbids clones.
The only driver that satisfies both *and* exists today is the harness **`/loop`**
skill: it re-pokes **this** session's agent at an interval (or self-paced), so it
is one-agent-one-session by construction — operator-visible and pausable.
- **Deferred successor:** the **autorunner** (in-session auto-OK, bounded
  wall-clock, `AUTORUNNER-OFF:` sentinel) is the durable unattended driver; not
  built (no `M-claude-autorunner`). The first outing does not wait on it.
- **Not the driver:** a native conductor daemon yields only supervised
  *heartbeats* without an agent in the loop (a complement, for field-freshness);
  Tickle is the **safety net** (R-N), not a pacer; bells/Codex are handoff.

### 8.5 R-C — Gate posture = supervised gate-at-merge via a staging branch

Pilot substantive edits are **committed to a dedicated branch
(`wm-outing/<date>`), never to master directly**; the operator reviews and merges
in the morning. This is the spec's gate-at-merge (`repl.spec.edn:219–222`) with
the operator as the merger — autopen substitutes the operator at exactly that one
location when we later graduate.

**Locks into R-A:** because R-A makes `sorrys.edn` git-tracked, each discharge is
already a commit — so the staging branch *is* the closure ledger and the
reversibility surface (`git revert` a bad discharge), and merge-to-master *is*
the consent gate. One structure serves R-A's ledger, R-C's gate, and R-M's
morning-review surface. (Multi-repo branch mechanics — futon2 / futon3c / futon5a
— deferred to INSTANTIATE.)

### 8.6 R-K — Duration = tightly-bounded first outing

Bound the first outing to **~10–15 substantive cycles OR a ~60–90 min wall-clock
box, whichever comes first**; any R-D halt condition ends it earlier. The purpose
is to *learn how it behaves*, not to maximise work — a post-outing review gates
any longer run. (Mirrors the autorunner's 60-min-cap precedent; small N keeps the
morning review surface reviewable.)

**Shape summary:** a `/loop`-driven, this-session agent runs a bounded ~10–15
cycles onto a `wm-outing/<date>` staging branch that the operator merges after
review. R-D…R-N (halt conditions, per-cycle gates, autonomy postures, telemetry,
write-up, observability, safety net) remain OPEN — DERIVE continues.

### 8.7 R-E — Per-cycle gate sequence (the `/loop` body)

Each cycle composes the existing apparatus; gates run *before* the cycle's edits
are committed to the staging branch:

1. `begin-live-cycle!` {mode `:substantive`} → READ (dT) + EVAL (agent picks `v`
   via §8 fork-resolution, mints `cg`).
2. Agent performs the **earned PRINT** (the real edit), citing the `cg`.
3. `request-tick!` → wait for the tick to land (SLA per R-J).
4. `close-live-cycle!` → emit the γ frame; compute realised + top-shift.
5. **G1 conformance** — `repl_spec_verify.clj` on the frame; V1–V4 must CONFORM.
6. **G2 regression** — the R-A.2 check-fn; must be `:ok` (no resolved-but-open).
7. **G3 earned-discharge honesty** — *if* the cycle claimed a discharge (flipped
   a sorry to `:addressed`), top-shift must be TRUE (the field actually moved).
   A claimed discharge with top-shift false is a fake-finish (V2 no-teleport).
8. **G1 ∧ G2 ∧ G3 ⇒ commit** the cycle's edits to `wm-outing/<date>` (one commit
   per cycle; message cites `cg` + sorry-id + frame run-id). Any gate fail ⇒
   halt (R-D), leave the working tree uncommitted, surface.

### 8.8 R-D — Halt conditions ("War Machine, not a pram") — REVISED 2026-05-30

Operator (2026-05-30): a fake-finish violates every contract — *but* "pushing the
ejector button the minute things get mildly tricky is not going to work either."
So the taxonomy is three-tier, with hard-halt reserved for the one trust-poisoning
breach:

**HARD HALT — contract breach (the only run-ending event):**
- **G3 fake-finish** — claimed a discharge the field doesn't support (V2
  no-teleport). Non-negotiable; the run can't be trusted to continue.

**QUARANTINE + CONTINUE — containable friction (NOT an ejector event):** an
uncaught error in a cycle · a slow/missed tick (R-J SLA) · a non-conformant frame
(G1) · a **single** regression (G2). Each is **recorded loudly, NOT committed,
surfaced** (run-summary + Arxana) — and the run **persists on other work**. The
silent-corruption defense is satisfied by *detect + surface*, not by aborting the
whole run for one old regression.

**SOFT STOP — genuine exhaustion (clean end + summary):** bounds reached (R-K) ·
**stuck-detector** — K consecutive cycles with NO progress *of any kind*, where
progress now includes a **decompose** (∇-deform) cycle, so deep-but-decomposable
work never counts as stuck · **regression-storm** (≥3 regressions ⇒ substrate
actually unstable) · operator sentinel.

On any halt/stop: write the run-summary (R-M); quarantined cycles are listed with
their reasons so the operator sees exactly what was worked around. Verified
offline 2026-05-30: error / regression / decompose / discharge all continue; only
fake-finish hard-halts.

### 8.9 R-M — Morning-review surface (assembled, one new artefact)

Mostly existing surfaces, composed:
- `wm-outing/<date>` branch `git log` — the discharge ledger, a commit per
  earned cycle (this is R-A's ledger + R-C's gate, same structure).
- the run's `data/repl-traces/live-*.edn` frames, each G1-CONFORMS-checked.
- the Arxana **Live Violations** surface — any `:sorry-closures-stick` hits.
- **NEW artefact:** a run-summary `data/outings/<date>.edn` — cycles attempted /
  committed, halt reason, Σ prediction-error, top-shifts, sorries discharged
  (with `cg` + frame refs), any regressions / non-conformances, wall-clock. The
  only thing built fresh; everything else is assembled.
- read against the R-L expectations write-up: predicted vs actual cycle
  disposition (self-bootstrapping vs outward).

**Harness-mechanics summary:** each cycle is gated (conformance, regression,
earned-discharge) before it commits; hard gates halt the run, soft limits end it
cleanly; the operator wakes to a branch + frames + Arxana violations + one
summary `.edn`. R-F…R-L, R-N remain OPEN.

### 8.10 R-F — LOOP-learning posture = ACCEPT v0 (agent-in-loop is the channel)

The driver (R-B) keeps *this session's agent* reasoning each cycle, so the
operator-B→A learning channel is **not** collapsed — the very condition that
makes LOOP-learning automation mandatory (`repl.spec.edn:142–143`) does not hold
for a `/loop`-driven outing. Decision: **accept v0** — the agent supplies
patterns-applied + sorries-mined into each frame (V6, `:derivation
:agent-supplied`), the `loop_learning.clj` auto-miner backstops
(`:auto-mined`). **Defer** transcript-mining + pattern-derivation automation to
`M-a-sorry-enterprise` (named, required only for *agent-absent* overnight runs).

### 8.11 R-G — Hole-budget / don't-flood = promotion OFF

Candidate→registry promotion is not wired today (auto-miner candidates live in
frames, advisory). Decision: **keep promotion OFF for the first outing.** The
registry is mutated *only* by the agent's earned discharges (real fix + sorry
flip), never by auto-promotion — so the flood risk cannot arise and the
E-cheesemonger hole-budget monitor is **deferred** (named; required only before
promotion turns on). Respects the anti-backdoor invariant: only persisted
sorries are actionable; mined candidates stay advisory.

### 8.12 R-H — Doc-currency = manual regen checkpoint at run-end

Per-cycle exo/VSATARCS regen is unbuilt and unnecessary for a bounded outing.
Decision: **one manual regen at run-end** (`bb futon4/scripts/generate_vsatarcs_md.bb`)
as a morning-review step, *conditioned on* the run-summary (R-M) flagging that a
cycle changed pheno/geno (registry-only cycles don't require it). Automation of
doc-currency is **deferred** (named; a sustained-operation concern).

### 8.13 R-I — Fork-resolution guardrails = action-class allowlist

§8 fork-resolution is witnessed once, untested at scale; the first outing
validates the loop, not its reach. Decision: **allowlist the autonomous action
surface.** `:address-sorry` discharges (audit / doc / targeted fix, as
witnessed) run autonomously. The higher-blast-radius forks — **`:open-mission`**
(unbounded scoped work) and any **∇-deforming PRINT** (new typed-edge family /
niche-construction, V4) — **halt-and-surface for operator decision** rather than
autonomous execution. The §8 fork-warrant is recorded every cycle regardless
(step 4), so the post-outing review audits whether warrants were sound.

**Autonomy-postures summary:** v0 learning accepted (agent is the channel);
promotion OFF (no flood); doc-regen once at run-end if pheno/geno moved;
autonomous surface limited to `:address-sorry`, with `:open-mission` /
niche-construction escalated. R-J, R-L, R-N remain OPEN.

### 8.14 R-J — Latency robustness = telemetry + SLA, the outing is the soak

No new machinery — reuse `scheduler/status` (`:tick-count`, `:last-tick-at`) and
the bounded poll already proven this session (wait for `:tick-count` to advance
past the pre-tick value, tolerating "tick already in progress" so requested and
auto-300s ticks interleave safely under the re-entrancy guard).
- **Telemetry:** record per cycle `{:tick-wait-seconds :cycle-wall-clock}` into
  the frame / run-summary.
- **SLA:** the tick must land within **120 s** of `request-tick!` (scan is ~50 s;
  120 s gives margin). Breach ⇒ R-D hard-halt (scheduler/JVM unhealthy).
- **Soak:** the bounded outing (R-K) *is* the first soak — post-run, inspect the
  tick-wait distribution. A dedicated 50-cycle **supervised-proposal** soak (mode
  `:supervised-proposal`, no edits — stresses only the tick/latency path cheaply)
  is a **deferred** option if a longer run is later wanted.

### 8.15 R-L — Expectations write-up = a pre-launch, scored prediction doc

A standalone `M-war-machine-first-outing-expectations.md`, authored and
timestamped **before cycle 1**, so the run is scored against it (V3 at the
*session* scale, not just per-cycle). §5 is its skeleton. It must carry:
- predicted cycle **disposition** (~60–75% self-bootstrapping / ~25–40% outward,
  tipping outward as apparatus sorries thin) and the **first-few-cycles** call —
  current top-of-field is `r3d-per-entity-attribution` (post this session's
  discharge), then the apparatus tail (`stub-lifts`, `hud-mode-rationale`);
- the **scored quantities**: Σ prediction-error trend (should fall), earned
  top-shift rate (most discharges TRUE), regression count (must be 0), and the
  discharges-vs-halts ratio.
Authoring the doc is an INSTANTIATE-prep deliverable; DERIVE fixes its shape and
the rule that it precedes the run.

### 8.16 R-N — Safety net = defer Tickle for THIS outing, name its trigger

The first outing is supervised (operator present) and bounded (R-K), and the
R-D **stuck-detector** already catches the agent *spinning*. A hard hang (agent
stops emitting / JVM wedge) is caught by operator presence — `/loop` visibly
stalls. Decision: **defer Tickle-as-watchdog for the first outing**; **wire it as
the stall-escalation safety net before the first agent-*absent* (autorunner)
run** — that is where the operator is no longer the watchdog. Recon confirmed
Tickle's right role is exactly this escalation, not loop-driving.

---

## 8.17 DERIVE — CLOSED (2026-05-30)

All 14 readiness criteria (R-A … R-N) have a disposition; the agenda table is
fully resolved. The first outing's shape, gates, halt criteria, autonomy
postures, review surface, latency contract, prediction-doc rule, and safety-net
trigger are all decided. The wiring diagram
(`M-war-machine-first-outing-wiring.edn`) reflects the closed set. **Deferred,
named follow-ons** (not silent): transcript-mining automation (R-F →
M-a-sorry-enterprise); hole-budget monitor + promotion (R-G → E-cheesemonger);
doc-currency automation (R-H); 50-cycle latency soak (R-J); Tickle wiring (R-N);
the autorunner as durable driver (R-B). DERIVE does not authorise the build —
next phase is **ARGUE** (stress the design) then **INSTANTIATE** (R-A build
first, per §8.3).

---

## 9. ARGUE (2026-05-30)

Short and strategic; per-tension *operational* hooks ("what we'll actually do
about it") are deferred to VERIFY. ARGUE stays open until those hooks are
ratified.

### 9.1 Central tension — an "outing", or a supervised crawl?

The mission's own words are *run for a while without stopping it*. The design
stops it readily: it aborts on any failed check, ends after ~10–15 cycles or
~90 minutes, hands the hardest choices back to the operator, and makes nothing
permanent without an operator merge.

- **IF** the goal were maximal autonomous distance, **THEN** the design fails —
  it is heavily gated and bounded.
- **HOWEVER** the goal of a *first* outing is not distance but *trustworthy*
  distance: learning whether the machine can be left alone **at all** without
  quietly corrupting its own record.
- **THEN** the gates and bounds are not a weakness — they are the instrument
  that makes the first run both informative and reversible; the machine still
  runs unattended *between* cycles, the operator acts only once, at the end.
- **BECAUSE** "without stopping it" means "without a human approving every
  step", not "without any stop conditions". Moving operator involvement from
  per-step to per-run *is* the outing — and the design is honest precisely
  because it is falsifiable: if it halts constantly, the run has told us
  "not ready", which is a true result, not a design failure.

### 9.2 Secondary tensions (strategic; hooks → VERIFY)

- **T2 — narrow surface.** Autonomy is limited to one task class; the forks that
  matter most (opening whole missions, inventing new capability) are escalated.
  So the outing tests the *safety machinery*, not *judgment at the hard forks*.
  Right thing to test first — but not to be mistaken for the whole.
- **T3 — present ≠ absent.** "The agent is the learning channel" leans on the
  agent being re-poked but *present* each cycle. The outing therefore validates
  the harness and safety, **not** true agent-absent overnight running. Do not
  let "first outing" launder into "overnight-ready".
- **T4 — single structure, single point.** One structure serves as record, gate,
  and review surface. Elegant, but if that structure's discipline breaks (a
  cycle fails to record, or multi-repo records desync), all three properties
  fail together.

### 9.3 Plain-language statement (no jargon)

> We have a tool that can look at its own to-do list, pick the most pressing
> item, try to fix it, document its work, and then check whether the fix really
> worked. Until now a
> person has had to sit with it and approve each step. The point of this first
> trial run is to find out whether it can do several items in a row on its own —
> while making sure that, if it ever marks something "done" that isn't really
> done, or quietly undoes work it had finished earlier, the run stops at once and
> nothing it did is made permanent until a person has looked it over afterwards.
> We are deliberately keeping the run short and giving it only the safer kinds of
> task to begin with. So this is not yet "leave it running overnight"; it is the
> smaller, honest question that has to be answered first: can it work unwatched
> for a little while without quietly fooling itself? If it keeps tripping the
> safety checks, that is not a failure of the plan — it is the trial telling us
> the answer is "not yet".

*Note (not part of the plain statement):* "document its work" spans two things,
and honesty requires keeping them distinct so the phrase isn't an overclaim
against the deferred R-H automation:
- **Per-cycle (real, always):** every cycle leaves a record of what it did — the
  γ frame plus the commit message on the staging branch. This is load-bearing
  now (R-E/R-M), and it is what makes the plain phrase true each cycle.
- **Run-end (R-H, manual for this outing):** the fuller refresh of the
  human-readable stack view (the **exo / VSATARCS layer**) happens once at the
  end, by hand, only if a cycle changed code/reasoning — automation deferred.

**R-H ratified 2026-05-30 (Joe):** per-cycle documentation (frame + commit) keeps
docs current **at the per-PR level** — accepted as load-bearing and required, not
optional. The run-end VSATARCS/exo refresh slightly dodges a strict "always up to
date" promise; Joe accepts this for the outing *because* per-PR currency holds.
The named future concern is the **WM↔VSATARCS speed-of-connection** (how fast the
readable view tracks code/reasoning changes) — deferred, not forgotten.

### 9.5 Insight — the REPL cycle IS the mission lifecycle, reshaped

Joe (2026-05-30): we want per-cycle docs for the same reason we want per-mission
ARGUE phases — because the M-pilot-appearance REPL (READ / EVAL / PRINT / LOOP)
and the futonic mission lifecycle (IDENTIFY → MAP → DERIVE → ARGUE → VERIFY →
INSTANTIATE → DOCUMENT) are **the same operator, reshaped**. A REPL cycle is a
micro-mission; per-cycle documentation is its DOCUMENT turn, exactly as
ARGUE/DOCUMENT are a mission's. This is a **new insight since the Operator's
Foreword was written** → **follow-on: fold it into the Foreword (VSATARCS §0).**
It also explains why this very session is self-similar: a REPL cycle primed the
pump, then the mission lifecycle ran over the outing — one shape at two scales,
which is itself the strongest argument for keeping per-cycle docs first-class.

So the one sentence quietly names three of the four depths — it *fixes* (code),
it *checks* (the machine's own reasoning), it *documents* (the record + the
readable view) — and the sentence itself is the fourth, the outward-facing
account.

### 9.4 ARGUE status

OPEN — the four tensions (9.1 central + T2/T3/T4) are named but not operationally
answered. VERIFY must attach a concrete hook to each (e.g. T1: a halt-rate
threshold above which we call the outing "not ready"; T3: the explicit gate that
must be added before an agent-absent run). ARGUE closes when those hooks are
ratified.

---

## 10. VERIFY — approach (operator-steered: check the logic model before the code)

Joe's method: express the outing's invariants/gates as a **core.logic model** and
**check the model before writing the harness**. This reuses the house idiom —
`agency/logic.clj`'s `snapshot → build-db → goals → query-violations` over
`clojure.core.logic` + `pldb` — so it is wiring, not invention.

### 10.1 Method

The model reasons over **abstract cycle-traces** (a run = a sequence of
cycle-records: claimed-discharge?, G1/G2/G3 pass?, committed-where?, top-shift?,
action-class, operator-merge?), **not** the live harness — so adversarial traces
can be enumerated. Verification = run the goal-queries over **(a)** witnessed
traces (this session's frames `live-2d50834b`, `live-00c07332`) → expect **zero**
violations; and **(b)** adversarial traces planted to break each invariant →
expect each **caught**. Same self-validating shape `repl_spec_verify` already
uses (a CONFORMS witness + a synthetic-failures fixture).

### 10.2 Invariants encoded (the goals)

The wiring's six + the allowlist: **I-1** (no second agent), **earned-discharge**
(∄ commit without G1∧G2∧G3), **gate-at-merge** (∄ master-write without operator
merge), **ledger-completeness** (∄ discharge without a commit), **no-flood** (∄
registry-growth except by earned discharge), **G3 no-teleport** (∄ claimed
discharge with top-shift=false that still commits), **R-I allowlist** (∄
autonomous step with action-class ∈ {`:open-mission`, ∇-deform}).

### 10.3 ARGUE-hook mapping (this is what closes ARGUE)

- **Central (outing vs crawl)** → a **halt-rate verdict** relation: "not-ready" is
  a computable predicate over the trace. *Proposed threshold (ratify):* any
  **hard-halt**, OR **>50% soft-halts**, ⇒ the outing reports "not ready". Makes
  the falsifiability claim a number, not a slogan.
- **T2 (narrow surface)** → the allowlist goal + an autonomous-vs-escalated count,
  so "we only tested the easy path" is explicit and queryable.
- **T3 (present ≠ absent)** → an **agent-present precondition** on every
  admissible trace; agent-absent traces are declared **unverified** until the
  R-N Tickle gate is added — encoded so the model refuses to certify overnight.
- **T4 (single-structure failure)** → covered by ledger-completeness (a
  record-desync = a discharge with no commit = a caught violation).

### 10.4 Dual use (the model is not throwaway)

Encoded in the structural-law style, these goals can later **register as live
probe families** (exactly like R-A.2's `:sorry-closures-stick`) — so the same
model is an **offline proof now** and a **live guard at runtime** on the Arxana
surface. One artefact, two uses.

### 10.5 Deliverable + choices to ratify

Deliverable: a model + trace fixtures + a check entrypoint, run **before** any
harness code. Open choices for Joe:
1. **Halt-rate threshold** (§10.3) — accept "any hard-halt OR >50% soft-halts ⇒
   not-ready", or set your own.
2. **Dual-use** — register the model as live probe families now, or keep it
   offline-only for VERIFY and wire it live during INSTANTIATE?
3. **Location** — `src/futon3c/logic/outing_invariants.clj` (+ fixtures), to sit
   beside the existing domain logic models?

### 10.6 VERIFY result — model checked, PASSES (2026-05-30)

Built `src/futon3c/logic/outing_invariants.clj` in the house idiom
(`build-db → query-violations` over core.logic + pldb), loaded into the running
JVM via Drawbridge (no new JVM, I-0), ran `run-verify`:

- **`:verified? true`** — the witnessed conforming trace (the abstract shape of
  this session's `live-2d50834b` / `live-00c07332` cycles) yields **zero**
  violations, and **all 8 adversarial traces are each caught by their own
  invariant** (i1-extra-agents, earned-discharge, gate-at-merge, ledger-complete,
  no-flood, no-teleport, allowlist, agent-present).
- Defaults taken (operator delegated): halt-rate threshold = any hard-halt OR
  >50% soft-halts ⇒ not-ready; offline-only for VERIFY; location as proposed.

**Honest scope (what this does and does NOT prove):** it verifies the *design* —
the invariant set is internally consistent and the checks discriminate
conforming from violating traces. It does **not** prove the *implementation*
matches the model. **INSTANTIATE obligation:** the harness must emit traces in
this exact record shape and gate commits on `query-violations` (ideally by
registering these as live probe families, R-A.2-style), so the running system
*refines* the verified model rather than merely resembling it.

### 10.7 ARGUE — CLOSED (2026-05-30)

The §9 tensions now each carry an operational hook, so ARGUE closes:
- **Central (outing vs crawl)** → `halt-rate-verdict` makes "not ready" a
  computed number (threshold ratified by default above).
- **T2 (narrow surface)** → the `allowlist` goal makes autonomous-vs-escalated
  explicit and queryable.
- **T3 (present ≠ absent)** → the `agent-present` goal makes the model **refuse
  to certify** any agent-absent trace until the R-N Tickle gate is added.
- **T4 (single-structure failure)** → `ledger-complete` catches the record-desync
  failure mode (a discharge with no commit).

**Phase state:** IDENTIFY ✓ · MAP ✓ · DERIVE ✓ · ARGUE ✓ · VERIFY ✓.
Next is **INSTANTIATE**, beginning with R-A (the `sorrys.edn` move + reloads),
then the harness built to refine this model.

---

## 11. INSTANTIATE (2026-05-30) — verify-as-we-go via a reversible probe

Operator directive: build a reversible synthetic sorry, promote it to the front
of the queue, and verify each component live as it comes online — "verifying as
we go" so the whole of INSTANTIATE is trustworthy.

### Car 1 — probe + regression detector (DONE, verified live)
- Built `src/futon3c/logic/sorry_closures.clj` (R-A.2): `regression-violations`
  predicate (sorry with `:resolved-at` present but `:status :open`) + probe-shape
  `check` (robust to the R-A.1 move: prefers `resources/`, falls back to `data/`)
  + a `register!` for the live family.
- Added reversible `:sorry/sorry-first-outing-probe` (`:probe? true`, `:kind
  :meta`) to the registry.
- **Verified live:** detector `:ok` on the real 17 sorries; fires
  discriminatingly on a synthetic re-opened sorry; after a tick the probe
  surfaces at **ranked-actions[0]** (G −4.945) — promoted to the front as asked.

### Car 2 — R-A.1 the move (DONE, verified live)
Relocated `futon2/data/sorrys.edn → futon2/resources/sorrys.edn`; tracked via a
surgical `.gitignore` negation (`!resources/` + `resources/*` + `!resources/sorrys.edn`)
mirroring the war-machine precedent — only `sorrys.edn` is tracked, other
`resources/` files stay ignored. Retargeted: `sorry_registry.clj` default-path,
`ingest_sorrys_to_futon1a.clj`, watcher regex (`file_ingest.clj` — matches BOTH
`data|resources` for transition-safety), `wm_outer_loop.clj` git pathspec.
Drawbridge-reloaded `file_ingest` + `sorry_registry` only (the watcher's `multi`
reaches the regex via var, so reloading it was unnecessary and avoided — the
reload-safety discipline from car 0). **Verified:** `sorry-registry-path?` true
for the new path; `load-sorrys` reads `resources/` (17); and post-tick the probe
still sits at `ranked-actions[0]` — the WM scan reads the relocated registry. No
JVM restart (I-0).

### INSTANTIATE finding — R-C staging branch needs a git worktree
`futon2` is a **single shared working tree** with the live JVM reading files from
it; a `git checkout -b wm-outing` would switch the tree for *all* agents/processes
and change files under the running JVM. So R-C's "staging branch" cannot be a
working-tree branch — it needs a **dedicated git worktree** (separate dir, same
repo) so the pilot commits to `wm-outing/<date>` without disturbing the live
tree. For the probe demo, a **path-limited commit** (`git commit resources/sorrys.edn`)
on the current branch is non-disruptive (commits only that file) and, with a
later `git revert`, directly demonstrates R-A.1 git-reversibility. Worktree
isolation is the refinement for real unattended runs (R-C update).

**jujutsu (jj) — experiment later (operator, 2026-05-30):** the shared-working-tree
hazard (other agents' WIP entangling any `git add -A`; branch-spaghetti for
overnight runs) is exactly where jj is strong — **workspaces** (multiple working
copies over one repo) isolate parallel agents, and the op-log is a cleaner
"reversible at the git level" than branch-juggling. Previously declined, but
worth a re-look *for this need*. NOT for the first outing (supervised, bounded,
single session — path-limited commits suffice). R-C follow-on: a small spike
comparing `git worktree` vs `jj workspace` for the overnight driver; operator
decides. Do not introduce jj unilaterally.

### Car 3 — live cycle on the probe (DONE, verified)
`begin-live-cycle!` → v = probe, predicted −4.945, `cg-ae3de1a1`, run
`live-9536b3ce` → PRINT flip probe → `:addressed` + path-limited commit
`bc10f6b` (the ledger entry) → tick → `close-live-cycle!`: top-shift TRUE
(probe rotated off rank-0), pred-error 0.0 → **G1** CONFORMS (V1–V4), **G2**
`:ok`, **G3** top-shift TRUE. All gates green.

### Car 4 — regression caught live (DONE — the silent-corruption defense)
Re-opened the probe (`:status :open`, kept `:resolved-at`, added `:reopened-at`).
G2 (`:sorry-closures-stick`) fired **`:violation`**, naming the exact sorry,
`would-hard-halt? true`. The original hard blocker — a discharged sorry silently
re-opening — is now a caught, halting condition, proven on a live case.

### Car 5 — full reversal (DONE — git-level reversibility demonstrated)
Removed the synthetic probe; committed `bcedec6`. Registry back to 16 real
sorries, probe gone from the queue, field restored (top = r3d-per-entity), G2
`:ok`, zero residue. `git log resources/sorrys.edn` shows the lifecycle
`bc10f6b (discharge) → bcedec6 (removal)` — the tracked-registry ledger + revert
path is real.

### R-A.1 confidence pass (2026-05-30, operator-gated "get really confident")
- **Straggler audit (all repos, all langs):** the earlier `.clj`-scoped grep MISSED
  functional consumers in futon4 — `arxana-vsatarcs-sorrys.el` (defcustom reader,
  also feeds `arxana-browser-vsatarcs.el`) and `lift_unlifted_stories.bb`
  (`SORRYS-PATH`). Both retargeted resources-first-with-data-fallback. (They're
  on-demand tools, not the live serving path, so the WM kept working — but they'd
  break on next use.)
- **Tests:** `file_ingest_test` + `multi_test` updated to assert the canonical
  `resources/` path (kept `data/` as transition-safe). Green: **13 tests / 43
  assertions / 0 fail**.
- **Git-mining synergy proven:** `wm_outer_loop` closure-counter sees the tracked
  registry's `:status :addressed` adds (one-time over-count on the initial
  tracking commit — noted, harmless).
- **FINDING (shared working tree):** futon2/futon3c hold *other agents'*
  uncommitted WIP. Any R-A.1 commit MUST be strictly path-limited (never
  `git add -A`). This is the same single-shared-tree hazard behind the R-C
  worktree refinement and the operator's branch-spaghetti concern — strengthens
  the case for per-agent worktrees (or jj workspaces) for unattended runs.

### INSTANTIATE outcome (2026-05-30)
**Verified end-to-end via the reversible probe** (operator's verify-as-we-go
plan): R-A.1 (relocated tracked registry + git ledger + reversibility), R-A.2
(regression invariant live), R-E (G1/G2/G3 gates), the cycle apparatus, and a
non-disruptive path-limited commit ledger. The COMPONENTS are proven.

**R-A.1/R-A.2 committed** (path-limited, others' WIP untouched): futon2 `219f026`,
futon3c `073497a`, futon4 `d36f3f6`. Confidence pass above.

### Car 6 — outing harness + live family + first live cycle (DONE)
- Built `src/futon3c/peripheral/outing.clj` (committed `0841fa1`): `start-outing!`
  / `record-cycle!` (R-E gating + R-D halt: hard / soft / stuck-detector) /
  `finalize-outing!` (R-M run-summary). **Offline-verified:** conforming cycle →
  continue; fake-finish cycle → hard-halt `:g3-fake-finish`; summary written.
- **`:sorry-closures-stick` registered as a live probe family** (R-A.2 dual-use)
  — now fires on the Arxana Live-Violations surface (hourly) in addition to the
  per-cycle G2 gate. (Runtime registration; bootstrap auto-register is a remaining
  wiring item so it survives a restart.)
- **First live cycle through the harness:** the WM ranked `r3d-per-entity-
  attribution` (a deep `:prototyping-forward` sorry) at rank-1. Honest outcome —
  **declined to fake-finish** it (no earned fix possible in a bounded cycle);
  closed as a no-progress cycle (top-shift false, pred-error 0.0, G1 CONFORMS),
  recorded via the harness, finalized → first real run-summary at
  `data/outings/2026-05-30-outing-2026-0.edn` (1 attempted, 0 discharged,
  `:no-tractable-work`). Probe reversed; registry matches HEAD.

### First-outing finding (the verify-as-we-go truth)
The apparatus runs end-to-end. But the **current queue is all deep
`:prototyping-forward` work** — `r3d-per-entity-attribution`, two `r3a-likelihood`
models, 32 `stub-lifts` — none earn-dischargeable in a bounded autonomous cycle;
the one tractable item (`wm-ui-hud-mode-rationale-hardcode`) **collides with
another agent's live `core.cljs` WIP**. So a *discharge-producing* first outing is
**gated on tractable, collision-free work**, exactly as §9 ARGUE predicted ("if it
does little, that's the trial telling us"). **Path forward:** author dedicated
excursions (E-prefix) for the likelihood-model / attribution sorries, or run when
the queue carries tractable items — not an autonomous grind on deep work.

### Car 7 — inline decomposition (REPERTOIRE UPGRADE, operator insight 2026-05-30)

Operator: *"it seems weird to pre-seed E-excursions to upsample data the system
already has — it's the same system — why can't it write the excursions inline as
part of the WM run?"* Correct, and it corrects a conservatism in car 6: a deep
sorry the runner can't one-shot is a **niche-construction signal** (`repl.spec.edn`
false-floor → ∇-deform; `[[niche-construction]]`), not a dead end. The in-loop
agent — being the *same system* — decomposes it inline.

**Cycle repertoire is therefore {discharge | decompose | no-progress | halt}**,
not just discharge/no-progress. A **decompose** cycle authors an excursion (+
optionally mines bounded sub-sorries) as a ∇-deform PRINT: productive (V4), does
NOT discharge the sorry, does NOT trip the stuck-detector, is NOT a fake-finish.

**Refines R-I:** authoring an excursion **doc** is low-blast-radius + reversible
→ allowed inline (esp. supervised). What stays gated for *unattended* runs is
**autonomously executing** the opened work; flooding is bounded by R-G (promotion
OFF) + the hole-budget. So decompose-inline ≠ the escalated ∇-deform.

**Demonstrated live:** WM ranked `r3d-per-entity-attribution` #1 (deep; its
blocker — the typed per-entity event vocabulary — is already gone, so it's
scopable now). The cycle's move was to author `E-r3d-per-entity-attribution.md`
inline (cg-f32696ac, run live-08b19d47), annotate r3d `:has-excursion`, and record
a decompose cycle: G1 CONFORMS, `:decompositions 1`, `:discharged 0` — path
constructed, not faked. Harness `record-cycle!` updated to treat `:delta-grad?`
as progress. Run-summary: `data/outings/2026-05-30-outing-2026-0.edn`.

### INSTANTIATE state
Apparatus **COMPLETE + verified**: R-A.1 (tracked registry/ledger), R-A.2
(regression invariant, per-cycle gate + live family), the cycle (begin/close +
G1/G2/G3), the harness (bounds/halt/summary), git-level reversibility — all live
and committed. **Remaining for a sustained run** (named, not silent): bootstrap
auto-registration of the family; the `/loop` driver for multi-cycle autonomy
(R-B); the git **worktree / jj workspace** for R-C isolation; the R-L expectations
write-up; and **tractable queue work** (the actual gating factor for a
discharge-producing outing).
