# Excursion: E-invariant-stepper — a live "green checks" surface for the stack's invariants

**Date:** 2026-07-02
**Status:** IDENTIFY (chartered; not built). Owner: TBD.
**Repo:** operator-side Emacs (sibling to `futon0/contrib/loop-lag.el`); models invariants that live
across futon3c (`CLAUDE.md` I-0..I-5, `docs/wiring-claims.edn`/`wiring-evidence.edn`, mission
logic-models), futon4 (`arxana-window-constraints.el` Reazon invariants), and futon-theory.
Provenance: proposed by Joe 2026-07-02 at the close of the loop-lag hunt.

## HEAD (one line)
An **opt-in Emacs mode** (shape of `loop-lag-mode`) that builds and displays a **live model of the
system's invariants — which are set up, tested, and currently validated** — as **"green checks" for
*live* tests**: the unit-test green-check experience, but continuously over the *running* system
rather than a fixture.

## Why (the motivating lesson, this session)
The Emacs finalisation lag turned out to be `arxana-window-constraints` — **Reazon-backed *live*
invariant checks** (window layout must satisfy a logic query) — but done **badly**: always-on, on the
global `window-configuration-change-hook`, silent, uninspectable, and expensive (a logic query per
window change → multi-second freezes). The fix made them **opt-in + early-out**. The deeper lesson:
the stack HAS live invariants (arxana constraints, the wiring-contract claims, mission logic-models
TB-1..7, the I-0..I-5 architectural invariants, the typed-bell contract), but there is **no
first-class, inspectable, cheap surface** to see *which invariants are set up, which are checked live,
and which currently hold*. `loop-lag` proved the value of a lightweight, opt-in, inspectable indicator
for one property (event-loop lag); **invariant-stepper is that shape generalised to invariants.**

## The shape
- **A registry of invariants**: each entry = `{:name :scope :check-fn :evidence}` — a name, what it
  guards, a **cheap live check** returning pass/fail/unknown, and a link to its witness (a
  wiring-evidence commit, a mission logic-model line, a test).
- **A report buffer** (like `loop-lag-report`): green / amber / red per invariant, grouped by scope,
  with last-checked time and the failure detail. The **"green checks for live tests"** surface.
- **Step, don't hammer** (the load-bearing lesson from arxana): checks run on **demand / step /
  idle**, never on a hot path (window-change, redisplay, per-turn). `invariant-stepper` = step through
  invariants one at a time (inspect each), plus an idle/on-request full sweep. Opt-in minor mode, OFF
  by default.
- **Model-building**: over a session it accumulates *which invariants have been validated live* — a
  growing, inspectable model of the system's proven-good state (distinct from unit tests, which prove
  it in a fixture). Ties to the futonic **status-as-witness** discipline: a live green check is a
  witness that the invariant holds *now*.

## Candidate invariant sources to register
- futon3c **I-0..I-5** (one JVM; agent identity singular; transport routes not creates; peripherals
  inhabited; read-before-write; no futon3 deps) — several are live-checkable (`pgrep java` == 1; grep
  for spawn-in-transport).
- **wiring-claims.edn / wiring-evidence.edn** — machine-readable architectural claims already exist;
  each could carry a live check.
- **mission logic-models** (e.g. typed-bells TB-1..7) — already stated as checkable propositions.
- **arxana window/data constraints** (Reazon) — already live checks; register them here (and *only*
  run via the stepper, not the global hook — this excursion is partly their proper home).
- **typed-bell contract**, substrate-2 canonical-id gate, etc.

## VERIFY (acceptance — to define at DERIVE)
A registered invariant shows green/red live; the report reads at a glance ("what's validated right
now"); checks are cheap (no hot-path cost — the arxana anti-pattern is not repeated); opt-in mode off
by default.

## Cross-links
- `futon0/contrib/loop-lag.el` — the sibling instrument (opt-in, inspectable, report buffer); reuse
  its mode/report shape.
- [[project_loop_lag_emacs_stalls]] — the arxana-constraints episode that motivates this (live
  invariant checks done badly → do them first-class + cheap + inspectable).
- `futon3c/docs/wiring-claims.edn` + `wiring-evidence.edn` — the existing machine-readable claims to
  register as live invariants.
- futon4 `arxana-window-constraints.el` — Reazon live-checks; their proper opt-in home.
